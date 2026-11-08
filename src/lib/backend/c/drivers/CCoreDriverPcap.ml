open CCoreSyntax
open CCoreUtils

(* Libpcap toplevel driver: replays input pcaps through the program in virtual
   time, for deterministic offline runs. Re-uses most of the rawsocket driver's
   pipeline; this driver supplies the pcap port map and packet library, and its
   own receive, dispatch and main loop, which keep the virtual clock.

   The clock. A record's timestamp is its packet's arrival time (inputs are read
   at nanosecond precision). Records from every port, recirculated events and
   delayed outputs are handled in time order. An event is stamped by the arrival
   rule (the clock advances to a later arrival; an earlier one takes the clock),
   and that stamp is Sys.time(). An output leaves at the stamp plus the larger of
   its delay and the latency (`--latency-ns N`, default 0): a recirculation
   arrives then, an output record is written with that time. The latency models
   a platform's (a Tofino's is about 600 ns, which Lucid's interpreter models);
   compiled C adds none of its own.

   Ports are wired like the raw-socket driver, but each binds an input AND output file:
     ./lucidprog --interface 0:in0.pcap:out0.pcap --interface 1:in1.pcap:out1.pcap
   The driver's own messages go to stderr, so stdout carries only the program's prints. *)

module R = CCoreDriverRawSocket

let imports = dforiegn {|
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <time.h>
#include <sys/time.h>
#include <pcap.h>

#ifdef DEBUG
  #define debug_printf(...) fprintf(stderr, __VA_ARGS__)
#else
  #define debug_printf(...)
#endif
|}

(* port map: each port gets separate input and output files, and holds the
   next record read from its input (the clock needs to see the earliest record
   across ports before taking one). *)
let port_map_lib = dforiegn {|
/********* ports (Lucid port number <-> pcap input/output) ***********/
typedef struct { int port_id; pcap_t* in; pcap_dumper_t* out; int in_eof; int has_pending; uint16_t pending; } port_t;
typedef struct { port_t ports[MAX_PORTS]; int nports; } port_map_t;

static pcap_dumper_t* get_out_descriptor(port_map_t* pm, int port_id) {
    for (int i = 0; i < pm->nports; i++) if (pm->ports[i].port_id == port_id) return pm->ports[i].out;
    return NULL;
}

// parse `--interface PORT:INFILE:OUTFILE` args (and `--latency-ns N`) and open the
// pcaps at nanosecond precision: a record's timestamp is the packet's arrival time.
static int init_port_map(port_map_t* pm, int argc, char** argv) {
    char errbuf[PCAP_ERRBUF_SIZE];
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--latency-ns") == 0 && i + 1 < argc) { g_latency_ns = strtoull(argv[++i], NULL, 10); continue; }
        if (strcmp(argv[i], "--interface") == 0 && i + 1 < argc) {
            char* spec = argv[++i];
            char* c1 = strchr(spec, ':');
            char* c2 = c1 ? strchr(c1 + 1, ':') : NULL;
            if (!c1 || !c2) { fprintf(stderr, "bad --interface '%s' (expected PORT:IN:OUT)\n", spec); return 1; }
            *c1 = '\0'; *c2 = '\0';
            int port_id = atoi(spec);
            const char* infile = c1 + 1;
            const char* outfile = c2 + 1;
            if (pm->nports >= MAX_PORTS) { fprintf(stderr, "too many interfaces (max %d)\n", MAX_PORTS); return 1; }
            pcap_t* in = pcap_open_offline_with_tstamp_precision(infile, PCAP_TSTAMP_PRECISION_NANO, errbuf);
            if (!in) { fprintf(stderr, "open input '%s': %s\n", infile, errbuf); return 1; }
            pcap_dumper_t* out = pcap_dump_open(in, outfile);
            if (!out) { fprintf(stderr, "open output '%s': %s\n", outfile, pcap_geterr(in)); return 1; }
            pm->ports[pm->nports].port_id = port_id;
            pm->ports[pm->nports].in = in;
            pm->ports[pm->nports].out = out;
            pm->ports[pm->nports].in_eof = 0;
            pm->ports[pm->nports].has_pending = 0;
            pm->nports++;
            fprintf(stderr, "bound port %d: %s -> %s\n", port_id, infile, outfile);
        }
        // ignore unknown args (e.g. the .dpt path, for argv-compatibility with lucidSwitch)
    }
    return 0;
}
|}

(* the pcap packet library: reading records into slots, and egress *)
let pcap_lib = dforiegn {|
// the node's clock: the stamp of the last event dequeued (the arrival rule advances it)
static uint64_t g_clock = 0;

// hold the port's next record in a slot (arrival = the record's timestamp); 1 if it has one
static int port_fetch(port_t* p, slab_t* s) {
    if (p->has_pending) return 1;
    if (p->in_eof) return 0;
    struct pcap_pkthdr* h; const u_char* data;
    int r = pcap_next_ex(p->in, &h, &data);
    if (r != 1) { p->in_eof = 1; return 0; }
    uint16_t idx = slot_alloc(s);
    if (idx == SLOT_NONE) { fprintf(stderr, "packet pool exhausted\n"); p->in_eof = 1; return 0; }
    qe_t* q = slot(s, idx);
    uint32_t len = h->caplen; if (len > SLOT_USABLE) len = SLOT_USABLE;
    memcpy(q->data + HEADROOM, data, len);
    q->pkt_len = len;
    q->arrival = (uint64_t)h->ts.tv_sec * 1000000000ull + (uint64_t)h->ts.tv_usec; // nanoseconds at this precision
    p->pending = idx; p->has_pending = 1;
    return 1;
}

// the port whose next record arrives first (its time in *t), or -1 when every input is done
static int next_input_port(port_map_t* pm, slab_t* s, uint64_t* t) {
    int best = -1; uint64_t bt = UINT64_MAX;
    for (int i = 0; i < pm->nports; i++) {
        port_t* p = &pm->ports[i];
        if (!port_fetch(p, s)) continue;
        uint64_t at = slot(s, p->pending)->arrival;
        if (at < bt) { bt = at; best = i; }
    }
    *t = bt;
    return best;
}

// egress: dump the deparsed frame [buf, buf+len) to the port's output pcap, stamped
// with its departure time
static void send_frame(pcap_dumper_t* out, uint8_t* buf, size_t len, uint64_t departure_ns) {
    if (out == NULL) { debug_printf("send_frame: no egress dumper (dropped)\n"); return; }
    struct pcap_pkthdr h = {0};
    h.ts.tv_sec = (time_t)(departure_ns / 1000000000ull);
    h.ts.tv_usec = (suseconds_t)(departure_ns % 1000000000ull);   // nanoseconds: the dumper has the input's precision
    h.caplen = (bpf_u_int32)len;
    h.len    = (bpf_u_int32)len;
    pcap_dump((u_char*)out, &h, buf);
}
|}

(* receive and dispatch with the virtual clock (the raw-socket driver's do_rx
   and do_dispatch read bursts and stamp real time) *)
let rx_dispatch =
  let packet_t_ty = R.packet_t_ty and parse_event_fn = R.parse_event_fn and handle_event_fn = R.handle_event_fn in
  dforiegn [%string {|
// parse the frame in slot `idx` into its event; 1 if it parsed (the slot is freed otherwise)
static int ingest_slot(uint16_t idx, int in_port) {
    qe_t* q = slot(&g_slab, idx);
    %{packet_t_ty} view;
    init_cursor(q->data + HEADROOM, q->pkt_len, &view);
    if (%{parse_event_fn}(&view, &q->ev, (uint32_t)in_port) != 1) { debug_printf("parse failed\n"); slot_free(&g_slab, idx); return 0; }
    q->payload_off = (uint32_t)(view.cursor - (q->data + HEADROOM));
    q->ev.meta.in_port = in_port;
    pkt_ct++;
    return 1;
}
// handle one element: the arrival rule stamps it, the handler runs, its outputs go to tx
static void dispatch_one(uint16_t idx) {
    qe_t* q = slot(&g_slab, idx);
    if (q->arrival > g_clock) g_clock = q->arrival;
    q->ev.meta.timestamp = g_clock & 0xFFFFFFFFFFFFull;
    q->n_out = %{handle_event_fn}(&q->ev, q->out_events);
    if (ring_push(&tx_in, idx) != 0) slot_free(&g_slab, idx);
}
|}]

let main = dforiegn {|
int main(int argc, char** argv) {
    slab_init(&g_slab);
    ring_init(&dispatch_in);
    ring_init(&tx_in);
    if (init_port_map(&g_port_map, argc, argv) != 0) { fprintf(stderr, "failed to init port map\n"); return 1; }
    if (g_port_map.nports == 0) {
        fprintf(stderr, "usage: %s --interface PORT:IN:OUT [--interface PORT:IN:OUT ...] [--latency-ns N]\n", argv[0]);
        return 1;
    }
    // the replay loop, in time order: the next thing to handle is whichever comes first,
    // a delayed output that is due, the earliest unread input record, or the head of the
    // recirculation queue (whose arrivals are non-decreasing, since parents are handled
    // in time order); outputs are drained after each, so the replay is lossless
    for (;;) {
        uint64_t t_in; int p = next_input_port(&g_port_map, &g_slab, &t_in);
        uint16_t qidx; int have_q = ring_peek(&dispatch_in, &qidx) == 0;
        uint64_t t_q = have_q ? slot(&g_slab, qidx)->arrival : UINT64_MAX;
        uint64_t t_cal; int have_cal = cal_peek(&g_calendar, &t_cal) == 0;
        if (p < 0 && !have_q && !have_cal) break;
        if (have_cal && t_cal <= t_in && t_cal <= t_q) { // a due departure first: it only writes or re-enqueues
            do_timers(t_cal);
            continue;
        }
        if (p >= 0 && t_in < t_q) {                    // a tie goes to the queue: it was scheduled first
            port_t* pt = &g_port_map.ports[p];
            uint16_t idx = pt->pending; pt->has_pending = 0;
            if (ingest_slot(idx, pt->port_id)) dispatch_one(idx);
        } else {
            ring_pop(&dispatch_in, &qidx);
            dispatch_one(qidx);
        }
        while (!ring_empty(&tx_in)) do_tx();
    }
    fflush(stdout);
    fprintf(stderr, "Processed %llu packets\n", (unsigned long long)pkt_ct);
    for (int i = 0; i < g_port_map.nports; i++) {
        pcap_dump_close(g_port_map.ports[i].out);
        pcap_close(g_port_map.ports[i].in);
    }
    return 0;
}
|}

(* the calendar is always there: the latency is a run-time option *)
let package_prog decls =
    let d = !CCoreHandlers.has_delays in
    let sm = section_marker in
    [
        "lucidprog.c", `Decls (
            [imports]
            @ [sm "program code"] @ decls
            @ [sm "driver config"; R.config]
            @ [sm "driver libraries"; R.helpers_lib; R.ring_lib; R.slab_lib true; R.calendar_lib; port_map_lib]
            @ [sm "driver pipeline"; R.pipe_state; pcap_lib; rx_dispatch; R.tx ~calendar:true ~has_delays:d]
            @ [sm "driver main"; main]);
        "makefile", `String "all: lucidprog\n\nlucidprog: lucidprog.c\n\tgcc -O2 -o lucidprog lucidprog.c -lpcap\n\n"
    ]
;;
