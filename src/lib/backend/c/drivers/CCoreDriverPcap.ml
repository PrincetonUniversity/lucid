open CCoreSyntax
open CCoreUtils

(* Libpcap toplevel driver.
   Re-uses most of the rawsocket driver's pipeline. 
   This driver just supplies a pcap-based packet library and port map library. 
   Note that output pcaps are stamped with ts=0, so that every run 
   can compare to a reference output pcap directly. 
   Ports are wired like the raw-socket driver, but each binds an input AND output file:
     ./lucidprog --interface 0:in0.pcap:out0.pcap --interface 1:in1.pcap:out1.pcap  *)

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

(* port map: each port gets separate input and output files. *)
let port_map_lib = dforiegn {|
/********* ports (Lucid port number <-> pcap input/output) ***********/
typedef struct { int port_id; pcap_t* in; pcap_dumper_t* out; int in_eof; } port_t;
typedef struct { port_t ports[MAX_PORTS]; int nports; } port_map_t;

// get_in_descriptor returns the whole port (so port_rx can set in_eof); get_out_descriptor
// looks a port up by id and returns its dumper (NULL = no such port). Consumed only by
// port_rx / send_frame; the pipeline treats them opaquely (mirrors the socket driver).
static port_t* get_in_descriptor(port_map_t* pm, int port_idx) { return &pm->ports[port_idx]; }
static pcap_dumper_t* get_out_descriptor(port_map_t* pm, int port_id) {
    for (int i = 0; i < pm->nports; i++) if (pm->ports[i].port_id == port_id) return pm->ports[i].out;
    return NULL;
}

// parse `--interface PORT:INFILE:OUTFILE` args and open the pcaps.
static int init_port_map(port_map_t* pm, int argc, char** argv) {
    char errbuf[PCAP_ERRBUF_SIZE];
    for (int i = 1; i < argc; i++) {
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
            pcap_t* in = pcap_open_offline(infile, errbuf);
            if (!in) { fprintf(stderr, "open input '%s': %s\n", infile, errbuf); return 1; }
            pcap_dumper_t* out = pcap_dump_open(in, outfile);
            if (!out) { fprintf(stderr, "open output '%s': %s\n", outfile, pcap_geterr(in)); return 1; }
            pm->ports[pm->nports].port_id = port_id;
            pm->ports[pm->nports].in = in;
            pm->ports[pm->nports].out = out;
            pm->ports[pm->nports].in_eof = 0;
            pm->nports++;
            printf("bound port %d: %s -> %s\n", port_id, infile, outfile);
        }
        // ignore unknown args (e.g. the .dpt path, for argv-compatibility with lucidSwitch)
    }
    return 0;
}
|}

(* ===== the pcap packet library: the read (port_rx via pcap_next_ex) and write
   (send_frame via pcap_dump) the shared do_rx / do_tx call. ===== *)
let pcap_lib = dforiegn {|
// a burst of freshly-read slab slots (pkt_len set, not yet parsed).
typedef struct { uint16_t n; uint16_t idx[BURST]; } rx_batch;

// read up to BURST frames from a port's input pcap into slots allocated from `s`. Sets
// in_eof when the capture is exhausted.
static rx_batch port_rx(port_t* p, slab_t* s) {
    rx_batch batch; batch.n = 0;
    while (batch.n < BURST) {
        uint16_t idx = slot_alloc(s);
        if (idx == SLOT_NONE) break;                       // pool exhausted -> drop-at-birth
        struct pcap_pkthdr* h; const u_char* data;
        int r = pcap_next_ex(p->in, &h, &data);
        if (r != 1) { slot_free(s, idx); p->in_eof = 1; break; } // EOF/error -> done with this port
        qe_t* q = slot(s, idx);
        uint32_t len = h->caplen; if (len > SLOT_USABLE) len = SLOT_USABLE;
        memcpy(q->data + HEADROOM, data, len);
        q->pkt_len = len;
        batch.idx[batch.n++] = idx;
    }
    return batch;
}

// egress: dump the deparsed frame [buf, buf+len) to the port's output pcap. The record
// timestamp is fixed at 0: this driver is for functional replay -- deterministic,
// byte-comparable output -- not timing (use another driver to profile). NULL dumper drops.
static void send_frame(pcap_dumper_t* out, uint8_t* buf, size_t len) {
    if (out == NULL) { debug_printf("send_frame: no egress dumper (dropped)\n"); return; }
    struct pcap_pkthdr h = {0};   // ts = 0 (see above)
    h.caplen = (bpf_u_int32)len;
    h.len    = (bpf_u_int32)len;
    pcap_dump((u_char*)out, &h, buf);
}
|}

let main = dforiegn {|
int main(int argc, char** argv) {
    if (init_port_map(&g_port_map, argc, argv) != 0) { fprintf(stderr, "failed to init port map\n"); return 1; }
    if (g_port_map.nports == 0) {
        fprintf(stderr, "usage: %s --interface PORT:IN:OUT [--interface PORT:IN:OUT ...]\n", argv[0]);
        return 1;
    }
    slab_init(&g_slab);
    ring_init(&dispatch_in);
    ring_init(&tx_in);
    printf("Init complete.\n");
    fflush(stdout);

    // replay loop: rx -> dispatch -> tx (each a bounded burst) until every input is
    // exhausted AND the pipeline has drained. Offline libpcap blasts through, so there
    // is no idle wait (unlike the socket driver's select loop).
    for (;;) {
        do_rx();
        do_dispatch();
        do_tx();
        int all_eof = 1;
        for (int i = 0; i < g_port_map.nports; i++) if (!g_port_map.ports[i].in_eof) all_eof = 0;
        if (all_eof && ring_empty(&dispatch_in) && ring_empty(&tx_in)) break;
    }

    printf("Processed %llu packets\n", (unsigned long long)pkt_ct);
    for (int i = 0; i < g_port_map.nports; i++) {
        pcap_dump_close(g_port_map.ports[i].out);
        pcap_close(g_port_map.ports[i].in);
    }
    return 0;
}
|}

let package_prog decls =
    let sm = section_marker in
    [
        "lucidprog.c", `Decls (
            [imports]
            @ [sm "program code"] @ decls
            @ [sm "driver config"; R.config]
            @ [sm "driver libraries"; R.helpers_lib; R.ring_lib; R.slab_lib; port_map_lib; pcap_lib]
            @ [sm "driver pipeline"; R.pipe_state; R.rx; R.dispatch; R.tx]
            @ [sm "driver main"; main]);
        "makefile", `String "all: lucidprog\n\nlucidprog: lucidprog.c\n\tgcc -O2 -o lucidprog lucidprog.c -lpcap\n\n"
    ]
;;
