open CCoreSyntax
open CCoreExceptions
open CCoreUtils


(* Simple Libpcap toplevel. 
    Just uses the default helpers, imports, pkt_handler and main_fun, 
    and has a simple main function that opens some pcaps from stdin *)   
     
(**** helpers ****)
let get_event_tag t_event = 
    let ev_param = cid"ev", tref t_event in
    dfun 
      (cid"get_event_tag")
      (tint event_tag_size)
      [ev_param]
      (sret (ecast (tint event_tag_size) (((param_evar ev_param)/->cid"data")/.cid"tag")))
  ;;
  let reset_event_tag t_event =
    (* this isn't right. Need an address.. *)
    let ev_param = cid"ev", tref t_event in
    let enum_ty = (((param_evar ev_param)/->cid"data")/.cid"tag").ety in
    dfun
      (cid"reset_event_tag")
      (tunit)
      [ev_param]
      (sassign_exp (((param_evar ev_param)/->cid"data")/.cid"tag") (ecast (enum_ty) (default_exp (tint event_tag_size))))
  ;;
  
  let init_cursor = 
    let bytes_ptr_t = tref CCoreParse.packet_t |> CCorePPrint.ty_to_string ~use_abstract_name:true in
    dforiegn [%string
 {|
 void init_cursor(uint8_t*  buf , uint32_t len , %{bytes_ptr_t}  bytes ){
    bytes->start = buf;
    bytes->cursor = buf;
    bytes->end = buf + len;
    bytes->bit_off = 0;
 }|}]
 ;;
 let reset_cursor = 
    let bytes_ptr_t = tref CCoreParse.packet_t |> CCorePPrint.ty_to_string ~use_abstract_name:true in
    dforiegn [%string
 {|
 void reset_cursor(%{bytes_ptr_t}  bytes){
    bytes->cursor = bytes->start;
    bytes->bit_off = 0;
 }|}]
 
 (* copy an input packet to an output packet *)
 (* Assumption: buf_out and buf_in are allocated by the same 
    internal function and have the same maximum size.
    This ensures that if data fits in buf_in, it will also fit in buf_out, 
    eliminating the need for explicit bounds checking within this function. *)
 let copy_packet = 
    dforiegn [%string
 {|
 void copy_packet(packet_t*  buf_out , packet_t*  buf_in ) {
    memcpy(buf_out->start, buf_in->start, buf_in->end - buf_in->start);
    buf_out->cursor = buf_out->start + (buf_in->cursor - buf_in->start);
    buf_out->end = buf_out->start + (buf_in->end - buf_in->start);
    buf_out->bit_off = 0;
 }|}]
 ;;
 

(* the queue-based dispatch model no longer needs the event-tag helpers (it
   dispatches on the handler's out_event count + out_loc, not on tags). *)
let helpers _decls =
    [
        init_cursor;
        reset_cursor;
        copy_packet;
    ]
;;

let imports = [
        CCoreSyntax.dinclude "<stdio.h>";
        CCoreSyntax.dinclude "<stdlib.h>";
        CCoreSyntax.dinclude "<stdint.h>";
        CCoreSyntax.dinclude "<stdbool.h>";
        dinclude "<pcap.h>";
        dinclude "<string.h>";
        dinclude "<time.h>";
        dforiegn 
{|
#ifdef DEBUG
    #define debug_printf(...) printf(__VA_ARGS__)
    #else
    #define debug_printf(...)
#endif            
|};
        dforiegn 
{|
#ifdef __GNUC__
    #define unroll GCC unroll
#endif
|}    
]
(* The pcap driver runs the dispatch pipeline (see the design in the driver
   comment below): parse -> internal dispatch queue -> handle -> {recirc back onto
   the queue | deparse + send out a port}. There are no OS threads; the queue is
   drained fully for each input packet, which keeps the input packet buffer valid
   for the whole drain so port events (even those produced by recirculated events)
   can reuse its payload via copy_packet. *)
let pkt_handler = dforiegn [%string
{|
/********* internal dispatch FIFO of events ***********/
#define EV_QUEUE_CAP 1024
typedef struct ev_queue_t {
    events buf[EV_QUEUE_CAP];
    int head;
    int tail;
    int count;
} ev_queue_t;

static void evq_init(ev_queue_t* q) { q->head = 0; q->tail = 0; q->count = 0; }
static int  evq_empty(ev_queue_t* q) { return q->count == 0; }
static void evq_push(ev_queue_t* q, events* ev) {
    // no overflow guard (see handler lowering); EV_QUEUE_CAP is generous.
    q->buf[q->tail] = *ev;
    q->tail = (q->tail + 1) % EV_QUEUE_CAP;
    q->count++;
}
static void evq_pull(ev_queue_t* q, events* out) {
    *out = q->buf[q->head];
    q->head = (q->head + 1) % EV_QUEUE_CAP;
    q->count--;
}

/********* per-handler context ***********/
uint64_t pkt_ct = 0;

// a 32-bit nanosecond timestamp, stamped onto each event at dequeue (Sys.time()).
// (replay/wall-clock time -- if a program serializes Sys.time() its pcap output would
// be non-deterministic; switch to pkthdr->ts here if that ever matters for a test.)
static uint32_t now_ns(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint32_t)((uint64_t)ts.tv_sec * 1000000000ull + (uint64_t)ts.tv_nsec);
}

typedef struct pkt_hdl_ctx_t {
    uint8_t ingress_port;
    pcap_dumper_t *out_pcap;
    packet_t in_pkt;
    packet_t out_pkt;
    struct pcap_pkthdr out_pkthdr;
    ev_queue_t queue;
} pkt_hdl_ctx_t;

void fill_out_pkthdr(const struct pcap_pkthdr *in_pkthdr, packet_t* out_pkt, uint8_t* dump_end, struct pcap_pkthdr* out_pkthdr) {
    // the deparsed packet runs from out_pkt->cursor to dump_end (= out_pkt->end for a
    // payload event, = the payload boundary for a no-payload event -- see below)
    out_pkthdr->ts = in_pkthdr->ts;
    out_pkthdr->caplen = (uint32_t)(dump_end - out_pkt->cursor);
    out_pkthdr->len    = (uint32_t)(dump_end - out_pkt->cursor);
}

void lpcap_packet_handler(u_char *ctx, const struct pcap_pkthdr *pkthdr, const u_char *packet) {
    pkt_hdl_ctx_t * hdl_ctx = (pkt_hdl_ctx_t *)ctx;
    init_cursor((uint8_t *)packet, pkthdr->len, &hdl_ctx->in_pkt); // construct a new cursor

    // parse round: parse the input packet, push the event onto the queue
    events ev0;
    if (parse_event(&hdl_ctx->in_pkt, &ev0) != 1) {
        debug_printf("parse failed!\n");
        return; // drop
    }
    evq_push(&hdl_ctx->queue, &ev0);

    // dispatch round: drain the queue
    while (!evq_empty(&hdl_ctx->queue)) {
        events ev;
        evq_pull(&hdl_ctx->queue, &ev);
        ev.meta.timestamp = now_ns(); // stamp at dequeue (covers arriving + recirculated events)
        ev.meta.in_port = hdl_ctx->ingress_port; // ingress port (read by the handler)
        out_event out_events[%{string_of_int CCoreHandlers.out_events_cap}];
        uint16_t n = handle_event(&ev, out_events);
        for (uint16_t i = 0; i < n; i++) {
            if (out_events[i].out_loc == 1) {
                // recirculation: re-queue for dispatch
                evq_push(&hdl_ctx->queue, &out_events[i].ev);
            } else if (out_events[i].out_loc == 2) {
                // output to a port: deparse over a copy of the input packet, then
                // dump. (pcap has a single output file, so out_events[i].port is
                // recorded but every port event goes to the same dump.)
                reset_cursor(&hdl_ctx->out_pkt);
                copy_packet(&hdl_ctx->out_pkt, &hdl_ctx->in_pkt);
                // out_pkt.cursor sits at the payload boundary (input parse position) until
                // deparse prepends the header. A no-payload event emits only its header
                // (drop the input tail); a payload event keeps the tail. (matches interp)
                uint8_t *payload_boundary = hdl_ctx->out_pkt.cursor;
                deparse_event(&out_events[i].ev, &hdl_ctx->out_pkt);
                uint8_t *dump_end = out_events[i].ev.meta.has_payload ? hdl_ctx->out_pkt.end : payload_boundary;
                fill_out_pkthdr(pkthdr, &hdl_ctx->out_pkt, dump_end, &hdl_ctx->out_pkthdr);
                pcap_dump((u_char *)hdl_ctx->out_pcap, &hdl_ctx->out_pkthdr, (u_char *)hdl_ctx->out_pkt.cursor);
            }
        }
    }
    pkt_ct++;
}|}]
;;
let main =
  dforiegn
  [%string{|
/********** allocation + main ***********/
// Deparse writes backwards (cursor -= N, prepending headers). If an output event's
// header is larger than the input event's, the cursor decrements past the logical
// packet start. HEADROOM reserves that slack at the front of the allocation so the
// backward writes stay inside the buffer. (Generous constant for now; once bit-packing
// lands we can derive a real bound from the largest event's serialized size.)
#define HEADROOM 256

pkt_hdl_ctx_t mk_pkt_hdl_ctx(pcap_dumper_t* out_pcap, u_char* out_buf, uint32_t out_buf_len) {
pkt_hdl_ctx_t ctx = {
    .ingress_port = 1, // always 1 in this driver
    .out_pcap = out_pcap,
    .in_pkt = {0},
    .out_pkt = {
    // logical packet starts HEADROOM into the allocation, leaving room to grow downward
    .start = (uint8_t *)out_buf + HEADROOM,
    .cursor = (uint8_t *)out_buf + HEADROOM,
    .end = (uint8_t *)out_buf + HEADROOM + out_buf_len
    },
    .out_pkthdr = {0},
    .queue = {0}
};
return ctx;
}

int main(int argc, char const *argv[]){
    if (argc != 3) {
        debug_printf(stderr, "Usage: %s <input pcap file> <output pcap file>\n", argv[0]);
        return 1;
    }

    char errbuf[PCAP_ERRBUF_SIZE];    
    // Open the input pcap file in read mode
    pcap_t *in_pcap = pcap_open_offline(argv[1], errbuf);
    if (in_pcap == NULL) {
        debug_printf(stderr, "Error opening input pcap file: %s\n", errbuf);
        return 1;
    }

    // Open the output pcap file in write mode
    pcap_dumper_t *out_pcap = pcap_dump_open(in_pcap, argv[2]);
    if (out_pcap == NULL) {
        debug_printf(stderr, "Error opening output pcap file: %s\n", pcap_geterr(in_pcap));
        return 1;
    }

    // prepare the context for the packet handler (reserve HEADROOM in front of the
    // 1600-byte usable region so backward deparse writes have room to grow)
    u_char outbuf[1600 + HEADROOM];
    pkt_hdl_ctx_t ctx = mk_pkt_hdl_ctx(out_pcap, outbuf, 1600);

    pcap_loop(in_pcap, 0, lpcap_packet_handler, (u_char *)&ctx);

    printf("Processed %llu packets\n", pkt_ct);

    // Close the pcap files
    pcap_dump_close(out_pcap);
    pcap_close(in_pcap);

    return 0;
}|}]

let package_prog decls = 
    [
        "lucidprog.c", `Decls (imports @ decls @ helpers decls @ [pkt_handler] @ [main]);
        "makefile", `String ("all: lucidprog\n\nlucidprog: lucidprog.c\n\tgcc -o lucidprog lucidprog.c -lpcap\n\n")
    ]
;;

