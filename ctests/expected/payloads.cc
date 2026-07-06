#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <pcap.h>
#include <string.h>
#include <time.h>

#ifdef DEBUG
    #define debug_printf(...) printf(__VA_ARGS__)
    #else
    #define debug_printf(...)
#endif            


#ifdef __GNUC__
    #define unroll GCC unroll
#endif

typedef struct {
  uint64_t _0;
  uint64_t _1;
  uint16_t _2;
} tuple_1;
typedef struct {
  uint32_t _0;
  uint32_t _1;
  uint32_t _2;
  uint32_t _3;
} tuple_2;
typedef struct {
  uint8_t*  start;
  uint8_t*  cursor;
  uint8_t*  end;
  uint32_t bit_off;
} packet_t;

uint64_t read_bits(packet_t* bs, int n) {
    /* assemble n bits MSB-first from (cursor,bit_off), then advance forward n bits.
       a read past the end yields 0 bits and still advances, so the parser drops via
       its `cursor <= end` check. */
    uint64_t v = 0;
    for (int i = 0; i < n; i++) {
        int b = bs->bit_off + i;
        int in_bounds = (bs->cursor + (b >> 3)) < bs->end;
        v = (v << 1) | (in_bounds ? ((bs->cursor[b >> 3] >> (7 - (b & 7))) & 1) : 0);
    }
    int tot = bs->bit_off + n;
    bs->cursor += tot >> 3;
    bs->bit_off = tot & 7;
    return v;
}

uint64_t peek_bits(packet_t* bs, int n) {
    uint64_t v = 0;
    for (int i = 0; i < n; i++) {
        int b = bs->bit_off + i;
        int in_bounds = (bs->cursor + (b >> 3)) < bs->end;
        v = (v << 1) | (in_bounds ? ((bs->cursor[b >> 3] >> (7 - (b & 7))) & 1) : 0);
    }
    return v;
}

void skip_bits(packet_t* bs, int n) {
    int tot = bs->bit_off + n;
    bs->cursor += tot >> 3;
    bs->bit_off = tot & 7;
}

void write_bits(packet_t* bs, uint64_t v, int n) {
    /* prepend: move the front back n bits, then write n bits MSB-first. clear-then-set
       each bit so a field sharing a boundary byte with an already-written neighbour
       composes correctly without pre-zeroing the buffer. */
    int no = (int)bs->bit_off - n;
    while (no < 0) { no += 8; bs->cursor -= 1; }
    bs->bit_off = no;
    for (int i = 0; i < n; i++) {
        int b = bs->bit_off + i;
        uint8_t m = (uint8_t)(1u << (7 - (b & 7)));
        uint8_t bit = (uint8_t)((v >> (n - 1 - i)) & 1);
        uint8_t* p = &bs->cursor[b >> 3];
        *p = (uint8_t)((*p & (uint8_t)(~m)) | (bit ? m : 0));
    }
}
uint32_t flood(uint32_t port ){ return port + 10000;/* TODO!*/ }
uint32_t hash_32(uint32_t seed , uint8_t*  str , uint32_t len_bits ){
  // len_bits is the value's bit width. Sum the whole bytes, then add the last
  // partial byte masked to its valid low bits (the value is stored little-endian,
  // so the leftover bits live in the low end of the final byte). Placeholder hash
  // (a byte sum) -- collisions are not a concern yet.
  int hashValue = seed;
  uint32_t full_bytes = len_bits / 8;
  for (uint32_t i = 0; i < full_bytes; i++) {
      hashValue += str[i];
  }
  uint32_t rem = len_bits % 8;
  if (rem != 0) {
      hashValue += str[full_bytes] & ((1 << rem) - 1);
  }
  return hashValue;
}
typedef struct {
  uint16_t len;
  uint8_t is_packet;
  uint8_t has_payload;
  uint32_t timestamp;
  uint32_t in_port;
} event_meta;
uint16_t background_tag  = 1;
uint16_t eth_tag  = 2;
uint16_t eth_ip_tag  = 3;
typedef struct {
  uint16_t tag;
  union {
    struct {
      uint16_t x_1625;
    } background_1626;
    struct {
      uint64_t e_0_1627;
      uint64_t e_1_1628;
      uint16_t e_2_1629;
    } eth_1630;
    struct {
      uint64_t e_0_1631;
      uint64_t e_1_1632;
      uint16_t e_2_1633;
      uint32_t ip_0_1634;
      uint32_t ip_1_1635;
      uint32_t ip_2_1636;
      uint32_t ip_3_1637;
    } eth_ip_1639;
  } args;
} event_variant_t;
event_variant_t background_1626(uint16_t x_1625 ){
  event_variant_t ev  = {0};
  ev.args.background_1626.x_1625 = x_1625;
  ev.tag = background_tag;
  return ev;
}
event_variant_t eth_1630(uint64_t e_0_1627 , uint64_t e_1_1628 , uint16_t e_2_1629 ){
  event_variant_t ev  = {0};
  ev.args.eth_1630.e_0_1627 = e_0_1627;
  ev.args.eth_1630.e_1_1628 = e_1_1628;
  ev.args.eth_1630.e_2_1629 = e_2_1629;
  ev.tag = eth_tag;
  return ev;
}
event_variant_t eth_ip_1639(uint64_t e_0_1631 , uint64_t e_1_1632 , uint16_t e_2_1633 , uint32_t ip_0_1634 , uint32_t ip_1_1635 , uint32_t ip_2_1636 , uint32_t ip_3_1637 ){
  event_variant_t ev  = {0};
  ev.args.eth_ip_1639.e_0_1631 = e_0_1631;
  ev.args.eth_ip_1639.e_1_1632 = e_1_1632;
  ev.args.eth_ip_1639.e_2_1633 = e_2_1633;
  ev.args.eth_ip_1639.ip_0_1634 = ip_0_1634;
  ev.args.eth_ip_1639.ip_1_1635 = ip_1_1635;
  ev.args.eth_ip_1639.ip_2_1636 = ip_2_1636;
  ev.args.eth_ip_1639.ip_3_1637 = ip_3_1637;
  ev.tag = eth_ip_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
typedef struct {
  event_t ev;
  uint32_t port;
} out_event_t;
event_t mk_background(uint16_t x_1625 ){
  event_t tmp_1801  = {.meta = {.len = 2, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = background_1626(x_1625)};
  return tmp_1801;
}
event_t mk_eth(uint64_t e_0_1627 , uint64_t e_1_1628 , uint16_t e_2_1629 ){
  event_t tmp_1802  = {.meta = {.len = 14, .is_packet = 1, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = eth_1630(e_0_1627, e_1_1628, e_2_1629)};
  return tmp_1802;
}
event_t mk_eth_ip(uint64_t e_0_1631 , uint64_t e_1_1632 , uint16_t e_2_1633 , uint32_t ip_0_1634 , uint32_t ip_1_1635 , uint32_t ip_2_1636 , uint32_t ip_3_1637 ){
  event_t tmp_1803  = {.meta = {.len = 30, .is_packet = 1, .has_payload = 1, .timestamp = 0, .in_port = 0}, .data = eth_ip_1639(e_0_1631, e_1_1632, e_2_1633, ip_0_1634, ip_1_1635, ip_2_1636, ip_3_1637)};
  return tmp_1803;
}
uint32_t recirculation_port  = 0;
uint32_t self  = 0;
typedef tuple_1 eth_1623;
typedef tuple_2 ip_1624;
uint8_t parse_event(packet_t*  pkt_1650 , event_t*  next_event ){
  uint64_t e_0_1651  = ((uint64_t)(read_bits(pkt_1650, 48))) & 281474976710655;
  uint64_t e_1_1652  = ((uint64_t)(read_bits(pkt_1650, 48))) & 281474976710655;
  uint16_t e_2_1653  = ((uint16_t)(read_bits(pkt_1650, 16)));
  switch (e_2_1653) {
    case 666: {
      uint16_t tag  = ((uint16_t)(read_bits(pkt_1650, 16)));
      switch (tag) {
        case 1: {
          uint16_t x_1625  = ((uint16_t)(read_bits(pkt_1650, 16)));
          (*(next_event)) = mk_background(x_1625);
          return pkt_1650->cursor <= pkt_1650->end;
          break;
        }
        default: {
          return 0;
          break;
        }
      }
      break;
    }
    case 2048: {
      uint32_t ip_0_1644  = ((uint32_t)(read_bits(pkt_1650, 32)));
      uint32_t ip_1_1645  = ((uint32_t)(read_bits(pkt_1650, 32)));
      uint32_t ip_2_1646  = ((uint32_t)(read_bits(pkt_1650, 32)));
      uint32_t ip_3_1647  = ((uint32_t)(read_bits(pkt_1650, 32)));
      
      (*(next_event)) = mk_eth_ip(e_0_1651, e_1_1652, e_2_1653, ip_0_1644, ip_1_1645, ip_2_1646, ip_3_1647);
      return pkt_1650->cursor <= pkt_1650->end;
      break;
    }
    case 0: {
      return 0;
      break;
    }
    default: {
      e_2_1653 = 39321;
      (*(next_event)) = mk_eth(e_0_1651, e_1_1652, e_2_1653);
      return pkt_1650->cursor <= pkt_1650->end;
      break;
    }
  }
  return 0;
}
uint16_t handle_event(event_t*  ev_in , out_event_t out_events [64]){
  uint16_t n  = 0;
  switch (ev_in->data.tag) {
    case 1: {
      uint16_t x_1654  = ev_in->data.args.background_1626.x_1625;
      event_t this  = mk_background(x_1654);
      printf("%d", x_1654);
      break;
    }
    case 2: {
      uint64_t e_0_1655  = ev_in->data.args.eth_1630.e_0_1627;
      uint64_t e_1_1656  = ev_in->data.args.eth_1630.e_1_1628;
      uint16_t e_2_1657  = ev_in->data.args.eth_1630.e_2_1629;
      event_t this  = mk_eth(e_0_1655, e_1_1656, e_2_1657);
      out_event_t tmp_1804  = {.ev = mk_background(e_2_1657), .port = 4294967295};
      out_events[n] = tmp_1804;
      n = n + 1;
      break;
    }
    case 3: {
      uint64_t e_0_1658  = ev_in->data.args.eth_ip_1639.e_0_1631;
      uint64_t e_1_1659  = ev_in->data.args.eth_ip_1639.e_1_1632;
      uint16_t e_2_1660  = ev_in->data.args.eth_ip_1639.e_2_1633;
      uint32_t ip_0_1661  = ev_in->data.args.eth_ip_1639.ip_0_1634;
      uint32_t ip_1_1662  = ev_in->data.args.eth_ip_1639.ip_1_1635;
      uint32_t ip_2_1663  = ev_in->data.args.eth_ip_1639.ip_2_1636;
      uint32_t ip_3_1664  = ev_in->data.args.eth_ip_1639.ip_3_1637;
      event_t this  = mk_eth_ip(e_0_1658, e_1_1659, e_2_1660, ip_0_1661, ip_1_1662, ip_2_1663, ip_3_1664);
      out_event_t tmp_1805  = {.ev = mk_background(e_2_1660), .port = 4294967295};
      out_events[n] = tmp_1805;
      n = n + 1;
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}
void deparse_event(event_t*  ev_out , packet_t*  buf_out ){
  switch (ev_out->data.tag) {
    case 1: {
      uint16_t x_1625  = ev_out->data.args.background_1626.x_1625;
      write_bits(buf_out, ((uint64_t)(x_1625)), 16);
      if ((ev_out->meta.is_packet) == (0)) {
        write_bits(buf_out, ((uint64_t)(1)), 16);
        write_bits(buf_out, ((uint64_t)(666)), 16);
        write_bits(buf_out, ((uint64_t)(2)), 48);
        write_bits(buf_out, ((uint64_t)(1)), 48);
      }
      return ;
      break;
    }
    case 2: {
      uint64_t e_0_1627  = ev_out->data.args.eth_1630.e_0_1627;
      uint64_t e_1_1628  = ev_out->data.args.eth_1630.e_1_1628;
      uint16_t e_2_1629  = ev_out->data.args.eth_1630.e_2_1629;
      write_bits(buf_out, ((uint64_t)(e_2_1629)), 16);
      write_bits(buf_out, ((uint64_t)(e_1_1628)), 48);
      write_bits(buf_out, ((uint64_t)(e_0_1627)), 48);
      if ((ev_out->meta.is_packet) == (0)) {
        write_bits(buf_out, ((uint64_t)(2)), 16);
        write_bits(buf_out, ((uint64_t)(666)), 16);
        write_bits(buf_out, ((uint64_t)(2)), 48);
        write_bits(buf_out, ((uint64_t)(1)), 48);
      }
      return ;
      break;
    }
    case 3: {
      uint64_t e_0_1631  = ev_out->data.args.eth_ip_1639.e_0_1631;
      uint64_t e_1_1632  = ev_out->data.args.eth_ip_1639.e_1_1632;
      uint16_t e_2_1633  = ev_out->data.args.eth_ip_1639.e_2_1633;
      uint32_t ip_0_1634  = ev_out->data.args.eth_ip_1639.ip_0_1634;
      uint32_t ip_1_1635  = ev_out->data.args.eth_ip_1639.ip_1_1635;
      uint32_t ip_2_1636  = ev_out->data.args.eth_ip_1639.ip_2_1636;
      uint32_t ip_3_1637  = ev_out->data.args.eth_ip_1639.ip_3_1637;
      write_bits(buf_out, ((uint64_t)(ip_3_1637)), 32);
      write_bits(buf_out, ((uint64_t)(ip_2_1636)), 32);
      write_bits(buf_out, ((uint64_t)(ip_1_1635)), 32);
      write_bits(buf_out, ((uint64_t)(ip_0_1634)), 32);
      write_bits(buf_out, ((uint64_t)(e_2_1633)), 16);
      write_bits(buf_out, ((uint64_t)(e_1_1632)), 48);
      write_bits(buf_out, ((uint64_t)(e_0_1631)), 48);
      if ((ev_out->meta.is_packet) == (0)) {
        write_bits(buf_out, ((uint64_t)(3)), 16);
        write_bits(buf_out, ((uint64_t)(666)), 16);
        write_bits(buf_out, ((uint64_t)(2)), 48);
        write_bits(buf_out, ((uint64_t)(1)), 48);
      }
      return ;
      break;
    }
  }
}

 void init_cursor(uint8_t*  buf , uint32_t len , packet_t*  bytes ){
    bytes->start = buf;
    bytes->cursor = buf;
    bytes->end = buf + len;
    bytes->bit_off = 0;
 }

 void reset_cursor(packet_t*  bytes){
    bytes->cursor = bytes->start;
    bytes->bit_off = 0;
 }

 void copy_packet(packet_t*  buf_out , packet_t*  buf_in ) {
    memcpy(buf_out->start, buf_in->start, buf_in->end - buf_in->start);
    buf_out->cursor = buf_out->start + (buf_in->cursor - buf_in->start);
    buf_out->end = buf_out->start + (buf_in->end - buf_in->start);
    buf_out->bit_off = 0;
 }

/********* the queue element + the dispatch FIFO ***********/
// The queue element mirrors the DPDK driver's (§28): an event plus where its payload
// begins. The difference is ownership -- DPDK's element is an mbuf that OWNS its bytes,
// whereas here the bytes live in the single input buffer (ctx->in_pkt) shared for the
// whole synchronous drain, and the element just records the payload boundary into it.
// packet_t is a pure view over that buffer. (No out_events list rides in the element:
// dispatch hands the handler's outputs straight to do_tx, so it stays a local.)
typedef struct { event_t ev; uint32_t payload_off; } qe_t;

#define EV_QUEUE_CAP 1024
typedef struct ev_queue_t {
    qe_t buf[EV_QUEUE_CAP];
    int head;
    int tail;
    int count;
} ev_queue_t;

static void evq_init(ev_queue_t* q) { q->head = 0; q->tail = 0; q->count = 0; }
static int  evq_empty(ev_queue_t* q) { return q->count == 0; }
static void evq_push(ev_queue_t* q, qe_t* e) {
    // no overflow guard (see handler lowering); EV_QUEUE_CAP is generous.
    q->buf[q->tail] = *e;
    q->tail = (q->tail + 1) % EV_QUEUE_CAP;
    q->count++;
}
static void evq_pull(ev_queue_t* q, qe_t* out) {
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
    const struct pcap_pkthdr *in_pkthdr; // input header of the packet being handled (ts source for tx)
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

// The dispatch pipeline is split into three phases with the same responsibilities as
// the DPDK reference (§28): rx parses into a queue element; dispatch handles it and
// hands the outputs to tx; tx fans out (recirc -> re-queue, egress -> deparse + dump).
// The divergence is that here they run synchronously per input packet -- there is no
// live competing input (pcap replays an offline file), so the queue is drained to empty,
// which keeps the shared input buffer valid for the whole drain and lets every output
// (including from recirculated events) build over it. No mbuf pool: the "queue element"
// owns nothing; the input buffer is the owner and packet_t is a view over it.

/******** TX: fan out the handler's outputs -- recirc back to the queue, egress deparse+dump ********/
static void do_tx(pkt_hdl_ctx_t *ctx, out_event_t *out_events, uint16_t n, uint32_t payload_off) {
    for (uint16_t i = 0; i < n; i++) {
        out_event_t *oe = &out_events[i];
        if (oe->port == 4294967295u) {          // recirculation: re-inject (inherits the boundary)
            qe_t re = { oe->ev, payload_off };
            evq_push(&ctx->queue, &re);
            continue;
        }
        // egress: build the output over a copy of the shared input buffer, deparse, dump.
        // (pcap has a single output file, so oe->port is recorded but every port event
        // goes to the same dump.)
        reset_cursor(&ctx->out_pkt);
        copy_packet(&ctx->out_pkt, &ctx->in_pkt);
        // set the cursor to the payload boundary (from the element); deparse prepends the
        // header before it. A no-payload event emits only its header (drop the input tail);
        // a payload event keeps the tail. (matches interp)
        ctx->out_pkt.cursor = ctx->out_pkt.start + payload_off;
        uint8_t *payload_boundary = ctx->out_pkt.cursor;
        deparse_event(&oe->ev, &ctx->out_pkt);
        uint8_t *dump_end = oe->ev.meta.has_payload ? ctx->out_pkt.end : payload_boundary;
        fill_out_pkthdr(ctx->in_pkthdr, &ctx->out_pkt, dump_end, &ctx->out_pkthdr);
        pcap_dump((u_char *)ctx->out_pcap, &ctx->out_pkthdr, (u_char *)ctx->out_pkt.cursor);
    }
}

/******** DISPATCH: drain the queue; handle each element and hand its outputs to tx ********/
static void do_dispatch(pkt_hdl_ctx_t *ctx) {
    while (!evq_empty(&ctx->queue)) {
        qe_t qe;
        evq_pull(&ctx->queue, &qe);
        qe.ev.meta.timestamp = now_ns();        // stamp at dequeue (covers arriving + recirculated events)
        qe.ev.meta.in_port = ctx->ingress_port; // ingress port (read by the handler)
        out_event_t out_events[64];
        uint16_t n = handle_event(&qe.ev, out_events);
        do_tx(ctx, out_events, n, qe.payload_off); // fan out this element's outputs
    }
}

/******** RX: parse the input packet into a queue element and enqueue it (pcap_loop callback) ********/
void lpcap_packet_handler(u_char *raw_ctx, const struct pcap_pkthdr *pkthdr, const u_char *packet) {
    pkt_hdl_ctx_t *ctx = (pkt_hdl_ctx_t *)raw_ctx;
    init_cursor((uint8_t *)packet, pkthdr->len, &ctx->in_pkt); // construct a new cursor over the input
    ctx->in_pkthdr = pkthdr;                                   // remembered for do_tx (ts source)
    qe_t qe;
    if (parse_event(&ctx->in_pkt, &qe.ev) != 1) {
        debug_printf("parse failed!\n");
        return; // drop
    }
    qe.payload_off = (uint32_t)(ctx->in_pkt.cursor - ctx->in_pkt.start); // where the payload begins
    evq_push(&ctx->queue, &qe);
    do_dispatch(ctx); // drain: handle + fan out this packet's events (and any it recirculates)
    pkt_ct++;
}

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
    .in_pkthdr = NULL,
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
}