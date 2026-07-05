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
  uint32_t _0;
  uint8_t _1;
} tuple_1;
typedef struct {
  uint8_t*  start;
  uint8_t*  cursor;
  uint8_t*  end;
  uint32_t bit_off;
  uint8_t*  driver_buf;
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
typedef enum {tag_hit_acn_2744 = 0, tag_miss_acn_2747 = 1} action_enum;
typedef struct {
  uint16_t len;
  uint8_t is_packet;
  uint8_t has_payload;
  uint32_t timestamp;
  uint32_t in_port;
  packet_t payload;
} event_meta;
uint16_t do_match_tag  = 1;
uint16_t do_install_tag  = 2;
typedef struct {
  uint16_t tag;
  union {
    struct {
      uint32_t s_2749;
    } do_match_2750;
    struct {
      uint32_t v_2751;
      uint32_t m_2752;
    } do_install_2753;
  } payload;
} event_variant;
event_variant do_match_2750(uint32_t s_2749 ){
  event_variant ev  = {0};
  ev.payload.do_match_2750.s_2749 = s_2749;
  ev.tag = do_match_tag;
  return ev;
}
event_variant do_install_2753(uint32_t v_2751 , uint32_t m_2752 ){
  event_variant ev  = {0};
  ev.payload.do_install_2753.v_2751 = v_2751;
  ev.payload.do_install_2753.m_2752 = m_2752;
  ev.tag = do_install_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant data;
} events;
typedef struct {
  events ev;
  uint8_t out_loc;
  uint32_t port;
} out_event;
events mk_do_match(uint32_t s_2749 ){
  events tmp_2954  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_match_2750(s_2749)};
  return tmp_2954;
}
events mk_do_install(uint32_t v_2751 , uint32_t m_2752 ){
  events tmp_2955  = {.meta = {.len = 8, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_install_2753(v_2751, m_2752)};
  return tmp_2955;
}
uint32_t recirculation_port  = 0;
uint32_t self  = 0;
typedef tuple_1 res_t_2741;
res_t_2741 hit_acn_2744(uint32_t x_2742 , uint32_t a_2743 ){
  res_t_2741 tmp_2956  = {._0 = x_2742, ._1 = true,};
  return tmp_2956;
}
res_t_2741 miss_acn_2747(uint32_t b_2745 , uint32_t a_2746 ){
  res_t_2741 tmp_2957  = {._0 = a_2746, ._1 = false,};
  return tmp_2957;
}
typedef struct {
  uint8_t valid;
  uint32_t key;
  uint32_t mask;
  action_enum action_tag;
  uint32_t action_arg;
} cellty_ftbl_2748;
typedef struct {
  struct {
    action_enum action_tag;
    uint32_t action_arg;
  } _default;
  cellty_ftbl_2748 entries [1024];
} ty_ftbl_2748;
ty_ftbl_2748 ftbl_2748  = {._default = {.action_tag = tag_miss_acn_2747, .action_arg = 1}, .entries = {0}};
void install_ftbl_2748(uint32_t key , action_enum action , uint32_t const_arg ){
  bool _continue = true;
  for (int _idx = 0; _idx < 1024; _idx++) {
    if ((ftbl_2748.entries[_idx].valid) == (false)) {
      _continue = false;
      cellty_ftbl_2748 tmp_2958  = {.valid = true, .key = key, .mask = key, .action_tag = action, .action_arg = const_arg};
      ftbl_2748.entries[_idx] = tmp_2958;
    }  if (!_continue) break;
    
  }
  return ;
}
void install_ternary_ftbl_2748(uint32_t key , uint32_t mask , action_enum action , uint32_t const_arg ){
  bool _continue = true;
  for (int _idx = 0; _idx < 1024; _idx++) {
    if ((ftbl_2748.entries[_idx].valid) == (false)) {
      _continue = false;
      cellty_ftbl_2748 tmp_2959  = {.valid = true, .key = key, .mask = mask, .action_tag = action, .action_arg = const_arg};
      ftbl_2748.entries[_idx] = tmp_2959;
    }  if (!_continue) break;
    
  }
  return ;
}
res_t_2741 lookup_ftbl_2748(uint32_t key , uint32_t arg ){
  res_t_2741 rv  = {._0 = 0, ._1 = false};
  switch (ftbl_2748._default.action_tag) {
    case tag_hit_acn_2744: {
      rv = hit_acn_2744(ftbl_2748._default.action_arg, arg);
      break;
    }
    case tag_miss_acn_2747: {
      rv = miss_acn_2747(ftbl_2748._default.action_arg, arg);
      break;
    }
  }
  bool _continue = true;
  for (int _idx = 0; _idx < 1024; _idx++) {
    switch (ftbl_2748.entries[_idx].action_tag) {
      case tag_hit_acn_2744: {
        if ((key & ftbl_2748.entries[_idx].mask) == (ftbl_2748.entries[_idx].key & ftbl_2748.entries[_idx].mask)) {
          rv = hit_acn_2744(ftbl_2748.entries[_idx].action_arg, arg);
          _continue = false;
        }
        break;
      }
      case tag_miss_acn_2747: {
        if ((key & ftbl_2748.entries[_idx].mask) == (ftbl_2748.entries[_idx].key & ftbl_2748.entries[_idx].mask)) {
          rv = miss_acn_2747(ftbl_2748.entries[_idx].action_arg, arg);
          _continue = false;
        }
        break;
      }
    }  if (!_continue) break;
    
  }
  return rv;
}
uint16_t handle_event(events*  ev_in , out_event out_events [64]){
  uint16_t n  = 0;
  switch (ev_in->data.tag) {
    case 1: {
      uint32_t s_2754  = ev_in->data.payload.do_match_2750.s_2749;
      events this  = mk_do_match(s_2754);
      res_t_2741 tup_2953  = lookup_ftbl_2748(s_2754, 1000);
      uint32_t tbl_out_0_2756  = tup_2953._0;
      uint8_t tbl_out_1_2757  = tup_2953._1;
      printf("return of table match: %d", tbl_out_0_2756);
      if (tbl_out_1_2757 == true) {
        printf("table HIT.");
      }else {
        printf("table MISS. Installing hit_acn(3) for current key. ");
        install_ftbl_2748(s_2754, tag_hit_acn_2744, 3);
      }
      break;
    }
    case 2: {
      uint32_t v_2758  = ev_in->data.payload.do_install_2753.v_2751;
      uint32_t m_2759  = ev_in->data.payload.do_install_2753.m_2752;
      events this  = mk_do_install(v_2758, m_2759);
      printf("installing entry: (%d &&& %d)", v_2758, m_2759);
      install_ftbl_2748(v_2758, tag_hit_acn_2744, 2);
      install_ternary_ftbl_2748(v_2758, m_2759, tag_hit_acn_2744, 3);
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}
uint8_t parse_event(packet_t*  packet , events*  next_event ){
  skip_bits(packet, 32);
  skip_bits(packet, 16);
  skip_bits(packet, 32);
  skip_bits(packet, 16);
  uint16_t ety  = ((uint16_t)(read_bits(packet, 16)));
  switch (ety) {
    case 666: {
      uint16_t tag  = ((uint16_t)(read_bits(packet, 16)));
      switch (tag) {
        case 1: {
          uint32_t s_2749  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_do_match(s_2749);
          return packet->cursor <= packet->end;
          break;
        }
        case 2: {
          uint32_t v_2751  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t m_2752  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_do_install(v_2751, m_2752);
          return packet->cursor <= packet->end;
          break;
        }
        default: {
          return 0;
          break;
        }
      }
      break;
    }
    default: {
      return 0;
      break;
    }
  }
  return 0;
}
void deparse_event(events*  ev_out , packet_t*  buf_out ){
  switch (ev_out->data.tag) {
    case 1: {
      uint32_t s_2749  = ev_out->data.payload.do_match_2750.s_2749;
      write_bits(buf_out, ((uint64_t)(s_2749)), 32);
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
      uint32_t v_2751  = ev_out->data.payload.do_install_2753.v_2751;
      uint32_t m_2752  = ev_out->data.payload.do_install_2753.m_2752;
      write_bits(buf_out, ((uint64_t)(m_2752)), 32);
      write_bits(buf_out, ((uint64_t)(v_2751)), 32);
      if ((ev_out->meta.is_packet) == (0)) {
        write_bits(buf_out, ((uint64_t)(2)), 16);
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
        out_event out_events[64];
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