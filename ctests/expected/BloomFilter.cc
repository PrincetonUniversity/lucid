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
typedef struct {
  uint16_t len;
  uint8_t is_packet;
  uint8_t has_payload;
  uint32_t timestamp;
  uint32_t in_port;
  packet_t payload;
} event_meta;
uint16_t BloomFilter_clear_all_bf1_tag  = 1;
uint16_t BloomFilter_clear_helper_bf1_tag  = 2;
uint16_t allowed_tag  = 3;
uint16_t denied_tag  = 4;
uint16_t in_tag  = 5;
typedef struct {
  uint16_t tag;
  union {
    struct {
      
    } BloomFilter_clear_all_bf1_5866;
    struct {
      uint8_t idx_5867;
    } BloomFilter_clear_helper_bf1_5868;
    struct {
      uint32_t x_5873;
    } allowed_5874;
    struct {
      uint32_t x_5875;
    } denied_5876;
    struct {
      uint8_t add_5877;
      uint8_t pad_5878;
      uint32_t x_5879;
    } in_5880;
  } payload;
} event_variant;
event_variant BloomFilter_clear_all_bf1_5866(){
  event_variant ev  = {0};
  
  ev.tag = BloomFilter_clear_all_bf1_tag;
  return ev;
}
event_variant BloomFilter_clear_helper_bf1_5868(uint8_t idx_5867 ){
  event_variant ev  = {0};
  ev.payload.BloomFilter_clear_helper_bf1_5868.idx_5867 = idx_5867;
  ev.tag = BloomFilter_clear_helper_bf1_tag;
  return ev;
}
event_variant allowed_5874(uint32_t x_5873 ){
  event_variant ev  = {0};
  ev.payload.allowed_5874.x_5873 = x_5873;
  ev.tag = allowed_tag;
  return ev;
}
event_variant denied_5876(uint32_t x_5875 ){
  event_variant ev  = {0};
  ev.payload.denied_5876.x_5875 = x_5875;
  ev.tag = denied_tag;
  return ev;
}
event_variant in_5880(uint8_t add_5877 , uint8_t pad_5878 , uint32_t x_5879 ){
  event_variant ev  = {0};
  ev.payload.in_5880.add_5877 = add_5877;
  ev.payload.in_5880.pad_5878 = pad_5878;
  ev.payload.in_5880.x_5879 = x_5879;
  ev.tag = in_tag;
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
events mk_BloomFilter_clear_all_bf1(){
  events tmp_6331  = {.meta = {.len = 0, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = BloomFilter_clear_all_bf1_5866()};
  return tmp_6331;
}
events mk_BloomFilter_clear_helper_bf1(uint8_t idx_5867 ){
  events tmp_6332  = {.meta = {.len = 1, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = BloomFilter_clear_helper_bf1_5868(idx_5867)};
  return tmp_6332;
}
events mk_allowed(uint32_t x_5873 ){
  events tmp_6333  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = allowed_5874(x_5873)};
  return tmp_6333;
}
events mk_denied(uint32_t x_5875 ){
  events tmp_6334  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = denied_5876(x_5875)};
  return tmp_6334;
}
events mk_in(uint8_t add_5877 , uint8_t pad_5878 , uint32_t x_5879 ){
  events tmp_6335  = {.meta = {.len = 6, .is_packet = 1, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = in_5880(add_5877, pad_5878, x_5879)};
  return tmp_6335;
}
uint32_t recirculation_port  = 0;
uint32_t self  = 0;
uint8_t bf1_0_0_5869 [256] = {0};
uint8_t Array_update_complex_bf1_0_0_5869_set_set_memop_8_bit(uint8_t _idx , uint8_t new_val_set_memop_8_bit , uint8_t unused_set_memop_8_bit ){
  uint8_t cell1  = bf1_0_0_5869[(((uint32_t)(_idx)) % 256)];
  uint8_t cell2  = 0;
  uint8_t ret  = 0;
  if (true) {
    cell1 = new_val_set_memop_8_bit;
  }
  bf1_0_0_5869[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint8_t Array_update_complex_bf1_0_0_5869_get_get_memop_8_bit(uint8_t _idx , uint8_t unused1_get_memop_8_bit , uint8_t unused2_get_memop_8_bit ){
  uint8_t cell1  = bf1_0_0_5869[(((uint32_t)(_idx)) % 256)];
  uint8_t cell2  = 0;
  uint8_t ret  = 0;
  if (true) {
    ret = bf1_0_0_5869[(((uint32_t)(_idx)) % 256)];
  }
  bf1_0_0_5869[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint8_t bf1_0_1_5870 [256] = {0};
uint8_t Array_update_complex_bf1_0_1_5870_set_set_memop_8_bit(uint8_t _idx , uint8_t new_val_set_memop_8_bit , uint8_t unused_set_memop_8_bit ){
  uint8_t cell1  = bf1_0_1_5870[(((uint32_t)(_idx)) % 256)];
  uint8_t cell2  = 0;
  uint8_t ret  = 0;
  if (true) {
    cell1 = new_val_set_memop_8_bit;
  }
  bf1_0_1_5870[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint8_t Array_update_complex_bf1_0_1_5870_get_get_memop_8_bit(uint8_t _idx , uint8_t unused1_get_memop_8_bit , uint8_t unused2_get_memop_8_bit ){
  uint8_t cell1  = bf1_0_1_5870[(((uint32_t)(_idx)) % 256)];
  uint8_t cell2  = 0;
  uint8_t ret  = 0;
  if (true) {
    ret = bf1_0_1_5870[(((uint32_t)(_idx)) % 256)];
  }
  bf1_0_1_5870[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint8_t bf1_0_2_5871 [256] = {0};
uint8_t Array_update_complex_bf1_0_2_5871_set_set_memop_8_bit(uint8_t _idx , uint8_t new_val_set_memop_8_bit , uint8_t unused_set_memop_8_bit ){
  uint8_t cell1  = bf1_0_2_5871[(((uint32_t)(_idx)) % 256)];
  uint8_t cell2  = 0;
  uint8_t ret  = 0;
  if (true) {
    cell1 = new_val_set_memop_8_bit;
  }
  bf1_0_2_5871[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint8_t Array_update_complex_bf1_0_2_5871_get_get_memop_8_bit(uint8_t _idx , uint8_t unused1_get_memop_8_bit , uint8_t unused2_get_memop_8_bit ){
  uint8_t cell1  = bf1_0_2_5871[(((uint32_t)(_idx)) % 256)];
  uint8_t cell2  = 0;
  uint8_t ret  = 0;
  if (true) {
    ret = bf1_0_2_5871[(((uint32_t)(_idx)) % 256)];
  }
  bf1_0_2_5871[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint8_t bf1_0_3_5872 [256] = {0};
uint8_t Array_update_complex_bf1_0_3_5872_set_set_memop_8_bit(uint8_t _idx , uint8_t new_val_set_memop_8_bit , uint8_t unused_set_memop_8_bit ){
  uint8_t cell1  = bf1_0_3_5872[(((uint32_t)(_idx)) % 256)];
  uint8_t cell2  = 0;
  uint8_t ret  = 0;
  if (true) {
    cell1 = new_val_set_memop_8_bit;
  }
  bf1_0_3_5872[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint8_t Array_update_complex_bf1_0_3_5872_get_get_memop_8_bit(uint8_t _idx , uint8_t unused1_get_memop_8_bit , uint8_t unused2_get_memop_8_bit ){
  uint8_t cell1  = bf1_0_3_5872[(((uint32_t)(_idx)) % 256)];
  uint8_t cell2  = 0;
  uint8_t ret  = 0;
  if (true) {
    ret = bf1_0_3_5872[(((uint32_t)(_idx)) % 256)];
  }
  bf1_0_3_5872[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint8_t parse_event(packet_t*  pkt , events*  next_event ){
  uint8_t add_5877  = ((uint8_t)(read_bits(pkt, 1)));
  uint8_t pad_5878  = ((uint8_t)(read_bits(pkt, 7))) & 127;
  uint32_t x_5879  = ((uint32_t)(read_bits(pkt, 32)));
  (*(next_event)) = mk_in(add_5877, pad_5878, x_5879);
  return pkt->cursor <= pkt->end;
}
uint16_t handle_event(events*  ev_in , out_event out_events [64]){
  uint16_t n  = 0;
  switch (ev_in->data.tag) {
    case 1: {
      
      events this  = mk_BloomFilter_clear_all_bf1();
      out_event tmp_6336  = {.ev = mk_BloomFilter_clear_helper_bf1(0), .out_loc = 1, .port = 0};
      out_events[n] = tmp_6336;
      n = n + 1;
      break;
    }
    case 2: {
      uint8_t idx_5881  = ev_in->data.payload.BloomFilter_clear_helper_bf1_5868.idx_5867;
      events this  = mk_BloomFilter_clear_helper_bf1(idx_5881);
      Array_update_complex_bf1_0_0_5869_set_set_memop_8_bit(idx_5881, 0, 0);
      Array_update_complex_bf1_0_1_5870_set_set_memop_8_bit(idx_5881, 0, 0);
      Array_update_complex_bf1_0_2_5871_set_set_memop_8_bit(idx_5881, 0, 0);
      Array_update_complex_bf1_0_3_5872_set_set_memop_8_bit(idx_5881, 0, 0);
      false;
      if (idx_5881 < 255) {
        out_event tmp_6337  = {.ev = mk_BloomFilter_clear_helper_bf1(idx_5881 + 1), .out_loc = 1, .port = 0};
        out_events[n] = tmp_6337;
        n = n + 1;
      }
      break;
    }
    case 3: {
      uint32_t x_5882  = ev_in->data.payload.allowed_5874.x_5873;
      events this  = mk_allowed(x_5882);
      break;
    }
    case 4: {
      uint32_t x_5883  = ev_in->data.payload.denied_5876.x_5875;
      events this  = mk_denied(x_5883);
      break;
    }
    case 5: {
      uint8_t add_5884  = ev_in->data.payload.in_5880.add_5877;
      uint8_t pad_5885  = ev_in->data.payload.in_5880.pad_5878;
      uint32_t x_5886  = ev_in->data.payload.in_5880.x_5879;
      events this  = mk_in(add_5884, pad_5885, x_5886);
      if (add_5884) {
        Array_update_complex_bf1_0_0_5869_set_set_memop_8_bit(hash_32((uint32_t)0, (uint8_t* )&x_5886, 32), 1, 0);
        Array_update_complex_bf1_0_1_5870_set_set_memop_8_bit(hash_32((uint32_t)1, (uint8_t* )&x_5886, 32), 1, 0);
        Array_update_complex_bf1_0_2_5871_set_set_memop_8_bit(hash_32((uint32_t)2, (uint8_t* )&x_5886, 32), 1, 0);
        Array_update_complex_bf1_0_3_5872_set_set_memop_8_bit(hash_32((uint32_t)3, (uint8_t* )&x_5886, 32), 1, 0);
        false;
      }else {
        uint8_t x_5889  = Array_update_complex_bf1_0_0_5869_get_get_memop_8_bit(hash_32((uint32_t)0, (uint8_t* )&x_5886, 32), 0, 0);
        uint8_t x_5890  = Array_update_complex_bf1_0_1_5870_get_get_memop_8_bit(hash_32((uint32_t)1, (uint8_t* )&x_5886, 32), 0, 0);
        uint8_t x_5891  = Array_update_complex_bf1_0_2_5871_get_get_memop_8_bit(hash_32((uint32_t)2, (uint8_t* )&x_5886, 32), 0, 0);
        uint8_t x_5892  = Array_update_complex_bf1_0_3_5872_get_get_memop_8_bit(hash_32((uint32_t)3, (uint8_t* )&x_5886, 32), 0, 0);
        if ((((x_5889 == 1) && (x_5890 == 1)) && (x_5891 == 1)) && (x_5892 == 1)) {
          out_event tmp_6338  = {.ev = mk_allowed(x_5886), .out_loc = 1, .port = 0};
          out_events[n] = tmp_6338;
          n = n + 1;
        }else {
          out_event tmp_6339  = {.ev = mk_denied(x_5886), .out_loc = 1, .port = 0};
          out_events[n] = tmp_6339;
          n = n + 1;
        }
      }
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}
void deparse_event(events*  ev_out , packet_t*  buf_out ){
  switch (ev_out->data.tag) {
    case 1: {
      
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
      uint8_t idx_5867  = ev_out->data.payload.BloomFilter_clear_helper_bf1_5868.idx_5867;
      write_bits(buf_out, ((uint64_t)(idx_5867)), 8);
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
      uint32_t x_5873  = ev_out->data.payload.allowed_5874.x_5873;
      write_bits(buf_out, ((uint64_t)(x_5873)), 32);
      if ((ev_out->meta.is_packet) == (0)) {
        write_bits(buf_out, ((uint64_t)(3)), 16);
        write_bits(buf_out, ((uint64_t)(666)), 16);
        write_bits(buf_out, ((uint64_t)(2)), 48);
        write_bits(buf_out, ((uint64_t)(1)), 48);
      }
      return ;
      break;
    }
    case 4: {
      uint32_t x_5875  = ev_out->data.payload.denied_5876.x_5875;
      write_bits(buf_out, ((uint64_t)(x_5875)), 32);
      if ((ev_out->meta.is_packet) == (0)) {
        write_bits(buf_out, ((uint64_t)(4)), 16);
        write_bits(buf_out, ((uint64_t)(666)), 16);
        write_bits(buf_out, ((uint64_t)(2)), 48);
        write_bits(buf_out, ((uint64_t)(1)), 48);
      }
      return ;
      break;
    }
    case 5: {
      uint8_t add_5877  = ev_out->data.payload.in_5880.add_5877;
      uint8_t pad_5878  = ev_out->data.payload.in_5880.pad_5878;
      uint32_t x_5879  = ev_out->data.payload.in_5880.x_5879;
      write_bits(buf_out, ((uint64_t)(x_5879)), 32);
      write_bits(buf_out, ((uint64_t)(pad_5878)), 7);
      write_bits(buf_out, ((uint64_t)(add_5877)), 1);
      if ((ev_out->meta.is_packet) == (0)) {
        write_bits(buf_out, ((uint64_t)(5)), 16);
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