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
  uint32_t _1;
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
typedef struct {
  uint16_t len;
  uint8_t is_packet;
  uint8_t has_payload;
  uint32_t timestamp;
  uint32_t in_port;
  packet_t payload;
} event_meta;
uint16_t inside_packet_tag  = 1;
uint16_t inside_continue_tag  = 2;
uint16_t outside_packet_tag  = 3;
uint16_t outside_continue_tag  = 4;
uint16_t add_to_nat_tag  = 5;
typedef struct {
  uint16_t tag;
  union {
    struct {
      uint32_t src_ip_4037;
      uint32_t src_port_4038;
    } inside_packet_4039;
    struct {
      uint32_t src_port_4040;
    } inside_continue_4041;
    struct {
      uint32_t dst_port_4042;
    } outside_packet_4043;
    struct {
      uint32_t dst_ip_4044;
      uint32_t dst_port_4045;
    } outside_continue_4046;
    struct {
      uint32_t src_ip_4047;
      uint32_t src_port_4048;
    } add_to_nat_4049;
  } args;
} event_variant_t;
event_variant_t inside_packet_4039(uint32_t src_ip_4037 , uint32_t src_port_4038 ){
  event_variant_t ev  = {0};
  ev.args.inside_packet_4039.src_ip_4037 = src_ip_4037;
  ev.args.inside_packet_4039.src_port_4038 = src_port_4038;
  ev.tag = inside_packet_tag;
  return ev;
}
event_variant_t inside_continue_4041(uint32_t src_port_4040 ){
  event_variant_t ev  = {0};
  ev.args.inside_continue_4041.src_port_4040 = src_port_4040;
  ev.tag = inside_continue_tag;
  return ev;
}
event_variant_t outside_packet_4043(uint32_t dst_port_4042 ){
  event_variant_t ev  = {0};
  ev.args.outside_packet_4043.dst_port_4042 = dst_port_4042;
  ev.tag = outside_packet_tag;
  return ev;
}
event_variant_t outside_continue_4046(uint32_t dst_ip_4044 , uint32_t dst_port_4045 ){
  event_variant_t ev  = {0};
  ev.args.outside_continue_4046.dst_ip_4044 = dst_ip_4044;
  ev.args.outside_continue_4046.dst_port_4045 = dst_port_4045;
  ev.tag = outside_continue_tag;
  return ev;
}
event_variant_t add_to_nat_4049(uint32_t src_ip_4047 , uint32_t src_port_4048 ){
  event_variant_t ev  = {0};
  ev.args.add_to_nat_4049.src_ip_4047 = src_ip_4047;
  ev.args.add_to_nat_4049.src_port_4048 = src_port_4048;
  ev.tag = add_to_nat_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
typedef struct {
  event_t ev;
  uint8_t out_loc;
  uint32_t port;
} out_event_t;
event_t mk_inside_packet(uint32_t src_ip_4037 , uint32_t src_port_4038 ){
  event_t tmp_4331  = {.meta = {.len = 8, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = inside_packet_4039(src_ip_4037, src_port_4038)};
  return tmp_4331;
}
event_t mk_inside_continue(uint32_t src_port_4040 ){
  event_t tmp_4332  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = inside_continue_4041(src_port_4040)};
  return tmp_4332;
}
event_t mk_outside_packet(uint32_t dst_port_4042 ){
  event_t tmp_4333  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = outside_packet_4043(dst_port_4042)};
  return tmp_4333;
}
event_t mk_outside_continue(uint32_t dst_ip_4044 , uint32_t dst_port_4045 ){
  event_t tmp_4334  = {.meta = {.len = 8, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = outside_continue_4046(dst_ip_4044, dst_port_4045)};
  return tmp_4334;
}
event_t mk_add_to_nat(uint32_t src_ip_4047 , uint32_t src_port_4048 ){
  event_t tmp_4335  = {.meta = {.len = 8, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = add_to_nat_4049(src_ip_4047, src_port_4048)};
  return tmp_4335;
}
uint32_t recirculation_port  = 0;
uint32_t self  = 0;
uint32_t nat_to_ip_4035 [16] = {0};
uint32_t Array_update_complex_nat_to_ip_4035_set_set_memop_32_bit(uint32_t _idx , uint32_t new_val_set_memop_32_bit , uint32_t unused_set_memop_32_bit ){
  uint32_t cell1  = nat_to_ip_4035[(((uint32_t)(_idx)) % 16)];
  uint32_t cell2  = 0;
  uint32_t ret  = 0;
  if (true) {
    cell1 = new_val_set_memop_32_bit;
  }
  nat_to_ip_4035[(((uint32_t)(_idx)) % 16)] = cell1;
  return ret;
}
uint32_t Array_update_complex_nat_to_ip_4035_get_get_memop_32_bit(uint32_t _idx , uint32_t unused1_get_memop_32_bit , uint32_t unused2_get_memop_32_bit ){
  uint32_t cell1  = nat_to_ip_4035[(((uint32_t)(_idx)) % 16)];
  uint32_t cell2  = 0;
  uint32_t ret  = 0;
  if (true) {
    ret = nat_to_ip_4035[(((uint32_t)(_idx)) % 16)];
  }
  nat_to_ip_4035[(((uint32_t)(_idx)) % 16)] = cell1;
  return ret;
}
uint32_t nat_to_port_4036 [16] = {0};
uint32_t Array_update_complex_nat_to_port_4036_set_set_memop_32_bit(uint32_t _idx , uint32_t new_val_set_memop_32_bit , uint32_t unused_set_memop_32_bit ){
  uint32_t cell1  = nat_to_port_4036[(((uint32_t)(_idx)) % 16)];
  uint32_t cell2  = 0;
  uint32_t ret  = 0;
  if (true) {
    cell1 = new_val_set_memop_32_bit;
  }
  nat_to_port_4036[(((uint32_t)(_idx)) % 16)] = cell1;
  return ret;
}
uint32_t Array_update_complex_nat_to_port_4036_get_get_memop_32_bit(uint32_t _idx , uint32_t unused1_get_memop_32_bit , uint32_t unused2_get_memop_32_bit ){
  uint32_t cell1  = nat_to_port_4036[(((uint32_t)(_idx)) % 16)];
  uint32_t cell2  = 0;
  uint32_t ret  = 0;
  if (true) {
    ret = nat_to_port_4036[(((uint32_t)(_idx)) % 16)];
  }
  nat_to_port_4036[(((uint32_t)(_idx)) % 16)] = cell1;
  return ret;
}
uint16_t handle_event(event_t*  ev_in , out_event_t out_events [64]){
  uint16_t n  = 0;
  switch (ev_in->data.tag) {
    case 2: {
      uint32_t src_port_4050  = ev_in->data.args.inside_continue_4041.src_port_4040;
      event_t this  = mk_inside_continue(src_port_4050);
      break;
    }
    case 4: {
      uint32_t dst_ip_4051  = ev_in->data.args.outside_continue_4046.dst_ip_4044;
      uint32_t dst_port_4052  = ev_in->data.args.outside_continue_4046.dst_port_4045;
      event_t this  = mk_outside_continue(dst_ip_4051, dst_port_4052);
      break;
    }
    case 5: {
      uint32_t src_ip_4053  = ev_in->data.args.add_to_nat_4049.src_ip_4047;
      uint32_t src_port_4054  = ev_in->data.args.add_to_nat_4049.src_port_4048;
      event_t this  = mk_add_to_nat(src_ip_4053, src_port_4054);
      tuple_1 tmp_4336  = {._0 = src_ip_4053, ._1 = src_port_4054,};
      uint32_t NAT_port_4055  = ((uint32_t)(hash_32((uint32_t)1234, (uint8_t* )&tmp_4336, 64) & 15));
      printf("Mapped (ip: %d, port: %d) to port %d", src_ip_4053, src_port_4054, NAT_port_4055);
      Array_update_complex_nat_to_ip_4035_set_set_memop_32_bit(NAT_port_4055, src_ip_4053, 0);
      Array_update_complex_nat_to_port_4036_set_set_memop_32_bit(NAT_port_4055, src_port_4054, 0);
      out_event_t tmp_4337  = {.ev = mk_inside_continue(NAT_port_4055), .out_loc = 1, .port = 0};
      out_events[n] = tmp_4337;
      n = n + 1;
      break;
    }
    case 1: {
      uint32_t src_ip_4056  = ev_in->data.args.inside_packet_4039.src_ip_4037;
      uint32_t src_port_4057  = ev_in->data.args.inside_packet_4039.src_port_4038;
      event_t this  = mk_inside_packet(src_ip_4056, src_port_4057);
      uint8_t ret_4059  = false;
      tuple_1 tmp_4338  = {._0 = src_ip_4056, ._1 = src_port_4057,};
      uint32_t idx_4060  = ((uint32_t)(hash_32((uint32_t)1234, (uint8_t* )&tmp_4338, 64) & 15));
      uint32_t ip_4061  = Array_update_complex_nat_to_ip_4035_get_get_memop_32_bit(idx_4060, 0, 0);
      uint32_t port_4062  = Array_update_complex_nat_to_port_4036_get_get_memop_32_bit(idx_4060, 0, 0);
      if (ip_4061 == src_ip_4056) {
        if (port_4062 == src_port_4057) {
          ret_4059 = true;
        }
      }
      if (ret_4059 == true) {
        tuple_1 tmp_4339  = {._0 = src_ip_4056, ._1 = src_port_4057,};
        uint32_t NAT_port_4064  = ((uint32_t)(hash_32((uint32_t)1234, (uint8_t* )&tmp_4339, 64) & 15));
        printf("IP already in NAT, maps to port %d", NAT_port_4064);
        out_event_t tmp_4340  = {.ev = mk_inside_continue(NAT_port_4064), .out_loc = 1, .port = 0};
        out_events[n] = tmp_4340;
        n = n + 1;
      }else {
        printf("Adding to NAT");
        out_event_t tmp_4341  = {.ev = mk_add_to_nat(src_ip_4056, src_port_4057), .out_loc = 1, .port = 0};
        out_events[n] = tmp_4341;
        n = n + 1;
      }
      break;
    }
    case 3: {
      uint32_t dst_port_4065  = ev_in->data.args.outside_packet_4043.dst_port_4042;
      event_t this  = mk_outside_packet(dst_port_4065);
      uint32_t ip_4066  = Array_update_complex_nat_to_ip_4035_get_get_memop_32_bit(dst_port_4065, 0, 0);
      uint32_t port_4067  = Array_update_complex_nat_to_port_4036_get_get_memop_32_bit(dst_port_4065, 0, 0);
      printf("Mapped port %d to (ip: %d, port: %d)", dst_port_4065, ip_4066, port_4067);
      if (ip_4066 == 0) {
        printf("dropped");
      }else {
        out_event_t tmp_4342  = {.ev = mk_outside_continue(ip_4066, port_4067), .out_loc = 1, .port = 0};
        out_events[n] = tmp_4342;
        n = n + 1;
      }
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}
uint8_t parse_event(packet_t*  packet , event_t*  next_event ){
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
          uint32_t src_ip_4037  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t src_port_4038  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_inside_packet(src_ip_4037, src_port_4038);
          return packet->cursor <= packet->end;
          break;
        }
        case 2: {
          uint32_t src_port_4040  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_inside_continue(src_port_4040);
          return packet->cursor <= packet->end;
          break;
        }
        case 3: {
          uint32_t dst_port_4042  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_outside_packet(dst_port_4042);
          return packet->cursor <= packet->end;
          break;
        }
        case 4: {
          uint32_t dst_ip_4044  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t dst_port_4045  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_outside_continue(dst_ip_4044, dst_port_4045);
          return packet->cursor <= packet->end;
          break;
        }
        case 5: {
          uint32_t src_ip_4047  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t src_port_4048  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_add_to_nat(src_ip_4047, src_port_4048);
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
void deparse_event(event_t*  ev_out , packet_t*  buf_out ){
  switch (ev_out->data.tag) {
    case 1: {
      uint32_t src_ip_4037  = ev_out->data.args.inside_packet_4039.src_ip_4037;
      uint32_t src_port_4038  = ev_out->data.args.inside_packet_4039.src_port_4038;
      write_bits(buf_out, ((uint64_t)(src_port_4038)), 32);
      write_bits(buf_out, ((uint64_t)(src_ip_4037)), 32);
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
      uint32_t src_port_4040  = ev_out->data.args.inside_continue_4041.src_port_4040;
      write_bits(buf_out, ((uint64_t)(src_port_4040)), 32);
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
      uint32_t dst_port_4042  = ev_out->data.args.outside_packet_4043.dst_port_4042;
      write_bits(buf_out, ((uint64_t)(dst_port_4042)), 32);
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
      uint32_t dst_ip_4044  = ev_out->data.args.outside_continue_4046.dst_ip_4044;
      uint32_t dst_port_4045  = ev_out->data.args.outside_continue_4046.dst_port_4045;
      write_bits(buf_out, ((uint64_t)(dst_port_4045)), 32);
      write_bits(buf_out, ((uint64_t)(dst_ip_4044)), 32);
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
      uint32_t src_ip_4047  = ev_out->data.args.add_to_nat_4049.src_ip_4047;
      uint32_t src_port_4048  = ev_out->data.args.add_to_nat_4049.src_port_4048;
      write_bits(buf_out, ((uint64_t)(src_port_4048)), 32);
      write_bits(buf_out, ((uint64_t)(src_ip_4047)), 32);
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
    event_t buf[EV_QUEUE_CAP];
    int head;
    int tail;
    int count;
} ev_queue_t;

static void evq_init(ev_queue_t* q) { q->head = 0; q->tail = 0; q->count = 0; }
static int  evq_empty(ev_queue_t* q) { return q->count == 0; }
static void evq_push(ev_queue_t* q, event_t* ev) {
    // no overflow guard (see handler lowering); EV_QUEUE_CAP is generous.
    q->buf[q->tail] = *ev;
    q->tail = (q->tail + 1) % EV_QUEUE_CAP;
    q->count++;
}
static void evq_pull(ev_queue_t* q, event_t* out) {
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

// The dispatch pipeline is split into three phases, mirroring the DPDK reference
// driver (rx -> dispatch -> tx). Here they run synchronously per input packet
// rather than as ring-connected stages: there is no live competing input (pcap
// replays an offline file), so the queue is drained to empty each packet, which
// keeps the input buffer valid for the whole drain -- port events (even from
// recirculated events) reuse its payload via copy_packet. (The buffer-ownership
// model is the pcap counterpart of the DPDK event-carries-its-mbuf design; it is
// still the copy-based approach, not yet the "event as a view" model.)

/******** TX: deparse one output event over a copy of the input packet, then dump it ********/
static void do_tx(pkt_hdl_ctx_t *ctx, out_event_t *oe) {
    // pcap has a single output file, so oe->port is recorded but every port event
    // goes to the same dump.
    reset_cursor(&ctx->out_pkt);
    copy_packet(&ctx->out_pkt, &ctx->in_pkt);
    // out_pkt.cursor sits at the payload boundary (input parse position) until deparse
    // prepends the header. A no-payload event emits only its header (drop the input
    // tail); a payload event keeps the tail. (matches interp)
    uint8_t *payload_boundary = ctx->out_pkt.cursor;
    deparse_event(&oe->ev, &ctx->out_pkt);
    uint8_t *dump_end = oe->ev.meta.has_payload ? ctx->out_pkt.end : payload_boundary;
    fill_out_pkthdr(ctx->in_pkthdr, &ctx->out_pkt, dump_end, &ctx->out_pkthdr);
    pcap_dump((u_char *)ctx->out_pcap, &ctx->out_pkthdr, (u_char *)ctx->out_pkt.cursor);
}

/******** DISPATCH: drain the queue, routing each output (recirc -> queue, port -> tx) ********/
static void do_dispatch(pkt_hdl_ctx_t *ctx) {
    while (!evq_empty(&ctx->queue)) {
        event_t ev;
        evq_pull(&ctx->queue, &ev);
        ev.meta.timestamp = now_ns();        // stamp at dequeue (covers arriving + recirculated events)
        ev.meta.in_port = ctx->ingress_port; // ingress port (read by the handler)
        out_event_t out_events[64];
        uint16_t n = handle_event(&ev, out_events);
        for (uint16_t i = 0; i < n; i++) {
            if (out_events[i].out_loc == 1)        // recirculation: re-queue for dispatch
                evq_push(&ctx->queue, &out_events[i].ev);
            else if (out_events[i].out_loc == 2)   // output to a port: deparse + dump
                do_tx(ctx, &out_events[i]);
        }
    }
}

/******** RX: parse the input packet into an event and enqueue it (pcap_loop callback) ********/
void lpcap_packet_handler(u_char *raw_ctx, const struct pcap_pkthdr *pkthdr, const u_char *packet) {
    pkt_hdl_ctx_t *ctx = (pkt_hdl_ctx_t *)raw_ctx;
    init_cursor((uint8_t *)packet, pkthdr->len, &ctx->in_pkt); // construct a new cursor
    ctx->in_pkthdr = pkthdr;                                   // remembered for do_tx (ts source)
    event_t ev0;
    if (parse_event(&ctx->in_pkt, &ev0) != 1) {
        debug_printf("parse failed!\n");
        return; // drop
    }
    evq_push(&ctx->queue, &ev0);
    do_dispatch(ctx); // drain: handle + route this packet's events (and any it recirculates)
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