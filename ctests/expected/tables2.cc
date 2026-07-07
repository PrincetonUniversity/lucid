
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


/********************************************************************************/
/*                             SECTION: program code                            */
/********************************************************************************/
typedef struct {
  uint32_t _0;
  uint8_t _1;
} tuple_1;
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
enum {tag_WriteCacheTable_hit_acn_4315 = 0, tag_WriteCacheTable_miss_acn_4317 = 1};
typedef struct {
  uint16_t len;
  uint8_t is_packet;
  uint8_t has_payload;
  uint32_t timestamp;
  uint32_t in_port;
} event_meta;
uint16_t do_set_tag  = 1;
uint16_t do_get_tag  = 2;
typedef struct {
  uint16_t tag;
  union {
    struct {
      uint32_t k_4324;
      uint32_t v_4325;
    } do_set_4326;
    struct {
      uint32_t k_4327;
    } do_get_4328;
  } args;
} event_variant_t;
event_variant_t do_set_4326(uint32_t k_4324 , uint32_t v_4325 ){
  event_variant_t ev  = {0};
  ev.args.do_set_4326.k_4324 = k_4324;
  ev.args.do_set_4326.v_4325 = v_4325;
  ev.tag = do_set_tag;
  return ev;
}
event_variant_t do_get_4328(uint32_t k_4327 ){
  event_variant_t ev  = {0};
  ev.args.do_get_4328.k_4327 = k_4327;
  ev.tag = do_get_tag;
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
event_t mk_do_set(uint32_t k_4324 , uint32_t v_4325 ){
  event_t tmp_4596  = {.meta = {.len = 8, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_set_4326(k_4324, v_4325)};
  return tmp_4596;
}
event_t mk_do_get(uint32_t k_4327 ){
  event_t tmp_4597  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_get_4328(k_4327)};
  return tmp_4597;
}
uint32_t recirculation_port  = 0;
uint32_t self  = 0;
typedef tuple_1 res_t_4313;
res_t_4313 WriteCacheTable_hit_acn_4315(uint32_t x_4314 ){
  res_t_4313 tmp_4598  = {._0 = x_4314, ._1 = true,};
  return tmp_4598;
}
res_t_4313 WriteCacheTable_miss_acn_4317(uint32_t unused_4316 ){
  res_t_4313 tmp_4599  = {._0 = 0, ._1 = false,};
  return tmp_4599;
}
uint32_t global_cached_table_0_4321 [256] = {0};
uint32_t Array_update_complex_global_cached_table_0_4321_combined_memop_WriteCacheTable_set_if_empty_WriteCacheTable_set_if_empty_4592(uint8_t _idx , uint32_t newval_4319 , uint32_t newval_set_4589 ){
  uint32_t cell1  = global_cached_table_0_4321[(((uint32_t)(_idx)) % 256)];
  uint32_t cell2  = 0;
  uint32_t ret  = 0;
  uint8_t mbool_4590  = global_cached_table_0_4321[(((uint32_t)(_idx)) % 256)] == 0;
  uint8_t mbool_4591  = global_cached_table_0_4321[(((uint32_t)(_idx)) % 256)] == 0;
  if (mbool_4590) {
    cell1 = newval_set_4589;
  }
  if (!mbool_4590) {
    cell1 = global_cached_table_0_4321[(((uint32_t)(_idx)) % 256)];
  }
  if (mbool_4591) {
    cell2 = newval_4319;
  }
  if (!mbool_4591) {
    cell2 = global_cached_table_0_4321[(((uint32_t)(_idx)) % 256)];
  }
  if (true) {
    ret = cell2;
  }
  global_cached_table_0_4321[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint32_t Array_update_complex_global_cached_table_0_4321_get_get_memop_32_bit(uint8_t _idx , uint32_t unused1_get_memop_32_bit , uint32_t unused2_get_memop_32_bit ){
  uint32_t cell1  = global_cached_table_0_4321[(((uint32_t)(_idx)) % 256)];
  uint32_t cell2  = 0;
  uint32_t ret  = 0;
  if (true) {
    ret = global_cached_table_0_4321[(((uint32_t)(_idx)) % 256)];
  }
  global_cached_table_0_4321[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint32_t global_cached_table_1_4322 [256] = {0};
uint32_t Array_update_complex_global_cached_table_1_4322_set_set_memop_32_bit(uint8_t _idx , uint32_t new_val_set_memop_32_bit , uint32_t unused_set_memop_32_bit ){
  uint32_t cell1  = global_cached_table_1_4322[(((uint32_t)(_idx)) % 256)];
  uint32_t cell2  = 0;
  uint32_t ret  = 0;
  if (true) {
    cell1 = new_val_set_memop_32_bit;
  }
  global_cached_table_1_4322[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint32_t Array_update_complex_global_cached_table_1_4322_get_get_memop_32_bit(uint8_t _idx , uint32_t unused1_get_memop_32_bit , uint32_t unused2_get_memop_32_bit ){
  uint32_t cell1  = global_cached_table_1_4322[(((uint32_t)(_idx)) % 256)];
  uint32_t cell2  = 0;
  uint32_t ret  = 0;
  if (true) {
    ret = global_cached_table_1_4322[(((uint32_t)(_idx)) % 256)];
  }
  global_cached_table_1_4322[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
typedef struct {
  uint8_t valid;
  uint32_t key;
  uint32_t mask;
  uint32_t action_tag;
  uint32_t action_arg;
} cellty_global_cached_table_2_4323;
typedef struct {
  struct {
    uint32_t action_tag;
    uint32_t action_arg;
  } _default;
  cellty_global_cached_table_2_4323 entries [1024];
} ty_global_cached_table_2_4323;
ty_global_cached_table_2_4323 global_cached_table_2_4323  = {._default = {.action_tag = tag_WriteCacheTable_miss_acn_4317, .action_arg = 0}, .entries = {0}};
void install_global_cached_table_2_4323(uint32_t key , uint32_t action , uint32_t const_arg ){
  bool _continue = true;
  for (int _idx = 0; _idx < 1024; _idx++) {
    if ((global_cached_table_2_4323.entries[_idx].valid) == (false)) {
      _continue = false;
      cellty_global_cached_table_2_4323 tmp_4600  = {.valid = true, .key = key, .mask = 4294967295, .action_tag = action, .action_arg = const_arg};
      global_cached_table_2_4323.entries[_idx] = tmp_4600;
    }  if (!_continue) break;
    
  }
  return ;
}
void install_ternary_global_cached_table_2_4323(uint32_t key , uint32_t mask , uint32_t action , uint32_t const_arg ){
  bool _continue = true;
  for (int _idx = 0; _idx < 1024; _idx++) {
    if ((global_cached_table_2_4323.entries[_idx].valid) == (false)) {
      _continue = false;
      cellty_global_cached_table_2_4323 tmp_4601  = {.valid = true, .key = key, .mask = mask, .action_tag = action, .action_arg = const_arg};
      global_cached_table_2_4323.entries[_idx] = tmp_4601;
    }  if (!_continue) break;
    
  }
  return ;
}
res_t_4313 lookup_global_cached_table_2_4323(uint32_t key ){
  for (int _idx = 0; _idx < 1024; _idx++) {
    if ((global_cached_table_2_4323.entries[_idx].valid) && ((key & global_cached_table_2_4323.entries[_idx].mask) == (global_cached_table_2_4323.entries[_idx].key & global_cached_table_2_4323.entries[_idx].mask))) {
      switch (global_cached_table_2_4323.entries[_idx].action_tag) {
        case tag_WriteCacheTable_hit_acn_4315: {
          return WriteCacheTable_hit_acn_4315(global_cached_table_2_4323.entries[_idx].action_arg);
          break;
        }
        case tag_WriteCacheTable_miss_acn_4317: {
          return WriteCacheTable_miss_acn_4317(global_cached_table_2_4323.entries[_idx].action_arg);
          break;
        }
      }
    }
  }
  switch (global_cached_table_2_4323._default.action_tag) {
    case tag_WriteCacheTable_hit_acn_4315: {
      return WriteCacheTable_hit_acn_4315(global_cached_table_2_4323._default.action_arg);
      break;
    }
    default: {
      return WriteCacheTable_miss_acn_4317(global_cached_table_2_4323._default.action_arg);
      break;
    }
  }
}
uint16_t handle_event(event_t*  ev_in , out_event_t out_events [64]){
  uint16_t n  = 0;
  switch (ev_in->data.tag) {
    case 1: {
      uint32_t k_4329  = ev_in->data.args.do_set_4326.k_4324;
      uint32_t v_4330  = ev_in->data.args.do_set_4326.v_4325;
      event_t this  = mk_do_set(k_4329, v_4330);
      uint8_t idx_4331  = hash_32((uint32_t)1, (uint8_t* )&k_4329, 32);
      printf("hash size arg: %d", 8);
      printf("[set] index: %d", idx_4331);
      uint32_t cache_key_4332  = Array_update_complex_global_cached_table_0_4321_combined_memop_WriteCacheTable_set_if_empty_WriteCacheTable_set_if_empty_4592(idx_4331, k_4329, k_4329);
      if (cache_key_4332 == k_4329) {
        printf("installing entry for %d into CACHE at index %d", k_4329, idx_4331);
        Array_update_complex_global_cached_table_1_4322_set_set_memop_32_bit(idx_4331, v_4330, 0);
      }else {
        printf("installing entry for %d into TABLE", k_4329);
        install_global_cached_table_2_4323(k_4329, tag_WriteCacheTable_hit_acn_4315, v_4330);
      }
      false;
      break;
    }
    case 2: {
      uint32_t k_4333  = ev_in->data.args.do_get_4328.k_4327;
      event_t this  = mk_do_get(k_4333);
      uint32_t WriteCacheTable_get_ret_0_4334  = 0;
      uint8_t WriteCacheTable_get_ret_1_4335  = false;
      uint8_t idx_4336  = hash_32((uint32_t)1, (uint8_t* )&k_4333, 32);
      uint32_t stored_key_4337  = Array_update_complex_global_cached_table_0_4321_get_get_memop_32_bit(idx_4336, 0, 0);
      if (stored_key_4337 == k_4333) {
        uint32_t stored_val_4338  = Array_update_complex_global_cached_table_1_4322_get_get_memop_32_bit(idx_4336, 0, 0);
        WriteCacheTable_get_ret_0_4334 = stored_val_4338;
        WriteCacheTable_get_ret_1_4335 = true;
      }else {
        
        res_t_4313 tup_4595  = lookup_global_cached_table_2_4323(k_4333);
        WriteCacheTable_get_ret_0_4334 = tup_4595._0;
        WriteCacheTable_get_ret_1_4335 = tup_4595._1;
      }
      if (WriteCacheTable_get_ret_1_4335) {
        printf("key: %d result: %d", k_4333, WriteCacheTable_get_ret_0_4334);
      }else {
        printf("key: %d result: NOT FOUND", k_4333);
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
          uint32_t k_4324  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t v_4325  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_do_set(k_4324, v_4325);
          return packet->cursor <= packet->end;
          break;
        }
        case 2: {
          uint32_t k_4327  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_do_get(k_4327);
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
      uint32_t k_4324  = ev_out->data.args.do_set_4326.k_4324;
      uint32_t v_4325  = ev_out->data.args.do_set_4326.v_4325;
      write_bits(buf_out, ((uint64_t)(v_4325)), 32);
      write_bits(buf_out, ((uint64_t)(k_4324)), 32);
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
      uint32_t k_4327  = ev_out->data.args.do_get_4328.k_4327;
      write_bits(buf_out, ((uint64_t)(k_4327)), 32);
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

/********************************************************************************/
/*                             SECTION: driver config                           */
/********************************************************************************/

/********* sizing constants (shared by the slab pool and the index rings) ***********/
#define HEADROOM 256               // slack before the packet for deparse to prepend a header
#define SLOT_USABLE 1600           // max packet bytes per slot
#define POOL_SIZE 1024             // number of slab slots (buffers in flight)
#define RING_CAP (POOL_SIZE + 1)   // ring capacity: a head/tail ring holds CAP-1 items, so this
                                   // holds up to POOL_SIZE indices (all slots free, or all in one ring)
#define BURST 64                   // max frames/elements handled per rx/dispatch/tx call
#define SLOT_NONE 0xFFFF           // slot_alloc's "pool exhausted" sentinel
#define MAX_PORTS 64               // max interfaces (Lucid ports) bound at once


/********************************************************************************/
/*                           SECTION: driver libraries                          */
/********************************************************************************/

 void init_cursor(uint8_t*  buf , uint32_t len , packet_t*  bytes ){
    bytes->start = buf;
    bytes->cursor = buf;
    bytes->end = buf + len;
    bytes->bit_off = 0;
}
// a 32-bit nanosecond timestamp, stamped onto each event at dispatch (Sys.time())
static uint32_t now_ns(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint32_t)((uint64_t)ts.tv_sec * 1000000000ull + (uint64_t)ts.tv_nsec);
}


/********* index ring (a FIFO of slot indices) ***********/
typedef struct { uint16_t buf[RING_CAP]; uint32_t head, tail; } idx_ring;
static void ring_init(idx_ring* r) { r->head = r->tail = 0; }
static int ring_empty(idx_ring* r) { return r->head == r->tail; }
static int ring_push(idx_ring* r, uint16_t idx) {
    uint32_t nh = (r->head + 1) % RING_CAP;
    if (nh == r->tail) return -1;                 // full (shouldn't happen: sized to pool)
    r->buf[r->head] = idx; r->head = nh; return 0;
}
static int ring_pop(idx_ring* r, uint16_t* out) {
    if (r->tail == r->head) return -1;            // empty
    *out = r->buf[r->tail]; r->tail = (r->tail + 1) % RING_CAP; return 0;
}


/********* the queue element (a slab slot) ***********/
// Mirrors the DPDK qe_priv_t + mbuf data region: the event, the handler's outputs,
// and the packet bytes this element OWNS. The packet occupies data[HEADROOM ..
// HEADROOM+pkt_len); payload_off marks where the payload begins within it.
typedef struct {
    event_t    ev;
    out_event_t out_events[64];
    uint16_t        n_out;
    uint32_t        pkt_len;        // packet bytes from data+HEADROOM
    uint32_t        payload_off;    // payload boundary, relative to data+HEADROOM
    uint8_t         data[HEADROOM + SLOT_USABLE];
} qe_t;

/********* the slab: the pool + its free-list (an idx_ring of free slot indices) ***********/
typedef struct {
    qe_t     pool[POOL_SIZE];
    idx_ring free;                 // the free-list: indices of currently-unused slots
} slab_t;

// slot(): the queue element at index idx -- callers go through this, never the pool directly.
static inline qe_t* slot(slab_t* s, uint16_t idx) { return &s->pool[idx]; }
static uint16_t slot_alloc(slab_t* s) {
    uint16_t idx;
    if (ring_pop(&s->free, &idx) != 0) return SLOT_NONE;   // pool exhausted
    return idx;
}
static void slot_free(slab_t* s, uint16_t idx) { ring_push(&s->free, idx); }
static void slab_init(slab_t* s) {
    ring_init(&s->free);
    for (uint16_t i = 0; i < POOL_SIZE; i++) ring_push(&s->free, i);   // every slot starts free
}


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


/********************************************************************************/
/*                            SECTION: driver pipeline                          */
/********************************************************************************/

/********* the driver's runtime state (instances of the slab + ring libraries) ***********/
static port_map_t   g_port_map;  // Lucid port <-> the driver's I/O (see its port_map_lib)
static slab_t       g_slab;        // the packet-buffer pool
static idx_ring     dispatch_in;   // parsed + recirculated elements awaiting handling
static idx_ring     tx_in;         // handled elements awaiting fan-out + deparse + TX
static uint64_t     pkt_ct = 0;    // rx packet counter


// parse the frame sitting in slot `idx` (pkt_len bytes at data+HEADROOM) into its event
// and enqueue it for dispatch; drop (free the slot) on parse failure.
static void ingest_slot(uint16_t idx, int in_port) {
    qe_t* q = slot(&g_slab, idx);
    packet_t view;
    init_cursor(q->data + HEADROOM, q->pkt_len, &view);
    if (parse_event(&view, &q->ev) != 1) { debug_printf("parse failed\n"); slot_free(&g_slab, idx); return; }
    q->payload_off = (uint32_t)(view.cursor - (q->data + HEADROOM)); // where the payload begins
    q->ev.meta.in_port = in_port;                                    // ingress (read by the handler)
    if (ring_push(&dispatch_in, idx) != 0) slot_free(&g_slab, idx);  // ring full (shouldn't happen)
    else pkt_ct++;
}

/******** RX: read a bounded burst from every port, parse + enqueue each on dispatch_in ********/
static void do_rx(void) {
    for (int p = 0; p < g_port_map.nports; p++) {
        rx_batch batch = port_rx(get_in_descriptor(&g_port_map, p), &g_slab);
        for (uint16_t i = 0; i < batch.n; i++) ingest_slot(batch.idx[i], g_port_map.ports[p].port_id);
    }
}


/******** DISPATCH: handle a bounded burst of elements; hand each to tx_in (no copy) ********/
static void do_dispatch(void) {
    for (int b = 0; b < BURST; b++) {
        uint16_t idx;
        if (ring_pop(&dispatch_in, &idx) != 0) break;
        qe_t* q = slot(&g_slab, idx);
        q->ev.meta.timestamp = now_ns();                    // stamp at dequeue (arriving + recirculated)
        q->n_out = handle_event(&q->ev, q->out_events); // ingress read from q->ev.meta.in_port
        if (ring_push(&tx_in, idx) != 0) slot_free(&g_slab, idx); // ring full (shouldn't happen) -> drop
    }
}


#define PORT_RECIRC 4294967295u // out_event.port sentinel: recirculate, don't egress

/******** TX: fan out each element into one owned clone per output, then route/deparse/send ********/
static void do_tx(void) {
    for (int b = 0; b < BURST; b++) {
        uint16_t idx;
        if (ring_pop(&tx_in, &idx) != 0) break;
        qe_t* in = slot(&g_slab, idx);
        for (uint16_t i = 0; i < in->n_out; i++) {
            out_event_t* oe = &in->out_events[i];
            uint16_t cidx = slot_alloc(&g_slab);
            if (cidx == SLOT_NONE) continue;             // pool exhausted -> drop this output
            qe_t* c = slot(&g_slab, cidx);
            c->ev = oe->ev;
            // the output owns a fresh copy of the input's payload (or none), placed at
            // data+HEADROOM with headroom in front for the deparsed header.
            uint32_t plen = oe->ev.meta.has_payload ? (in->pkt_len - in->payload_off) : 0;
            if (plen) memcpy(c->data + HEADROOM, in->data + HEADROOM + in->payload_off, plen);
            c->pkt_len = plen;
            c->payload_off = 0;                          // the payload now sits at the front of c
            if (oe->port == PORT_RECIRC) {               // recirculation (generate_self)
                c->ev.meta.in_port = in->ev.meta.in_port; // recirc inherits ingress
                if (ring_push(&dispatch_in, cidx) != 0) slot_free(&g_slab, cidx);
            } else {                                     // output to a port: deparse + send
                packet_t view;
                init_cursor(c->data + HEADROOM, plen, &view); // cursor at the payload boundary (front)
                deparse_event(&c->ev, &view);      // writes the header backwards into headroom
                // a no-payload event emits only its header (drop the tail); a payload event keeps it.
                uint8_t* dump_end = oe->ev.meta.has_payload ? (c->data + HEADROOM + plen) : (c->data + HEADROOM);
                size_t out_len = (size_t)(dump_end - view.cursor);
                // resolve egress port -> descriptor (mirrors do_rx's ingress lookup) and send;
                // send_frame drops if the port is unknown.
                send_frame(get_out_descriptor(&g_port_map, (int)oe->port), view.cursor, out_len);
                slot_free(&g_slab, cidx);                // egress done
            }
        }
        slot_free(&g_slab, idx);                         // input consumed (cloned per output)
    }
}


/********************************************************************************/
/*                              SECTION: driver main                            */
/********************************************************************************/

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
