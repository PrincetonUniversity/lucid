
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
  uint64_t timestamp;
  uint32_t in_port;
} event_meta;
uint16_t pkt_in_tag  = 1;
uint16_t bg_cmd_tag  = 2;
uint16_t pkt_out_tag  = 3;
typedef struct {
  uint16_t tag;
  union {
    struct {
      uint32_t src_ip_1184;
      uint32_t src_port_1185;
    } pkt_in_1186;
    struct {
      uint32_t src_port_1187;
    } bg_cmd_1188;
    struct {
      uint32_t src_ip_1189;
      uint32_t src_port_1190;
    } pkt_out_1191;
  } args;
} event_variant_t;
event_variant_t pkt_in_1186(uint32_t src_ip_1184 , uint32_t src_port_1185 ){
  event_variant_t ev  = {0};
  ev.args.pkt_in_1186.src_ip_1184 = src_ip_1184;
  ev.args.pkt_in_1186.src_port_1185 = src_port_1185;
  ev.tag = pkt_in_tag;
  return ev;
}
event_variant_t bg_cmd_1188(uint32_t src_port_1187 ){
  event_variant_t ev  = {0};
  ev.args.bg_cmd_1188.src_port_1187 = src_port_1187;
  ev.tag = bg_cmd_tag;
  return ev;
}
event_variant_t pkt_out_1191(uint32_t src_ip_1189 , uint32_t src_port_1190 ){
  event_variant_t ev  = {0};
  ev.args.pkt_out_1191.src_ip_1189 = src_ip_1189;
  ev.args.pkt_out_1191.src_port_1190 = src_port_1190;
  ev.tag = pkt_out_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_pkt_in(uint32_t src_ip_1184 , uint32_t src_port_1185 ){
  event_t tmp_1270  = {.meta = {.len = 8, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = pkt_in_1186(src_ip_1184, src_port_1185)};
  return tmp_1270;
}
event_t mk_bg_cmd(uint32_t src_port_1187 ){
  event_t tmp_1271  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = bg_cmd_1188(src_port_1187)};
  return tmp_1271;
}
event_t mk_pkt_out(uint32_t src_ip_1189 , uint32_t src_port_1190 ){
  event_t tmp_1272  = {.meta = {.len = 8, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = pkt_out_1191(src_ip_1189, src_port_1190)};
  return tmp_1272;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
uint8_t parse_event(packet_t*  packet , event_t*  next_event , uint32_t ingress_port ){
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
          uint32_t src_ip_1184  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t src_port_1185  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_pkt_in(src_ip_1184, src_port_1185);
          return packet->cursor <= packet->end;
          break;
        }
        case 2: {
          uint32_t src_port_1187  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_bg_cmd(src_port_1187);
          return packet->cursor <= packet->end;
          break;
        }
        case 3: {
          uint32_t src_ip_1189  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t src_port_1190  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_pkt_out(src_ip_1189, src_port_1190);
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
      uint32_t src_ip_1184  = ev_out->data.args.pkt_in_1186.src_ip_1184;
      uint32_t src_port_1185  = ev_out->data.args.pkt_in_1186.src_port_1185;
      write_bits(buf_out, ((uint64_t)(src_port_1185)), 32);
      write_bits(buf_out, ((uint64_t)(src_ip_1184)), 32);
      if (ev_out->meta.is_packet == 0) {
        write_bits(buf_out, ((uint64_t)(1)), 16);
        write_bits(buf_out, ((uint64_t)(666)), 16);
        write_bits(buf_out, ((uint64_t)(2)), 48);
        write_bits(buf_out, ((uint64_t)(1)), 48);
      }
      return ;
      break;
    }
    case 2: {
      uint32_t src_port_1187  = ev_out->data.args.bg_cmd_1188.src_port_1187;
      write_bits(buf_out, ((uint64_t)(src_port_1187)), 32);
      if (ev_out->meta.is_packet == 0) {
        write_bits(buf_out, ((uint64_t)(2)), 16);
        write_bits(buf_out, ((uint64_t)(666)), 16);
        write_bits(buf_out, ((uint64_t)(2)), 48);
        write_bits(buf_out, ((uint64_t)(1)), 48);
      }
      return ;
      break;
    }
    case 3: {
      uint32_t src_ip_1189  = ev_out->data.args.pkt_out_1191.src_ip_1189;
      uint32_t src_port_1190  = ev_out->data.args.pkt_out_1191.src_port_1190;
      write_bits(buf_out, ((uint64_t)(src_port_1190)), 32);
      write_bits(buf_out, ((uint64_t)(src_ip_1189)), 32);
      if (ev_out->meta.is_packet == 0) {
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
typedef struct {
  event_t ev;
  uint32_t port;
} out_event_t;
uint16_t handle_event(event_t*  ev_in , out_event_t out_events [64]){
  uint16_t n  = 0;
  switch (ev_in->data.tag) {
    case 2: {
      uint32_t src_port_1192  = ev_in->data.args.bg_cmd_1188.src_port_1187;
      event_t this  = mk_bg_cmd(src_port_1192);
      break;
    }
    case 3: {
      uint32_t src_ip_1193  = ev_in->data.args.pkt_out_1191.src_ip_1189;
      uint32_t src_port_1194  = ev_in->data.args.pkt_out_1191.src_port_1190;
      event_t this  = mk_pkt_out(src_ip_1193, src_port_1194);
      break;
    }
    case 1: {
      uint32_t src_ip_1195  = ev_in->data.args.pkt_in_1186.src_ip_1184;
      uint32_t src_port_1196  = ev_in->data.args.pkt_in_1186.src_port_1185;
      event_t this  = mk_pkt_in(src_ip_1195, src_port_1196);
      printf("Received packet from (ip: %u, port: %u)\n", src_ip_1195, src_port_1196);
      event_t MY_EVENT_1197  = mk_pkt_out(src_ip_1195, src_port_1196);
      out_event_t tmp_1273  = {.ev = mk_bg_cmd(src_port_1196), .port = 4294967295};
      out_events[n] = tmp_1273;
      n = n + 1;
      switch (MY_EVENT_1197.data.tag) {
        case 3: {
          uint32_t ip_1198  = MY_EVENT_1197.data.args.pkt_out_1191.src_ip_1189;
          uint32_t port_1199  = MY_EVENT_1197.data.args.pkt_out_1191.src_port_1190;
          printf("Sending packet to (ip: %u, port: %u)\n", ip_1198, port_1199);
          out_event_t tmp_1274  = {.ev = mk_pkt_out(src_ip_1195, src_port_1196), .port = 0};
          out_events[n] = tmp_1274;
          n = n + 1;
          break;
        }
        default: {
          
          break;
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
// a platform latency, max'ed with each output's delay: the pcap driver's --latency-ns,
// which models one; 0 on a real platform
static uint64_t g_latency_ns = 0;


/********************************************************************************/
/*                           SECTION: driver libraries                          */
/********************************************************************************/

 void init_cursor(uint8_t*  buf , uint32_t len , packet_t*  bytes ){
    bytes->start = buf;
    bytes->cursor = buf;
    bytes->end = buf + len;
    bytes->bit_off = 0;
}
// a 48-bit nanosecond timestamp, stamped onto each event at dispatch (Sys.time() is its low 32 bits)
static uint64_t now_ns(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ((uint64_t)ts.tv_sec * 1000000000ull + (uint64_t)ts.tv_nsec) & 0xFFFFFFFFFFFFull;
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
static int ring_peek(idx_ring* r, uint16_t* out) {
    if (r->tail == r->head) return -1;            // empty
    *out = r->buf[r->tail]; return 0;
}


/********* the queue element (a slab slot) ***********/
// the event, the handler's outputs, and the element's packet bytes (this driver's
// analogue of the DPDK mbuf + private area). The packet occupies data[HEADROOM ..
// HEADROOM+pkt_len); payload_off marks where the payload begins within it.
typedef struct {
    event_t    ev;
    out_event_t out_events[64];
    uint16_t        n_out;
    uint32_t        pkt_len;        // packet bytes from data+HEADROOM
    uint32_t        payload_off;    // payload boundary, relative to data+HEADROOM
    uint64_t        arrival;        // arrival time (ns) for drivers with a virtual clock (pcap)
    uint32_t        out_port;       // a delayed out-event's egress, while it waits in the calendar
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


/********* the calendar: delayed out-events waiting for their departure time ***********/
typedef struct { uint64_t at; uint64_t seq; uint16_t idx; } cal_ent;
typedef struct { cal_ent heap[POOL_SIZE]; uint32_t n; uint64_t seq; } calendar_t;
static calendar_t g_calendar;
static int cal_before(cal_ent* a, cal_ent* b) { return a->at < b->at || (a->at == b->at && a->seq < b->seq); }
static int cal_push(calendar_t* c, uint64_t at, uint16_t idx) {
    if (c->n >= POOL_SIZE) return -1;
    uint32_t i = c->n++;
    c->heap[i].at = at; c->heap[i].seq = c->seq++; c->heap[i].idx = idx;
    while (i > 0) {
        uint32_t p = (i - 1) / 2;
        if (!cal_before(&c->heap[i], &c->heap[p])) break;
        cal_ent t = c->heap[p]; c->heap[p] = c->heap[i]; c->heap[i] = t; i = p;
    }
    return 0;
}
static int cal_peek(calendar_t* c, uint64_t* at) { if (c->n == 0) return -1; *at = c->heap[0].at; return 0; }
static uint16_t cal_pop(calendar_t* c) {
    uint16_t idx = c->heap[0].idx;
    c->n--;
    c->heap[0] = c->heap[c->n];
    uint32_t i = 0;
    for (;;) {
        uint32_t l = 2 * i + 1, r = l + 1, m = i;
        if (l < c->n && cal_before(&c->heap[l], &c->heap[m])) m = l;
        if (r < c->n && cal_before(&c->heap[r], &c->heap[m])) m = r;
        if (m == i) break;
        cal_ent t = c->heap[m]; c->heap[m] = c->heap[i]; c->heap[i] = t; i = m;
    }
    return idx;
}


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


/********************************************************************************/
/*                            SECTION: driver pipeline                          */
/********************************************************************************/

/********* driver runtime state (instances of the slab + ring libraries) ***********/
static port_map_t   g_port_map;  // Lucid port <-> the driver's I/O (see its port_map_lib)
static slab_t       g_slab;        // the packet-buffer pool
static idx_ring     dispatch_in;   // parsed + recirculated elements awaiting handling
static idx_ring     tx_in;         // handled elements awaiting fan-out + deparse + TX
static uint64_t     pkt_ct = 0;    // rx packet counter


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


// parse the frame in slot `idx` into its event; 1 if it parsed (the slot is freed otherwise)
static int ingest_slot(uint16_t idx, int in_port) {
    qe_t* q = slot(&g_slab, idx);
    packet_t view;
    init_cursor(q->data + HEADROOM, q->pkt_len, &view);
    if (parse_event(&view, &q->ev, (uint32_t)in_port) != 1) { debug_printf("parse failed\n"); slot_free(&g_slab, idx); return 0; }
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
    q->n_out = handle_event(&q->ev, q->out_events);
    if (ring_push(&tx_in, idx) != 0) slot_free(&g_slab, idx);
}


#define PORT_RECIRC 4294967295u // out_event.port sentinel: recirculate, don't egress

// the clone in slot cidx leaves at `departure`: recirculated (re-enqueued for
// dispatch), or deparsed in front of its payload and sent out of `port`
static void emit_clone(uint16_t cidx, uint32_t port, uint64_t departure) {
    qe_t* c = slot(&g_slab, cidx);
    if (port == PORT_RECIRC) {                       // recirculation (generate_self)
        c->ev.meta.in_port = recirculation_port; // it arrives on the recirculation port
        c->arrival = departure;
        if (ring_push(&dispatch_in, cidx) != 0) slot_free(&g_slab, cidx);
    } else {                                         // output to a port: deparse + send
        packet_t view;
        init_cursor(c->data + HEADROOM, c->pkt_len, &view); // cursor at the payload boundary (front)
        deparse_event(&c->ev, &view);      // writes the header backwards into headroom
        // a no-payload event emits only its header (drop the tail); a payload event keeps it.
        uint8_t* dump_end = c->ev.meta.has_payload ? (c->data + HEADROOM + c->pkt_len) : (c->data + HEADROOM);
        size_t out_len = (size_t)(dump_end - view.cursor);
        // resolve egress port -> descriptor (mirrors do_rx's ingress lookup) and send;
        // send_frame drops if the port is unknown.
        send_frame(get_out_descriptor(&g_port_map, (int)port), view.cursor, out_len, departure);
        slot_free(&g_slab, cidx);                    // egress done
    }
}

// release the delayed out-events whose departure time has come, earliest first
static void do_timers(uint64_t now) {
    uint64_t at;
    while (cal_peek(&g_calendar, &at) == 0 && at <= now) {
        uint16_t cidx = cal_pop(&g_calendar);
        emit_clone(cidx, slot(&g_slab, cidx)->out_port, at);
    }
}

/******** TX: fan out each element into one copy per output, then route/deparse/send ********/
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
            // each output gets its own copy of the input's payload (or none), with
            // headroom in front for the deparsed header.
            uint32_t plen = oe->ev.meta.has_payload ? (in->pkt_len - in->payload_off) : 0;
            if (plen) memcpy(c->data + HEADROOM, in->data + HEADROOM + in->payload_off, plen);
            c->pkt_len = plen;
            c->payload_off = 0;                          // the payload now sits at the front of c
            uint64_t departure = (uint64_t)in->ev.meta.timestamp; // the sender's stamp
            uint64_t delay = 0;
            if (delay < g_latency_ns) delay = g_latency_ns;
            if (delay > 0) {                             // hold it until the sender's stamp + delay
                c->out_port = oe->port;
                if (cal_push(&g_calendar, departure + delay, cidx) != 0) {
                    fprintf(stderr, "calendar full: a delayed event was dropped\n");
                    slot_free(&g_slab, cidx);
                }
                continue;
            }
            emit_clone(cidx, oe->port, departure);
        }
        slot_free(&g_slab, idx);                         // input consumed (cloned per output)
    }
}


/********************************************************************************/
/*                              SECTION: driver main                            */
/********************************************************************************/

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
