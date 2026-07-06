
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/select.h>
#include <net/ethernet.h>
#ifdef __linux__
  #define USE_AF_PACKET
  #include <linux/if_packet.h>
#else
  #define USE_BPF
  #include <net/bpf.h>
#endif
#include <netinet/in.h>
#include <net/if.h>
#include <arpa/inet.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <fcntl.h>
#include <unistd.h>
#include <time.h>

#ifdef DEBUG
  #define debug_printf(...) fprintf(stderr, __VA_ARGS__)
#else
  #define debug_printf(...)
#endif

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
uint16_t ethpkt_tag  = 1;
typedef struct {
  uint16_t tag;
  union {
    struct {
      uint64_t dst_669;
      uint64_t src_670;
      uint16_t ety_671;
    } ethpkt_673;
  } args;
} event_variant_t;
event_variant_t ethpkt_673(uint64_t dst_669 , uint64_t src_670 , uint16_t ety_671 ){
  event_variant_t ev  = {0};
  ev.args.ethpkt_673.dst_669 = dst_669;
  ev.args.ethpkt_673.src_670 = src_670;
  ev.args.ethpkt_673.ety_671 = ety_671;
  ev.tag = ethpkt_tag;
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
event_t mk_ethpkt(uint64_t dst_669 , uint64_t src_670 , uint16_t ety_671 ){
  event_t tmp_713  = {.meta = {.len = 14, .is_packet = 1, .has_payload = 1, .timestamp = 0, .in_port = 0}, .data = ethpkt_673(dst_669, src_670, ety_671)};
  return tmp_713;
}
uint32_t recirculation_port  = 0;
uint32_t self  = 0;
uint8_t parse_event(packet_t*  pkt , event_t*  next_event ){
  uint64_t dst_669  = ((uint64_t)(read_bits(pkt, 48))) & 281474976710655;
  uint64_t src_670  = ((uint64_t)(read_bits(pkt, 48))) & 281474976710655;
  uint16_t ety_671  = ((uint16_t)(read_bits(pkt, 16)));
  (*(next_event)) = mk_ethpkt(dst_669, src_670, ety_671);
  return pkt->cursor <= pkt->end;
}
uint16_t handle_event(event_t*  ev_in , out_event_t out_events [64]){
  uint16_t n  = 0;
  switch (ev_in->data.tag) {
    case 1: {
      uint64_t dst_674  = ev_in->data.args.ethpkt_673.dst_669;
      uint64_t src_675  = ev_in->data.args.ethpkt_673.src_670;
      uint16_t ety_676  = ev_in->data.args.ethpkt_673.ety_671;
      event_t this  = mk_ethpkt(dst_674, src_675, ety_676);
      out_event_t tmp_714  = {.ev = mk_ethpkt(src_675, dst_674, ety_676), .port = ev_in->meta.in_port};
      out_events[n] = tmp_714;
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
      uint64_t dst_669  = ev_out->data.args.ethpkt_673.dst_669;
      uint64_t src_670  = ev_out->data.args.ethpkt_673.src_670;
      uint16_t ety_671  = ev_out->data.args.ethpkt_673.ety_671;
      write_bits(buf_out, ((uint64_t)(ety_671)), 16);
      write_bits(buf_out, ((uint64_t)(src_670)), 48);
      write_bits(buf_out, ((uint64_t)(dst_669)), 48);
      if ((ev_out->meta.is_packet) == (0)) {
        write_bits(buf_out, ((uint64_t)(1)), 16);
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

/********* ports (Lucid port number <-> interface socket) ***********/
#define MAX_PORTS 64
typedef struct { int port_id; int fd; char ifname[IFNAMSIZ]; } port_t;
static port_t g_ports[MAX_PORTS];
static int g_nports = 0;
static int port_fd(int port_id) {
    for (int i = 0; i < g_nports; i++) if (g_ports[i].port_id == port_id) return g_ports[i].fd;
    return -1;
}

static uint64_t pkt_ct = 0;

// a 32-bit nanosecond timestamp, stamped onto each event at dispatch (Sys.time())
static uint32_t now_ns(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint32_t)((uint64_t)ts.tv_sec * 1000000000ull + (uint64_t)ts.tv_nsec);
}


/********* the queue element (a slab slot) ***********/
// Mirrors the DPDK qe_priv_t + mbuf data region: the event, the handler's outputs,
// and the packet bytes this element OWNS. The packet occupies data[HEADROOM ..
// HEADROOM+pkt_len); payload_off marks where the payload begins within it. HEADROOM
// is slack in front so the deparsed header can be prepended (written backwards).
#define HEADROOM 256
#define SLOT_USABLE 1600           // max packet bytes per slot
#define POOL_SIZE 1024             // number of slab slots (buffers in flight)
#define RING_CAP (POOL_SIZE + 1)   // ring capacity (>= pool, so rings never overflow)
#define BURST 64                   // max frames/elements handled per rx/dispatch/tx call
#define SLOT_NONE 0xFFFF

typedef struct {
    event_t    ev;
    out_event_t out_events[64];
    uint16_t        n_out;
    uint32_t        pkt_len;        // packet bytes from data+HEADROOM
    uint32_t        payload_off;    // payload boundary, relative to data+HEADROOM
    uint8_t         data[HEADROOM + SLOT_USABLE];
} qe_t;

/********* the slab state: the pool + its free-list ring, in one struct so the helpers
   take a slab_t* and the instance (g_slab) is declared after them ***********/
typedef struct {
    qe_t     pool[POOL_SIZE];
    uint16_t free_ring[RING_CAP];   // free indices sit in free_ring[free_tail .. free_head)
    uint32_t free_head, free_tail;
} slab_t;

// slot(): the queue element at index idx -- callers go through this, never the pool directly.
static inline qe_t* slot(slab_t* s, uint16_t idx) { return &s->pool[idx]; }
static uint16_t slot_alloc(slab_t* s) {
    if (s->free_tail == s->free_head) return SLOT_NONE;   // pool exhausted
    uint16_t idx = s->free_ring[s->free_tail];
    s->free_tail = (s->free_tail + 1) % RING_CAP;
    return idx;
}
static void slot_free(slab_t* s, uint16_t idx) {
    s->free_ring[s->free_head] = idx;
    s->free_head = (s->free_head + 1) % RING_CAP;
}
static void slab_init(slab_t* s) {
    for (uint16_t i = 0; i < POOL_SIZE; i++) s->free_ring[i] = i;
    s->free_tail = 0; s->free_head = POOL_SIZE;   // POOL_SIZE free slots enqueued
}

static slab_t g_slab;   // the driver's slab instance


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

/********* the pipeline's rings (each init'd via ring_init in main) ***********/
static idx_ring dispatch_in;   // parsed + recirculated elements awaiting handling
static idx_ring tx_in;         // handled elements awaiting fan-out + deparse + TX


#define RXBUF_SIZE 65536

#ifdef USE_BPF
static unsigned int g_bpf_blen = RXBUF_SIZE; // actual BPF read size (set at open)
#endif

// open a raw socket bound to ifname; returns the fd or -1.
static int raw_open(const char* ifname) {
#ifdef USE_AF_PACKET
    int fd = socket(AF_PACKET, SOCK_RAW, htons(ETH_P_ALL));
    if (fd < 0) { perror("socket(AF_PACKET)"); return -1; }
    int ifidx = if_nametoindex(ifname);
    if (ifidx == 0) { perror("if_nametoindex"); close(fd); return -1; }
    struct sockaddr_ll sll;
    memset(&sll, 0, sizeof(sll));
    sll.sll_family = AF_PACKET;
    sll.sll_ifindex = ifidx;
    sll.sll_protocol = htons(ETH_P_ALL);
    if (bind(fd, (struct sockaddr*)&sll, sizeof(sll)) < 0) { perror("bind"); close(fd); return -1; }
    // don't deliver our own outgoing frames back to us (avoids reflect loops)
#ifdef PACKET_IGNORE_OUTGOING
    { int one = 1; setsockopt(fd, SOL_PACKET, PACKET_IGNORE_OUTGOING, &one, sizeof(one)); }
#endif
    // promiscuous so we see all frames on the wire
    { struct packet_mreq mr; memset(&mr, 0, sizeof(mr));
      mr.mr_ifindex = ifidx; mr.mr_type = PACKET_MR_PROMISC;
      setsockopt(fd, SOL_PACKET, PACKET_ADD_MEMBERSHIP, &mr, sizeof(mr)); }
    fcntl(fd, F_SETFL, O_NONBLOCK);
    return fd;
#else // USE_BPF
    int fd = -1;
    for (int i = 0; i < 99; i++) {
        char path[16]; snprintf(path, sizeof(path), "/dev/bpf%d", i);
        fd = open(path, O_RDWR);
        if (fd >= 0) break;
        if (errno == EBUSY) continue;
        break;
    }
    if (fd < 0) { perror("open(/dev/bpf)"); return -1; }
    unsigned int enable = 1, disable = 0, blen = RXBUF_SIZE;
    ioctl(fd, BIOCSSEESENT, &disable);  // don't see our own sent frames (avoids loops)
    ioctl(fd, BIOCSHDRCMPLT, &enable);  // we supply complete link-layer headers
    ioctl(fd, BIOCSBLEN, &blen);        // set + read back the kernel buffer length
    g_bpf_blen = blen;
    struct ifreq ifr; memset(&ifr, 0, sizeof(ifr));
    strncpy(ifr.ifr_name, ifname, IFNAMSIZ - 1);
    if (ioctl(fd, BIOCSETIF, &ifr) < 0) { perror("BIOCSETIF"); close(fd); return -1; }
    ioctl(fd, BIOCIMMEDIATE, &enable);  // deliver each packet immediately
    ioctl(fd, BIOCPROMISC, NULL);
    fcntl(fd, F_SETFL, O_NONBLOCK);
    return fd;
#endif
}

// a burst of freshly-read slab slots (pkt_len set, not yet parsed).
typedef struct { uint16_t n; uint16_t idx[BURST]; } rx_batch;

// read up to BURST frames from one port's socket into freshly-allocated slab slots and
// return them (pkt_len set); does NOT parse -- do_rx ingests each.
static rx_batch port_rx(int fd) {
    rx_batch batch; batch.n = 0;
#ifdef USE_AF_PACKET
    while (batch.n < BURST) {
        uint16_t idx = slot_alloc(&g_slab);
        if (idx == SLOT_NONE) break;                     // pool exhausted -> drop-at-birth
        qe_t* q = slot(&g_slab, idx);
        ssize_t n = read(fd, q->data + HEADROOM, SLOT_USABLE);
        if (n <= 0) { slot_free(&g_slab, idx); break; }   // EWOULDBLOCK/error -> done with this port
        q->pkt_len = (uint32_t)n;
        batch.idx[batch.n++] = idx;
    }
#else // USE_BPF: one read yields a buffer of bpf_hdr-prefixed frames (capped at BURST here;
      // BPF is the dev-only path, so dropping a rare >BURST single-read burst is acceptable)
    static uint8_t rxbuf[RXBUF_SIZE];
    ssize_t n = read(fd, rxbuf, g_bpf_blen);
    if (n > 0) {
        uint8_t* ptr = rxbuf; uint8_t* end = rxbuf + n;
        while (ptr + sizeof(struct bpf_hdr) <= end && batch.n < BURST) {
            struct bpf_hdr* bh = (struct bpf_hdr*)ptr;
            if (bh->bh_caplen == bh->bh_datalen) {        // skip truncated captures
                uint16_t idx = slot_alloc(&g_slab);
                if (idx == SLOT_NONE) break;              // pool exhausted -> drop-at-birth
                uint32_t flen = bh->bh_caplen; if (flen > SLOT_USABLE) flen = SLOT_USABLE;
                qe_t* q = slot(&g_slab, idx);
                memcpy(q->data + HEADROOM, ptr + bh->bh_hdrlen, flen);
                q->pkt_len = flen;
                batch.idx[batch.n++] = idx;
            }
            ptr += BPF_WORDALIGN(bh->bh_hdrlen + bh->bh_caplen);
        }
    }
#endif
    return batch;
}


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
    for (int p = 0; p < g_nports; p++) {
        rx_batch batch = port_rx(g_ports[p].fd);
        for (uint16_t i = 0; i < batch.n; i++) ingest_slot(batch.idx[i], g_ports[p].port_id);
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
                int fd = port_fd((int)oe->port);
                if (fd < 0) { debug_printf("no interface for port %u\n", oe->port); slot_free(&g_slab, cidx); continue; }
                packet_t view;
                init_cursor(c->data + HEADROOM, plen, &view); // cursor at the payload boundary (front)
                deparse_event(&c->ev, &view);      // writes the header backwards into headroom
                // a no-payload event emits only its header (drop the tail); a payload event keeps it.
                uint8_t* dump_end = oe->ev.meta.has_payload ? (c->data + HEADROOM + plen) : (c->data + HEADROOM);
                size_t out_len = (size_t)(dump_end - view.cursor);
                ssize_t w = write(fd, view.cursor, out_len);
                if (w < 0) debug_printf("write to port %u failed: %s\n", oe->port, strerror(errno));
                slot_free(&g_slab, cidx);                // egress done
            }
        }
        slot_free(&g_slab, idx);                         // input consumed (cloned per output)
    }
}


static volatile int g_running = 1;

int main(int argc, char** argv) {
    // parse `--interface PORT:IFNAME` args (same form as lucidSwitch)
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--interface") == 0 && i + 1 < argc) {
            char* spec = argv[++i];
            char* colon = strchr(spec, ':');
            if (!colon) { fprintf(stderr, "bad --interface '%s' (expected PORT:IFNAME)\n", spec); return 1; }
            *colon = '\0';
            int port_id = atoi(spec);
            const char* ifname = colon + 1;
            if (g_nports >= MAX_PORTS) { fprintf(stderr, "too many interfaces (max %d)\n", MAX_PORTS); return 1; }
            int fd = raw_open(ifname);
            if (fd < 0) { fprintf(stderr, "failed to open interface '%s' for port %d\n", ifname, port_id); return 1; }
            g_ports[g_nports].port_id = port_id;
            g_ports[g_nports].fd = fd;
            strncpy(g_ports[g_nports].ifname, ifname, IFNAMSIZ - 1);
            g_nports++;
            printf("bound port %d to interface %s\n", port_id, ifname);
        } else {
            // ignore unknown args (e.g. the .dpt path, for argv-compatibility with lucidSwitch)
        }
    }
    if (g_nports == 0) {
        fprintf(stderr, "usage: %s --interface PORT:IFNAME [--interface PORT:IFNAME ...]\n", argv[0]);
        return 1;
    }

    slab_init(&g_slab);
    ring_init(&dispatch_in);
    ring_init(&tx_in);
    // the test harness waits for this line on stdout before sending traffic
    printf("Init complete.\n");
    fflush(stdout);

    // the pipeline loop: rx -> dispatch -> tx, each a bounded burst. select() blocks
    // when everything is idle, but only polls (zero timeout) while there is queued
    // dispatch/tx work -- so recirculation makes progress without waiting on a read,
    // and an endless self-recirculating handler still can't block fresh input.
    while (g_running) {
        int have_work = !ring_empty(&dispatch_in) || !ring_empty(&tx_in);
        fd_set rfds; FD_ZERO(&rfds); int maxfd = 0;
        for (int i = 0; i < g_nports; i++) {
            FD_SET(g_ports[i].fd, &rfds);
            if (g_ports[i].fd > maxfd) maxfd = g_ports[i].fd;
        }
        struct timeval zero = {0, 0};
        int r = select(maxfd + 1, &rfds, NULL, NULL, have_work ? &zero : NULL);
        if (r < 0) { if (errno == EINTR) continue; perror("select"); break; }
        do_rx();        // read available frames into slots, enqueue on dispatch_in
        do_dispatch();  // handle a bounded burst, forward to tx_in
        do_tx();        // fan out a bounded burst: deparse+send; recirc re-enqueues
    }
    printf("Processed %llu packets\n", (unsigned long long)pkt_ct);
    return 0;
}
