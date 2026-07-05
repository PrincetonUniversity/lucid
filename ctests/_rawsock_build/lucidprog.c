
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
  } payload;
} event_variant;
event_variant ethpkt_673(uint64_t dst_669 , uint64_t src_670 , uint16_t ety_671 ){
  event_variant ev  = {0};
  ev.payload.ethpkt_673.dst_669 = dst_669;
  ev.payload.ethpkt_673.src_670 = src_670;
  ev.payload.ethpkt_673.ety_671 = ety_671;
  ev.tag = ethpkt_tag;
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
events mk_ethpkt(uint64_t dst_669 , uint64_t src_670 , uint16_t ety_671 ){
  events tmp_713  = {.meta = {.len = 14, .is_packet = 1, .has_payload = 1, .timestamp = 0, .in_port = 0}, .data = ethpkt_673(dst_669, src_670, ety_671)};
  return tmp_713;
}
uint32_t recirculation_port  = 0;
uint32_t self  = 0;
uint8_t parse_event(packet_t*  pkt , events*  next_event ){
  uint64_t dst_669  = ((uint64_t)(read_bits(pkt, 48))) & 281474976710655;
  uint64_t src_670  = ((uint64_t)(read_bits(pkt, 48))) & 281474976710655;
  uint16_t ety_671  = ((uint16_t)(read_bits(pkt, 16)));
  (*(next_event)) = mk_ethpkt(dst_669, src_670, ety_671);
  return pkt->cursor <= pkt->end;
}
uint16_t handle_event(events*  ev_in , out_event out_events [64]){
  uint16_t n  = 0;
  switch (ev_in->data.tag) {
    case 1: {
      uint64_t dst_674  = ev_in->data.payload.ethpkt_673.dst_669;
      uint64_t src_675  = ev_in->data.payload.ethpkt_673.src_670;
      uint16_t ety_676  = ev_in->data.payload.ethpkt_673.ety_671;
      events this  = mk_ethpkt(dst_674, src_675, ety_676);
      out_event tmp_714  = {.ev = mk_ethpkt(src_675, dst_674, ety_676), .out_loc = 2, .port = ev_in->meta.in_port};
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
void deparse_event(events*  ev_out , packet_t*  buf_out ){
  switch (ev_out->data.tag) {
    case 1: {
      uint64_t dst_669  = ev_out->data.payload.ethpkt_673.dst_669;
      uint64_t src_670  = ev_out->data.payload.ethpkt_673.src_670;
      uint16_t ety_671  = ev_out->data.payload.ethpkt_673.ety_671;
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

// forward decl: dispatch one parsed packet from the given ingress port
static void dispatch_packet(int ingress_port, uint8_t* pkt, uint32_t len);

// hand a freshly-read raw buffer (n bytes) to dispatch_packet, one frame at a time.
static void process_rx(int ingress_port, uint8_t* buf, ssize_t n) {
#ifdef USE_AF_PACKET
    dispatch_packet(ingress_port, buf, (uint32_t)n);
#else // BPF: the buffer holds zero or more bpf_hdr-prefixed frames
    uint8_t* p = buf;
    uint8_t* end = buf + n;
    while (p + sizeof(struct bpf_hdr) <= end) {
        struct bpf_hdr* bh = (struct bpf_hdr*)p;
        if (bh->bh_caplen == bh->bh_datalen) // skip truncated captures
            dispatch_packet(ingress_port, p + bh->bh_hdrlen, bh->bh_caplen);
        p += BPF_WORDALIGN(bh->bh_hdrlen + bh->bh_caplen);
    }
#endif
}


/********* internal dispatch FIFO of events ***********/
#define EV_QUEUE_CAP 1024
typedef struct ev_queue_t {
    events buf[EV_QUEUE_CAP];
    int head; int tail; int count;
} ev_queue_t;
static int  evq_empty(ev_queue_t* q) { return q->count == 0; }
static void evq_push(ev_queue_t* q, events* ev) {
    q->buf[q->tail] = *ev; q->tail = (q->tail + 1) % EV_QUEUE_CAP; q->count++;
}
static void evq_pull(ev_queue_t* q, events* out) {
    *out = q->buf[q->head]; q->head = (q->head + 1) % EV_QUEUE_CAP; q->count--;
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

/********* single dispatch context (single-threaded) ***********/
// Deparse writes backwards (prepending headers); HEADROOM reserves slack at the front
// of the out buffer so a larger output header doesn't underflow it (see pcap driver).
#define HEADROOM 256
#define OUTBUF_USABLE 1600
static uint8_t g_outbuf[OUTBUF_USABLE + HEADROOM];
static packet_t g_in_pkt;
static packet_t g_out_pkt;
static ev_queue_t g_queue;
static uint64_t pkt_ct = 0;

// a 32-bit nanosecond timestamp, stamped onto each event at dequeue (Sys.time())
static uint32_t now_ns(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint32_t)((uint64_t)ts.tv_sec * 1000000000ull + (uint64_t)ts.tv_nsec);
}

static void init_ctx(void) {
    g_out_pkt.start  = g_outbuf + HEADROOM;
    g_out_pkt.cursor = g_outbuf + HEADROOM;
    g_out_pkt.end    = g_outbuf + HEADROOM + OUTBUF_USABLE;
    g_out_pkt.bit_off = 0;
    g_queue.head = g_queue.tail = g_queue.count = 0;
}

static void dispatch_packet(int ingress_port, uint8_t* pkt, uint32_t len) {
    init_cursor(pkt, len, &g_in_pkt);
    events ev0;
    if (parse_event(&g_in_pkt, &ev0) != 1) { debug_printf("parse failed\n"); return; }
    evq_push(&g_queue, &ev0);

    while (!evq_empty(&g_queue)) {
        events ev;
        evq_pull(&g_queue, &ev);
        ev.meta.timestamp = now_ns(); // stamp at dequeue (covers arriving + recirculated events)
        ev.meta.in_port = ingress_port; // ingress port (read by the handler)
        out_event out_events[64];
        uint16_t n = handle_event(&ev, out_events);
        for (uint16_t i = 0; i < n; i++) {
            if (out_events[i].out_loc == 1) {
                // recirculation: re-queue for dispatch
                evq_push(&g_queue, &out_events[i].ev);
            } else if (out_events[i].out_loc == 2) {
                // output to a port: deparse over a copy of the input packet, then write
                // it to that port's interface socket.
                int out_port = (int)out_events[i].port;
                int fd = port_fd(out_port);
                if (fd < 0) { debug_printf("no interface for port %d\n", out_port); continue; }
                reset_cursor(&g_out_pkt);
                copy_packet(&g_out_pkt, &g_in_pkt);
                // a no-payload event emits only its header (drop the input tail); a
                // payload event keeps the tail (matches interp). boundary = cursor before
                // deparse prepends the header.
                uint8_t* payload_boundary = g_out_pkt.cursor;
                deparse_event(&out_events[i].ev, &g_out_pkt);
                uint8_t* dump_end = out_events[i].ev.meta.has_payload ? g_out_pkt.end : payload_boundary;
                size_t out_len = (size_t)(dump_end - g_out_pkt.cursor);
                ssize_t w = write(fd, g_out_pkt.cursor, out_len);
                if (w < 0) debug_printf("write to port %d failed: %s\n", out_port, strerror(errno));
            }
        }
    }
    pkt_ct++;
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

    init_ctx();
    // the test harness waits for this line on stdout before sending traffic
    printf("Init complete.\n");
    fflush(stdout);

    uint8_t rxbuf[RXBUF_SIZE];
#ifdef USE_BPF
    size_t rxlen = g_bpf_blen;          // BPF reads must use the configured buffer length
#else
    size_t rxlen = RXBUF_SIZE;
#endif

    while (g_running) {
        fd_set rfds; FD_ZERO(&rfds); int maxfd = 0;
        for (int i = 0; i < g_nports; i++) {
            FD_SET(g_ports[i].fd, &rfds);
            if (g_ports[i].fd > maxfd) maxfd = g_ports[i].fd;
        }
        int r = select(maxfd + 1, &rfds, NULL, NULL, NULL);
        if (r < 0) { if (errno == EINTR) continue; perror("select"); break; }
        for (int i = 0; i < g_nports; i++) {
            if (!FD_ISSET(g_ports[i].fd, &rfds)) continue;
            // drain this socket until it would block
            for (;;) {
                ssize_t n = read(g_ports[i].fd, rxbuf, rxlen);
                if (n <= 0) break; // EAGAIN/EWOULDBLOCK or error
                process_rx(g_ports[i].port_id, rxbuf, n);
            }
        }
    }
    printf("Processed %llu packets\n", (unsigned long long)pkt_ct);
    return 0;
}
