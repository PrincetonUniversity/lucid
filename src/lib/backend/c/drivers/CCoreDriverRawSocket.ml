open CCoreSyntax
open CCoreUtils

(* names of compiler-generated types referenced in this driver's raw C, taken from
   the cids the codegen emits and inlined with %{...} below (see the dpdk driver). *)
let events_ty    = CCoreCPrint.cid_to_string events_cid
let out_event_ty = CCoreCPrint.cid_to_string CCoreHandlers.out_event_cid
(* sentinel port value that marks a recirculated (generate_self) out_event *)
let port_recirc  = string_of_int CCoreHandlers.port_recirc
let out_events_cap = string_of_int CCoreHandlers.out_events_cap

(* Raw-socket driver: run a compiled Lucid program on real POSIX network
   interfaces, the same way the `lucidSwitch` interpreter does (AF_PACKET raw
   sockets on Linux, /dev/bpf on macOS -- the C here is adapted from the vendored
   rawlink_stubs.c, minus the OCaml glue).

   Ports are wired to interfaces at runtime, exactly like lucidSwitch:
     ./lucidprog --interface 0:veth0 --interface 1:veth1
   binds Lucid port 0 to veth0 and port 1 to veth1. A packet read on a port's
   socket is dispatched with that port as its ingress port; a port output event is
   written to the target port's socket.

   Architecture (§29): the same queue-separated rx -> dispatch -> tx pipeline as the
   DPDK reference driver (§28), with the DPDK primitives hand-rolled -- a fixed slab
   of packet buffers with a free-list stands in for rte_mempool, and index rings for
   rte_ring. Each slab slot is a queue element that OWNS its bytes (like an mbuf), so
   a recirculated event gets its own cloned buffer and survives across dispatch
   iterations. That lets dispatch run in bounded bursts: an endless self-recirculating
   handler can't starve fresh input (non-blocking recirculation, matching the DPDK
   driver and the interpreter). *)

(* reuse the cursor helper from the pcap driver (identical) *)
let helpers = [
  CCoreDriverPcap.init_cursor;
]

(* all includes in one block so the platform conditionals + ordering are explicit
   (mirrors the vendored rawlink_stubs.c include order, which is known to work) *)
let imports = [
  dforiegn {|
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
|}
]

(* the raw-socket layer: open a socket bound to an interface. (Turning a raw read
   into 1+ frames -- AF_PACKET delivers one per read; BPF a buffer of bpf_hdr-prefixed
   frames -- happens in do_rx below.) Adapted from rawlink_stubs.c. *)
let socket_layer = dforiegn {|
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
|}

(* ===== helpers: the port table, packet counter, and Sys.time() clock ===== *)
let driver_helpers = dforiegn {|
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
|}

(* ===== the slab allocator: a hand-rolled rte_mempool (the queue-element pool + its
   free-list) plus the two index rings (rte_ring) connecting the stages. Each slot OWNS
   its packet bytes; see §29. ===== *)
let slab = dforiegn [%string {|
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
    %{events_ty}    ev;
    %{out_event_ty} out_events[%{out_events_cap}];
    uint16_t        n_out;
    uint32_t        pkt_len;        // packet bytes from data+HEADROOM
    uint32_t        payload_off;    // payload boundary, relative to data+HEADROOM
    uint8_t         data[HEADROOM + SLOT_USABLE];
} qe_t;

static qe_t g_pool[POOL_SIZE];

/********* the pool's free list (a ring of free slot indices; no ABA, parallel-ready) ***********/
static uint16_t g_free_ring[RING_CAP];
static uint32_t g_free_head, g_free_tail;   // free indices sit in ring[tail .. head)
static uint16_t slot_alloc(void) {
    if (g_free_tail == g_free_head) return SLOT_NONE;   // pool exhausted
    uint16_t idx = g_free_ring[g_free_tail];
    g_free_tail = (g_free_tail + 1) % RING_CAP;
    return idx;
}
static void slot_free(uint16_t idx) {
    g_free_ring[g_free_head] = idx;
    g_free_head = (g_free_head + 1) % RING_CAP;
}

/********* pipeline rings (index rings connecting the stages) ***********/
typedef struct { uint16_t buf[RING_CAP]; uint32_t head, tail; } idx_ring;
static idx_ring dispatch_in;   // parsed + recirculated elements awaiting handling
static idx_ring tx_in;         // handled elements awaiting fan-out + deparse + TX
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

static void pool_init(void) {
    for (uint16_t i = 0; i < POOL_SIZE; i++) g_free_ring[i] = i;
    g_free_tail = 0; g_free_head = POOL_SIZE;     // POOL_SIZE free slots enqueued
    dispatch_in.head = dispatch_in.tail = 0;
    tx_in.head = tx_in.tail = 0;
}
|}]

(* ===== RX: read a bounded burst of frames per port into slab slots and enqueue them.
   The per-port, platform-specific read is factored into port_rx (AF_PACKET: one frame
   per read; BPF: a buffer of bpf_hdr-prefixed frames); do_rx fans it over the ports. ===== *)
let rx = dforiegn {|
// parse the frame already sitting in slot `idx` (pkt_len bytes at data+HEADROOM) into
// its event and enqueue it for dispatch; drop (free the slot) on parse failure.
static void ingest_slot(uint16_t idx, int in_port) {
    qe_t* q = &g_pool[idx];
    packet_t view;
    init_cursor(q->data + HEADROOM, q->pkt_len, &view);
    if (parse_event(&view, &q->ev) != 1) { debug_printf("parse failed\n"); slot_free(idx); return; }
    q->payload_off = (uint32_t)(view.cursor - (q->data + HEADROOM)); // where the payload begins
    q->ev.meta.in_port = in_port;                                    // ingress (read by the handler)
    if (ring_push(&dispatch_in, idx) != 0) slot_free(idx);           // ring full (shouldn't happen)
    else pkt_ct++;
}

// read up to a burst of frames from one port's socket into slab slots and ingest them.
static inline void port_rx(int fd, int in_port) {
#ifdef USE_AF_PACKET
    for (int b = 0; b < BURST; b++) {
        uint16_t idx = slot_alloc();
        if (idx == SLOT_NONE) return;                   // pool exhausted -> drop-at-birth
        ssize_t n = read(fd, g_pool[idx].data + HEADROOM, SLOT_USABLE);
        if (n <= 0) { slot_free(idx); return; }          // EWOULDBLOCK/error -> done with this port
        g_pool[idx].pkt_len = (uint32_t)n;
        ingest_slot(idx, in_port);
    }
#else // USE_BPF: one read yields a buffer of bpf_hdr-prefixed frames
    static uint8_t rxbuf[RXBUF_SIZE];
    ssize_t n = read(fd, rxbuf, g_bpf_blen);
    if (n <= 0) return;
    uint8_t* ptr = rxbuf; uint8_t* end = rxbuf + n;
    while (ptr + sizeof(struct bpf_hdr) <= end) {
        struct bpf_hdr* bh = (struct bpf_hdr*)ptr;
        if (bh->bh_caplen == bh->bh_datalen) {           // skip truncated captures
            uint16_t idx = slot_alloc();
            if (idx == SLOT_NONE) return;                // pool exhausted -> drop-at-birth
            uint32_t flen = bh->bh_caplen; if (flen > SLOT_USABLE) flen = SLOT_USABLE;
            memcpy(g_pool[idx].data + HEADROOM, ptr + bh->bh_hdrlen, flen);
            g_pool[idx].pkt_len = flen;
            ingest_slot(idx, in_port);
        }
        ptr += BPF_WORDALIGN(bh->bh_hdrlen + bh->bh_caplen);
    }
#endif
}

/******** RX: read a bounded burst from every port into slab slots, enqueue on dispatch_in ********/
static void do_rx(void) {
    for (int p = 0; p < g_nports; p++) port_rx(g_ports[p].fd, g_ports[p].port_id);
}
|}

(* ===== DISPATCH: handle a bounded burst of elements; forward each to tx_in (no copy) ===== *)
let dispatch = dforiegn {|
/******** DISPATCH: handle a bounded burst of elements; hand each to tx_in (no copy) ********/
static void do_dispatch(void) {
    for (int b = 0; b < BURST; b++) {
        uint16_t idx;
        if (ring_pop(&dispatch_in, &idx) != 0) break;
        qe_t* q = &g_pool[idx];
        q->ev.meta.timestamp = now_ns();                 // stamp at dequeue (arriving + recirculated)
        q->n_out = handle_event(&q->ev, q->out_events);  // ingress read from q->ev.meta.in_port
        if (ring_push(&tx_in, idx) != 0) slot_free(idx); // ring full (shouldn't happen) -> drop
    }
}
|}

(* ===== TX: fan out each element into one owned clone per output, then route each
   (recirc -> dispatch_in, else deparse + write to the port). ===== *)
let tx = dforiegn [%string {|
#define PORT_RECIRC %{port_recirc}u // out_event.port sentinel: recirculate, don't egress

/******** TX: fan out each element into one owned clone per output, then route/deparse/send ********/
static void do_tx(void) {
    for (int b = 0; b < BURST; b++) {
        uint16_t idx;
        if (ring_pop(&tx_in, &idx) != 0) break;
        qe_t* in = &g_pool[idx];
        for (uint16_t i = 0; i < in->n_out; i++) {
            %{out_event_ty}* oe = &in->out_events[i];
            uint16_t cidx = slot_alloc();
            if (cidx == SLOT_NONE) continue;             // pool exhausted -> drop this output
            qe_t* c = &g_pool[cidx];
            c->ev = oe->ev;
            // the output owns a fresh copy of the input's payload (or none), placed at
            // data+HEADROOM with headroom in front for the deparsed header.
            uint32_t plen = oe->ev.meta.has_payload ? (in->pkt_len - in->payload_off) : 0;
            if (plen) memcpy(c->data + HEADROOM, in->data + HEADROOM + in->payload_off, plen);
            c->pkt_len = plen;
            c->payload_off = 0;                          // the payload now sits at the front of c
            if (oe->port == PORT_RECIRC) {               // recirculation (generate_self)
                c->ev.meta.in_port = in->ev.meta.in_port; // recirc inherits ingress
                if (ring_push(&dispatch_in, cidx) != 0) slot_free(cidx);
            } else {                                     // output to a port: deparse + send
                int fd = port_fd((int)oe->port);
                if (fd < 0) { debug_printf("no interface for port %u\n", oe->port); slot_free(cidx); continue; }
                packet_t view;
                init_cursor(c->data + HEADROOM, plen, &view); // cursor at the payload boundary (front)
                deparse_event(&c->ev, &view);            // writes the header backwards into headroom
                // a no-payload event emits only its header (drop the tail); a payload event keeps it.
                uint8_t* dump_end = oe->ev.meta.has_payload ? (c->data + HEADROOM + plen) : (c->data + HEADROOM);
                size_t out_len = (size_t)(dump_end - view.cursor);
                ssize_t w = write(fd, view.cursor, out_len);
                if (w < 0) debug_printf("write to port %u failed: %s\n", oe->port, strerror(errno));
                slot_free(cidx);                         // egress done
            }
        }
        slot_free(idx);                                  // input consumed (cloned per output)
    }
}
|}]

let main = dforiegn {|
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

    pool_init();
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
|}

let package_prog decls =
  [
    "lucidprog.c", `Decls (imports @ decls @ helpers @
      [socket_layer; driver_helpers; slab; rx; dispatch; tx; main]);
    "makefile", `String "all: lucidprog\n\nlucidprog: lucidprog.c\n\tgcc -O2 -o lucidprog lucidprog.c\n\n"
  ]
;;
