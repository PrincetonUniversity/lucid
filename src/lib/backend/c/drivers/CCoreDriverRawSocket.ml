open CCoreSyntax
open CCoreUtils

(* ---- names of compiler-generated constructs referenced in the driver's raw C,
   taken from the cids the codegen emits and inlined with %{...} below, so the driver
   tracks generated names instead of hard-coding them (mirrors the dpdk driver). ---- *)
let events_ty        = CCoreCPrint.cid_to_string events_cid
let out_event_ty     = CCoreCPrint.cid_to_string CCoreHandlers.out_event_cid
let packet_t_ty      = CCoreCPrint.cid_to_string
  (match CCoreParse.packet_t.raw_ty with TName c -> c | _ -> failwith "packet_t is not a named type")
let parse_event_fn   = CCoreCPrint.cid_to_string CCoreParse.parser_cid
let deparse_event_fn = CCoreCPrint.cid_to_string CCoreParse.deparse_id
let handle_event_fn  = CCoreCPrint.cid_to_string CCoreHandlers.handler_cid
let out_events_cap   = string_of_int CCoreHandlers.out_events_cap
(* sentinel port value that marks a recirculated (generate_self) out_event *)
let port_recirc      = string_of_int CCoreHandlers.port_recirc

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

(* the raw-socket layer: open a socket bound to an interface (raw_open), and read a
   bounded burst of frames from one into slab slots (port_rx: AF_PACKET delivers one
   frame per read; BPF a buffer of bpf_hdr-prefixed frames). Adapted from
   rawlink_stubs.c. do_rx (below) ingests -- parses + enqueues -- each slot read.
   Emitted after the slab block, which port_rx allocates from. *)
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

// a burst of freshly-read slab slots (pkt_len set, not yet parsed).
typedef struct { uint16_t n; uint16_t idx[BURST]; } rx_batch;

// read up to BURST frames from one port's socket into slots freshly allocated from `s`
// and return them (pkt_len set); does NOT parse -- do_rx ingests each.
static rx_batch port_rx(int fd, slab_t* s) {
    rx_batch batch; batch.n = 0;
#ifdef USE_AF_PACKET
    while (batch.n < BURST) {
        uint16_t idx = slot_alloc(s);
        if (idx == SLOT_NONE) break;                     // pool exhausted -> drop-at-birth
        qe_t* q = slot(s, idx);
        ssize_t n = read(fd, q->data + HEADROOM, SLOT_USABLE);
        if (n <= 0) { slot_free(s, idx); break; }         // EWOULDBLOCK/error -> done with this port
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
                uint16_t idx = slot_alloc(s);
                if (idx == SLOT_NONE) break;              // pool exhausted -> drop-at-birth
                uint32_t flen = bh->bh_caplen; if (flen > SLOT_USABLE) flen = SLOT_USABLE;
                qe_t* q = slot(s, idx);
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
|}

(* ===== helpers: the port table, packet counter, and Sys.time() clock ===== *)
let driver_helpers = dforiegn {|
/********* ports (Lucid port number <-> interface socket) ***********/
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

(* ===== sizing constants, shared by the slab pool and the pipeline rings ===== *)
let config = dforiegn {|
/********* sizing constants (shared by the slab pool and the index rings) ***********/
#define HEADROOM 256               // slack before the packet for deparse to prepend a header
#define SLOT_USABLE 1600           // max packet bytes per slot
#define POOL_SIZE 1024             // number of slab slots (buffers in flight)
#define RING_CAP (POOL_SIZE + 1)   // ring capacity: a head/tail ring holds CAP-1 items, so this
                                   // holds up to POOL_SIZE indices (all slots free, or all in one ring)
#define BURST 64                   // max frames/elements handled per rx/dispatch/tx call
#define SLOT_NONE 0xFFFF           // slot_alloc's "pool exhausted" sentinel
#define MAX_PORTS 64               // max interfaces (Lucid ports) bound at once
|}

(* ===== the slab allocator: a hand-rolled rte_mempool -- a fixed pool of queue elements
   whose free-list is itself an idx_ring (so there is one ring implementation). Each slot
   OWNS its packet bytes; see §29. Pure library: types + helpers taking a slab_t* (the
   driver's instance lives in the `state` block). Emitted after rings, whose idx_ring the
   free-list uses. ===== *)
let slab = dforiegn [%string {|
/********* the queue element (a slab slot) ***********/
// Mirrors the DPDK qe_priv_t + mbuf data region: the event, the handler's outputs,
// and the packet bytes this element OWNS. The packet occupies data[HEADROOM ..
// HEADROOM+pkt_len); payload_off marks where the payload begins within it.
typedef struct {
    %{events_ty}    ev;
    %{out_event_ty} out_events[%{out_events_cap}];
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
|}]

(* ===== the index ring: a small FIFO of uint16_t slot indices (a hand-rolled rte_ring).
   Pure library (the instances -- the slab's free-list and the pipeline rings -- live in the
   slab_t and the `state` block). ring_init takes one ring, so each is initialized explicitly. ===== *)
let rings = dforiegn {|
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
|}

(* ===== the driver's runtime state: instances of the slab + ring libraries. Kept out of
   the library blocks so those stay pure (types + functions). Emitted just before the
   stages that use it. ===== *)
let state = dforiegn {|
/********* the driver's runtime state (instances of the slab + ring libraries) ***********/
static slab_t   g_slab;        // the packet-buffer pool
static idx_ring dispatch_in;   // parsed + recirculated elements awaiting handling
static idx_ring tx_in;         // handled elements awaiting fan-out + deparse + TX
|}

(* ===== RX: parse + enqueue the frames port_rx (socket_layer) read from each port,
   mirroring the dpdk driver's rx_burst-then-loop shape. ===== *)
let rx = dforiegn [%string {|
// parse the frame sitting in slot `idx` (pkt_len bytes at data+HEADROOM) into its event
// and enqueue it for dispatch; drop (free the slot) on parse failure.
static void ingest_slot(uint16_t idx, int in_port) {
    qe_t* q = slot(&g_slab, idx);
    %{packet_t_ty} view;
    init_cursor(q->data + HEADROOM, q->pkt_len, &view);
    if (%{parse_event_fn}(&view, &q->ev) != 1) { debug_printf("parse failed\n"); slot_free(&g_slab, idx); return; }
    q->payload_off = (uint32_t)(view.cursor - (q->data + HEADROOM)); // where the payload begins
    q->ev.meta.in_port = in_port;                                    // ingress (read by the handler)
    if (ring_push(&dispatch_in, idx) != 0) slot_free(&g_slab, idx);  // ring full (shouldn't happen)
    else pkt_ct++;
}

/******** RX: read a bounded burst from every port, parse + enqueue each on dispatch_in ********/
static void do_rx(void) {
    for (int p = 0; p < g_nports; p++) {
        rx_batch batch = port_rx(g_ports[p].fd, &g_slab);
        for (uint16_t i = 0; i < batch.n; i++) ingest_slot(batch.idx[i], g_ports[p].port_id);
    }
}
|}]

(* ===== DISPATCH: handle a bounded burst of elements; forward each to tx_in (no copy) ===== *)
let dispatch = dforiegn [%string {|
/******** DISPATCH: handle a bounded burst of elements; hand each to tx_in (no copy) ********/
static void do_dispatch(void) {
    for (int b = 0; b < BURST; b++) {
        uint16_t idx;
        if (ring_pop(&dispatch_in, &idx) != 0) break;
        qe_t* q = slot(&g_slab, idx);
        q->ev.meta.timestamp = now_ns();                    // stamp at dequeue (arriving + recirculated)
        q->n_out = %{handle_event_fn}(&q->ev, q->out_events); // ingress read from q->ev.meta.in_port
        if (ring_push(&tx_in, idx) != 0) slot_free(&g_slab, idx); // ring full (shouldn't happen) -> drop
    }
}
|}]

(* ===== TX: fan out each element into one owned clone per output, then route each
   (recirc -> dispatch_in, else deparse + write to the port). ===== *)
let tx = dforiegn [%string {|
#define PORT_RECIRC %{port_recirc}u // out_event.port sentinel: recirculate, don't egress

/******** TX: fan out each element into one owned clone per output, then route/deparse/send ********/
static void do_tx(void) {
    for (int b = 0; b < BURST; b++) {
        uint16_t idx;
        if (ring_pop(&tx_in, &idx) != 0) break;
        qe_t* in = slot(&g_slab, idx);
        for (uint16_t i = 0; i < in->n_out; i++) {
            %{out_event_ty}* oe = &in->out_events[i];
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
                %{packet_t_ty} view;
                init_cursor(c->data + HEADROOM, plen, &view); // cursor at the payload boundary (front)
                %{deparse_event_fn}(&c->ev, &view);      // writes the header backwards into headroom
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
|}

let package_prog decls =
  [
    "lucidprog.c", `Decls (imports @ decls @ helpers @
      [config; rings; slab; driver_helpers; socket_layer; state; rx; dispatch; tx; main]);
    "makefile", `String "all: lucidprog\n\nlucidprog: lucidprog.c\n\tgcc -O2 -o lucidprog lucidprog.c\n\n"
  ]
;;
