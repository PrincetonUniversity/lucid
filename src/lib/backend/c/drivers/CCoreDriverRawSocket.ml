open CCoreSyntax
open CCoreUtils

(* Raw-socket driver: run a compiled Lucid program on POSIX interfaces. 
   Intended to be compatible with the same platforms as lucidSwitch's 
   interpreter (AF_PACKET on Linux, bpf on macOS).
   C is copied / adapted from rawlink_stubs.c

   Ports are wired to interfaces at runtime, like lucidSwitch:
     ./lucidprog --interface 0:veth0 --interface 1:veth1
   binds Lucid port 0 to veth0 and port 1 to veth1.

   Data structures are a large part of this file: a simple ring buffer 
   and slab allocator for packet buffers. There is also a 
   port table (Lucid port number <-> interface socket). 

   The overall architecture is a pipeline with 3 stages: rx -> dispatch -> tx.
*)

(* compiler-generated constructs referenced in the driver's raw C *)
let events_ty        = CCoreCPrint.cid_to_string events_cid
let out_event_ty     = CCoreCPrint.cid_to_string CCoreHandlers.out_event_cid
let packet_t_ty      = CCoreCPrint.cid_to_string
  (match CCoreParse.packet_t.raw_ty with TName(c, _) -> c | _ -> failwith "packet_t is not a named type")
let packet_t_ptr_ty = packet_t_ty ^ "*"
let parse_event_fn   = CCoreCPrint.cid_to_string CCoreParse.parser_cid
let deparse_event_fn = CCoreCPrint.cid_to_string CCoreParse.deparse_id
let handle_event_fn  = CCoreCPrint.cid_to_string CCoreHandlers.handler_cid
let out_events_cap   = string_of_int CCoreHandlers.out_events_cap
let port_recirc      = string_of_int CCoreHandlers.port_recirc



let imports = dforiegn {|
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

(* constants, some shared by the slab pool and the pipeline rings *)
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

(* misc helpers: the port table, packet counter, and Sys.time() clock *)
let helpers_lib = dforiegn [%string {|
 void init_cursor(uint8_t*  buf , uint32_t len , %{packet_t_ptr_ty}  bytes ){
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
|}]

let port_map_lib = dforiegn {|
/********* ports (Lucid port number <-> interface socket) ***********/
typedef struct { int port_id; int fd; char ifname[IFNAMSIZ]; } port_t;
typedef struct { port_t ports[MAX_PORTS]; int nports; } port_map_t;
static int port_fd(int port_id, port_map_t* g_port_map) {
    for (int i = 0; i < g_port_map->nports; i++) if (g_port_map->ports[i].port_id == port_id) return g_port_map->ports[i].fd;
    return -1;
}
// get in / out descriptor. Two separate functions for pipeline compatibility with pcap driver.
static int get_in_descriptor(port_map_t* pm, int port_idx) { return pm->ports[port_idx].fd; }
static int get_out_descriptor(port_map_t* pm, int port_id)  { return port_fd(port_id, pm); }
int init_port_map(port_map_t* g_port_map, int argc, char** argv) {
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--interface") == 0 && i + 1 < argc) {
            char* spec = argv[++i];
            char* colon = strchr(spec, ':');
            if (!colon) { fprintf(stderr, "bad --interface '%s' (expected PORT:IFNAME)\n", spec); return 1; }
            *colon = '\0';
            int port_id = atoi(spec);
            const char* ifname = colon + 1;
            if (g_port_map->nports >= MAX_PORTS) { fprintf(stderr, "too many interfaces (max %d)\n", MAX_PORTS); return 1; }
            int fd = raw_open(ifname);
            if (fd < 0) { fprintf(stderr, "failed to open interface '%s' for port %d\n", ifname, port_id); return 1; }
            g_port_map->ports[g_port_map->nports].port_id = port_id;
            g_port_map->ports[g_port_map->nports].fd = fd;
            strncpy(g_port_map->ports[g_port_map->nports].ifname, ifname, IFNAMSIZ - 1);
            g_port_map->nports++;
            printf("bound port %d to interface %s\n", port_id, ifname);
        } else {
            // ignore unknown args (e.g. the .dpt path, for argv-compatibility with lucidSwitch)
        }
    }
    return 0;
}
|}


(* ===== the index ring: a small FIFO of slot indices (a hand-rolled rte_ring) ===== *)
let ring_lib = dforiegn {|
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


(* the slab allocator: a fixed pool of queue elements (a hand-rolled
   rte_mempool) whose free-list is an idx_ring. *)
let slab_lib = dforiegn [%string {|
/********* the queue element (a slab slot) ***********/
// the event, the handler's outputs, and the element's packet bytes (this driver's
// analogue of the DPDK mbuf + private area). The packet occupies data[HEADROOM ..
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


(* the raw-socket library is a simple inline block to open, rx, and tx from a
   raw socket interface. Adapted from rawlink_stubs.c. *)
let socket_lib = dforiegn {|
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

// egress: write a deparsed frame [buf, buf+len) out of the descriptor fd.
static void send_frame(int fd, uint8_t* buf, size_t len) {
    if (fd < 0) { debug_printf("send_frame: no egress descriptor (dropped)\n"); return; }
    if (write(fd, buf, len) < 0) debug_printf("write to fd %d failed: %s\n", fd, strerror(errno));
}
|}

(* Runtime state used by the stages *)
let pipe_state = dforiegn {|
/********* driver runtime state (instances of the slab + ring libraries) ***********/
static port_map_t   g_port_map;  // Lucid port <-> the driver's I/O (see its port_map_lib)
static slab_t       g_slab;        // the packet-buffer pool
static idx_ring     dispatch_in;   // parsed + recirculated elements awaiting handling
static idx_ring     tx_in;         // handled elements awaiting fan-out + deparse + TX
static uint64_t     pkt_ct = 0;    // rx packet counter
|}

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
    for (int p = 0; p < g_port_map.nports; p++) {
        rx_batch batch = port_rx(get_in_descriptor(&g_port_map, p), &g_slab);
        for (uint16_t i = 0; i < batch.n; i++) ingest_slot(batch.idx[i], g_port_map.ports[p].port_id);
    }
}
|}]

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

(* ===== TX: clone packets and route recircs back to dispatch input queue  ===== *)
let tx = dforiegn [%string {|
#define PORT_RECIRC %{port_recirc}u // out_event.port sentinel: recirculate, don't egress

/******** TX: fan out each element into one copy per output, then route/deparse/send ********/
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
            // each output gets its own copy of the input's payload (or none), with
            // headroom in front for the deparsed header.
            uint32_t plen = oe->ev.meta.has_payload ? (in->pkt_len - in->payload_off) : 0;
            if (plen) memcpy(c->data + HEADROOM, in->data + HEADROOM + in->payload_off, plen);
            c->pkt_len = plen;
            c->payload_off = 0;                          // the payload now sits at the front of c
            if (oe->port == PORT_RECIRC) {               // recirculation (generate_self)
                c->ev.meta.in_port = in->ev.meta.in_port; // recirc inherits ingress
                if (ring_push(&dispatch_in, cidx) != 0) slot_free(&g_slab, cidx);
            } else {                                     // output to a port: deparse + send
                %{packet_t_ty} view;
                init_cursor(c->data + HEADROOM, plen, &view); // cursor at the payload boundary (front)
                %{deparse_event_fn}(&c->ev, &view);      // writes the header backwards into headroom
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
|}]

let main = dforiegn {|
static volatile int g_running = 1;

int main(int argc, char** argv) {
    // parse `--interface PORT:IFNAME` args (same form as lucidSwitch)
    int ret = init_port_map(&g_port_map, argc, argv);
    if (ret != 0) {
        fprintf(stderr, "failed to init port map\n");
        return 1;
    }
    if (g_port_map.nports == 0) {
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
        for (int i = 0; i < g_port_map.nports; i++) {
            FD_SET(g_port_map.ports[i].fd, &rfds);
            if (g_port_map.ports[i].fd > maxfd) maxfd = g_port_map.ports[i].fd;
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
    let imports = [imports] in
    let decls = [section_marker "program code"] @ decls in
    let driver_config = [section_marker "driver config"; config] in
    (* order within the libs is dependency-driven: socket_lib's port_rx allocates from the
       slab (so after slab_lib), and port_map_lib's init_port_map calls raw_open (so after
       socket_lib). *)
    let driver_libs = [section_marker "driver libraries"; helpers_lib; ring_lib; slab_lib; socket_lib; port_map_lib;] in
    let pipe = [section_marker "driver pipeline"; pipe_state; rx; dispatch; tx;] in
    let main = [section_marker "driver main"; main] in
  [
    "lucidprog.c", `Decls (imports @ decls @ driver_config @ driver_libs @ pipe @ main);
    "makefile", `String "all: lucidprog\n\nlucidprog: lucidprog.c\n\tgcc -O2 -o lucidprog lucidprog.c\n\n"
  ]
;;
