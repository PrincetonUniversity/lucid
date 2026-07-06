open CCoreSyntax
open CCoreUtils

(* names of compiler-generated types referenced in this driver's raw C, taken from
   the cids the codegen emits and inlined with %{...} below (see the dpdk driver). *)
let events_ty    = CCoreCPrint.cid_to_string events_cid
let out_event_ty = CCoreCPrint.cid_to_string CCoreHandlers.out_event_cid

(* Raw-socket driver: run a compiled Lucid program on real POSIX network
   interfaces, the same way the `lucidSwitch` interpreter does (AF_PACKET raw
   sockets on Linux, /dev/bpf on macOS -- the C here is adapted from the vendored
   rawlink_stubs.c, minus the OCaml glue).

   Ports are wired to interfaces at runtime, exactly like lucidSwitch:
     ./lucidprog --interface 0:veth0 --interface 1:veth1
   binds Lucid port 0 to veth0 and port 1 to veth1. A packet read on a port's
   socket is dispatched with that port as its ingress port; a port output event is
   written to the target port's socket.

   The dispatch pipeline (parse -> queue -> handle -> {recirc | deparse+send}) is
   the same as the pcap driver; only the I/O endpoints differ (sockets vs pcap
   files) and the ingress port is real (the arriving interface) rather than a
   constant. *)

(* reuse the cursor/packet helpers from the pcap driver (identical) *)
let helpers = [
  CCoreDriverPcap.init_cursor;
  CCoreDriverPcap.reset_cursor;
  CCoreDriverPcap.copy_packet;
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

(* the raw-socket layer: open a socket bound to an interface, and turn a raw read
   into 1+ packets (AF_PACKET delivers one frame per read; BPF delivers a buffer of
   bpf_hdr-prefixed frames). Adapted from rawlink_stubs.c. *)
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
|}

(* ports, dispatch context, and the parse->queue->handle->send pipeline *)
let dispatch = dforiegn [%string {|
/********* internal dispatch FIFO of events ***********/
#define EV_QUEUE_CAP 1024
typedef struct ev_queue_t {
    %{events_ty} buf[EV_QUEUE_CAP];
    int head; int tail; int count;
} ev_queue_t;
static int  evq_empty(ev_queue_t* q) { return q->count == 0; }
static void evq_push(ev_queue_t* q, %{events_ty}* ev) {
    q->buf[q->tail] = *ev; q->tail = (q->tail + 1) % EV_QUEUE_CAP; q->count++;
}
static void evq_pull(ev_queue_t* q, %{events_ty}* out) {
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

// The dispatch pipeline is split into three phases, mirroring the DPDK reference
// driver (rx -> dispatch -> tx). Here they run synchronously per received frame
// rather than as ring-connected stages: the queue is drained to empty each frame,
// which keeps the input buffer (g_in_pkt) valid for the whole drain -- port events
// (even from recirculated events) reuse its payload via copy_packet. (This is still
// the copy-based buffer model, not yet the "event as a view" design; and because it
// drains to empty, a self-recirculating handler could delay the next read -- unlike
// the DPDK driver's bounded-burst dispatch, which owns its payloads via the mempool.)

// forward decl: do_dispatch routes port outputs through do_tx (defined below).
static void do_tx(%{out_event_ty} *oe);

/******** RX: parse a raw frame into an event and enqueue it (0 = dropped) ********/
static int do_rx(uint8_t *pkt, uint32_t len) {
    init_cursor(pkt, len, &g_in_pkt);
    %{events_ty} ev0;
    if (parse_event(&g_in_pkt, &ev0) != 1) { debug_printf("parse failed\n"); return 0; } // drop
    evq_push(&g_queue, &ev0);
    return 1;
}

/******** DISPATCH: drain the queue, routing each output (recirc -> queue, port -> tx) ********/
static void do_dispatch(int ingress_port) {
    while (!evq_empty(&g_queue)) {
        %{events_ty} ev;
        evq_pull(&g_queue, &ev);
        ev.meta.timestamp = now_ns();   // stamp at dequeue (covers arriving + recirculated events)
        ev.meta.in_port = ingress_port; // ingress port (read by the handler)
        %{out_event_ty} out_events[%{string_of_int CCoreHandlers.out_events_cap}];
        uint16_t n = handle_event(&ev, out_events);
        for (uint16_t i = 0; i < n; i++) {
            if (out_events[i].out_loc == 1)        // recirculation: re-queue for dispatch
                evq_push(&g_queue, &out_events[i].ev);
            else if (out_events[i].out_loc == 2)   // output to a port: deparse + send
                do_tx(&out_events[i]);
        }
    }
}

/******** TX: deparse one output event over a copy of the input packet, write it to the port ********/
static void do_tx(%{out_event_ty} *oe) {
    int out_port = (int)oe->port;
    int fd = port_fd(out_port);
    if (fd < 0) { debug_printf("no interface for port %d\n", out_port); return; }
    reset_cursor(&g_out_pkt);
    copy_packet(&g_out_pkt, &g_in_pkt);
    // a no-payload event emits only its header (drop the input tail); a payload event
    // keeps the tail (matches interp). boundary = cursor before deparse prepends the header.
    uint8_t *payload_boundary = g_out_pkt.cursor;
    deparse_event(&oe->ev, &g_out_pkt);
    uint8_t *dump_end = oe->ev.meta.has_payload ? g_out_pkt.end : payload_boundary;
    size_t out_len = (size_t)(dump_end - g_out_pkt.cursor);
    ssize_t w = write(fd, g_out_pkt.cursor, out_len);
    if (w < 0) debug_printf("write to port %d failed: %s\n", out_port, strerror(errno));
}

// per-frame entry: rx (parse + enqueue) then dispatch (drain + route this frame's
// events, and any it recirculates). only a successfully-parsed frame is counted.
static void dispatch_packet(int ingress_port, uint8_t* pkt, uint32_t len) {
    if (!do_rx(pkt, len)) return;
    do_dispatch(ingress_port);
    pkt_ct++;
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
|}

let package_prog decls =
  [
    "lucidprog.c", `Decls (imports @ decls @ helpers @ [socket_layer; dispatch; main]);
    "makefile", `String "all: lucidprog\n\nlucidprog: lucidprog.c\n\tgcc -O2 -o lucidprog lucidprog.c\n\n"
  ]
;;
