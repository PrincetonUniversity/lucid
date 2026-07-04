open CCoreSyntax
open CCoreExceptions
open CCoreUtils


(* 
  Single-core DPDK toplevel driver. 
  Assumes that all the output ports used in the program are ids that are 
  bound to valid devices. *)   

(* the dpdk_header is everything we need except: 
   1) the helpers; 
   2) the application-specific generated code; 
   3) the fixed packet_handler that calls the app-specific code  *)
let dpdk_header = dforiegn {|
#include <stdlib.h> 
#include <stdint.h>
#include <inttypes.h> 
#include <unistd.h> // for sleep
#include <stdatomic.h> // atomics
#include <sys/mman.h> // shared memory
// dpdk imports
#include <rte_eal.h>
#include <rte_ethdev.h>
#include <rte_cycles.h>
#include <rte_lcore.h>
#include <rte_mbuf.h>

// the dispatch pipeline entry point (defined below, after the generated code):
// parse the input mbuf, run the queue-based dispatch loop, and TX port events.
static inline void dispatch_packet(struct rte_mbuf *buf, uint16_t in_port);

// the packet mempool, set in dpdk_init; used to allocate output mbufs.
struct rte_mempool *mbuf_pool = NULL;

// dpdk initialization helpers 
typedef struct cfg_t {
	uint16_t rx_ring_size;
	uint16_t tx_ring_size;
	int      num_mbufs;
	int      mbuf_cache_size;
	unsigned num_ports;
} cfg_t;


static inline int
port_init(cfg_t cfg, uint16_t port, struct rte_mempool *mbuf_pool)
{
	printf("initializing port %u\n", port);
	struct rte_eth_conf port_conf;
	const uint16_t rx_rings = 1, tx_rings = 1;
	uint16_t nb_rxd = cfg.rx_ring_size;
	uint16_t nb_txd = cfg.tx_ring_size;
	int retval;
	uint16_t q;
	struct rte_eth_dev_info dev_info;
	struct rte_eth_txconf txconf;

	if (!rte_eth_dev_is_valid_port(port))
		return -1;

	memset(&port_conf, 0, sizeof(struct rte_eth_conf));

	retval = rte_eth_dev_info_get(port, &dev_info);
	if (retval != 0) {
		printf("Error during getting device (port %u) info: %s\n",
				port, strerror(-retval));
		return retval;
	}

	if (dev_info.tx_offload_capa & RTE_ETH_TX_OFFLOAD_MBUF_FAST_FREE)
		port_conf.txmode.offloads |= RTE_ETH_TX_OFFLOAD_MBUF_FAST_FREE;

	/* Configure the Ethernet device. */
	retval = rte_eth_dev_configure(port, rx_rings, tx_rings, &port_conf);
	if (retval != 0) return retval;

	retval = rte_eth_dev_adjust_nb_rx_tx_desc(port, &nb_rxd, &nb_txd);
	if (retval != 0) return retval;

	/* Allocate and set up 1 RX queue per Ethernet port. */
	for (q = 0; q < rx_rings; q++) {
		retval = rte_eth_rx_queue_setup(port, q, nb_rxd,
				rte_eth_dev_socket_id(port), NULL, mbuf_pool);
		if (retval < 0) return retval;
	}

	txconf = dev_info.default_txconf;
	txconf.offloads = port_conf.txmode.offloads;
	/* Allocate and set up 1 TX queue per Ethernet port. */
	for (q = 0; q < tx_rings; q++) {
		retval = rte_eth_tx_queue_setup(port, q, nb_txd,
				rte_eth_dev_socket_id(port), &txconf);
		if (retval < 0)
			return retval;
	}
	retval = rte_eth_dev_start(port);
	if (retval < 0)
		return retval;

	/* Enable RX in promiscuous mode for the Ethernet device. */
	retval = rte_eth_promiscuous_enable(port);
	/* End of setting RX port in promiscuous mode. */
	if (retval != 0)
		return retval;

	return 0;
}

void dpdk_init(cfg_t* cfg, int argc, char *argv[]) {
	if (rte_eal_init(argc, argv) < 0)
		rte_exit(EXIT_FAILURE, "Error with EAL initialization\n");
	unsigned nb_ports = rte_eth_dev_count_avail();
	printf("number of ports: %u\n", nb_ports);
	if (nb_ports == 0)
		rte_exit(EXIT_FAILURE, "No Ethernet ports - bye\n");
	cfg->num_ports = nb_ports;	
	mbuf_pool = rte_pktmbuf_pool_create("MBUF_POOL", cfg->num_mbufs * nb_ports,
		cfg->mbuf_cache_size, 0, RTE_MBUF_DEFAULT_BUF_SIZE, rte_socket_id());
	if (mbuf_pool == NULL) rte_exit(EXIT_FAILURE, "failed to cread mbuf pool. not enough memory?");

	uint16_t portid;
	RTE_ETH_FOREACH_DEV(portid)
		if (port_init(*cfg, portid, mbuf_pool) != 0)
			rte_exit(EXIT_FAILURE, "Cannot init port %"PRIu16 "\n", portid);
	return;
}

// packet handler loop and main
#define BURST_SIZE 64
cfg_t cfg = {
	.rx_ring_size = 1024,
	.tx_ring_size = 1024,
	.num_mbufs = 8191,
	.mbuf_cache_size = 512,
	.num_ports = 0 // will be filled in by init
};

static __rte_noreturn void lcore_main(void) {
	printf("handler loop running -- ctrl-c to quit\n");
	uint16_t port = 0;
	for (;;) {
		// get packets
		struct rte_mbuf *bufs[BURST_SIZE];
		const uint16_t nb_rx = rte_eth_rx_burst(port, 0,bufs, BURST_SIZE);
		if (unlikely(nb_rx == 0))
			continue;
		for (uint16_t i = 0; i < nb_rx; i++) {
			// run the dispatch pipeline; it TXes any port events itself (it
			// allocates fresh output mbufs), so we always free the input mbuf.
			dispatch_packet(bufs[i], bufs[i]->port);
			rte_pktmbuf_free(bufs[i]);
		}
	}
}

int main(int argc, char *argv[]) {	
	dpdk_init(&cfg, argc, argv); // init dpdk, ports, and memory pools
	lcore_main(); // call the rx, handle, tx loop
	rte_eal_cleanup(); // cleanup, happens after ctrl-c (I think?)
	return 0;
}
|};;


(* NOTE: this DPDK driver is not validated in the local build environment (no
   DPDK present); it mirrors the pcap driver's queue-based dispatch structure
   using DPDK primitives. The mbuf data_off / pkt_len arithmetic in
   send_port_event follows the same "copy input, deparse over it at the
   header/payload boundary" approach as the pcap copy_packet path. *)
let pkt_handler = dforiegn [%string {|
/********* internal dispatch FIFO of events (single core, single queue) ***********/
#define EV_QUEUE_CAP 1024
typedef struct ev_queue_t {
    events buf[EV_QUEUE_CAP];
    int head;
    int tail;
    int count;
} ev_queue_t;

static ev_queue_t dispatch_queue = {0};

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

// Deparse one port event over a copy of the input mbuf and TX it. The input
// packet is copied so its payload is preserved (mirrors the pcap copy_packet
// path); deparse writes the event headers backwards from the header/payload
// boundary, and we fix up the output mbuf's data_off / length accordingly.
static void send_port_event(struct rte_mbuf *in_buf, packet_t *in_pkt, events *ev, uint16_t out_port) {
    if (out_port >= cfg.num_ports) {
        printf("WARNING: dropping packet to out-of-range port: %u\n", out_port);
        return;
    }
    struct rte_mbuf *out = rte_pktmbuf_copy(in_buf, mbuf_pool, 0, UINT32_MAX);
    if (unlikely(out == NULL)) return;

    uint8_t *out_base = rte_pktmbuf_mtod(out, uint8_t*);
    uint32_t boundary = (uint32_t)(in_pkt->cursor - in_pkt->start); // input header length
    packet_t out_pkt = {
        .start  = out_base,
        .cursor = out_base + boundary,
        .end    = out_base + rte_pktmbuf_pkt_len(out)
    };
    uint8_t *payload_boundary = out_base + boundary; // payload start, before deparse prepends
    deparse_event(ev, &out_pkt); // writes backwards from .cursor

    // the deparsed packet runs from out_pkt.cursor to dump_end. A no-payload event emits
    // only its header (drop the input tail); a payload event keeps the tail. (matches
    // interp) Shift the mbuf's data start to out_pkt.cursor (negative shift == headers
    // grew into the mbuf headroom) and set the new length.
    uint8_t *dump_end = ev->meta.has_payload ? out_pkt.end : payload_boundary;
    int32_t front_shift = (int32_t)(out_pkt.cursor - out_base);
    uint16_t new_len = (uint16_t)(dump_end - out_pkt.cursor);
    out->data_off = (uint16_t)((int32_t)out->data_off + front_shift);
    out->data_len = new_len;
    out->pkt_len  = new_len;

    uint16_t nb_tx = rte_eth_tx_burst(out_port, 0, &out, 1);
    if (unlikely(nb_tx < 1)) rte_pktmbuf_free(out);
}

// The dispatch pipeline (mirror of the pcap driver): parse -> dispatch queue ->
// handle -> {recirc back onto the queue | deparse + TX out a port}. The input
// mbuf stays valid for the whole drain, so port events (even those from
// recirculated events) can reuse its payload.
static inline void dispatch_packet(struct rte_mbuf *buf, uint16_t in_port) {
    packet_t in_pkt = {
        .start  = rte_pktmbuf_mtod(buf, uint8_t*),
        .cursor = rte_pktmbuf_mtod(buf, uint8_t*),
        .end    = rte_pktmbuf_mtod(buf, uint8_t*) + rte_pktmbuf_pkt_len(buf)
    };

    // parse round
    events ev0;
    if (parse_event(&in_pkt, &ev0) != 1) {
        return; // parse failed, drop
    }
    evq_push(&dispatch_queue, &ev0);

    // dispatch round: drain the queue
    while (!evq_empty(&dispatch_queue)) {
        events ev;
        evq_pull(&dispatch_queue, &ev);
        ev.meta.timestamp = (uint32_t)rte_get_tsc_cycles(); // stamp at dequeue (Sys.time())
        out_event out_events[%{string_of_int CCoreHandlers.out_events_cap}];
        uint16_t n = handle_event(in_port, &ev, out_events);
        for (uint16_t i = 0; i < n; i++) {
            if (out_events[i].out_loc == 1) {
                // recirculation: re-queue for dispatch
                evq_push(&dispatch_queue, &out_events[i].ev);
            } else if (out_events[i].out_loc == 2) {
                // output to a port
                send_port_event(buf, &in_pkt, &out_events[i].ev, out_events[i].port);
            }
        }
    }
}
|}]
;;

let get_event_tag t_event = 
	let ev_param = cid"ev", tref t_event in
	dfun 
		(cid"get_event_tag")
		(tint event_tag_size)
		[ev_param]
		(sret (ecast (tint event_tag_size) (((param_evar ev_param)/->cid"data")/.cid"tag")))
;;
let reset_event_tag t_event =
	(* this isn't right. Need an address.. *)
	let ev_param = cid"ev", tref t_event in
	let enum_ty = (((param_evar ev_param)/->cid"data")/.cid"tag").ety in
	dfun
		(cid"reset_event_tag")
		(tunit)
		[ev_param]
		(sassign_exp (((param_evar ev_param)/->cid"data")/.cid"tag") (ecast (enum_ty) (default_exp (tint event_tag_size))))
;;


let tag_helpers decls = 
    let teventstruct = match (find_ty_opt events_cid decls) with
       | Some(ty) -> ty
       | _ -> err "no tevent"
    in  
    [
       get_event_tag teventstruct;
       reset_event_tag teventstruct;
    ]
 ;;
 
(* the queue-based dispatch model needs no extra helpers (no cursor init / pkt
   copy, and it dispatches on the handler's out_event count + out_loc rather than
   on event tags, so the tag helpers are gone too). *)
let helpers _decls = ([] : decls)
let _ = tag_helpers (* silence unused-value warning; kept for reference *)
let imports = [dpdk_header];;
let pkt_handler = pkt_handler
let main = dforiegn ""
let cflags = ""
let other_files = []
(* *)

let progname = "lucidprog"

let makefile = [%string{| 
# binary name and src must match
APP = %{progname}
SRCS-y := %{progname}.c

PKGCONF ?= pkg-config
ifneq ($(shell $(PKGCONF) --exists libdpdk && echo 0),0)
	$(error "no DPDK")
endif

all: shared
.PHONY: shared static
shared: build/$(APP)-shared
	ln -sf $(APP)-shared build/$(APP)
static: build/$(APP)-static
	ln -sf $(APP)-static build/$(APP)

PC_FILE := $(shell $(PKGCONF) --path libdpdk 2>/dev/null)
CFLAGS += -O3 $(shell $(PKGCONF) --cflags libdpdk)
LDFLAGS_SHARED = $(shell $(PKGCONF) --libs libdpdk)
LDFLAGS_STATIC = $(shell $(PKGCONF) --static --libs libdpdk)

ifeq ($(MAKECMDGOALS),static)
# check for broken pkg-config
ifeq ($(shell echo $(LDFLAGS_STATIC) | grep 'whole-archive.*l:lib.*no-whole-archive'),)
$(warning "pkg-config output list does not contain drivers between 'whole-archive'/'no-whole-archive' flags.")
$(error "Cannot generate statically-linked binaries with this version of pkg-config")
endif
endif

CFLAGS += -DALLOW_EXPERIMENTAL_API

build/$(APP)-shared: $(SRCS-y) Makefile $(PC_FILE) | build
	$(CC) $(CFLAGS) $(SRCS-y) -o $@ $(LDFLAGS) $(LDFLAGS_SHARED)

build/$(APP)-static: $(SRCS-y) Makefile $(PC_FILE) | build
	$(CC) $(CFLAGS) $(SRCS-y) -o $@ $(LDFLAGS) $(LDFLAGS_STATIC)

build:
	@mkdir -p $@

.PHONY: clean
clean:
	rm -f build/$(APP) build/$(APP)-static build/$(APP)-shared
	test -d build && rmdir -p build || true
|}]
;;

let run_sh = 
	[%string{|
sudo ./build/%{progname}-shared --log-level=8 -l 1 -n 4 --no-pci --vdev 'net_pcap0,rx_pcap=small_in.pcap,tx_pcap=small_out.pcap'	
	|}]
;;
(* return a list of files *)
let package_prog decls = 
[
	"lucidprog.c", `Decls (imports @ decls @ (helpers decls) @ [pkt_handler]);
	(* capital M: the DPDK makefile lists `Makefile` as a prerequisite of its build
	   rules, so the on-disk name must match (Linux is case-sensitive). *)
	"Makefile", `String makefile;
	"run.sh", `String run_sh;
]
