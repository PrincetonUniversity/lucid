open CCoreSyntax
open CCoreExceptions
open CCoreUtils

(*
  Single-core DPDK toplevel driver.

  Structure (each a dforiegn block below, in dataflow order):
    includes  ->  <generated code: types, parse/handle/deparse>  ->
    rings  ->  config + EAL/port/ring init  ->  rx/dispatch/tx phases  ->  main

  The runtime is a three-stage pipeline connected by two DPDK rings:
      RX   : poll every port, parse each frame to an event, push to dispatch_in.
      DISP : handle ONE event, route its outputs (recirc -> dispatch_in, port -> tx_in).
      TX   : deparse ONE output event and transmit it.
  Doing one event per main-loop iteration (rather than draining a queue to empty)
  interleaves recirculated events with fresh input, so a self-recirculating handler
  can't starve the RX path.

  Payload lifetime: an event's payload rides along as an mbuf in the ring element
  (NULL for events with no payload). RX strips the parsed header so the mbuf *is*
  the payload; TX prepends the deparsed header back onto it. A handler output that
  carries a payload gets its own copy of the input payload (copy-per-output).

  Assumes every output port id used by the program is bound to a valid device. *)

(* ---- names of compiler-generated constructs referenced in the driver's raw C.
   Taken from the cids the codegen emits (cid_to_string is the printer's own
   namer) and inlined with %{...} below, so the driver tracks generated names
   instead of hard-coding them. (event_meta / event_variant are internal to the
   generated code and not referenced by the driver, so they need no binding.) ---- *)
let events_ty        = CCoreCPrint.cid_to_string events_cid
let out_event_ty     = CCoreCPrint.cid_to_string CCoreHandlers.out_event_cid
let packet_t_ty      = CCoreCPrint.cid_to_string
  (match CCoreParse.packet_t.raw_ty with TName c -> c | _ -> err "packet_t is not a named type")
let parse_event_fn   = CCoreCPrint.cid_to_string CCoreParse.parser_cid
let deparse_event_fn = CCoreCPrint.cid_to_string CCoreParse.deparse_id
let handle_event_fn  = CCoreCPrint.cid_to_string CCoreHandlers.handler_cid
let out_events_cap   = string_of_int CCoreHandlers.out_events_cap

(* ===== includes (before the generated code, which needs stdint etc.) ===== *)
let dpdk_includes = dforiegn {|
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdbool.h>
#include <string.h>
#include <unistd.h>
// dpdk imports
#include <rte_eal.h>
#include <rte_ethdev.h>
#include <rte_cycles.h>
#include <rte_lcore.h>
#include <rte_mbuf.h>
#include <rte_ring.h>
|}

(* ===== pipeline rings (reference `events`, so emitted after the generated code) ===== *)
let dpdk_rings = dforiegn [%string {|
/******** pipeline rings (DPDK rte_ring, single-producer / single-consumer) ********/
#define RING_SIZE 1024

// an event flowing through the pipeline + the mbuf carrying its payload (NULL for
// events with no payload -- all background / recirc traffic). in_port is the ingress
// port, threaded through so the handler sees it even for recirculated events.
typedef struct { %{events_ty} ev; struct rte_mbuf *payload; uint16_t in_port; } disp_elem;
// an output event bound for a port, + its payload mbuf (NULL if none).
typedef struct { %{events_ty} ev; struct rte_mbuf *payload; uint16_t port; } tx_elem;

static struct rte_ring *dispatch_in;  // parsed + recirculated events awaiting handling
static struct rte_ring *tx_in;        // output events awaiting deparse + TX
|}]

(* ===== config + EAL / port / ring initialization ===== *)
let dpdk_config_init = dforiegn {|
/******** config + EAL / port / ring initialization ********/
// the packet mempool, set in dpdk_init; used to allocate output mbufs.
struct rte_mempool *mbuf_pool = NULL;

typedef struct cfg_t {
	uint16_t rx_ring_size;
	uint16_t tx_ring_size;
	int      num_mbufs;
	int      mbuf_cache_size;
	unsigned num_ports;
} cfg_t;

cfg_t cfg = {
	.rx_ring_size = 1024,
	.tx_ring_size = 1024,
	.num_mbufs = 8191,
	.mbuf_cache_size = 512,
	.num_ports = 0 // filled in by dpdk_init
};

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
	if (mbuf_pool == NULL) rte_exit(EXIT_FAILURE, "failed to create mbuf pool. not enough memory?");

	// single core -> single producer / single consumer rings (lockless).
	dispatch_in = rte_ring_create_elem("dispatch_in", sizeof(disp_elem), RING_SIZE,
		rte_socket_id(), RING_F_SP_ENQ | RING_F_SC_DEQ);
	tx_in = rte_ring_create_elem("tx_in", sizeof(tx_elem), RING_SIZE,
		rte_socket_id(), RING_F_SP_ENQ | RING_F_SC_DEQ);
	if (dispatch_in == NULL || tx_in == NULL)
		rte_exit(EXIT_FAILURE, "failed to create pipeline rings\n");

	uint16_t portid;
	RTE_ETH_FOREACH_DEV(portid)
		if (port_init(*cfg, portid, mbuf_pool) != 0)
			rte_exit(EXIT_FAILURE, "Cannot init port %"PRIu16 "\n", portid);
	return;
}
|}

(* ===== pipeline phases (reference the generated parse/handle/deparse + types) ===== *)
let dpdk_phases = dforiegn [%string {|
/******** pipeline phases: rx -> dispatch -> tx ********/
#define BURST_SIZE 64

// RX: poll every port, parse each frame to an event, push it to dispatch_in. The
// mbuf is stripped to its payload tail and carried iff the event has a payload;
// otherwise it is freed (the event travels by value).
static void do_rx(void) {
	uint16_t port;
	RTE_ETH_FOREACH_DEV(port) {
		struct rte_mbuf *bufs[BURST_SIZE];
		const uint16_t nb_rx = rte_eth_rx_burst(port, 0, bufs, BURST_SIZE);
		for (uint16_t i = 0; i < nb_rx; i++) {
			struct rte_mbuf *m = bufs[i];
			uint8_t *data = rte_pktmbuf_mtod(m, uint8_t*);
			%{packet_t_ty} pkt = { .start = data, .cursor = data,
			                       .end = data + rte_pktmbuf_pkt_len(m), .bit_off = 0 };
			disp_elem e = { .in_port = port, .payload = NULL };
			if (%{parse_event_fn}(&pkt, &e.ev) != 1) { rte_pktmbuf_free(m); continue; } // drop
			if (e.ev.meta.has_payload) {
				// strip the parsed header; the mbuf now *is* the payload tail.
				rte_pktmbuf_adj(m, (uint16_t)(pkt.cursor - data));
				e.payload = m;
			}
			if (rte_ring_enqueue_elem(dispatch_in, &e, sizeof(e)) != 0) {
				rte_pktmbuf_free(m);        // ring full -> drop (frees the payload too if m)
			} else if (e.payload == NULL) {
				rte_pktmbuf_free(m);        // enqueued by value; no payload to keep
			}
		}
	}
}

// DISPATCH: handle exactly ONE event per call, so recirculated events interleave
// with fresh input instead of starving it. Route each out_event: recirc back onto
// dispatch_in, port output onto tx_in. A payload-carrying output gets its own copy
// of the input payload.
static void do_dispatch(void) {
	disp_elem in;
	if (rte_ring_dequeue_elem(dispatch_in, &in, sizeof(in)) != 0) return; // empty
	in.ev.meta.timestamp = (uint32_t)rte_get_tsc_cycles(); // stamp at dequeue (Sys.time())
	%{out_event_ty} out_events[%{out_events_cap}];
	uint16_t n = %{handle_event_fn}(in.in_port, &in.ev, out_events);
	for (uint16_t i = 0; i < n; i++) {
		uint8_t loc = out_events[i].out_loc;
		struct rte_mbuf *pl = NULL;
		if (out_events[i].ev.meta.has_payload && in.payload != NULL)
			pl = rte_pktmbuf_copy(in.payload, mbuf_pool, 0, UINT32_MAX);
		if (loc == 1) {          // recirculation (generate_self)
			disp_elem e = { .ev = out_events[i].ev, .payload = pl, .in_port = in.in_port };
			if (rte_ring_enqueue_elem(dispatch_in, &e, sizeof(e)) != 0 && pl) rte_pktmbuf_free(pl);
		} else if (loc == 2) {   // output to a port
			tx_elem e = { .ev = out_events[i].ev, .payload = pl, .port = out_events[i].port };
			if (rte_ring_enqueue_elem(tx_in, &e, sizeof(e)) != 0 && pl) rte_pktmbuf_free(pl);
		} else if (pl) {
			rte_pktmbuf_free(pl);
		}
	}
	if (in.payload) rte_pktmbuf_free(in.payload); // input payload consumed (copied per output)
}

// TX: deparse exactly ONE output event's header and transmit it. A payload event
// prepends its header onto the carried payload mbuf; a no-payload event builds a
// fresh header-only mbuf. Deparse writes the header backwards into the mbuf's
// headroom, then prepend extends the data region to include it.
static void do_tx(void) {
	tx_elem e;
	if (rte_ring_dequeue_elem(tx_in, &e, sizeof(e)) != 0) return; // empty
	if (e.port >= cfg.num_ports) {
		printf("WARNING: dropping packet to out-of-range port: %u\n", e.port);
		if (e.payload) rte_pktmbuf_free(e.payload);
		return;
	}
	struct rte_mbuf *m = e.payload;
	if (m == NULL) {                       // no-payload event -> fresh, header-only mbuf
		m = rte_pktmbuf_alloc(mbuf_pool);
		if (unlikely(m == NULL)) return;
	}
	uint8_t *data = rte_pktmbuf_mtod(m, uint8_t*);
	%{packet_t_ty} pkt = { .start = data, .cursor = data,
	                       .end = data + rte_pktmbuf_pkt_len(m), .bit_off = 0 };
	%{deparse_event_fn}(&e.ev, &pkt);      // writes the header backwards into headroom
	rte_pktmbuf_prepend(m, (uint16_t)(data - pkt.cursor)); // include the prepended header
	uint16_t nb_tx = rte_eth_tx_burst(e.port, 0, &m, 1);
	if (unlikely(nb_tx < 1)) rte_pktmbuf_free(m);
}
|}]

(* ===== main loop ===== *)
let dpdk_main = dforiegn {|
/******** main loop ********/
static __rte_noreturn void lcore_main(void) {
	printf("handler loop running -- ctrl-c to quit\n");
	for (;;) {
		do_rx();        // poll all ports, parse, enqueue
		do_dispatch();  // handle one event, route its outputs
		do_tx();        // deparse + transmit one output event
	}
}

int main(int argc, char *argv[]) {
	dpdk_init(&cfg, argc, argv); // init dpdk, ports, memory pools, rings
	lcore_main();                // the rx / dispatch / tx loop
	rte_eal_cleanup();
	return 0;
}
|}

(* the pipeline model needs no extra generated helpers (it dispatches on the
   handler's out_event count + out_loc, and carries payloads as mbufs). *)
let helpers _decls = ([] : decls)

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
	"lucidprog.c", `Decls (
		[dpdk_includes] @ decls @ (helpers decls) @
		[dpdk_rings; dpdk_config_init; dpdk_phases; dpdk_main]);
	(* capital M: the DPDK makefile lists `Makefile` as a prerequisite of its build
	   rules, so the on-disk name must match (Linux is case-sensitive). *)
	"Makefile", `String makefile;
	"run.sh", `String run_sh;
]
