open CCoreSyntax
open CCoreExceptions
open CCoreUtils

(*
  Single-core DPDK toplevel driver.

  Structure (each a dforiegn block below, in dataflow order):
    includes  ->  <generated code: types, parse/handle/deparse>  ->
    rings  ->  config + EAL/port/ring init  ->  rx/dispatch/tx phases  ->  main

  The runtime is a three-stage pipeline connected by two DPDK rings that carry mbuf
  POINTERS -- every event rides an mbuf (the "queue element"):
      RX   : poll every port, parse each frame into the mbuf's event, push to dispatch_in.
      DISP : handle a bounded burst of events, filling each mbuf's out_events list; hand
             the mbuf to tx_in with NO copy.
      TX   : fan out each mbuf into one copy per output event, then route each copy
             (recirc -> dispatch_in, else deparse + transmit to its port).
  Each stage pulls a BOUNDED burst (<= BURST_SIZE) rather than draining to empty, so a
  self-recirculating handler can't starve the RX path.

  Ownership: the mbuf owns the packet bytes; the event and (on the input mbuf) the
  handler's output list live in the mbuf's private area (qe_priv_t). packet_t is only
  ever a transient VIEW built over the mbuf's data to parse/deparse -- it owns nothing.
  RX records where the payload begins (payload_off) instead of stripping the header;
  each TX copy takes just that payload (or empty) with fresh headroom, and deparse
  prepends the output's header into it. The copy is the fan-out -- dispatch never copies.

  Assumes every output port id used by the program is bound to a valid device. *)

(* ---- names of compiler-generated constructs referenced in the driver's raw C.
   Taken from the cids the codegen emits (cid_to_string is the printer's own
   namer) and inlined with %{...} below, so the driver tracks generated names
   instead of hard-coding them. (event_meta / event_variant are internal to the
   generated code and not referenced by the driver, so they need no binding.) ---- *)
let events_ty        = CCoreCPrint.cid_to_string events_cid
let out_event_ty     = CCoreCPrint.cid_to_string CCoreHandlers.out_event_cid
let packet_t_ty      = CCoreCPrint.cid_to_string
  (match CCoreParse.packet_t.raw_ty with TName(c, _) -> c | _ -> err "packet_t is not a named type")
let parse_event_fn   = CCoreCPrint.cid_to_string CCoreParse.parser_cid
let deparse_event_fn = CCoreCPrint.cid_to_string CCoreParse.deparse_id
let handle_event_fn  = CCoreCPrint.cid_to_string CCoreHandlers.handler_cid
let out_events_cap   = string_of_int CCoreHandlers.out_events_cap
let port_recirc      = string_of_int CCoreHandlers.port_recirc

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

(* ===== queue element + pipeline rings (reference the generated event/packet types) ===== *)
let dpdk_queue = dforiegn [%string {|
/******** the queue element: an mbuf whose private area carries the event ********/
// Every event rides an mbuf. The mbuf OWNS the packet bytes (its data region); the
// mbuf's private area (priv_size, set at pool creation) holds the event and -- on the
// input mbuf between dispatch and tx -- the handler's output-event list. packet_t is
// only ever a transient VIEW built over the data to parse/deparse; it owns nothing.
typedef struct {
	%{events_ty}    ev;                        // the event this mbuf carries
	%{out_event_ty} out_events[%{out_events_cap}]; // handler outputs (input mbuf only)
	uint16_t        n_out;                     // number of outputs the handler produced
	uint32_t        payload_off;               // byte offset in the data where the payload begins
} qe_priv_t;

static inline qe_priv_t *qe(struct rte_mbuf *m) { return (qe_priv_t *)rte_mbuf_to_priv(m); }

// a transient cursor view over an mbuf's data (front .. pkt_len); borrows, never owns.
static inline %{packet_t_ty} view_over(struct rte_mbuf *m) {
	uint8_t *d = rte_pktmbuf_mtod(m, uint8_t *);
	return (%{packet_t_ty}){ .start = d, .cursor = d, .end = d + rte_pktmbuf_pkt_len(m), .bit_off = 0 };
}

/******** pipeline rings (DPDK rte_ring, single-producer / single-consumer) ********/
#define RING_SIZE 1024
#define BURST_SIZE 64   // max mbufs pulled per rx / dispatch / tx call (bounded, so a
                        // self-recirculating handler can't starve the RX path)
#define PORT_RECIRC %{port_recirc}u  // out_event.port sentinel: recirculate, don't egress

// The rings carry mbuf POINTERS (the queue elements themselves).
static struct rte_ring *dispatch_in;  // parsed + recirculated event-mbufs awaiting handling
static struct rte_ring *tx_in;        // handled event-mbufs awaiting fan-out + deparse + TX
|}]

(* ===== config + EAL / port / ring initialization ===== *)
let dpdk_config_init = dforiegn [%string {|
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
	// priv_size carries qe_priv_t (the event + output list) alongside each mbuf's data.
	uint16_t priv_size = RTE_ALIGN(sizeof(qe_priv_t), RTE_MBUF_PRIV_ALIGN);
	mbuf_pool = rte_pktmbuf_pool_create("MBUF_POOL", cfg->num_mbufs * nb_ports,
		cfg->mbuf_cache_size, priv_size, RTE_MBUF_DEFAULT_BUF_SIZE, rte_socket_id());
	if (mbuf_pool == NULL) rte_exit(EXIT_FAILURE, "failed to create mbuf pool. not enough memory?");

	// single core -> single producer / single consumer rings (lockless); they carry
	// mbuf pointers, so plain rte_ring (not the value-carrying _elem variant).
	dispatch_in = rte_ring_create("dispatch_in", RING_SIZE, rte_socket_id(),
		RING_F_SP_ENQ | RING_F_SC_DEQ);
	tx_in = rte_ring_create("tx_in", RING_SIZE, rte_socket_id(),
		RING_F_SP_ENQ | RING_F_SC_DEQ);
	if (dispatch_in == NULL || tx_in == NULL)
		rte_exit(EXIT_FAILURE, "failed to create pipeline rings\n");

	uint16_t portid;
	RTE_ETH_FOREACH_DEV(portid)
		if (port_init(*cfg, portid, mbuf_pool) != 0)
			rte_exit(EXIT_FAILURE, "Cannot init port %"PRIu16 "\n", portid);
	return;
}
|}]

(* ===== pipeline phases (reference the generated parse/handle/deparse + types) =====
   One dforiegn block per phase. Each pulls a *bounded* burst (<= BURST_SIZE) and
   loops over it -- so dispatch/tx don't drain to empty, which would let a
   self-recirculating handler starve the RX path. *)

(* RX: poll every port, parse each frame into the mbuf's event, enqueue the mbuf on
   dispatch_in. No header stripping -- we record where the payload begins (payload_off)
   and leave the bytes in place; the mbuf IS the queue element. *)
let dpdk_do_rx = dforiegn [%string {|
/******** RX: poll every port, parse, enqueue the event-mbuf on dispatch_in ********/
static void do_rx(void) {
	uint16_t port;
	RTE_ETH_FOREACH_DEV(port) {
		struct rte_mbuf *bufs[BURST_SIZE];
		const uint16_t nb_rx = rte_eth_rx_burst(port, 0, bufs, BURST_SIZE);
		for (uint16_t i = 0; i < nb_rx; i++) {
			struct rte_mbuf *m = bufs[i];
			qe_priv_t *p = qe(m);
			%{packet_t_ty} view = view_over(m);
			if (%{parse_event_fn}(&view, &p->ev) != 1) { rte_pktmbuf_free(m); continue; } // drop
			p->payload_off = (uint32_t)(view.cursor - view.start); // where the payload begins
			p->ev.meta.in_port = port;                             // stamp ingress (read by the handler)
			if (rte_ring_enqueue(dispatch_in, m) != 0) rte_pktmbuf_free(m); // ring full -> drop
		}
	}
}
|}]

(* DISPATCH: handle a bounded burst of event-mbufs. The handler fills each mbuf's
   out_events list; we hand the mbuf straight to tx_in with NO copy (the fan-out
   happens in TX). Bounded burst -> recirculated events (re-enqueued by TX onto
   dispatch_in) land in a future burst, so a self-recirculating handler can't starve RX. *)
let dpdk_do_dispatch = dforiegn [%string {|
/******** DISPATCH: handle a burst of event-mbufs; hand each to tx_in (no copy) ********/
static void do_dispatch(void) {
	void *batch[BURST_SIZE];
	unsigned nb = rte_ring_dequeue_burst(dispatch_in, batch, BURST_SIZE, NULL);
	for (unsigned b = 0; b < nb; b++) {
		struct rte_mbuf *m = (struct rte_mbuf *)batch[b];
		qe_priv_t *p = qe(m);
		p->ev.meta.timestamp = (uint32_t)rte_get_tsc_cycles(); // stamp at dequeue (Sys.time())
		p->n_out = %{handle_event_fn}(&p->ev, p->out_events);   // ingress read from p->ev.meta.in_port
		if (rte_ring_enqueue(tx_in, m) != 0) rte_pktmbuf_free(m); // ring full -> drop the element
	}
}
|}]

(* TX: fan out a bounded burst of event-mbufs. For each input mbuf we make one copy
   per output event -- the copy holds just that output's payload (the input's bytes at
   payload_off, or empty for a no-payload output) with fresh headroom -- set the copy's
   event, then route it: a recirc output (port == PORT_RECIRC) goes back onto dispatch_in
   un-deparsed; any other output is deparsed (header prepended into the headroom) and
   transmitted. The input mbuf is freed once all its outputs are copied out. *)
let dpdk_do_tx = dforiegn [%string {|
/******** TX: fan out each event-mbuf into one copy per output, then route/deparse/send ********/
static void do_tx(void) {
	void *batch[BURST_SIZE];
	unsigned nb = rte_ring_dequeue_burst(tx_in, batch, BURST_SIZE, NULL);
	for (unsigned b = 0; b < nb; b++) {
		struct rte_mbuf *m = (struct rte_mbuf *)batch[b];
		qe_priv_t *ip = qe(m);
		uint32_t pkt_len = rte_pktmbuf_pkt_len(m);
		for (uint16_t i = 0; i < ip->n_out; i++) {
			%{out_event_ty} *oe = &ip->out_events[i];
			// each output owns an independent copy: just the input's payload (or empty),
			// with fresh headroom for the deparsed header to prepend into.
			uint32_t plen = oe->ev.meta.has_payload ? (pkt_len - ip->payload_off) : 0;
			struct rte_mbuf *c = plen > 0
				? rte_pktmbuf_copy(m, mbuf_pool, ip->payload_off, plen)
				: rte_pktmbuf_alloc(mbuf_pool);
			if (unlikely(c == NULL)) continue;
			qe_priv_t *cp = qe(c);
			cp->ev = oe->ev;
			cp->payload_off = 0;                // the payload now sits at the front of c
			if (oe->port == PORT_RECIRC) {          // recirculation (generate_self)
				cp->ev.meta.in_port = ip->ev.meta.in_port; // recirc inherits ingress
				if (rte_ring_enqueue(dispatch_in, c) != 0) rte_pktmbuf_free(c);
			} else if (oe->port >= cfg.num_ports) {
				printf("WARNING: dropping packet to out-of-range port: %u\n", oe->port);
				rte_pktmbuf_free(c);
			} else {                                // output to a port: deparse + transmit
				%{packet_t_ty} view = view_over(c); // cursor at the payload boundary (front of c)
				%{deparse_event_fn}(&cp->ev, &view); // writes the header backwards into headroom
				rte_pktmbuf_prepend(c, (uint16_t)(view.start - view.cursor)); // include the header
				if (unlikely(rte_eth_tx_burst(oe->port, 0, &c, 1) < 1)) rte_pktmbuf_free(c);
			}
		}
		rte_pktmbuf_free(m); // input consumed (copied per output)
	}
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
		[dpdk_queue; dpdk_config_init; dpdk_do_rx; dpdk_do_dispatch; dpdk_do_tx; dpdk_main]);
	(* capital M: the DPDK makefile lists `Makefile` as a prerequisite of its build
	   rules, so the on-disk name must match (Linux is case-sensitive). *)
	"Makefile", `String makefile;
	"run.sh", `String run_sh;
]
