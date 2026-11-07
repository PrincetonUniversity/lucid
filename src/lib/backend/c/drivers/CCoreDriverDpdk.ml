open CCoreSyntax
open CCoreExceptions
open CCoreUtils

(* 
	TODO: TX should not copy mbuf of the first event 
			// loop: start with b = 1, do all the stuff for the copies
			// after loop: if nb > 0, do event 0 else free the packet (drop)

	TODO: 
		- Multiple cores for each stage as default
		- Then, the more interesting questions avout how to parallelize and balance


*)

(*
  Single-core DPDK toplevel driver.

	The architecture is a pipeline with 3 stages: rx -> dispatch -> tx, 
	using DPDK rings carrying mbufs between the stages. 
	Each mbuf carries an event, and program-generated output events, in its private area. 
      RX   : poll every port for at most what fits on rx_in, parse each frame
             into the mbuf's event, push to rx_in.
      DISP : a burst from recirc_in and one from rx_in: execute event handlers,
             fill the mbuf's out_events list, send to tx_in.
      TX   : drain tx_in: clone / route each output event; a recirculation
             goes onto recirc_in.
	Stages operate on bursts of mbufs (<= BURST_SIZE)

  The queues. Arrivals and recirculations have separate rings into dispatch,
  served a burst each, so neither starves the other. Receive pulls only what
  fits on rx_in, so under overload packets wait in the NIC's descriptor ring
  (the NIC drops and counts them, or pauses the sender) and nothing arriving is
  dropped inside. recirc_in is drop-tail: it is the one edge that closes the
  loop, so it never waits, which keeps the pipeline free of deadlock, and a
  program whose recirculation multiplies drops there, as at a switch's
  recirculation port. tx_in is drained whole every iteration and cannot fill.
  Every drop is counted by cause. Multi-segment frames are dropped at receive:
  the packet view is one segment.
  An output to a program port bound to no DPDK port is dropped and counted.

  Command line: the EAL's arguments, then `--`, then the program's:
    --port PROG:DPDK bind the program's port number PROG to DPDK port id DPDK
                     (a DPDK port not named maps to its own number)
    --idle-exit MS   exit after MS milliseconds without a packet received, sent
                     or recirculated (for runs over pcap devices; 0 = never)
    --stats S        every S seconds, a line on stderr with the period's rates
  SIGINT and SIGTERM stop the loop; then the ports are stopped and closed (a
  pcap device flushes its output there) and the counters go to stderr. The
  driver's own messages go to stderr, so stdout carries only the program's
  prints. *)

(* compiler-generated constructs referenced in the driver's raw C *)
let events_ty        = CCoreCPrint.cid_to_string events_cid
let out_event_ty     = CCoreCPrint.cid_to_string CCoreHandlers.out_event_cid
let packet_t_ty      = CCoreCPrint.cid_to_string
  (match CCoreParse.packet_t.raw_ty with TName(c, _) -> c | _ -> err "packet_t is not a named type")
let parse_event_fn   = CCoreCPrint.cid_to_string CCoreParse.parser_cid
let deparse_event_fn = CCoreCPrint.cid_to_string CCoreParse.deparse_id
let handle_event_fn  = CCoreCPrint.cid_to_string CCoreHandlers.handler_cid
let out_events_cap   = string_of_int CCoreHandlers.out_events_cap
let port_recirc      = string_of_int CCoreHandlers.port_recirc
let recirc_port_var  = CCoreCPrint.cid_to_string (Cid.id Builtins.recirc_id)

(* ===== includes (before the generated code, which needs stdint etc.) ===== *)
let dpdk_includes = dforiegn {|
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdbool.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
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
// An event is carried in the private region of the mbuf that stores 
// the packet that generated it. Recirculared events have no payload.
typedef struct {
	%{events_ty}    ev;                        // the event this mbuf carries
	%{out_event_ty} out_events[%{out_events_cap}]; // handler outputs (input mbuf only)
	uint16_t        n_out;                     // number of outputs the handler produced
	uint32_t        payload_off;               // byte offset in the data where the payload begins
} qe_priv_t;

static inline qe_priv_t *qe(struct rte_mbuf *m) { return (qe_priv_t *)rte_mbuf_to_priv(m); }

// packet_t is a view of the mbuf's data (one segment: multi-segment frames are dropped at rx).
static inline %{packet_t_ty} view_over(struct rte_mbuf *m) {
	uint8_t *d = rte_pktmbuf_mtod(m, uint8_t *);
	return (%{packet_t_ty}){ .start = d, .cursor = d, .end = d + rte_pktmbuf_pkt_len(m), .bit_off = 0 };
}

/******** pipeline rings (DPDK rte_ring, single-producer / single-consumer) ********/
#define RING_SIZE 1024
#define BURST_SIZE 64   // max mbufs pulled per port per rx call and per ring per dispatch call
#define PORT_RECIRC %{port_recirc}u  // out_event.port sentinel: recirculate, don't egress

// The rings carry mbuf POINTERS (the queue elements themselves).
static struct rte_ring *rx_in;      // parsed arrivals awaiting handling (rx pulls only what fits)
static struct rte_ring *recirc_in;  // recirculated events awaiting handling (drop-tail when full)
static struct rte_ring *tx_in;      // handled event-mbufs awaiting fan-out (drained whole each iteration)

/******** counters, printed at exit ********/
static struct {
	uint64_t rx, tx, recirc;     // frames received / sent / recirculated
	uint64_t parse_drop;         // the entry parser refused the bytes
	uint64_t seg_drop;           // a multi-segment (jumbo) frame
	uint64_t recirc_drop;        // recirc_in full (the program recirculates faster than it drains)
	uint64_t alloc_drop;         // no mbuf for an output
	uint64_t port_drop;          // an output to a program port bound to no DPDK port
	uint64_t tx_drop;            // the NIC's transmit queue full
	uint64_t bug_drop;           // a ring the design says cannot fill was full
} st;

/******** ports (program port number <-> DPDK port id) ********/
#define PORT_NONE 0xFFFFu
static uint32_t prog_of_dpdk[RTE_MAX_ETHPORTS];  // ingress stamp, by DPDK port id
static uint16_t dpdk_ports[RTE_MAX_ETHPORTS];    // the available DPDK port ids
static unsigned n_dpdk_ports;

static inline uint16_t dpdk_of_prog(uint32_t prog) {  // PORT_NONE when unbound
	for (unsigned i = 0; i < n_dpdk_ports; i++)
		if (prog_of_dpdk[dpdk_ports[i]] == prog) return dpdk_ports[i];
	return PORT_NONE;
}

/******** the clock: nanoseconds since the driver started (Sys.time) ********/
static uint64_t tsc_hz, tsc0;
static inline uint64_t now_ns(void) {
	uint64_t d = rte_rdtsc() - tsc0;   // split so that d * 1e9 cannot overflow
	return (d / tsc_hz) * 1000000000ull + ((d % tsc_hz) * 1000000000ull) / tsc_hz;
}

/******** the program's arguments (after the EAL's `--`) ********/
static uint64_t idle_exit_ms = 0;   // --idle-exit, 0 = never
static uint64_t stats_s = 0;        // --stats, 0 = never

// rte_eal_init returns the number of arguments it consumed and puts the program
// name where the `--` was, so the caller passes argv from there and this starts at 1.
static int parse_prog_args(int argc, char *argv[]) {
	for (int i = 1; i < argc; i++) {
		if (strcmp(argv[i], "--port") == 0 && i + 1 < argc) {
			char *spec = argv[++i], *colon = strchr(spec, ':');
			if (!colon) { fprintf(stderr, "bad --port '%s' (expected PROG:DPDK)\n", spec); return 1; }
			uint32_t prog = (uint32_t)strtoul(spec, NULL, 10);
			uint16_t dpdk = (uint16_t)strtoul(colon + 1, NULL, 10);
			if (!rte_eth_dev_is_valid_port(dpdk)) { fprintf(stderr, "--port %s: no DPDK port %u\n", spec, dpdk); return 1; }
			prog_of_dpdk[dpdk] = prog;
			fprintf(stderr, "bound port %u to DPDK port %u\n", prog, dpdk);
		} else if (strcmp(argv[i], "--idle-exit") == 0 && i + 1 < argc) {
			idle_exit_ms = strtoull(argv[++i], NULL, 10);
		} else if (strcmp(argv[i], "--stats") == 0 && i + 1 < argc) {
			stats_s = strtoull(argv[++i], NULL, 10);
		} else {
			fprintf(stderr, "unknown argument '%s' (after --: --port PROG:DPDK, --idle-exit MS, --stats S)\n", argv[i]);
			return 1;
		}
	}
	return 0;
}
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
} cfg_t;

cfg_t cfg = {
	.rx_ring_size = 1024,
	.tx_ring_size = 1024,
	.num_mbufs = 8191,
	.mbuf_cache_size = 512,
};

static inline int
port_init(cfg_t cfg, uint16_t port, struct rte_mempool *mbuf_pool)
{
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
		fprintf(stderr, "Error during getting device (port %u) info: %s\n",
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

	fprintf(stderr, "DPDK port %u up (%s)\n", port, dev_info.driver_name);
	return 0;
}

void dpdk_init(cfg_t* cfg, int argc, char *argv[]) {
	int eal_args = rte_eal_init(argc, argv);
	if (eal_args < 0)
		rte_exit(EXIT_FAILURE, "Error with EAL initialization\n");
	unsigned nb_ports = rte_eth_dev_count_avail();
	fprintf(stderr, "number of ports: %u\n", nb_ports);
	if (nb_ports == 0)
		rte_exit(EXIT_FAILURE, "No Ethernet ports - bye\n");
	// priv_size carries qe_priv_t (the event + output list) alongside each mbuf's data.
	uint16_t priv_size = RTE_ALIGN(sizeof(qe_priv_t), RTE_MBUF_PRIV_ALIGN);
	mbuf_pool = rte_pktmbuf_pool_create("MBUF_POOL", cfg->num_mbufs * nb_ports,
		cfg->mbuf_cache_size, priv_size, RTE_MBUF_DEFAULT_BUF_SIZE, rte_socket_id());
	if (mbuf_pool == NULL) rte_exit(EXIT_FAILURE, "failed to create mbuf pool. not enough memory?");

	// single core -> single producer / single consumer rings (lockless); they carry
	// mbuf pointers, so plain rte_ring (not the value-carrying _elem variant).
	rx_in     = rte_ring_create("rx_in",     RING_SIZE, rte_socket_id(), RING_F_SP_ENQ | RING_F_SC_DEQ);
	recirc_in = rte_ring_create("recirc_in", RING_SIZE, rte_socket_id(), RING_F_SP_ENQ | RING_F_SC_DEQ);
	tx_in     = rte_ring_create("tx_in",     RING_SIZE, rte_socket_id(), RING_F_SP_ENQ | RING_F_SC_DEQ);
	if (rx_in == NULL || recirc_in == NULL || tx_in == NULL)
		rte_exit(EXIT_FAILURE, "failed to create pipeline rings\n");

	uint16_t portid;
	n_dpdk_ports = 0;
	RTE_ETH_FOREACH_DEV(portid) {
		if (port_init(*cfg, portid, mbuf_pool) != 0)
			rte_exit(EXIT_FAILURE, "Cannot init port %"PRIu16 "\n", portid);
		prog_of_dpdk[portid] = portid;   // identity unless --port says otherwise
		dpdk_ports[n_dpdk_ports++] = portid;
	}
	if (parse_prog_args(argc - eal_args, argv + eal_args) != 0)
		rte_exit(EXIT_FAILURE, "bad program arguments\n");
	tsc_hz = rte_get_tsc_hz();
	tsc0 = rte_rdtsc();
	return;
}

static void dpdk_shutdown(void) {
	uint16_t port;
	RTE_ETH_FOREACH_DEV(port) {
		struct rte_eth_stats es;   // the NIC's own counters
		if (rte_eth_stats_get(port, &es) == 0)
			fprintf(stderr, "DPDK port %u: rx %"PRIu64" tx %"PRIu64" rx-missed %"PRIu64" rx-nombuf %"PRIu64" tx-errors %"PRIu64"\n",
				port, es.ipackets, es.opackets, es.imissed, es.rx_nombuf, es.oerrors);
		rte_eth_dev_stop(port);
		rte_eth_dev_close(port);   // a pcap device flushes its output file here
	}
	rte_eal_cleanup();
	fprintf(stderr, "rx %"PRIu64" tx %"PRIu64" recirc %"PRIu64" dropped: parse %"PRIu64" jumbo %"PRIu64
		" recirc-full %"PRIu64" alloc %"PRIu64" no-port %"PRIu64" tx-full %"PRIu64" bug %"PRIu64"\n",
		st.rx, st.tx, st.recirc, st.parse_drop, st.seg_drop, st.recirc_drop, st.alloc_drop, st.port_drop,
		st.tx_drop, st.bug_drop);
}
|}]

(* ===== pipeline stages ===== *)

(* RX: poll every port for at most what fits on rx_in, parse, enqueue. *)
let dpdk_do_rx = dforiegn [%string {|
/******** RX: poll every port for what fits on rx_in, parse, enqueue ********/
static uint64_t last_active;   // rdtsc of the last packet received, sent or recirculated
static unsigned do_rx(void) {
	unsigned total = 0;
	uint16_t port;
	RTE_ETH_FOREACH_DEV(port) {
		unsigned room = rte_ring_free_count(rx_in);   // pull only what fits: the rest waits in the NIC
		if (room == 0) break;
		struct rte_mbuf *bufs[BURST_SIZE];
		const uint16_t nb_rx = rte_eth_rx_burst(port, 0, bufs, room < BURST_SIZE ? (uint16_t)room : BURST_SIZE);
		total += nb_rx;
		for (uint16_t i = 0; i < nb_rx; i++) {
			struct rte_mbuf *m = bufs[i];
			if (unlikely(m->nb_segs > 1)) { st.seg_drop++; rte_pktmbuf_free(m); continue; } // the view is one segment
			qe_priv_t *p = qe(m);
			uint32_t in_port = prog_of_dpdk[port];                 // the program's number for it
			%{packet_t_ty} view = view_over(m);
			if (%{parse_event_fn}(&view, &p->ev, in_port) != 1) { st.parse_drop++; rte_pktmbuf_free(m); continue; } // drop
			p->payload_off = (uint32_t)(view.cursor - view.start); // where the payload begins
			p->ev.meta.in_port = in_port;                          // stamp ingress (read by the handler)
			st.rx++;
			if (unlikely(rte_ring_enqueue(rx_in, m) != 0)) { st.bug_drop++; rte_pktmbuf_free(m); } // room was checked
		}
	}
	return total;
}
|}]

(* DISPATCH: a burst from each input ring; handle each, populate the out_event
   list and continue to the tx stage. *)
let dpdk_do_dispatch = dforiegn [%string {|
/******** DISPATCH: a burst from recirc_in and one from rx_in; handle each, hand it to tx_in ********/
static inline void handle_one(struct rte_mbuf *m) {
	qe_priv_t *p = qe(m);
	p->ev.meta.timestamp = now_ns() & 0xFFFFFFFFFFFFull; // stamp at dequeue (Sys.time())
	p->n_out = %{handle_event_fn}(&p->ev, p->out_events);   // ingress read from p->ev.meta.in_port
	if (unlikely(rte_ring_enqueue(tx_in, m) != 0)) { st.bug_drop++; rte_pktmbuf_free(m); } // tx_in is drained every iteration
}
static void do_dispatch(void) {
	void *batch[BURST_SIZE];
	unsigned nb = rte_ring_dequeue_burst(recirc_in, batch, BURST_SIZE, NULL);
	for (unsigned b = 0; b < nb; b++) handle_one((struct rte_mbuf *)batch[b]);
	nb = rte_ring_dequeue_burst(rx_in, batch, BURST_SIZE, NULL);
	for (unsigned b = 0; b < nb; b++) handle_one((struct rte_mbuf *)batch[b]);
}
|}]

(* TX: copy mbufs to output and recirculation ports. For events leaving 
   on ports, deparse the event into the mbuf's headroom and transmit.
	 Recirculated events are enqueued on recirc_in without deparsing. *)
let dpdk_do_tx = dforiegn [%string {|
/******** TX: fan out each event-mbuf into one copy per output, then route/deparse/send ********/
static void do_tx(void) {
	void *batch[BURST_SIZE];
	unsigned nb;
	while ((nb = rte_ring_dequeue_burst(tx_in, batch, BURST_SIZE, NULL)) > 0) { // drain it whole
		for (unsigned b = 0; b < nb; b++) {
			struct rte_mbuf *m = (struct rte_mbuf *)batch[b];
			qe_priv_t *ip = qe(m);
			uint32_t pkt_len = rte_pktmbuf_pkt_len(m);
			for (uint16_t i = 0; i < ip->n_out; i++) {
				%{out_event_ty} *oe = &ip->out_events[i];
				// each output gets its own copy of the input's payload (or none), with
				// headroom in front for the deparsed header.
				uint32_t plen = oe->ev.meta.has_payload ? (pkt_len - ip->payload_off) : 0;
				struct rte_mbuf *c = plen > 0
					? rte_pktmbuf_copy(m, mbuf_pool, ip->payload_off, plen)
					: rte_pktmbuf_alloc(mbuf_pool);
				if (unlikely(c == NULL)) { st.alloc_drop++; continue; }
				qe_priv_t *cp = qe(c);
				cp->ev = oe->ev;
				cp->payload_off = 0;                // the payload now sits at the front of c
				if (oe->port == PORT_RECIRC) {          // recirculation (generate_self)
					cp->ev.meta.in_port = %{recirc_port_var}; // it arrives on the recirculation port
					if (rte_ring_enqueue(recirc_in, c) != 0) { st.recirc_drop++; rte_pktmbuf_free(c); continue; } // drop-tail
					st.recirc++;
				} else {                                // output to a port: deparse + transmit
					uint16_t dport = dpdk_of_prog(oe->port);
					if (dport == PORT_NONE) { st.port_drop++; rte_pktmbuf_free(c); continue; }
					%{packet_t_ty} view = view_over(c); // cursor at the payload boundary (front of c)
					%{deparse_event_fn}(&cp->ev, &view); // writes the header backwards into headroom
					rte_pktmbuf_prepend(c, (uint16_t)(view.start - view.cursor)); // include the header
					if (unlikely(rte_eth_tx_burst(dport, 0, &c, 1) < 1)) { st.tx_drop++; rte_pktmbuf_free(c); continue; }
					st.tx++;
				}
				last_active = rte_rdtsc();
			}
			rte_pktmbuf_free(m); // input consumed (copied per output)
		}
	}
}
|}]

(* ===== main loop ===== *)
let dpdk_main = dforiegn {|
/******** main loop ********/
static volatile bool force_quit = false;
static void on_signal(int sig) { (void)sig; force_quit = true; }

// --stats: the period's rates, the program's and the NIC's (summed over its ports)
static void print_stats(uint64_t now, uint64_t hz) {
	static uint64_t t0, rx0, tx0, recirc0, nic_rx0, nic_missed0;
	uint64_t nic_rx = 0, nic_missed = 0;
	struct rte_eth_stats es;
	uint16_t port;
	RTE_ETH_FOREACH_DEV(port)
		if (rte_eth_stats_get(port, &es) == 0) { nic_rx += es.ipackets; nic_missed += es.imissed; }
	if (t0 != 0) {
		double dt = (double)(now - t0) / (double)hz;
		fprintf(stderr, "stats rx %.0f tx %.0f recirc %.0f nic-rx %.0f nic-missed %.0f pkt/s\n",
			(st.rx - rx0) / dt, (st.tx - tx0) / dt, (st.recirc - recirc0) / dt,
			(nic_rx - nic_rx0) / dt, (nic_missed - nic_missed0) / dt);
	}
	t0 = now; rx0 = st.rx; tx0 = st.tx; recirc0 = st.recirc; nic_rx0 = nic_rx; nic_missed0 = nic_missed;
}

static void lcore_main(void) {
	fprintf(stderr, "handler loop running -- ctrl-c to quit\n");
	uint64_t hz = tsc_hz;
	uint64_t idle_cycles = idle_exit_ms * hz / 1000;
	uint64_t stats_cycles = stats_s * hz, stats_next = rte_rdtsc() + stats_cycles;
	if (stats_cycles) print_stats(rte_rdtsc(), hz);
	last_active = rte_rdtsc();
	while (!force_quit) {
		if (stats_cycles && rte_rdtsc() >= stats_next) { print_stats(rte_rdtsc(), hz); stats_next += stats_cycles; }
		if (do_rx() > 0) last_active = rte_rdtsc(); // poll the ports for what fits, parse, enqueue on rx_in
		do_dispatch();  // a burst from each input ring, handled and forwarded to tx_in
		do_tx();        // drain tx_in: clone, deparse, send; a recirculation goes onto recirc_in
		if (idle_cycles && rte_rdtsc() - last_active > idle_cycles) break;
	}
}

int main(int argc, char *argv[]) {
	setvbuf(stdout, NULL, _IOLBF, 0); // the program's prints, line by line even into a pipe
	signal(SIGINT, on_signal);
	signal(SIGTERM, on_signal);
	dpdk_init(&cfg, argc, argv); // init dpdk, ports, memory pools, rings
	lcore_main();                // the rx / dispatch / tx loop
	dpdk_shutdown();
	return 0;
}
|}

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
	let sm = section_marker in
[
	"lucidprog.c", `Decls (
		[dpdk_includes]
		@ [sm "program code"] @ decls
		@ [sm "driver config + init"; dpdk_queue; dpdk_config_init]
		@ [sm "driver pipeline"; dpdk_do_rx; dpdk_do_dispatch; dpdk_do_tx]
		@ [sm "driver main"; dpdk_main]);
	(* capital M: the DPDK makefile lists `Makefile` as a prerequisite of its build
	   rules, so the on-disk name must match (Linux is case-sensitive). *)
	"Makefile", `String makefile;
	"run.sh", `String run_sh;
]
