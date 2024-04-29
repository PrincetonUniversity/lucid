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

// the user/compiler-written packet handler function. 
// return 1 to send, 0 to drop
static inline uint8_t handle_packet(struct rte_mbuf *buf) __attribute__((always_inline));

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
	struct rte_mempool *mbuf_pool;
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
			// run handler
			uint8_t acn = handle_packet(bufs[i]);
			if (likely(acn==1)) {
				// send packet
				if (unlikely(bufs[i]->port >= cfg.num_ports)) {
					rte_pktmbuf_free(bufs[i]);
					printf("WARNING: dropping packet to out-of-range port: %u\n", bufs[i]->port);
				} else {
					uint16_t nb_tx = rte_eth_tx_burst(port, 0, &(bufs[i]), 1);
					if (unlikely(nb_tx < 1)) rte_pktmbuf_free(bufs[i]);
				}
			}
			else {
				rte_pktmbuf_free(bufs[i]);
			}
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


let pkt_handler = dforiegn {|
static inline uint8_t handle_packet(struct rte_mbuf *buf) {

	// setup packet, ingress and egress port locals
	packet_t pkt_in_val = {
		.start = 0,
		.payload = rte_pktmbuf_mtod(buf, char*),
		.end = 0
	};
	packet_t* pkt_in = &pkt_in_val;
	uint16_t ingress_port = buf->port;
	uint16_t egress_port = 0;

	// locals
	event_t ev1_v = {0};
	event_t ev2_v = {0};
	event_t ev_out_v = {0};

	uint8_t send_pkt = 0;

	event_t * ev1 = &ev1_v;
	event_t * ev2 = &ev2_v;
	event_t * ev_out = &ev_out_v;
	event_t * ev_tmp;
	uint8_t parse_success = parse_event(pkt_in, ev1);   
	uint16_t ev_in_len = ev1->len;    
	if (parse_success == 1) {
		// event continuation trampoline
		#pragma unroll 4
		for (int i=0; i < 100; i++) {
			reset_event_tag(ev2);
			reset_event_tag(ev_out); // NEW
			handle_event(ingress_port, ev1, ev2, ev_out, &egress_port);
			if (get_event_tag(ev2) == 0) { // no continuation event to process
				break;
			}
			ev_tmp = ev1;
			ev1 = ev2; 
			ev2 = ev_tmp;
		}
		// we have generated an output event, so we need to 
		// modify the packet buffer so it gets sent out
		if (get_event_tag(ev_out) !=0) {
			buf->port = egress_port;
			// deparse the packet right back to pkt_in
			// first adjust the data offset in the mbuf
			int16_t header_diff = ev_out->len - ev_in_len;
			buf -> data_off -= header_diff;
			deparse_event(ev_out, pkt_in); 
			send_pkt = 1; // NEW
				// writes to payload - hdr, which should 
				// be exactly at the mbuf's data start
		}
	}
	else {
        // parse failed, drop packet
        send_pkt = 0;
	}
	return send_pkt;
} 
|}
;;

let get_event_tag t_event = 
	let ev_param = cid"ev", tref t_event in
	dfun 
		(cid"get_event_tag")
		(tint event_tag_size)
		[ev_param]
		(sret (ecast (tint event_tag_size) ((param_evar ev_param)/->cid"tag")))
;;
let reset_event_tag t_event = 
	(* this isn't right. Need an address.. *)
	let ev_param = cid"ev", tref t_event in
	let enum_ty = ((param_evar ev_param)/->cid"tag").ety in 
	dfun 
		(cid"reset_event_tag")
		(tunit)
		[ev_param]
		(sassign_exp ((param_evar ev_param)/->cid"tag") (ecast (enum_ty) (default_exp (tint event_tag_size))))
;;


let tag_helpers decls = 
    let teventstruct = match (find_ty_opt (CCoreEvents.event_ty_id) decls) with 
       | Some(ty) -> ty
       | _ -> err "no tevent"
    in  
    [
       get_event_tag teventstruct;
       reset_event_tag teventstruct;
    ]
 ;;
 
let helpers = tag_helpers (* don't need cursor init or pkt copy *)
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
	"makefile", `String makefile;
	"run.sh", `String run_sh;
]
