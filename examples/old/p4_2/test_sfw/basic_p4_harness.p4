// Tofino-specific definitions.
#include <core.p4>
#include <tna.p4>

// #include "./dptgen/dptHeaders.p4"
@DPT_HEADERS

// DPT Fixed definitions and functions. 
// #define DPT_RECIRC_PORT 196
// we use port 1 on the simulator.
#define DPT_RECIRC_PORT 1
// port 128 in closetlab
#define TEST_PORT 0

#define IS_HIT 2

#define DPT_MIR_SES     66
#define DPT_ETYPE 0x6666



/*=============================================
=            Headers and metadata.            =
=============================================*/
typedef bit<48> mac_addr_t;
header ethernet_h {
    mac_addr_t dst_addr;
    mac_addr_t src_addr;
    bit<16> ether_type;
}

typedef bit<32> ipv4_addr_t;
header ipv4_h {
    bit<4> version;
    bit<4> ihl;
    bit<8> diffserv;
    bit<16> total_len;
    bit<16> identification;
    bit<3> flags;
    bit<13> frag_offset;
    bit<8> ttl;
    bit<8> protocol;
    bit<16> hdr_checksum;
    ipv4_addr_t src_addr;
    ipv4_addr_t dst_addr;
}

// Global headers.
struct header_t {
    // no DPT headers required in header stack 
    // the parse functions read per-event headers and 
    // set appropriate metadata fields in md.dptArgs
    ethernet_h ethernet;
    @DPT_HEADER_INSTANCES
    ipv4_h ip;
}

// Global metadata.
struct metadata_t {
    @DPT_METADATA_INSTANCES
    MirrorId_t dpt_mir_ses;
    bit<9> ingress_port;
    bit<32> src_addr;
    bit<32> dst_addr;
}

// DPT works best when you give it input that 
// cannot be overlaid with packet headers. 
// Otherwise, the compiler will fuck itself 
// up by trying to over-eagerly optimize.
@pa_solitary("ingress", "md.src_addr")
@pa_no_overlay("ingress", "md.src_addr")


@DPT_PARSER
// #include "./dptgen/dptParser.p4"

/*===============================
=            Parsing            =
===============================*/
// Parser for tofino-specific metadata.
parser TofinoIngressParser(
        packet_in pkt,        
        out ingress_intrinsic_metadata_t ig_intr_md,
        out header_t hdr,
        out metadata_t md) {
    state start {
        pkt.extract(ig_intr_md);
        // DPT: populate metadata.
        md.dptMeta.exitEventType = 0;
        md.dptMeta.nextEventType = 0;        
        md.dptMeta.timestamp = (bit<32>)(ig_intr_md.ingress_mac_tstamp[47:16]); 
        md.dpt_mir_ses = DPT_MIR_SES;
        md.ingress_port = ig_intr_md.ingress_port;
        transition select(ig_intr_md.resubmit_flag) {
            1 : parse_resubmit;
            0 : parse_port_metadata;
        }
    }
    state parse_resubmit {
        // Parse resubmitted packet here.
        transition reject;
    }
    state parse_port_metadata {
        pkt.advance(64); // skip this.
        transition accept;
    }
}
// Parser for eth/ip.
const bit<16> ETHERTYPE_IPV4 = 16w0x0800;
const bit<16> ETHERTYPE_DPT = 0x6666;

parser EthIpParser(packet_in pkt, out header_t hdr, out metadata_t md){
    DptIngressParser() dptIngressParser; // FIXED INTEGRATION.
    state start {
        pkt.extract(hdr.ethernet);
        transition select(hdr.ethernet.ether_type) {
            ETHERTYPE_IPV4 : parse_ip;
            ETHERTYPE_DPT  : parse_dpt;
            default : accept;
        }
    }
    state parse_dpt {
        dptIngressParser.apply(pkt, hdr, md);                        
        transition parse_ip;
    }
    state parse_ip {
        pkt.extract(hdr.ip);
        md.src_addr = hdr.ip.src_addr;
        md.dst_addr = hdr.ip.dst_addr;
        transition accept;
    }
}


parser TofinoEgressParser(
        packet_in pkt,
        out egress_intrinsic_metadata_t eg_intr_md) {
    state start {
        pkt.extract(eg_intr_md);
        transition accept;
    }
}


/*========================================
=            Ingress parsing             =
========================================*/

parser IngressParser(
        packet_in pkt,
        out header_t hdr, 
        out metadata_t md,
        out ingress_intrinsic_metadata_t ig_intr_md)
{
    // <forwarding.p4>
    TofinoIngressParser() tofino_parser; 
    EthIpParser() eth_ip_parser;

    state start {
        tofino_parser.apply(pkt, ig_intr_md, hdr, md);
        EthIpParser.apply(pkt, hdr, md);
        transition accept;
    }
}



/*===========================================
=            ingress match-action             =
===========================================*/
control Ingress(
        inout header_t hdr, 
        inout metadata_t md,
        in ingress_intrinsic_metadata_t ig_intr_md,
        in ingress_intrinsic_metadata_from_parser_t ig_prsr_md,
        inout ingress_intrinsic_metadata_for_deparser_t ig_dprsr_md,
        inout ingress_intrinsic_metadata_for_tm_t ig_tm_md) {

    bit<32> out_port; 
    bit<32> in_port = (bit<32>)ig_intr_md.ingress_port;
    @DPT_OBJECTS

    action dispatch_setup(){
        // record when the packet entered the switch in the source mac.
        hdr.ethernet.src_addr = ig_intr_md.ingress_mac_tstamp;
        // use IP source as key. 
        md.dpt_pktin.pktin_ip = md.src_addr;
        md.dptMeta.eventType = pktin;
    }
    action handle_dispatch_result() {
        // in the lucid implementation, finish only gets called 
        // when there's a hit.
        // encode HIT / MISS in ip dst addr.
        // record when the packet left the switch in the dest mac.
        hdr.ethernet.dst_addr = ig_intr_md.ingress_mac_tstamp;
        hdr.ip.dst_addr = IS_HIT;
        // hdr.ip.dst_addr = md.dpt_continue.continue_status;
        // set port to test server.
        ig_tm_md.ucast_egress_port  = TEST_PORT; // mirror.
    }
    apply {
        dispatch_setup();
        @DPT_DISPATCH
        handle_dispatch_result();
    }
}



control IngressDeparser(
        packet_out pkt, 
        inout header_t hdr, 
        in metadata_t md,
        in ingress_intrinsic_metadata_for_deparser_t ig_dprsr_md) {
    apply {
        pkt.emit(hdr);
    }
}


/*======================================
=            Egress parsing            =
======================================*/
parser EgressParser(
        packet_in pkt,
        out header_t hdr, 
        out metadata_t eg_md,
        out egress_intrinsic_metadata_t eg_intr_md) {
    TofinoEgressParser() tofino_parser;
    EthIpParser() eth_ip_parser; 
    state start {
        tofino_parser.apply(pkt, eg_intr_md);
        transition parse_packet;
    }
    state parse_packet {
        eth_ip_parser.apply(pkt, hdr, eg_md);
        transition accept;        
    }
}

/*=========================================
=            Egress match-action            =
=========================================*/
control Egress(
        inout header_t hdr, 
        inout metadata_t eg_mg,
        in egress_intrinsic_metadata_t eg_intr_md,
        in egress_intrinsic_metadata_from_parser_t eg_prsr_md,
        inout egress_intrinsic_metadata_for_deparser_t eg_dprsr_md,
        inout egress_intrinsic_metadata_for_output_port_t eg_oport_md){
    apply { 
        @DPT_EGRESS
    }
}


control EgressDeparser(
        packet_out pkt,
        inout header_t hdr, 
        in metadata_t eg_md,
        in egress_intrinsic_metadata_for_deparser_t eg_dprsr_md) {
    apply {
        pkt.emit(hdr);
    }
}
/*==============================================
=            The switch's pipeline             =
==============================================*/
Pipeline(
    IngressParser(), Ingress(), IngressDeparser(),
    EgressParser(), Egress(), EgressDeparser()) pipe;

Switch(pipe) main;