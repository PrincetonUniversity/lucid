// A template for a generic harness that passes 
// some arbitrary fields from the IPv4 header 
// to an entry event. 
// generate_template.py should put application-specific 
// code in DISPATCH_SETUP_ACN
#include <core.p4>
#include <tna.p4>


@DPT_HEADERS

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
    ethernet_h ethernet;
    @DPT_HEADER_INSTANCES
    ipv4_h ip;
}

// Copy of fields passed to entry events. 
struct hdrcpy_t {    
    bit<9> ingress_port;
    bit<32> arg0;
    bit<32> arg1;    
    bit<32> arg2;
}

// Global metadata.
struct metadata_t {
    @DPT_METADATA_INSTANCES
    hdrcpy_t hdrcpy;
}

@DPT_PARSER

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
        // these three lines should be inserted by the lucid compiler. 
        md.dptMeta.exitEventType = 0;
        md.dptMeta.nextEventType = 0;        
        md.dptMeta.timestamp = (bit<32>)(ig_intr_md.ingress_mac_tstamp[47:16]); 
        md.hdrcpy.ingress_port = ig_intr_md.ingress_port;
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
        transition set_hdrcpy;
    }
    state set_hdrcpy {
        md.hdrcpy.arg0 = hdr.ip.src_addr;
        md.hdrcpy.arg1 = hdr.ip.dst_addr;   
        md.hdrcpy.arg2 = 0x1;   
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
    state start {
        TofinoIngressParser.apply(pkt, ig_intr_md, hdr, md);
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

    // Giving entry events copies of inputs makes PHV allocation easier
    // for the p4 compiler.
    @pa_solitary("ingress", "md.hdrcpy.arg1")
    @pa_no_overlay("ingress", "md.hdrcpy.arg1")
    @pa_solitary("ingress", "md.hdrcpy.arg2")
    @pa_no_overlay("ingress", "md.hdrcpy.arg2")
    @pa_solitary("ingress", "md.hdrcpy.arg3")
    @pa_no_overlay("ingress", "md.hdrcpy.arg3")

    bit<32> out_port; 
    bit<32> in_port = (bit<32>)ig_intr_md.ingress_port;

    @DPT_OBJECTS

    action dispatch_setup()
    {
    md.pktin.src = md.hdrcpy.arg0;
    md.pktin.dst = md.hdrcpy.arg1;
    md.dptMeta.eventType = e_pktin;
	}

    apply {
        dispatch_setup();
        @DPT_DISPATCH
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
    @DPT_EGRESS_OBJECTS
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