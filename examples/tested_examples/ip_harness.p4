/* 
This harness is for ingress programs that p
    - entry event generators and exit event handlers are generated from 
      ip_harness_triggers.json and inlined into ingress at @ENTRY_TRIGGER_CALL
      and @EXIT_ACTION_CALL
*/

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
    bit<8> tos;
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

struct ip_event_fields_t {
    bit<8> tos; 
    bit<16> len;
    ipv4_addr_t src;
    ipv4_addr_t dst;    
}

// Global headers and metadata
struct header_t {
    ethernet_h ethernet;
    @DPT_HEADER_INSTANCES
    ipv4_h ip;
}
struct metadata_t {
    @DPT_METADATA_INSTANCES
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
        // DPT: populate metadata.
        md.dptMeta.exitEventType = 0;
        md.dptMeta.nextEventType = 0;        
        md.dptMeta.timestamp = (bit<32>)(ig_intr_md.ingress_mac_tstamp[47:16]); 
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

// MANUAL HARNESS CODE
const bit<16> ETHERTYPE_IPV4 = 16w0x0800;
const bit<16> ETHERTYPE_DPT = 0x1111;
parser EthIpParser(packet_in pkt, out header_t hdr, out metadata_t md){
    DptIngressParser() dptIngressParser; // MANUAL HARNESS CODE
    state start {
        pkt.extract(hdr.ethernet);
        transition select(hdr.ethernet.ether_type) {
            ETHERTYPE_IPV4 : parse_ip;
            ETHERTYPE_DPT  : parse_dpt;
            default : accept;
        }
    }
    // MANUAL HARNESS CODE
    state parse_dpt {
        dptIngressParser.apply(pkt, hdr, md);                        
        transition parse_ip;
    }
    state parse_ip {
        pkt.extract(hdr.ip);
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


control CiL2Fwd(
        in ingress_intrinsic_metadata_t ig_intr_md,
        inout ingress_intrinsic_metadata_for_tm_t ig_tm_md) {
    /* Basic L2 forwarding */
    action aiOut(bit<9> out_port) {
        ig_tm_md.ucast_egress_port = out_port;
    }
    action aiNoop() {}
    action aiReflect() {
        ig_tm_md.ucast_egress_port = ig_intr_md.ingress_port;
    }
    table tiWire {
        key = {
            ig_intr_md.ingress_port : exact;
        }
        actions = {
            aiOut; 
            aiNoop;
            aiReflect;
        }
        const default_action = aiReflect();
    }
    apply {
        tiWire.apply();
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

    @ENTRY_OBJECTS
    @EXIT_OBJECTS

    @DPT_OBJECTS

    CiL2Fwd() ciL2Fwd; 

    apply {
        // call the entry trigger table. 
        @ENTRY_CALL

        // If there's an event, call the Lucid handlers. 
        if (md.dptMeta.eventType != 0) {
            @DPT_HANDLERS

            // Handle any exit events produced by lucid. 
            @EXIT_CALL
        }
        // If there was no event, don't call lucid. 
        else {            
            ciL2Fwd.apply(ig_intr_md, ig_tm_md);                       
        }
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