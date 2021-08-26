// Tofino-specific definitions.
#include <core.p4>
#include <tna.p4>

#define TEST_OUT_PORT 0

//Compiler-generated code for: @DPT_HEADERS
//Config defs
#define DPT_ETYPE 0x6666
#define DPT_RECIRC_PORT 196

//Event ID defs
#define pktin 1
#define extra_processing 2
#define continue 3

//DPT context -- external state (from P4).
struct dpt_meta_h {
    bit<8> eventType;
    bit<8> nextEventType;
    bit<8> exitEventType;
    bit<32> timestamp;
}
//Input headers for events
struct dpt_pktin_t {
    bit<8> eventType;
    bit<32> ip;
}
@flexible header dpt_extra_processing_out_t {
    bit<8> dpt_82_eventType;
    bit<32> dpt_74_ip;
}
struct dpt_continue_t {
    bit<8> eventType;
    bit<32> ip;
}
@flexible header dpt_extra_processing_in_t {
    bit<8> dpt_87_eventType;
    bit<32> dpt_39_ip;
}

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
    //Compiler-generated code for: @DPT_HEADER_INSTANCES

    dpt_extra_processing_in_t dpt_extra_processing_in;
    dpt_extra_processing_out_t dpt_extra_processing_out;
    ipv4_h ip;
}

// Copy of fields passed to entry events. 
struct hdrcpy_t {
    bit<9> ingress_port;
    bit<32> src_addr;
    bit<32> dst_addr;    
}

// Global metadata.
struct metadata_t {
    //Compiler-generated code for: @DPT_METADATA_INSTANCES
dpt_meta_h dptMeta;
    dpt_pktin_t dpt_pktin;
    dpt_continue_t dpt_continue;
    hdrcpy_t hdrcpy;
}

//Compiler-generated code for: @DPT_PARSER

parser DptIngressParser (
        packet_in pkt,
        out header_t hdr,
        out  metadata_t md) {
    state start {
        bit<8> eventType = pkt.lookahead<bit<8>>();
        transition select(eventType) {
            extra_processing : parse_dpt_extra_processing_in_t;
            default : accept;
        }
    }
    state parse_dpt_extra_processing_in_t {
        pkt.extract(hdr.dpt_extra_processing_in);
        md.dptMeta.eventType = hdr.dpt_extra_processing_in.dpt_87_eventType;
        transition accept;
     }
}
control DptIngressDeparser(packet_out pkt, inout header_t hdr, in metadata_t md) {
    apply {
            
    }
}

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
        md.hdrcpy.src_addr = hdr.ip.src_addr;
        md.hdrcpy.dst_addr = hdr.ip.dst_addr;   
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

    // DPT gets along with the P4 compiler best if you give entry 
    // event inputs that cannot be overlaid with packet headers. 
    @pa_solitary("ingress", "md.hdrcpy.src_addr")
    @pa_no_overlay("ingress", "md.hdrcpy.src_addr")

    bit<32> out_port; 
    bit<32> in_port = (bit<32>)ig_intr_md.ingress_port;

    //Compiler-generated code for: @DPT_OBJECTS
action dpt_113_noop( ){
    //next tables: []
    
    }
    
    action dpt_88_alu_89_generate_0_extra_processing( ){
        hdr.dpt_extra_processing_out.setValid();
        md.dptMeta.nextEventType = 2;
        hdr.dpt_extra_processing_out.dpt_82_eventType = 2;
        hdr.dpt_extra_processing_out.dpt_74_ip = 0;
        
    }
    
    action dpt_92_alu_93_generate_0_continue( ){
        md.dptMeta.exitEventType = 3;
        md.dpt_continue.eventType = 3;
        md.dpt_continue.ip = hdr.dpt_extra_processing_in.dpt_39_ip;
        
    }
    
    action dpt_88_merged_acn_1_acn_90_generate_0_extra_processing( ){
        dpt_88_alu_89_generate_0_extra_processing();
        //next tables: []
        
    }
    
    action dpt_92_merged_acn_1_acn_94_generate_0_continue( ){
        dpt_92_alu_93_generate_0_continue();
        //next tables: []
        
    }
    
    @pragma stage 0
    table dpt_1_merged_tbl {
        key = {
            md.dptMeta.eventType : ternary;
        }
        actions = {
            dpt_88_merged_acn_1_acn_90_generate_0_extra_processing;
            dpt_92_merged_acn_1_acn_94_generate_0_continue;
            dpt_113_noop;
        }
        const default_action = dpt_113_noop();
        const entries = {
            (1) : dpt_88_merged_acn_1_acn_90_generate_0_extra_processing();
            (2) : dpt_92_merged_acn_1_acn_94_generate_0_continue();
        }
        
    }
     // return to P4 and also clone the packet for event generation.
 action invalidate_input_headers() {
 hdr.dpt_extra_processing_in.setInvalid();
 }
 action continueAndGenerate() {
 invalidate_input_headers();
 ig_tm_md.mcast_grp_a = 1066; // DPT_MC_GRP
 hdr.ethernet.ether_type = DPT_ETYPE;
 }
 // just return to P4
 action continueOnly() { 
 invalidate_input_headers();
 }
 // clone the packet, but don't return to P4
 action generateOnly() {
 invalidate_input_headers();
 hdr.ethernet.ether_type = DPT_ETYPE;
 ig_tm_md.mcast_grp_a = 1066; // DPT_MC_GRP
 exit;
 }
 // no event generated.
 action noEventsOut() {
 invalidate_input_headers();
 // disable unicast, multicast, and resubmission.
 ig_dprsr_md.drop_ctl = 0x1;
 exit;
 }
 table dptContinueHandler {
 key = {md.dptMeta.exitEventType : ternary; md.dptMeta.nextEventType : ternary;}
 actions = {continueAndGenerate; continueOnly; generateOnly; noEventsOut;}
 const entries = {
 (0x0, 0x0) : noEventsOut();
 (_,   0x0) : continueOnly();
 (0x0,   _) : generateOnly();
 (_   ,  _) : continueAndGenerate();
 }
 }
    

    action dispatch_setup(){
        md.dpt_pktin.ip = md.hdrcpy.src_addr;
        md.dptMeta.eventType = pktin;
    }
    action handle_dispatch_result() {
        hdr.ip.src_addr = md.dpt_continue.ip;
        ig_tm_md.ucast_egress_port  = TEST_OUT_PORT; 
    }
    apply {
	//Linker-added condition: don't apply pre-dispatch tables to background event packets.
	if (md.dptMeta.eventType == 0){
        dispatch_setup();
        }
	//Compiler-generated code for: @DPT_DISPATCH

    dpt_1_merged_tbl.apply();
    dptContinueHandler.apply();
    
	//Linker-added condition: don't apply post-dispatch tables to packets without exit events.
	if (md.dptMeta.exitEventType != 0){
        handle_dispatch_result();
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
    apply { 
        //Compiler-generated code for: @DPT_EGRESS
    if (eg_intr_md.egress_port == DPT_RECIRC_PORT){
     // todo for multi-generate support:
     // invalidate all but one event header from each packet.
       
     } else {
     // todo: support packets besides IPv4 by storing original ethertype somewhere.
     hdr.ethernet.ether_type = 0x0800;
       hdr.dpt_extra_processing_out.setInvalid();
    hdr.dpt_extra_processing_in.setInvalid();
     }
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