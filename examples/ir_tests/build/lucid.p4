// Tofino-specific definitions.
#include <core.p4>
#include <tna.p4>

#define TEST_OUT_PORT 0
#define DPT_ETYPE 1111
#define DPT_RECIRC_PORT 196

//Compiler-generated code for: @DPT_HEADERS
    const bit<8> e_pktin=0;
    struct pktin_t {
        bit<8> eventType;
        bit<8> eventMc;
        bit<32> eventLoc;
        bit<32> eventDelay;
        bit<32> src_ip;
        bit<32> dst_ip;
    }
    struct dptMeta_t {
        bit<32> timestamp;
        bit<8> eventType;
        bit<8> exitEventType;
        bit<8> nextEventType;
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
    pktin_t pktin;
    dptMeta_t dptMeta;
    hdrcpy_t hdrcpy;
}

//Compiler-generated code for: @DPT_PARSER
parser DptIngressParser (packet_in pkt, out header_t hdr, out metadata_t md) {
    state start {
        md.dptMeta.eventType = pkt.lookahead<bit<8>>();
        transition select(md.dptMeta.eventType) {
            
        }
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
 
    bit<32> x_114;
    
    bit<32> y_115;
    
    
    
    action dpt_363_alu_0_opstmt( ){
        x_114 = md.pktin.src_ip >> 10;
        
    }
    
    action dpt_1_merged_acn( ){
        //next tables: []
        
    }
    
    action dpt_363_merged_acn_1_acn_0_opstmt( ){
        dpt_363_alu_0_opstmt();
        //next tables: [dpt_364_tbl_0_opstmt]
        
    }
    
    @pragma stage 0
    table dpt_1_merged_tbl {
        key = {
            md.dptMeta.eventType : ternary;
        }
        actions = {
            dpt_363_merged_acn_1_acn_0_opstmt;
            dpt_1_merged_acn;
        }
        const entries = {
            0 : dpt_363_merged_acn_1_acn_0_opstmt();
            _ : dpt_1_merged_acn();
        }
        
    }
    
    action dpt_364_alu_0_opstmt( ){
        y_115 = x_114 << 10;
        
    }
    
    action dpt_2_merged_acn( ){
        //next tables: []
        
    }
    
    action dpt_364_merged_acn_2_acn_0_opstmt( ){
        dpt_364_alu_0_opstmt();
        //next tables: []
        
    }
    
    @pragma stage 1
    table dpt_2_merged_tbl {
        key = {
            md.dptMeta.eventType : ternary;
        }
        actions = {
            dpt_364_merged_acn_2_acn_0_opstmt;
            dpt_2_merged_acn;
        }
        const entries = {
            0 : dpt_364_merged_acn_2_acn_0_opstmt();
            _ : dpt_2_merged_acn();
        }
        
    }
     
    action acn_stop() {
	exit;
	ig_dprsr_md.drop_ctl = 0x1;
	}
action acn_return() {
	hdr.ethernet.ether_type = 0x800;
	}
table dpt_0_lucid_return_table {
	key = {md.dptMeta.exitEventType : ternary; md.dptMeta.nextEventType : ternary;}
	actions = {acn_stop; acn_return;}
	const entries = {(0x0, 0x0) : acn_stop(); (0x0, 0x0) : acn_return();}
}
    

    action dispatch_setup(){
        md.pktin.src_ip = md.hdrcpy.src_addr;
        md.dptMeta.eventType = e_pktin;
    }
    action handle_dispatch_result() {
        ig_tm_md.ucast_egress_port  = TEST_OUT_PORT; 
    }
    apply {
	//Linker-added condition: don't apply pre-dispatch tables to background event packets.
	if (md.dptMeta.eventType == 0){
        dispatch_setup();
        }
	//Compiler-generated code for: @DPT_DISPATCH
 
    dpt_1_merged_tbl.apply();
    dpt_2_merged_tbl.apply(); 
    dpt_0_lucid_return_table.apply();
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
    //Compiler-generated code for: @DPT_EGRESS_OBJECTS
 
    
        action egr_disable_events() {
	hdr.ethernet.ether_type = 0x800;
	}
        action egr_noop() { }
        table dpt_0_egr_serialize_clone {
          key = {hdr.ethernet.ether_type : ternary; eg_intr_md.egress_rid : ternary;}
          actions = {egr_disable_events; egr_noop;}
          const entries = {
            (0x1066, 0) : egr_disable_events();
            (0x1066, _) : egr_noop();
            (_, _)      : egr_noop();  
          }
        }
        
    
    apply { 
        //Compiler-generated code for: @DPT_EGRESS
 
    dpt_0_egr_serialize_clone.apply();
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