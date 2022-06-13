/* 
This harness is for ingress programs that p
    - entry event generators and exit event handlers are generated from 
      ip_harness_triggers.json and inlined into ingress at @ENTRY_TRIGGER_CALL
      and @EXIT_ACTION_CALL
*/

#include <core.p4>
#include <tna.p4>


//Compiler-generated code for: @DPT_HEADERS
    #define e_ip_in 1
    #define e_ip_out 2
    #define e_raw_out 3
    struct fmt_arr_1 {
        bit<32> lo;
    }
    struct fmt_arr_2 {
        bit<32> lo;
    }
    struct ip_in_t {
        bit<8> eventType;
        bit<32> eventLoc;
        bit<32> eventDelay;
        bit<9> igr_port;
        bit<32> src;
        bit<32> dst;
        bit<16> len;
        bit<8> tos;
    }
    struct ip_out_t {
        bit<8> eventType;
        bit<32> eventLoc;
        bit<32> eventDelay;
        bit<32> src;
        bit<32> dst;
    }
    struct raw_out_t {
        bit<8> eventType;
        bit<32> eventLoc;
        bit<32> eventDelay;
    }
    header lucid_footer_t {
        bit<8> end;
    }
    header ev_out_flags_t {
        bit<1> ip_in;
        bit<1> ip_out;
        bit<1> raw_out;
        bit<5> pad_1_meta;
    }
    struct dptMeta_t {
        bit<32> timestamp;
        bit<8> eventType;
        bit<8> exitEventType;
        bit<8> nextEventType;
        bit<16> eventsCount;
        bit<9> outPort;
        bit<16> outGroup;
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
    //Compiler-generated code for: @DPT_HEADER_INSTANCES
    lucid_footer_t lucid_footer;
    ev_out_flags_t ev_out_flags;
    ipv4_h ip;
}
struct metadata_t {
    //Compiler-generated code for: @DPT_METADATA_INSTANCES
    ip_in_t ip_in;
    ip_out_t ip_out;
    raw_out_t raw_out;
    dptMeta_t dptMeta;
}

//Compiler-generated code for: @DPT_PARSER
parser DptIngressParser (packet_in pkt, out header_t hdr, out metadata_t md) {
    state start {
        md.dptMeta.eventType = pkt.lookahead<bit<8>>();
        transition select(md.dptMeta.eventType) {
            0 : finish;
            
        }
    }
    state selector_1 {
        bit<8> tmp = pkt.lookahead<bit<8>>();
        transition select(tmp) {
            0 : finish;
            
        }
    }
    state selector_2 {
        bit<8> tmp = pkt.lookahead<bit<8>>();
        transition select(tmp) {
            0 : finish;
            
        }
    }
    state selector_3 {
        bit<8> tmp = pkt.lookahead<bit<8>>();
        transition select(tmp) {
            0 : finish;
            
        }
    }
    state finish {
        pkt.extract(hdr.lucid_footer);
        
        pkt.extract(hdr.ev_out_flags);
        transition accept;
        
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

    //Compiler-generated code for: @ENTRY_OBJECTS
action trigger_ip_in() {
  md.dptMeta.eventType=e_ip_in;
  md.ip_in.igr_port = ig_intr_md.ingress_port;
  md.ip_in.src = hdr.ip.src_addr;
  md.ip_in.dst = hdr.ip.dst_addr;
  md.ip_in.len = hdr.ip.total_len;
  md.ip_in.tos = hdr.ip.tos;
}
table entry_table {
  key = {
    hdr.ip.isValid() : ternary;
    ig_intr_md.ingress_port : ternary;
  }
  actions = {
    trigger_ip_in;
  }
  const entries = {
    (true, 128) : trigger_ip_in();
  }
}
    //Compiler-generated code for: @EXIT_OBJECTS
action a_ip_out() {
  hdr.ip.src_addr = md.ip_out.src;
  hdr.ip.dst_addr = md.ip_out.dst;
}
action a_raw_out() {
  
}
table exit_table {
  key = {
    md.dptMeta.exitEventType : ternary;
  }
  actions = {
    a_ip_out; a_raw_out;
  }
  const entries = {
    (e_ip_out) : a_ip_out();
    (e_raw_out) : a_raw_out();
  }
}

    //Compiler-generated code for: @DPT_OBJECTS
 
    bit<32> a1_out;
    
    bit<32> a2_out;
    
    Register<fmt_arr_1, bit<32>>(1024) arr_1;
    
    Register<fmt_arr_2, bit<32>>(1024) arr_2;
    
    
    
    RegisterAction<fmt_arr_1,bit<32>,bit<32>>(arr_1) dpt_776_sprog_809_salu_0_opstmt = {
        void apply(inout fmt_arr_1 remote, out bit<32> remoteRet) {
            fmt_arr_1 local = remote;
            bit<32> remoteHi = 0;
            if (local.lo == 10) { remote.lo = 0; }
            if (! (local.lo == 10)) { remote.lo = (1 + local.lo); }
            
            remoteRet = remote.lo;
            }
        };
        action dpt_776_salu_0_opstmt() {
            a1_out=dpt_776_sprog_809_salu_0_opstmt.execute((bit<32>)0);
        }
        
        action dpt_786_merged_acn_1_noop( ){
            //next tables: []
            
        }
        
        action dpt_776_merged_acn_1_acn_0_opstmt( ){
            dpt_776_salu_0_opstmt();
            //next tables: [dpt_777_tbl_0_opstmt]
            
        }
        
        @pragma stage 0
        @ignore_table_dependency("Ingress.dpt_2_merged_tbl")
        table dpt_1_merged_tbl {
            key = {
                md.dptMeta.eventType : ternary;
            }
            actions = {
                dpt_776_merged_acn_1_acn_0_opstmt;
                dpt_786_merged_acn_1_noop;
            }
            const entries = {
                1 : dpt_776_merged_acn_1_acn_0_opstmt();
                _ : dpt_786_merged_acn_1_noop();
            }
            
        }
        
        RegisterAction<fmt_arr_2,bit<32>,bit<32>>(arr_2) dpt_777_sprog_810_salu_0_opstmt = {
            void apply(inout fmt_arr_2 remote, out bit<32> remoteRet) {
                fmt_arr_2 local = remote;
            bit<32> remoteHi = 0;
            remote.lo = md.ip_in.src;
            
            if (local.lo == md.ip_in.dst) { remoteHi = 1; }
            if (! (local.lo == md.ip_in.dst)) { remoteHi = 0; }
            remoteRet = remoteHi;
                }
            };
            action dpt_777_salu_0_opstmt() {
                a2_out=dpt_777_sprog_810_salu_0_opstmt.execute((bit<32>)0);
            }
            
            action dpt_777_merged_acn_2_acn_0_opstmt( ){
                dpt_777_salu_0_opstmt();
                //next tables: [dpt_779_tbl_0_opstmt]
                
            }
            
            @pragma stage 0
            @ignore_table_dependency("Ingress.dpt_1_merged_tbl")
            table dpt_2_merged_tbl {
                key = {
                    md.dptMeta.eventType : ternary;
                }
                actions = {
                    dpt_777_merged_acn_2_acn_0_opstmt;
                    dpt_786_merged_acn_1_noop;
                }
                const entries = {
                    1 : dpt_777_merged_acn_2_acn_0_opstmt();
                    _ : dpt_786_merged_acn_1_noop();
                }
                
            }
            
            action dpt_779_generate_port_alu_0_opstmt( ){
                md.ip_out.src = a1_out;
                md.ip_out.dst = a2_out;
                md.ip_out.eventType = 2;
                md.ip_out.eventLoc = 0;
                md.ip_out.eventDelay = 0;
                md.dptMeta.exitEventType = 2;
                ig_tm_md.ucast_egress_port = md.ip_in.igr_port;
                
            }
            
            action dpt_779_merged_acn_3_acn_0_opstmt( ){
                dpt_779_generate_port_alu_0_opstmt();
                //next tables: []
                
            }
            
            @pragma stage 1
            table dpt_3_merged_tbl {
                key = {
                    md.dptMeta.eventType : ternary;
                }
                actions = {
                    dpt_779_merged_acn_3_acn_0_opstmt;
                    dpt_786_merged_acn_1_noop;
                }
                const entries = {
                    1 : dpt_779_merged_acn_3_acn_0_opstmt();
                    _ : dpt_786_merged_acn_1_noop();
                }
                
            }
             
    action from_any_to_none() {
  ig_dprsr_md.drop_ctl = 0x1;
  exit;
}
action from_bg_to_wire() {
  hdr.ethernet.ether_type = 0x0800;
  hdr.lucid_footer.setInvalid();
  hdr.ev_out_flags.setInvalid();
}
action from_wire_to_bg() {
  ig_tm_md.mcast_grp_a = 1065 + md.dptMeta.eventsCount;
  hdr.ethernet.ether_type = 0x1111;
  exit;
}
action from_wire_to_bg_wire() {
  ig_tm_md.mcast_grp_a = 1065 + md.dptMeta.eventsCount;
  hdr.ethernet.ether_type = 0x1111;
}
table dpt_0_lucid_return_table {
  key = {
    md.dptMeta.eventType : ternary;
    md.dptMeta.eventsCount : ternary;
    md.dptMeta.exitEventType : ternary;
  }
  actions = {
    from_any_to_none; from_bg_to_wire; from_wire_to_bg; from_wire_to_bg_wire;
  }
  const entries = {
    (_, 0, 0) : from_any_to_none();
    (_, 0, _) : from_bg_to_wire();
    (_, _, 0) : from_wire_to_bg();
    (_, _, _) : from_wire_to_bg_wire();
  }
}
    

    CiL2Fwd() ciL2Fwd; 

    apply {
        // call the entry trigger table. 
        //Compiler-generated code for: @ENTRY_CALL
if (md.dptMeta.eventType == 0) { entry_table.apply();}

        // If there's an event, call the Lucid handlers. 
        if (md.dptMeta.eventType != 0) {
            //Compiler-generated code for: @DPT_HANDLERS
 
    dpt_1_merged_tbl.apply();
    dpt_2_merged_tbl.apply();
    dpt_3_merged_tbl.apply(); 
    dpt_0_lucid_return_table.apply();

            // Handle any exit events produced by lucid. 
            //Compiler-generated code for: @EXIT_CALL
if (md.dptMeta.exitEventType != 0) { exit_table.apply();}
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
    //Compiler-generated code for: @DPT_EGRESS_OBJECTS
 
    action acn_nonlucid_wire() {
  
}
action acn_lucid_wire() {
  hdr.ethernet.ether_type = 0x0800;
  hdr.lucid_footer.setInvalid();
  hdr.ev_out_flags.setInvalid();
}
table dpt_0_egr_serialize_clone {
  key = {
    hdr.ethernet.ether_type : ternary;
    eg_intr_md.egress_rid : ternary;
  }
  actions = {
    acn_nonlucid_wire; acn_lucid_wire;
  }
  const entries = {
    (2048, 0) : acn_nonlucid_wire();
    (4369, 0) : acn_lucid_wire();
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