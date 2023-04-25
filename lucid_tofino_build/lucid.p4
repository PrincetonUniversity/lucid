//Global declarations
//(includes, headers, structs, constants, register arrays)
#include <core.p4>
#include <tna.p4>
header lucid_eth_t {
  bit<48> dst_addr;
  bit<48> src_addr;
  bit<16> etype;
}
header wire_ev_t {
  bit<8> event_id;
}
header bridge_ev_t {
  bit<8> port_event_id;
  bit<4> flag_pad_1608;
  bit<1> mysrereset;
  bit<1> ttl_changes_found;
  bit<1> DNS_packet_out;
  bit<1> DNS_packet_fwd;
}
header mysrereset_t {
  bit<32> mysrereset_mysreidx;
}
header ttl_changes_found_t {
  bit<32> ttl_changes_found_idx;
  bit<8> ttl_changes_found_count;
}
header DNS_packet_out_t {
  bit<32> DNS_packet_out_sip;
  bit<32> DNS_packet_out_cip;
  bit<32> DNS_packet_out_smac;
  bit<32> DNS_packet_out_cmac;
  bit<32> DNS_packet_out_ttl;
}
header DNS_packet_fwd_t {
  bit<32> DNS_packet_fwd_sip;
  bit<32> DNS_packet_fwd_cip;
  bit<32> DNS_packet_fwd_smac;
  bit<32> DNS_packet_fwd_cmac;
  bit<32> DNS_packet_fwd_ttl;
}
struct hdr_t {
  lucid_eth_t lucid_eth;
  wire_ev_t wire_ev;
  bridge_ev_t bridge_ev;
  mysrereset_t mysrereset;
  ttl_changes_found_t ttl_changes_found;
  DNS_packet_out_t DNS_packet_out;
  DNS_packet_fwd_t DNS_packet_fwd;
}
struct meta_t {
  bit<8> egress_event_id;
}
Register<bit<32>,_>(32w8) assigned_var_mysrefst_ttl;
Register<bit<8>,_>(32w8) mysre;
//Main program components (ingress/egress parser, control, deparser)
parser IngressParser(packet_in pkt,
    out hdr_t hdr,
    out meta_t meta,
    out ingress_intrinsic_metadata_t ig_intr_md){
  state start {
    pkt.extract(ig_intr_md);
    pkt.advance(64);
    transition select(ig_intr_md.ingress_port){
      (196) : parse_lucid_eth;
      (_) : default_setup;
    }
  }
  state default_setup {
    hdr.wire_ev.setValid();
    hdr.wire_ev.event_id=1;
    hdr.bridge_ev.setValid();
    hdr.bridge_ev.port_event_id=0;
    hdr.bridge_ev.mysrereset=0;
    hdr.bridge_ev.ttl_changes_found=0;
    hdr.bridge_ev.DNS_packet_out=0;
    hdr.bridge_ev.DNS_packet_fwd=0;
    transition parse_DNS_packet_fwd;
  }
  state parse_lucid_eth {
    pkt.advance(112);
    transition parse_wire_ev;
  }
  state parse_wire_ev {
    pkt.extract(hdr.wire_ev);
    pkt.extract(hdr.bridge_ev);
    transition select(hdr.wire_ev.event_id){
      (255) : parse_all_events;
      (4) : parse_mysrereset;
      (3) : parse_ttl_changes_found;
      (2) : parse_DNS_packet_out;
      (1) : parse_DNS_packet_fwd;
    }
  }
  state parse_mysrereset {
    pkt.extract(hdr.mysrereset);
    transition accept;
  }
  state parse_ttl_changes_found {
    pkt.extract(hdr.ttl_changes_found);
    transition accept;
  }
  state parse_DNS_packet_out {
    pkt.extract(hdr.DNS_packet_out);
    transition accept;
  }
  state parse_DNS_packet_fwd {
    pkt.extract(hdr.DNS_packet_fwd);
    transition accept;
  }
  state parse_all_events {
    pkt.extract(hdr.mysrereset);
    pkt.extract(hdr.ttl_changes_found);
    pkt.extract(hdr.DNS_packet_out);
    pkt.extract(hdr.DNS_packet_fwd);
    transition accept;
  }
}
control IngressControl(inout hdr_t hdr,
    inout meta_t meta,
    in ingress_intrinsic_metadata_t ig_intr_md,
    in ingress_intrinsic_metadata_from_parser_t ig_prsr_md,
    inout ingress_intrinsic_metadata_for_deparser_t ig_dprsr_md,
    inout ingress_intrinsic_metadata_for_tm_t ig_tm_md){
  bit<32> mysre_idx_1590;
  bit<32> assigned_var_mysrefst_ttl_idx_1589;
  action labeledstmt_36(){
    hdr.DNS_packet_out.setInvalid();
  }
  action labeledstmt_1(){
    labeledstmt_36();
  }
  action labeledstmt_37(){
    hdr.ttl_changes_found.setInvalid();
  }
  action labeledstmt_2(){
    labeledstmt_37();
  }
  action labeledstmt_39(){
    mysre_idx_1590=hdr.mysrereset.mysrereset_mysreidx;
  }
  action labeledstmt_38(){
    assigned_var_mysrefst_ttl_idx_1589=hdr.mysrereset.mysrereset_mysreidx;
  }
  action labeledstmt_3(){
    labeledstmt_38();
    labeledstmt_39();
  }
  bit<8> resmysre1279;
  action labeledstmt_47(){
    resmysre1279=8w0;
  }
  bit<8> resmysre;
  action labeledstmt_46(){
    resmysre=8w0;
  }
  bit<8> countermysre;
  action labeledstmt_45(){
    countermysre=8w0;
  }
  bit<8> memop_synthesized_meta_varmysre;
  action labeledstmt_44(){
    memop_synthesized_meta_varmysre=8w0;
  }
  bit<8> g_synthesized_meta_varmysre;
  action labeledstmt_43(){
    g_synthesized_meta_varmysre=8w0;
  }
  bit<8> f_synthesized_meta_varmysre;
  action labeledstmt_42(){
    f_synthesized_meta_varmysre=8w0;
  }
  bit<32> mysreidx;
  action labeledstmt_41(){
    mysreidx=32w0;
  }
  bit<1> ansmysre;
  action labeledstmt_40(){
    ansmysre=1w1;
  }
  action labeledstmt_4(){
    labeledstmt_40();
    labeledstmt_41();
    labeledstmt_42();
    labeledstmt_43();
    labeledstmt_44();
    labeledstmt_45();
    labeledstmt_46();
    labeledstmt_47();
    hdr.bridge_ev.DNS_packet_out=1;
    hdr.DNS_packet_out.setValid();
   
hdr.DNS_packet_out.DNS_packet_out_sip=hdr.DNS_packet_fwd.DNS_packet_fwd_sip;
   
hdr.DNS_packet_out.DNS_packet_out_cip=hdr.DNS_packet_fwd.DNS_packet_fwd_cip;
   
hdr.DNS_packet_out.DNS_packet_out_smac=hdr.DNS_packet_fwd.DNS_packet_fwd_smac;
   
hdr.DNS_packet_out.DNS_packet_out_cmac=hdr.DNS_packet_fwd.DNS_packet_fwd_cmac;
   
hdr.DNS_packet_out.DNS_packet_out_ttl=hdr.DNS_packet_fwd.DNS_packet_fwd_ttl;
  }
  action labeledstmt_5(){
    //NOOP
  }
  action labeledstmt_6(){
    //NOOP
  }
  action labeledstmt_48(){
    hdr.mysrereset.setInvalid();
  }
  action labeledstmt_7(){
    labeledstmt_48();
  }
  action labeledstmt_49(){
    assigned_var_mysrefst_ttl_idx_1589=mysreidx;
  }
  action labeledstmt_8(){
    labeledstmt_49();
  }
  action labeledstmt_9(){
    //NOOP
  }
  RegisterAction<bit<32>,bit<32>,bit<32>>(assigned_var_mysrefst_ttl)
  assigned_var_mysrefst_ttl_regaction_1591 = {
    void apply(inout bit<32> cell1_remote,
        out bit<32> ret_remote){
      bit<32> cell1_local=cell1_remote;
      bit<32> cell2_local=0;
      if(true){
        cell1_remote=32w0;
      }
      //NOOP
    }
  };
  action labeledstmt_50(){
   
assigned_var_mysrefst_ttl_regaction_1591.execute(assigned_var_mysrefst_ttl_idx_1589);
  }
  action labeledstmt_10(){
    labeledstmt_50();
  }
  bit<32> fst_ttl;
  RegisterAction<bit<32>,bit<32>,bit<32>>(assigned_var_mysrefst_ttl)
  assigned_var_mysrefst_ttl_regaction_1592 = {
    void apply(inout bit<32> cell1_remote,
        out bit<32> ret_remote){
      bit<32> cell1_local=cell1_remote;
      bit<32> cell2_local=0;
      if((cell1_local==32w0)){
        cell2_local=hdr.DNS_packet_fwd.DNS_packet_fwd_ttl;
      }
      if((!(cell1_local==32w0))){
        cell2_local=cell1_local;
      }
      if(true){
        ret_remote=cell2_local;
      }
    }
  };
  action labeledstmt_51(){
   
fst_ttl=assigned_var_mysrefst_ttl_regaction_1592.execute(assigned_var_mysrefst_ttl_idx_1589);
  }
  action labeledstmt_11(){
    labeledstmt_51();
  }
  action labeledstmt_12(){
    //NOOP
  }
  bit<32> if_precomp;
  action labeledstmt_52(){
    if_precomp=(fst_ttl-hdr.DNS_packet_fwd.DNS_packet_fwd_ttl);
  }
  action labeledstmt_13(){
    labeledstmt_52();
  }
  action labeledstmt_14(){
    //NOOP
  }
  action labeledstmt_54(){
    countermysre=(countermysre<<32w1);
  }
  action labeledstmt_53(){
    hdr.DNS_packet_fwd.setInvalid();
  }
  action labeledstmt_15(){
    labeledstmt_53();
    labeledstmt_54();
  }
  bit<8> to_immediate_tmp;
  action labeledstmt_55(){
    to_immediate_tmp=(countermysre<<32w1);
  }
  action labeledstmt_16(){
    labeledstmt_53();
    labeledstmt_55();
  }
  action labeledstmt_17(){
    //NOOP
  }
  action labeledstmt_56(){
    countermysre=(8w1+to_immediate_tmp);
  }
  action labeledstmt_18(){
    labeledstmt_56();
  }
  action labeledstmt_19(){
    //NOOP
  }
  action labeledstmt_59(){
    f_synthesized_meta_varmysre=8w173;
  }
  action labeledstmt_58(){
    g_synthesized_meta_varmysre=8w5;
  }
  action labeledstmt_57(){
    memop_synthesized_meta_varmysre=8w0;
  }
  action labeledstmt_20(){
    labeledstmt_57();
    labeledstmt_58();
    labeledstmt_59();
  }
  action labeledstmt_62(){
    f_synthesized_meta_varmysre=8w243;
  }
  action labeledstmt_61(){
    g_synthesized_meta_varmysre=8w181;
  }
  action labeledstmt_60(){
    memop_synthesized_meta_varmysre=8w2;
  }
  action labeledstmt_21(){
    labeledstmt_60();
    labeledstmt_61();
    labeledstmt_62();
  }
  action labeledstmt_22(){
    //NOOP
  }
  action labeledstmt_63(){
    mysre_idx_1590=mysreidx;
  }
  action labeledstmt_23(){
    labeledstmt_63();
  }
  action labeledstmt_24(){
    labeledstmt_63();
  }
  action labeledstmt_25(){
    //NOOP
  }
  RegisterAction<bit<8>,bit<32>,bit<8>>(mysre)
  mysre_regaction_1593 = {
    void apply(inout bit<8> cell1_remote,
        out bit<8> ret_remote){
      bit<8> cell1_local=cell1_remote;
      bit<8> cell2_local=0;
      if(true){
        cell1_remote=8w0;
      }
      //NOOP
    }
  };
  action labeledstmt_64(){
    mysre_regaction_1593.execute(mysre_idx_1590);
  }
  action labeledstmt_26(){
    labeledstmt_64();
  }
  RegisterAction<bit<8>,bit<32>,bit<8>>(mysre)
  mysre_regaction_1594 = {
    void apply(inout bit<8> cell1_remote,
        out bit<8> ret_remote){
      bit<8> cell1_local=cell1_remote;
      bit<8> cell2_local=0;
     
if((((cell1_local+g_synthesized_meta_varmysre)<8w2)||(g_synthesized_meta_varmysre==8w8))){
        cell1_remote=(8w8|cell1_local);
      }
     
if(((!((cell1_local+g_synthesized_meta_varmysre)<8w2))&&(!(g_synthesized_meta_varmysre==8w8)))){
        cell1_remote=(f_synthesized_meta_varmysre+8w8);
      }
      if(true){
        ret_remote=cell1_remote;
      }
    }
  };
  action labeledstmt_65(){
    resmysre1279=mysre_regaction_1594.execute(mysre_idx_1590);
  }
  action labeledstmt_27(){
    labeledstmt_65();
  }
  RegisterAction<bit<8>,bit<32>,bit<8>>(mysre)
  mysre_regaction_1595 = {
    void apply(inout bit<8> cell1_remote,
        out bit<8> ret_remote){
      bit<8> cell1_local=cell1_remote;
      bit<8> cell2_local=0;
     
if((((cell1_local+f_synthesized_meta_varmysre)<8w8)||((cell1_local+8w0)<8w1))){
        cell1_remote=(g_synthesized_meta_varmysre^cell1_local);
      }
     
if(((!((cell1_local+f_synthesized_meta_varmysre)<8w8))&&(!((cell1_local+8w0)<8w1)))){
        cell1_remote=(f_synthesized_meta_varmysre+8w8);
      }
      if(true){
        ret_remote=cell1_remote;
      }
    }
  };
  action labeledstmt_66(){
    resmysre1279=mysre_regaction_1595.execute(mysre_idx_1590);
  }
  action labeledstmt_28(){
    labeledstmt_66();
  }
  action labeledstmt_29(){
    //NOOP
  }
  action labeledstmt_67(){
    ansmysre=1w1;
  }
  action labeledstmt_30(){
    labeledstmt_67();
  }
  action labeledstmt_68(){
    ansmysre=1w0;
  }
  action labeledstmt_31(){
    labeledstmt_68();
  }
  action labeledstmt_32(){
    //NOOP
  }
  action labeledstmt_69(){
    ig_tm_md.mcast_grp_a=(ig_tm_md.mcast_grp_a+16w1);
  }
  action labeledstmt_33(){
    hdr.bridge_ev.ttl_changes_found=1;
    hdr.ttl_changes_found.setValid();
    hdr.ttl_changes_found.ttl_changes_found_idx=32w0;
    hdr.ttl_changes_found.ttl_changes_found_count=8w0;
    labeledstmt_69();
  }
  action labeledstmt_34(){
    //NOOP
  }
  action labeledstmt_35(){
    labeledstmt_69();
  }
  table table_1607 {
    key = {
      hdr.wire_ev.event_id : ternary;
    }
    actions = {
      labeledstmt_1;
      labeledstmt_2;
      labeledstmt_3;
      labeledstmt_4;
      labeledstmt_5;
    }
    const entries = {
      (2) : labeledstmt_1();
      (3) : labeledstmt_2();
      (4) : labeledstmt_3();
      (1) : labeledstmt_4();
      (_) : labeledstmt_5();
    } 
  } 
  table table_1606 {
    key = {
      hdr.wire_ev.event_id : ternary;
    }
    actions = {
      labeledstmt_6;
      labeledstmt_7;
      labeledstmt_8;
    }
    const entries = {
      (2) : labeledstmt_6();
      (3) : labeledstmt_6();
      (4) : labeledstmt_7();
      (1) : labeledstmt_8();
      (_) : labeledstmt_6();
    } 
  } 
  table table_1605 {
    key = {
      hdr.wire_ev.event_id : ternary;
    }
    actions = {
      labeledstmt_9;
      labeledstmt_10;
      labeledstmt_11;
    }
    const entries = {
      (2) : labeledstmt_9();
      (3) : labeledstmt_9();
      (4) : labeledstmt_10();
      (1) : labeledstmt_11();
      (_) : labeledstmt_9();
    } 
  } 
  table table_1604 {
    key = {
      hdr.wire_ev.event_id : ternary;
    }
    actions = {
      labeledstmt_12;
      labeledstmt_13;
    }
    const entries = {
      (2) : labeledstmt_12();
      (3) : labeledstmt_12();
      (4) : labeledstmt_12();
      (1) : labeledstmt_13();
      (_) : labeledstmt_12();
    } 
  } 
  table table_1603 {
    key = {
      hdr.wire_ev.event_id : ternary;
      if_precomp : ternary;
    }
    actions = {
      labeledstmt_14;
      labeledstmt_15;
      labeledstmt_16;
    }
    const entries = {
      (2,0) : labeledstmt_14();
      (3,0) : labeledstmt_14();
      (4,0) : labeledstmt_14();
      (1,0) : labeledstmt_15();
      (2,_) : labeledstmt_14();
      (3,_) : labeledstmt_14();
      (4,_) : labeledstmt_14();
      (1,_) : labeledstmt_16();
      (_,_) : labeledstmt_14();
    } 
  } 
  table table_1602 {
    key = {
      if_precomp : ternary;
      hdr.wire_ev.event_id : ternary;
    }
    actions = {
      labeledstmt_17;
      labeledstmt_18;
    }
    const entries = {
      (0,2) : labeledstmt_17();
      (0,3) : labeledstmt_17();
      (0,4) : labeledstmt_17();
      (0,1) : labeledstmt_17();
      (_,2) : labeledstmt_17();
      (_,3) : labeledstmt_17();
      (_,4) : labeledstmt_17();
      (_,1) : labeledstmt_18();
      (_,_) : labeledstmt_17();
    } 
  } 
  table table_1601 {
    key = {
      countermysre : ternary;
      hdr.wire_ev.event_id : ternary;
    }
    actions = {
      labeledstmt_19;
      labeledstmt_20;
      labeledstmt_21;
    }
    const entries = {
      (1,2) : labeledstmt_19();
      (1,3) : labeledstmt_19();
      (1,4) : labeledstmt_19();
      (1,1) : labeledstmt_20();
      (0,2) : labeledstmt_19();
      (0,3) : labeledstmt_19();
      (0,4) : labeledstmt_19();
      (0,1) : labeledstmt_21();
      (_,2) : labeledstmt_19();
      (_,3) : labeledstmt_19();
      (_,4) : labeledstmt_19();
      (_,1) : labeledstmt_19();
      (_,_) : labeledstmt_19();
    } 
  } 
  table table_1600 {
    key = {
      memop_synthesized_meta_varmysre : ternary;
      hdr.wire_ev.event_id : ternary;
    }
    actions = {
      labeledstmt_22;
      labeledstmt_23;
      labeledstmt_24;
    }
    const entries = {
      (0,2) : labeledstmt_22();
      (0,3) : labeledstmt_22();
      (0,4) : labeledstmt_22();
      (0,1) : labeledstmt_23();
      (2,2) : labeledstmt_22();
      (2,3) : labeledstmt_22();
      (2,4) : labeledstmt_22();
      (2,1) : labeledstmt_24();
      (_,2) : labeledstmt_22();
      (_,3) : labeledstmt_22();
      (_,4) : labeledstmt_22();
      (_,1) : labeledstmt_22();
      (_,_) : labeledstmt_22();
    } 
  } 
  table table_1599 {
    key = {
      hdr.wire_ev.event_id : ternary;
      memop_synthesized_meta_varmysre : ternary;
    }
    actions = {
      labeledstmt_25;
      labeledstmt_26;
      labeledstmt_27;
      labeledstmt_28;
    }
    const entries = {
      (2,0) : labeledstmt_25();
      (3,0) : labeledstmt_25();
      (4,0) : labeledstmt_26();
      (1,0) : labeledstmt_27();
      (2,2) : labeledstmt_25();
      (3,2) : labeledstmt_25();
      (4,2) : labeledstmt_26();
      (1,2) : labeledstmt_28();
      (2,_) : labeledstmt_25();
      (3,_) : labeledstmt_25();
      (4,_) : labeledstmt_26();
      (_,_) : labeledstmt_25();
    } 
  } 
  table table_1598 {
    key = {
      resmysre1279 : ternary;
      hdr.wire_ev.event_id : ternary;
    }
    actions = {
      labeledstmt_29;
      labeledstmt_30;
      labeledstmt_31;
    }
    const entries = {
      (181,2) : labeledstmt_29();
      (181,3) : labeledstmt_29();
      (181,4) : labeledstmt_29();
      (181,1) : labeledstmt_30();
      (_,2) : labeledstmt_29();
      (_,3) : labeledstmt_29();
      (_,4) : labeledstmt_29();
      (_,1) : labeledstmt_31();
      (_,_) : labeledstmt_29();
    } 
  } 
  table table_1597 {
    key = {
      ansmysre : ternary;
      hdr.wire_ev.event_id : ternary;
    }
    actions = {
      labeledstmt_32;
      labeledstmt_33;
    }
    const entries = {
      (1,2) : labeledstmt_32();
      (1,3) : labeledstmt_32();
      (1,4) : labeledstmt_32();
      (1,1) : labeledstmt_33();
      (_,2) : labeledstmt_32();
      (_,3) : labeledstmt_32();
      (_,4) : labeledstmt_32();
      (_,1) : labeledstmt_32();
      (_,_) : labeledstmt_32();
    } 
  } 
  table table_1596 {
    key = {
      hdr.wire_ev.event_id : ternary;
    }
    actions = {
      labeledstmt_34;
      labeledstmt_35;
    }
    const entries = {
      (2) : labeledstmt_34();
      (3) : labeledstmt_34();
      (4) : labeledstmt_34();
      (1) : labeledstmt_35();
      (_) : labeledstmt_34();
    } 
  } 
  apply {
    table_1607.apply();
    table_1606.apply();
    table_1605.apply();
    table_1604.apply();
    table_1603.apply();
    table_1602.apply();
    table_1601.apply();
    table_1600.apply();
    table_1599.apply();
    table_1598.apply();
    table_1597.apply();
    table_1596.apply();
  }
} 
control IngressDeparser(packet_out pkt,
    inout hdr_t hdr,
    in meta_t meta,
    in ingress_intrinsic_metadata_for_deparser_t ig_dprsr_md){

  apply {
    pkt.emit(hdr);
  }
} 
parser EgressParser(packet_in pkt,
    out hdr_t hdr,
    out meta_t meta,
    out egress_intrinsic_metadata_t eg_intr_md){
  state start {
    pkt.extract(eg_intr_md);
    hdr.lucid_eth.setValid();
    hdr.lucid_eth.dst_addr=0;
    hdr.lucid_eth.src_addr=0;
    hdr.lucid_eth.etype=1638;
    pkt.extract(hdr.wire_ev);
    pkt.extract(hdr.bridge_ev);
    meta.egress_event_id=0;
    transition select(hdr.bridge_ev.mysrereset,
hdr.bridge_ev.ttl_changes_found, hdr.bridge_ev.DNS_packet_out,
hdr.bridge_ev.DNS_packet_fwd){
      (0, 1, 1, 0) : parse_eventset_0;
    }
  }
  state parse_eventset_0 {
    pkt.extract(hdr.ttl_changes_found);
    pkt.extract(hdr.DNS_packet_out);
    transition accept;
  }
}
control EgressControl(inout hdr_t hdr,
    inout meta_t meta,
    in egress_intrinsic_metadata_t eg_intr_md,
    in egress_intrinsic_metadata_from_parser_t eg_prsr_md,
    inout egress_intrinsic_metadata_for_deparser_t eg_dprsr_md,
    inout egress_intrinsic_metadata_for_output_port_t eg_oport_md){
  @pa_no_overlay("egress","hdr.mysrereset.mysrereset_mysreidx")
  @pa_no_overlay("egress","hdr.ttl_changes_found.ttl_changes_found_idx")
  @pa_no_overlay("egress","hdr.ttl_changes_found.ttl_changes_found_count")
  @pa_no_overlay("egress","hdr.DNS_packet_out.DNS_packet_out_sip")
  @pa_no_overlay("egress","hdr.DNS_packet_out.DNS_packet_out_cip")
  @pa_no_overlay("egress","hdr.DNS_packet_out.DNS_packet_out_smac")
  @pa_no_overlay("egress","hdr.DNS_packet_out.DNS_packet_out_cmac")
  @pa_no_overlay("egress","hdr.DNS_packet_out.DNS_packet_out_ttl")
  @pa_no_overlay("egress","hdr.DNS_packet_fwd.DNS_packet_fwd_sip")
  @pa_no_overlay("egress","hdr.DNS_packet_fwd.DNS_packet_fwd_cip")
  @pa_no_overlay("egress","hdr.DNS_packet_fwd.DNS_packet_fwd_smac")
  @pa_no_overlay("egress","hdr.DNS_packet_fwd.DNS_packet_fwd_cmac")
  @pa_no_overlay("egress","hdr.DNS_packet_fwd.DNS_packet_fwd_ttl")
  action egr_noop(){
    //NOOP
  }
  action mysrereset_recirc(){
    hdr.ttl_changes_found.setInvalid();
    hdr.DNS_packet_out.setInvalid();
    hdr.DNS_packet_fwd.setInvalid();
    hdr.wire_ev.event_id=4;
    meta.egress_event_id=4;
    hdr.bridge_ev.port_event_id=0;
    hdr.bridge_ev.mysrereset=0;
    hdr.bridge_ev.ttl_changes_found=0;
    hdr.bridge_ev.DNS_packet_out=0;
    hdr.bridge_ev.DNS_packet_fwd=0;
  }
  action ttl_changes_found_recirc(){
    hdr.mysrereset.setInvalid();
    hdr.DNS_packet_out.setInvalid();
    hdr.DNS_packet_fwd.setInvalid();
    hdr.wire_ev.event_id=3;
    meta.egress_event_id=3;
    hdr.bridge_ev.port_event_id=0;
    hdr.bridge_ev.mysrereset=0;
    hdr.bridge_ev.ttl_changes_found=0;
    hdr.bridge_ev.DNS_packet_out=0;
    hdr.bridge_ev.DNS_packet_fwd=0;
  }
  action DNS_packet_out_recirc(){
    hdr.mysrereset.setInvalid();
    hdr.ttl_changes_found.setInvalid();
    hdr.DNS_packet_fwd.setInvalid();
    hdr.wire_ev.event_id=2;
    meta.egress_event_id=2;
    hdr.bridge_ev.port_event_id=0;
    hdr.bridge_ev.mysrereset=0;
    hdr.bridge_ev.ttl_changes_found=0;
    hdr.bridge_ev.DNS_packet_out=0;
    hdr.bridge_ev.DNS_packet_fwd=0;
  }
  action DNS_packet_fwd_recirc(){
    hdr.mysrereset.setInvalid();
    hdr.ttl_changes_found.setInvalid();
    hdr.DNS_packet_out.setInvalid();
    hdr.wire_ev.event_id=1;
    meta.egress_event_id=1;
    hdr.bridge_ev.port_event_id=0;
    hdr.bridge_ev.mysrereset=0;
    hdr.bridge_ev.ttl_changes_found=0;
    hdr.bridge_ev.DNS_packet_out=0;
    hdr.bridge_ev.DNS_packet_fwd=0;
  }
  action mysrereset_to_external(){
    meta.egress_event_id=4;
    hdr.lucid_eth.setInvalid();
    hdr.wire_ev.setInvalid();
    hdr.bridge_ev.setInvalid();
    hdr.ttl_changes_found.setInvalid();
    hdr.DNS_packet_out.setInvalid();
    hdr.DNS_packet_fwd.setInvalid();
  }
  action ttl_changes_found_to_external(){
    meta.egress_event_id=3;
    hdr.lucid_eth.setInvalid();
    hdr.wire_ev.setInvalid();
    hdr.bridge_ev.setInvalid();
    hdr.mysrereset.setInvalid();
    hdr.DNS_packet_out.setInvalid();
    hdr.DNS_packet_fwd.setInvalid();
  }
  action DNS_packet_out_to_external(){
    meta.egress_event_id=2;
    hdr.lucid_eth.setInvalid();
    hdr.wire_ev.setInvalid();
    hdr.bridge_ev.setInvalid();
    hdr.mysrereset.setInvalid();
    hdr.ttl_changes_found.setInvalid();
    hdr.DNS_packet_fwd.setInvalid();
  }
  action DNS_packet_fwd_to_external(){
    meta.egress_event_id=1;
    hdr.lucid_eth.setInvalid();
    hdr.wire_ev.setInvalid();
    hdr.bridge_ev.setInvalid();
    hdr.mysrereset.setInvalid();
    hdr.ttl_changes_found.setInvalid();
    hdr.DNS_packet_out.setInvalid();
  }
  action mysrereset_to_internal(){
    meta.egress_event_id=4;
    hdr.wire_ev.event_id=4;
    hdr.bridge_ev.port_event_id=0;
    hdr.bridge_ev.mysrereset=0;
    hdr.bridge_ev.ttl_changes_found=0;
    hdr.bridge_ev.DNS_packet_out=0;
    hdr.bridge_ev.DNS_packet_fwd=0;
    hdr.ttl_changes_found.setInvalid();
    hdr.DNS_packet_out.setInvalid();
    hdr.DNS_packet_fwd.setInvalid();
  }
  action ttl_changes_found_to_internal(){
    meta.egress_event_id=3;
    hdr.wire_ev.event_id=3;
    hdr.bridge_ev.port_event_id=0;
    hdr.bridge_ev.mysrereset=0;
    hdr.bridge_ev.ttl_changes_found=0;
    hdr.bridge_ev.DNS_packet_out=0;
    hdr.bridge_ev.DNS_packet_fwd=0;
    hdr.mysrereset.setInvalid();
    hdr.DNS_packet_out.setInvalid();
    hdr.DNS_packet_fwd.setInvalid();
  }
  action DNS_packet_out_to_internal(){
    meta.egress_event_id=2;
    hdr.wire_ev.event_id=2;
    hdr.bridge_ev.port_event_id=0;
    hdr.bridge_ev.mysrereset=0;
    hdr.bridge_ev.ttl_changes_found=0;
    hdr.bridge_ev.DNS_packet_out=0;
    hdr.bridge_ev.DNS_packet_fwd=0;
    hdr.mysrereset.setInvalid();
    hdr.ttl_changes_found.setInvalid();
    hdr.DNS_packet_fwd.setInvalid();
  }
  action DNS_packet_fwd_to_internal(){
    meta.egress_event_id=1;
    hdr.wire_ev.event_id=1;
    hdr.bridge_ev.port_event_id=0;
    hdr.bridge_ev.mysrereset=0;
    hdr.bridge_ev.ttl_changes_found=0;
    hdr.bridge_ev.DNS_packet_out=0;
    hdr.bridge_ev.DNS_packet_fwd=0;
    hdr.mysrereset.setInvalid();
    hdr.ttl_changes_found.setInvalid();
    hdr.DNS_packet_out.setInvalid();
  }
  table t_extract_recirc_event {
    key = {
      eg_intr_md.egress_rid : ternary;
      hdr.bridge_ev.port_event_id : ternary;
      hdr.bridge_ev.mysrereset : ternary;
      hdr.bridge_ev.ttl_changes_found : ternary;
      hdr.bridge_ev.DNS_packet_out : ternary;
      hdr.bridge_ev.DNS_packet_fwd : ternary;
    }
    actions = {
      egr_noop;
      mysrereset_recirc;
      ttl_changes_found_recirc;
      DNS_packet_out_recirc;
      DNS_packet_fwd_recirc;
    }
    const entries = {
      (1,0,0,1,1,0) : ttl_changes_found_recirc();
      (1,2,0,1,1,0) : ttl_changes_found_recirc();
      (1,3,0,1,1,0) : DNS_packet_out_recirc();
      (2,0,0,1,1,0) : DNS_packet_out_recirc();
    } 
  } 
  table t_extract_port_event {
    key = {
      hdr.bridge_ev.port_event_id : ternary;
      eg_intr_md.egress_port : ternary;
    }
    actions = {
      mysrereset_to_external;
      mysrereset_to_internal;
      ttl_changes_found_to_external;
      ttl_changes_found_to_internal;
      DNS_packet_out_to_external;
      DNS_packet_out_to_internal;
      DNS_packet_fwd_to_external;
      DNS_packet_fwd_to_internal;
    }
    const entries = {
      (4,196) : mysrereset_to_internal();
      (4,_) : mysrereset_to_external();
      (3,196) : ttl_changes_found_to_internal();
      (3,_) : ttl_changes_found_to_external();
      (2,196) : DNS_packet_out_to_internal();
      (2,_) : DNS_packet_out_to_external();
      (1,196) : DNS_packet_fwd_to_internal();
      (1,_) : DNS_packet_fwd_to_external();
    } 
  } 
  apply {
    if ((eg_intr_md.egress_rid==0)){
      t_extract_port_event.apply();
    } else {
      t_extract_recirc_event.apply();
    }
  }
} 
control EgressDeparse(packet_out pkt,
    inout hdr_t hdr,
    in meta_t meta,
    in egress_intrinsic_metadata_for_deparser_t eg_dprsr_md){

  apply {
    pkt.emit(hdr);
  }
} 
//Pipeline and main declarations
Pipeline(IngressParser(),
  IngressControl(),
  IngressDeparser(),
  EgressParser(),
  EgressControl(),
  EgressDeparse()) pipe;
Switch(pipe) main;
