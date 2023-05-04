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
  bit<6> flag_pad_1512;
  bit<1> ATP_release;
  bit<1> ATP_add;
}
header ATP_release_t {
  bit<32> ATP_release_idx;
  bit<32> ATP_release_jobid;
}
header ATP_add_t {
  bit<32> ATP_add_idx;
  bit<32> ATP_add_jobid;
  bit<32> ATP_add_clientid;
  bit<32> ATP_add_val;
}
struct hdr_t {
  lucid_eth_t lucid_eth;
  wire_ev_t wire_ev;
  bridge_ev_t bridge_ev;
  ATP_release_t ATP_release;
  ATP_add_t ATP_add;
}
struct meta_t {
  bit<8> egress_event_id;
}
Register<bit<32>,_>(32w1024)
assigned_var_msresaved_jobid;
Register<bit<8>,_>(32w1024) msre;
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
    hdr.bridge_ev.setValid();
    hdr.bridge_ev.port_event_id=0;
    hdr.bridge_ev.ATP_release=0;
    hdr.bridge_ev.ATP_add=0;
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
      (2) : parse_ATP_release;
      (1) : parse_ATP_add;
    }
  }
  state parse_ATP_release {
    pkt.extract(hdr.ATP_release);
    transition accept;
  }
  state parse_ATP_add {
    pkt.extract(hdr.ATP_add);
    transition accept;
  }
  state parse_all_events {
    pkt.extract(hdr.ATP_release);
    pkt.extract(hdr.ATP_add);
    transition accept;
  }
}
control IngressControl(inout hdr_t hdr,
    inout meta_t meta,
    in ingress_intrinsic_metadata_t ig_intr_md,
    in ingress_intrinsic_metadata_from_parser_t ig_prsr_md,
    inout ingress_intrinsic_metadata_for_deparser_t ig_dprsr_md,
    inout ingress_intrinsic_metadata_for_tm_t ig_tm_md){
  bit<8>
merged_var_g_synthesized_meta_varmsre1206_g_synthesized_meta_varmsre1206_g_synthesized_meta_varmsre_g_synthesized_meta_varmsre_1498;
  bit<8>
merged_var_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre_f_synthesized_meta_varmsre_1497;
  bit<32> merged_var_msreidx1204_msreidx_1496;
  bit<8> resmsre;
  action labeledstmt_30(){
    resmsre=8w0;
  }
  bit<8> memop_synthesized_meta_varmsre;
  action labeledstmt_29(){
    memop_synthesized_meta_varmsre=8w0;
  }
  action labeledstmt_28(){
   
merged_var_g_synthesized_meta_varmsre1206_g_synthesized_meta_varmsre1206_g_synthesized_meta_varmsre_g_synthesized_meta_varmsre_1498=8w0;
  }
  action labeledstmt_27(){
   
merged_var_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre_f_synthesized_meta_varmsre_1497=8w0;
  }
  action labeledstmt_26(){
    merged_var_msreidx1204_msreidx_1496=hdr.ATP_add.ATP_add_idx;
  }
  bit<1> ansmsre;
  action labeledstmt_25(){
    ansmsre=1w1;
  }
  action labeledstmt_1(){
    labeledstmt_25();
    labeledstmt_26();
    labeledstmt_27();
    labeledstmt_28();
    labeledstmt_29();
    labeledstmt_30();
  }
  bit<8> resmsre1210;
  action labeledstmt_34(){
    resmsre1210=8w0;
  }
  bit<8> memop_synthesized_meta_varmsre1207;
  action labeledstmt_33(){
    memop_synthesized_meta_varmsre1207=8w0;
  }
  action labeledstmt_32(){
    merged_var_msreidx1204_msreidx_1496=hdr.ATP_release.ATP_release_idx;
  }
  bit<1> ansmsre1203;
  action labeledstmt_31(){
    ansmsre1203=1w1;
  }
  action labeledstmt_2(){
    labeledstmt_31();
    labeledstmt_32();
    labeledstmt_27();
    labeledstmt_28();
    labeledstmt_33();
    labeledstmt_34();
  }
  action labeledstmt_3(){
    //NOOP
  }
  bit<32> saved_jobid;
  RegisterAction<bit<32>,bit<32>,bit<32>>(assigned_var_msresaved_jobid)
  assigned_var_msresaved_jobid_regaction_1499 = {
    void apply(inout bit<32> cell1_remote,
        out bit<32> ret_remote){
      bit<32> cell1_local=cell1_remote;
      bit<32> cell2_local=0;
      if((cell1_local==32w0)){
        cell2_local=hdr.ATP_add.ATP_add_jobid;
      }
      if((!(cell1_local==32w0))){
        cell2_local=cell1_local;
      }
      if(true){
        ret_remote=cell2_local;
      }
    }
  };
  action labeledstmt_38(){
   
saved_jobid=assigned_var_msresaved_jobid_regaction_1499.execute(merged_var_msreidx1204_msreidx_1496);
  }
  action labeledstmt_37(){
    memop_synthesized_meta_varmsre=8w2;
  }
  action labeledstmt_36(){
   
merged_var_g_synthesized_meta_varmsre1206_g_synthesized_meta_varmsre1206_g_synthesized_meta_varmsre_g_synthesized_meta_varmsre_1498=8w251;
  }
  action labeledstmt_35(){
   
merged_var_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre_f_synthesized_meta_varmsre_1497=8w241;
  }
  action labeledstmt_4(){
    labeledstmt_35();
    labeledstmt_36();
    labeledstmt_37();
    labeledstmt_38();
  }
  bit<32> saved_jobid1208;
  RegisterAction<bit<32>,bit<32>,bit<32>>(assigned_var_msresaved_jobid)
  assigned_var_msresaved_jobid_regaction_1500 = {
    void apply(inout bit<32> cell1_remote,
        out bit<32> ret_remote){
      bit<32> cell1_local=cell1_remote;
      bit<32> cell2_local=0;
      if(true){
        ret_remote=cell1_local;
      }
    }
  };
  action labeledstmt_39(){
   
saved_jobid1208=assigned_var_msresaved_jobid_regaction_1500.execute(merged_var_msreidx1204_msreidx_1496);
  }
  action labeledstmt_5(){
    labeledstmt_39();
  }
  action labeledstmt_6(){
    //NOOP
  }
  action labeledstmt_40(){
    hdr.ATP_add.setInvalid();
  }
  action labeledstmt_7(){
    labeledstmt_40();
  }
  bit<32> pred0msre;
  action labeledstmt_41(){
    pred0msre=(hdr.ATP_release.ATP_release_jobid-saved_jobid1208);
  }
  action labeledstmt_8(){
    labeledstmt_41();
  }
  action labeledstmt_9(){
    //NOOP
  }
  action labeledstmt_10(){
    //NOOP
  }
  action labeledstmt_45(){
   
merged_var_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre_f_synthesized_meta_varmsre_1497=8w246;
  }
  action labeledstmt_44(){
   
merged_var_g_synthesized_meta_varmsre1206_g_synthesized_meta_varmsre1206_g_synthesized_meta_varmsre_g_synthesized_meta_varmsre_1498=8w23;
  }
  action labeledstmt_43(){
    memop_synthesized_meta_varmsre1207=8w1;
  }
  action labeledstmt_42(){
    hdr.ATP_release.setInvalid();
  }
  action labeledstmt_11(){
    labeledstmt_42();
    labeledstmt_43();
    labeledstmt_44();
    labeledstmt_45();
  }
  action labeledstmt_47(){
   
merged_var_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre_f_synthesized_meta_varmsre_1497=8w29;
  }
  action labeledstmt_46(){
   
merged_var_g_synthesized_meta_varmsre1206_g_synthesized_meta_varmsre1206_g_synthesized_meta_varmsre_g_synthesized_meta_varmsre_1498=8w129;
  }
  action labeledstmt_12(){
    labeledstmt_42();
    labeledstmt_43();
    labeledstmt_46();
    labeledstmt_47();
  }
  RegisterAction<bit<8>,bit<32>,bit<8>>(msre)
  msre_regaction_1501 = {
    void apply(inout bit<8> cell1_remote,
        out bit<8> ret_remote){
      bit<8> cell1_local=cell1_remote;
      bit<8> cell2_local=0;
     
if((((cell1_local+merged_var_g_synthesized_meta_varmsre1206_g_synthesized_meta_varmsre1206_g_synthesized_meta_varmsre_g_synthesized_meta_varmsre_1498)<8w8)||((cell1_local+merged_var_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre_f_synthesized_meta_varmsre_1497)<8w7))){
        cell1_remote=(8w2|cell1_local);
      }
     
if(((!((cell1_local+merged_var_g_synthesized_meta_varmsre1206_g_synthesized_meta_varmsre1206_g_synthesized_meta_varmsre_g_synthesized_meta_varmsre_1498)<8w8))&&(!((cell1_local+merged_var_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre_f_synthesized_meta_varmsre_1497)<8w7)))){
        cell1_remote=(8w2|8w1);
      }
      if(true){
        ret_remote=cell1_remote;
      }
    }
  };
  action labeledstmt_48(){
    resmsre=msre_regaction_1501.execute(merged_var_msreidx1204_msreidx_1496);
  }
  action labeledstmt_13(){
    labeledstmt_48();
  }
  RegisterAction<bit<8>,bit<32>,bit<8>>(msre)
  msre_regaction_1502 = {
    void apply(inout bit<8> cell1_remote,
        out bit<8> ret_remote){
      bit<8> cell1_local=cell1_remote;
      bit<8> cell2_local=0;
     
if((((cell1_local+merged_var_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre_f_synthesized_meta_varmsre_1497)>8w2)&&((cell1_local+8w0)!=8w3))){
       
cell1_remote=(merged_var_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre_f_synthesized_meta_varmsre_1497+cell1_local);
      }
     
if((!(((cell1_local+merged_var_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre_f_synthesized_meta_varmsre_1497)>8w2)&&((cell1_local+8w0)!=8w3)))){
       
cell1_remote=(merged_var_g_synthesized_meta_varmsre1206_g_synthesized_meta_varmsre1206_g_synthesized_meta_varmsre_g_synthesized_meta_varmsre_1498+8w8);
      }
      if(true){
        ret_remote=cell1_remote;
      }
    }
  };
  action labeledstmt_49(){
    resmsre=msre_regaction_1502.execute(merged_var_msreidx1204_msreidx_1496);
  }
  action labeledstmt_14(){
    labeledstmt_49();
  }
  action labeledstmt_15(){
    //NOOP
  }
  RegisterAction<bit<8>,bit<32>,bit<8>>(msre)
  msre_regaction_1503 = {
    void apply(inout bit<8> cell1_remote,
        out bit<8> ret_remote){
      bit<8> cell1_local=cell1_remote;
      bit<8> cell2_local=0;
     
if((((cell1_local+merged_var_g_synthesized_meta_varmsre1206_g_synthesized_meta_varmsre1206_g_synthesized_meta_varmsre_g_synthesized_meta_varmsre_1498)<8w8)||((cell1_local+merged_var_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre_f_synthesized_meta_varmsre_1497)<8w7))){
        cell1_remote=(8w2|cell1_local);
      }
     
if(((!((cell1_local+merged_var_g_synthesized_meta_varmsre1206_g_synthesized_meta_varmsre1206_g_synthesized_meta_varmsre_g_synthesized_meta_varmsre_1498)<8w8))&&(!((cell1_local+merged_var_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre_f_synthesized_meta_varmsre_1497)<8w7)))){
        cell1_remote=(8w2|8w1);
      }
      if(true){
        ret_remote=cell1_remote;
      }
    }
  };
  action labeledstmt_50(){
   
resmsre1210=msre_regaction_1503.execute(merged_var_msreidx1204_msreidx_1496);
  }
  action labeledstmt_16(){
    labeledstmt_50();
  }
  RegisterAction<bit<8>,bit<32>,bit<8>>(msre)
  msre_regaction_1504 = {
    void apply(inout bit<8> cell1_remote,
        out bit<8> ret_remote){
      bit<8> cell1_local=cell1_remote;
      bit<8> cell2_local=0;
     
if((((cell1_local+merged_var_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre_f_synthesized_meta_varmsre_1497)>8w2)&&((cell1_local+8w0)!=8w3))){
       
cell1_remote=(merged_var_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre_f_synthesized_meta_varmsre_1497+cell1_local);
      }
     
if((!(((cell1_local+merged_var_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre1205_f_synthesized_meta_varmsre_f_synthesized_meta_varmsre_1497)>8w2)&&((cell1_local+8w0)!=8w3)))){
       
cell1_remote=(merged_var_g_synthesized_meta_varmsre1206_g_synthesized_meta_varmsre1206_g_synthesized_meta_varmsre_g_synthesized_meta_varmsre_1498+8w8);
      }
      if(true){
        ret_remote=cell1_remote;
      }
    }
  };
  action labeledstmt_51(){
   
resmsre1210=msre_regaction_1504.execute(merged_var_msreidx1204_msreidx_1496);
  }
  action labeledstmt_17(){
    labeledstmt_51();
  }
  action labeledstmt_18(){
    //NOOP
  }
  action labeledstmt_52(){
    ansmsre=1w1;
  }
  action labeledstmt_19(){
    labeledstmt_52();
  }
  action labeledstmt_53(){
    ansmsre=1w0;
  }
  action labeledstmt_20(){
    labeledstmt_53();
  }
  action labeledstmt_54(){
    ansmsre1203=1w1;
  }
  action labeledstmt_21(){
    labeledstmt_54();
  }
  action labeledstmt_55(){
    ansmsre1203=1w0;
  }
  action labeledstmt_22(){
    labeledstmt_55();
  }
  action labeledstmt_23(){
    //NOOP
  }
  action labeledstmt_24(){
    //NOOP
  }
  table table_1511 {
    key = {
      hdr.wire_ev.event_id : ternary;
    }
    actions = {
      labeledstmt_1;
      labeledstmt_2;
      labeledstmt_3;
    }
    const entries = {
      (1) : labeledstmt_1();
      (2) : labeledstmt_2();
      (_) : labeledstmt_3();
    } 
  } 
  table table_1510 {
    key = {
      hdr.wire_ev.event_id : ternary;
    }
    actions = {
      labeledstmt_4;
      labeledstmt_5;
      labeledstmt_6;
    }
    const entries = {
      (1) : labeledstmt_4();
      (2) : labeledstmt_5();
      (_) : labeledstmt_6();
    } 
  } 
  table table_1509 {
    key = {
      memop_synthesized_meta_varmsre : ternary;
      hdr.wire_ev.event_id : ternary;
    }
    actions = {
      labeledstmt_7;
      labeledstmt_8;
      labeledstmt_9;
    }
    const entries = {
      (1,1) : labeledstmt_7();
      (2,1) : labeledstmt_7();
      (_,1) : labeledstmt_7();
      (_,2) : labeledstmt_8();
      (_,_) : labeledstmt_9();
    } 
  } 
  table table_1508 {
    key = {
      hdr.wire_ev.event_id : ternary;
      pred0msre : ternary;
    }
    actions = {
      labeledstmt_10;
      labeledstmt_11;
      labeledstmt_12;
    }
    const entries = {
      (1,0) : labeledstmt_10();
      (2,0) : labeledstmt_11();
      (1,_) : labeledstmt_10();
      (2,_) : labeledstmt_12();
      (_,_) : labeledstmt_10();
    } 
  } 
  table table_1507 {
    key = {
      memop_synthesized_meta_varmsre1207 : ternary;
      hdr.wire_ev.event_id : ternary;
      memop_synthesized_meta_varmsre : ternary;
    }
    actions = {
      labeledstmt_13;
      labeledstmt_14;
      labeledstmt_15;
      labeledstmt_16;
      labeledstmt_17;
      labeledstmt_18;
    }
    const entries = {
      (1,1,1) : labeledstmt_13();
      (1,1,2) : labeledstmt_14();
      (1,1,_) : labeledstmt_15();
      (1,2,_) : labeledstmt_16();
      (2,1,1) : labeledstmt_13();
      (2,1,2) : labeledstmt_14();
      (2,1,_) : labeledstmt_15();
      (2,2,_) : labeledstmt_17();
      (_,1,1) : labeledstmt_13();
      (_,1,2) : labeledstmt_14();
      (_,1,_) : labeledstmt_15();
      (_,2,_) : labeledstmt_18();
      (_,_,_) : labeledstmt_15();
    } 
  } 
  table table_1506 {
    key = {
      resmsre1210 : ternary;
      hdr.wire_ev.event_id : ternary;
      resmsre : ternary;
    }
    actions = {
      labeledstmt_19;
      labeledstmt_20;
      labeledstmt_21;
      labeledstmt_22;
      labeledstmt_23;
    }
    const entries = {
      (18,1,18) : labeledstmt_19();
      (_,1,18) : labeledstmt_19();
      (18,1,_) : labeledstmt_20();
      (_,1,_) : labeledstmt_20();
      (18,2,_) : labeledstmt_21();
      (_,2,_) : labeledstmt_22();
      (_,_,_) : labeledstmt_23();
    } 
  } 
  table table_1505 {
    key = {
      ansmsre1203 : ternary;
      hdr.wire_ev.event_id : ternary;
      ansmsre : ternary;
    }
    actions = {
      labeledstmt_24;
    }
    const entries = {
      (1,1,1) : labeledstmt_24();
      (_,1,1) : labeledstmt_24();
      (1,1,_) : labeledstmt_24();
      (_,1,_) : labeledstmt_24();
      (1,2,_) : labeledstmt_24();
      (_,2,_) : labeledstmt_24();
      (_,_,_) : labeledstmt_24();
    } 
  } 
  apply {
    table_1511.apply();
    table_1510.apply();
    table_1509.apply();
    table_1508.apply();
    table_1507.apply();
    table_1506.apply();
    table_1505.apply();
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
    transition select(hdr.bridge_ev.ATP_release, hdr.bridge_ev.ATP_add){

    }
  }
}
control EgressControl(inout hdr_t hdr,
    inout meta_t meta,
    in egress_intrinsic_metadata_t eg_intr_md,
    in egress_intrinsic_metadata_from_parser_t eg_prsr_md,
    inout egress_intrinsic_metadata_for_deparser_t eg_dprsr_md,
    inout egress_intrinsic_metadata_for_output_port_t eg_oport_md){
  @pa_no_overlay("egress","hdr.ATP_release.ATP_release_idx")
  @pa_no_overlay("egress","hdr.ATP_release.ATP_release_jobid")
  @pa_no_overlay("egress","hdr.ATP_add.ATP_add_idx")
  @pa_no_overlay("egress","hdr.ATP_add.ATP_add_jobid")
  @pa_no_overlay("egress","hdr.ATP_add.ATP_add_clientid")
  @pa_no_overlay("egress","hdr.ATP_add.ATP_add_val")
  action egr_noop(){
    //NOOP
  }
  action ATP_release_recirc(){
    hdr.ATP_add.setInvalid();
    hdr.wire_ev.event_id=2;
    meta.egress_event_id=2;
    hdr.bridge_ev.port_event_id=0;
    hdr.bridge_ev.ATP_release=0;
    hdr.bridge_ev.ATP_add=0;
  }
  action ATP_add_recirc(){
    hdr.ATP_release.setInvalid();
    hdr.wire_ev.event_id=1;
    meta.egress_event_id=1;
    hdr.bridge_ev.port_event_id=0;
    hdr.bridge_ev.ATP_release=0;
    hdr.bridge_ev.ATP_add=0;
  }
  action ATP_release_to_external(){
    meta.egress_event_id=2;
    hdr.lucid_eth.setInvalid();
    hdr.wire_ev.setInvalid();
    hdr.bridge_ev.setInvalid();
    hdr.ATP_add.setInvalid();
  }
  action ATP_add_to_external(){
    meta.egress_event_id=1;
    hdr.lucid_eth.setInvalid();
    hdr.wire_ev.setInvalid();
    hdr.bridge_ev.setInvalid();
    hdr.ATP_release.setInvalid();
  }
  action ATP_release_to_internal(){
    meta.egress_event_id=2;
    hdr.wire_ev.event_id=2;
    hdr.bridge_ev.port_event_id=0;
    hdr.bridge_ev.ATP_release=0;
    hdr.bridge_ev.ATP_add=0;
    hdr.ATP_add.setInvalid();
  }
  action ATP_add_to_internal(){
    meta.egress_event_id=1;
    hdr.wire_ev.event_id=1;
    hdr.bridge_ev.port_event_id=0;
    hdr.bridge_ev.ATP_release=0;
    hdr.bridge_ev.ATP_add=0;
    hdr.ATP_release.setInvalid();
  }
  table t_extract_recirc_event {
    key = {
      eg_intr_md.egress_rid : ternary;
      hdr.bridge_ev.port_event_id : ternary;
      hdr.bridge_ev.ATP_release : ternary;
      hdr.bridge_ev.ATP_add : ternary;
    }
    actions = {
      egr_noop;
      ATP_release_recirc;
      ATP_add_recirc;
    }
     
  } 
  table t_extract_port_event {
    key = {
      hdr.bridge_ev.port_event_id : ternary;
      eg_intr_md.egress_port : ternary;
    }
    actions = {
      ATP_release_to_external;
      ATP_release_to_internal;
      ATP_add_to_external;
      ATP_add_to_internal;
    }
    const entries = {
      (2,196) : ATP_release_to_internal();
      (2,_) : ATP_release_to_external();
      (1,196) : ATP_add_to_internal();
      (1,_) : ATP_add_to_external();
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
