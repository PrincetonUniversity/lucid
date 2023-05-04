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
  bit<7> flag_pad_1470;
  bit<1> ip_pkt;
}
header ip_pkt_t {
  bit<48> ip_pkt_eth_0;
  bit<48> ip_pkt_eth_1;
  bit<16> ip_pkt_eth_2;
  bit<8> ip_pkt_ip_0;
  bit<8> ip_pkt_ip_1;
  bit<16> ip_pkt_ip_2;
  bit<16> ip_pkt_ip_3;
  bit<16> ip_pkt_ip_4;
  bit<8> ip_pkt_ip_5;
  bit<8> ip_pkt_ip_6;
  bit<16> ip_pkt_ip_7;
  bit<32> ip_pkt_src;
  bit<32> ip_pkt_dst;
}
struct hdr_t {
  lucid_eth_t lucid_eth;
  wire_ev_t wire_ev;
  bridge_ev_t bridge_ev;
  ip_pkt_t ip_pkt;
}
struct meta_t {
  bit<8> egress_event_id;
}
Register<bit<32>,_>(32w4096)
assigned_var_allowedstart_time;
Register<bit<8>,_>(32w4096) allowed;
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
    hdr.bridge_ev.ip_pkt=0;
    transition parse_ip_pkt;
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
      (1) : parse_ip_pkt;
    }
  }
  state parse_ip_pkt {
    pkt.extract(hdr.ip_pkt);
    transition accept;
  }
  state parse_all_events {
    pkt.extract(hdr.ip_pkt);
    transition accept;
  }
}
control IngressControl(inout hdr_t hdr,
    inout meta_t meta,
    in ingress_intrinsic_metadata_t ig_intr_md,
    in ingress_intrinsic_metadata_from_parser_t ig_prsr_md,
    inout ingress_intrinsic_metadata_for_deparser_t ig_dprsr_md,
    inout ingress_intrinsic_metadata_for_tm_t ig_tm_md){
  bit<48> pre_gen_copy_ip_pkt_eth_0;
  bit<48> pre_gen_copy_ip_pkt_eth_1;
  bit<16> pre_gen_copy_ip_pkt_eth_2;
  bit<8> pre_gen_copy_ip_pkt_ip_0;
  bit<8> pre_gen_copy_ip_pkt_ip_1;
  bit<16> pre_gen_copy_ip_pkt_ip_2;
  bit<16> pre_gen_copy_ip_pkt_ip_3;
  bit<16> pre_gen_copy_ip_pkt_ip_4;
  bit<8> pre_gen_copy_ip_pkt_ip_5;
  bit<8> pre_gen_copy_ip_pkt_ip_6;
  bit<16> pre_gen_copy_ip_pkt_ip_7;
  bit<32> pre_gen_copy_ip_pkt_src;
  bit<32> pre_gen_copy_ip_pkt_dst;
  bit<8> resallowed;
  action labeledstmt_49(){
    resallowed=8w0;
  }
  bit<8> pred1allowed;
  action labeledstmt_48(){
    pred1allowed=(hdr.ip_pkt.ip_pkt_ip_1-8w1);
  }
  bit<32> to_immediate_tmp1452;
  action labeledstmt_47(){
    to_immediate_tmp1452=(ig_intr_md.ingress_mac_tstamp[47:16]);
  }
  bit<32> precompute;
  action labeledstmt_46(){
    precompute=(ig_intr_md.ingress_mac_tstamp[47:16]);
  }
  bit<8> memop_synthesized_meta_varallowed;
  action labeledstmt_45(){
    memop_synthesized_meta_varallowed=8w0;
  }
  bit<8> g_synthesized_meta_varallowed;
  action labeledstmt_44(){
    g_synthesized_meta_varallowed=8w0;
  }
  bit<8> f_synthesized_meta_varallowed;
  action labeledstmt_43(){
    f_synthesized_meta_varallowed=8w0;
  }
  bit<1> ansallowed;
  action labeledstmt_42(){
    ansallowed=1w1;
  }
  bit<12> to_immediate_tmp;
  CRCPolynomial<bit<12>>(19,false, false, false, 0, 0) hash_14560_crc;
  Hash<bit<12>>(HashAlgorithm_t.CUSTOM,hash_14560_crc) hash_14560;
  action labeledstmt_41(){
    to_immediate_tmp=hash_14560.get({hdr.ip_pkt.ip_pkt_src,
hdr.ip_pkt.ip_pkt_dst});
  }
  bit<32> ip_pkt_input_arg_12;
  action labeledstmt_40(){
    ip_pkt_input_arg_12=hdr.ip_pkt.ip_pkt_dst;
  }
  bit<32> ip_pkt_input_arg_11;
  action labeledstmt_39(){
    ip_pkt_input_arg_11=hdr.ip_pkt.ip_pkt_src;
  }
  bit<16> ip_pkt_input_arg_10;
  action labeledstmt_38(){
    ip_pkt_input_arg_10=hdr.ip_pkt.ip_pkt_ip_7;
  }
  bit<8> ip_pkt_input_arg_9;
  action labeledstmt_37(){
    ip_pkt_input_arg_9=hdr.ip_pkt.ip_pkt_ip_6;
  }
  bit<8> ip_pkt_input_arg_8;
  action labeledstmt_36(){
    ip_pkt_input_arg_8=hdr.ip_pkt.ip_pkt_ip_5;
  }
  bit<16> ip_pkt_input_arg_7;
  action labeledstmt_35(){
    ip_pkt_input_arg_7=hdr.ip_pkt.ip_pkt_ip_4;
  }
  bit<16> ip_pkt_input_arg_6;
  action labeledstmt_34(){
    ip_pkt_input_arg_6=hdr.ip_pkt.ip_pkt_ip_3;
  }
  bit<16> ip_pkt_input_arg_5;
  action labeledstmt_33(){
    ip_pkt_input_arg_5=hdr.ip_pkt.ip_pkt_ip_2;
  }
  bit<8> ip_pkt_input_arg_4;
  action labeledstmt_32(){
    ip_pkt_input_arg_4=hdr.ip_pkt.ip_pkt_ip_1;
  }
  bit<8> ip_pkt_input_arg_3;
  action labeledstmt_31(){
    ip_pkt_input_arg_3=hdr.ip_pkt.ip_pkt_ip_0;
  }
  bit<16> ip_pkt_input_arg_2;
  action labeledstmt_30(){
    ip_pkt_input_arg_2=hdr.ip_pkt.ip_pkt_eth_2;
  }
  bit<48> ip_pkt_input_arg_1;
  action labeledstmt_29(){
    ip_pkt_input_arg_1=hdr.ip_pkt.ip_pkt_eth_1;
  }
  bit<48> ip_pkt_input_arg_0;
  action labeledstmt_28(){
    ip_pkt_input_arg_0=hdr.ip_pkt.ip_pkt_eth_0;
  }
  action labeledstmt_1(){
    labeledstmt_28();
    labeledstmt_29();
    labeledstmt_30();
    labeledstmt_31();
    labeledstmt_32();
    labeledstmt_33();
    labeledstmt_34();
    labeledstmt_35();
    labeledstmt_36();
    labeledstmt_37();
    labeledstmt_38();
    labeledstmt_39();
    labeledstmt_40();
    labeledstmt_41();
    labeledstmt_42();
    labeledstmt_43();
    labeledstmt_44();
    labeledstmt_45();
    labeledstmt_46();
    labeledstmt_47();
    labeledstmt_48();
    labeledstmt_49();
  }
  action labeledstmt_2(){
    //NOOP
  }
  bit<32> idx;
  action labeledstmt_50(){
    idx=((bit<32>)to_immediate_tmp);
  }
  action labeledstmt_3(){
    labeledstmt_50();
  }
  action labeledstmt_4(){
    //NOOP
  }
  bit<32> allowedidx;
  action labeledstmt_51(){
    allowedidx=idx;
  }
  action labeledstmt_5(){
    labeledstmt_51();
  }
  action labeledstmt_6(){
    //NOOP
  }
  bit<32> start_time;
  RegisterAction<bit<32>,bit<32>,bit<32>>(assigned_var_allowedstart_time)
  assigned_var_allowedstart_time_regaction_1457 = {
    void apply(inout bit<32> cell1_remote,
        out bit<32> ret_remote){
      bit<32> cell1_local=cell1_remote;
      bit<32> cell2_local=0;
      if((cell1_local==32w0)){
        cell2_local=precompute;
      }
      if((!(cell1_local==32w0))){
        cell2_local=cell1_local;
      }
      if(true){
        ret_remote=cell2_local;
      }
    }
  };
  action labeledstmt_52(){
   
start_time=assigned_var_allowedstart_time_regaction_1457.execute(allowedidx);
  }
  action labeledstmt_7(){
    labeledstmt_52();
  }
  action labeledstmt_8(){
    //NOOP
  }
  bit<32> to_immediate_tmp1453;
  action labeledstmt_53(){
    to_immediate_tmp1453=(to_immediate_tmp1452-start_time);
  }
  action labeledstmt_9(){
    labeledstmt_53();
  }
  action labeledstmt_10(){
    //NOOP
  }
  bit<32> pred0allowed;
  action labeledstmt_54(){
    pred0allowed=(to_immediate_tmp1453-32w10000);
  }
  action labeledstmt_11(){
    labeledstmt_54();
  }
  action labeledstmt_12(){
    //NOOP
  }
  action labeledstmt_57(){
    f_synthesized_meta_varallowed=8w1;
  }
  action labeledstmt_56(){
    g_synthesized_meta_varallowed=8w27;
  }
  action labeledstmt_55(){
    memop_synthesized_meta_varallowed=8w1;
  }
  action labeledstmt_13(){
    labeledstmt_55();
    labeledstmt_56();
    labeledstmt_57();
  }
  action labeledstmt_58(){
    g_synthesized_meta_varallowed=8w213;
  }
  action labeledstmt_14(){
    labeledstmt_55();
    labeledstmt_58();
    labeledstmt_57();
  }
  action labeledstmt_60(){
    f_synthesized_meta_varallowed=8w62;
  }
  action labeledstmt_59(){
    g_synthesized_meta_varallowed=8w8;
  }
  action labeledstmt_15(){
    labeledstmt_55();
    labeledstmt_59();
    labeledstmt_60();
  }
  action labeledstmt_63(){
    f_synthesized_meta_varallowed=8w56;
  }
  action labeledstmt_62(){
    g_synthesized_meta_varallowed=8w54;
  }
  action labeledstmt_61(){
    memop_synthesized_meta_varallowed=8w0;
  }
  action labeledstmt_16(){
    labeledstmt_61();
    labeledstmt_62();
    labeledstmt_63();
  }
  action labeledstmt_17(){
    //NOOP
  }
  RegisterAction<bit<8>,bit<32>,bit<8>>(allowed)
  allowed_regaction_1458 = {
    void apply(inout bit<8> cell1_remote,
        out bit<8> ret_remote){
      bit<8> cell1_local=cell1_remote;
      bit<8> cell2_local=0;
      if(((8w0<8w0)&&((cell1_local+f_synthesized_meta_varallowed)>8w8))){
        cell1_remote=(8w0|8w0);
      }
      if((!((8w0<8w0)&&((cell1_local+f_synthesized_meta_varallowed)>8w8)))){
        cell1_remote=(g_synthesized_meta_varallowed|8w8);
      }
      if(true){
        ret_remote=cell1_remote;
      }
    }
  };
  action labeledstmt_64(){
    resallowed=allowed_regaction_1458.execute(allowedidx);
  }
  action labeledstmt_18(){
    labeledstmt_64();
  }
  RegisterAction<bit<8>,bit<32>,bit<8>>(allowed)
  allowed_regaction_1459 = {
    void apply(inout bit<8> cell1_remote,
        out bit<8> ret_remote){
      bit<8> cell1_local=cell1_remote;
      bit<8> cell2_local=0;
     
if((((cell1_local+f_synthesized_meta_varallowed)<8w8)||((cell1_local+g_synthesized_meta_varallowed)==8w8))){
        cell1_remote=(f_synthesized_meta_varallowed|8w0);
      }
     
if(((!((cell1_local+f_synthesized_meta_varallowed)<8w8))&&(!((cell1_local+g_synthesized_meta_varallowed)==8w8)))){
        cell1_remote=(8w0|cell1_local);
      }
      if(true){
        ret_remote=cell1_remote;
      }
    }
  };
  action labeledstmt_65(){
    resallowed=allowed_regaction_1459.execute(allowedidx);
  }
  action labeledstmt_19(){
    labeledstmt_65();
  }
  action labeledstmt_20(){
    //NOOP
  }
  action labeledstmt_21(){
    //NOOP
  }
  action labeledstmt_66(){
    ansallowed=1w1;
  }
  action labeledstmt_22(){
    labeledstmt_66();
  }
  action labeledstmt_67(){
    ansallowed=1w0;
  }
  action labeledstmt_23(){
    labeledstmt_67();
  }
  action labeledstmt_24(){
    //NOOP
  }
  action labeledstmt_80(){
    pre_gen_copy_ip_pkt_eth_0=hdr.ip_pkt.ip_pkt_eth_0;
  }
  action labeledstmt_79(){
    pre_gen_copy_ip_pkt_eth_1=hdr.ip_pkt.ip_pkt_eth_1;
  }
  action labeledstmt_78(){
    pre_gen_copy_ip_pkt_eth_2=hdr.ip_pkt.ip_pkt_eth_2;
  }
  action labeledstmt_77(){
    pre_gen_copy_ip_pkt_ip_0=hdr.ip_pkt.ip_pkt_ip_0;
  }
  action labeledstmt_76(){
    pre_gen_copy_ip_pkt_ip_1=hdr.ip_pkt.ip_pkt_ip_1;
  }
  action labeledstmt_75(){
    pre_gen_copy_ip_pkt_ip_2=hdr.ip_pkt.ip_pkt_ip_2;
  }
  action labeledstmt_74(){
    pre_gen_copy_ip_pkt_ip_3=hdr.ip_pkt.ip_pkt_ip_3;
  }
  action labeledstmt_73(){
    pre_gen_copy_ip_pkt_ip_4=hdr.ip_pkt.ip_pkt_ip_4;
  }
  action labeledstmt_72(){
    pre_gen_copy_ip_pkt_ip_5=hdr.ip_pkt.ip_pkt_ip_5;
  }
  action labeledstmt_71(){
    pre_gen_copy_ip_pkt_ip_6=hdr.ip_pkt.ip_pkt_ip_6;
  }
  action labeledstmt_70(){
    pre_gen_copy_ip_pkt_ip_7=hdr.ip_pkt.ip_pkt_ip_7;
  }
  action labeledstmt_69(){
    pre_gen_copy_ip_pkt_src=hdr.ip_pkt.ip_pkt_src;
  }
  action labeledstmt_68(){
    pre_gen_copy_ip_pkt_dst=hdr.ip_pkt.ip_pkt_dst;
  }
  action labeledstmt_25(){
    hdr.bridge_ev.ip_pkt=1;
    hdr.ip_pkt.setValid();
    hdr.ip_pkt.ip_pkt_eth_0=ip_pkt_input_arg_0;
    hdr.ip_pkt.ip_pkt_eth_1=ip_pkt_input_arg_1;
    hdr.ip_pkt.ip_pkt_eth_2=ip_pkt_input_arg_2;
    hdr.ip_pkt.ip_pkt_ip_0=ip_pkt_input_arg_3;
    hdr.ip_pkt.ip_pkt_ip_1=ip_pkt_input_arg_4;
    hdr.ip_pkt.ip_pkt_ip_2=ip_pkt_input_arg_5;
    hdr.ip_pkt.ip_pkt_ip_3=ip_pkt_input_arg_6;
    hdr.ip_pkt.ip_pkt_ip_4=ip_pkt_input_arg_7;
    hdr.ip_pkt.ip_pkt_ip_5=ip_pkt_input_arg_8;
    hdr.ip_pkt.ip_pkt_ip_6=ip_pkt_input_arg_9;
    hdr.ip_pkt.ip_pkt_ip_7=ip_pkt_input_arg_10;
    hdr.ip_pkt.ip_pkt_src=ip_pkt_input_arg_11;
    hdr.ip_pkt.ip_pkt_dst=ip_pkt_input_arg_12;
    hdr.bridge_ev.port_event_id=1;
    ig_tm_md.ucast_egress_port=9w100;
    labeledstmt_68();
    labeledstmt_69();
    labeledstmt_70();
    labeledstmt_71();
    labeledstmt_72();
    labeledstmt_73();
    labeledstmt_74();
    labeledstmt_75();
    labeledstmt_76();
    labeledstmt_77();
    labeledstmt_78();
    labeledstmt_79();
    labeledstmt_80();
  }
  action labeledstmt_81(){
    hdr.ip_pkt.setInvalid();
  }
  action labeledstmt_26(){
    labeledstmt_81();
  }
  action labeledstmt_27(){
    //NOOP
  }
  table table_1469 {
    key = {
      hdr.wire_ev.event_id : ternary;
    }
    actions = {
      labeledstmt_1;
      labeledstmt_2;
    }
    const entries = {
      (1) : labeledstmt_1();
      (_) : labeledstmt_2();
    } 
  } 
  table table_1468 {
    key = {
      hdr.wire_ev.event_id : ternary;
    }
    actions = {
      labeledstmt_3;
      labeledstmt_4;
    }
    const entries = {
      (1) : labeledstmt_3();
      (_) : labeledstmt_4();
    } 
  } 
  table table_1467 {
    key = {
      hdr.wire_ev.event_id : ternary;
    }
    actions = {
      labeledstmt_5;
      labeledstmt_6;
    }
    const entries = {
      (1) : labeledstmt_5();
      (_) : labeledstmt_6();
    } 
  } 
  table table_1466 {
    key = {
      hdr.wire_ev.event_id : ternary;
    }
    actions = {
      labeledstmt_7;
      labeledstmt_8;
    }
    const entries = {
      (1) : labeledstmt_7();
      (_) : labeledstmt_8();
    } 
  } 
  table table_1465 {
    key = {
      hdr.wire_ev.event_id : ternary;
    }
    actions = {
      labeledstmt_9;
      labeledstmt_10;
    }
    const entries = {
      (1) : labeledstmt_9();
      (_) : labeledstmt_10();
    } 
  } 
  table table_1464 {
    key = {
      hdr.wire_ev.event_id : ternary;
    }
    actions = {
      labeledstmt_11;
      labeledstmt_12;
    }
    const entries = {
      (1) : labeledstmt_11();
      (_) : labeledstmt_12();
    } 
  } 
  table table_1463 {
    key = {
      pred1allowed : ternary;
      pred0allowed : ternary;
      hdr.wire_ev.event_id : ternary;
    }
    actions = {
      labeledstmt_13;
      labeledstmt_14;
      labeledstmt_15;
      labeledstmt_16;
      labeledstmt_17;
    }
    const entries = {
      (0,2147483648 &&& 2147483648,1) : labeledstmt_13();
      (_,2147483648 &&& 2147483648,1) : labeledstmt_14();
      (0,_,1) : labeledstmt_15();
      (_,_,1) : labeledstmt_16();
      (_,_,_) : labeledstmt_17();
    } 
  } 
  table table_1462 {
    key = {
      memop_synthesized_meta_varallowed : ternary;
      hdr.wire_ev.event_id : ternary;
    }
    actions = {
      labeledstmt_18;
      labeledstmt_19;
      labeledstmt_20;
      labeledstmt_21;
    }
    const entries = {
      (0,1) : labeledstmt_18();
      (1,1) : labeledstmt_19();
      (_,1) : labeledstmt_20();
      (_,_) : labeledstmt_21();
    } 
  } 
  table table_1461 {
    key = {
      resallowed : ternary;
      hdr.wire_ev.event_id : ternary;
    }
    actions = {
      labeledstmt_22;
      labeledstmt_23;
      labeledstmt_24;
    }
    const entries = {
      (1,1) : labeledstmt_22();
      (_,1) : labeledstmt_23();
      (_,_) : labeledstmt_24();
    } 
  } 
  table table_1460 {
    key = {
      ansallowed : ternary;
      hdr.wire_ev.event_id : ternary;
    }
    actions = {
      labeledstmt_25;
      labeledstmt_26;
      labeledstmt_27;
    }
    const entries = {
      (1,1) : labeledstmt_25();
      (_,1) : labeledstmt_26();
      (_,_) : labeledstmt_27();
    } 
  } 
  apply {
    table_1469.apply();
    table_1468.apply();
    table_1467.apply();
    table_1466.apply();
    table_1465.apply();
    table_1464.apply();
    table_1463.apply();
    table_1462.apply();
    table_1461.apply();
    table_1460.apply();
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
    transition select(hdr.bridge_ev.ip_pkt){
      (1) : parse_eventset_0;
    }
  }
  state parse_eventset_0 {
    pkt.extract(hdr.ip_pkt);
    transition accept;
  }
}
control EgressControl(inout hdr_t hdr,
    inout meta_t meta,
    in egress_intrinsic_metadata_t eg_intr_md,
    in egress_intrinsic_metadata_from_parser_t eg_prsr_md,
    inout egress_intrinsic_metadata_for_deparser_t eg_dprsr_md,
    inout egress_intrinsic_metadata_for_output_port_t eg_oport_md){
  @pa_no_overlay("egress","hdr.ip_pkt.ip_pkt_eth_0")
  @pa_no_overlay("egress","hdr.ip_pkt.ip_pkt_eth_1")
  @pa_no_overlay("egress","hdr.ip_pkt.ip_pkt_eth_2")
  @pa_no_overlay("egress","hdr.ip_pkt.ip_pkt_ip_0")
  @pa_no_overlay("egress","hdr.ip_pkt.ip_pkt_ip_1")
  @pa_no_overlay("egress","hdr.ip_pkt.ip_pkt_ip_2")
  @pa_no_overlay("egress","hdr.ip_pkt.ip_pkt_ip_3")
  @pa_no_overlay("egress","hdr.ip_pkt.ip_pkt_ip_4")
  @pa_no_overlay("egress","hdr.ip_pkt.ip_pkt_ip_5")
  @pa_no_overlay("egress","hdr.ip_pkt.ip_pkt_ip_6")
  @pa_no_overlay("egress","hdr.ip_pkt.ip_pkt_ip_7")
  @pa_no_overlay("egress","hdr.ip_pkt.ip_pkt_src")
  @pa_no_overlay("egress","hdr.ip_pkt.ip_pkt_dst")
  action egr_noop(){
    //NOOP
  }
  action ip_pkt_recirc(){
    hdr.wire_ev.event_id=1;
    meta.egress_event_id=1;
    hdr.bridge_ev.port_event_id=0;
    hdr.bridge_ev.ip_pkt=0;
  }
  action ip_pkt_to_external(){
    meta.egress_event_id=1;
    hdr.lucid_eth.setInvalid();
    hdr.wire_ev.setInvalid();
    hdr.bridge_ev.setInvalid();
  }
  action ip_pkt_to_internal(){
    meta.egress_event_id=1;
    hdr.wire_ev.event_id=1;
    hdr.bridge_ev.port_event_id=0;
    hdr.bridge_ev.ip_pkt=0;
  }
  table t_extract_recirc_event {
    key = {
      eg_intr_md.egress_rid : ternary;
      hdr.bridge_ev.port_event_id : ternary;
      hdr.bridge_ev.ip_pkt : ternary;
    }
    actions = {
      egr_noop;
      ip_pkt_recirc;
    }
    const entries = {
      (1,0,1) : ip_pkt_recirc();
      (_,_,_) : egr_noop();
    } 
  } 
  table t_extract_port_event {
    key = {
      hdr.bridge_ev.port_event_id : ternary;
      eg_intr_md.egress_port : ternary;
    }
    actions = {
      ip_pkt_to_external;
      ip_pkt_to_internal;
    }
    const entries = {
      (1,196) : ip_pkt_to_internal();
      (1,_) : ip_pkt_to_external();
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
