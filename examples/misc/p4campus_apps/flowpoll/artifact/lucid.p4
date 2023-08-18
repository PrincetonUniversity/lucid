/*****Global declarations
(includes, headers, structs, constants, register arrays)*****/
#include <core.p4>
#include <tna.p4>
Register<bit<32>,_>(32w1024) arr_cts;
Register<bit<32>,_>(32w1024) arr_flow_ids;
struct ingress_input_tag_s {
  bit<16> tag;
}
struct pktin_s {
  bit<48> pktin_e_0;
  bit<48> pktin_e_1;
  bit<16> pktin_e_2;
  bit<8> pktin_ip_0;
  bit<8> pktin_ip_1;
  bit<16> pktin_ip_2;
  bit<16> pktin_ip_3;
  bit<16> pktin_ip_4;
  bit<8> pktin_ip_5;
  bit<8> pktin_ip_6;
  bit<16> pktin_ip_7;
  bit<32> pktin_ip_8;
  bit<32> pktin_ip_9;
}
struct record_out_s {
  bit<32> record_out_flow_id;
  bit<32> record_out_ct;
}
struct ingress_input_s {
  ingress_input_tag_s ingress_input_tag;
  pktin_s pktin;
  record_out_s record_out;
}
header ingress_output_tag_h {
  bit<16> tag;
}
header pktin_ingress_output_flag_h {

}
struct pktin_ingress_output_s {
  pktin_ingress_output_flag_h pktin_ingress_output_flag;
}
header record_out_ingress_output_flag_h {

}
struct record_out_ingress_output_s {
  record_out_ingress_output_flag_h record_out_ingress_output_flag;
}
struct ingress_output_s {
  ingress_output_tag_h ingress_output_tag;
  pktin_ingress_output_s pktin_ingress_output;
  record_out_ingress_output_s
record_out_ingress_output;
}
@pa_solitary("ingress","ingress_input.ingress_input_tag.tag")
struct egress_input_tag_s {
  bit<16> tag;
}
struct egress_input_s {
  egress_input_tag_s egress_input_tag;
  pktin_s pktin;
  record_out_s record_out;
}
header eth_h {
  bit<48> dst;
  bit<48> src;
  bit<16> ety;
}
header pktin_egress_output_tag_h {
  bit<16> tag;
}
header pktin_h {
  bit<48> pktin_e_0;
  bit<48> pktin_e_1;
  bit<16> pktin_e_2;
  bit<8> pktin_ip_0;
  bit<8> pktin_ip_1;
  bit<16> pktin_ip_2;
  bit<16> pktin_ip_3;
  bit<16> pktin_ip_4;
  bit<8> pktin_ip_5;
  bit<8> pktin_ip_6;
  bit<16> pktin_ip_7;
  bit<32> pktin_ip_8;
  bit<32> pktin_ip_9;
}
struct pktin_egress_output_s {
  pktin_egress_output_tag_h pktin_egress_output_tag;
  pktin_h pktin;
}
header record_out_egress_output_tag_h {
  bit<16> tag;
}
header record_out_h {
  bit<32> record_out_flow_id;
  bit<32> record_out_ct;
}
struct record_out_egress_output_s {
  record_out_egress_output_tag_h record_out_egress_output_tag;
  record_out_h record_out;
}
struct egress_output_s {
  eth_h eth;
  pktin_egress_output_s pktin_egress_output;
  record_out_egress_output_s
record_out_egress_output;
}
@pa_solitary("egress","egress_input.egress_input_tag.tag")
/*****Main program components (ingress/egress parser, control,
deparser)*****/
parser ingress_main(packet_in pkt,
    out ingress_output_s ingress_output,
    out ingress_input_s ingress_input,
    out ingress_intrinsic_metadata_t ingress_intrinsic_metadata){
  state block_1 {
    bit<16> tag=pkt.lookahead<bit<16>>();
    pkt.advance(16);
    transition select(tag){

    }
  }
  state block_2 {
    ingress_input.pktin.pktin_ip_0=pkt.lookahead<bit<8>>();
    pkt.advance(8);
    ingress_input.pktin.pktin_ip_1=pkt.lookahead<bit<8>>();
    pkt.advance(8);
    ingress_input.pktin.pktin_ip_2=pkt.lookahead<bit<16>>();
    pkt.advance(16);
    ingress_input.pktin.pktin_ip_3=pkt.lookahead<bit<16>>();
    pkt.advance(16);
    ingress_input.pktin.pktin_ip_4=pkt.lookahead<bit<16>>();
    pkt.advance(16);
    ingress_input.pktin.pktin_ip_5=pkt.lookahead<bit<8>>();
    pkt.advance(8);
    ingress_input.pktin.pktin_ip_6=pkt.lookahead<bit<8>>();
    pkt.advance(8);
    ingress_input.pktin.pktin_ip_7=pkt.lookahead<bit<16>>();
    pkt.advance(16);
    ingress_input.pktin.pktin_ip_8=pkt.lookahead<bit<32>>();
    pkt.advance(32);
    ingress_input.pktin.pktin_ip_9=pkt.lookahead<bit<32>>();
    pkt.advance(32);
    ingress_input.ingress_input_tag.tag=16w1;
    transition accept;
  }
  state start {
    pkt.extract(ingress_intrinsic_metadata);
    pkt.advance(64);
    ingress_input.pktin.pktin_e_0=pkt.lookahead<bit<48>>();
    pkt.advance(48);
    ingress_input.pktin.pktin_e_1=pkt.lookahead<bit<48>>();
    pkt.advance(48);
    ingress_input.pktin.pktin_e_2=pkt.lookahead<bit<16>>();
    bit<16> ehdr_2=pkt.lookahead<bit<16>>();
    pkt.advance(16);
    transition select(ehdr_2){
      (666) : block_1;
      (2048) : block_2;
    }
  }
}
control ingress_hdl(inout ingress_output_s ingress_output,
    inout ingress_input_s ingress_input,
    in ingress_intrinsic_metadata_t ingress_intrinsic_metadata,
    in ingress_intrinsic_metadata_from_parser_t
ingress_intrinsic_metadata_from_parser,
    inout ingress_intrinsic_metadata_for_deparser_t
ingress_intrinsic_metadata_for_deparser,
    inout ingress_intrinsic_metadata_for_tm_t
ingress_intrinsic_metadata_for_tm){
  bit<32> flow_id;
  CRCPolynomial<bit<32>>(13,false, false, false, 0, 0) hash_9160_crc;
  Hash<bit<32>>(HashAlgorithm_t.CUSTOM,hash_9160_crc) hash_9160;
  action labeledstmt_10(){
    flow_id=hash_9160.get({ingress_input.pktin.pktin_ip_8,
ingress_input.pktin.pktin_ip_9});
  }
  action labeledstmt_11(){
    ingress_output.ingress_output_tag.setValid();
  }
  action labeledstmt_12(){
    ingress_output.ingress_output_tag.tag=16w1;
  }
  action labeledstmt_1(){
    labeledstmt_10();
    labeledstmt_11();
    labeledstmt_12();
  }
  action labeledstmt_13(){
    ingress_output.ingress_output_tag.tag=16w2;
  }
  action labeledstmt_2(){
    labeledstmt_11();
    labeledstmt_13();
  }
  action labeledstmt_3(){
    //NOOP
  }
  bit<10> idx;
  CRCPolynomial<bit<10>>(91,false, false, false, 0, 0) hash_9170_crc;
  Hash<bit<10>>(HashAlgorithm_t.CUSTOM,hash_9170_crc) hash_9170;
  action labeledstmt_14(){
    idx=hash_9170.get({flow_id});
  }
  action labeledstmt_4(){
    labeledstmt_14();
  }
  action labeledstmt_5(){
    //NOOP
  }
  bit<32> unused;
  RegisterAction<bit<32>,bit<10>,bit<32>>(arr_cts)
  arr_cts_regaction_918 = {
    void apply(inout bit<32> cell1_remote,
        out bit<32> ret_remote){
      bit<32> cell1_local=cell1_remote;
      bit<32> cell2_local=0;
      if(true){
        cell1_remote=(cell1_local+32w1);
      }
      if(true){
        cell2_local=(cell1_local+32w1);
      }
      if(true){
        ret_remote=cell2_local;
      }
    }
  };
  action labeledstmt_15(){
    unused=arr_cts_regaction_918.execute(idx);
  }
  action labeledstmt_6(){
    labeledstmt_15();
  }
  action labeledstmt_7(){
    //NOOP
  }
  bit<32> prev_id;
  RegisterAction<bit<32>,bit<10>,bit<32>>(arr_flow_ids)
  arr_flow_ids_regaction_919 = {
    void apply(inout bit<32> cell1_remote,
        out bit<32> ret_remote){
      bit<32> cell1_local=cell1_remote;
      bit<32> cell2_local=0;
      if(true){
        cell1_remote=flow_id;
      }
      if(true){
        cell2_local=cell1_local;
      }
      if(true){
        ret_remote=cell2_local;
      }
    }
  };
  action labeledstmt_16(){
    prev_id=arr_flow_ids_regaction_919.execute(idx);
  }
  action labeledstmt_8(){
    labeledstmt_16();
  }
  action labeledstmt_9(){
    //NOOP
  }
  table table_923 {
    key = {
      ingress_input.ingress_input_tag.tag : ternary;
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
  table table_922 {
    key = {
      ingress_input.ingress_input_tag.tag : ternary;
    }
    actions = {
      labeledstmt_4;
      labeledstmt_5;
    }
    const entries = {
      (1) : labeledstmt_4();
      (_) : labeledstmt_5();
    } 
  } 
  @ignore_table_dependency("ingress_hdl.table_921")table table_920 {
    key = {
      ingress_input.ingress_input_tag.tag : ternary;
    }
    actions = {
      labeledstmt_6;
      labeledstmt_7;
    }
    const entries = {
      (1) : labeledstmt_6();
      (_) : labeledstmt_7();
    } 
  } 
  @ignore_table_dependency("ingress_hdl.table_920")table table_921 {
    key = {
      ingress_input.ingress_input_tag.tag : ternary;
    }
    actions = {
      labeledstmt_8;
      labeledstmt_9;
    }
    const entries = {
      (1) : labeledstmt_8();
      (_) : labeledstmt_9();
    } 
  } 
  apply {
    table_923.apply();
    table_922.apply();
    table_920.apply();
    table_921.apply();
  }
} 
control ingress_deparser(packet_out pkt,
    inout ingress_output_s ingress_output,
    in ingress_input_s ingress_input,
    in ingress_intrinsic_metadata_for_deparser_t
ingress_intrinsic_metadata_for_deparser){

  apply {
    pkt.emit(ingress_output);
  }
} 
parser egress_main(packet_in pkt,
    out egress_output_s egress_output,
    out egress_input_s egress_input,
    out egress_intrinsic_metadata_t egress_intrinsic_metadata){
  state block_3 {
    transition select(egress_intrinsic_metadata.egress_rid){

    }
  }
  state start {
    pkt.extract(egress_intrinsic_metadata);
    bit<16> ingress_union_tag=pkt.lookahead<bit<16>>();
    pkt.advance(16);
    transition select(ingress_union_tag){
      (1) : block_3;
      (2) : block_3;
    }
  }
}
control egress_hdl(inout egress_output_s egress_output,
    inout egress_input_s egress_input,
    in egress_intrinsic_metadata_t egress_intrinsic_metadata,
    in egress_intrinsic_metadata_from_parser_t
egress_intrinsic_metadata_from_parser,
    inout egress_intrinsic_metadata_for_deparser_t
egress_intrinsic_metadata_for_deparser,
    inout egress_intrinsic_metadata_for_output_port_t
egress_intrinsic_metadata_for_output_port){
  action labeledstmt_23(){
    egress_intrinsic_metadata_for_deparser.drop_ctl=3w1;
  }
  action labeledstmt_24(){
    egress_output.pktin_egress_output.pktin.setValid();
  }
  action labeledstmt_25(){
   
egress_output.pktin_egress_output.pktin.pktin_e_0=egress_input.pktin.pktin_e_0;
  }
  action labeledstmt_26(){
   
egress_output.pktin_egress_output.pktin.pktin_e_1=egress_input.pktin.pktin_e_1;
  }
  action labeledstmt_27(){
   
egress_output.pktin_egress_output.pktin.pktin_e_2=egress_input.pktin.pktin_e_2;
  }
  action labeledstmt_28(){
   
egress_output.pktin_egress_output.pktin.pktin_ip_0=egress_input.pktin.pktin_ip_0;
  }
  action labeledstmt_29(){
   
egress_output.pktin_egress_output.pktin.pktin_ip_1=egress_input.pktin.pktin_ip_1;
  }
  action labeledstmt_30(){
   
egress_output.pktin_egress_output.pktin.pktin_ip_2=egress_input.pktin.pktin_ip_2;
  }
  action labeledstmt_31(){
   
egress_output.pktin_egress_output.pktin.pktin_ip_3=egress_input.pktin.pktin_ip_3;
  }
  action labeledstmt_32(){
   
egress_output.pktin_egress_output.pktin.pktin_ip_4=egress_input.pktin.pktin_ip_4;
  }
  action labeledstmt_33(){
   
egress_output.pktin_egress_output.pktin.pktin_ip_5=egress_input.pktin.pktin_ip_5;
  }
  action labeledstmt_34(){
   
egress_output.pktin_egress_output.pktin.pktin_ip_6=egress_input.pktin.pktin_ip_6;
  }
  action labeledstmt_35(){
   
egress_output.pktin_egress_output.pktin.pktin_ip_7=egress_input.pktin.pktin_ip_7;
  }
  action labeledstmt_36(){
   
egress_output.pktin_egress_output.pktin.pktin_ip_8=egress_input.pktin.pktin_ip_8;
  }
  action labeledstmt_37(){
   
egress_output.pktin_egress_output.pktin.pktin_ip_9=egress_input.pktin.pktin_ip_9;
  }
  action labeledstmt_17(){
    labeledstmt_23();
    labeledstmt_24();
    labeledstmt_25();
    labeledstmt_26();
    labeledstmt_27();
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
  }
  action labeledstmt_38(){
    egress_output.record_out_egress_output.record_out.setValid();
  }
  action labeledstmt_39(){
   
egress_output.record_out_egress_output.record_out.record_out_flow_id=egress_input.record_out.record_out_flow_id;
  }
  action labeledstmt_40(){
   
egress_output.record_out_egress_output.record_out.record_out_ct=egress_input.record_out.record_out_ct;
  }
  action labeledstmt_18(){
    labeledstmt_23();
    labeledstmt_38();
    labeledstmt_39();
    labeledstmt_40();
  }
  action labeledstmt_19(){
    labeledstmt_23();
  }
  action labeledstmt_41(){
    egress_intrinsic_metadata_for_deparser.drop_ctl=3w0;
  }
  action labeledstmt_20(){
    labeledstmt_41();
  }
  action labeledstmt_21(){
    labeledstmt_41();
  }
  action labeledstmt_22(){
    //NOOP
  }
  table table_925 {
    key = {
      egress_input.egress_input_tag.tag : ternary;
    }
    actions = {
      labeledstmt_17;
      labeledstmt_18;
      labeledstmt_19;
    }
    const entries = {
      (1) : labeledstmt_17();
      (2) : labeledstmt_18();
      (_) : labeledstmt_19();
    } 
  } 
  table table_924 {
    key = {
      egress_input.egress_input_tag.tag : ternary;
    }
    actions = {
      labeledstmt_20;
      labeledstmt_21;
      labeledstmt_22;
    }
    const entries = {
      (1) : labeledstmt_20();
      (2) : labeledstmt_21();
      (_) : labeledstmt_22();
    } 
  } 
  apply {
    table_925.apply();
    table_924.apply();
  }
} 
control egress_deparser(packet_out pkt,
    inout egress_output_s egress_output,
    in egress_input_s egress_input,
    in egress_intrinsic_metadata_for_deparser_t
egress_intrinsic_metadata_for_deparser){

  apply {
    pkt.emit(egress_output);
  }
} 
/*****Pipeline and main declarations*****/
Pipeline(ingress_main(),
  ingress_hdl(),
  ingress_deparser(),
  egress_main(),
  egress_hdl(),
  egress_deparser()) pipe;
Switch(pipe) main;
