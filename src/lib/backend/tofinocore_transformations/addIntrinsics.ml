(* add target-specific intrinsics to each component. 
   These intrinsics are the P4-tofino intrinsic headers *)

open Angstrom
open CoreSyntax
open TofinoCoreNew

(* tofino intrinsics -- should match tofino1_base.p4 *)

type p4_header = {
  name : string;
  fields : (int * string) list;
}

let p4_header_to_usertydecl h = 
  TDUserTy({
    tyid = Id.create h.name;
    tyfields = List.map (fun (w, f) -> (Id.create f, ty (TInt w))) h.fields;
    tyextern = true;
  })
;;

let p4header_to_tname h =
  let cid = Cid.create [h.name] in
  ty (TName(cid, [], false))
;;

(* p4 ingress intrinsics *)
let ingress_intrinsic_metadata_t = {
  name = "ingress_intrinsic_metadata_t";
  fields = [
    (1, "resubmit_flag"); 
    (1, "_pad1"); 
    (2, "packet_version"); 
    (3, "_pad2");
    (9, "ingress_port"); 
    (48, "ingress_mac_tstamp")];
}


let ingress_intrinsic_metadata_for_tm_t = {
  name = "ingress_intrinsic_metadata_for_tm_t";
  fields = [
    (9, "ucast_egress_port");
    (1, "bypass_egress");
    (1, "deflect_on_drop");
    (3, "ingress_cos");
    (5, "qid");
    (3, "icos_for_copy_to_cpu");
    (1, "copy_to_cpu");
    (2, "packet_color");
    (1, "disable_ucast_cutthru");
    (1, "enable_mcast_cutthru");
    (16, "mcast_grp_a");
    (16, "mcast_grp_b");
    (13, "level1_mcast_hash");
    (13, "level2_mcast_hash");
    (16, "level1_exclusion_id");
    (9, "level2_exclusion_id");
    (16, "rid");
  ];
}
;;

let ingress_intrinsic_metadata_from_parser_t = {
  name = "ingress_intrinsic_metadata_from_parser_t";
  fields = [
    (48, "global_tstamp");
    (32, "global_ver");
    (16, "parser_err");
  ];
}
let ingress_intrinsic_metadata_for_deparser_t = {
  name = "ingress_intrinsic_metadata_for_deparser_t";
  fields = [
    (3, "drop_ctl");
    (3, "digest_type");
    (3, "resubmit_type");
    (3, "mirror_type");
  ];
}

(* p4 egress intrinsics *)
let egress_intrinsic_metadata_t = {
  name = "egress_intrinsic_metadata_t";
  fields = [
    (8, "_pad0");
    (9, "egress_port");
    (5, "_pad1");
    (19, "enq_qdepth");
    (6, "_pad2");
    (2, "enq_congest_stat");
    (14, "_pad3");
    (18, "enq_tstamp");
    (5, "_pad4");
    (19, "deq_qdepth");
    (6, "_pad5");
    (2, "deq_congest_stat");
    (8, "app_pool_congest_stat");
    (14, "_pad6");
    (18, "deq_timedelta");
    (16, "egress_rid");
    (7, "_pad7");
    (1, "egress_rid_first");
    (8, "_pad8");
    (5, "egress_qid");
    (5, "_pad9");
    (3, "egress_cos");
    (7, "_pad10");
    (1, "deflection_flag");
    (16, "pkt_length");
  ];
}

let egress_intrinsic_metadata_from_parser_t = {
  name = "egress_intrinsic_metadata_from_parser_t";
  fields = [
    (48, "global_tstamp");
    (32, "global_ver");
    (16, "parser_err");
  ];
}

let egress_intrinsic_metadata_for_deparser_t = {
  name = "egress_intrinsic_metadata_for_deparser_t"; 
  fields = [
    (3, "drop_ctl");
    (3, "mirror_type");
    (1, "coalesce_flush");
    (7, "coalesce_length");
  ];
}

let egress_intrinsic_metadata_for_output_port_t = {
  name = "egress_intrinsic_metadata_for_output_port_t";
  fields = [
    (1, "capture_tstamp_on_tx");
    (1, "update_delay_on_tx");
  ];
}

(* list out the intrinsics of each component, along with their parameter names 
   and directions. Later, the intrinsics for the parsers (if any) will go here too. *)
let ingress_handler_intrinsics = [
  (Id.create "ingress_intrinsic_metadata", p4header_to_tname ingress_intrinsic_metadata_t, Some("in"));
  (Id.create "ingress_intrinsic_metadata_from_parser", p4header_to_tname ingress_intrinsic_metadata_from_parser_t, Some("in")); 
  (Id.create "ingress_intrinsic_metadata_for_tm", p4header_to_tname ingress_intrinsic_metadata_for_tm_t, Some("inout"));
  (Id.create "ingress_intrinsic_metadata_for_deparser", p4header_to_tname ingress_intrinsic_metadata_for_deparser_t, Some("inout"));
]
;;
let egress_handler_intrinsics = [
  (Id.create "egress_intrinsic_metadata", p4header_to_tname egress_intrinsic_metadata_t, Some("in"));
  (Id.create "egress_intrinsic_metadata_from_parser", p4header_to_tname egress_intrinsic_metadata_from_parser_t, Some("in"));
  (Id.create "egress_intrinsic_metadata_for_deparser", p4header_to_tname egress_intrinsic_metadata_for_deparser_t, Some("inout"));
  (Id.create "egress_intrinsic_metadata_for_output_port", p4header_to_tname egress_intrinsic_metadata_for_output_port_t, Some("inout"));
]
;;

(* add intrinsics to ingress and egress handlers and parsers *)
let rec add_intrinsic_metadata prog = match prog with
  | [] -> []
  | {comp_id; comp_succ; comp_decls; comp_sort}::prog -> 
    match comp_sort with
    | HData -> 
      (* set intrinsics in main handler *)
      let comp_decls = List.map (fun td -> match td.td with 
        | TDHandler(HEvent(igr_main)) ->
           {td with td = TDHandler(HEvent(
              {igr_main with hdl_intrinsics = ingress_handler_intrinsics;}))}
        | _ -> td)
        comp_decls
      in
      {comp_id; comp_succ; comp_decls; comp_sort}::(add_intrinsic_metadata prog)
    | HEgress -> 
      (* set intrinsics in main handler *)
      let comp_decls = List.map (fun td -> match td.td with 
        | TDHandler(HEvent(egr_main)) ->
           {td with td = TDHandler(HEvent(
              {egr_main with hdl_intrinsics = egress_handler_intrinsics;}))}
        | _ -> td)
        comp_decls
      in
      {comp_id; comp_succ; comp_decls; comp_sort}::(add_intrinsic_metadata prog)
    | _ -> {comp_id; comp_succ; comp_decls; comp_sort}::(add_intrinsic_metadata prog)
;;

