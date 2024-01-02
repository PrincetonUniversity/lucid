(* Add target-specific intrinsics types to the program. 
   These intrinsics are defined in the tofino 
   architectural specification, and used in placed like 
   the ingress parser. *)

open CoreSyntax

(* tofino intrinsics -- should match tofino1_base.p4 *)

type p4_intrinsic_t = {
  tyid : id;
  tyfields : (int * id) list;
}

type p4_intrinsic_param = {
  ipid : id;
  pdir : string option;
  pty : p4_intrinsic_t;
}


let remove_trailing_t str =
  if String.length str >= 2 && String.sub str (String.length str - 2) 2 = "_t" then
    String.sub str 0 (String.length str - 2)
  else
    str
let ty_to_param pty dir = 
  { ipid =  Id.name pty.tyid |> remove_trailing_t |> Id.create;
    pdir = dir;
    pty = pty;
  }
;;

  

let p4header_to_tname h =
  let cid = Cid.create_ids [h.tyid] in
  ty (TName(cid, [], false))
;;
let id = Id.create;;
(* p4 ingress intrinsics *)
let ingress_intrinsic_metadata_t = {
  tyid = Id.create "ingress_intrinsic_metadata_t";
  tyfields = [
    (1, id"resubmit_flag"); 
    (1, id"_pad1"); 
    (2, id"packet_version"); 
    (3,id "_pad2");
    (9,id "ingress_port"); 
    (48,id "ingress_mac_tstamp")];
}


let ingress_intrinsic_metadata_for_tm_t = {
  tyid = Id.create "ingress_intrinsic_metadata_for_tm_t";
  tyfields = [
    (9,id "ucast_egress_port");
    (1,id "bypass_egress");
    (1,id "deflect_on_drop");
    (3,id "ingress_cos");
    (5,id "qid");
    (3,id "icos_for_copy_to_cpu");
    (1,id "copy_to_cpu");
    (2,id "packet_color");
    (1,id "disable_ucast_cutthru");
    (1,id "enable_mcast_cutthru");
    (16,id "mcast_grp_a");
    (16,id "mcast_grp_b");
    (13,id "level1_mcast_hash");
    (13,id "level2_mcast_hash");
    (16,id "level1_exclusion_id");
    (9,id "level2_exclusion_id");
    (16,id "rid");
  ];
}
;;

let ingress_intrinsic_metadata_from_parser_t = {
  tyid = Id.create "ingress_intrinsic_metadata_from_parser_t";
  tyfields = [
    (48,id "global_tstamp");
    (32,id "global_ver");
    (16,id "parser_err");
  ];
}
let ingress_intrinsic_metadata_for_deparser_t = {
  tyid = Id.create "ingress_intrinsic_metadata_for_deparser_t";
  tyfields = [
    (3,id "drop_ctl");
    (3,id "digest_type");
    (3,id "resubmit_type");
    (3,id "mirror_type");
  ];
}

(* p4 egress intrinsics *)
let egress_intrinsic_metadata_t = {
  tyid = Id.create "egress_intrinsic_metadata_t";
  tyfields = [
    (8,id "_pad0");
    (9,id "egress_port");
    (5,id "_pad1");
    (19,id "enq_qdepth");
    (6,id "_pad2");
    (2,id "enq_congest_stat");
    (14,id "_pad3");
    (18,id "enq_tstamp");
    (5,id "_pad4");
    (19,id "deq_qdepth");
    (6,id "_pad5");
    (2,id "deq_congest_stat");
    (8,id "app_pool_congest_stat");
    (14,id "_pad6");
    (18,id "deq_timedelta");
    (16,id "egress_rid");
    (7,id "_pad7");
    (1,id "egress_rid_first");
    (8,id "_pad8");
    (5,id "egress_qid");
    (5,id "_pad9");
    (3,id "egress_cos");
    (7,id "_pad10");
    (1,id "deflection_flag");
    (16,id "pkt_length");
  ];
}

let egress_intrinsic_metadata_from_parser_t = {
  tyid = Id.create "egress_intrinsic_metadata_from_parser_t";
  tyfields = [
    (48,id "global_tstamp");
    (32,id "global_ver");
    (16,id "parser_err");
  ];
}

let egress_intrinsic_metadata_for_deparser_t = {
  tyid = Id.create "egress_intrinsic_metadata_for_deparser_t"; 
  tyfields = [
    (3,id "drop_ctl");
    (3,id "mirror_type");
    (1,id "coalesce_flush");
    (7,id "coalesce_length");
  ];
}

let egress_intrinsic_metadata_for_output_port_t = {
  tyid = Id.create "egress_intrinsic_metadata_for_output_port_t";
  tyfields = [
    (1,id "capture_tstamp_on_tx");
    (1,id "update_delay_on_tx");
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

let egress_parser_intrinsics = [
  ty_to_param egress_intrinsic_metadata_t (Some "out")
]

(* make sure a field, by name, 
   is defined in intr_param's type *)
let ensure_field intr_param field_name : bool =
  let fields_of_param =
    List.map (fun (_, id) -> id) intr_param.pty.tyfields
  in 
  List.exists (fun id -> (fst id) = field_name) fields_of_param
;;



(* CLEANUP: this should be the only way we use the 
intrinsics. As extern user types in the program. 
All other accessor functions above should be deleted. *)
(* convert a P4 intrinsic into an extern type in Core IR *)
let ty_to_dextern p4_intr =
  let fields = List.map 
    (fun (i, id) -> 
      (id,TInt(Sz i))) 
    p4_intr.tyfields
  in
  let record_ty = ty (TRecord(fields)) in
  decl (DExtern(p4_intr.tyid, record_ty))
;;

(* add extern types for necessary p4 intrinsics. 
   This will grow as the backend functionality fills in. *)
let add_intrinsics ds = List.map ty_to_dextern 
  [
    ingress_intrinsic_metadata_t;
    ingress_intrinsic_metadata_from_parser_t;
    ingress_intrinsic_metadata_for_deparser_t;
    ingress_intrinsic_metadata_for_tm_t;
    egress_intrinsic_metadata_t;
    egress_intrinsic_metadata_from_parser_t;
    egress_intrinsic_metadata_for_deparser_t;
    egress_intrinsic_metadata_for_output_port_t
  ]@ds
;;

let intrinsic_to_param intrinsic =
  intrinsic.tyid |> Id.name |> remove_trailing_t |> Id.create, 
  ty (TName(Cid.id intrinsic.tyid, [], false))
;;  


let field_of_intrinsic_opt intrinsic varcid fieldcid =
  (* check to see that fieldcid is defined in the intrinsic, 
     return the full name (varcid.fieldcid) and the type. *)
  let fields = List.map 
    (fun (i, id) -> 
      (id,TInt(Sz i))) 
      intrinsic.tyfields
  in
  match (List.assoc_opt (Cid.to_id fieldcid) fields) with
    | Some(t) -> Some(Cid.concat varcid fieldcid, ty t)
    | None -> None
;;
let field_of_intrinsic intrinsic varcid fieldcid =
  match field_of_intrinsic_opt intrinsic varcid fieldcid with 
    | Some(t) -> t
    | None -> error "[intrinsic.field_of_intrinsic] field not found in intrinsic"
;;

let param_to_cidty intr_param field_name : (cid * ty) = 
  let field_info = (List.find_opt
    (fun (_, id) -> (fst id) = field_name)
    intr_param.pty.tyfields) 
  in
  match field_info with 
  | Some((width, id)) -> Cid.id id, tint@@Sz width
  | None -> error "[intrinsic.param_to_cidty] this field is not a member of the param"
;;