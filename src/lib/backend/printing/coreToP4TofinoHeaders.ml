(* tofinoCore --> P4 Tofino Syntax
  headers and parsing *)
open TofinoCore
open InterpHelpers
open P4TofinoSyntax
module T = P4TofinoSyntax 
module C = TofinoCore
module CS = CoreSyntax
(* open P4TofinoParsing *)
open P4TofinoPrinting

let translate_rty rty =
  match rty with 
  | C.TInt(s) -> T.TInt(s)
  | _ -> error "[translate_rty] not a TInt"
;;

(*** structs ***)

(* tofino builtins -- ingress *)
let ig_intr_md_t = id "ingress_intrinsic_metadata_t"
let ig_intr_md_arg = id "ig_intr_md"

let ig_prsr_md_t = id "ingress_intrinsic_metadata_from_parser_t"
let ig_prsr_md_arg = id "ig_prsr_md"

let ig_dprsr_md_t = id "ingress_intrinsic_metadata_for_deparser_t"
let ig_dprsr_md_arg = id "ig_dprsr_md"

let ig_tm_md_t = id "ingress_intrinsic_metadata_for_tm_t"
let ig_tm_md_arg = id "ig_tm_md"

let igr_port_field = id "ingress_port"
let igr_port_arg = Cid.create_ids [ig_intr_md_arg; igr_port_field]

let egr_port_field = id "ucast_egress_port"
let egr_port_arg = Cid.create_ids [ig_tm_md_arg; egr_port_field]


(* multicast group a references multicast groups that broadcast 
   different numbers of packets to the recirculation port. 
   It allows us to support multiple event generations per control flow *)
let mcast_grp_a_field = id "mcast_grp_a"
let mcast_grp_b_field = id "mcast_grp_b"
let mcast_grp_a_arg = Cid.create_ids [ig_tm_md_arg; mcast_grp_a_field]
let mcast_grp_b_arg = Cid.create_ids [ig_tm_md_arg; mcast_grp_b_field]
let mcast_grp_a_init = 0
let mcast_locations_start = 1024
let mcast_flood_start = 512
let mcast_flood_num port = mcast_flood_start + port
(* any packet created by matching on group b gets rid 0 -- just like unicast packets *)
let port_event_rid = 0

(* ingress header and metadata stacks *)
let hdr_t_id = id "hdr_t"
let hdr_t_arg = id "hdr"
let md_t_id = id "meta_t"
let md_t_arg = id "meta"

(* extracted into header for lucid internal packets, 
   added to header of external packets parsed as default events *)
let eth_t_id = id "lucid_eth_t"
let eth_field = id "lucid_eth"
let edst_field = id "dst_addr"
let esrc_field = id "src_addr"
let etype_field = id "etype"
let eth_arg = Cid.create_ids [hdr_t_arg; eth_field]

let edst_arg = Cid.concat eth_arg (Cid.id edst_field)
let esrc_arg = Cid.concat eth_arg (Cid.id esrc_field)
let etype_arg = Cid.concat eth_arg (Cid.id etype_field)
let lucid_etype = 0x666

let eth_struct = dstruct eth_t_id THdr
  [
    (edst_field, tint 48);
    (esrc_field, tint 48);
    (etype_field, tint 16)
  ]

(* wire event header -- holds the int id of the event in the packet *)
let single_ev_t_id = id "wire_ev_t"
let single_ev_field = id "wire_ev"
let single_ev_arg = Cid.create_ids [hdr_t_arg; single_ev_field]
let cur_ev_field = id "event_id"
let cur_ev_width = 8
let cur_ev_arg = Cid.concat single_ev_arg (Cid.id cur_ev_field)

let wire_ev_struct = dstruct single_ev_t_id THdr
  [cur_ev_field, tint 8]
;;


(* bridged event header -- carries events from ingress to egress
  contains the id of the event to send to another port, and a 
  vector of flags indicating which events the packet contains *)
let multi_ev_t = id "bridge_ev_t"
let multi_ev_field = id "bridge_ev"
let multi_ev_arg = Cid.create_ids [hdr_t_arg; multi_ev_field]

let port_out_event_field = id "port_event_id"
let port_out_event_width = cur_ev_width
let port_out_event_arg = Cid.concat multi_ev_arg (Cid.id port_out_event_field)

let handler_multi_ev_flag_arg evid = Cid.concat multi_ev_arg (Cid.id evid)
let event_bridged_arg evid = handler_multi_ev_flag_arg evid

let dmulti_ev_struct evids =
  let flag_fields = List.map (fun (evid) -> (evid, tint 1)) evids in
  let multi_ev_pad_field = 
    (id "flag_pad", tint (8 - (List.length flag_fields) mod 8))
  in
  dstruct 
    multi_ev_t THdr 
    ((port_out_event_field, tint port_out_event_width)::multi_ev_pad_field::flag_fields)
;;


(* event headers -- carry event arguments. Created later. Names here. *)
(* reference to the struct instance of event evid *)
let ev_arg evid = Cid.create_ids [hdr_t_arg; evid]
let handler_struct_arg = ev_arg
(* struct type of an event / handler *)
let ev_struct_t evid = ((fst evid)^"_t", snd evid)
let handler_struct_id = ev_struct_t
(* reference to a single parameter of an event / struct *)
let ev_param_arg evid id = Cid.concat (ev_arg evid) (Cid.id id)
let handler_param_arg = ev_param_arg
;;


let devent_struct (evid, params) =
  let fields = 
    List.map 
      (fun (id, ty) -> 
        id, tint (InterpHelpers.width_from_ty ty))
      params
  in 
  dstruct (ev_struct_t evid) THdr fields 
;;

let devent_structs ev_params =
  List.map devent_struct ev_params
;;

let hdr_struct evids = 
  dstruct hdr_t_id TMeta
    (
      [
        (eth_field,tstruct eth_t_id);
       (single_ev_field, tstruct single_ev_t_id);
       (multi_ev_field, tstruct multi_ev_t)
      ]@
    (List.map (fun evid -> (evid, tstruct (ev_struct_t evid))) evids)
    )
;;

let meta_struct = 
  dstruct md_t_id TMeta []
;;
(* generate all ingress structures *)
let generate_structs tds =
  let m = main tds in 
  let evids = m.hdl_enum |> List.split |> fst in 

  let multi_ev_struct = dmulti_ev_struct (m.hdl_enum |> List.split |> fst) in
  let ev_structs = devent_structs m.hdl_params in 

  [eth_struct; wire_ev_struct; multi_ev_struct]
  @ev_structs
  @[hdr_struct evids; meta_struct]
;;

(*** ingress parser ***)

(* parser-only parameters *)
let pkt_t, pkt_arg = id "packet_in", id "pkt"

(* parse state ids *)
let start_state_id = id "start"
let default_setup_id = id "default_setup"
let eth_state_id = id "parse_eth"
let single_ev_state_id = id ("parse_"^(fst single_ev_field))

let ingress_parser_params = [
  param (tstruct pkt_t) pkt_arg;
  outparam (tstruct hdr_t_id) hdr_t_arg;
  outparam (tstruct md_t_id) md_t_arg;
  outparam (tstruct ig_intr_md_t) ig_intr_md_arg;
] ;; 


let event_parse_state id =
  Id.create (("parse_")^(fst id))
;;

let extract field_inst = 
  let extract_fcn = 
    (Cid.concat (Cid.Id pkt_arg) (Cid.create ["extract"]))
  in
  sunit (ecall extract_fcn [evar field_inst])
;;

let advance nbits =
  let advance_fcn = 
    (Cid.concat (Cid.Id pkt_arg) (Cid.create ["advance"]))
  in
  sunit (ecall advance_fcn [eval_int nbits])
;;

let transition id = sunit (ejump (Cid.create_ids [id]))
let transition_accept = transition (id "accept")


let transition_select field cases default_opt =
  let branches = (List.map 
      (fun (i, state) -> ([pnum i], sunit (ejump (Cid.id state))))
      cases)
  in 
  let branches = match default_opt with 
    | Some(id) -> branches@[([pwild], sunit (ejump (Cid.id id)))]
    | _ -> branches
  in 
  smatch 
    [evar field]
    branches
;;    

let transition_select_fields field_ids int_pats state_ids default_state_id =
  let branches = List.map 
    (fun (int_pat, state_id) -> 
      (List.map pnum int_pat, transition state_id))
    (List.combine int_pats state_ids)
  in
  let default_branch = match default_state_id with 
    | None -> []
    | Some (id) -> [([pwild], transition id)]
  in
  let branches = branches@default_branch in 
  let exps = List.map evar field_ids in
  smatch exps branches
;;    


let validate local =
  sunit (ecall (Cid.concat local (cid "setValid")) [])
;;
let invalidate local = 
  sunit (ecall (Cid.concat local (cid "setInvalid")) [])

let set_int local i =
  sassign local (eval_int i)
;;

let dparsestate id stmt =
  decl (DParseState {id; body = stmt;})
;;

let generate_ingress_parser block_id (m:main_handler) lucid_internal_ports =
  (* parse states: *)
  let start_state = DParseState
    { id=start_state_id; 
      body=sseq (
        [
        extract (Cid.id ig_intr_md_arg);
        advance 64;
        (* validate multi_ev_arg; *)
        ]
(*         @
        (List.map 
          (fun (evid, _) -> set_int (handler_multi_ev_flag_arg evid) 0)
          m.hdl_enum) *)
        @
        [
        transition_select
          igr_port_arg
          (List.map 
            (fun port -> (port, eth_state_id))
            lucid_internal_ports)
          (Some default_setup_id)
      ]);
    } 
  in 
  let default_setup_state = DParseState
  (* case: packet on non-lucid port. 
           1. enable empty ethernet header 
           2. enable single_lucid_event header 
           3. set event type to default_ev (if there's a default event)
           4. jump to extract default event (if there's a default event) *)
    {
      id = default_setup_id;
      body = sseq ([
        (validate eth_arg);
        (set_int edst_arg 0);
        (set_int esrc_arg 0);
        (set_int etype_arg lucid_etype);
        (validate single_ev_arg)]
        (* set event arg *)
        @(match m.default_hdl with 
          | Some def_hdl_id ->
            let def_hdl_num = (List.assoc def_hdl_id m.hdl_enum) in
            [set_int cur_ev_arg def_hdl_num]
          | None -> []
        )
        @[
        (validate multi_ev_arg);
        (set_int port_out_event_arg 0)
        ]
      (* zero multi_ev_arg *)
      @(List.map (fun evid -> set_int (handler_multi_ev_flag_arg evid) 0) (m.hdl_enum |> List.split |> fst) )
      (* transition to default event *)
      @(
        match m.default_hdl with 
        | Some def_hdl_id -> [transition (event_parse_state def_hdl_id)]
        | None -> []
        ));
    }
  in
  let eth_state = DParseState
    {
      id=eth_state_id;
      body = sseq [
        extract (eth_arg);
        transition_select 
          etype_arg 
          [lucid_etype, single_ev_state_id]
          None (* undefined behavior when a non-lucid packet arrives on a lucid port *)
      ];
    }
  in
  (* This parse state is never used, we add it so that the tofino compiler doesn't try to 
     "optimize" the program by overlaying different event headers... *)
  let parse_all_events_state_id = id "parse_all_events" in 
  let parse_all_events_state = DParseState {
    id=parse_all_events_state_id;
    body = sseq (List.map 
      (fun evid -> extract (ev_arg evid)) 
      (List.split m.hdl_enum |> fst)@[transition_accept]);
    }
  in


  let single_ev_state = 
    let branches = (255, parse_all_events_state_id)::(List.map
          ((fun (evid, evnum) -> (evnum, event_parse_state evid)))
          m.hdl_enum
        )
    in
  DParseState
    {
      id=single_ev_state_id;
      body = sseq [
        extract single_ev_arg;
        extract multi_ev_arg; (* may want to remove this if its only on egress *)
        transition_select 
          (cur_ev_arg)
          branches
          None
      ];
    }
  in 

  let parse_state_of_ev evid =
(*     let extract_stmts = 
      let init_evfields evid =
        let params = List.assoc evid m.hdl_params in
        (validate (handler_struct_arg evid))::(List.fold_left 
          (fun stmts (param_id, _) ->         
            stmts@[set_int (handler_param_arg evid param_id) 0]
          )
          []
          params
        )
      in  
      List.fold_left
      (fun stmts evid_alt -> 
        stmts@(
          if (Id.equal evid evid_alt)
          then ([extract (ev_arg evid_alt)])
          else (init_evfields evid_alt)
        )
      )
      []
      (List.split m.hdl_params |> fst)
    in
 *)    let extract_stmts = [extract (ev_arg evid)] in 
    DParseState{id=event_parse_state evid;
      body = sseq (
        (* enable all the other event headers, set them to 0, and extract this one *)
        extract_stmts@
        [transition_accept]
      );
    }
  in 
  let parse_ev_states = 
    List.map
      parse_state_of_ev
      (List.split m.hdl_enum |> fst)
  in 

  let decls = 
    List.map 
      (fun d -> {d=d; dpragma=[];})
      ([start_state; default_setup_state; eth_state; single_ev_state]@parse_ev_states@[parse_all_events_state]) 
  in 
  decl (DParse({id=block_id; params=ingress_parser_params; decls; body=None;}))
;;


(*** ingress deparser ***)
(* deparser is simple. Everything valid gets emitted. *)

let pkt_out_t, pkt_out_arg = id "packet_out", id "pkt"
(* the _param_ is the Cid, because its the one you use in the body. *)
let pkt_out_param = Cid.id pkt_out_arg

let ingress_deparser_params = [
  param (tstruct pkt_out_t) pkt_out_arg;
  inoutparam (tstruct hdr_t_id) hdr_t_arg;
  inparam (tstruct md_t_id) md_t_arg;
  inparam (tstruct ig_dprsr_md_t) ig_dprsr_md_arg
] ;; 

let ecall_emit pkt args =
  ecall (Cid.concat pkt (Cid.create ["emit"])) args
;;

let generate_ingress_deparser block_id tds =
  let m = main tds in 
  let _ = m in 
  (* old, when we thought we could validate all event headers in parser *)
(*   let emit_calls = [eth_arg; multi_ev_arg]
    @(List.map handler_struct_arg
      (m.hdl_enum |> List.split |> fst))
    |> List.map (fun cid -> [evar cid])
    |> List.map (ecall_emit pkt_out_param)
    |> List.map (sunit)
  in
 *)
  let emit_calls = [sunit (ecall_emit pkt_out_param ([Cid.id hdr_t_arg|> evar]) )] in 

  decl (DDeparse{
    id=block_id;
    params=ingress_deparser_params;
    decls = [];
    body = Some(sseq emit_calls);
  })
;;




(*** egress parsing ***)

(* 
  egress parser: 
    the packet coming into egress will always have: 
      lucid_eth
      wire_meta
      bridged_meta
    it will then have up to 1 of each event. 
      The combination of events is determined by the event 
      present flags in bridged meta. 
      The TofinoCore program has a list of all the possible 
      combinations of events that a program may generate.
      We use this to generate the parser's select statement.
*)

let eg_intr_md_t = id "egress_intrinsic_metadata_t"
let eg_intr_md = id "eg_intr_md"

let rid_field = id "egress_rid"
let egress_port_field = id "egress_port"

let rid_local = Cid.create_ids [eg_intr_md; rid_field]
let egress_port_local = Cid.create_ids [eg_intr_md; egress_port_field]

let eg_prsr_md_t = id "egress_intrinsic_metadata_from_parser_t"
let eg_prsr_md = id "eg_prsr_md"

let eg_dprsr_md_t = id "egress_intrinsic_metadata_for_deparser_t"
let eg_dprsr_md = id "eg_dprsr_md"

let eg_oport_md_t = id "egress_intrinsic_metadata_for_output_port_t"
let eg_oport_md = id "eg_oport_md"

let egress_parser_params = [
  param (tstruct pkt_t) pkt_arg;
  outparam (tstruct hdr_t_id) hdr_t_arg;
  outparam (tstruct md_t_id) md_t_arg;
  outparam (tstruct eg_intr_md_t) eg_intr_md;
] ;;

open Batteries


(* assuming there is a flag for each ev in evid, 
   generate a pattern of flag values corresponding 
   to the generation of the subset of evids in 
   evid_seq *)
let intpats_of_evseq evids evid_seq =
  let base_flagpat = List.map (fun _ -> 0) evids in 
  List.fold_left (fun flagpat evid ->
      let idx = (List.index_of evid evids |> Option.get) in
      List.modify_at idx (fun _ -> 1) flagpat
    )
    base_flagpat
    evid_seq
;;

(*generate the transition statement for egress *)
let eventset_parse_tree tds =
  let evids = (main tds).hdl_enum |> List.split |> fst in 
  let event_id_sequences = (main tds).event_output.ev_gen_seqs in 

  (* generate a parse state, transition statement, and pattern list *)
  let generate_objs_for_evseq (i:int) (generated_evids:Id.t list) =
    (* order events by declaration order, since that is the order 
       that they will appear in the packet.*)
    let generated_evids = List.filter
      (fun evid -> MiscUtils.contains generated_evids evid)
      evids
    in  

    let state_id = (id ("parse_eventset_"^(string_of_int i))) in 
    let stmts = 
      (List.map (fun evid -> extract (ev_arg evid)) generated_evids) 
      @[transition_accept]
    in
    let state = dparsestate state_id (sseq stmts) in 
    let flagpat = intpats_of_evseq evids generated_evids in 
    (flagpat, state_id), state
  in
  let branches, states = 
    List.mapi generate_objs_for_evseq event_id_sequences |> List.split
  in 
  let pats, state_ids = List.split branches in 
  let key_cids = List.map (handler_multi_ev_flag_arg) evids in 
  (* the transition statement to put in start state *)
  let transition_stmt = transition_select_fields key_cids pats state_ids None in 
  let parse_states = states in
  transition_stmt, parse_states
;;

let generate_egress_parser block_id tds =
  let transition_stmt, ev_parse_states = eventset_parse_tree tds in 
  let start_state = decl (DParseState
    { id=start_state_id; 
      body=sseq ([
        extract (Cid.id eg_intr_md);
        extract (eth_arg);
        extract (single_ev_arg);
        extract (multi_ev_arg);
        transition_stmt
        ]
      );
    })
  in
  dparse block_id egress_parser_params (start_state::ev_parse_states)
;;


(*** egress deparse ***)

let eg_dprsr_md_t = id "egress_intrinsic_metadata_for_deparser_t"
let eg_dprsr_md_arg = id "eg_dprsr_md"

let egress_deparser_params = [
  param (tstruct pkt_out_t) pkt_out_arg;
  inoutparam (tstruct hdr_t_id) hdr_t_arg;
  inparam (tstruct md_t_id) md_t_arg;
  inparam (tstruct eg_dprsr_md_t) eg_dprsr_md_arg
] ;; 

(* emit everything *)
let generate_egress_deparse block_id tds =
  let m = main tds in 
  let _ = m in 
(*   let emit_calls = 
    [eth_arg; single_ev_arg] (* eth and single_ev *)
    @(List.map handler_struct_arg (* events *)
      (m.hdl_enum |> List.split |> fst))
    |> List.map (fun cid -> [evar cid])
    |> List.map (ecall_emit pkt_out_param)
    |> List.map (sunit)
  in
 *)  let emit_calls = [sunit (ecall_emit pkt_out_param ([Cid.id hdr_t_arg|> evar]) )] in 

  decl (DDeparse{
    id=block_id;
    params=egress_deparser_params;
    decls = [];
    body = Some(sseq emit_calls);
  })
;;


