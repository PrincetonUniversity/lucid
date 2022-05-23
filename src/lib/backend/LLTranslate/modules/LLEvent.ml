(* Translate events into LLSyntax
    - generate expressions
    - event combinators (not implemented yet) *)
open CoreSyntax
open LLSyntax
open InterpHelpers
open LLContext
open LLOp
open LLConstants
open MiscUtils

(* "interface" for this module -- the functions implemented here. *)
let module_name = "Event"
let module_id = Id.create module_name
let cid_of_fname name_str = Cid.create_ids [module_id; Id.create name_str]


let tuple_of_list l = 
  match l with 
  | [a; b] -> (a, b)
  | _ -> error "[tuple_of_list] list does not have two elements."
;;

(*** Event declaration translator ***)


(* code generators for events *)
module TranslateEvents = struct

  (* translate the ith event declaration into LL code,
     and save details about the event in the LL context.
     This should be the ONLY FUNCTION that writes the event records to 
   context. *)
  let translate (ll_decls, last_eviid) dec =
    match dec.d with
    | DEvent (evid, ev_sort, params) ->
      let cur_eviid = last_eviid + 1 in (* start event ids at 1 *)
      (* all the details about the event that the translator
         needs, in one context record. *)
      let erec =
        { event_id = evid
        ; field_defs = vardefs_from_params params
        ; hidden_fields = hidden_event_fields
        ; event_iid = cur_eviid 
        ; hdl_param_ids = [] (* filled in by DHandler match *)
        ; event_sort = ev_sort
        ; event_struct = TofinoStructs.structname_from_evid evid
        ; event_struct_instance = TofinoStructs.full_struct_from_ev evid ev_sort
        ; event_generated_flag =
            Cid.concat event_out_flags_instance (Cid.id evid)
        }
      in
      (* add the event to the context. *)
      ctx_add_erec erec;

      (* generate IR code for the event *)
      let all_event_fields =
        (* hidden params and user-defined params *)
        erec.hidden_fields @ erec.field_defs
      in
      let struct_ty =
        match erec.event_sort with
        | EBackground ->
          IS.SHeader (* backround events get serialized to packet *)
        | EEntry _ | EExit -> IS.SMeta
        (* entry and exit events get serialized to metadata... 
            kind of backwards seeming. *)
      in
      (* the struct is the datatype *)
      let ev_struct_decl =
        IS.new_structdef erec.event_struct struct_ty all_event_fields
      in
      (* each event has one global instance of the struct. *)
      let ev_struct_inst =
        IS.new_struct erec.event_struct () erec.event_struct_instance
      in
      (* the enum lets the P4 program set the event type. *)
      let ev_enum =
        IS.new_public_constdef
          (TofinoStructs.defname_from_evid erec.event_id)
          event_id_width
          erec.event_iid
      in
      ll_decls @ [ev_struct_decl; ev_struct_inst; ev_enum], cur_eviid
    (* record the mapping from event to handler in the context *)
    | DHandler (hdl_id, (params, _)) ->
      ctx_set_hdl_param_ids (Cid.id hdl_id) (CL.split params |> fst);
      ll_decls, last_eviid (* nothing to generate, just updating context *)
    (* all other decls -- do nothing *)
    | _ -> ll_decls, last_eviid
  ;;

  (* create the struct and instance of the event_generated bitvector *)
  let event_triggered_bv () =
    let to_generate_flag_field ev =
      Cid.last_id ev.event_generated_flag |> Cid.id, 1
    in
    let field_defs = ctx_get_event_recs () |> CL.map to_generate_flag_field in
    [ IS.new_header_structdef event_out_flags_struct field_defs
    ; IS.new_struct event_out_flags_struct () event_out_flags_instance ]
  ;;

  (* translate all the event declarations and fill the context. *)
  let translate_events ds =
    (* generate headers and header instances for events *)
    let event_decls = CL.fold_left translate ([], 0) ds |> fst in
    (* generate the event_out flag struct *)
    let event_out_bv_decls = event_triggered_bv () in
    event_decls @ [footer_struct; footer_instance] @ event_out_bv_decls
  ;;

  (*** parse generator for background events ***)
  (* note: this reads event records from context! 
    so all events must be translated before it can 
    be called. *)
  let parsetree_from_events _ =
    let parse_start_cid = Cid.create ["start"] in 
    let parse_end_cid = Cid.create ["finish"] in 
    (* get the name of the event's struct *)
    let evrec_hdrvar evrec =
      TofinoStructs.full_struct_from_ev evrec.event_id EBackground
    in 
    (* get the name of the event's parse node *)
    let evrec_pnodecid pos evrec =
      Cid.id
        (Id.prepend_string
           ("event_" ^ string_of_int pos ^ "_parse_")
           evrec.event_id)
    in
    (* the selector node id for a layer of the parser *)
    let pos_selectorcid pos = 
      Cid.create ["selector_" ^ string_of_int pos] 
    in 
    (* create a parser to extract the evrec
       from the i'th event in the lucid header. *)
    let evrec_parse_node max_layers layer evrec =
      let parse_instr = IS.new_PStruct (evrec_hdrvar evrec) in
      let next_instr =
        match max_layers - 1 = layer with
        | true -> IS.new_PNext (Some parse_end_cid)
        | false -> IS.new_PNext (Some (pos_selectorcid (layer + 1)))
      in
      IS.new_parse_node (evrec_pnodecid layer evrec) [parse_instr] next_instr
    in
    (* create the ith selector node *)
    let pos_selector layer evrecs =
      let selector_cid =
        match layer with
        | 0 -> parse_start_cid
        | _ -> pos_selectorcid layer
      in
      let peek_target_decl_cid, peek_target_cid =
        match layer with
        | 0 -> handle_selector_name, handle_selector_name
        | _ -> Cid.create ["bit<8> tmp"], Cid.create ["tmp"]
      in
      let extract_eventType =
        IS.new_PPeek peek_target_decl_cid event_id_width
      in
      let evrec_branch evrec =
        IS.new_SConst_branch evrec.event_iid (Some (evrec_pnodecid layer evrec))
      in
      let evrec_branches =
        IS.new_SConst_branch 0 (Some parse_end_cid)
        :: CL.map evrec_branch evrecs
      in
      IS.new_parse_node
        selector_cid
        [extract_eventType]
        (IS.new_PSelect peek_target_cid evrec_branches)
    in
    (* create all nodes for a layer *)
    let layer_nodes max_layers evrecs layer =
      pos_selector layer evrecs
      :: CL.map (evrec_parse_node max_layers layer) evrecs
    in
    (* create num_evs selectors and num_ev layers of parse nodes *)
    let evrecs =
      ctx_get_event_recs ()
      |> CL.filter (fun evr ->
             match evr.event_sort with
             | EBackground -> true
             | _ -> false)
    in
    let nodes =
      CL.map
        (layer_nodes max_generated_events evrecs)
        (range 0 max_generated_events)
      |> CL.flatten
    in
    (* end state just parses a single 8 bit header and exits (no next instr) *)
    let end_state =
      let parse_instr =
        IS.new_PStruct (TofinoStructs.qualify_struct footer IS.SHeader)
      in
      let flags_parse_instr = IS.new_PStruct event_out_flags_instance in
      let next_instr = IS.new_PNext None in
      IS.new_parse_node
        parse_end_cid
        [parse_instr; flags_parse_instr]
        next_instr
    in
    (*     print_endline ("HERE");
    printf "number of node states: %i\n" (CL.length nodes);
    exit 1;
 *)
    IS.new_ParseTree lucid_parser_name (nodes @ [end_state])
  ;;
end






(*   
  new generate functions strategy:
    generating an entry or exit event is an in-place modification of the current 
    packet header. 
    generating a "control" event is a copy. It always recirculates. 

    Implemented now:
    generate(control event);
    generate_port(portexp, packet event);

*)


(*** temporary solution to event combinator inlining ***)
let unpack_ecall exp =
  match exp.e with
  | ECall (fcn_id, args) -> fcn_id, args
  | _ -> error "[unpack_ecall] not a call!"
;;

let unpack_dinstr d =
  match d with
  | IS.InstrVec (oid, ivec) -> oid, ivec
  | _ -> error "[unpack dinstr] not an instruction vec declaration"
;;

let replace_in_list xs i x =
  let map_f c_i c_x =
    match c_i == i with
    | true -> x
    | false -> c_x
  in
  CL.mapi map_f xs
;;

let replace_rhs iassign new_rhs =
  match iassign with
  | IAssign (lhs, _) -> IAssign (lhs, new_rhs)
  | _ -> error "[replace_rhs] not an IAssign"
;;

  (* set the visible arguments of the event, 
     its fields defined in ev_rec. *)
  let event_visible_args_instrs hdl_id ev_rec ev_args = 
    let visible_ev_fields = CL.map 
      (fun fld -> Cid.concat ev_rec.event_struct_instance fld)
      (ev_rec.field_defs |> CL.split |> fst)
    in 
    let rhs_exps = CL.map (eoper_from_immediate hdl_id) ev_args in 
    Generators.oper_assign_instrs 
      visible_ev_fields 
      rhs_exps 
  ;;
  (* set the hidden fields of the ev_rec *)
  let event_hidden_args_instrs ev_rec args =
    let hidden_ev_fields = CL.map 
      (fun fld -> Cid.concat ev_rec.event_struct_instance fld)
      (ev_rec.hidden_fields |> CL.split |> fst)
    in 
    Generators.oper_assign_instrs 
      hidden_ev_fields 
      args 
  ;;

  (* instructions to set headers to valid. *)
  let event_other_setup_instrs evrec = 
    let ev_struct_id = evrec.event_struct_instance in
    (* background events are carried in headers that need to be set to valid.
         Background events must also be sure to set up the footer. *)
    match evrec.event_sort with
    | EBackground ->
      [ GS.validate_instr ev_struct_id
      ; GS.validate_instr (TofinoStructs.qualify_struct footer IS.SHeader)
      ; GS.int_assign_instr
          (TofinoStructs.qualify_struct
             (Cid.concat footer (CL.hd footer_fields |> fst))
             IS.SHeader)
          0 ]
    | _ -> []
  ;;

  let event_meta_init_instrs evrec = 
    let hidden_args = CL.map 
      Generators.int_expr  
      [evrec.event_iid; 0; 0] 
    in 
    (event_hidden_args_instrs evrec hidden_args)
    @
    (event_other_setup_instrs evrec)
  ;;
  let runtime_meta_init_instrs evrec =
    (* todo: want a cleaner way to access the elements of the runtime metadata struct. *)
    match evrec.event_sort with
    | EBackground ->
      let ev_ct_cid =
        Cid.create [md_instance_prefix; dpt_meta_str; events_count_str]
      in
      [ (* md.dptMeta.nextEvent = i:int *)
        GS.int_assign_instr
          (Cid.create [md_instance_prefix; dpt_meta_str; next_event_str])
          evrec.event_iid
        (* md.dptMeta.eventCt += 1 *)
      ; GS.incr_assign_instr ev_ct_cid ev_ct_cid 1
        (* md.eventGeneratedFlags.<eventname> = 1 *)
      ; GS.validate_instr event_out_flags_instance
      ; GS.int_assign_instr evrec.event_generated_flag 1 ]
    | EEntry _ | EExit ->
      [ (* md.dptMeta.exitEvent = i:int *)
        GS.int_assign_instr
          (Cid.create [md_instance_prefix; dpt_meta_str; exit_event_str])
          evrec.event_iid ]
  ;;

(* generate an alu instruction from a call to an event. *)
let from_event_call hdl_id alu_basename ev_id ev_args =
  !dprint_endline
    ("[from_event_instantiation] event id: " ^ Cid.to_string ev_id);
  !dprint_endline "[from_event_instantiation] event args: ";
  let iter_f ev_arg = !dprint_endline (Printing.exp_to_string ev_arg) in
  CL.iter iter_f ev_args;
  (* get a list of qualified out struct field parameters *)
  (* generate alu instructions of the form: field_param := ev_arg *)
  (* since the out fields are written, the variable references must be
     lmids, else dataflow analysis will fail. *)
  let to_lmid (mid : Cid.t) : IS.lmid = mid in
  let out_struct_fields =
    TofinoStructs.qual_out_fieldnames_of_event (Cid.to_id ev_id)
    |> CL.map to_lmid
  in
  let alu_rhs_exps = CL.map (eoper_from_immediate hdl_id) ev_args in
  let to_ass_f (lhs, rhs) = IS.IAssign (lhs, rhs) in

  let (ivec : IS.instrVec) =
    CL.map to_ass_f (CL.combine out_struct_fields alu_rhs_exps)
  in
  let ev_rec = ctx_find_eventrec (ev_id) in 
  (* add instructions to set hidden fields in event header, e.g., event name *)
  let event_meta_instrs = event_meta_init_instrs ev_rec in
  (* instructions to set non-serialized variables in runtime *)
  let runtime_instrs = runtime_meta_init_instrs ev_rec in
  let ivec = event_meta_instrs @ ivec @ runtime_instrs in
  (* return a declaration of an alu with this vector of instructions *)
  let alu_id = Cid.compound (Id.create "generate_alu") alu_basename in
  let alu_obj = IS.new_dinstr alu_id ivec in
  alu_id, alu_obj
;;

(* end from LLOp *)

(* start temporary inlined-only solution for event combinators *)
(* adjust an instruction that populates an event instance
so that it also sets a delay *)
let event_delay_cid = cid_of_fname "delay"

let inlined_event_delay hdl_id alu_obj args =
  let oid, ivec = unpack_dinstr alu_obj in
  let delay = new_eoper (oper_from_immediate hdl_id (CL.hd args)) in
  match ivec with
  | id_set :: mc_set :: loc_set :: delay_set :: fields_set ->
    let delay_set = replace_rhs delay_set delay in
    new_dinstr oid (id_set :: mc_set :: loc_set :: delay_set :: fields_set)
  | _ -> error "[inlined_event_delay] unexpected ivec length"
;;

let event_combinator_inliners =
  [ event_delay_cid, inlined_event_delay ]
;;

(* generate an alu object from a generate statement that has
an arbitrary sequence of event combinators inlined.
for example: generate Event.delay(foo(x), 100); *)
(* This is probably depreciated. If not, it should be. *)
let rec generate_modified_self hdl_id alu_name fcn_id fcn_args =
  match Cid.exists event_combinator_inliners fcn_id with
  | true ->
    (* recurse on inner function. *)
    let inner_fcn_id, inner_args = unpack_ecall (CL.hd fcn_args) in
    let alu_id, alu_obj =
      generate_modified_self hdl_id alu_name inner_fcn_id inner_args
    in
    (* next, update the alu instruction based on the combinator. *)
    let inliner_f = Cid.lookup event_combinator_inliners fcn_id in
    let updated_alu_obj = inliner_f hdl_id alu_obj (CL.tl fcn_args) in
    alu_id, updated_alu_obj
  | false -> from_event_call hdl_id alu_name fcn_id fcn_args
;;

(*** end temporary event combinator inlining solution ***)

(* id of the generate function in the backend *)
let generate_self (args : codegenInput) : codegenOutput =
  (* id of the callee handle *)
  let hdl_id = Option.get args.hdl_id in
  (* only process the first argument -- only supports 
     the basic generate *)
  match (CL.hd args.args).e with
  | ECall (ev_id, ev_args) ->
    (* if ev_id is an event combinator, we:
          1) recurse on the first argument;
          2) then call the combinator function.
        *)
    let oid, obj =
      match Cid.exists event_combinator_inliners ev_id with
      | true ->
        generate_modified_self hdl_id (Option.get args.basename) ev_id ev_args
      | false ->
        from_event_call
          hdl_id
          (Option.get args.basename)
          ev_id
          ev_args
    in
    { names = [oid]; objs = [obj] }
  | EVar _ ->
    error "backend does not support generating previously declared events\
      -- all events must be declared inline."
    (* { names = []; objs = [] }     *)
    (* generating pre-defined events it not supported yet. *)
  | _ -> error "unsupported argument to generate."
;;

(* instructions to set user-defined params, common 
   hidden params, and common lucid runtime params. *)
let common_generate_port_instrs hdl_id ev_rec ev_args = 
  (* set user-defined parameters. *)  
  let user_ivec = 
    (event_visible_args_instrs hdl_id ev_rec ev_args)
  in 
  (* set hidden parameters. *)
  let hidden_field_args = 
  [
    Generators.int_expr ev_rec.event_iid;
    Generators.int_expr 0;
    Generators.int_expr 0
  ] 
  in   let hidden_ivec = 
    event_hidden_args_instrs ev_rec hidden_field_args
  in 
  (* set internal Lucid metadata. *)
  (* out port and exit event ID. Exit event ID is set because right now, 
     we assume that generate_port is only used with a wire event. *)
  let internal_ivec = 
    [
      GS.int_assign_instr 
        exit_event_field 
        ev_rec.event_iid
    ]
  in 
  user_ivec@hidden_ivec@internal_ivec

;;

let rec_args_from_event_exp event_exp = 
  (* ge the event record and event args *)
  let ev_rec, ev_args = match (event_exp.e) with 
    | ECall (ev_cid, ev_args) ->
      let ev_rec = match ctx_find_eventrec_opt ev_cid with 
        | Some ev_rec -> ev_rec 
        | None -> error ("[generate_port] could not find \
              declaration of event: "^(Cid.to_string ev_cid))
      in 
      ev_rec, ev_args
    | EVar _ -> error "[generate_port] event variables \
      not supported"
    | _ -> error "[generate_port] unexpected expression form\
      for event argument."
  in 
  ev_rec, ev_args
;;

let generate_port (args : codegenInput) : codegenOutput =
  (* id of the callee handle *)
  let hdl_id = Option.get args.hdl_id in
  let (port_exp, event_exp) = match args.args with 
    | [port_exp; event_exp] -> (port_exp, event_exp) 
    | _ -> error "[generate_port] invalid arguments."
  in 
  (* ge the event record and event args *)
  let ev_rec, ev_args = rec_args_from_event_exp event_exp in 

  (* the instructions *)
  let ivec = common_generate_port_instrs hdl_id ev_rec ev_args
    @[
        GS.oper_assign_instr 
          packet_unicast_field
          (eoper_from_immediate hdl_id port_exp);
     ]
  in 
  (* return a declaration of an alu with this vector of instructions *)
  let alu_id = Cid.compound (Id.create "generate_port_alu") (Option.get args.basename) in
  let alu_obj = IS.new_dinstr alu_id ivec in
  { names = [alu_id]; objs = [alu_obj] }
;;

let generate_ports (args : codegenInput) : codegenOutput =
  let hdl_id = Option.get args.hdl_id in
  let (group_exp, event_exp) = tuple_of_list args.args in 
  let ev_rec, ev_args = rec_args_from_event_exp event_exp in 
  let set_mcid_instr, supporting_objs = match group_exp.e with 
  | EVar(group_var) ->
    GS.oper_assign_instr packet_multicast_field (GS.cid_expr group_var), []
  | EVal(value) ->
    let mcid_oper, mc_decls = TofinoAlu.ll_of_vgroup value in 
    GS.oper_assign_instr packet_multicast_field (GS.oper_expr mcid_oper), mc_decls
  | _ -> error "[generate_ports] first arg of generate ports must be a group value or variable."
  in 
  let clear_outport_instr =  GS.oper_assign_instr event_port_field (GS.int_expr 0) in 
  let ivec = 
    (common_generate_port_instrs hdl_id ev_rec ev_args)
    @[clear_outport_instr; set_mcid_instr]
  in 
  (* return a declaration of an alu with this vector of instructions, 
     and the supporting objects in case we had to create a new group. *)
  let alu_id = Cid.compound (Id.create "generate_ports_alu") (Option.get args.basename) in
  let alu_obj = IS.new_dinstr alu_id ivec in
  { names = [alu_id]; objs = alu_obj::supporting_objs }
;;



let delay_event (args : codegenInput) : codegenOutput =
  let _ = args in
  t_info "delay_event called";
  ignore @@ exit 1;
  { names = []; objs = [] }
;;
