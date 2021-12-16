(* Translate events into LLSyntax
    - generate expressions
    - event combinators (not implemented yet) *)
open CoreSyntax
open LLSyntax
open InterpHelpers
open LLContext
open LLOp
open LLConstants

(* "interface" for this module -- the functions implemented here. *)
let module_name = "Event"
let module_id = Id.create module_name
let cid_of_fname name_str = Cid.create_ids [module_id; Id.create name_str]

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

(* from LLOp *)


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

(*   let hidden_fields_of_event evrec = 
    let fields = CL.map 
    (fun fld -> Cid.concat evrec.event_struct_instance fld)
    []
 *)
  (* set the hidden aruguments the event 
     instance defined in ev_rec. *)



  let event_hidden_args_instrs ev_rec =
    let hidden_ev_fields = CL.map 
      (fun fld -> Cid.concat ev_rec.event_struct_instance fld)
      (ev_rec.hidden_fields |> CL.split |> fst)
    in 
    let rhs_exps = CL.map 
      Generators.int_expr  
      [ev_rec.event_iid; 0; 0] 
    in 
      Generators.oper_assign_instrs 
        hidden_ev_fields 
        rhs_exps 
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
  (* left off here: get rid of this, 
     then update runtime_meta_init_instrs. *)
  let event_meta_init_instrs evrec = 
    (event_hidden_args_instrs evrec)
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

let generate_port (args : codegenInput) : codegenOutput =
  (* id of the callee handle *)
  let hdl_id = Option.get args.hdl_id in
  let (port_exp, event_exp) = match args.args with 
    | [port_exp; event_exp] -> (port_exp, event_exp) 
    | _ -> error "[generate_port] invalid arguments."
  in 
  let port = int_from_const_exp port_exp in 
  let _ = port in 
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
  (* the vector of instructions to 
     set fields for this generate call. *)
  let ivec = 
    (event_visible_args_instrs hdl_id ev_rec ev_args)
  in 

  (* event_meta_instrs @ ivec @ runtime_instrs in *)
  (* return a declaration of an alu with this vector of instructions *)
  let alu_id = Cid.compound (Id.create "generate_alu") (Option.get args.basename) in
  let alu_obj = IS.new_dinstr alu_id ivec in
  { names = [alu_id]; objs = [alu_obj] }
;;

let delay_event (args : codegenInput) : codegenOutput =
  let _ = args in
  t_info "delay_event called";
  ignore @@ exit 1;
  { names = []; objs = [] }
;;
