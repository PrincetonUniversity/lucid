(* Tofino events builtins
  - generate expression
  - event combinators *)
open Syntax
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

let event_sslocate_cid = cid_of_fname "sslocate"

let inlined_event_sslocate hdl_id alu_obj args =
  let oid, ivec = unpack_dinstr alu_obj in
  let loc = new_eoper (oper_from_immediate hdl_id (CL.hd args)) in
  match ivec with
  | id_set :: mc_set :: loc_set :: delay_set :: fields_set ->
    let loc_set = replace_rhs loc_set loc in
    new_dinstr oid (id_set :: mc_set :: loc_set :: delay_set :: fields_set)
  | _ -> error "[inlined_event_ssloc] unexpected ivec length"
;;

let event_smlocate_cid = cid_of_fname "smlocate"

let inlined_event_smlocate hdl_id alu_obj args =
  let oid, ivec = unpack_dinstr alu_obj in
  let loc = new_eoper (oper_from_immediate hdl_id (CL.hd args)) in
  let mc = new_expr_of_int 1 in
  match ivec with
  | id_set :: mc_set :: loc_set :: delay_set :: fields_set ->
    let loc_set = replace_rhs loc_set loc in
    let mc_set = replace_rhs mc_set mc in
    new_dinstr oid (id_set :: mc_set :: loc_set :: delay_set :: fields_set)
  | _ -> error "[inlined_event_ssloc] unexpected ivec length"
;;

let event_combinator_inliners =
  [ event_delay_cid, inlined_event_delay
  ; event_sslocate_cid, inlined_event_sslocate
  ; event_smlocate_cid, inlined_event_smlocate ]
;;

(* generate an alu object from a generate statement that has
an arbitrary sequence of event combinators inlined.
for example: generate Event.delay(foo(x), 100); *)

let rec generate_modified_event hdl_id alu_name fcn_id fcn_args =
  match Cid.exists event_combinator_inliners fcn_id with
  | true ->
    (* recurse on inner function. *)
    let inner_fcn_id, inner_args = unpack_ecall (CL.hd fcn_args) in
    let alu_id, alu_obj =
      generate_modified_event hdl_id alu_name inner_fcn_id inner_args
    in
    (* next, update the alu instruction based on the combinator. *)
    let inliner_f = Cid.lookup event_combinator_inliners fcn_id in
    let updated_alu_obj = inliner_f hdl_id alu_obj (CL.tl fcn_args) in
    alu_id, updated_alu_obj
  | false -> TofinoAlu.from_event_instantiation hdl_id alu_name fcn_id fcn_args
;;

(*** end temporary event combinator inlining solution ***)

let generate_event (args : codegenInput) : codegenOutput =
  let hdl_id = Option.get args.hdl_id in
  match (CL.hd args.args).e with
  | ECall (ev_id, ev_args) ->
    (* if ev_id is an event combinator, we:
          1) recurse on the first argument;
          2) then call the combinator function.
        *)
    let oid, obj =
      match Cid.exists event_combinator_inliners ev_id with
      | true ->
        generate_modified_event hdl_id (Option.get args.basename) ev_id ev_args
      | false ->
        TofinoAlu.from_event_instantiation
          hdl_id
          (Option.get args.basename)
          ev_id
          ev_args
    in
    t_info "returning generated event.";
    { names = [oid]; objs = [obj] }
  | EVar _ ->
    t_info "generating an event from a variable.";
    { names = []; objs = [] }
    (* this is a generate statement for an event that has effectively already been generated. *)
    (* may need to add an is_valid type statement here. *)
  | _ -> error "unsupported argument to generate."
;;

let delay_event (args : codegenInput) : codegenOutput =
  let _ = args in
  t_info "delay_event called";
  ignore @@ exit 1;
  { names = []; objs = [] }
;;
