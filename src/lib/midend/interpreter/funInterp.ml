(* A function interpreter to call individual functions in a program. 
   1. call init_function ds on a midend program to initialize a single pipeline and 
      return the initial state and name / params of the function tagged with "@main"
   2. call run_function state fcn_id fcn_params int_args to run fcn_id(int_args) and get its 
      return values as ints *)
open Batteries
open Yojson.Basic
(* open Syntax *)
open CoreSyntax
open InterpSyntax
open InterpState
open InterpControl
open CoreSyntaxGlobalDirectory

let initial_state (pp : Preprocess.t) (spec : InterpSpec.t) =
  let nst =
    { (State.create spec.config) with
      event_sorts = Env.map (fun (a, _, _) -> a) pp.events
    ; event_signatures  = List.fold_left
        (fun acc (evid, event) -> 
          let (_, num_opt, arg_tys) = event in
          (match num_opt with 
          | None -> print_endline ("event has no number: " ^ (Cid.to_string evid)); 
          | _ -> ());
          let num = Option.get num_opt in
          IntMap.add num (evid, arg_tys) acc)
        IntMap.empty
        (Env.bindings pp.events)
    ; switches = Array.init 1 
      ((InterpSwitch.create spec.config State.network_utils))
    ; links = State.empty_topology 1 [1]
    }
  in
  (* Add builtins *)
  List.iter
    (fun f -> State.add_global_function f nst)
    (System.defs @ Events.defs @ Counters.defs @ Arrays.defs @ PairArrays.defs @Payloads.defs);
  (* Add externs for recirc port on switch 0 (only switch here) *)
  let recirc_id = Cid.id (Builtins.recirc_id) in
  State.add_global 0 recirc_id (V (C.vint 1 9)) nst;
  (* Add foreign functions *)
  Env.iter
    (fun cid fval ->
      Array.iteri (fun i _ -> State.add_global i cid fval nst) nst.switches)
    spec.extern_funs;
  nst
;;

type main_params = 
  (* the parameters of main can be either a standard list of params or an event *)
  | Params of params 
  | Event of Cid.t * params

type fctx = {
  nst : State.network_state;
  main_id : Cid.t;
  main_params : main_params;
}

(* initialize a single pipeline and return the initial state 
   and name / params of the function tagged with "@main" *)
let init_function ds = 
  let pp, ds = Preprocess.preprocess ds in
  let nst = initial_state pp (InterpSpec.empty_spec) in
  let _ = InterpCore.process_decls nst ds in
  (* find the main function and the event constructors *)
  let main_sigs, event_constrs = List.fold_left 
    (fun (main_fcns, event_constrs) decl -> 
      match decl.d, decl.dpragma with 
      | C.DFun(main_id, _, (main_params, _)), Some(prag) -> (
        match (Pragma.find_sprag "main" [prag]) with 
        | Some((_, pragma_args)) -> ((main_id, main_params, pragma_args)::main_fcns, event_constrs)
        | _ -> (main_fcns, event_constrs))
      | C.DEvent(ctr), _ -> (main_fcns, ctr::event_constrs)
      | _ -> (main_fcns, event_constrs))
    ([], [])
    ds
  in
  (* figure out the main id and parameters *)
  match main_sigs with 
  | [] -> failwith "cannot continue -- no main function"
  | [(main_id, main_params, pragma_args)] -> (
    match pragma_args with 
      (* no pragma args --> the function is just annotated with @main, its not an event function *)
      | [] -> {nst; main_id=(Cid.id (main_id)); main_params = Params(main_params)}
      (* pragma args --> the function is annotated with specific in and out event constructors *)
      | in_event_name::_::[] -> ( 
        (* find the in event constructor *)
        match (List.fold_left 
          (fun main_params_opt (ev_id, _, _, ev_params) -> 
            if (Id.name ev_id) = in_event_name 
            then Some({nst; main_id=(Cid.id (main_id)); main_params = Event((Cid.id ev_id), ev_params)})
            else main_params_opt)
          None
          event_constrs) with 
          | Some(main_params) -> main_params
          | None -> failwith ("cannot continue -- could not find event constructor for " ^ in_event_name))
    | _ -> failwith "wrong arguments to @main pragma"
  )
  | _ -> failwith "cannot continue -- multiple main functions"
;;

let int_args_to_exp int_arg ty = 
  C.vint_exp_ty int_arg ty
;;

(* pack a flat list of arguments into the appropriate structure. 
   returns arg expressions and unused inputs list *)
let rec pack_args (param_tys : (C.ty) list) (int_args : int list) : C.value list * int list = 
  match param_tys with 
  | (ty)::rest_params -> (
    match ty.raw_ty with 
    | TTuple(tys) -> 
      let inner_tys = List.map C.ty tys in
      (* fill the inner params first *)
      if ((List.length inner_tys) > (List.length int_args)) then error "failure: not enough arguments";
      let inner_values, rest_int_args = pack_args inner_tys int_args in
      let inner_vs = List.map (fun (value : C.value) -> value.v) inner_values in
      let arg_value = C.value (VTuple(inner_vs)) in
      (* recurse on the rest *)
      let rest_values, unused_args = pack_args rest_params rest_int_args in
      arg_value::rest_values, unused_args
    | _ -> 
      if (List.length int_args = 0) then error "failure: not enough arguments";
      let arg_value = C.vint (List.hd int_args) (C.ty_to_size ty) in
      let rest_values, unused_args = pack_args rest_params (List.tl int_args) in 
      (arg_value::rest_values), unused_args
  )
  | [] -> [], []
;;

let check_params_len fcn_params int_args = 
  if ((List.length int_args) <> (List.length fcn_params))
  then (
    Printf.printf "Expected %d arguments, got %d\n" (List.length fcn_params) (List.length int_args);
    exit 1;)
;;
let run_function init_result int_args = 
  let nst = init_result.nst in
  let fcn_id = init_result.main_id in
  (* processing is slightly different for functions and event functions *)
  let main_call = match init_result.main_params with
  | Params(params) -> (* regular function: pack args, call, decode return value *)
    check_params_len params int_args;
    let param_tys = List.map (fun (_, ty) -> ty) params in
    let args_values, unused_args = pack_args param_tys int_args in
    let args = List.map C.value_to_exp args_values in
    if (List.length unused_args > 0) then error "failure: too many arguments";
    C.call fcn_id args (C.ty C.TBool)
  | Event(ev_id, params) -> (* event function: wrap input in approprate event id *)
    check_params_len params int_args;
    let param_tys = List.map (fun (_, ty) -> ty) params in
    let args_values, unused_args = pack_args param_tys int_args in
    let args = List.map C.value_to_exp args_values in
    if (List.length unused_args > 0) then error "failure: too many arguments";
    let main_arg = C.call ev_id args (C.tevent) in 
    C.call fcn_id [main_arg] (C.ty C.TBool)
  in  
  let res = InterpCore.interp_exp nst 0 Env.empty main_call |> extract_ival in
  let rec decode_v (v : C.v) = match v with 
    | C.VInt(z) -> [Integer.to_int z]
    | C.VBool(b) -> [if b then 1 else 0]
    | C.VTuple(vs) -> List.flatten (List.map decode_v vs)
    | C.VEvent({data}) -> List.flatten (List.map (fun (v: C.value) -> decode_v v.v) (data: C.value list))
    | _ -> error "unsupported return type"
  in
  (decode_v (res.v))
;;
