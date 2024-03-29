(* A function interpreter to call individual functions in a program. 
   1. call init_function ds on a midend program to initialize a single pipeline and 
      return the initial state and name / params of the function tagged with "@main"
   2. call run_function state fcn_id fcn_params int_args to run fcn_id(int_args) and get its 
      return values as ints *)
open Batteries
open Yojson.Basic
open Syntax
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

let init_function ds = 
  let pp, ds = Preprocess.preprocess ds in
  let nst = initial_state pp (InterpSpec.empty_spec) in
  let _ = InterpCore.process_decls nst ds in
  let main_id, main_params = List.find_map 
    (fun (decl:C.decl) -> 
      match decl.d with
      | C.DFun(id, _, (params, _)) -> (
        match decl.dpragma with 
        | Some(p) -> (
          if (Pragma.exists_sprag "main" [] [p])
            then Some(id, params)
            else None
        )
        | _ -> None)
      | _ -> None)
    ds
  in
  nst, main_id, main_params
;;
let run_function nst fcn_id fcn_params args =   
  let args = List.map2 
    (fun int_arg (_, ty)  -> C.vint_exp_ty int_arg ty)
    args
    fcn_params
  in
  let main_call = C.call(Cid.id fcn_id) args (C.ty C.TBool) in
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
