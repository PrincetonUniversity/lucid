(* Interpretation of stateful counters. *)
open Batteries
open Syntax
open InterpState

(* Generic Counter defs *)
let counter_name = "Counter"
let counter_id = Id.create counter_name

let counter_error fun_name msg =
  error (counter_name ^ ": " ^ fun_name ^ ": " ^ msg)
;;

let module_id = counter_id
let t_id = Cid.create_ids [counter_id; Id.create "t"]
let sizes_labels = [Id.create "dummy_sz"], []

(* Constructor *)
let counter_create_id = Cid.create_ids [counter_id; Id.create "create"]

let counter_create_sig =
  let sz_id = Id.fresh "sz" in
  t_id, [sz_id], [TInt (IVar (QVar sz_id))]
;;

(* Counter.add *)
let counter_add_name = "add"
let counter_add_id = Id.create counter_add_name
let counter_add_cid = Cid.create_ids [counter_id; counter_add_id]
let counter_add_error msg = counter_error counter_add_name msg

let counter_add_ty =
  let size = IVar (QVar (Id.fresh "sz")) in
  let ref_eff = FVar (QVar (Id.fresh "eff")) in
  let start_eff = FVar (QVar (Id.fresh "eff")) in
  TFun
    { arg_tys = [TGlobal ((t_id, [size]), ref_eff); TInt size]
    ; ret_ty = TInt size
    ; start_eff
    ; end_eff = FSucc ref_eff
    ; constraints = ref [CLeq (start_eff, ref_eff)]
    }
;;

let dummy_memop = State.F (fun _ _ args -> extract_ival (List.hd args))
let setop = State.F (fun _ _ args -> extract_ival (List.nth args 1))
let dummy_int = State.V (vinteger (Integer.of_int 0))

let counter_add_fun nst swid args =
  let open State in
  match args with
  | [V { v = VGlobal stage }; V { v = VInt addval }] ->
    let get_f arg = vinteger arg in
    let set_f arg = Integer.add arg addval in
    update_switch swid stage 0 get_f set_f nst
  | _ ->
    counter_add_error "Incorrect number or type of arguments to Counter.add"
;;

let constructors = [counter_create_id, counter_create_sig]

let defs : State.global_fun list =
  [{ cid = counter_add_cid; body = counter_add_fun; ty = counter_add_ty }]
;;
