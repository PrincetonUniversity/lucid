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
  let counter_size = IVar (QVar (Id.fresh "a")) in
  let counter_eff = FVar (QVar (Id.fresh "eff")) in
  let start_eff = FVar (QVar (Id.fresh "eff")) in
  { arg_tys = [ty @@ TInt counter_size]
  ; ret_ty = ty_eff (TName (t_id, [counter_size], true)) counter_eff
  ; start_eff
  ; end_eff = start_eff
  ; constraints = ref []
  }
;;

(* Counter.add *)
let counter_add_name = "add"
let counter_add_id = Id.create counter_add_name
let counter_add_cid = Cid.create_ids [counter_id; counter_add_id]
let counter_add_error msg = counter_error counter_add_name msg

let counter_add_ty =
  let counter_size = IVar (QVar (Id.fresh "a")) in
  let counter_eff = FVar (QVar (Id.fresh "eff")) in
  let start_eff = FVar (QVar (Id.fresh "eff")) in
  ty
  @@ TFun
       { arg_tys =
           [ ty_eff (TName (t_id, [counter_size], true)) counter_eff
           ; ty @@ TInt counter_size ]
       ; ret_ty = ty @@ TInt counter_size
       ; start_eff
       ; end_eff = FSucc counter_eff
       ; constraints = ref [CLeq (start_eff, counter_eff)]
       }
;;

let dummy_memop = State.F (fun _ _ args -> extract_ival (List.hd args))
let setop = State.F (fun _ _ args -> extract_ival (List.nth args 1))
let dummy_int = State.V (CoreSyntax.vinteger (Integer.of_int 0))

let counter_add_fun nst swid args =
  let open State in
  let open CoreSyntax in
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

let signature =
  let sz = IVar (QVar (Id.fresh "sz")) in
  ( module_id
  , [Cid.last_id t_id, [sz], TName (t_id, [sz], true) |> ty]
  , defs
  , constructors )
;;
