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
let sizes = 1
let ty_args = 0
let global = true

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

let dummy_memop = InterpSyntax.F (fun _ _ args -> V(extract_ival (List.hd args)))
let setop = InterpSyntax.F (fun _ _ args -> V(extract_ival (List.nth args 1)))
let dummy_int = InterpSyntax.V (CoreSyntax.vinteger (Integer.of_int 0))

let counter_add_fun nst swid args =
  let open State in
  let open InterpSyntax in
  let open CoreSyntax in
  match args with
  | [V { v = VGlobal (_, stage) }; V { v = VInt addval }] ->
    let get_f arg = vinteger arg in
    let set_f arg = Integer.add arg addval in
    V(Pipeline.update ~stage ~idx:0 ~getop:get_f ~setop:set_f (sw nst swid).pipeline)
  | _ ->
    counter_add_error "Incorrect number or type of arguments to Counter.add"
;;

let constructors = [counter_create_id, counter_create_sig]

let defs : State.global_fun list =
  [{ cid = counter_add_cid; body = counter_add_fun; ty = counter_add_ty }]
;;

let signature =
  let sz = IVar (QVar (Id.fresh "sz")) in
  LibraryInterface.tup_to_sigty
  ( module_id
  , [Cid.last_id t_id, [sz], TName (t_id, [sz], true) |> ty]
  , defs
  , constructors )
;;
