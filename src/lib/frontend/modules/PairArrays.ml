(* Interpretation of stateful pairarrays. *)
open Batteries
open Syntax
open InterpState

(* Generic PairArray defs *)
let pairarray_name = "PairArray"
let pairarray_id = Id.create pairarray_name

let pairarray_error fun_name msg =
  error (pairarray_name ^ ": " ^ fun_name ^ ": " ^ msg)
;;

let module_id = pairarray_id
let t_id = Cid.create_ids [pairarray_id; Id.create "t"]
let sizes = 1
let ty_args = 0
let global = true

(* Constructor *)
let pairarray_create_id = Cid.create_ids [pairarray_id; Id.create "create"]

let pairarray_create_sig =
  let pairarray_size = IVar (QVar (Id.fresh "a")) in
  let pairarray_eff = FVar (QVar (Id.fresh "eff")) in
  let start_eff = FVar (QVar (Id.fresh "eff")) in
  { arg_tys = [ty @@ TInt pairarray_size]
  ; ret_ty = ty_eff (TName (t_id, [pairarray_size], true)) pairarray_eff
  ; start_eff
  ; end_eff = start_eff
  ; constraints = ref []
  }
;;

(* PairArray.update *)

let pairarray_update_name = "update"
let pairarray_update_id = Id.create pairarray_update_name
let pairarray_update_cid = Cid.create_ids [pairarray_id; pairarray_update_id]
let pairarray_update_error msg = pairarray_error pairarray_update_name msg

let pairarray_update_ty =
  let a = IVar (QVar (Id.fresh "a")) in
  let arr_eff = FVar (QVar (Id.fresh "eff")) in
  let start_eff = FVar (QVar (Id.fresh "eff")) in
  ty
  @@ TFun
       { arg_tys =
           [ ty_eff (TName (t_id, [a], true)) arr_eff
           ; ty @@ TInt (IVar (QVar (Id.fresh "sz")))
           ; ty @@ TMemop (4, a)
           ; ty @@ TInt a
           ; ty @@ TInt a
           ; ty @@ TInt a ]
       ; ret_ty = ty @@ TInt a
       ; start_eff
       ; end_eff = FSucc arr_eff
       ; constraints = ref [CLeq (start_eff, arr_eff)]
       }
;;

let pairarray_update_fun nst swid args =
  let open InterpSyntax in
  let open State in 
  match args with
  | [V { v = VGlobal (_, stage) }; V { v = VInt idx }; F memop; arg1; arg2; default]
    ->
    let update_f mem1 mem2 =
      let args =
        [ V (CoreSyntax.vinteger mem1)
        ; V (CoreSyntax.vinteger mem2)
        ; arg1
        ; arg2
        ; default ]
      in
      let v = memop nst swid args |> extract_ival in
      match v.v with
      | VTuple [VInt n1; VInt n2; v3] -> n1, n2, { v with v = v3 }
      | _ -> failwith "array_update: Internal error"
    in
    V(Pipeline.update_complex ~stage ~idx:(Integer.to_int idx) ~memop:update_f (sw nst swid).pipeline)
  | _ -> pairarray_update_error "Incorrect number or type of arguments"
;;

let constructors = [pairarray_create_id, pairarray_create_sig]

let defs : State.global_fun list =
  [ { cid = pairarray_update_cid
    ; body = pairarray_update_fun
    ; ty = pairarray_update_ty
    } ]
;;

let signature =
  let sz = IVar (QVar (Id.fresh "sz")) in
  LibraryInterface.tup_to_sigty
  ( module_id
  , [Cid.last_id t_id, [sz], TName (t_id, [sz], true) |> ty]
  , defs
  , constructors )
;;
