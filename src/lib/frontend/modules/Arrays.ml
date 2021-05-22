(* Interpretation of stateful arrays. *)
open Syntax
open Batteries
open InterpState

let rec create_ls size buckets =
  List.init buckets (fun _ -> Integer.create ~value:0 ~size)
;;

(* Generic Array definition *)
let array_name = "Array"
let array_id = Id.create array_name
let array_error fun_name msg = error (array_name ^ ": " ^ fun_name ^ ": " ^ msg)
let module_id = array_id
let t_id = Cid.create_ids [array_id; Id.create "t"]
let sizes_labels = [Id.create "dummy_sz"], []

(* Constructor *)
let array_create_id = Cid.create_ids [array_id; Id.create "create"]

let array_create_sig =
  t_id, [Id.create "dummy_sz"], [TInt (IVar (QVar (Id.fresh "sz")))]
;;

(* Array.update *)

let array_update_name = "update"
let array_update_id = Id.create array_update_name
let array_update_cid = Cid.create_ids [array_id; array_update_id]
let array_update_error msg = array_error array_update_name msg

(* Type of Array.update:
    Array<<'a>> -> int<<32>> -> TMemop(int<<'a>>, 'b) -> 'b -> TMemop (int<<'a>>, 'c) -> 'c -> int<<'a>>
  *)
let array_update_ty =
  let size = IVar (QVar (Id.fresh "sz")) in
  let b = TQVar (QVar (Id.fresh "b")) in
  let c = TQVar (QVar (Id.fresh "c")) in
  let ref_eff = FVar (QVar (Id.fresh "eff")) in
  let start_eff = FVar (QVar (Id.fresh "eff")) in
  TFun
    { arg_tys =
        [ TGlobal ((t_id, [size]), ref_eff)
        ; TInt (IVar (QVar (Id.fresh "sz")))
        ; TMemop (size, b)
        ; b
        ; TMemop (size, c)
        ; c ]
    ; ret_ty = TInt size
    ; start_eff
    ; end_eff = FSucc ref_eff
    ; constraints = ref [CLeq (start_eff, ref_eff)]
    }
;;

let update_fun err nst swid args =
  (* Hack to make the types work *)
  let err str = failwith (err str) in
  let open State in
  match args with
  | [ V { v = VGlobal stage }
    ; V { v = VInt idx }
    ; F getop
    ; getarg
    ; F setop
    ; setarg ] ->
    let get_f arg = getop nst swid [V (vinteger arg); getarg] in
    let set_f arg =
      match setop nst swid [V (vinteger arg); setarg] with
      | { v = VInt v } -> v
      | _ -> err "Wrong type of value from set op"
    in
    update_switch swid stage (Integer.to_int idx) get_f set_f nst
  | _ -> err "Incorrect number or type of arguments to Array.update"
;;

let array_update_fun = update_fun array_update_error
let dummy_memop = State.F (fun _ _ args -> extract_ival (List.hd args))
let setop = State.F (fun _ _ args -> extract_ival (List.nth args 1))
let dummy_int = State.V (vinteger (Integer.of_int 0))

(* Array.get *)
let array_get_name = "get"
let array_get_id = Id.create array_get_name
let array_get_cid = Cid.create_ids [array_id; array_get_id]
let array_get_error msg = array_error array_get_name msg

let array_get_fun nst swid args =
  let open State in
  match args with
  | [arg1; arg2] ->
    update_fun
      array_get_error
      nst
      swid
      [arg1; arg2; dummy_memop; dummy_int; dummy_memop; dummy_int]
  | _ -> array_get_error "Incorrect number of arguments to Array.get"
;;

(* Array.getm *)
let array_getm_name = "getm"
let array_getm_id = Id.create array_getm_name
let array_getm_cid = Cid.create_ids [array_id; array_getm_id]
let array_getm_error msg = array_error array_getm_name msg

let array_getm_fun nst swid args =
  let open State in
  match args with
  | [arg1; arg2; getop; getarg] ->
    update_fun
      array_getm_error
      nst
      swid
      [arg1; arg2; getop; getarg; dummy_memop; dummy_int]
  | _ -> array_getm_error "Incorrect number of arguments to Array.getm"
;;

(* Array.set *)
let array_set_name = "set"
let array_set_id = Id.create array_set_name
let array_set_cid = Cid.create_ids [array_id; array_set_id]
let array_set_error msg = array_error array_set_name msg

let array_set_fun nst swid args =
  let open State in
  match args with
  | [arg1; arg2; setval] ->
    update_fun
      array_set_error
      nst
      swid
      [arg1; arg2; dummy_memop; dummy_int; setop; setval]
  | _ -> array_set_error "Incorrect number of arguments to Array.set"
;;

(* Array.setm *)
let array_setm_name = "setm"
let array_setm_id = Id.create array_setm_name
let array_setm_cid = Cid.create_ids [array_id; array_setm_id]
let array_setm_error msg = array_error array_setm_name msg

let array_setm_fun nst swid args =
  let open State in
  match args with
  | [arg1; arg2; setop; setarg] ->
    update_fun
      array_setm_error
      nst
      swid
      [arg1; arg2; dummy_memop; dummy_int; setop; setarg]
  | _ -> array_setm_error "Incorrect number of arguments to Array.setm"
;;

(* Types for the above four functions *)
let array_get_ty, array_set_ty, array_getm_ty =
  let size = IVar (QVar (Id.fresh "sz")) in
  let b = TQVar (QVar (Id.fresh "b")) in
  let ref_eff = FVar (QVar (Id.fresh "eff")) in
  let start_eff = FVar (QVar (Id.fresh "eff")) in
  let fty =
    { arg_tys =
        [TGlobal ((t_id, [size]), ref_eff); TInt (IVar (QVar (Id.fresh "sz")))]
    ; ret_ty = TInt size
    ; start_eff
    ; end_eff = FSucc ref_eff
    ; constraints = ref [CLeq (start_eff, ref_eff)]
    }
  in
  ( TFun fty
  , TFun { fty with arg_tys = fty.arg_tys @ [TInt size] }
  , TFun { fty with arg_tys = fty.arg_tys @ [TMemop (size, b); b] } )
;;

let array_setm_ty = array_getm_ty
let constructors = [array_create_id, array_create_sig]

let defs : State.global_fun list =
  [ { cid = array_get_cid; body = array_get_fun; ty = array_get_ty }
  ; { cid = array_getm_cid; body = array_getm_fun; ty = array_getm_ty }
  ; { cid = array_set_cid; body = array_set_fun; ty = array_set_ty }
  ; { cid = array_setm_cid; body = array_setm_fun; ty = array_setm_ty }
  ; { cid = array_update_cid; body = array_update_fun; ty = array_update_ty } ]
;;
