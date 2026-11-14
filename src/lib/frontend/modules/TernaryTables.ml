(* Ternary tables as a builtin module *)
(* A TernaryTable is a Table (Tables.ml) whose entries have a mask and
   a priority. An entry is identified by its key, mask, and priority.
   A lookup uses the matching entry with the highest priority. *)
open Batteries
open Syntax
open InterpSwitch
open LibraryUtils
open Pipeline
open InterpSyntax

let error= Syntax.error

let name = "TernaryTable"
let module_error fun_name msg =
  error (name ^ ": " ^ fun_name ^ ": " ^ msg)
;;
let id = Id.create name
let module_id = id
let t_id = Cid.create_ids [id; Id.create "t"]

let sizes = 0
let ty_args = 4
let global = true


(* TernaryTable.t<<'key_ty, 'installarg_ty, 'matcharg_ty, 'ret_ty>> *)
let fresh_t (key_ty, iarg_ty, marg_ty, ret_ty) =
  TBuiltin(t_id, [key_ty; iarg_ty; marg_ty; ret_ty], true)
;;

(* an entry's priority is a 32-bit int. A higher number is a higher priority. *)
let prio_ty = ty @@ TInt (IConst 32)

let create_id = Cid.create_ids [id; Id.create "create"]
let create_sig =
  let key_rty, iarg_rty, marg_rty, ret_rty =
    fresh_rawty "table_key_ty",
    fresh_rawty "table_iarg_ty",
    fresh_rawty "table_marg_ty",
    fresh_rawty "table_ret_ty"
  in
  let acn_rty = taction iarg_rty marg_rty ret_rty in
  let acns_rty = TVector(acn_rty, fresh_size "acn_vec_len") in
  let start_eff = FVar (QVar (Id.fresh "tbl_create_eff")) in
  let arg_tys = [
      fresh_tint () ;                            (* number of entries *)
      ty @@ acns_rty;                            (* actions bound to the table *)
      ty @@ acn_rty;                             (* default action *)
      ty @@ iarg_rty                              (* default action argument *)
    ]
  in
  let tbl_eff = FVar (QVar (Id.fresh "tbl_eff")) in
  let ret_ty = ty_eff (fresh_t (key_rty, iarg_rty, marg_rty, ret_rty)) tbl_eff in
  let ctor_ty = {
      arg_tys
    ; ret_ty
    ; start_eff
    ; end_eff = start_eff
    ; constraints = ref []
  } in
  ctor_ty
;;

(* interpreter implementation *)
(* a ternary table is created the same way as a table *)
let create_ctor = Tables.create_ctor

let priority_arg vprio =
  match (extract_ival vprio).v with
  | CoreSyntax.VInt(prio) -> Integer.to_int prio
  | _ -> error "TernaryTable: priority arg didn't eval to an int"
;;


(* TernaryTable.install : TernaryTable.t<<'k, iarg, marg, ret>> -> 'k -> 'k -> int -> (iarg -> marg -> ret) -> iarg -> () *)
let install_name = "install"
let install_id = Id.create install_name
let install_cid = Cid.create_ids [id; install_id]
let install_error msg = module_error install_name msg

let install_ty =
  let key_rty, iarg_rty, marg_rty, ret_rty =
    fresh_rawty "table_key_ty",
    fresh_rawty "table_iarg_ty",
    fresh_rawty "table_marg_ty",
    fresh_rawty "table_ret_ty"
  in
  let mask_rawty = key_rty in
  let acn_rty = taction iarg_rty marg_rty ret_rty in
  let start_eff = FVar (QVar (Id.fresh "eff")) in (* effect where this call begins *)
  (* table argument *)
  let tbl_eff = FVar (QVar (Id.fresh "eff")) in (* effect attached to table arg *)
  let tbl_t = ty_eff (fresh_t (key_rty, iarg_rty, marg_rty, ret_rty)) tbl_eff in
  let arg_tys = [
      tbl_t;
      ty @@ key_rty;
      ty @@ mask_rawty;
      prio_ty;
      ty @@ acn_rty;
      ty @@ iarg_rty
    ]
  in
  let ret_ty = ty @@ TVoid in
  ty @@ TFun
    { arg_tys
    ; ret_ty
    ; start_eff
    ; end_eff = FSucc tbl_eff
    ; constraints = ref [CLeq (start_eff, tbl_eff)]
    }
;;

(* install a masked pattern with a priority *)
let install_fun st args =
  match args with
  | [vtbl; vkey; vmask; vprio; vaction; vaction_const_arg_tup] ->
    let tbl_id, stage = table_arg vtbl in
    let acn, acn_cid, vaction_const_args =
      bound_action st vaction vaction_const_arg_tup
    in
    (* install to the pipeline *)
    let installed =
      Pipeline.install_table_entry
        stage
        (priority_arg vprio)
        (ternary_pats vkey vmask)
        acn
        acn_cid
        vaction_const_args
        st.pipeline
    in
    if (not installed) then warn_full_table st install_cid tbl_id;
    V(CoreSyntax.vtup [] Span.default) (* no return *)
  | _ ->
    install_error "Incorrect number of arguments to TernaryTable.install"



(* TernaryTable.update : TernaryTable.t<<'k, iarg, marg, ret>> -> 'k -> 'k -> int -> (iarg -> marg -> ret) -> iarg -> () *)
let update_name = "update"
let update_id = Id.create update_name
let update_cid = Cid.create_ids [id; update_id]
let update_error msg = module_error update_name msg

let update_ty = install_ty

(* replace the action and action argument of the entry for a key, mask and priority *)
let update_fun st args =
  match args with
  | [vtbl; vkey; vmask; vprio; vaction; vaction_const_arg_tup] ->
    let tbl_id, stage = table_arg vtbl in
    let acn, acn_cid, vaction_const_args =
      bound_action st vaction vaction_const_arg_tup
    in
    let updated =
      Pipeline.update_table_entry
        stage
        (priority_arg vprio)
        (ternary_pats vkey vmask)
        acn
        acn_cid
        vaction_const_args
        st.pipeline
    in
    if (not updated) then warn_missing_entry st update_cid tbl_id;
    V(CoreSyntax.vtup [] Span.default) (* no return *)
  | _ ->
    update_error "Incorrect number of arguments to TernaryTable.update"



(* TernaryTable.remove : TernaryTable.t<<'k, iarg, marg, ret>> -> 'k -> 'k -> int -> () *)
let remove_name = "remove"
let remove_id = Id.create remove_name
let remove_cid = Cid.create_ids [id; remove_id]
let remove_error msg = module_error remove_name msg

let remove_ty =
  let key_rty, iarg_rty, marg_rty, ret_rty =
    fresh_rawty "table_key_ty",
    fresh_rawty "table_iarg_ty",
    fresh_rawty "table_marg_ty",
    fresh_rawty "table_ret_ty"
  in
  let mask_rawty = key_rty in
  let start_eff = FVar (QVar (Id.fresh "eff")) in (* effect where this call begins *)
  let tbl_eff = FVar (QVar (Id.fresh "eff")) in (* effect attached to table arg *)
  let tbl_t = ty_eff (fresh_t (key_rty, iarg_rty, marg_rty, ret_rty)) tbl_eff in
  let arg_tys = [
      tbl_t;
      ty @@ key_rty;
      ty @@ mask_rawty;
      prio_ty
    ]
  in
  let ret_ty = ty @@ TVoid in
  ty @@ TFun
    { arg_tys
    ; ret_ty
    ; start_eff
    ; end_eff = FSucc tbl_eff
    ; constraints = ref [CLeq (start_eff, tbl_eff)]
    }
;;

(* remove the entry for a key, mask and priority *)
let remove_fun st args =
  match args with
  | [vtbl; vkey; vmask; vprio] ->
    let tbl_id, stage = table_arg vtbl in
    let removed =
      Pipeline.remove_table_entry
        stage
        (priority_arg vprio)
        (ternary_pats vkey vmask)
        st.pipeline
    in
    if (not removed) then warn_missing_entry st remove_cid tbl_id;
    V(CoreSyntax.vtup [] Span.default) (* no return *)
  | _ ->
    remove_error "Incorrect number of arguments to TernaryTable.remove"



(* TernaryTable.lookup *)
let lookup_name = "lookup"
let lookup_id = Id.create lookup_name
let lookup_cid = Cid.create_ids [id; lookup_id]
let lookup_error msg = module_error lookup_name msg
(* TernaryTable.lookup : TernaryTable.t<<'k, iarg, marg, ret>> -> 'k -> marg -> ret *)
let lookup_ty =
  let key_rty, iarg_rty, marg_rty, ret_rty =
    fresh_rawty "table_key_ty",
    fresh_rawty "table_iarg_ty",
    fresh_rawty "table_marg_ty",
    fresh_rawty "table_ret_ty"
  in
  let start_eff = FVar (QVar (Id.fresh "eff")) in (* effect where this call begins *)
  let tbl_eff = FVar (QVar (Id.fresh "eff")) in (* effect attached to table arg *)
  let tbl_t = ty_eff (fresh_t (key_rty, iarg_rty, marg_rty, ret_rty)) tbl_eff in
  let arg_tys = [
      tbl_t;
      ty @@ key_rty;
      ty @@ marg_rty
    ]
  in
  let ret_ty = ty ret_rty in
  ty @@ TFun
    { arg_tys
    ; ret_ty
    ; start_eff
    ; end_eff = FSucc tbl_eff
    ; constraints = ref [CLeq (start_eff, tbl_eff)]
    }
;;

(* the pipeline keeps a table's entries in priority order,
   so a lookup is the same as a table's: the first match *)
let lookup_fun = Tables.lookup_fun


let constructors = [create_id, create_sig]

let defs : global_fun list =
  [
    { cid = install_cid; body = install_fun; ty = install_ty };
    { cid = update_cid; body = update_fun; ty = update_ty };
    { cid = remove_cid; body = remove_fun; ty = remove_ty };
    { cid = lookup_cid; body = lookup_fun; ty = lookup_ty }
  ]
;;

let signature =
  let key_rty, iarg_rty, marg_rty, ret_rty =
    fresh_rawty "table_key_ty",
    fresh_rawty "table_iarg_ty",
    fresh_rawty "table_marg_ty",
    fresh_rawty "table_ret_ty"
  in
  LibraryInterface.tup_to_sigty
  ( module_id
  , [Cid.last_id t_id, [], ty (fresh_t (key_rty, iarg_rty, marg_rty, ret_rty))]
  , defs
  , constructors )
;;
