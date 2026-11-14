open Batteries
open Syntax
open InterpSwitch

(*  some type constructor helpers *)
let effectless_fun_rawty arg_tys ret_ty = 
  let start_eff = FVar (QVar (Id.fresh "eff")) in
  TFun
    { arg_tys
    ; ret_ty
    ; start_eff
    ; end_eff = start_eff
    ; constraints = ref []
    }
;;

let acn_rawty arg_tys ret_ty =
  TAction {
    aarg_tys = arg_tys;
    aret_tys = [ret_ty];
  }
;;

let tynum = ref (-1)
let fresh_rawty base_tyname = 
  incr tynum;
  let ty_id = Id.create (base_tyname ^ string_of_int !tynum) in
  (TQVar (QVar ty_id))
;;
let fresh_ty base_tyname =  
  ty_sp (fresh_rawty base_tyname) Span.default
;;

let fresh_tint () = 
  ty @@ TInt (IVar (QVar (Id.fresh "tint_sz")))
;;

let fresh_size base_id = 
  incr tynum;
  let size_id = Id.create ( base_id ^ string_of_int !tynum) in
   (IVar (QVar size_id))
;;

open InterpSyntax
let taction iarg marg ret = 
  TActionConstr(
    {
      aconst_param_tys=[ty iarg];
      aacn_ty={
        aarg_tys=[ty marg];
        aret_tys=[ty ret];
      }
    })
;;
(* convert a function from ivals -> ivals to a function from values -> values *)
let ival_fcn_to_internal_action st vaction =
  let open CoreSyntax in
  let open InterpState in
  let acn_cid, action_f = match vaction with
    | F (Some(cid), f) -> cid, f
    | F (None, _) -> error "Table.install: interpreter error -- the action was added to the global context without a name"
    | _ -> error "Table.install: expected a function"
  in
  (* fill the switch-state arg of the action function,
     which doesn't matter because its a pure function  *)
  let acn_f  (vs : value list) : value list =
    (* wrap vs in ivals, call action_f, unwrap results *)
    let ivals = List.map (fun v -> V v) vs in
    let result = action_f st ivals in
    (* passing action and args separately to install makes the
       action return a function *)
    let result = match result with
      | F(_, f) ->
        f st []
      | V v ->
        V(v)
        (* extract_ival result *)
    in
    let result = extract_ival result in
    match result.v with
    | VTuple(vs) -> List.map value vs
    | _ -> [result]
  in
  acn_cid, acn_f
;;

let rec flatten_v (v : CoreSyntax.v) =
  match v with
  | VTuple(vs) ->
    (List.map flatten_v vs) |> List.flatten
  | VRecord(id_vs) ->
    List.split id_vs |> snd |> List.map flatten_v |> List.flatten
  | VBool _ | VInt _ | VEvent _ | VGlobal _
  | VPat _  | VGroup _ | VBits _ -> [v]
;;

(*** helpers for the table install / update / remove functions ***)

(* the name and pipeline stage of a table argument *)
let table_arg vtbl =
  match (extract_ival vtbl).v with
  | CoreSyntax.VGlobal(tbl_id, stage) -> tbl_id, stage
  | _ -> error "table arg didn't eval to a global"
;;

(* a key and mask as match patterns: a (value, mask) tuple for each field *)
let ternary_pats vkey vmask =
  let keys = flatten_v (extract_ival vkey).v in
  let masks = flatten_v (extract_ival vmask).v in
  if (List.length keys <> List.length masks) then
    error "table key and mask do not have the same number of fields";
  List.map2
    (fun k m -> CoreSyntax.value (CoreSyntax.VTuple([k; m])))
    keys
    masks
;;

(* an action bound to its install-time arguments.
   Returns the action function, the action's name, and the arguments. *)
let bound_action st vaction vaction_const_arg_tup =
  let vaction_const_args = match vaction_const_arg_tup with
    | V({v=VTuple(vs)}) -> List.map CoreSyntax.value vs
    | V(v) -> [v]
    | _ -> error "expected a tuple for the action args"
  in
  let acn_cid, acn_ctor =
    ival_fcn_to_internal_action st vaction
  in
  let acn remaining_args =
    acn_ctor (vaction_const_args@remaining_args)
  in
  acn, acn_cid, vaction_const_args
;;

(* report a warning from a builtin function. The interpreter keeps running. *)
let warn (st : InterpSwitch.state) msg =
  if InterpConfig.cfg.json || InterpConfig.cfg.interactive
  then print_endline (InterpJson.interp_report_json "warning" msg (Some st.swid))
  else Console.warning msg
;;

let warn_full_table st fun_cid tbl_id =
  warn st (Printf.sprintf "%s: table %s is full, the entry was not installed"
    (String.concat "." (Cid.names fun_cid)) (Id.name tbl_id))
;;

let warn_missing_entry st fun_cid tbl_id =
  warn st (Printf.sprintf "%s: table %s has no such entry"
    (String.concat "." (Cid.names fun_cid)) (Id.name tbl_id))
;;


(* typing *)
let equiv_raw_tys raw_tys = 
  List.fold_left
    (fun (prev_res, prev_rty) rty  -> 
      let res = SyntaxUtils.equiv_raw_ty (TyTQVar.strip_links prev_rty) (TyTQVar.strip_links rty) in
      if (not res) then (
        print_endline ("type mismatch in actions. ");
      print_endline ("expected type: "^(Printing.raw_ty_to_string prev_rty));
      print_endline ("got type: "^(Printing.raw_ty_to_string rty));
      );
      (prev_res && res, prev_rty)) 
    (true, List.hd raw_tys)
    raw_tys
  |> fst
;;  


(* let bitmatch bits n =
  let open Z in
  let bits = List.rev bits in
  let two = Z.of_int 2 in
  let rec aux bits n =
    match bits with
    | [] -> n = zero
    | hd :: tl ->
      aux tl (shift_right n 1)
      &&
      (match hd with
       | 0 -> rem n two = zero
       | 1 -> rem n two <> zero
       | _ -> true)
  in
  aux bits n
;; *)
