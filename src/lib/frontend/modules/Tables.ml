(* Tables as a builtin module *)
open Batteries
open Syntax
open InterpState
open LibraryUtils

let err_sp = Console.error_position
let err = Console.error

let name = "Table"
let id = Id.create name
let module_id = id
let t_id = Cid.create_ids [id; Id.create "t"]

let error fun_name msg =
  error (name ^ ": " ^ fun_name ^ ": " ^ msg)
;;

let sizes = 3 (* key dimensions, action args, action ret *)
let ty_args = 0
let global = true


(* Table.t<'k, 'a, 'r> *)
let unbound_t (key_fmt,acn_arg,acn_ret) = 
  TName(t_id, [key_fmt; acn_arg; acn_ret], true)
;;
let fresh_t_sizes () = 
  fresh_size "table_key_fmt",
  fresh_size "table_acn_arg", 
  fresh_size "table_acn_ret"
;;

(*  Table.create  :   int -> 'a list (or tup)  -> 'a -> Table.t<'k><<'a>> *)
let create_id = Cid.create_ids [id; Id.create "create"]
let create_sig =
  let key_fmt, acn_arg, acn_ret = fresh_t_sizes () in
  let acn_rty  = fresh_rawty   "table_acn_rty" in
  let acns_rty = fresh_rawty "table_acns_rty" in
  let start_eff = FVar (QVar (Id.fresh "tbl_create_eff")) in
  let arg_tys = [
      fresh_tint () ;                            (* number of entries *)
      ty @@ acns_rty;                            (* actions bound to the table *)
      ty @@ acn_rty                              (* default action *)
    ] 
  in
  let tbl_eff = FVar (QVar (Id.fresh "tbl_eff")) in
  let ret_ty = ty_eff (unbound_t (key_fmt, acn_arg, acn_ret)) tbl_eff in
  let ctor_ty = {
      arg_tys
    ; ret_ty
    ; start_eff
    ; end_eff = start_eff
    ; constraints = ref []
  } in
  ctor_ty 
;;

(* Table.install *)
let install_name = "install"
let install_id = Id.create install_name
let install_cid = Cid.create_ids [id; install_id]
let install_error msg = error install_name msg

let install_ty = 
  let key_fmt, acn_arg, acn_ret = fresh_t_sizes () in
  let key_ty = fresh_rawty "tbl_key_ty" in
  let acn_ty = fresh_rawty "tbl_acn_ty" in
  let start_eff = FVar (QVar (Id.fresh "eff")) in (* effect where this call begins *)
  (* table argument *)
  let tbl_eff = FVar (QVar (Id.fresh "eff")) in (* effect attached to table arg *)
  let tbl_t = ty_eff (unbound_t (key_fmt, acn_arg, acn_ret)) tbl_eff in
  let arg_tys = [
      tbl_t; 
      ty @@ key_ty;
      ty @@ acn_ty
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

let install_fun nst swid args =
  let _, _ = nst, swid in
  let open State in
  let open InterpSyntax in
  let open CoreSyntax in
  match args with
  | [V { v = VGlobal _ }; V { v = VTuple(_) }; F _] ->    
    install_error "Table.install not implemented"
  | _ ->
    install_error "Incorrect number or type of arguments to Table.install"


(* Table.lookup *)
let lookup_name = "lookup"
let lookup_id = Id.create lookup_name
let lookup_cid = Cid.create_ids [id; lookup_id]
let lookup_error msg = error lookup_name msg
(* Table.match   :   Table.t<'k> -> int<'k> -> 'a -> 'r *)
let lookup_ty = 
  let key_fmt, acn_arg, acn_ret = fresh_t_sizes () in

  let key_ty = fresh_rawty "tbl_key_ty" in
  let acn_arg_ty = fresh_rawty "tbl_acn_arg_ty" in
  let acn_ret_ty = fresh_rawty "tbl_acn_ret_ty" in

  let start_eff = FVar (QVar (Id.fresh "eff")) in (* effect where this call begins *)
  let tbl_eff = FVar (QVar (Id.fresh "eff")) in (* effect attached to table arg *)
  let tbl_t = ty_eff (unbound_t (key_fmt, acn_arg, acn_ret)) tbl_eff in
  let arg_tys = [
      tbl_t; 
      ty @@ key_ty;
      ty @@ acn_arg_ty
    ]
  in
  let ret_ty = ty acn_ret_ty in 
  ty @@ TFun
    { arg_tys
    ; ret_ty
    ; start_eff
    ; end_eff = FSucc tbl_eff
    ; constraints = ref [CLeq (start_eff, tbl_eff)]
    }
;;

let lookup_fun nst swid args =
  let _, _ = nst, swid in
  let open State in
  let open InterpSyntax in
  let open CoreSyntax in
  match args with
  | [V { v = VGlobal _ }; V { v = VTuple(_) }; F _] ->    
    lookup_error "Table.lookup not implemented"
  | _ ->
    lookup_error "Incorrect number or type of arguments to Table.lookup"


let constructors = [create_id, create_sig]

let defs : State.global_fun list =
  [{ cid = install_cid; body = install_fun; ty = install_ty }
  ;{ cid = lookup_cid; body = lookup_fun; ty = lookup_ty }  
  ]
;;

let signature =  
  let key_fmt, acn_arg, acn_ret = fresh_t_sizes () in
  ( module_id
  , [Cid.last_id t_id, [key_fmt; acn_arg; acn_ret], ty (unbound_t (key_fmt, acn_arg, acn_ret))]
  , defs
  , constructors )
;;



(*** Table method call typing -- this runs after the main pass of the frontend type checker ***)
let strip_links ty = { ty with raw_ty = TyTQVar.strip_links ty.raw_ty }
let rty_to_size rty = 
  match rty with 
  | TInt(sz) -> sz
  | TBool -> IConst(1)
  | _ -> err "[rty_to_size] expected an integer, but got something else"
;;

(* convert a raw type into a list of sizes *)
let rec rty_to_sizes rty = 
  match (TyTQVar.strip_links rty) with 
  | TQVar(_) -> [fresh_size "any_sz"]
  | TTuple(rtys) -> List.map rty_to_sizes rtys |> List.flatten
  | TRecord(label_rtys) -> List.map (fun (_, rty) -> rty_to_sizes rty) label_rtys |> List.flatten
  | TVector(rty, len) -> List.init (SyntaxUtils.extract_size len) (fun _ -> rty_to_size rty)
  | _ -> [rty_to_size rty]
;;
  (* unpack and flatten sizes *)
let rec unpack_sizes sz = match (STQVar.strip_links sz) with 
  | ITup(sizes) -> List.map unpack_sizes sizes |> List.flatten
  | sz -> [sz]
;;

(* type and size flatteners  *)
let rec flatten_rty rty = 
  match (TyTQVar.strip_links rty) with 
  | TQVar(_) -> [rty]
  | TTuple(rtys) -> List.map flatten_rty rtys |> List.flatten
  | TRecord(label_rtys) -> List.map (fun (_, rty) -> flatten_rty rty) label_rtys |> List.flatten
  | TVector(rty, len) -> List.init (SyntaxUtils.extract_size len) (fun _ -> rty)
  | _ -> [rty]
;;
let rec flatten_size size = 
  match (STQVar.strip_links size) with 
  | ITup(sizes) -> List.map flatten_size sizes |> List.flatten
  | size -> [size]
;;

(* given a raw_ty and a size with the same shape, 
  replace any TQVar in the raw_ty with TInt(sz) *)
  let rec replace_qvars raw_ty size = 
  match (TyTQVar.strip_links raw_ty), size with 
    | TQVar(_), sz -> TInt(sz)
    | TTuple(rtys), ITup(sizes) -> 
      if List.length rtys != List.length sizes then
        err "Tables.typer.replace_qvars: rtys and sizes have different lengths in TTuple"
      else
        TTuple(List.map2 replace_qvars rtys sizes)
    | TRecord(label_rtys), ITup(sizes) -> 
      if List.length label_rtys != List.length sizes then
        err "Tables.typer.replace_qvars: label_rtys and sizes have different lengths in TRecord"
      else
        TRecord(List.map2 (fun (label, rty) sz -> (label, replace_qvars rty sz)) label_rtys sizes)
    | TVector(rty, len), ITup(sizes) -> 
      if List.length sizes != 1 then
        err "Tables.typer.replace_qvars: sizes should have one element in TVector"
      else
        TVector(replace_qvars rty (List.hd sizes), len)
    (* no replacement necessary for any other case *)
    | _, _ -> raw_ty
;;  

let rec flatten_exp exp = match exp.e with 
  | ETuple(es) -> List.map flatten_exp es |> List.flatten
  | EVector(es) -> List.map flatten_exp es |> List.flatten
  | ERecord(label_exps) -> List.map (fun (_, exp) -> flatten_exp exp) label_exps |> List.flatten
  | _ -> [exp]
;;

let to_action_ty sp raw_ty = match raw_ty with 
  | TAction _ -> raw_ty
  | TActionConstr({aacn_ty}) -> TAction(aacn_ty)
  | _ -> err_sp sp "[table type checker] expected an action or action constructor"
;;

let check_create (exp : Syntax.exp) =
   (* make sure all the actions have the same type *)
  let actions, default_action = match exp.e with 
    | ECall(_, [_; action_list; default_action], _) -> flatten_exp action_list, default_action 
    | _ -> err_sp exp.espan "[table type checker] Table.create has invalid number of arguments"
  in  
  let acn_raw_tys = List.map 
    (fun acn_exp -> 
      (Option.get acn_exp.ety).raw_ty
      |> to_action_ty acn_exp.espan) 
    (default_action::actions) 
  in
  if (not (equiv_raw_tys acn_raw_tys)) then (
    err_sp exp.espan "[table type checker] actions in Table.create don't have matching types"
  );
  exp
;;
let check_install exp = 
  let table_arg, key_arg, acn_arg = match exp.e with 
    | ECall(_, [table_arg; key_arg; acn_arg], _) -> table_arg, key_arg, acn_arg
    | _ -> err_sp exp.espan "[table type checker] Table.install called with wrong number of arguments"
  in   
  let key_fmt, acn_arg_fmt, acn_ret_fmt = match (Option.get table_arg.ety).raw_ty with 
    | TName(_, [key_fmt; acn_arg; acn_ret], _) -> key_fmt, acn_arg, acn_ret
    | _ -> err_sp table_arg.espan "[table type checker] Wrong number of size parameters for Table.t"
  in
  (* make sure the sizes of the key argument match the key format size attached to Table.t *)
  let key_rawty = (Option.get key_arg.ety).raw_ty in
  let expected_key_rawty = TTuple(List.map (fun sz -> TPat(sz)) (unpack_sizes key_fmt)) in
  if (not (SyntaxUtils.equiv_raw_ty ~qvars_wild:true expected_key_rawty key_rawty)) then 
    (err_sp key_arg.espan "[table type checker] Table.install's key argument has wrong type");
  (* make sure the size of the action's arguments and return match the Table.t's formats *)
  let acn_arg_rawtys, acn_ret_rawtys = match (Option.get acn_arg.ety).raw_ty with 
    | TAction({aarg_tys; aret_tys;}) -> 
      let acn_arg_rawtys = List.map (fun arg_ty -> arg_ty.raw_ty) aarg_tys in
      let acn_ret_rawtys = List.map (fun arg_ty -> arg_ty.raw_ty) aret_tys in
      acn_arg_rawtys, acn_ret_rawtys
    | TActionConstr(acn_ctor) -> 
      let acn_arg_rawtys = List.map (fun arg_ty -> arg_ty.raw_ty) acn_ctor.aacn_ty.aarg_tys in
      let acn_ret_rawtys = List.map (fun arg_ty -> arg_ty.raw_ty) acn_ctor.aacn_ty.aret_tys in
      acn_arg_rawtys, acn_ret_rawtys
    | _ -> err_sp acn_arg.espan ("[table type checker] Table.install's action argument ("^(Printing.exp_to_string acn_arg)^") has wrong type")
  in
  let acn_arg_sizes = List.map rty_to_sizes acn_arg_rawtys |> List.flatten in
  let acn_ret_sizes = List.map rty_to_sizes acn_ret_rawtys |> List.flatten in
  (* check list lengths first *)
  if (List.length acn_arg_sizes <> List.length (unpack_sizes acn_arg_fmt)) then (
    let err_msg = Printf.sprintf 
      "[table type checker] Table.install's action (%s) has wrong number of arguments. Expected %d, got %d" 
      (Printing.exp_to_string acn_arg)
      (List.length (unpack_sizes acn_arg_fmt))
      (List.length acn_arg_sizes) 
    in
    err_sp acn_arg.espan err_msg);
  if (List.length acn_ret_sizes <> List.length (unpack_sizes acn_ret_fmt)) then
    (err_sp acn_arg.espan "[table type checker] Table.install's action has wrong number of return values");
  (* check that the sizes match *)
  if (not (List.for_all2 (SyntaxUtils.equiv_size ~qvars_wild:true) acn_arg_sizes (unpack_sizes acn_arg_fmt))) then 
    (err_sp acn_arg.espan "[table type checker] Table.install's action has wrong argument type");
  if (not (List.for_all2 (SyntaxUtils.equiv_size ~qvars_wild:true) acn_ret_sizes (unpack_sizes acn_ret_fmt))) then
    (err_sp acn_arg.espan "[table type checker] Table.install's action has wrong return type");
  exp
;;



let check_lookup exp : exp = 
  let table_arg, key_arg, acn_args = match exp.e with 
    | ECall(_, [table_arg; key_arg; acn_args], _) -> table_arg, key_arg, acn_args
    | _ -> err_sp exp.espan "[table type checker] Table.lookup called with wrong number of arguments"
  in
  let key_fmt, acn_arg_fmt, acn_ret_fmt = match (Option.get table_arg.ety).raw_ty with 
    | TName(_, [key_fmt; acn_arg; acn_ret], _) -> key_fmt, acn_arg, acn_ret
    | _ -> err_sp table_arg.espan "[table type checker] Wrong number of size parameters for Table.t"
  in
  (* make sure the sizes of the key argument match the key format size attached to Table.t *)
  let key_rawty = (Option.get key_arg.ety).raw_ty in
  let key_sizes = rty_to_sizes key_rawty in
  let expected_key_sizes = unpack_sizes key_fmt in
  if (not (List.for_all2 (SyntaxUtils.equiv_size ~qvars_wild:true) key_sizes expected_key_sizes)) then    
    (err_sp key_arg.espan "[table type checker] Table.install's key argument has wrong type");

  (* make sure the action args match the acn_arg_fmt *)
  let acn_arg_rawtys = flatten_rty (Option.get acn_args.ety).raw_ty in    
  let acn_arg_sizes = List.map rty_to_sizes acn_arg_rawtys |> List.flatten in
  (* check list lengths first *)
  if (List.length acn_arg_sizes <> List.length (unpack_sizes acn_arg_fmt)) then 
    (err_sp acn_args.espan "[table type checker] Table.lookup called with wrong number of action arguments");
  (* check that the sizes match *)
  if (not (List.for_all2 (SyntaxUtils.equiv_size ~qvars_wild:true) acn_arg_sizes (unpack_sizes acn_arg_fmt))) then (
    let err_msg = Printf.sprintf 
      "[table type checker] Table.lookup called with wrong action argument sizes. Expected %s, got %s" 
      (Printing.size_to_string (acn_arg_fmt))
      (Printing.sizes_to_string acn_arg_sizes)
    in
    err_sp acn_args.espan err_msg);

  match (strip_links ((Option.get exp.ety))).raw_ty with 
  | TQVar _ -> exp (* if the return's size is unbound, it can be anything *)
  | raw_ty -> (
    (* qvars can happen here if an element from the tuple is never used. *)
    let raw_ty = replace_qvars raw_ty acn_ret_fmt in
    let acn_ret_sizes = rty_to_sizes raw_ty in
    if (List.length acn_ret_sizes <> List.length (unpack_sizes acn_ret_fmt)) then (
      err_sp exp.espan "[table type checker] return variable has wrong number of values for table's action type.");
    (* check that the sizes match *)
    if (not (List.for_all2 (SyntaxUtils.equiv_size ~qvars_wild:false) acn_ret_sizes (unpack_sizes acn_ret_fmt))) then
      (err_sp exp.espan "[table type checker] return variable has wrong type for table's action type.");
    {exp with ety = Some({(Option.get exp.ety) with raw_ty = raw_ty})}
  )
;;

(* type checker signatures *)
type module_typer = (Syntax.exp -> Syntax.exp)
type module_typer_map = (Cid.t * module_typer) list

let (typers : module_typer_map) = [
  (create_id, check_create);
  (install_cid, check_install);
  (lookup_cid, check_lookup);
]

(* scan through the declarations, each time a call to 
   an entry in module_typers is found, 
   call the corresponding typer *)
let module_type_checker (module_typers: module_typer_map) decls = 
  let checker = object(_) 
    inherit [_] s_map as super 
    method! visit_exp () exp = 
      match exp.e with 
      | ECall(cid, _, _) -> (
        match (List.assoc_opt cid module_typers) with 
        | Some(typer) -> typer exp
        | None -> super#visit_exp () exp
      )
      | _ -> 
        super#visit_exp () exp
    end 
  in 
  checker#visit_decls () decls
;;

let type_checker decls = 
  module_type_checker typers decls
;;

let is_tbl_ty (raw_ty : CoreSyntax.raw_ty) = 
  match raw_ty with 
  | TName(cid, _, _) -> Cid.equal cid t_id
  | _ -> false
;;

type core_tbl_ty = 
  { tkey_sizes : CoreSyntax.size list
  ; tparam_tys : CoreSyntax.ty list
  ; tret_tys : CoreSyntax.ty list
  }
;;

let size_ints (sz : CoreSyntax.size) = match sz with 
  | Sz sz -> [sz]
  | Szs szs -> szs
;;
let size_to_ty (sz: int) = (CoreSyntax.ty) @@ TInt(CoreSyntax.Sz sz) ;;

let tname_to_ttable (rty : CoreSyntax.raw_ty) : core_tbl_ty =
  match rty with
  | TName(cid, [key_sizes; param_sizes; ret_sizes], _) when Cid.equal cid t_id -> 
    let param_tys = List.map size_to_ty (size_ints param_sizes) in
    let ret_tys = List.map size_to_ty (size_ints ret_sizes) in
    { 
      tkey_sizes = List.map (fun sz -> (CoreSyntax.Sz sz)) (size_ints key_sizes); 
      tparam_tys = param_tys;
      tret_tys = ret_tys;
    }
  | _ -> err "[tname_to_ttable] table does not have type table."
;;
