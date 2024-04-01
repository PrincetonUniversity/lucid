open CCoreSyntax
open CCoreExceptions

(* These functions type check calls to builtin functions 
   before their implementations are generated in CCore. 
   To type check builtins with the regular type system, 
   we'd have to add support for polymorphic types, 
   which I don't want to do right now. Or maybe ever. *)

type builtin_checker_t = ty option -> exp -> ty

(*** tables ***)
let check_action_ty aparam_ty aarg_ty aret_ty taction = 
  let inf_aparam_ty, inner_action_ty, _ = extract_func_ty taction in
  let inf_aarg_ty, inf_aret_ty, _ = extract_func_ty inner_action_ty in
  if (equiv_tys (List.hd inf_aparam_ty) aparam_ty) then () 
    else (
    print_endline ("param_ty: "^(CCorePPrint.ty_to_string (List.hd inf_aparam_ty))^" vs "^(CCorePPrint.ty_to_string aparam_ty));                  
    ty_err "param tys not equiv");
  if (equiv_tys (List.hd inf_aarg_ty) aarg_ty) then () 
    else (
    print_endline ("arg_ty: "^(CCorePPrint.ty_to_string (List.hd inf_aarg_ty))^" vs "^(CCorePPrint.ty_to_string aarg_ty));                  
    ty_err "arg tys not equiv");
  let inf_aret_ty = match inf_aret_ty.raw_ty with 
    | TTuple [inner_ty] -> inner_ty
    | _ -> inf_aret_ty
  in
  if (equiv_tys (inf_aret_ty) aret_ty) then () 
    else (
    print_endline ("ret_ty: "^(CCorePPrint.ty_to_string (inf_aret_ty))^" vs "^(CCorePPrint.ty_to_string aret_ty));                  
    ty_err "ret tys not equiv");
;;

let table_create_cid = Tables.constructors |> List.hd |> fst

let table_create_check ty_opt constr_exp =
  let ty = Option.get ty_opt in
  let constr_args = extract_ecall constr_exp |> snd in
  let bty_cid, bty_args = extract_tbuiltin ty in
  match (Cid.names bty_cid), bty_args, constr_args with 
    | ["Table"; "t"], [_; aparam_ty; aarg_ty; aret_ty], [size_arg; actions_arg; def_action_arg; def_action_def_arg_arg] -> (

      check_action_ty aparam_ty aarg_ty aret_ty def_action_arg.ety;
      let all_actions = extract_etuple actions_arg in
      List.iter (fun action -> check_action_ty aparam_ty aarg_ty aret_ty action.ety) all_actions;
      let inf_aparam_ty, _, _ = extract_func_ty def_action_arg.ety in
      if (equiv_tys (List.hd inf_aparam_ty) def_action_def_arg_arg.ety) then () 
        else (
        print_endline ("default action arg arg ty: "^(CCorePPrint.ty_to_string (List.hd inf_aparam_ty))^" vs "^(CCorePPrint.ty_to_string def_action_def_arg_arg.ety));                  
        ty_err "default action arg arg tys not equiv");
      (* check size arg is an int *)
      if (not (is_tint size_arg.ety)) then 
        ty_err "size arg is not an int";
      ty
    )
    | _ -> ty_err "Table.create call is malformed.";
;;

let table_lookup_cid = Cid.create ["Table"; "lookup"] ;;
(* Table.lookup(tbl, key, arg) *)
let table_lookup_check _ exp = 
  let _, args = extract_ecall exp in
  if List.length args <> 3 then 
    ty_err "Table.lookup takes exactly three arguments";
  let tbl = List.hd args in
  let key = List.nth args 1 in
  let arg = List.nth args 2 in
  let bty_cid, bty_args = extract_tbuiltin tbl.ety in
  match (Cid.names bty_cid), bty_args with 
    | ["Table"; "t"], [key_ty'; _; arg_ty'; ret_ty'] -> (
      if (not (equiv_tys key.ety key_ty')) then 
        ty_err "key type does not match table key type";
      if (not (equiv_tys arg.ety arg_ty')) then 
        ty_err "arg type does not match table arg type";
      ret_ty'
    )
    | _ -> ty_err "Table.lookup call is malformed.";
;;

let table_install_cid = Cid.create ["Table"; "install"] ;;
(* Table.install(tbl, key, action, param) *)
let table_install_check _ exp = 
  let _, args = extract_ecall exp in
  if List.length args <> 4 then 
    ty_err "Table.install takes exactly four arguments";
  let tbl = (List.hd args) in
  let key = List.nth args 1 in
  let action = List.nth args 2 in
  print_endline ("action: "^(CCorePPrint.exp_to_string action));
  print_endline ("action ty: "^(CCorePPrint.ty_to_string action.ety));
  let forwarded_param = List.nth args 3 in
  let bty_cid, bty_args = extract_tbuiltin tbl.ety in
  match (Cid.names bty_cid), bty_args with 
    | ["Table"; "t"], [key_ty'; param_ty' ; arg_ty'; ret_ty'] -> (
      if (not (equiv_tys key.ety key_ty')) then 
        ty_err "table key type does not match table key type";
      let arg_tys, ret_ty, _ = extract_func_ty action.ety in 
      let (param_ty, arg_ty) = match arg_tys with 
        | [param_ty; arg_ty] -> (param_ty, arg_ty)
        | _ -> ty_err "action type is malformed";
      in
      if (not (equiv_tys arg_ty' arg_ty)) then 
        ty_err "action arg type does not match table arg type";
      if (not (equiv_tys param_ty' param_ty)) then
        ty_err "action param type does not match table param type";
      if (not (equiv_tys forwarded_param.ety param_ty')) then 
        ty_err "argument to action does not match its install-time param type";
      if (not (equiv_tys ret_ty' ret_ty)) then 
        ty_err "action return type does not match table return type";
      ret_ty'
    )
    | _ -> ty_err "Table.install call is malformed.";
;;

(* Arrays *)
let array_create_cid = Arrays.constructors |> List.hd |> fst
let array_create_check ty_opt constr_exp =
  let ty = Option.get ty_opt in
  let constr_args = extract_ecall constr_exp |> snd in
  let bty_cid, bty_args = extract_tbuiltin ty in
  match (Cid.names bty_cid), bty_args, constr_args with 
    | ["Array"; "t"], [_], [size_arg] -> (
      if (not (is_tint size_arg.ety)) then 
        ty_err "size arg is not an int";
      ty
    )
    | _ -> ty_err "Array.create call is malformed.";
;;

let array_update_complex_cid = Cid.create ["Array"; "update_complex"] ;;
(* array.update_complex(array, index, memop function, memop arg1, memop arg2, memop arg 3) *)
(* make sure that: 
    1. array is an array with a cell type
    2. index is an int
    3. the memop function takes 3 arguments and returns a value of the same type as the array's cell type
    4. the forwarded memop args match the array's cell type
*)
let array_update_complex_check _ exp = 
  let _, args = extract_ecall exp in
  if List.length args <> 6 then 
    ty_err "Array.update_complex takes exactly six arguments";
  let arr = List.hd args in
  let idx = List.nth args 1 in
  let memop = List.nth args 2 in
  let memop_arg1 = List.nth args 3 in
  let memop_arg2 = List.nth args 4 in
  let default_val = List.nth args 5 in
  if (not (is_tint idx.ety)) then 
    ty_err "index arg is not an int";
  let memop_arg_tys, memop_ret_ty, _ = extract_func_ty memop.ety in
  
  let bty_cid, bty_args = extract_tbuiltin arr.ety in  
  match (Cid.names bty_cid), bty_args with 
    | ["Array"; "t"], [elem_ty] -> (
      if (not@@List.for_all (equiv_tys elem_ty) memop_arg_tys) then 
        ty_err "memop arguments do not match array element type";
      let expected_ret_ty = ttuple [elem_ty; elem_ty] in
      if (not@@equiv_tys expected_ret_ty memop_ret_ty) then 
        ty_err@@
          "unexpected memop return type:\n"
        ^("memop_ret_ty: "^(CCorePPrint.ty_to_string memop_ret_ty)^" vs "^(CCorePPrint.ty_to_string expected_ret_ty));
      if (not@@equiv_tys elem_ty memop_arg1.ety) then 
        ty_err "arg for memop value does not match array element type";
      if (not@@equiv_tys elem_ty memop_arg2.ety) then
        ty_err "arg for memop value does not match array element type";
      if (not@@equiv_tys elem_ty default_val.ety) then
        ty_err "default value does not match array element type";
      elem_ty
    )
    | _ -> ty_err "Array.update_complex call is malformed.";
;;

(* parse functions *)
let parse_skip_cid = Cid.create ["parse"; "skip"] ;;
let parse_skip_check _ exp = 
  let _, args = extract_ecall exp in
  (* parse skip should really be changed to take the payload / packet as an arg *)
  if List.length args > 0 then 
    ty_err "parse_skip takes no arguments";
  exp.ety;
;;
let parse_read_cid = Cid.create ["parse"; "read"] ;;
let parse_read_check _ exp = 
  let _, args = extract_ecall exp in
  if List.length args <> 1 then 
    ty_err "parse_read takes exactly one argument";
  if (not (is_tbits (List.hd args).ety))
    then ty_err "parse_read takes a bits argument";
  exp.ety
;;

let builtin_checkers = 
  [
    table_create_cid, table_create_check;
    table_lookup_cid, table_lookup_check;
    table_install_cid, table_install_check;
    array_create_cid, array_create_check;
    array_update_complex_cid, array_update_complex_check;
    parse_skip_cid, parse_skip_check;
    parse_read_cid, parse_read_check;
  ]
;;

let get_checker fexp = 
  let cid = eval_exp fexp |> extract_vsymbol in
  List.assoc_opt cid builtin_checkers
;;
