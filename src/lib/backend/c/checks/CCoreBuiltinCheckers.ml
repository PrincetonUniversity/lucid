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
  let param_tys, inf_aret_ty, _ = extract_func_ty taction in
  let inf_aparam_ty, inf_aarg_ty = match param_tys with 
    | [a; b] -> a, b
    | _ -> err "action type has wrong number of arguments (expected exactly 2)"
  in
  let check_pass = List.for_all2 equiv_tys
    [inf_aparam_ty; inf_aarg_ty; inf_aret_ty]
    [aparam_ty; aarg_ty; aret_ty]
  in
  if check_pass then () 
  else err "action type mismatch"
;;

(* the type arguments of a Table.t or TernaryTable.t: 
   the types of its key, action param, action arg, and action return *)
let table_ty_args (tbl_ty : ty) = 
  let bty_cid, bty_args = extract_tbuiltin tbl_ty in
  match (Cid.names bty_cid), bty_args with 
    | ["Table"; "t"], [key_ty; aparam_ty; aarg_ty; aret_ty] 
    | ["TernaryTable"; "t"], [key_ty; aparam_ty; aarg_ty; aret_ty] -> 
      key_ty, aparam_ty, aarg_ty, aret_ty
    | _ -> ty_err "expected a Table.t or a TernaryTable.t"
;;

let table_create_cid = Tables.constructors |> List.hd |> fst
let ternary_table_create_cid = TernaryTables.constructors |> List.hd |> fst

(* Table.create(size, actions, default_action, default_action_arg), 
   TernaryTable.create(size, actions, default_action, default_action_arg) *)
let table_create_check ty_opt constr_exp =
  let ty = Option.get ty_opt in
  let constr_args = extract_ecall constr_exp |> snd in
  let _, aparam_ty, aarg_ty, aret_ty = table_ty_args ty in
  match constr_args with 
    | [size_arg; actions_arg; def_action_arg; def_action_def_arg_arg] -> (
      check_action_ty aparam_ty aarg_ty aret_ty def_action_arg.ety;
      let all_actions = extract_etuple actions_arg in
      List.iter (fun action -> check_action_ty aparam_ty aarg_ty aret_ty action.ety) all_actions;
      let inf_aparam_ty, _, _ = extract_func_ty def_action_arg.ety in
      if (equiv_tys (List.hd inf_aparam_ty) def_action_def_arg_arg.ety) then () 
        else (
        print_endline ("default action arg arg ty: "^(CCorePPrint.ty_to_string ~use_abstract_name:true (List.hd inf_aparam_ty))^" vs "^(CCorePPrint.ty_to_string ~use_abstract_name:true def_action_def_arg_arg.ety));                  
        ty_err "default action arg arg tys not equiv");
      (* check size arg is an int *)
      if (not (is_tint size_arg.ety)) then 
        ty_err "size arg is not an int";
      ty
    )
    | _ -> ty_err "table create call is malformed.";
;;

let table_lookup_cid = Cid.create ["Table"; "lookup"] ;;
let ternary_table_lookup_cid = Cid.create ["TernaryTable"; "lookup"] ;;
(* Table.lookup(tbl, key, arg), TernaryTable.lookup(tbl, key, arg) *)
let table_lookup_check _ exp = 
  match extract_ecall exp |> snd with 
    | [tbl; key; arg] -> (
      let key_ty', _, arg_ty', ret_ty' = table_ty_args tbl.ety in
      if (not (equiv_tys key.ety (base_type key_ty'))) then (
        print_endline@@"argument key type:\n"^(CCorePPrint.ty_to_string ~use_abstract_name:true key.ety)^"\ntable key type:\n"^(CCorePPrint.ty_to_string ~use_abstract_name:true key_ty');
        ty_err "key type does not match table key type");
      if (not (equiv_tys arg.ety (base_type arg_ty'))) then 
        ty_err "arg type does not match table arg type";
      ret_ty'
    )
    | _ -> ty_err "table lookup takes exactly three arguments";
;;

(* check a key or a mask of an entry *)
let check_entry_key (tbl : exp) (key : exp) = 
  let key_ty', _, _, _ = table_ty_args tbl.ety in
  if (not (equiv_tys key.ety (base_type key_ty'))) then 
    ty_err "entry key or mask type does not match table key type";
;;
let check_entry_prio (prio : exp) = 
  if (not (equiv_tys prio.ety (tint 32))) then 
    ty_err "entry priority is not a 32-bit int";
;;
(* check the action of an entry and the argument that it is bound to *)
let check_entry_action (tbl : exp) (action : exp) (forwarded_param : exp) = 
  let _, param_ty', arg_ty', ret_ty' = table_ty_args tbl.ety in
  check_action_ty param_ty' arg_ty' ret_ty' action.ety;
  if (not (equiv_tys forwarded_param.ety (base_type param_ty'))) then (
    print_endline ("forwarded param: "^(CCorePPrint.ty_to_string ~use_abstract_name:true (forwarded_param.ety))^" vs "^(CCorePPrint.ty_to_string ~use_abstract_name:true param_ty'));                  
    ty_err "argument to action does not match its install-time param type");
;;
let table_ret_ty (tbl : exp) = 
  let _, _, _, ret_ty' = table_ty_args tbl.ety in
  ret_ty'
;;

let table_install_cid = Cid.create ["Table"; "install"] ;;
let table_update_cid = Cid.create ["Table"; "update"] ;;
(* Table.install(tbl, key, action, param), Table.update(tbl, key, action, param) *)
let table_install_check _ exp = 
  match extract_ecall exp |> snd with 
    | [tbl; key; action; forwarded_param] -> 
      check_entry_key tbl key;
      check_entry_action tbl action forwarded_param;
      table_ret_ty tbl
    | _ -> ty_err "Table.install and Table.update take exactly four arguments";
;;

let table_remove_cid = Cid.create ["Table"; "remove"] ;;
(* Table.remove(tbl, key) *)
let table_remove_check _ exp = 
  match extract_ecall exp |> snd with 
    | [tbl; key] -> 
      check_entry_key tbl key;
      table_ret_ty tbl
    | _ -> ty_err "Table.remove takes exactly two arguments";
;;

let ternary_table_install_cid = Cid.create ["TernaryTable"; "install"] ;;
let ternary_table_update_cid = Cid.create ["TernaryTable"; "update"] ;;
(* TernaryTable.install(tbl, key, mask, prio, action, param), 
   TernaryTable.update(tbl, key, mask, prio, action, param) *)
let ternary_table_install_check _ exp = 
  match extract_ecall exp |> snd with 
    | [tbl; key; mask; prio; action; forwarded_param] -> 
      check_entry_key tbl key;
      check_entry_key tbl mask;
      check_entry_prio prio;
      check_entry_action tbl action forwarded_param;
      table_ret_ty tbl
    | _ -> ty_err "TernaryTable.install and TernaryTable.update take exactly six arguments";
;;

let ternary_table_remove_cid = Cid.create ["TernaryTable"; "remove"] ;;
(* TernaryTable.remove(tbl, key, mask, prio) *)
let ternary_table_remove_check _ exp = 
  match extract_ecall exp |> snd with 
    | [tbl; key; mask; prio] -> 
      check_entry_key tbl key;
      check_entry_key tbl mask;
      check_entry_prio prio;
      table_ret_ty tbl
    | _ -> ty_err "TernaryTable.remove takes exactly four arguments";
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
(* the generate builtins: rewritten away by CCoreHandlers, never declared;
   their calls are typed here *)
let generate_arg_check name arg_tys _ exp =
  let _, args = extract_ecall exp in
  if List.length args <> List.length arg_tys then
    ty_err (name ^ ": wrong number of arguments");
  List.iter2
    (fun aty (arg : exp) ->
      if not (equiv_tys aty arg.ety) then ty_err (name ^ ": argument type mismatch"))
    arg_tys args;
  tunit
;;
let generate_self_cid = Cid.create ["generate_self"]
let generate_port_cid = Cid.create ["generate_port"]
let generate_switch_cid = Cid.create ["generate_switch"]
let generate_group_cid = Cid.create ["generate_group"]
let generate_delay_cid = Cid.create ["generate_delay"]
let generate_port_delay_cid = Cid.create ["generate_port_delay"]
let delay_size = 32

(* Sys functions *)
let sys_time_cid = System.sys_time_cid ;;
let sys_time_check _ exp = 
  let _, args = extract_ecall exp in
  if List.length args > 0 then 
    ty_err "sys_time takes no arguments";
  (tint 32)

let flood_cid = Cid.create ["flood"] ;;
let flood_check _ exp = 
  let _, args = extract_ecall exp in
  if List.length args <> 1 then 
    ty_err "flood takes exactly one argument";
  exp.ety


let builtin_checkers = 
  [
    table_create_cid, table_create_check;
    table_lookup_cid, table_lookup_check;
    table_install_cid, table_install_check;
    table_update_cid, table_install_check;
    table_remove_cid, table_remove_check;
    ternary_table_create_cid, table_create_check;
    ternary_table_lookup_cid, table_lookup_check;
    ternary_table_install_cid, ternary_table_install_check;
    ternary_table_update_cid, ternary_table_install_check;
    ternary_table_remove_cid, ternary_table_remove_check;
    array_create_cid, array_create_check;
    array_update_complex_cid, array_update_complex_check;
    generate_self_cid, generate_arg_check "generate_self" [tevent];
    generate_port_cid, generate_arg_check "generate_port" [tint CConfig.c_cfg.port_id_size; tevent];
    generate_switch_cid, generate_arg_check "generate_switch" [tint CConfig.c_cfg.switch_id_size; tevent];
    generate_group_cid, generate_arg_check "generate_group" [tint CConfig.c_cfg.port_id_size; tevent];
    generate_delay_cid, generate_arg_check "generate_delay" [tevent; tint delay_size];
    generate_port_delay_cid, generate_arg_check "generate_port_delay" [tint CConfig.c_cfg.port_id_size; tevent; tint delay_size];
    sys_time_cid, sys_time_check;
    flood_cid, flood_check
  ]
;;


(* builtin globals from the frontend that are not implemented as globals 
   in the backend. These should not be necessary in the final type checking pass. *)
let builtin_vars () =
  [
    Builtins.ingr_port_id, (CConfig.c_cfg.port_id_size) |> tint;
    (* the `checksum` hash seed; only valid as hash<16>'s first arg;
       implemented in CCoreSystem *)
    Builtins.checksum_id, tint 32;
  ]
;;

let get_checker fexp = 
  let cid = eval_exp fexp |> extract_vsymbol in
  List.assoc_opt cid builtin_checkers
;;

