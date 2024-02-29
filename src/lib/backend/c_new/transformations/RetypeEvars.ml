(* helper function: update all the types of evar expressions in the program  *)
open CCoreSyntax
let err = Console.error ;;

type env = (cid * ty) list
let ccid = Cid.id

let env_add env cid ty  = 
  (* make sure cid is not already in the env *)
  if List.exists (fun (c, _) -> Cid.equal c cid) env then 
    err (Printf.sprintf "duplicate variable binding for %s (perhaps you now need a unique naming pass)" (Cid.to_string cid))
  else
    (cid, ty) :: env
;;
let env_adds env cids tys = 
  List.fold_left2 (fun env cid ty -> env_add env cid ty) env cids tys
;;

let env_exists env cid = List.mem_assoc cid env
;;
let env_find env cid = List.assoc cid env
let env_find_opt env cid = List.assoc_opt cid env

let env_add_params env (params : params) = 
  let ids, tys = List.split params in
  let cids = List.map ccid ids in
  env_adds env cids tys
;;

exception UnboundVar of cid

let update_var_types decls = 
  (* update  *)
  let rec visit_exp env exp = match exp.e with 
  (* this is the main case we are concerned about -- 
     update the type of EVars, in case the variable's type has changed. 
     The rest of the cases are just recursing through the tree, 
     retyping evars, and propagating any changes up. *)
  | EVar(cid) -> (
    match (env_find_opt env cid) with
    | Some ty -> {exp with ety=ty}, env
    | None -> 
      raise (UnboundVar cid)
  )
  (* | EVal({v=VClosure({env=cenv; params; fexp}); vspan}) -> 
      (* visit closure body *)
      let env' = env_add_params env (List.map (fun (id, value) -> (id, value.vty)) cenv) in
      let env' = env_add_params env' params in
      let fexp' = visit_exp env' fexp |> fst in
      (* recalculate the closures function type, propagate up to expression *)
      let vty' = tfun (List.map snd params) fexp'.ety in
      let v' = VClosure({env=cenv; params; fexp=fexp'}) in
      let value' = {v=v'; vty=vty'; vspan} in
      let e' = EVal(value') in
      {exp with e=e'; ety=vty'}, env
    | EClosure({env=cenv; params; fexp}) -> (
      let env' = env_add_params env (List.map (fun (id, exp) -> (id, exp.ety)) cenv) in
      let env' = env_add_params env' params in
      let fexp' = visit_exp env' fexp |> fst in
      (* recalculate the closures function type, propagate up to expression *)
      (* print_endline ("closure expression type: ");
      print_endline (FCorePrinting.show_ty exp.ety); *)
      let ety' = match exp.ety.raw_ty with 
        | TName(tcid, [t_env; t_fun]) -> 
          let func_kind = kind_of_tfun t_fun.raw_ty in
          let t_fun_raw =( tfun_kind (List.map snd params) fexp'.ety func_kind).raw_ty in
          let t_fun = {t_fun with raw_ty = t_fun_raw} in
          {exp.ety with raw_ty = TName(tcid, [t_env; t_fun])}
        | raw_ty -> 
          let func_kind = kind_of_tfun raw_ty in 
          let raw_ty = (tfun_kind (List.map snd params) fexp'.ety func_kind).raw_ty in
          {exp.ety with raw_ty}
      in        
      (* print_endline ("new closure type: ");
      print_endline (FCorePrinting.show_ty ety'); *)
      {exp with e = EClosure({env=cenv; params; fexp=fexp'}); ety=ety'}, env
    ) *)
    | EVal _ -> exp, env
    | ERecord{labels; es} -> 
        (* recurse on members *)
        let es = List.map (fun exp -> visit_exp env exp |> fst) es in
        (* update record type *)
        let ety = match labels with 
        | Some(labels) ->         
            trecord labels (List.map (fun exp -> exp.ety) es)
        | None -> 
            ttuple (List.map (fun exp -> exp.ety) es)
        in
        {exp with ety}, env
    | ECall{f; args; call_kind;} ->
      (* recurse on function and args *)
      (* if f is a builtin, it may be unbound *)
      let f = try visit_exp env f |> fst with 
        | UnboundVar _ -> f
      in
      let args = List.map (fun exp -> visit_exp env exp |> fst) args in
      (* update expression type based on function return type *)
      let ety = match call_kind, f.ety.raw_ty with
        | CNormal, TFun{ret_ty} -> 
          ret_ty
        | CNormal, _ -> err "the target of a function call should be a function"
        | CEvent, _ -> f.ety
      in
      let e = ECall{f; args; call_kind;} in
      {exp with e; ety}, env
    | EOp(op, args) -> 
      (* recurse on args *)
      let args = List.map (fun exp -> visit_exp env exp |> fst) args in
      (* recompure exp and type *)
      let exp' = eop op args in
      ewrap exp.espan exp', env
  in
  (* return updated statement, updated environment, and return type *)
  let rec visit_stmt env stmt = 
    match stmt.s with 
    | SNoop -> stmt, env, None
    | SUnit(exp) -> 
      (* bindings inside the expression are locally scoped *)
      let exp' = visit_exp env exp |> fst in
      {stmt with s = SUnit(exp')}, env, None
    | SAssign{ids; new_vars=true; exp} -> 
      (* visit expression, ignore env updates *)      
      let exp' = visit_exp env exp |> fst in
      let tys' = List.map (fun id -> env_find env id) ids in
      (* add new vars to env *)
      let env' = env_adds env ids tys' in
      {stmt with s = SAssign{ids; tys=tys'; new_vars=true; exp = exp'}}, env', None
    | SAssign{ids; new_vars=false; exp} -> 
      (* still have to recurse on exp *)
      let exp' = visit_exp env exp |> fst in
      (* update declared types, in case they have changed *)
      let tys' = List.map (fun id -> env_find env id) ids in
      {stmt with s = SAssign{ids; tys=tys'; new_vars=false; exp = exp'}}, env, None
    | SIf(exp, stmt1, stmt2) ->
      let exp' = visit_exp env exp |> fst in
      (* new vars inside of if block are locally scoped *)
      let stmt1', _, ret1 = visit_stmt env stmt1 in
      let stmt2', _, _ = visit_stmt env stmt2 in
      (* both return types should agree *)
      {stmt with s = SIf(exp', stmt1', stmt2')}, env, ret1
    | SMatch(exp, branches) ->
      let exp' = visit_exp env exp |> fst in
      (* new vars inside of match branches are locally scoped *)
      let rty = ref None in
      let branches' = List.map (fun (pats, stmt) -> 
        let stmt' = match stmt with 
          | stmt -> 
            let stmt, _, ret = visit_stmt env stmt in
            rty := ret;
            stmt
          (* | E(exp) -> E(visit_exp env exp |> fst) *)
        in
        (pats, stmt')
      ) branches in
      {stmt with s = SMatch(exp', branches')}, env, (!rty)
    | SSeq(stmt1, stmt2) ->
      let stmt1', env', _ = visit_stmt env stmt1 in
      let stmt2', env'', ret2 = visit_stmt env' stmt2 in
      {stmt with s = SSeq(stmt1', stmt2')}, env'', ret2
    | SRet(exp_opt) ->
      let exp_opt' = Option.map (fun exp -> visit_exp env exp |> fst) exp_opt in
      let rty = match exp_opt' with
        | Some exp -> Some(exp.ety)
        | None -> None
    in
      {stmt with s = SRet(exp_opt')}, env, rty
  in

  let visit_decl env decl = match decl.d with
  (* functions: add params; visit body; discard params and add function to env *)
  | DFun(fkind, id, rty, params, stmt_opt) -> (
    let local_env = env_add_params env params in 
    let stmt_opt', rty_opt = match stmt_opt with  
        (* declared function -- type _might_ change *)
      | Some stmt -> 
        let stmt, _, rty_opt = visit_stmt local_env stmt in
        Some(stmt), rty_opt
        (* extern -- type can't change *)
      | None -> None, Some(rty)
    in
    let rty' = match rty_opt with
      | Some rty' -> 
        (* print_endline ("old rty: "^(FCorePrinting.show_ty rty));
        print_endline ("new rty: "^(FCorePrinting.show_ty rty')); *)
        rty'
      | None -> tunit ()
    in
    let decl' = {decl with d = DFun(fkind, id, rty', params, stmt_opt')} in
    (* update the functions type and put it in the env *)
    let fty = tfun_kind (List.split params |> snd) rty' fkind in
    let env' = env_add env (ccid id) fty in
    decl', env'
  )
  | DVar(id, ty, exp_opt) -> 
    let exp_opt' = Option.map (fun exp -> visit_exp env exp |> fst) exp_opt in
    let ty' = match exp_opt' with
      | Some exp -> exp.ety
      | None -> ty
    in
    let decl' = {decl with d = DVar(id, ty', exp_opt')} in
    let env' = env_add env (ccid id) ty' in
    decl', env'
  | DTy _ -> decl, env 
  | DEvent{evconstrid} -> 
    let decl' = decl in
    let env' = env_add env (ccid evconstrid) tevent in
    decl', env'
  in
  let decls', _ = List.fold_left (fun (decls, env) decl -> 
    let decl', env' = visit_decl env decl in
    decl'::decls, env'
    ) ([], []) decls
  in
  List.rev decls'
;;
