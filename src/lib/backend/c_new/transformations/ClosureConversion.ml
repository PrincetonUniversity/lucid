(* Depreciated -- no more closures *)
open FCoreSyntax
let err = Console.error ;;

(* closure conversion pass
    walk the program, tracking variable bindings in a scoped environment. 
    whenever you reach a closure, find all the free variables in its 
    body and add the appropriate expressions to the closure's context.   
*)
let ccid = Cid.id
type env = cid list

let env_add env cid  = 
  (* make sure cid is not already in the env *)
  if List.exists (Cid.equal cid) env then 
    err (Printf.sprintf "closure conversion: duplicate variable binding for %s" (Cid.to_string cid))
  else
    cid :: env
;;
let env_adds env cids = 
  List.fold_left (fun env cid -> env_add env cid) env cids
;;

let env_exists env cid = 
  List.exists (Cid.equal cid) env


let env_add_params env (params : params) = 
  env_adds env (List.map (fun (id, _) -> (ccid id)) params)
;;   

(* get the unbound variables in an expression, given that 
    the variables in bound_vars have been bound *)
let rec get_unbound_vars bound_vars exp = 
  match exp.e with 
  (* closures: bind environment and parameter vars, visit expression *)
  | EClosure{env; params; fexp} -> 
    let bound_vars = 
      (List.map (fun (id, _) -> (ccid id)) env) @
      (List.map (fun (id, _) -> (ccid id)) params) 
    in
    get_unbound_vars bound_vars fexp
  | EVal({v=VClosure({env; params; fexp})}) -> 
    let bound_vars = 
      (List.map (fun (id, _) -> (ccid id)) env) @
      (List.map (fun (id, _) -> (ccid id)) params) 
    in
    get_unbound_vars bound_vars fexp
  | EVal _ -> [] (* only closure values have unbound vars *)
  | EVar(cid, _) -> 
    if env_exists bound_vars cid then [] else [(cid, Cid.fresh (Cid.names cid)), exp.ety]
  | ERecord{es} -> 
    List.concat (List.map (get_unbound_vars bound_vars) es)
  | ECall{f; args} -> 
    (get_unbound_vars bound_vars f) @
    (List.concat (List.map (get_unbound_vars bound_vars) args))
  | EOp(_, args) -> 
    List.concat (List.map (get_unbound_vars bound_vars) args)
;;

let vclosure_to_eclosure = function 
  | VClosure{env;params;fexp} ->
  let env = List.map 
    (fun (id, value) -> (id, eval value))
    env
  in
  EClosure({env; params; fexp})
  |  _-> err "not a vclosure"
;;


let capture_vars decls = 
  let rec visit_exp env exp = match exp.e with 
    | EClosure{env=cenv; params; fexp} -> 
      (* get unbound variables in the closure *)
      let unbound_var_tys = get_unbound_vars [] exp in
      (* move those unbound vars into the closure environment *)
      let cenv_ext = List.map (fun ((outer_cid, inner_cid), ty) -> (Cid.to_id inner_cid, local_var outer_cid ty)) unbound_var_tys in
      let cenv' = cenv@cenv_ext in
      (* recurse on closure body, adding only the declared params (captured vars should already be in the environment) *)
      let env' = env_add_params env params in
      let fexp' = visit_exp env' fexp |> fst in     
      (* return updated closure with unchanged env *)
      let e' = EClosure{env=cenv'; params; fexp=fexp'} in
      {exp with e = e'}, env
    | EVal({v}) when (match v with VClosure _ -> true | _ -> false) ->
      let exp' = {exp with e=vclosure_to_eclosure v} in
      visit_exp env exp'
    | EVar _ -> exp, env
    | EVal _ -> exp, env
      (* recurse cases *)
    | ERecord{labels; es} -> 
      let es = List.map (fun exp -> visit_exp env exp |> fst) es in
      {exp with e=ERecord{labels; es}}, env
    | ECall{f; args; call_kind} -> 
      let f = visit_exp env f |> fst in
      let args = List.map (fun exp -> visit_exp env exp |> fst) args in
      {exp with e=ECall{f; args; call_kind}}, env
    | EOp(op, args) -> 
      let args = List.map (fun exp -> visit_exp env exp |> fst) args in
      {exp with e=EOp(op, args)}, env
    in
  let rec visit_statement env stmt = match stmt.s with 
    | SNoop -> stmt, env
    | SUnit(exp) -> 
      (* bindings inside the expression are locally scoped *)
      let exp' = visit_exp env exp |> fst in
      {stmt with s = SUnit(exp')}, env
    | SAssign{ids; tys; new_vars=true; exp} -> 
      (* visit expression, ignore env updates *)      
      let exp' = visit_exp env exp |> fst in
      let env' = env_adds env ids in
      (* add new vars to env *)
      {stmt with s = SAssign{ids; tys; new_vars=true; exp = exp'}}, env'
    | SAssign{ids; tys; new_vars=false; exp} -> 
      (* still have to recurse on exp *)
      let exp' = visit_exp env exp |> fst in
      {stmt with s = SAssign{ids; tys; new_vars=false; exp = exp'}}, env      
    | SIf(exp, stmt1, stmt2) -> 
      let exp' = visit_exp env exp |> fst in
      (* new vars inside of if block are locally scoped *)
      let stmt1' = visit_statement env stmt1 |> fst in
      let stmt2' = visit_statement env stmt2 |> fst in
      {stmt with s = SIf(exp', stmt1', stmt2')}, env
    | SMatch(exp, branches) -> 
      let exp' = visit_exp env exp |> fst in
      (* new vars inside of match branches are locally scoped *)
      let branches' = List.map (fun (pats, stmt) -> 
        let stmt' = match stmt with 
          | S(stmt) -> S(visit_statement env stmt |> fst)
          | E(exp) -> E(visit_exp env exp |> fst)
        in
        (pats, stmt')
      ) branches in
      {stmt with s = SMatch(exp', branches')}, env
    | SSeq(stmt1, stmt2) ->
      let stmt1', env' = visit_statement env stmt1 in
      let stmt2', env'' = visit_statement env' stmt2 in
      {stmt with s = SSeq(stmt1', stmt2')}, env''
    | SRet(exp_opt) -> 
      let exp_opt' = Option.map (fun exp -> visit_exp env exp |> fst) exp_opt in
      {stmt with s = SRet(exp_opt')}, env
    (* | _ -> err "not done" *)
  in 
  let visit_decl env decl = match decl.d with
  (* functions: add params; visit body; discard params and add function to env *)
  | DFun(fkind, id, ty, params, stmt_opt) -> (
    let local_env = env_add_params env params in 
    let stmt_opt' = match stmt_opt with  
      | Some stmt -> Some(visit_statement local_env stmt |> fst)
      | None -> None
    in
    let env' = env_add env (ccid id) in
    let decl' = {decl with d = DFun(fkind, id, ty, params, stmt_opt')} in
    decl', env'
  )
  (* vars and events: just add to the environment *)
  | DVar(id, ty, exp_opt) ->
    let exp_opt' = Option.map (fun exp -> visit_exp env exp |> fst) exp_opt in
    let d' = DVar(id, ty, exp_opt') in
    let env' = env_add env (ccid id)in 
    {decl with d=d'}, env'
  | DEvent{evconstrid} -> 
    let env' = env_add env (ccid evconstrid) in
    decl, env'
  | DTy _ -> decl, env (* this is a value environment, not a type environment *)
  in

  let decls', _ = List.fold_left (fun (decls, env) decl -> 
    let decl', env' = visit_decl env decl in
    decl'::decls, env'
  ) ([], []) decls 
  in
  List.rev decls'
;;

let eenv_to_rec_ty env = 
  let fields, tys = List.map (fun (id, exp) -> (id, exp.ety)) env |> List.split in
  trecord fields tys
;;
let venv_to_rec_ty env = 
  let fields, tys = List.map (fun (id, value) -> (id, value.vty)) env |> List.split in
  trecord fields tys
;;
(* we want to change the type of the action closure to "Closure(ctx_struct, fun_ty)" *)
let add_closure_types decls = 
  let v = object 
    inherit [_] s_map as super 
    method! visit_exp () exp = 
      let exp = super#visit_exp () exp in
      match exp.ety.raw_ty, exp.e with
      | TFun{func_kind=FAction;}, EClosure({env}) -> 
        let ctx_ty = eenv_to_rec_ty env in
        let fun_ty = exp.ety in
        {exp with ety=tclosure ctx_ty fun_ty}
      | TFun{func_kind=FAction;}, EVal({v=VClosure{env}}) -> 
        let ctx_ty = venv_to_rec_ty env in
        let fun_ty = exp.ety in
        {exp with ety=tclosure ctx_ty fun_ty}
      | _ -> exp
    end
  in
  v#visit_decls () decls |> RetypeEvars.update_var_types
