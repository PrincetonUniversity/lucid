(* 
  Translate actions to open functions.
  Input form: 
    - each action is bound to a single table
    - each table has a single match statement
  Input form implies that each action is used by a 
  single match statement. So, we basically just go 
  through those match statements and bind parameters.

  Output: 
    - each action is replaced with a function that 
      has the same ID. 
*)
open TofinoCore

module IdMap = Collections.IdMap

type env = {
  actions : (Id.t * action) list;
  tables : (Id.t * Tables.core_tbl_def) list;
}
let empty_env = {actions = []; tables = [];}


(* (copied from FunctionInlining.ml in frontend) *)
let subst =
  object
    inherit [_] s_map as super
    method! visit_exp (env : exp IdMap.t) exp =
      match exp.e with
      | EVar(x) -> (
        match IdMap.find_opt (Cid.to_id x) env with
        | Some exp' -> exp'
        | None -> exp
      )
      | _ -> exp
  end
;;

let lookup_acn env exp = List.assoc
  (CoreSyntax.id_of_exp exp)
  env.actions
;;
let lookup_tbl env exp = List.assoc
  (CoreSyntax.id_of_exp exp)
  env.tables
;; 
let openfcn id params stmt vars span = 
  let _ = vars in 
  {td=TDOpenFunction(id, params, stmt); tdspan=span; tdpragma=[]; }
;;

(* convert an action into a function *)
let action_to_function env (tmatch: Tables.core_tbl_match) acn_exp =
  let acn = lookup_acn env acn_exp in
  (* map from ids (params) to expressions (args) *)
  let subst_map =
    List.fold_left 
      (fun acc (id, arg) -> 
        IdMap.add id arg acc) 
      IdMap.empty 
      (List.combine 
        (List.split acn.aparams |> fst)
        (tmatch.args))
  in
  (* replace params with arg exps *)
  let action_body' = List.map
    (subst#visit_exp subst_map)
    acn.abody
  in
  (* create statements to set return variables *)
  let new_body = 
    match tmatch.out_tys with
    | None -> 
      List.fold_left2
        (fun ret_stmt id exp -> 
          CoreSyntax.sseq ret_stmt (CoreSyntax.sassign (Cid.id id) exp))
        CoreSyntax.snoop
        tmatch.outs
        action_body'
    | Some tys -> 
      List.fold_left2
        (fun ret_stmt (id, ty) exp -> 
          CoreSyntax.sseq ret_stmt (CoreSyntax.slocal id ty exp))
        CoreSyntax.snoop
        (List.combine tmatch.outs tys)
        action_body'
  in  
  (* create the open function for the action *)
  (* note: names are already table-specific! *)
  openfcn 
    acn.aid
    (acn.aconst_params)
    new_body
    []
    Span.default
;;


(* transform all actions called by table matches 
   in the statement into functions *)
let actions_to_functions env tdecl =
  let fcn_decls = ref [] in
  let v =
    object
      inherit [_] s_iter as super
      method! visit_STupleAssign env tassn = 
        match (Tables.s_to_tbl_match_opt (STupleAssign(tassn))) with 
        | Some(tmatch) -> 
            let tbl = lookup_tbl env tmatch.tbl in
            fcn_decls := (!fcn_decls)@(
              List.map (action_to_function env tmatch) tbl.tactions)
        | None -> ()
      (* method! visit_STableMatch env tmatch =
        let tbl = lookup_tbl env tmatch.tbl in
        fcn_decls := (!fcn_decls)@(
          List.map (action_to_function env tmatch) tbl.tactions) *)
    end
  in
  v#visit_tdecl env tdecl;
  !fcn_decls
;;

let actionty_to_functionty aty = 
  match aty.raw_ty with
  | TActionConstr(acn_ctor_ty) -> 
    {aty with raw_ty=TFun({arg_tys = acn_ctor_ty.aconst_param_tys; ret_ty=CoreSyntax.ty TBool;})}
  | _ -> aty
;;

let rec _process env tdecls =
  match tdecls with
    | [] -> []
    | tdecl::tdecls ->
      match tdecl.td with
      (* add the action to context and delete its declaration -- 
         we will generate the appropriate function from the 
         match statement that uses the action. *)
      | TDActionConstr(acn) -> 
        let actions' = (acn.aid, acn)::env.actions in
        (_process {env with actions=actions'} tdecls)
      | TDGlobal(id, gty, etblconstr) -> 
        let tbl = Tables.dglobal_params_to_tbl_def id etblconstr in
        (* remember the table. *)
        let tables' = (tbl.tid, tbl)::env.tables in
        let env' = {env with tables=tables'} in
        (* transform the type of the action references *)
        let tactions' = List.map
          (fun eacn -> {eacn with ety=actionty_to_functionty eacn.ety;})
          tbl.tactions
        in
        let tbl' = {tbl with tactions=tactions'} in
        let etblconstr' = Tables.tbl_def_to_econstr tbl' in
        let etblconstr' = {etblconstr with e=etblconstr'.e} in
        let tdecl' = {tdecl with td=TDGlobal(id, gty, etblconstr')} in
        tdecl'::(_process env' tdecls)
      (* | TDGlobal(id, gty, {e=ETableCreate(tbl); ety=ety; espan=espan;}) -> 
        (* remember the table. *)
        let tables' = (tbl.tid, tbl)::env.tables in
        let env' = {env with tables=tables'} in
        (* transform the type of the action references *)
        let tactions' = List.map
          (fun eacn -> {eacn with ety=actionty_to_functionty eacn.ety;})
          tbl.tactions
        in
        let tbl' = {tbl with tactions=tactions'} in
        let tdecl' = {tdecl with td=TDGlobal(id, gty, {e=ETableCreate(tbl'); ety; espan})} in
        tdecl'::(_process env' tdecls) *)
      (* transform all the actions that 
         appear in table_install statements *)
      | _ -> 
        let action_decls = actions_to_functions env tdecl in
        action_decls@[tdecl]@(_process env tdecls)
;;

let process tdecls =
  _process empty_env tdecls
;;
let process_core prog =
  List.map (fun comp -> {comp with comp_decls = process comp.comp_decls}) prog
;;
