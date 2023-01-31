(* 
  Inline the actions of each table match. 

  table foo {... actions = (bar); };
  action int bar(int c1)(int a1, int a2) {
    return a1 + c1;  
  }
  // ---> 
  table foo {... actions = (foo_bar); };
  action int foo_bar(int ct)() {
    return aarg1 + c1;
  }

  ...

  int outvar = table_match(foo, (k1, k2), (aarg1, aarg2));

  for each table match statement:
    - find the passed action args (exps) and the out variable (id)
    for each action of the table:
      - create a map from action params : passed args
      - generate an open function that contains: 
        - params = action's const params
        - body with params replaced by args
        - return expression replaced with statements to set the match statement's return vars
        - defined_vars contain the return vars (not really needed, just for debugging) 
 *)
open TofinoCore

module IdMap = Collections.IdMap

type env = {
  actions : (Id.t * action) list;
  tables : (Id.t * tbl_def) list;
}
let empty_env = {actions = []; tables = [];}


(* (copied from FunctionInlining.ml in frontend) *)
(* Given a mapping of variables to expressions, return the input statement
   with each of those expressions substituted in for the corresponding
   variable. Not context-sensitive, so make sure none of the expressions
   include variables that might change between lines! *)
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


let process_main env main_decl =
  (* find all the match statements.
     Look up table.
     Rebuild actions, table, and match statement. 
     Accumulate new action and table declarations, to return 
     with the new main. *)
  let lookup_acn exp = List.assoc
    (CoreSyntax.id_of_exp exp)
    env.actions
  in 
  let lookup_tbl exp = List.assoc
    (CoreSyntax.id_of_exp exp)
    env.tables
  in 
  let openfcn id params stmt vars span = 
    {td=TDOpenFunction(id, params, stmt, vars); tdspan=span; tdpragma=None;}
  in
  let new_decls = ref [] in
  let inliner = 
    object
      inherit [_] s_map as super
      method! visit_statement ctx stmt = 
        let stmt = super#visit_statement ctx stmt in
        match stmt.s with 
        | STableMatch(tbl_match) -> (
          let tbl = lookup_tbl tbl_match.tbl in
          let tbl_id = CoreSyntax.id_of_exp tbl_match.tbl in 
          (* transform all the actions into open functions *)
          let acnfcn_map_defs = List.fold_left 
            (fun acnfcn_map_defs eacn -> 
              let acn = lookup_acn eacn in
              (* map from ids (params) to expressions (args) *)
              let subst_map =
                List.fold_left 
                  (fun acc (id, arg) -> 
                    IdMap.add id arg acc) 
                  IdMap.empty 
                  (List.combine 
                    (List.split acn.aparams |> fst)
                    (tbl_match.args))
              in
              (* replace params with arg exps *)
              let action_body' = List.map
                (subst#visit_exp subst_map)
                acn.abody
              in
              (* create statements to set return variables *)
              let new_body = 
                match tbl_match.out_tys with
                | None -> 
                  List.fold_left2
                    (fun ret_stmt id exp -> 
                      CoreSyntax.sseq ret_stmt (CoreSyntax.sassign id exp))
                    CoreSyntax.snoop
                    tbl_match.outs
                    action_body'
                | Some tys -> 
                  List.fold_left2
                    (fun ret_stmt (id, ty) exp -> 
                      CoreSyntax.sseq ret_stmt (CoreSyntax.slocal id ty exp))
                    CoreSyntax.snoop
                    (List.combine tbl_match.outs tys)
                    action_body'
              in
              (* create the open function for the action *)
              let acn_fcn_id = (Id.concat_names "_" tbl.tid acn.aid) in
              let f = openfcn 
                acn_fcn_id
                (acn.aconst_params)
                new_body
                []
                Span.default
              in              
              acnfcn_map_defs@[((acn.aid, acn_fcn_id), f)])
            []
            tbl.tactions
          in (* end definition of action_functions *)
          (* we have the new action functions, now update the table to use them *) 
          let acn_fcn_idmap, acn_decls = List.split acnfcn_map_defs in
          let tty' = match tbl.tty.raw_ty with
            | TTable(tbl_ty) -> {tbl.tty with raw_ty=TTable({tbl_ty with tparam_tys = [];})}
            | _ -> error "[table-action inlining] table object does not have table type."
          in

          let tactions' = List.fold_left2
            (fun tactions' id ty -> tactions'@[CoreSyntax.exp_of_id id ty])
            []
            (List.split acn_fcn_idmap |> snd)
            (List.map (fun eacn -> eacn.ety) tbl.tactions)
          in

          let tbl' = {tbl with 
            tty = tty';
            tactions = tactions';
            tdefault = (
              (List.assoc (Cid.to_id (fst tbl.tdefault)) acn_fcn_idmap |> Cid.id),
              (snd tbl.tdefault));} 
          in 
          let tbl_decl = tdecl_of_decl
            (CoreSyntax.dglobal_sp 
              tbl_id 
              tbl'.tty 
              (CoreSyntax.exp (ETableCreate(tbl')) tbl'.tty) 
              Span.default)
          in
          (* we have the new actions and table...
            the match statement doesn't actually need to change. 
            The args and outs aren't really needed, but we can leave 
            them for debugging. *)
          new_decls := (!new_decls)@acn_decls@[tbl_decl];
          stmt)
        | _ -> 
          stmt
    end
  in
  let main_decl' = inliner#visit_tdecl env main_decl in
  (!new_decls)@[main_decl']
;;


let rec _process env tdecls =
  match tdecls with
    | [] -> []
    | tdecl::tdecls -> (
      match tdecl.td with
      (* process_main will generate copies of this action as needed, 
         so delete it. *)
      | TDAction(acn) -> 
        let actions' = (acn.aid, acn)::env.actions in
        (_process {env with actions=actions'} tdecls)
      (* process_main will generate a new copy of this table, 
         using the new actions, as needed. So delete it. *)
      | TDGlobal(_, _, {e=ETableCreate(tbl);}) -> 
        let tables' = (tbl.tid, tbl)::env.tables in
        (_process {env with tables=tables'} tdecls)
      | TDMain(_) -> 
        (process_main env tdecl)@(_process env tdecls)
      | _ -> 
        tdecl::(_process env tdecls)
    )
;;

let process tdecls =
  _process empty_env tdecls
;;
