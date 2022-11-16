(* convert tables to inlined tables 

  We inline tables because without tuples or records in the backend 
  ir, non-inlined tables cannot return / modify more than 1 variable. 
*)

(* 
Left off here. TODO: table inlining.

*)

open Syntax
open Collections

module CMap = Collections.CidMap

type acnrec = {
  aid : id; 
  rty : ty;
  cps : params;
  dps : params;
  stmt : statement;  
}
type acnmap = acnrec CMap.t

type aty = (string * raw_ty list)

type tblrec = {
  acntys : aty list;
  acnrecs: acnrec list;
  n_keys : int;
  entries: case list;
}

type tblmap = tblrec CMap.t

type ctx = {
  acns : acnmap;
  tbls : tblmap;
}
let empty_ctx = {acns = CMap.empty; tbls = CMap.empty;}

let tblty_rewriter = 
  (* translate table_type declarations 
     so that they are inline-able (no arg or ret types) *)
  let v =
    object (_)
      inherit [_] s_map
      method !visit_TTable _ key_size _ _ action_tys num_entries = 
        TTable({key_size; arg_ty=[]; ret_ty=TVoid; action_tys; num_entries})
    end
  in
  v#visit_decls ()

let action_deleter decls = 
  List.filter 
    (fun decl -> match decl.d with 
      | DAction _ -> false
      | _ -> true)
    decls
;;


let delete_returns s = 
  let v =
    object (_)
      inherit [_] s_map as super
      method! visit_SRet _ _ = SNoop
    end
  in
  v#visit_statement () s
;;  


(* create an action from the action type and record of a non-inlined table *)
let build_inline_action arg_exps (retvar_cid_opt: Cid.t option) (acnty:aty) (acnrec:acnrec) : statement list * action =
  (* Compute bindings for arguments and substitute them into the body *)
  let params, body = acnrec.dps, acnrec.stmt in 
  let args = List.combine params arg_exps in
  List.iter
    (fun ((_, pty), e) -> TyperUnify.unify_ty e.espan pty (Option.get e.ety))
    args;
  (* Replace any compound arguments with new intermediate variables, and include
     the creation statements for those variables *)
  let intermediate_var_decls, args =
    List.map
      (fun ((id, ty), e) ->
        if SyntaxUtils.is_compound e || FunctionInlining.is_assigned id acnrec.stmt
        then (
          let new_id = id in
          Some (slocal new_id ty e), (id, EVar (Id new_id)))
        else None, (id, e.e))
      args
    |> List.split
  in    
  (* Compute bindings for arguments and substitute them into the body *)
  let subst_map =
    List.fold_left (fun acc (id, arg) -> IdMap.add id arg acc) IdMap.empty args
  in
  let body = FunctionInlining.subst#visit_statement subst_map body in
  let body = match retvar_cid_opt with 
    | Some(retvar_cid) -> FunctionInlining.replace_returns (Cid.to_id retvar_cid) body 
    | None -> delete_returns body 
    (* if there's no return variable, delete the return statement *)
  in
  (* note: action keeps const params *)
  let acn = (fst acnty, (acnrec.cps, body)) in
  (List.filter_map (fun v -> v) intermediate_var_decls), acn
;;

let build_inline_table ctx ty args retvar_cid_opt = 
  (* retvar_cid should be passed in -- the outer method decides if it needs 
     to be created *)
  (* let retvar_cid = Cid.fresh ["tbl_ret"] in  *)

  (* inline the table called by args of type ty *)
  let acn_tys = match ty.raw_ty with
    | TTable(t) -> t.action_tys
    | _ -> error "[build_inline_table] type is not a table type"
  in 
  let etbl, eargs = match args with
    | etbl::eargs -> etbl, eargs
    | _ -> error "[build_inline_table] wrong number of args"
  in 
  let tblrec = match etbl.e with
    | EVar(tbl_cid) -> CMap.find tbl_cid ctx.tbls
    | _ -> error "[TableInlining.build_inline_table] expected a table variable as first arg, got something else."
  in
  let ekeys, arg_exps = Batteries.List.split_at tblrec.n_keys eargs in

  let intermediate_decl_stmts, inline_actions = List.map2 
      (build_inline_action arg_exps retvar_cid_opt)
      acn_tys
      (tblrec.acnrecs)
    |> List.split
  in
  let intermediate_decl_stmts = List.flatten intermediate_decl_stmts in
  let ss = List.fold_left
    (fun ss s -> sseq_sp ss s (Span.extend ss.sspan s.sspan))
    snoop
    intermediate_decl_stmts
  in

  (* construct the table statement and return it along with intermediate decls *)
  let inline_tbl_stmt = (statement (SInlineTable(ty, etbl, ekeys, inline_actions, tblrec.entries)))
  in 
  sseq_sp ss inline_tbl_stmt (Span.extend ss.sspan inline_tbl_stmt.sspan)
;;



let apply_inliner ctx =
  (* convert assign or local statements with table apply expressions 
     into inline table statements *)
  let v =
    object
      inherit [_] s_map as super
      method! visit_statement ctx stmt =
        let stmt = super#visit_statement ctx stmt in 
          match stmt.s with
          | SAssign(id, exp) -> (
            match exp.e with 
            | ETableApply(ty, args) -> 
              let retvar_cid = Cid.id id in
              let inline_tbl_stmt = build_inline_table ctx ty args (Some retvar_cid) in
              inline_tbl_stmt
            | _ -> {stmt with s=SAssign(id, exp)})
          | SLocal(id, ret_ty, exp) -> (
            match exp.e with 
            | ETableApply(ty, args) -> (
              let retvar_cid = Cid.id id in
              match ret_ty.raw_ty with 
              | TVoid -> 
                (* note: this is a type error *)
                build_inline_table ctx ty args None
              | _ -> 
                (* make a statement setting the id to a default value for the type *)
                let retvar_create_stmt =
                  slocal id ret_ty (SyntaxUtils.default_expression ret_ty)
                in
                let inline_tbl_stmt = build_inline_table ctx ty args (Some retvar_cid) in
                sseq retvar_create_stmt inline_tbl_stmt
              )
            | _ -> {stmt with s=SLocal(id, ret_ty, exp)})
          | SUnit(exp) -> (
            (* note: this probably shouldn't happen. Tables don't modify mutables *)
            match exp.e with 
            | ETableApply(ty, args) -> build_inline_table ctx ty args None
            | _ -> {stmt with s=SUnit(exp)})
          | _ -> stmt
    end
  in
  v#visit_decl ctx
;;

let inline_decl ctx decl = 
  match decl.d with

    (* remember the action declaration *)
    | DAction(aid, rty, cps, (dps, stmt)) -> 
      let acns = CMap.add
        (Cid.id aid)
        {aid; rty; cps; dps; stmt}
        ctx.acns
      in 
      {ctx with acns = acns;}, decl
    (* remember the table declaration with its actions, translate into an inline create *)
    | DGlobal(id, ty, exp) -> (
      match exp.e with 
      | ECreateTable(t) -> (
        let n_keys, acntys = match ty.raw_ty with 
          | TTable(t) -> List.length t.key_size, t.action_tys
          | _ -> error "Type error: assigning a table to a variable that is not a table type."
        in 
        let acnrecs = List.map 
          (fun eacn -> 
            match eacn.e with 
            | EVar(acn_cid) -> (
              match (CMap.find_opt acn_cid ctx.acns) with
              | None -> error@@"could not find action: "^(Printing.cid_to_string acn_cid);
              | Some(acnrec) -> acnrec)
            | _ -> error@@"action must be an expression")
          t.tactions
        in
        let tbls = CMap.add (Cid.id id) {acnrecs; acntys; n_keys;entries=t.tentries} ctx.tbls in
        let new_exp = {exp with e=ECreateTableInline(ty)} in
        let new_dglobal = {decl with d=DGlobal(id, ty, new_exp)} in
        {ctx with tbls}, new_dglobal)
      | _ -> ctx, decl (* other globals don't change *)
    )
    (* everywhere else, 
       1. replace table_match with inline_table_match 
       2. rewrite non-inlineable table types (*DO THIS IN ANOTHER PASS!*)  *)
    | _ -> 
      ctx, apply_inliner ctx decl
       (* |> tblty_rewriter *)

let inline_prog (ds: decl list) =
  let _, decls = List.fold_left
    (fun (ctx, decls) decl -> 
      let ctx, decl = inline_decl ctx decl in
      ctx, decl::decls)
    (empty_ctx, [])
    ds
  in
  (* rewrite all the table declarations to be units *)
  let decls = List.rev decls |> tblty_rewriter |> action_deleter in
  decls 
;;