open Syntax
open SyntaxUtils
open Batteries

let rec inline_exp e =
  match e.e with
  | EStmt (s1, e1) ->
    let s1' = inline_stmt s1 in
    let stmt, e1' = inline_exp e1 in
    sseq s1' stmt, e1'
  (* Everything else is just recursive *)
  | EVal _ | EInt _ | EVar _ | ESizeCast _ -> snoop, e
  | EOp (op, es) ->
    let stmt, es' = inline_exps es in
    stmt, { e with e = EOp (op, es') }
  | ECall (cid, es, u) ->
    let stmt, es' = inline_exps es in
    stmt, { e with e = ECall (cid, es', u) }
  | EHash (sz, es) ->
    let stmt, es' = inline_exps es in
    stmt, { e with e = EHash (sz, es') }
  | EFlood e ->
    let stmt, e' = inline_exp e in
    stmt, { e with e = EFlood e' }
  | ERecord lst ->
    let strs, es = List.split lst in
    let stmt, es = inline_exps es in
    stmt, { e with e = ERecord (List.combine strs es) }
  | EWith (e1, lst) ->
    let stmt1, e1' = inline_exp e1 in
    let strs, es = List.split lst in
    let stmt2, es = inline_exps es in
    sseq stmt1 stmt2, { e with e = EWith (e1', List.combine strs es) }
  | EProj (e1, str) ->
    let stmt, e1' = inline_exp e1 in
    stmt, { e with e = EProj (e1', str) }
  | EVector es ->
    let stmt, es' = inline_exps es in
    stmt, { e with e = EVector es' }
  | EComp _ ->
    Console.error_position
      e.espan
      "Internal error: cannot eliminate EStmts inside a vector comprehension!"
  | EIndex (e1, sz) ->
    let stmt, e1' = inline_exp e1 in
    stmt, { e with e = EIndex (e1', sz) }
  | ETuple es ->
    let stmt, es' = inline_exps es in
    stmt, { e with e = ETuple es' }
  | ETableCreate tc -> 
    let acn_stmt, tactions = inline_exps tc.tactions in
    let def_cid, def_args, def_flag = unpack_default_action tc.tdefault.e in
    let def_stmt, def_args = inline_exps def_args in
    let tdefault = {tc.tdefault with e = ECall(def_cid, def_args, def_flag)} in
    sseq 
      acn_stmt def_stmt
      ,{e with e = ETableCreate({tc with tactions; tdefault})}  
  | ETableMatch(tm) -> 
    let stmt, tm' = inline_tbl_match tm in
    stmt, {e with e = ETableMatch(tm')}
  | EPatWild _ -> snoop, e

and inline_tbl_match tm =
    let tbl_stmt, tbl = inline_exp tm.tbl in
    let keys_stmt, keys = inline_exps tm.keys in
    let args_stmt, args = inline_exps tm.args in
    sseq tbl_stmt (sseq keys_stmt args_stmt), 
    {tm with tbl; keys; args}


and inline_exps es =
  List.fold_right
    (fun e (acc_s, acc_es) ->
      let stmt, e' = inline_exp e in
      sseq stmt acc_s, e' :: acc_es)
    es
    (snoop, [])


and inline_stmt s =
  match s.s with
  | SNoop | SRet None -> s
  | SUnit e ->
    let s', e' = inline_exp e in
    sseq s' { s with s = SUnit e' }
  | SLocal (id, ty, e) ->
    let s', e' = inline_exp e in
    sseq s' { s with s = SLocal (id, ty, e') }
  | SAssign (id, e) ->
    let s', e' = inline_exp e in
    sseq s' { s with s = SAssign (id, e') }
  | SPrintf (str, es) ->
    let s', es' = inline_exps es in
    sseq s' { s with s = SPrintf (str, es') }
  | SIf (e, s1, s2) ->
    let s', e' = inline_exp e in
    sseq s' { s with s = SIf (e', inline_stmt s1, inline_stmt s2) }
  | SGen (b, e) ->
    let s', e' = inline_exp e in
    sseq s' { s with s = SGen (b, e') }
  | SRet (Some e) ->
    let s', e' = inline_exp e in
    sseq s' { s with s = SRet (Some e') }
  | SSeq (s1, s2) -> { s with s = SSeq (inline_stmt s1, inline_stmt s2) }
  | SMatch (es, branches) ->
    let s', es' = inline_exps es in
    let branches' = List.map (fun (p, stmt) -> p, inline_stmt stmt) branches in
    sseq s' { s with s = SMatch (es', branches') }
  | SLoop (s1, id, sz) -> { s with s = SLoop (inline_stmt s1, id, sz) }
  | STableMatch(tm) -> 
    let pre_s, tm' = inline_tbl_match tm in
    sseq pre_s {s with s=STableMatch(tm')}
  | STableInstall(tbl_id, entries) -> 
    let stmt, entries_rev = List.fold_left
      (fun (s,entries) entry -> 
        let acn_cid, eargs, flag = unpack_default_action entry.eaction.e in
        let a_s, eargs = inline_exps eargs in
        let entry = {entry with eaction = {entry.eaction with e = ECall(acn_cid, eargs, flag)}} in
        sseq a_s s, entry::entries)
      (snoop, [])
      entries
    in
    let entries = List.rev entries_rev in
    sseq stmt {s with s=STableInstall(tbl_id, entries)}
;;

let eliminator =
  object
    inherit [_] s_map as super

    (* Note that we don't replace expressions which appear directly inside decls, because
       there's nowhere for the stmt part of an EStmt to go. *)
    method! visit_statement _ s = inline_stmt s

    (* Don't replace inside memops, it confuses the typer and is semantically a no-op anyway *)
    method! visit_DMemop _ id params body = DMemop (id, params, body)
  end
;;

let eliminate_prog ds = eliminator#visit_decls () ds
