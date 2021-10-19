open Syntax
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
  | ECall (cid, es) ->
    let stmt, es' = inline_exps es in
    stmt, { e with e = ECall (cid, es') }
  | EHash (sz, es) ->
    let stmt, es' = inline_exps es in
    stmt, { e with e = EHash (sz, es') }
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
;;

let eliminator =
  object
    inherit [_] s_map as super

    (* Note that we don't replace expressions which appear directly inside decls, because
       there's nowhere for the stmt part of an EStmt to go. *)
    method! visit_statement _ s = inline_stmt s

    (* Don't replace inside memops, it confuses the typer and is semantically a no-op anyway *)
    method! visit_DMemop _ id body = DMemop (id, body)
  end
;;

let eliminate_prog ds = eliminator#visit_decls () ds
