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

(* This pass translates ETableApply(TTable..., args) expressions 
   into SApplyTable {tblty; tble; key; aargs; aout; decl_out_vars}

  The translation depends on where the expression appears. 
 *)

let fresh_intermediate () = 
  Id.fresh "tbl_ret"
;; 

(* eliminate table expression in an expression *)
let rec eliminate_exp e = 
  match e.e with 
  | ETableApply(tr) ->
    (* replace table apply expression with: 
       1. an intermediate variable that gets set in a pre statement
       2. an evar of the intermediate *)
      let outvar = fresh_intermediate () in
      let outvar_ty = Option.get e.ety in 
      let args_pre_stmt, args' = eliminate_exps tr.args in
      let new_tr = {tr with outs=[outvar]; out_tys=Some [outvar_ty]; args=args';} in 
      let sapply = {s=SApplyTable(new_tr); sspan=Span.default} in
      sseq args_pre_stmt sapply, {e with e=EVar(Cid.id outvar)}
  (* all other cases just recurse *)    
  | EStmt (s1, e1) -> 
    let s1' = eliminate_stmt s1 in
    let e1s', e1' = eliminate_exp e1 in
    e1s', {e with e=EStmt(s1', e1')}
  | EVal _ | EInt _ | EVar _ | ESizeCast _ -> snoop, e
  | EOp (op, es) ->
    let stmt, es' = eliminate_exps es in
    stmt, { e with e = EOp (op, es') }  
  | ECall (cid, es) ->
    let stmt, es' = eliminate_exps es in
    stmt, { e with e = ECall (cid, es') }
  | EHash (sz, es) ->
    let stmt, es' = eliminate_exps es in
    stmt, { e with e = EHash (sz, es') }
  | EFlood e ->
    let stmt, e' = eliminate_exp e in
    stmt, { e with e = EFlood e' }
  | ERecord lst ->
    let strs, es = List.split lst in
    let stmt, es = eliminate_exps es in
    stmt, { e with e = ERecord (List.combine strs es) }
  | EWith (e1, lst) ->
    let stmt1, e1' = eliminate_exp e1 in
    let strs, es = List.split lst in
    let stmt2, es = eliminate_exps es in
    sseq stmt1 stmt2, { e with e = EWith (e1', List.combine strs es) }
  | EProj (e1, str) ->
    let stmt, e1' = eliminate_exp e1 in
    stmt, { e with e = EProj (e1', str) }
  | EVector es ->
    let stmt, es' = eliminate_exps es in
    stmt, { e with e = EVector es' }  
  | EIndex (e1, sz) ->
    let stmt, e1' = eliminate_exp e1 in
    stmt, { e with e = EIndex (e1', sz) }
  | ETuple es ->
    let stmt, es' = eliminate_exps es in
    stmt, { e with e = ETuple es' }
  (* table apply can't appear in a table create expression *)
  | ETableCreate _ -> snoop, e
  | EComp(e, id, size) -> 
    let stmt, e' = eliminate_exp e in
    stmt, { e with e = EComp (e', id, size) }





(* eliminate table expressions in a list of expressions *)
and eliminate_exps exps = 
  let acc = List.fold_left 
    (fun (pre_stmt, exps) exp -> 
      match eliminate_exp exp with
      | {s=SNoop;}, exp' -> (pre_stmt, exp'::exps)
      | stmt, exp' -> (sseq pre_stmt stmt, exp'::exps))
    (snoop, [])
    exps
  in
  let pre_stmt, args' = fst acc, (List.rev (snd acc)) in  
  pre_stmt, args'


(* eliminate table expression in a statement. 
   SAssign and SLocal with rhs of ETableApply are translated directly
   for all other statements, recurse on inner components to generate 
   pre-compute statement, then return {pre-compute statement; statement;} *)
and eliminate_stmt stmt =
  match stmt.s with
  (* locals and assigns get special cased to avoid copy overhead *)
  | SAssign(id, {e=ETableApply(tr)}) ->   
    let pre_stmt, args' = eliminate_exps tr.args in
    let new_tr = {tr with args=args'; outs=[id]; out_tys=None;} in
    sseq pre_stmt {stmt with s=SApplyTable(new_tr)}
  | SLocal(id, ty, {e=ETableApply(tr)}) -> 
    (* I think we can throw away the local's type because its 
       in the table type *)
    let pre_stmt, args' = eliminate_exps tr.args in
    let new_tr = {tr with args=args'; outs=[id]; out_tys=Some[ty];} in
    sseq pre_stmt {stmt with s=SApplyTable(new_tr)}
  (* everything else is just recursing *)
  | SNoop -> stmt
  | SUnit exp -> 
    let pre_stmt, exp' = eliminate_exp exp in
    sseq pre_stmt {stmt with s=SUnit(exp')}
  | SLocal(id, ty, exp) -> 
    let pre_stmt, exp' = eliminate_exp exp in
    sseq pre_stmt {stmt with s=SLocal(id, ty, exp')}
  | SAssign(id, exp) -> 
    let pre_stmt, exp' = eliminate_exp exp in
    sseq pre_stmt {stmt with s=SAssign(id, exp')}
  | SPrintf(str, exps) -> 
    let pre_stmt, exps' = eliminate_exps exps in
    sseq pre_stmt {stmt with s=SPrintf(str, exps')}
  | SIf(exp, s1, s2) -> 
    let pre_stmt, exp' = eliminate_exp exp in 
    let s1', s2' = eliminate_stmt s1, eliminate_stmt s2 in 
    sseq pre_stmt {stmt with s=SIf(exp', s1', s2')}
  | SGen(gty, exp) -> 
    let pre_stmt, exp = eliminate_exp exp in
    sseq pre_stmt {stmt with s=SGen(gty, exp)}
  | SRet(None) -> stmt
  | SRet(Some(exp)) -> 
    let pre_stmt, exp = eliminate_exp exp in
    sseq pre_stmt {stmt with s=SRet(Some(exp))}
  | SSeq (s1, s2) -> {stmt with s=SSeq(eliminate_stmt s1, eliminate_stmt s2)}
  | SMatch(exps, branches) -> 
    let pre_stmt, exps = eliminate_exps exps in
    let branches = List.map
      (fun (pats, statement) -> pats, eliminate_stmt statement)
      branches
    in
    sseq pre_stmt {stmt with s = SMatch(exps, branches)}
  | SLoop(stmt, id, size) -> 
    {stmt with s=SLoop(eliminate_stmt stmt, id, size)}
  | SApplyTable (t) -> 
    let pre_tble, tbl = eliminate_exp t.tbl in
    let pre_key, keys =  eliminate_exps t.keys in
    let pre_aargs, args = eliminate_exps t.args in
    let pre_stmt = sseq (sseq pre_tble pre_key) pre_aargs in
    let t' = {t with tbl; keys; args} in 
    sseq pre_stmt {stmt with s=SApplyTable(t')}
;;

let eliminator = 
    object
      inherit [_] s_map as super
      method !visit_statement _ s = eliminate_stmt s
      (* notice that we don't recurse, so this will be 
         the first statement of every declaration *)
    end
;;


let eliminate_prog (ds: decl list) =
  eliminator#visit_decls () ds
;;
