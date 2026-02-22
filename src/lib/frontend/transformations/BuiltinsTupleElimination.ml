(* This pass ensures that calls to builtin functions that 
   return tuples are placed in their own TupleAssign statements. 
   It should be run immediately before the standard tuple elimination. *) 


open Syntax
open SyntaxUtils
open Collections
module CMap = Collections.CidMap

let fresh_intermediate () = Id.fresh "tbl_ret"

let builtin_cids = 
List.map
  (fun (_, _, global_funs, constructors) ->
    let fun_cids =
      List.map
        (fun (gf : InterpSwitch.global_fun) -> gf.cid)
        global_funs
    in
    let constructor_cids = List.map fst constructors in
    fun_cids @ constructor_cids)
  (List.map LibraryInterface.sigty_to_tup Builtins.builtin_modules)
|> List.flatten
;;

let is_tuple ty =
  match ty.raw_ty with
  | TTuple _ -> true
  | _ -> false
;;
let rec eliminate_exp e =
  match e.e with
  (* call to a builtin that returns a tuple *)
  (* 
    given a call to a builtin that returns a tuple,
    return a statement that sets a temp variable in a 
    tuple assign and an evar that replaces the 
    call in the statement where it appears.
      (int, int) foo = doit();
        ==> 
      (int, int) tmp_var = doit(); // STupleAssign
      foo = tmp_var;   
  *)
  | ECall (cid, args, u) when 
      ((List.mem cid builtin_cids) && (Option.get e.ety |> is_tuple)) ->
    let outvar = fresh_intermediate () in
    let outvar_ty = Option.get e.ety in
    let args_pre_stmt, args' = eliminate_exps args in
    (* a statement that only makes the call *)
    let scall = statement@@STupleAssign {
      ids = [outvar];
      tys = Some([outvar_ty]);
      exp = aexp (ECall (cid, args', u)) e.ety e.espan;
      }
    in
    sseq args_pre_stmt scall, { e with e = EVar (Cid.id outvar) }
  (* all other cases just recurse *)
  | EStmt (s1, e1) ->
    let s1' = eliminate_stmt s1 in
    let e1s', e1' = eliminate_exp e1 in
    e1s', { e with e = EStmt (s1', e1') }
  | EVal _ | EInt _ | EVar _ | ESizeCast _ -> snoop, e
  | EOp (op, es) ->
    let stmt, es' = eliminate_exps es in
    stmt, { e with e = EOp (op, es') }
  | ECall (cid, es, u) ->
    let stmt, es' = eliminate_exps es in
    stmt, { e with e = ECall (cid, es', u) }
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
  | EComp (e, id, size) ->
    let stmt, e' = eliminate_exp e in
    stmt, { e with e = EComp (e', id, size) }
  (* | EPatWild _ -> snoop, e *)
  | ETableMatch _ -> snoop, e
  | ETableCreate _ -> snoop, e 
  (* error "special table syntax is depreciated" *)

and eliminate_exps exps =
  let acc =
    List.fold_left
      (fun (pre_stmt, exps) exp ->
        match eliminate_exp exp with
        | { s = SNoop }, exp' -> pre_stmt, exp' :: exps
        | stmt, exp' -> sseq pre_stmt stmt, exp' :: exps)
      (snoop, [])
      exps
  in
  let pre_stmt, args' = fst acc, List.rev (snd acc) in
  pre_stmt, args'

(* eliminate calls to tuple-returning builtins in statements -- 
    this just special cases certain assigns and locals, 
    then recurses on the rest *)
and eliminate_stmt stmt =
  match stmt.s with
  (* special case: change assigns with rhs's that directly call 
     tuple-returning builtins into TupleAssigns  *)
  | SAssign (id, { e = ECall(cid, args, u); ety; espan}) 
    when ((List.mem cid builtin_cids) && (Option.get ety |> is_tuple)) ->
    let args_pre_stmt, args' = eliminate_exps args in
    let exp' = { e = ECall (cid, args', u); ety; espan } in
    let stupleassign = statement@@STupleAssign { ids = [id]; tys = None; exp = exp' } in
    sseq args_pre_stmt stupleassign
  (* same special case for sLocal, except we fill in the type of the tuple assign *)
  | SLocal (id, ty, { e = ECall(cid, args, u); ety; espan}) 
    when ((List.mem cid builtin_cids) && (Option.get ety |> is_tuple)) ->
    (* print_endline ("found a local tuple assign" ^ (Cid.to_string cid)); *)
    (* print_endline (Printing.ty_to_string ty); *)
    let args_pre_stmt, args' = eliminate_exps args in
    let exp' = { e = ECall (cid, args', u); ety; espan } in
    let stupleassign = statement@@STupleAssign { ids = [id]; tys = Some [ty]; exp = exp' } in
    sseq args_pre_stmt stupleassign
  (* everything else is just recursing *)
  | SNoop -> stmt
  | SUnit exp ->
    let pre_stmt, exp' = eliminate_exp exp in
    sseq pre_stmt { stmt with s = SUnit exp' }
  | SAssign (id, exp) ->
    let pre_stmt, exp' = eliminate_exp exp in
    sseq pre_stmt { stmt with s = SAssign (id, exp') }
  | SLocal (id, ty, exp) ->
    let pre_stmt, exp' = eliminate_exp exp in
    sseq pre_stmt { stmt with s = SLocal (id, ty, exp') }
  | SPrintf (str, exps) ->
    let pre_stmt, exps' = eliminate_exps exps in
    sseq pre_stmt { stmt with s = SPrintf (str, exps') }
  | SIf (exp, s1, s2) ->
    let pre_stmt, exp' = eliminate_exp exp in
    let s1', s2' = eliminate_stmt s1, eliminate_stmt s2 in
    sseq pre_stmt { stmt with s = SIf (exp', s1', s2') }
  | SGen (gty, exp) ->
    let pre_stmt, exp = eliminate_exp exp in
    sseq pre_stmt { stmt with s = SGen (gty, exp) }
  | SRet None -> stmt
  | SRet (Some exp) ->
    let pre_stmt, exp = eliminate_exp exp in
    sseq pre_stmt { stmt with s = SRet (Some exp) }
  | SSeq (s1, s2) ->
    { stmt with s = SSeq (eliminate_stmt s1, eliminate_stmt s2) }
  | SMatch (exps, branches) ->
    let pre_stmt, exps = eliminate_exps exps in
    let branches =
      List.map
        (fun (pats, statement) -> pats, eliminate_stmt statement)
        branches
    in
    sseq pre_stmt { stmt with s = SMatch (exps, branches) }
  | SLoop (stmt, id, size) ->
    { stmt with s = SLoop (eliminate_stmt stmt, id, size) }
  | STupleAssign _ -> stmt (* noop for tuple assignments *)
  | STableMatch _ -> stmt 
  | STableInstall _ -> stmt
;;

let eliminator =
  object
    inherit [_] s_map as super
    method! visit_statement _ s = eliminate_stmt s
    (* notice that we don't recurse, so this will be
        the first statement of every declaration *)
  end
;;

let eliminate_prog (ds : decl list) = eliminator#visit_decls () ds
