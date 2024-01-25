(* convert Core parser to / from statement form *)
open CoreSyntax
open CorePrinting
let err = Console.error ;;

(* parse instructions get turned into calls to these functions *)
let read_cid = Cid.create ["parse"; "read"] ;;
let peek_cid = Cid.create ["parse"; "peek"] ;;
let skip_cid = Cid.create ["parse"; "skip"] ;;
let drop_cid = Cid.create ["parse"; "drop"] ;;


(* desugaring *)
let rec from_parser_block (pb : parser_block) : statement = 
  let rec from_parser_step (ps : parser_step) : statement = 
    match ps with 
    | PMatch(exps, branches) -> 
      let branches = List.map from_parser_branch branches in
      smatch exps branches
    | PGen(exp) -> 
      let stmt = gen_sp (GSingle (None)) (exp) Span.default in
      print_endline "---- parser step ----";
      print_endline (parser_step_to_string ps);
      print_endline "---- gen statement ----";
      print_endline (stmt_to_string stmt);
      stmt
    | PCall(exp) -> sexp_sp exp Span.default
    | PDrop -> 
      scall_sp drop_cid [] (ty TBool) Span.default
  and from_parser_branch (pb : parser_branch) : branch = 
    let pats =(fst pb) in
    let stmt = from_parser_block (snd pb) in
    (pats, stmt)
  and from_parser_action (pa : parser_action * Span.t) : statement = 
    match pa with 
    | PRead(cid, ty, pkt_arg), sp -> 
      slocal (Cid.to_id cid) ty (call_sp (read_cid) [pkt_arg] ty sp)
    | PPeek(cid, ty, pkt_arg), sp -> 
      slocal (Cid.to_id cid) ty (call_sp (peek_cid) [pkt_arg] ty sp)
    | PSkip(ty), sp -> 
      sexp_sp (call_sp skip_cid [] ty sp) sp
    | PAssign(cid, exp), sp -> sassign_sp cid exp sp
    | PLocal(cid, ty, exp), sp -> slocal_sp (Cid.to_id cid) ty exp sp
  in
  let actions = List.map from_parser_action pb.pactions in
  let step = from_parser_step (fst pb.pstep) in
  sequence_stmts (actions@[step])
;;
(* resugaring *)
let rec to_parser_block (stmt : statement) : parser_block =
let stmts = flatten_stmt stmt in
(* the step is the last statement, the actions are all previous statements *)
let step = List.hd (List.rev stmts) in
let actions = List.rev (List.tl (List.rev stmts)) in
let action_sps = List.map to_parser_action actions in
let step_sp = to_parser_step step in
{pactions=action_sps; pstep=step_sp}
and to_parser_action (stmt : statement) : (parser_action * Span.t) =
match stmt.s with
  | SLocal(id, ty, {e=ECall(fcid, [pkt_arg], _)}) when (Cid.equal fcid read_cid) -> 
    (PRead(Cid.id id, ty, pkt_arg), stmt.sspan)
  | SLocal(id, ty, {e=ECall(fcid, [pkt_arg], _)}) when (Cid.equal fcid peek_cid) -> 
    (PPeek(Cid.id id, ty, pkt_arg), stmt.sspan)
  | SUnit({e=ECall(fcid, _, _); ety}) when (Cid.equal fcid skip_cid) ->
    (PSkip(ety), stmt.sspan)
  | SAssign(cid, exp) -> (PAssign(cid, exp), stmt.sspan)
  | SLocal(id, ty, exp) -> (PLocal(Cid.id id, ty, exp), stmt.sspan)
  | _ -> 
    print_endline@@"statement: "^(stmt_to_string stmt)^"";
    err@@"[to_parser_action] statement is not a valid parser action"
and to_parser_step (stmt : statement) : parser_step * Span.t = 
match stmt.s with
| SMatch(exps, branches) -> 
  let branches = List.map to_parser_branch branches in
  (PMatch(exps, branches), stmt.sspan)
| SGen(GSingle(None), exp) -> (PGen(exp), stmt.sspan)
| SUnit(exp) when 
  (match exp.e with 
    | ECall(cid, [], _) when (Cid.equal cid (Cid.create ["parse"; "drop"])) -> true
    | _ -> false
  ) -> (PDrop, stmt.sspan)
| SUnit(exp) when (match exp.e with ECall(_) -> true | _ -> false) -> (PCall(exp), stmt.sspan)
| _ -> 
  print_endline (stmt_to_string stmt);
  err@@"[to_parser_step] statement is not a valid parser step"
and to_parser_branch (branch : branch) : parser_branch = 
let pats, stmt = branch in
pats, to_parser_block stmt
;;


let rec equiv_parser_block pb1 pb2 = 
let pas1 = List.map fst pb1.pactions in
let pas2 = List.map fst pb2.pactions in
let pss1 = fst pb1.pstep in
let pss2 = fst pb2.pstep in
let res = equiv_list equiv_parser_action pas1 pas2 && equiv_parser_step pss1 pss2 in
if not res then 
  Console.error@@"[equiv_parser_block] parser blocks not equivalent";
res

and equiv_parser_action pa1 pa2 = 
let res = match pa1, pa2 with
| PRead(cid1, ty1, exp1), PRead(cid2, ty2, exp2) -> 
  Cid.equal cid1 cid2 && equiv_ty ty1 ty2 && equiv_exp exp1 exp2
| PPeek(cid1, ty1, exp1), PPeek(cid2, ty2, exp2) ->
  Cid.equal cid1 cid2 && equiv_ty ty1 ty2 && equiv_exp exp1 exp2
| PSkip(ty1), PSkip(ty2) -> equiv_ty ty1 ty2
| PAssign(cid1, exp1), PAssign(cid2, exp2) ->
  Cid.equal cid1 cid2 && equiv_exp exp1 exp2
| PLocal(cid1, ty1, exp1), PLocal(cid2, ty2, exp2) ->
  Cid.equal cid1 cid2 && equiv_ty ty1 ty2 && equiv_exp exp1 exp2
| _ -> false 
in
if not res then (
  print_endline (parser_action_to_string pa1);
  print_endline (parser_action_to_string pa2);
  Console.error@@"[equiv_parser_action] parser actions not equivalent"
);
res

and equiv_parser_step ps1 ps2 = 
let res = match ps1, ps2 with
| PMatch(es1, bs1), PMatch(es2, bs2) ->
  equiv_list equiv_exp es1 es2 && equiv_list equiv_parser_branch bs1 bs2
| PGen(exp1), PGen(exp2) -> equiv_exp exp1 exp2
| PCall(exp1), PCall(exp2) -> equiv_exp exp1 exp2
| PDrop, PDrop -> true
| _ -> false
in
if not res then 
  Console.error@@"[equiv_parser_step] parser steps not equivalent";
res

and equiv_parser_branch pb1 pb2 =
let res = 
equiv_list equiv_pat (fst pb1) (fst pb2) && equiv_parser_block (snd pb1) (snd pb2)
in
if not res then 
  Console.error@@"[equiv_parser_branch] parser branches not equivalent";
res
;;


let test_parser_desugaring decls = 
 (*find all the parser decls, translate them into statement, then translate the statements back into 
    parser blocks. Use this to make sure both directions work as expected. *)
  List.map 
    (fun decl -> 
      match decl.d with 
        | DParser(id, params, block) -> 
        let stmt = from_parser_block block in
        let block' = to_parser_block stmt in
        if not (equiv_parser_block block block') then 
          err@@"[test_parser_desugaring] parser block not equivalent after desugaring/resugaring"
        else ();
        let d' = parser id params block' in
        {decl with d=d'}
        | _ -> decl)
    decls
;;