(* turn special functions (parsers, memops) into standard functions, 
   (and also rebuild the special functions from their 
   standardized forms)*)
open CoreSyntax
open CorePrinting
let err = Console.error ;;

(* parse instructions get turned into calls to these functions *)
let read_cid = Cid.create ["parse"; "read"] ;;
let peek_cid = Cid.create ["parse"; "peek"] ;;
let skip_cid = Cid.create ["parse"; "skip"] ;;
let drop_cid = Cid.create ["parse"; "drop"] ;;


(* desugaring *)
let rec despecialize_parser_block (pb : parser_block) : statement = 
  let rec from_parser_step (ps : parser_step) : statement = 
    match ps with 
    | PMatch(exps, branches) -> 
      let branches = List.map from_parser_branch branches in
      smatch exps branches
    | PGen(exp) -> 
      let stmt = gen_sp (GSingle (None)) (exp) Span.default in
      stmt
    | PCall(exp) -> sexp_sp exp Span.default
    | PDrop -> 
      scall_sp drop_cid [] (ty TBool) Span.default
  and from_parser_branch (pb : parser_branch) : branch = 
    let pats =(fst pb) in
    let stmt = despecialize_parser_block (snd pb) in
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
let rec specialize_parser_block (stmt : statement) : parser_block =
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
pats, specialize_parser_block stmt
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


let test_parser_despecialization decls = 
 (*find all the parser decls, translate them into statement, then translate the statements back into 
    parser blocks. Use this to make sure both directions work as expected. *)
  List.map 
    (fun decl -> 
      match decl.d with 
        | DParser(id, params, block) -> 
        let stmt = despecialize_parser_block block in
        let block' = specialize_parser_block stmt in
        if not (equiv_parser_block block block') then 
          err@@"[test_parser_despecialization] parser block not equivalent after desugaring/resugaring"
        else ();
        let d' = parser id params block' in
        {decl with d=d'}
        | _ -> decl)
    decls
;;



(**** memops ****)

let b1_id = Cid.create ["b1"] ;;
let b2_id = Cid.create ["b2"] ;;
let cell1_id = Cid.create ["cell1"] ;;
let cell2_id = Cid.create ["cell2"] ;;
let ret_id = Cid.create ["ret"] ;;

let despecialize_memop (m : memop)  : (id * params * statement * ty) = 
let args = m.mparams in
let cell_ty = match args with 
| [] -> err "a memop must have arguments"
| (_, ty)::_ -> ty
in
let cell_sz = match cell_ty.raw_ty with 
  | TInt(Sz(sz)) -> sz
  | _ -> err "memop return type must be an integer"
in 
let body, ret_ty = match m.mbody, List.length args with 
  | MBReturn(exp), _ -> statement@@SRet(Some(exp)), exp.ety
  | MBIf(expc, expl, expr), _ -> 
    sifte expc (statement@@SRet(Some(expl))) (statement@@SRet(Some(expr))), expl.ety
  | MBComplex(body), n_args -> (
    let bool_update_stmt = function 
    | None -> None 
    | Some(id, exp) -> Some(slocal id (ty TBool) exp)
    in 
    let cell_update_stmt cell_cid = function 
      | None -> None
      | Some(econd, eval) -> 
        Some(sifte econd (statement@@SAssign(cell_cid, eval)) (statement@@SNoop))
    in
    (* cell1, cell2, and ret are all _return variables_, 
       and need to be initialized.  *)
    let cell1_init = 
      let init_exp = var (List.hd args |> fst |> Cid.id) cell_ty in
      Some(slocal (Cid.to_id cell1_id) cell_ty init_exp)
    in
    (* cell2 might come from an argument, or not *)
    let cell2_init = match n_args with 
      | 3 -> 
        let init_val = vint_exp 0 cell_sz in 
        Some(slocal (Cid.to_id cell2_id) cell_ty init_val)
      | 4 -> 
        let init_exp = var (List.nth args 2 |> fst |> Cid.id) cell_ty in
        Some(slocal (Cid.to_id cell1_id) cell_ty init_exp)
      | _ -> err "wrong number of args for memop"
    in
    (* the return variable is always new *)
    let ret_id  = Cid.create ["ret"] in
    let ret_init = 
      let init_val = vint_exp 0 cell_sz in 
      Some(slocal (Cid.to_id ret_id) cell_ty init_val)
    in
    (* the body is a sequence of optional assignments *)
    let update_stmts = List.filter_map (fun x -> x ) [
      cell1_init;
      cell2_init;
      ret_init;
      bool_update_stmt body.b1;
      bool_update_stmt body.b2;
      cell_update_stmt cell1_id (fst body.cell1);
      cell_update_stmt cell1_id (snd body.cell1);
      cell_update_stmt cell2_id (fst body.cell2);
      cell_update_stmt cell2_id (snd body.cell2);
      cell_update_stmt ret_id body.ret;
    ]      
    in
    let eret = var ret_id cell_ty in
    let ecell1 = var cell1_id cell_ty in
    let ecell2 = var cell2_id cell_ty in
    let ret_tuple = match n_args with 
      | 4 -> exp (* for pairarray, return both cells *)
        (ETuple [ecell1; ecell2; eret]) 
        (ty@@TTuple [ecell1.ety.raw_ty; ecell2.ety.raw_ty; eret.ety.raw_ty;]) 
      | 3 -> exp (* for plain array, return only cell 1*)
        (ETuple [ecell1; eret]) 
        (ty@@TTuple [ecell1.ety.raw_ty; eret.ety.raw_ty;]) 
      | _ -> err "wrong number of args for memop"
    in
    let sret = statement@@SRet(Some(ret_tuple)) in
    let stmts = update_stmts@[sret] in
    let body = List.fold_left 
      (fun acc stmt -> sseq acc stmt) 
        (List.hd stmts)
        (List.tl stmts)
    in
    body, ret_tuple.ety
  )
  in
  m.mid, m.mparams, body, ret_ty
;;

(* turn a function body back into a memop *)
let specialize_memop (id : id) (params : params) (body : statement) : memop = 
  let mbody = match body.s with 
    | SRet Some(exp) -> MBReturn(exp)
    | SIf(exp, {s=SRet(Some(exp1))}, {s=SRet(Some(exp2))}) -> 
      MBIf(exp, exp1, exp2)
    | _ -> 
    begin
      let rec build_body (bdy : complex_body) (stmt : statement) : complex_body = 
        match stmt.s with 
        | SRet _ -> bdy
        | SLocal(bool_var_id, ty, exp) when ty.raw_ty = TBool -> (          
          match bdy.b1 with 
          | None -> 
            {bdy with b1 = Some(bool_var_id, exp)}
          | Some _ -> (
            match bdy.b2 with 
            | None -> {bdy with b2 = Some(bool_var_id, exp)}
            | Some _ -> err "memop body has more than 2 bool variables"
          )
        )
        | SIf(cond_exp, {s=SAssign(cell_var_id, val_exp)}, {s=SNoop}) -> (
          match cell_var_id with
          | c when Cid.equal c cell1_id -> (
            match bdy.cell1 with 
            | (None, c) -> {bdy with cell1 = (Some(cond_exp, val_exp), c)}
            | (Some c, None) -> {bdy with cell1 = (Some(c), Some(cond_exp, val_exp))}
            | _ -> err "memop body has more than 2 updates to cell1"
          )
          | c when Cid.equal c cell2_id -> (
            match bdy.cell2 with 
            | (None, c) -> {bdy with cell2 = (Some(cond_exp, val_exp), c)}
            | (Some c, None) -> {bdy with cell2 = (Some(c), Some(cond_exp, val_exp))}
            | _ -> err "memop body has more than 2 updates to cell2"
          )
          | c when Cid.equal c ret_id -> (
            match bdy.ret with 
            | None -> {bdy with ret = Some(cond_exp, val_exp)}
            | _ -> err "memop body has more than 1 update to ret"
          )
          | _ -> err@@"invalid cell variable id: "^(Cid.to_string cell_var_id)
        )
        | SSeq (s1, s2) -> 
          let bdy = build_body bdy s1 in
          build_body bdy s2
        | _ -> 
          bdy
      in
      MBComplex(build_body {b1=None; b2=None; cell1=(None, None); cell2=(None, None); extern_calls = []; ret=None} body)
    end      
  in
  {mid=id; mparams=params; mbody=mbody}
;;

let test_memop_despecialization (ds : decl list) = 
  List.map 
    (fun decl -> 
      match decl.d with 
        | DMemop(m) -> 
          let id, params, body, _ = despecialize_memop m in
          (* print_endline ("------original memop ------");
          print_endline (CorePrinting.d_to_string (DMemop(m)));
          print_endline ("-------- despecialized function body ------");
          print_endline (stmt_to_string body); *)
          let m' = specialize_memop id params body in
          (* print_endline ("------reconstructed memop ------");
          print_endline (CorePrinting.d_to_string (DMemop(m'))); *)
          if not (equiv_memop m m') then (
            err@@"[test_memop_despecialization] memop not equivalent after de/respecialization";)
          else ();
          let d' = DMemop(m') in
          {decl with d=d'}
        | _ -> decl)
    ds
;;