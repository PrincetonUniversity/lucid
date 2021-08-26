(* memop translation for tofino *)
module S = Syntax
module PR = Printing
open LLSyntax
open LLContext
open InterpHelpers
open LLOp

(**** memop translation ****)
type memop_kind =
  | Get
  | Set

let from_relop relop =
  match relop with
  | S.Eq -> Eq
  | S.Neq -> Neq
  | S.Less -> Lt
  | S.More -> Gt
  | _ -> error "not a relational operation"
;;

let from_arithop arithop =
  match arithop with
  | S.Plus -> Add
  | S.Sub -> Sub
  | _ -> error "not an arith op, or op not supported"
;;

let flip_relop relop =
  match relop with
  | Eq -> Eq
  | Neq -> Neq
  | Lt -> Gt
  | Gt -> Lt
;;

let from_testexp hdl_id mem_var (exp : S.exp) =
  (* this function expects the constants in EXP to be folded! *)
  print_endline "test expression in memop: ";
  match exp.e with
  (* root of expression tree should always be a relational operation *)
  | S.EOp (rel_op, [lhs; rhs]) ->
    (match lhs, rhs with
    (* (a + b) < c *)
    | { e = S.EOp (arith_op, [e1; e2]); _ }, e3 ->
      let rel_op = from_relop rel_op in
      let arith_op = from_arithop arith_op in
      let o1 = soper_from_immediate hdl_id (Some mem_var) e1 in
      let o2 = soper_from_immediate hdl_id (Some mem_var) e2 in
      let o3 = soper_from_immediate hdl_id (Some mem_var) e3 in
      Comp (o1, arith_op, o2, rel_op, o3), Neg (o1, arith_op, o2, rel_op, o3)
    (* a < (b + c) *)
    | e3, { e = S.EOp (arith_op, [e1; e2]); _ } ->
      let rel_op = flip_relop (from_relop rel_op) in
      let arith_op = from_arithop arith_op in
      let o1 = soper_from_immediate hdl_id (Some mem_var) e1 in
      let o2 = soper_from_immediate hdl_id (Some mem_var) e2 in
      let o3 = soper_from_immediate hdl_id (Some mem_var) e3 in
      Comp (o1, arith_op, o2, rel_op, o3), Neg (o1, arith_op, o2, rel_op, o3)
    (* a < c *)
    | e1, e2 ->
      let rel_op = from_relop rel_op in
      let arith_op = Add in
      let o1 = soper_from_immediate hdl_id (Some mem_var) e1 in
      let o2 = Const (Integer.of_int 0) in
      let o3 = soper_from_immediate hdl_id (Some mem_var) e2 in
      Comp (o1, arith_op, o2, rel_op, o3), Neg (o1, arith_op, o2, rel_op, o3))
  | _ ->
    error
      "[tofino memop translator] memop test expression does not contain a \
       relational operation at its root."
;;

let from_retexp hdl_id mem_var (exp : S.exp) =
  (* the backend syntax only currently supports expressions that are immediates or have 1 binary operation. *)
  match exp.e with
  (* a + b *)
  | S.EOp (arith_op, [e1; e2]) ->
    let arith_op = from_arithop arith_op in
    let o1 = soper_from_immediate hdl_id (Some mem_var) e1 in
    let o2 = soper_from_immediate hdl_id (Some mem_var) e2 in
    SBinOp (arith_op, o1, o2)
  (* a *)
  | _ -> SVar (soper_from_immediate hdl_id (Some mem_var) exp)
;;

let from_memop
    hdl_id
    (args : S.cid list)
    (bdy : S.statement)
    (kind : memop_kind)
    (in_var : S.exp)
    : sExpr list
  =
  let mem_var, arg_var =
    match args with
    | [mem_var; arg_var] -> mem_var, arg_var
    | _ -> error "memop has wrong number of arguments"
  in
  (* replace arg_var with in_var wherever it appears *)
  let bdy = replace_in_stmt bdy arg_var in_var in
  (* translate the body into an instruction. *)
  let sinstr_tups =
    match bdy.s with
    (* evaluate expression *)
    | S.SRet (Some exp) -> [None, from_retexp hdl_id mem_var exp]
    (* test-then-evaluate expression *)
    | S.SIf
        (test_exp, { s = SRet (Some rexp1); _ }, { s = SRet (Some rexp2); _ })
      ->
      let true_test, false_test = from_testexp hdl_id mem_var test_exp in
      let true_exp = from_retexp hdl_id mem_var rexp1 in
      let false_exp = from_retexp hdl_id mem_var rexp2 in
      [Some true_test, true_exp; Some false_test, false_exp]
    | _ -> error "invalid form for a memop"
  in
  let sinstrs =
    match kind with
    | Get -> CL.map (fun (a, b) -> RetExpr (a, b)) sinstr_tups
    | Set -> CL.map (fun (a, b) -> MemExpr (a, b)) sinstr_tups
  in
  sinstrs
;;

let from_memop_name hdl_id (name : Cid.t) (kind : memop_kind) (in_var : S.exp) =
  let param_cids, body_stmt = ctx_bdy_of_memop name in
  from_memop hdl_id param_cids body_stmt kind in_var
;;
