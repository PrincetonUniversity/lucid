(* Z3 interface from core syntax *)

open Collections
open Batteries
module CL = Caml.List
open CoreSyntax
open Z3
open Solver
module Z3Bool = Boolean
module Z3Int = Arithmetic.Integer


module Z3ExprMap = Map.Make(
  struct
    type t = Z3.Expr.expr
    let compare = Z3.Expr.compare
  end
)

type solver = {
  ctx : Z3.context;
  solver : Z3.Solver.solver;
  emap : exp Z3ExprMap.t; (* map z3 var and val exprs to coresyntax exps *)
}

let def_ctx = 
  mk_context ["model", "true"; "proof", "true"] 
;;

let start_solver ctx = 
  {ctx=ctx; solver = Solver.mk_simple_solver ctx; emap = Z3ExprMap.empty}
;;


(*** encoding to z3 ***)
let val_to_expr solver value = 
  match value.v with 
  | VBool(b) -> Z3Bool.mk_val solver.ctx b 
  | VInt(_) -> error "[val_to_expr] Int vals not implemented. Should make a bitvector, though."
  | _ -> error "[val_to_eqn] unsupported value type for z3 conversion."
;;
let rec exp_to_expr solver exp = 
  match exp.e, exp.ety with 
  | (EVal(v), _) -> 
    let expr = val_to_expr solver v in 
    let emap = Z3ExprMap.add expr exp solver.emap in 
    {solver with emap}, expr
  | (EVar(cid), {raw_ty=TBool; _}) ->
    let expr = Z3Bool.mk_const_s def_ctx (Cid.to_string cid) in 
    let emap = Z3ExprMap.add expr exp solver.emap in 
    {solver with emap}, expr
  | (EVar(_), _) ->
      error "[exp_to_expr] unsupported variable type for z3 conversion."
  | (EOp(op, args), {raw_ty=TBool; _}) -> (
    let arg_fold_f (solver, arg_exprs) arg = 
      let new_solver, expr = exp_to_expr solver arg in 
      new_solver, arg_exprs@[expr]
    in 
    let new_solver, arg_exprs = 
      List.fold_left arg_fold_f (solver, []) args
    in 
    let expr = match op with 
      | And -> Z3Bool.mk_and def_ctx arg_exprs
      | Or  -> Z3Bool.mk_or def_ctx arg_exprs
      | Not -> Z3Bool.mk_not def_ctx (List.hd arg_exprs)
      | _ -> error "[exp_to_expr] unsupported operation for z3 conversion"
    in 
    new_solver, expr
  )
  | (EOp(_), _) ->
    error ("[exp_to_expr] unsupported expression type for z3 conversion: "^(CorePrinting.exp_to_string exp))
  | _ -> error "[exp_to_expr] unsupported expression construct for z3 conversion"
;;

(* public interface *)
let exp_to_expr exp = 
  exp_to_expr (start_solver def_ctx) exp
;;


(*** decoding from z3 ***)
(* type of a z3 expression. *)
let expr_to_rawty expr = 
  let sort = Z3.Expr.get_sort expr in 
  match Z3.Sort.get_sort_kind sort with 
    | Z3enums.BOOL_SORT -> TBool
    | Z3enums.BV_SORT -> TInt (Sz(Z3.BitVector.get_size sort))
    | _ -> error "[expr_to_raw_ty] unsupported kind of z3 expression."
;;

let opkind_to_op opkind = 
  match opkind with 
    | Z3enums.OP_AND -> And
    | Z3enums.OP_OR -> Or
    | Z3enums.OP_NOT -> Not
    | _ -> error "[expr_to_exp] unsupported operation in z3 expression"
;;

let rec expr_to_exp solver expr = 
  (* 1. look for the expression in the context, for variables and values. *)
  (* 2. if not found, its an operation, so recurse on args then translate. *) 
  match Z3ExprMap.find_opt expr solver.emap with 
    | Some exp -> exp
    | None -> (
      let args = Expr.get_args expr |> CL.map (expr_to_exp solver) in
      let opkind = Expr.get_func_decl expr 
        |> FuncDecl.get_decl_kind 
      in
      let op = opkind_to_op opkind in 
      let ety = ty (expr_to_rawty expr) in 
      op_sp op args ety Span.default
    )
;;

(*** doing cool stuff in z3 ***)
let simplify_bool_expr solver expr = 
  solver, Z3.Expr.simplify expr None 
;;


(* simplify a boolean expression to dnf form... I think... *)
let simplify_to_dnf solver expr = 
  let ctx = solver.ctx in 
  let goal = Z3.Goal.mk_goal ctx true false false in 
  Z3.Goal.add goal [expr];
  let split_clause = Z3.Tactic.mk_tactic ctx "split-clause" in
  let skip = Z3.Tactic.mk_tactic ctx "skip" in
  let or_else = Z3.Tactic.or_else ctx split_clause skip in
  let repeat = Z3.Tactic.repeat ctx or_else 9999 in
  let propagate_values = Z3.Tactic.mk_tactic ctx "propagate-values" in
  let simplify = Z3.Tactic.mk_tactic ctx "simplify" in
  let and_then =
    Z3.Tactic.and_then ctx simplify propagate_values [repeat]
  in
  let app_res = Z3.Tactic.apply and_then goal None in
  let disjunc_exprs =
    List.map Goal.as_expr (Tactic.ApplyResult.get_subgoals app_res)
  in
  match disjunc_exprs with 
    | [] -> (* true *) Z3Bool.mk_val solver.ctx true
    | [e] -> e
    | [e1; e2] -> Z3Bool.mk_or solver.ctx [e1; e2]
    | _ -> error "[simplify_to_dnf] currently unsupported: an output dnf with more than 2 terms"
;;


