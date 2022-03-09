(* memop translation for tofino *)
(* OLD memops -- adaptation in progress. *)
module S = CoreSyntax
module PR = CorePrinting
open LLSyntax
open LLContext
open InterpHelpers
open LLOp
module CidMap = Collections.CidMap

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
  | S.BitAnd -> BAnd
  | S.BitOr -> BOr
  | _ -> error "not an arith op, or op not supported"
;;

let flip_relop relop =
  match relop with
  | Eq -> Eq
  | Neq -> Neq
  | Lt -> Gt
  | Gt -> Lt
;;
(* workaround for assembler: transform the
  operation (memcell + const) --> (const + memcell) *)
let _workaround_flip_memcell_const operation : sEvalExpr =
  match operation with
  | SBinOp (Add, a, b) ->
    (match a, b with
    | RegVar _, Const _ -> SBinOp (Add, b, a)
    | _ -> operation)
  | _ -> operation
;;

let map_from_lists xs ys = 
  let bind map (x, y) = 
    CidMap.add x y map 
  in 
  CL.fold_left bind (CidMap.empty) (CL.combine xs ys) 
;;
let param_to_cid param = Cid.id (fst param) ;;

(* translate memop parameters to operands in the LLIR. 
   the parameters are of the form memory_params@local_params
   memory_params get translated to regSlices
   local_params get translated into references to the variables in args *)
let ordered_arg_opers hdl_id params args : LLSyntax.oper list = 
  let num_meta_params = CL.length args in 
  let num_regvar_params = (CL.length params) - num_meta_params in   
  match num_regvar_params with 
    | 1 -> (RegVar Lo)::(args |> CL.map (oper_from_immediate hdl_id))
    | 2 -> error "[LLMemop.translate_params] memops for pair arrays are not supported yet."
    | _ -> error "[LLMemop.translate_params] got invalid arguments for given parameter list"
;;

(* translate an expression in a memop into... things in the LLIR *)

(* evars and evals get translated into operands *)
let exp_to_oper param_bindings (exp:S.exp) = 
  match exp.e with 
  | S.EVar cid -> 
    CidMap.find cid param_bindings
  | S.EVal _ -> IS.Const(zint_from_evalue exp)
  | _ -> error "[LLMemop.exp_to_oper] not an EVar!"
;;

(* translate an expression into a computation sub-instruction *) 
let exp_to_sEvalExpr param_bindings (exp:S.exp) = 
  match exp.e with 
    (* a <op> b *)
  | S.EOp (arith_op, [e1; e2]) -> (
    let arith_op = from_arithop arith_op in
    let o1 = exp_to_oper param_bindings e1 in 
    let o2 = exp_to_oper param_bindings e2 in     
    SBinOp (arith_op, o1, o2) |> _workaround_flip_memcell_const
  )
  | S.EVar _ | S.EVal _ -> IS.SVar(exp_to_oper param_bindings exp)
  | _ -> error "[LLMemop.from_ret] unsupported expression."
;;

(* translate an expression into a predicate sub-instruction *)
let exp_to_sPredExpr param_bindings (exp:S.exp) = 
  match exp.e with 
  | S.EOp (rel_op, [lhs; rhs]) -> (
    match (lhs, rhs) with 
    (* (a + b) < c *)
    | { e = S.EOp (arith_op, [e1; e2]); _ }, e3 ->
      let rel_op = from_relop rel_op in
      let arith_op = from_arithop arith_op in
      let o1 = exp_to_oper param_bindings e1 in
      let o2 = exp_to_oper param_bindings e2 in
      let o3 = exp_to_oper param_bindings e3 in
      Comp (o1, arith_op, o2, rel_op, o3)
    (* a < (b + c) *)
    | e3, { e = S.EOp (arith_op, [e1; e2]); _ } ->
      (* only difference between this 
         and previous case is the flip *)
      let rel_op = flip_relop (from_relop rel_op) in
      let arith_op = from_arithop arith_op in
      let o1 = exp_to_oper param_bindings e1 in
      let o2 = exp_to_oper param_bindings e2 in
      let o3 = exp_to_oper param_bindings e3 in
      Comp (o1, arith_op, o2, rel_op, o3)
    (* a < c *)
    | e1, e3 ->
      let rel_op = from_relop rel_op in
      let arith_op = Add in
      let o1 = exp_to_oper param_bindings e1 in
      let o2 = Const (Integer.of_int 0) in
      let o3 = exp_to_oper param_bindings e3 in
      Comp (o1, arith_op, o2, rel_op, o3)
  )
  | _ ->
    error
      "[LLMemop.translate] memop predicate does not contain a \
       relational operation at its root."

(* invert predicate *)
let negate_pred pred_exp = 
  match pred_exp with 
    | Comp(o1, b, o2, c, o3) -> Neg(o1, b, o2, c, o3)
    | Neg(o1, b, o2, c, o3) -> Comp(o1, b, o2, c, o3)
;;


(* new core method: translate a memop into 
   a list of stateful instruction right hand sides.
   So its basically a list of instructions with the 
   output destination not set. *) 
let translate_memop_body 
  param_bindings
  (body : CoreSyntax.memop_body) (* memop body *)
  : sExprRhs list
  = 
    (* translate body *)
    match body with 
      | S.MBReturn exp -> 
        [(None, exp_to_sEvalExpr param_bindings exp)]
      | S.MBIf (ifexp, b1exp, b2exp) ->
        let pos_pred = exp_to_sPredExpr param_bindings ifexp in 
        let b1_subinstr = exp_to_sEvalExpr param_bindings b1exp in 
        let b2_subinstr = exp_to_sEvalExpr param_bindings b2exp in 
        (* two instructions that return to the same place *)
        [(Some pos_pred, b1_subinstr); (Some (negate_pred pos_pred), b2_subinstr)]
      | S.MBComplex(_) -> error "Complex memops not yet implemented"
;;

(* bind the parameters of a memop to LLIR operands 
   referencing memory cells or variables. *)
let bind_memop_params hdl_id params args =  
  map_from_lists 
    (CL.map param_to_cid params)
    (ordered_arg_opers hdl_id params args)
;;

(* translate a simple read or write memop 
   into a vector of stateful alu compute instructions. *)
let translate_simple_memop hdl_id memop_kind name_exp arg_exp : IS.sExpr list = 
  (* get the memop *)
  let memop_cid = name_from_exp name_exp in 
  let params, body = ctx_bdy_of_memop memop_cid in

  (* translate the memop body *)
  translate_memop_body 
    (bind_memop_params hdl_id params [arg_exp]) 
    body
  |> CL.map (fun sexpr -> 
      match memop_kind with 
        | Get -> RetExpr(sexpr)
        | Set -> MemExpr(sexpr)
      )
;;
