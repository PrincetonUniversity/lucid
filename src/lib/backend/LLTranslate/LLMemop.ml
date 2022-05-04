(* memop translation for tofino *)
(* OLD memops -- adaptation in progress. *)
module S = CoreSyntax
module PR = CorePrinting
open LLSyntax
open LLContext
open InterpHelpers
open LLOp
module CidMap = Collections.CidMap

(* the memop context stores: 
    param and builtin cids : operands (opers)
    local bool var cids : boolean expressions *)
type memop_context_entry = 
  | MCtxOper of oper
  | MCtBoolExp of sBoolExp
;;

let bind ctx (x, y) : memop_context_entry CidMap.t = 
  CidMap.add x y ctx
;;

let bind_lists ctx xs ys = 
  CL.fold_left bind ctx (CL.combine xs ys)
;;

let find_oper ctx x = 
  match CidMap.find_opt x ctx with 
   | Some(MCtxOper(o)) -> o
   | _ -> error ("[Memop context] could not find oper for"^(Cid.to_string x))
;;

let find_boolexp ctx x = 
  match CidMap.find_opt x ctx with 
   | Some(MCtBoolExp(o)) -> o
   | _ -> error ("[Memop context] could not find boolexp for"^(Cid.to_string x))
;;


(**** memop translation ****)
type memop_kind =
  | Get
  | Set



let translate_binop op = 
  match op with 
    | CoreSyntax.And -> And
    | CoreSyntax.Or -> Or
    | CoreSyntax.Not -> Not
    | _ -> error "[LLMemop.translate_binop] cannot be translated into a binop"
;;

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
let _workaround_flip_memcell_const operation : sArithExp =
  match operation with
  | SBinOp (Add, a, b) ->
    (match a, b with
    | RegVar _, Const _ -> SBinOp (Add, b, a)
    | _ -> operation)
  | _ -> operation
;;

let param_to_cid param = Cid.id (fst param) ;;

(* translate memop parameters to operands in the LLIR. 
   the parameters are of the form memory_params@local_params
   memory_params get translated to memCells
   local_params get translated into references to the variables in args *)
let ordered_arg_opers hdl_id params args : LLSyntax.oper list = 
  let num_meta_params = CL.length args in 
  let num_regvar_params = (CL.length params) - num_meta_params in   
  match num_regvar_params with 
    | 1 -> [RegVar Lo]@(args |> CL.map (oper_from_immediate hdl_id))
    | 2 -> [RegVar Lo; RegVar Hi]@(args |> CL.map (oper_from_immediate hdl_id))
    | _ -> error "[LLMemop.translate_params] got invalid arguments for given parameter list"
;;

(* core translators *) 

(* evars and evals get translated into operands *)
let exp_to_oper param_bindings (exp:S.exp) = 
  match exp.e with 
  | S.EVar cid -> 
    find_oper param_bindings cid
  | S.EVal _ -> IS.Const(zint_from_evalue exp)
  | _ -> error "[LLMemop.exp_to_oper] not an EVar!"
;;

(* translate an expression into a computation sub-instruction *) 
let exp_to_sArithExp param_bindings (exp:S.exp) = 
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
      "[LLMemop.translate] memop predicates must be a relational \
       operation or boolean value."

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
        [(None, exp_to_sArithExp param_bindings exp)]
      | S.MBIf (ifexp, b1exp, b2exp) ->
        let pos_pred = exp_to_sPredExpr param_bindings ifexp in 
        let b1_subinstr = exp_to_sArithExp param_bindings b1exp in 
        let b2_subinstr = exp_to_sArithExp param_bindings b2exp in 
        (* two instructions that return to the same place *)
        [(Some pos_pred, b1_subinstr); (Some (negate_pred pos_pred), b2_subinstr)]
      | S.MBComplex(_) -> error "Complex memops not yet implemented"
;;

(* bind the parameters of a memop to LLIR operands 
   referencing memory cells or variables. *)
let bind_memop_params hdl_id params args =  
  (* bind parameters of the memop *)
  let map = bind_lists 
    CidMap.empty 
    (CL.map param_to_cid params)
    (CL.map 
      (fun oper -> MCtxOper(oper)) 
      (ordered_arg_opers hdl_id params args))
  in 
  (* bind the builtins cell1 and cell2, which 
     reference the to-memory outputs of the sALU. *)
  let cell_cids = CL.map 
    Cid.id 
    [Builtins.cell1_id; Builtins.cell2_id] 
  in
  let cell_opers = [MCtxOper(IS.RegVar(IS.LoNew)); MCtxOper(IS.RegVar(IS.HiNew))] in
  let map = bind_lists map cell_cids cell_opers in    
  map 
;;


(* translate an expression into a boolean expression *)
let translate_bool_exp param_bindings (exp:S.exp) = 
  match exp.e with 
    (* (a + b) < c *)
  | S.EOp (rel_op, [lhs; rhs]) -> (
    match (lhs, rhs) with 
    | { e = S.EOp (_); _ }, _ ->
      let arith_expr = exp_to_sArithExp param_bindings lhs in       
      let rel_op = from_relop rel_op in
      let oper = exp_to_oper param_bindings rhs in 
      BRel(arith_expr, rel_op, oper)
    (* a < (b + c) *)
    | _, { e = S.EOp (_); _ } ->
      let oper = exp_to_oper param_bindings lhs in 
      let rel_op = flip_relop (from_relop rel_op) in
      let arith_expr = exp_to_sArithExp param_bindings rhs in       
      BRel(arith_expr, rel_op, oper)
    (* a < c *)
    | _, _ ->
      let arith_expr = exp_to_sArithExp param_bindings lhs in       
      let rel_op = from_relop rel_op in
      let oper = exp_to_oper param_bindings rhs in 
      BRel(arith_expr, rel_op, oper)
  )
  | S.EVal({v = S.VBool(b); _}) -> 
    BVal b
  | _ ->
    error
      "[LLMemop.translate] memop predicate does not contain a \
       relational operation at its root."

(* bind a bool declared by a bool statement in a complex body *)
let bind_memop_bool ctx b_opt = 
  match b_opt with 
    | Some(id, exp) -> (
        let b1 = translate_bool_exp ctx exp in 
        bind ctx (Cid.id id, MCtBoolExp(b1))
    )
    | _ -> ctx
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

(* let translate_memop_arith param_bindings arith_exp = 
;;
 *)
(*  Translate a boolean in a memop body 
    into a sBoolExp. *)
(* let translate_memop_bool param_bindings bool_exp = 

;;
 *)


(* helper: simplify a conditional expression in a memop
            this is an IR transformation that should maybe 
            be in its own pass. *)


(* 
Translation steps: 

1. translate b1 and b2 into boolean expressions 
   and put them into the context. 
2. in all non-return expressions, replace memArg1 with memLo
    (just inlining)
3. in the return expression, replace memArg1 with memLo and cell1 / cell2.
    cell1/cell2 should only occur here. 

  So really, this is just an inlining problem with several steps.
  1. translate params and add to context. 
  2. translate boolean expressions using context and add to context. 
  3. translate cell1, cell2, and cell3 using context. 
*)
let rec translate_cond_exp ctx (cond_exp:CoreSyntax.exp) : sCondExp = 
  match cond_exp.e with 
    | EVal({v = VBool(true); _}) -> CTrue
    | EVar(cid) ->
      (* variables must be locally declared, i.e. in context. *)
      let boolExpr = find_boolexp ctx cid in 
      CBool(boolExpr)
    (* negation or boolean operation *)
    | EOp(op, args) -> 
      let op = translate_binop op in 
      let args = CL.map (translate_cond_exp ctx) args in 
      COp(op, args)
    | _ -> error "[translate_cond_exp] Cannot translate conditional expression. Invalid form."
;;


(* translate a statement that updates either cell1 or cell2. *)
let translate_mem_update ctx update_exp_opt : sUpdateExp option = 
  match update_exp_opt with 
    | None -> None (* empty *)
    | Some(cond_exp, arith_exp) -> (
      let cond_exp = translate_cond_exp ctx cond_exp in 
      let arith_exp = exp_to_sArithExp ctx arith_exp in 
      match cond_exp with 
        | CTrue -> Some(None, arith_exp)
        | _ -> Some(Some cond_exp, arith_exp)
    )
;;

let translate_ret_update ctx (update_exp_opt:CoreSyntax.conditional_return option) : sUpdateExp option = 
  match update_exp_opt with 
    | None -> None (* empty *)
    | Some(cond_exp, oper_exp) -> (
      let cond_exp = translate_cond_exp ctx cond_exp in 
      (* the operand must be a variable in the context (specifically, 
         cell1, cell2, or one of the memory arguments) *)
      let ret_oper = match oper_exp.e with 
        | CoreSyntax.EVar(cid) -> SVar (find_oper ctx cid)
        | _ -> error "[LLMemop.translate_ret_update] error translating return expression -- can only be cell1, cell2, or one of the memory cell inputs"
      in 
      match cond_exp with 
        | CTrue -> Some(None, ret_oper)
        | _ -> Some(Some cond_exp, ret_oper)
    )
;;



(* These functions get the second return statement's condition into a 
   form that is easy for the backend P4 compiler to handle. *)

(* distribute the precondition x over the expression y, recursively *)
let rec distribute_precondition (x: S.exp) (y: S.exp) = 
  print_endline ("[distribute_precondition]");
  print_endline ("x: "^(CorePrinting.exp_to_string x));
  print_endline ("y: "^(CorePrinting.exp_to_string y));
  let y_e = match y.e with 
    | S.EVar(_)
    | S.EVal(_) 
    | S.ECall(_) -> S.EOp(S.And, [x; y])
    | S.EOp(o, inners) -> S.EOp(o, CL.map (distribute_precondition x) inners)
    | S.EHash(_) -> error "[distribute_precondition] got a hash -- expected bool expr"
    | S.EFlood(_) -> error "[distribute_precondition] got a flood -- expected bool expr"
  in 
  let new_y = {y with e=y_e;} in 
  print_endline ("new_y: "^(CorePrinting.exp_to_string new_y));
  new_y
;;

let distribute_and (b : S.exp) = 
  match b.e with 
    | S.EOp(And, [x; y]) -> 
      distribute_precondition x y
    | _ -> b
;;

(* simplify conjunctions and disjunctions that 
   contain only a single variables and its negation *)
   (* 
    annoying: 

    !(x && y) && y
    
    equality is not the right test. 
    I think implication is the test.     
    if one side implies the other, reduce the expression 
    to the stronger side. 

    ...not quite...
    !(x && y) && y
    --> 
    (!x || !y) && y
    --> 
    (!x && y) || (!y && y)
    --> 
    true

    (assert (= 
          true 
          (and 
              y
              (not
                  (and
                       x
                       y
                  )
              )
          )
        )
)
      and y (not (and x y))
      y and (not (x and y))
      !(x && y) && y

      !(x && y) --> 
      (x && !y) || (y && !x)

      !(x || y) --> (!x && !y)

      ---------
      !(x && y) && y
      ((x && !y) || (y && !x)) && y

      ((x && !y) && y) || ((y && !x) && y)
      x || (y && !x)
      (y || x) && (x || !x) 
      y || x

      How would we ever reduce that... 

      for each expression e, do: 
          1. recurse on each inner term
          2. 
            distribute negation 
            distribute and
            distribute or
            simplify
          3. 
            repeat if the e changed

      How do I know this is correct?




        simplify
        distribute
      do this recursively?





      okay, so maybe this is too complicated and we should just make a giant truth table... 

      expression 1 is:
      a
      b
      !a
      !b
      a  && b
      !a && b
      a  && !b
      !a && !b
      a  || b
      !a || b
      a  || !b
      !a || !b

      expression 2 is also that. 
      So there are 144 cases.... (12 * 12)

   *)
let rec simplify_negation_clauses (b : S.exp) = 
  let b_e = match b.e with 
    | S.EVar(_)
    | S.EVal(_) -> b.e
    (*  x ||!x 
       !x || x -> true
     *)
    | S.EOp(S.Or, [x; {e=S.EOp(S.Not, [y]); _}]) 
    | S.EOp(S.Or, [{e=S.EOp(S.Not, [x]); _}; y]) -> (
      if (S.equiv_exp x y)
        then ((S.vbool true |> S.value_to_exp).e)
        else (b.e)
    )
    (*  x &&!x 
       !x && x -> false
     *)
    | S.EOp(S.And, [x; {e=S.EOp(S.Not, [y]); _}]) 
    | S.EOp(S.And, [{e=S.EOp(S.Not, [x]); _}; y]) -> (
      if (S.equiv_exp x y)
        then ((S.vbool false |> S.value_to_exp).e)
        else (b.e)
    )
    (* x && x -> x; *)
    | S.EOp(S.And, [x; y]) -> (
      if (S.equiv_exp x y)
        then (x.e)
        else (b.e)
    )
    (* x || x -> x *)
    | S.EOp(S.Or, [x; y]) -> (
      if (S.equiv_exp x y)
        then (x.e)
        else (b.e)
    )
    (* recurse for non-atoms *)
    | S.EOp(o, inners) -> (
      let new_b = S.exp 
        (S.EOp(o, CL.map simplify_negation_clauses inners))
        b.ety
       in 
      if (S.equiv_exp new_b b)
      then (b.e)
      else ((simplify_negation_clauses new_b).e) 
    )
    | S.ECall(_) -> error "[simplify_negation_clauses] unexpected expression form"
    | _ -> error ""
  in 
  {b with e=b_e;}
;;








(* 
   condition the second return statement of both cell1 and cell2, 
   so that they can be implemented as a sequence of ifs instead of 
   an if / else: 

    if (<x>) 
      --> 
    if (!cell1_condition  && x) 
*)
let condition_snd_cr_stmts (b:CoreSyntax.complex_body) = 
  (* condition the second return statement of a single cell. *)
  let condition_snd_cr_stmt (cr_opt1, cr_opt2)  = 
    let cr_opt2 = match cr_opt1, cr_opt2 with 
      | None, None -> cr_opt2
      | Some _, None -> cr_opt2
      | None, Some _ -> cr_opt2
      | Some(c1_cond, _), Some(c2_cond, c2_exp) -> 
        (* c2_cond = !c1_cond && c2_cond 
          but, we have to simplify it for the P4 compiler
          DNF form seems to work well... *)
        (
          let not_c1 = S.exp (S.EOp(S.Not, [c1_cond])) c1_cond.ety in 
          let not_c1_and_c2 = S.exp (S.EOp(S.And, [not_c1; c2_cond])) c2_cond.ety in           
          let s, ze = CoreZ3.exp_to_expr not_c1_and_c2 in 
          let ze_simp = CoreZ3.simplify_to_dnf s ze in 
          let simplified_c2_cond = CoreZ3.expr_to_exp s ze_simp in 
          !dprint_endline ("not_c1: "^(CorePrinting.exp_to_string not_c1));
          !dprint_endline ("c2_cond: "^(CorePrinting.exp_to_string c2_cond));
          !dprint_endline ("raw c2: "^(CorePrinting.exp_to_string not_c1_and_c2));
          !dprint_endline ("simplified c2: "^(CorePrinting.exp_to_string simplified_c2_cond));
          Some(simplified_c2_cond, c2_exp)
        )
    in 
    (cr_opt1, cr_opt2)
  in
  {b with cell1 = condition_snd_cr_stmt b.cell1; cell2 = condition_snd_cr_stmt b.cell2;}
;;


let translate_complex_memop hdl_id name_exp arg1_ex arg2_ex default_ex : IS.sInstrBody = 
  let _, _ ,_, _, _ = hdl_id, name_exp, arg1_ex, arg2_ex, default_ex in 
  (* get the memop *)
  let memop_cid = name_from_exp name_exp in 
  let params, body = ctx_bdy_of_memop memop_cid in
  let b = match body with 
    | S.MBComplex(b) -> b
    | _ -> error "[translate_complex_memop] this function only handles complex memops right now."
  in 
  (* transform and simplify the condition of the second conditional statement *)
  let b = condition_snd_cr_stmts b in 
  (* 1. translate params and add to context *)
  let ctx = bind_memop_params hdl_id params [arg1_ex; arg2_ex] in 
  (* 2. translate bools and add to context. *)
  let ctx = bind_memop_bool ctx b.b1 in 
  let ctx = bind_memop_bool ctx b.b2 in 
  (* 3. useing context, translate cell1, cell2, and ret expressions *)

  let cell1 = 
    ( translate_mem_update ctx (fst b.cell1)
    , translate_mem_update ctx (snd b.cell1) )
  in 
  let cell2 = 
    ( translate_mem_update ctx (fst b.cell2)
    , translate_mem_update ctx (snd b.cell2) )
  in 
  let ret = translate_ret_update ctx b.ret in 

  { IS.cell1 = cell1
  ; IS.cell2 = cell2
  ; IS.ret = ret }
;;

