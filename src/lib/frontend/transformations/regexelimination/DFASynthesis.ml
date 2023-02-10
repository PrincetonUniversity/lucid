open Syntax
open PlainRegex
open Z3

module LetterMap = Map.Make(struct type t = plain_re_symbol let compare = compare end)
module StatesMap = Map.Make(struct type t = plain_re let compare = compare end)

let bv_size = 8;;
let regact_ids = [0];;

type memop_response = 
{
  regact_int : int;
  id : id;
  params : params;
  memop_body : memop_body
}

type synthesis_response = 
{
  accepting : int list;
  f : int LetterMap.t;
  g : int LetterMap.t;
  whichop : int LetterMap.t;
  memops : memop_response list;
  init : int
}

type pred = {
  pregact_id : int;
  pred_id : int; 
  pop1 : Expr.expr;
  pop2 : Expr.expr;
  pconst : Expr.expr;
  psym_opt_const : Expr.expr;
  psym_opt_which_sym : Expr.expr;
  pstate_opt_const : Expr.expr
}

let make_transition_pred_cond ctx pred pre_state f g =
  let zero = Expr.mk_numeral_int ctx 0 (BitVector.mk_sort ctx bv_size) in
  let sym_choice = Boolean.mk_ite ctx pred.psym_opt_which_sym f g in
  let sym_opt_val = Boolean.mk_ite ctx pred.psym_opt_const zero sym_choice in
  let pred_LHS = Boolean.mk_ite ctx pred.pstate_opt_const sym_opt_val (BitVector.mk_add ctx pre_state sym_opt_val) in 
  let pred_val = Boolean.mk_ite ctx pred.pop1 
    (Boolean.mk_ite ctx pred.pop2 (Boolean.mk_eq ctx pred_LHS pred.pconst) (BitVector.mk_ugt ctx pred_LHS pred.pconst)) 
    (Boolean.mk_ite ctx pred.pop2 (BitVector.mk_ult ctx pred_LHS pred.pconst) (Boolean.mk_not ctx (Boolean.mk_eq ctx pred_LHS pred.pconst))) in
  pred_val
;;

type arith = {
  aregact_id : int;
  arith_id : int;
  aop1 : Expr.expr; 
  aop2 : Expr.expr;
  aop3 : Expr.expr; 
  asym_const : Expr.expr;
  asym_opt_const : Expr.expr;
  asym_opt_which_sym : Expr.expr;
  astate_const : Expr.expr;
  astate_opt_const : Expr.expr
}

let make_transition_arith_cond ctx arith pre_state f g = 
  let sym_choice = Boolean.mk_ite ctx arith.asym_opt_const arith.asym_const (Boolean.mk_ite ctx arith.asym_opt_which_sym f g) in
  let state_choice = Boolean.mk_ite ctx arith.astate_opt_const arith.astate_const pre_state in
  let make_ite_ops b op1 op2 = Boolean.mk_ite ctx b (op1 ctx state_choice sym_choice) (op2 ctx state_choice sym_choice) in
  Boolean.mk_ite ctx arith.aop1 (make_ite_ops arith.aop2 BitVector.mk_add BitVector.mk_and) (make_ite_ops arith.aop2 BitVector.mk_xor BitVector.mk_or)
;;


type regact = {
  regact_id : int;
  num_pred : int;
  num_arith : int;
  pred_logic_op : Expr.expr;
  preds : pred list;
  ariths : arith list;
}

let make_transition_regact_cond ctx regact pre_state f g = 
  let pred_conds = List.map (fun pred -> make_transition_pred_cond ctx pred pre_state f g) regact.preds in
  let connect f = f ctx pred_conds in
  let pred_val = Boolean.mk_ite ctx regact.pred_logic_op (connect Boolean.mk_and) (connect Boolean.mk_or) in
  let arith_vals = List.map (fun arith -> make_transition_arith_cond ctx arith pre_state f g) regact.ariths in
  Boolean.mk_ite ctx pred_val (List.nth arith_vals 0) (List.nth arith_vals 1)
;;

let letter_to_sym_f letter = 
  "sym_f"^(print_letter letter);;
let letter_to_sym_g letter = 
  "sym_g"^(print_letter letter);;
let state_to_state_1 pre = 
  "states_1"^(plain_re_to_string pre);;
let letter_to_whichop_out letter = 
  "whichop_outer"^(print_letter letter);;
let letter_to_whichop_in letter = 
  "whichop_inner"^(print_letter letter);;

let make_transition_constraints ctx regacts pre_state whichop_pair f g post_state = 
  (*let outer = (fst whichop_pair) in
  let inner = (snd whichop_pair) in
  let premise_i i = 
    (match i with 
    | 0 -> Boolean.mk_and ctx [(Boolean.mk_not ctx outer); (Boolean.mk_not ctx inner)]
    | 1 -> Boolean.mk_and ctx [(Boolean.mk_not ctx outer); inner]
    | 2 -> Boolean.mk_and ctx [outer; (Boolean.mk_not ctx inner)]
    | 3 -> Boolean.mk_and ctx [outer; inner]) in
  List.mapi (fun i regact -> Boolean.mk_implies ctx (premise_i i) (Boolean.mk_eq ctx post_state (make_transition_regact_cond ctx regact pre_state f g))) regacts;;*)
  List.map (fun regact -> Boolean.mk_eq ctx post_state (make_transition_regact_cond ctx regact pre_state f g)) regacts;;
  (*(*TODO: update for multiple regact*)
  
  let reg_list = List.map (fun regact -> make_transition_regact_cond ctx regact pre_state f g) regacts in
  (*let lhs = Boolean.mk_ite ctx outer (Boolean.mk_ite ctx inner (List.nth reg_list 3) (List.nth reg_list 2)) (Boolean.mk_ite ctx inner (List.nth reg_list 1) (List.nth reg_list 0)) in*)
  [Boolean.mk_eq ctx post_state (List.nth reg_list 0)];;*)

let make_pred ctx regact_id pred_id = 
  {
    pregact_id = regact_id;
    pred_id = pred_id;
    pop1 = Boolean.mk_const_s ctx (Printf.sprintf "pred_op_1_%d_%d" regact_id pred_id);
    pop2 = Boolean.mk_const_s ctx (Printf.sprintf "pred_op_2_%d_%d" regact_id pred_id);
    pconst = BitVector.mk_const_s ctx (Printf.sprintf "pred_const_%d_%d" regact_id pred_id) bv_size;
    psym_opt_const = Boolean.mk_const_s ctx (Printf.sprintf "pred_sym_opt_const_%d_%d" regact_id pred_id);
    psym_opt_which_sym = Boolean.mk_const_s ctx (Printf.sprintf "pred_sym_opt_which_sym_%d_%d" regact_id pred_id);
    pstate_opt_const = Boolean.mk_const_s ctx (Printf.sprintf "pred_state_opt_const_%d_%d" regact_id pred_id)
  };;

let make_arith ctx regact_id arith_id = 
  {
    aregact_id = regact_id;
    arith_id = arith_id;
    aop1 = Boolean.mk_const_s ctx (Printf.sprintf "arith_op_1_%d_%d" regact_id arith_id); 
    aop2 = Boolean.mk_const_s ctx (Printf.sprintf "arith_op_2_%d_%d" regact_id arith_id);
    aop3 = Boolean.mk_const_s ctx (Printf.sprintf "arith_op_3_%d_%d" regact_id arith_id);
    asym_const = BitVector.mk_const_s ctx (Printf.sprintf "arith_sym_const_%d_%d" regact_id arith_id) bv_size;
    asym_opt_const = Boolean.mk_const_s ctx (Printf.sprintf "arith_sym_opt_const_%d_%d" regact_id arith_id); 
    asym_opt_which_sym = Boolean.mk_const_s ctx (Printf.sprintf "arith_sym_opt_which_sym_%d_%d" regact_id arith_id); 
    astate_const = BitVector.mk_const_s ctx (Printf.sprintf "arith_state_const_%d_%d" regact_id arith_id) bv_size;
    astate_opt_const = Boolean.mk_const_s ctx (Printf.sprintf "arith_state_opt_const_%d_%d" regact_id arith_id)
  };;

let make_regact ctx regact_id = 
  {
    regact_id = regact_id;
    num_pred = 1;
    num_arith=2;
    pred_logic_op = Boolean.mk_const_s ctx (Printf.sprintf "pred_logic_op_%d" regact_id);
    preds = List.map (make_pred ctx regact_id) [0; 1];
    ariths = List.map (make_arith ctx regact_id) [0; 1]
  };;

let make_uniqueness_constraints ctx states_map = 
  let uniqueness_constraint s1 s2 = Boolean.mk_not ctx (Boolean.mk_eq ctx s1 s2) in
  let fold_f state seen key v acc = 
    if States.mem key seen then acc else (uniqueness_constraint state v) :: acc in
  snd (StatesMap.fold (fun key v acc -> let seen = (States.add key (fst acc)) in (seen, (List.append (StatesMap.fold (fold_f v seen) states_map []) (snd acc)))) states_map (States.empty, []))
;;

let make_evar id = (exp (EVar (Cid.create_ids [id])))

let make_num i = exp (EVal(vinteger (Integer.of_int i)));;

let make_num_size i size = exp (EVal (vinteger (Integer.create i size)))

let eval_bool model b =
  match Model.eval model b true with
  | None -> false
  | Some expr -> Boolean.is_true expr
;;

let eval_bv ctx model bv =
  match Model.eval model (BitVector.mk_bv2int ctx bv false) true with 
  | None -> 0
  | Some expr -> (int_of_string (Expr.to_string expr))
;;

let make_memop_pred_exp ctx model pred = 
  let state = make_evar (Id.create "memval") in
  let sym_choice = make_evar (Id.create (if (eval_bool model pred.psym_opt_which_sym) then "f" else "g")) in
  let sym_const = (make_num_size (eval_bv ctx model pred.pconst)) bv_size in
  let sym_compound = if (eval_bool model pred.psym_opt_const) then (make_num_size 0 bv_size) else sym_choice in
  let lhs = if (eval_bool model pred.pstate_opt_const) then sym_compound else (exp (EOp (Plus, [state; sym_compound]))) in
  let comp = if (eval_bool model pred.pop1) then (if (eval_bool model pred.pop2) then Eq else More) else (if (eval_bool model pred.pop2) then Less else Neq) in
  (exp (EOp (comp, [lhs; sym_const])))
;;

let make_memop_arith_exp ctx model arith = 
  let state = make_evar (Id.create "memval") in
  let sym_choice = 
    if (eval_bool model arith.asym_opt_const) then make_num_size (eval_bv ctx model arith.asym_const) bv_size else 
      make_evar (Id.create (if (eval_bool model arith.asym_opt_which_sym) then "f" else "g")) in
  let state_choice = if (eval_bool model arith.astate_opt_const) then make_num_size (eval_bv ctx model arith.astate_const) bv_size else state in
  let op1 = (eval_bool model arith.aop1) in
  let op2 = (eval_bool model arith.aop2) in
  let op = if op1 then (if op2 then Plus else BitAnd) else (if op2 then BitXor else BitOr) in
  (exp (EOp (op, [sym_choice; state_choice])))
;;

let make_memop_connect ctx model regact = 
  exp (EOp ((if (eval_bool model regact.pred_logic_op) then And else Or), [(make_evar (Id.create "b1")); (make_evar (Id.create "b2"))]))

let make_memop ctx id model regact =
{
  regact_int = regact.regact_id;
  id = Id.create ((fst id)^"memop"^(string_of_int regact.regact_id));
  params = [((Id.create "memval"), (ty (TInt (IConst bv_size)))); ((Id.create "f"), (ty (TInt (IConst bv_size)))); ((Id.create "g"), (ty (TInt (IConst bv_size))))];
  memop_body = MBComplex({
    b1 = Some ((Id.create "b1"), make_memop_pred_exp ctx model (List.nth regact.preds 0));
    b2 = Some ((Id.create "b2"), make_memop_pred_exp ctx model (List.nth regact.preds 1));
    cell1 = (Some ((make_memop_connect ctx model regact), (make_memop_arith_exp ctx model (List.nth regact.ariths 0))), 
            Some ((exp (EVal (value (VBool true)))), (make_memop_arith_exp ctx model (List.nth regact.ariths 1))));
    cell2 = None, None;
    extern_calls = [];
    ret = Some ((exp (EVal (value (VBool true)))), (make_evar (Id.create "cell1")))
  })
}  


let mock_memop_response id = 
  {
    regact_int = 0;
    id = Id.create ((fst id)^"memop0");
    params = [((Id.create "memval"), (ty (TInt (IConst bv_size)))); ((Id.create "f"), (ty (TInt (IConst bv_size)))); ((Id.create "g"), (ty (TInt (IConst bv_size))))];
    memop_body= MBComplex ({
      b1 = None;
      b2 = None;
      cell1 = (Some ((exp (EVal (value (VBool true)))), (make_evar (Id.create "f"))), None);
      cell2 = (None, None);
      extern_calls = [];
      ret = Some ((exp (EVal (value (VBool true)))), (make_evar (Id.create "cell1")))
    })
  }


let int_id_of_pair model (out, inner) = 0;;
  (*let out = (eval_bool model out) in
  let inner = (eval_bool model inner) in
  if out then (if inner then 3 else 2) else (if inner then 1 else 0)*)

let at_most_eight ctx bv = 
  BitVector.mk_ult ctx bv (Expr.mk_numeral_int ctx 9 (BitVector.mk_sort ctx bv_size));;


let make_init_constraints ctx regact = 
  let pred_inits = List.map (fun p -> (at_most_eight ctx p.pconst)) regact.preds in
  let arith_inits = List.flatten (List.map (fun a -> [(at_most_eight ctx a.asym_const);(at_most_eight ctx a.astate_const)]) regact.ariths) in
  List.append pred_inits arith_inits

let synthesize id dfa = 
  let time = Sys.time() in
  let cfg = [("model", "true"); ("proof", "false")] in
  let ctx = mk_context cfg in
  let sim = Tactic.mk_tactic ctx "simplify" in
  let solve_eq = Tactic.mk_tactic ctx "solve-eqs" in
  let bit_blast = Tactic.mk_tactic ctx "bit-blast" in
  let qffd = Tactic.mk_tactic ctx "qffd" in
  let sat = Tactic.mk_tactic ctx "sat" in
  let tac = Tactic.and_then ctx sim solve_eq [bit_blast; qffd; sat] in 
  let solver = Solver.mk_solver_t ctx tac in
  let regacts = List.map (fun id -> (make_regact ctx id)) regact_ids in
  List.iter (fun regact -> (Solver.add solver (make_init_constraints ctx regact))) regacts; 
  let symbols_whichop = List.fold_left (fun map letter -> LetterMap.add letter ((Boolean.mk_const_s ctx (letter_to_whichop_out letter)), (Boolean.mk_const_s ctx (letter_to_whichop_in letter))) map) LetterMap.empty dfa.alphabet in
  let symbols_f = List.fold_left (fun map letter -> LetterMap.add letter (BitVector.mk_const_s ctx (letter_to_sym_f letter) bv_size) map) LetterMap.empty dfa.alphabet in
  let symbols_g = List.fold_left (fun map letter -> LetterMap.add letter (BitVector.mk_const_s ctx (letter_to_sym_g letter) bv_size) map) LetterMap.empty dfa.alphabet in
  let states = States.fold (fun pre map -> StatesMap.add pre (BitVector.mk_const_s ctx (state_to_state_1 pre) bv_size) map) dfa.states StatesMap.empty in 
  Solver.add solver [(Boolean.mk_eq ctx (StatesMap.find dfa.initial states) (Expr.mk_numeral_int ctx 0 (BitVector.mk_sort ctx bv_size)))];
  Solver.add solver [Boolean.mk_and ctx (make_uniqueness_constraints ctx states)];
  let add_transition key res = 
    let pre_state = StatesMap.find (fst key) states in
    let post_state = StatesMap.find res states in
    let whichop_pair = LetterMap.find (snd key) symbols_whichop in
    let f = LetterMap.find (snd key) symbols_f in
    let g = LetterMap.find (snd key) symbols_g in 
    Solver.add solver (make_transition_constraints ctx regacts pre_state whichop_pair f g post_state) in
  Transition.iter add_transition dfa.transition;
  print_endline (Solver.to_string solver);
  let stat = (Solver.check solver []) in
  Printf.printf "Status is %s. Time spent on synthesis is %f\n" (Solver.string_of_status stat) (Sys.time() -. time);
  let model = Solver.get_model solver in
    (match model with 
    | None -> Console.error "Failed to find DFA synthesis model."
    | Some model -> 
      Printf.printf "States map has size %d" (StatesMap.cardinal states);
      StatesMap.iter (fun st var -> (Printf.printf "%s:%d\n") (plain_re_to_string st) (eval_bv ctx model var)) states;
      Printf.printf "solved.";
      {
        accepting = States.fold (fun state acc -> (eval_bv ctx model (StatesMap.find state states)) :: acc) dfa.accepting []; 
        f = LetterMap.map (fun res -> (eval_bv ctx model res)) symbols_f;
        g = LetterMap.map (fun res -> (eval_bv ctx model res)) symbols_g;
        whichop = LetterMap.map (fun res -> (int_id_of_pair model) res) symbols_whichop;
        memops = List.map (make_memop ctx id model) regacts;
        init = 0
      })
  ;;

