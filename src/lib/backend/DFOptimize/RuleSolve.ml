(***** use z3 to reason about ordered rule matching feasibility *****)

open Format
open LLSyntax
open MiscUtils
open DFSyntax
open Consts
open Batteries
open Printf
open MiscUtils
open Z3
open Solver
module Z3Bool = Boolean
module Z3Int = Arithmetic.Integer

(* print rule lists *)
let string_of_match (m : pattern) : string =
  CL.map
    (fun (m_var, m_val) ->
      sprintf
        "%s = %s"
        (P4tPrint.str_of_varid m_var)
        (match m_val with
        | Exact i -> Int.to_string (Integer.to_int i)
        | Any -> "*"))
    m
  |> Caml.String.concat ","
;;

let string_of_rule (r : rule) =
  match r with
  | Match (r_id, m_exp, a_id) ->
    sprintf
      "%s: (%s) -> %s"
      (P4tPrint.str_of_private_oid r_id)
      (string_of_match m_exp)
      (P4tPrint.str_of_private_oid a_id)
  | OffPath m_exp -> sprintf "    (%s) -> %s" (string_of_match m_exp) "OFFPATH"
;;

let string_of_rules t : string =
  Caml.String.concat "\n" (CL.map (fun r -> sprintf "%s" (string_of_rule r)) t)
;;

let string_of_rule_pair ((r1 : rule), (r2 : rule)) =
  sprintf "(%s, %s)" (string_of_rule r1) (string_of_rule r2)
;;

(* generate a z3 equation from a pattern *)
let eqn_of_pat ctx (m_exp : pattern) =
  let fold_f (ctx, terms) m_exp_entry =
    match m_exp_entry with
    | vid, Exact vint ->
      let z3_vid = Z3Int.mk_numeral_i ctx (Integer.to_int vint) in
      let z3_vint = Z3Int.mk_const_s ctx (Cid.to_string vid) in
      let term = Z3Bool.mk_eq ctx z3_vid z3_vint in
      ctx, terms @ [term]
    | _ -> ctx, terms
  in
  let ctx, terms = CL.fold_left fold_f (ctx, []) m_exp in
  let eqn = Z3Bool.mk_and ctx terms in
  ctx, eqn
;;

(* generate a z3 equation from a rule *)
let eqn_of_rule ctx (r : rule) =
  let m_exp =
    match r with
    | Match (_, m_exp, _) -> m_exp
    | OffPath m_exp -> m_exp
  in
  eqn_of_pat ctx m_exp
;;

(* build an equation that represents matching a 
   rule at a specific index in an ordered rules list *)
let eqn_of_ordered_rule ctx (rules : rule list) (rule : rule) =
  let matched_fst_rules, _ =
    BatList.split_at (Caml.Option.get (BatList.index_of rule rules)) rules
  in
  let ctx, neg_eqns =
    CL.fold_left
      (fun (ctx, neg_eqns) rule ->
        let ctx, new_eqn = eqn_of_rule ctx rule in
        ctx, neg_eqns @ [new_eqn])
      (ctx, [])
      matched_fst_rules
  in
  let neg_eqn = Z3Bool.mk_not ctx (Z3Bool.mk_or ctx neg_eqns) in
  let ctx, pos_eqn = eqn_of_rule ctx rule in
  let full_eqn = Z3Bool.mk_and ctx [neg_eqn; pos_eqn] in
  full_eqn
;;

let is_rule_tuple_feasible
    (s : rule list)
    (t : rule list)
    (s_rule : rule)
    (t_rule : rule)
  =
  let ctx = mk_context ["model", "true"; "proof", "true"] in
  let s_eqn = eqn_of_ordered_rule ctx s s_rule in
  let t_eqn = eqn_of_ordered_rule ctx t t_rule in
  let solver = Solver.mk_simple_solver ctx in
  Solver.add solver [s_eqn; t_eqn];
  let is_sat = Solver.check solver [] in
  match is_sat with
  | UNSATISFIABLE -> false
  | SATISFIABLE -> true
  | UNKNOWN ->
    Printf.printf "unknown\n";
    error "unknown sat..."
;;

(* return an assoc list from s * t tuples to feasibility *)
let find_feasible_paths (s : rule list) (t : rule list)
    : ((rule * rule) * bool) list
  =
  printf "---finding feasible paths between rule lists---\n";
  let check_pair_by_name (s_rule, t_rule) =
    let s_idx = BatList.index_of s_rule s |> Option.get in
    let t_idx = BatList.index_of t_rule t |> Option.get in
    let feas = is_rule_tuple_feasible s t s_rule t_rule in
    printf
      "\t%s [position %i] --> %s [position %i] feasible? %s\n"
      (string_of_rule s_rule)
      s_idx
      (string_of_rule t_rule)
      t_idx
      (Bool.to_string feas);
    feas
  in
  let all_rule_pairs = get_all_pairs s t in
  let pair_feas = CL.map check_pair_by_name all_rule_pairs in
  CL.combine all_rule_pairs pair_feas
;;

(* is s_rule in s feasible? 
(it may not be, if higher priority rules 
match everything that it matches) *)
let is_rule_feasible (s : rule list) (s_rule : rule) =
  let ctx = mk_context ["model", "true"; "proof", "true"] in
  let rule_eqn = eqn_of_ordered_rule ctx s s_rule in
  let solver = Solver.mk_simple_solver ctx in
  Solver.add solver [rule_eqn];
  let is_sat = Solver.check solver [] in
  match is_sat with
  | UNSATISFIABLE -> false
  | SATISFIABLE -> true
  | UNKNOWN ->
    Printf.printf "unknown\n";
    error "unknown sat..."
;;

(* is the intersection of s and t feasible? *)
let is_intersection_feasible (s : rule) (t : rule) =
  let ctx = mk_context ["model", "true"; "proof", "true"] in
  let ctx, s_eqn = eqn_of_rule ctx s in
  let ctx, t_eqn = eqn_of_rule ctx t in
  let intersect_eqn = Z3Bool.mk_and ctx [s_eqn; t_eqn] in
  let solver = Solver.mk_simple_solver ctx in
  Solver.add solver [intersect_eqn];
  let is_sat = Solver.check solver [] in
  match is_sat with
  | UNSATISFIABLE -> false
  | SATISFIABLE -> true
  | UNKNOWN ->
    Printf.printf "unknown\n";
    error "unknown sat..."
;;

(* is rule r still feasible after matching on the rule in qs?
		(can anything match ~qs and r)?
*)
let is_r_still_feasible (r : rule) (qs : rule list) =
  let ctx = mk_context ["model", "true"; "proof", "true"] in
  let ctx, r_eqn = eqn_of_rule ctx r in
  let _, q_eqns = CL.split (CL.map (eqn_of_rule ctx) qs) in
  let qs_eqn = Z3Bool.mk_and ctx q_eqns in
  let not_q_eqn = Z3Bool.mk_not ctx qs_eqn in
  let intersect_eqn = Z3Bool.mk_and ctx [not_q_eqn; r_eqn] in
  let solver = Solver.mk_simple_solver ctx in
  Solver.add solver [intersect_eqn];
  let is_sat = Solver.check solver [] in
  match is_sat with
  | UNSATISFIABLE -> false
  | SATISFIABLE -> true
  | UNKNOWN ->
    Printf.printf "unknown\n";
    error "unknown sat..."
;;

(* is a pattern pat still feasible after matching all the 
   patterns in preds? (i.e., can anything match <pat && (!preds)> *)
let is_pat_still_feasible (pat : pattern) (preds : pattern list) =
  match preds with
  | [] -> true
  | _ ->
    let ctx = mk_context ["model", "true"; "proof", "true"] in
    let ctx, pat_eqn = eqn_of_pat ctx pat in
    let _, pred_eqns = CL.split (CL.map (eqn_of_pat ctx) preds) in
    let preds_eqn = Z3Bool.mk_and ctx pred_eqns in
    let not_preds_eqn = Z3Bool.mk_not ctx preds_eqn in
    let intersect_eqn = Z3Bool.mk_and ctx [not_preds_eqn; pat_eqn] in
    let solver = Solver.mk_simple_solver ctx in
    Solver.add solver [intersect_eqn];
    let is_sat = Solver.check solver [] in
    (match is_sat with
    | UNSATISFIABLE -> false
    | SATISFIABLE -> true
    | UNKNOWN ->
      Printf.printf "unknown\n";
      error "unknown sat...")
;;

(* can rule r be reached after missing all the previous rules? *)
let is_reachable_in_order all_rules rule =
  let ctx = mk_context ["model", "true"; "proof", "true"] in
  let eqn = eqn_of_ordered_rule ctx all_rules rule in
  let solver = Solver.mk_simple_solver ctx in
  Solver.add solver [eqn];
  let is_sat = Solver.check solver [] in
  match is_sat with
  | UNSATISFIABLE -> false
  | SATISFIABLE -> true
  | UNKNOWN ->
    Printf.printf "unknown\n";
    error "unknown sat..."
;;
