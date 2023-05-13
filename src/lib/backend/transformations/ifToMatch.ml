(* ifToMatch -- convert if statements to match statements.

Before running this pass, the expressions that if 
statements test must be in the following canonical form: 
a disjunction of terms, 
    where each term is a conjunction of atom, 
        where each atom is an equality or inequality check 
        testing a variable against a constant


TODO: We should merge this with MatchAlgebra.ml.
The two modules have similar syntaxes for patterns and 
rules and there are many duplicated functions. 

BUG: this pass breaks for if statements that have 
multiple equality conditions on the same variable 
(unreachable, can detect statically). 
Either fix pass or add static analysis to simplify those if's
*) 

open Core

open CoreSyntax
open TofinoCore
open InterpHelpers

exception Error of string
let error s = raise (Error s)



(* the core algorithm is pulled out of the old backend *)
module FromOldBackend = struct 
  type pattern = (Cid.t * pat) list 
  type binary_rule =
    | BTrue of pattern
    | BFalse of pattern

  type binary_rules = binary_rule list

  let string_of_pattern pattern =
    CL.map 
      (
        fun (cid, pat) -> (
          (Cid.to_string cid)
          ^" : "
          ^(CorePrinting.pat_to_string pat)
        )
      )
      pattern
    |> Caml.String.concat ","
  ;;

  let string_of_patterns brs =
    CL.map string_of_pattern brs |> Caml.String.concat "\n"
  ;;

  let string_of_binary_rule br = 
    match br with
    | BTrue(pattern) -> "BTrue("^(string_of_pattern pattern)^")"
    | BFalse(pattern) -> "BFalse("^(string_of_pattern pattern)^")"
  ;;

  let string_of_binary_rules brs =
    CL.map string_of_binary_rule brs |> Caml.String.concat "\n"
  ;;

    let get_keys exp =
        vars_in_exp exp |> MiscUtils.unique_list_of
    ;;
    let get_toplevel_exps = flatten_disjunction
    let get_atom_exps = flatten_conjunction

  (* get the mapping from key (which appears in the atom) to value *)
    let field_pat_of_atom_exp exp : (Cid.t * pat) =
        match exp.e with
        | EOp (Eq, [evar; eval]) | EOp (Neq, [evar; eval]) ->
          ( name_from_exp evar
          , CoreSyntax.PNum (Z.of_int (int_from_exp eval)))
        | _ -> error "unexpected form of expression to convert into a pattern. "
    ;;
    (* get a pattern that encodes exact matches for the keys in the atoms,
    and wildcard for the other keys. *)
    let pattern_of_atoms keys atoms : pattern =
        let pattern = CL.map (field_pat_of_atom_exp) atoms in
        let condition_of_key key =
          match CL.assoc_opt key pattern with
          | Some cond -> cond
          | None -> PWild
        in
        let key_conditions = CL.map condition_of_key keys in
        CL.combine keys key_conditions
    ;;

    let binary_rules_from_toplevel_exp keys toplevel_exp : binary_rules =
        let atom_exps = get_atom_exps toplevel_exp in
        let eqs = CL.filter (filter_eop_kind Eq) atom_exps in
        let neqs = CL.filter (filter_eop_kind Neq) atom_exps in
        (* neq_rules -- one rule that evaluates to false for
           every atom that does a not equals test *)
        let neq_rules =
          let mapper neq : binary_rule = 
              BFalse (pattern_of_atoms keys (neq :: eqs))
          in 
          CL.map mapper neqs
        in
        (* eq_rules -- one rule for all the conditions that must hold for
           the term to evaluate to true, after all the inequalities
           have been tested *)
        let (eq_rules:binary_rule list) = [BTrue (pattern_of_atoms keys eqs)] in
        neq_rules @ eq_rules
    ;;

    open MiscUtils
    (**** from mergeutils ****)

    (* extend a pattern so that it has a column for every var in vars *)
    let extend_pat (vars : Cid.t list) (pat : pattern) : pattern =
      let conds =
        CL.map (fun v -> Caml.Option.value (Cid.lookup_opt pat v) ~default:PWild) vars
      in
      CL.combine vars conds
    ;;
    (* make sure that a_pat and b_pat both have the same columns in the same order *)
    let normalize_patterns a_pat b_pat =
      let a_vars, _ = CL.split a_pat in
      let b_vars, _ = CL.split b_pat in
      let vars = unique_list_of (a_vars @ b_vars) in
      let a_pat = extend_pat vars a_pat in
      let b_pat = extend_pat vars b_pat in
      a_pat, b_pat
    ;;

(* produce a bitstring condition that is x && y 
   return None if the condition is unsatisfiable *)
let rec and_bitstrings (xs: int list) (ys:int list) : (int list option) = 
  match (xs, ys) with
  | ([], []) -> 
    (* empty bitstrings *)
    Some []    
  | (_, []) | ([], _) -> 
    error "[and_bitstrings] bitstring length mismatch"
    (* length mismatch *) 
  | (x::xs, y::ys) -> (
    let tail_opt = and_bitstrings xs ys in 
    match tail_opt with 
      | None -> None
      | Some (tail) -> (
        match (x, y) with 
        | (0, 1) 
        | (1, 0) -> None
        | (0, 0)
        | (0, -1)
        | (-1, 0) -> Some (0::tail)
        | (1, 1)
        | (1, -1) 
        | (-1, 1) -> Some (1::tail)
        | (-1, -1) -> Some (-1::tail)
        | (_, _) -> error "something in a bitstring is not a 0, 1, or -1"
      )
  )
;;

(* convert an integer into a bitstring *)
let int_to_bitstring (i:z) : int list = 
  let rec int_to_bits_rev (i:int) : int list = 
    match (i) with 
    | 0 -> []
    | _ -> 
      let lastbit = match (Int.(land) i 1) with
        | 0 -> 0
        | 1 -> 1
        | _ -> error "[int_to_bits_rev] impossible case: (i && 1) > 1"
      in 
      lastbit::(int_to_bits_rev (Int.shift_right i 1))
  in 
  CL.rev (int_to_bits_rev (Z.to_int i))
;;

let bitstring_to_maskedint (bs : int list) : int*int = 
  let to_val_and_mask bit = 
    match bit with 
    | 0    -> (0, 1) (* val: 0, mask 1 *) 
    | 1    -> (1, 1) (* val: 1, mask 1 *) 
    | -1  -> (0, 0) (* val: 0, mask :0 *)
    | _ -> error "invalid bitstring bit"
  in
  let rec bitstring_to_int bits =  
    match bits with 
      | [] -> 0
      | hd::tl ->
        (Int.shift_left hd (List.length tl)) + (bitstring_to_int tl)
  in      
  let vbits, mbits = CL.map to_val_and_mask bs |> CL.split in 
  bitstring_to_int vbits, bitstring_to_int mbits
;;


(* find the intersection of two pats *)
let intersect_pats a b : pat option = 
    match (a, b) with 
    | (PWild, _) -> Some b
    | (_, PWild) -> Some a
    | (PNum za, PNum zb) -> (
        if (Z.equal za zb)
        then Some a
        else None
    )
    | (PBit ba, PBit bb) -> (
        match (and_bitstrings ba bb) with 
        | Some ab_bits -> Some (PBit(ab_bits))
        | None -> None
    )
    | (PBit bits, PNum const) | (PNum const , PBit bits) -> (
        match (and_bitstrings bits (int_to_bitstring const)) with 
            | Some ab_bits -> Some (PBit ab_bits)
            | None -> None
    )
;;


    (* find the intersection of patterns a and b *)
    let intersect_patterns (a_pat : pattern) (b_pat : pattern) : pattern option =
      let a_pat, b_pat = normalize_patterns a_pat b_pat in
      let vars, _ = CL.split a_pat in
      (* get the intersection conditions *)
      let _, a_conds = CL.split a_pat in
      let _, b_conds = CL.split b_pat in
      let ab_conds = CL.combine a_conds b_conds in
      let ab_conds =
        CL.map (fun (a_cond, b_cond) -> intersect_pats a_cond b_cond) ab_conds
      in
      let has_intersect =
        CL.fold_left
          (fun intersect_exists cond_opt ->
            match cond_opt with
            | None -> false
            | Some _ -> intersect_exists)
          true
          ab_conds
      in
      match has_intersect with
      (* if there's an intersection, return it *)
      | true -> Some (CL.combine vars (CL.map Caml.Option.get ab_conds))
      (* *)
      | false -> None
    ;;    
 (**** end from mergeutil ****)


  let remove_shadow true_rule pred =
    (* Poke a hole in pred so that true_rule is not shadowed. *)
    match true_rule, pred with
    | BTrue _, BTrue _ -> [pred]
    | BTrue tpat, BFalse ppat ->
      (match intersect_patterns tpat ppat with
      | None -> [pred] (* no shadow *)
      | Some ipat ->
        (* ipat is the shadow *)
        [BTrue ipat; pred])
    | BFalse _, _ -> error "[remove_shadow] it is okay to shadow a false rule."
  ;;



  let rec remove_shadows rule pred_rules =
    (* If rule is true, poke holes in any false rules in pred_rules, so that
       rule matches. *)
    match rule with
    | BTrue _ ->
      (match pred_rules with
      | [] -> []
      | [pred_rule] -> remove_shadow rule pred_rule
      | pred_rule :: pred_rules ->
        remove_shadow rule pred_rule @ remove_shadows rule pred_rules)
    | BFalse _ -> pred_rules
  ;;

  let remove_shadow_outer rule_lists =
    let remove_shadow_inner pred_lists rule_list =
      (* find the positive rule in rule list. *)
      let pos_rules =
        CL.filter
          (fun r ->
            match r with
            | BTrue _ -> true
            | _ -> false)
          rule_list
      in
      let pos_rule = CL.hd pos_rules in
      (* poke holes in all the predecessor lists *)
      let deshadowed_pred_lists = CL.map (remove_shadows pos_rule) pred_lists in
      (* return the list up to here. *)
      deshadowed_pred_lists @ [rule_list]
    in
    CL.fold_left remove_shadow_inner [] rule_lists
  ;;

  (***** from rulesolve *****)

let bitstring_to_maskedint (bs : int list) : int*int = 
  let to_val_and_mask bit = 
    match bit with 
    | 0    -> (0, 1) (* val: 0, mask 1 *) 
    | 1    -> (1, 1) (* val: 1, mask 1 *) 
    | -1  -> (0, 0) (* val: 0, mask :0 *)
    | _ -> error "invalid bitstring bit"
  in
  let rec bitstring_to_int bits =  
    match bits with 
      | [] -> 0
      | hd::tl ->
        (Core.Int.shift_left hd (List.length tl)) + (bitstring_to_int tl)
  in      
  let vbits, mbits = CL.map to_val_and_mask bs |> CL.split in 
  bitstring_to_int vbits, bitstring_to_int mbits
;;

open Z3
open Solver
module Z3Bool = Boolean
module Z3Int = Arithmetic.Integer
module Z3Bit = BitVector


let eqn_of_core_pat ctx (m_exp : pattern) =
  let var_bw = 32 in (* for now, use 32-bit vectors throughout *)
  (* one term for each field of the pattern *)
  let fold_f (ctx, terms) (m_exp_entry:(Cid.t * CoreSyntax.pat)) =
    match m_exp_entry with
    | vid,  PNum(z) -> (
      (* output equation: var == const; *)
      let z3_vid = Z3Bit.mk_const_s ctx (Cid.to_string vid) var_bw in 
      let z3_val = Z3Bit.mk_numeral ctx (string_of_int (Z.to_int z)) var_bw in 
      let term = Z3Bool.mk_eq ctx z3_vid z3_val in

      (* let z3_vid = Z3Int.mk_const_s ctx (Cid.to_string vid) in *)
      (* let z3_vint = Z3Int.mk_numeral_i ctx (Integer.to_int vint) in *)
      (* let term = Z3Bool.mk_eq ctx z3_vid z3_vint in *)
      ctx, terms @ [term]
    )
    | vid, PBit bits -> (
      (* output equation: var && mask == const; *)
      let vint, mint = bitstring_to_maskedint bits in 

      let z3_vid = Z3Bit.mk_const_s ctx (Cid.to_string vid) var_bw in 
      let z3_v   = Z3Bit.mk_numeral   ctx (string_of_int vint) var_bw in 
      let z3_m   = Z3Bit.mk_numeral   ctx (string_of_int mint) var_bw in 
      let z3_lhs = Z3Bit.mk_and ctx z3_vid z3_m in 
      let term = Z3Bool.mk_eq ctx z3_lhs z3_v in
      ctx, terms @ [term]
    )
    | vid, PWild -> 
      (* output equation: var && 0 == 0. This says "var may be anything". 
         It is important to add a negatable constraint for checking feasibility 
         of matching rules in sequence. *)
      let z3_vid = Z3Bit.mk_const_s ctx (Cid.to_string vid) var_bw in 
      let z3_v   = Z3Bit.mk_numeral   ctx (string_of_int 0) var_bw in 
      let z3_m   = Z3Bit.mk_numeral   ctx (string_of_int 0) var_bw in 
      let z3_lhs = Z3Bit.mk_and ctx z3_vid z3_m in 
      let term = Z3Bool.mk_eq ctx z3_lhs z3_v in
      (* print_endline ("encoded "^(Cid.to_string vid)^"= wild"); *)
      ctx, terms @ [term]  in
  let ctx, terms = CL.fold_left fold_f (ctx, []) m_exp in
  let eqn = Z3Bool.mk_and ctx terms in
  ctx, eqn
;;

let is_core_pat_still_feasible (pat : pattern) (preds : pattern list) =
(*   print_endline ("[is_core_pat_still_feasible]");
  print_endline ("----");
  print_endline ("pattern:");
  print_endline (string_of_pattern pat);
  print_endline ("previous patterns:");
  print_endline (string_of_patterns preds);
  print_endline ("----"); *)
  let res = match preds with
  | [] -> true
  | _ ->
    let ctx = mk_context ["model", "true"; "proof", "true"] in
    (* encode current rule as equation *)
    let ctx, pat_eqn = eqn_of_core_pat ctx pat in
    (* encode previous rules as equation *)
    let _, pred_eqns = CL.split (CL.map (eqn_of_core_pat ctx) preds) in
    (* the previous rules, overall, are an or *)
    let preds_eqn = Z3Bool.mk_or ctx pred_eqns in
    (* we are interested in something that doesn't match any previous rule *)
    let not_preds_eqn = Z3Bool.mk_not ctx preds_eqn in
    (* but matches the current rule *)
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
  in
  (* print_endline ("RESULT: "^(string_of_bool res)); *)
  res
;;


  let is_rule_matchable pred_rules rule =
    let pat_of_rule r =
      match r with
      | BTrue pat -> pat
      | BFalse pat -> pat
    in
    let pred_pats = CL.map pat_of_rule pred_rules in
    let pat = pat_of_rule rule in
    is_core_pat_still_feasible pat pred_pats
  ;;


  (* delete any rules that cannot ever be matched due to
     shadows from combined predecessors. *)
  let delete_unmatchable_rules rules =
    let fold_if_reachable preds rule =
      match is_rule_matchable preds rule with
      | true -> preds @ [rule]
      | false -> preds
    in
    CL.fold_left fold_if_reachable [] rules
  ;;


  let new_merge keys toplevel_rule_lists =
    (* for each true rule in a rule set, make sure that it is not shadowed by
       and rules in previous rule lists. *)
    let deshadowed_rule_lists = remove_shadow_outer toplevel_rule_lists in
    (* flatten the list of rules *)
    let rules = CL.flatten deshadowed_rule_lists in
    (* add the default rule *)
    let default_false = BFalse (pattern_of_atoms keys []) in
    let full_rules = rules @ [default_false] in
    (* remove any unreachable rules. *)
    delete_unmatchable_rules full_rules
  ;;



    (**** THE PRIZE JEWEL

            convert an if expression into a list of binary rules, 
            where ba_true means the expression evals to true, ba_false means it evals to false
        ****)
    let from_if_core exp =
        (* get the toplevel expressions *)
        let toplevel_exps = get_toplevel_exps exp in
        (* get the keys *)
        let key_vars = get_keys exp in
        (* generate a list of binary rules for each toplevel expression *)
        let toplevel_rule_lists =
          CL.map (binary_rules_from_toplevel_exp key_vars) toplevel_exps
        in
    (* merge the rule lists together, being careful about shadows and reachability *)
        let rule_list = new_merge  key_vars toplevel_rule_lists in
    (* 
      *** the rule list is a list of binary rules***
        BA_true means exp evaluates to true
        BA_false means exp evaluates to false 
    *) 
        rule_list
    ;;

end

let binary_rule_to_branch s1 s2 brule = 
    match brule with
    | FromOldBackend.BTrue(pattern) -> (
        let pats = CL.split pattern |> snd in 
        (pats, s1)
    )
    | FromOldBackend.BFalse(pattern) -> (
        let pats = CL.split pattern |> snd in 
        (pats, s2)
    )
;;


let match_of_if exp s1 s2 =
    (* calculate rules from exp. 
        When these rules are applied sequentially, 
        every BTrue rule is a branch where the exp evaluates to true, 
        and every BFalse rule is a branch where the exp ecaluates to false
     *)
    let rules = FromOldBackend.from_if_core exp in 
(*     print_endline ("rules: ");
    print_endline ("----");
    FromOldBackend.string_of_binary_rules rules |> print_endline;
    print_endline ("----"); *)

    (* get keys *)
    let key_exps = evars_in_exp exp |> ShareMemopInputs.unique_list_of_eq CoreSyntax.equiv_exp in 
    (* construct branches from rules and keys *)
    let branches = CL.map (binary_rule_to_branch s1 s2) rules in 
    let res = SMatch(key_exps, branches) in
    (* print_endline ("RESULT:"); *)
    (* print_endline (CorePrinting.statement_to_string (statement res)); *)
    res
;;

let rec process_old tds = 
    let v = 
        object
            inherit [_] s_map as super
            method! visit_SIf ctx exp s1 s2 = 
            match_of_if exp 
              (super#visit_statement ctx s1) 
              (super#visit_statement ctx s2)
        end
    in
    v#visit_tdecls () tds
;;


(*** new match_of_if that uses MatchAlgebra ***)
(* convert an atom in a if expression into a (var id, pattern) tuple *)
let atom_exp_to_var_pat exp : (Cid.t * pat) =
    match exp.e with
    | EOp (Eq, [evar; eval]) | EOp (Neq, [evar; eval]) ->
      ( name_from_exp evar
      , CoreSyntax.PNum (Z.of_int (int_from_exp eval)))
    | _ -> error "unexpected form of expression to convert into a pattern. "
;;

let atoms_to_pats keys atoms : pat list =
  (* convert a list of atoms into a list of patterns, 
     where any key tested in an atom is converted 
     into an exact match pattern, and any key not 
     tested in an atom is converted into a wildcard pattern *)
  (* note: this function should be used with either: 
     - 1 neq atom of an and expression
     - all the eq atoms of an and expression  *)
    let varid_pat_assoc = CL.map (atom_exp_to_var_pat) atoms in
    let pat_of_key key =
      match CL.assoc_opt key varid_pat_assoc with
      | Some cond -> cond
      | None -> PWild
    in
    CL.map pat_of_key keys
;;

(* 
  A new algorithm for converting an if statement into a match statement. 
  Assumptions: 
    - the if statement's expression is in DNF form, with atomic operations 
      that are either equality or inequality tests of variables against values.
  Approach: 
  The if statement's expression is a disjunction of conjunctions. That means 
  if any conjunction is satisfied, we should take the true branch. So: 
  1) given an SIf(exp, s1, s2), extract a list of conjunctions from exp.
  2) create a match statement for each conjunction. The branches 
     of the match statement are: 
      1) Neq branches: 
        for each Neq rule (x != C) in the conjunction, construct a 
        branch that executes s2 if x == C. 
        These branches handle the case where the conjunction evaluates 
        to false because any one of its Neq atoms evaluates to false. 
      2) Eq branch: 
        for the Eq rules in the conjunction (y == D, ...), construct a 
        single branch that executes s1 if (y = D, ...). This handles 
        the case where the conjunction evaluates to true because: 
          1) all of its Neq atoms evaluate to true (due to Neq branches)
          2) all of its Eq atoms evaluate to true
      3) "miss" branch: 
        finally, construct a branch that executes s2 if none of the 
        previous branches match. This handles the case where all of 
        the Neq atoms evaluate to true, but at least one of the 
        Eq atoms evaluates to false. 
  3) Merge all of the atomic (i.e., per-conjunction) match statements together
     into 1 match statement, using MatchAlgbra from the layout algorithm.
  4) Finally, clean up the branches of the merged match statement. 
     The branches need to be cleaned up because some of them may 
     contain _both_ s1 and s2, if the branch represents a case where 
     one of the atomic tables would branch to s1 and the other would 
     branch to s2. Recall that each atomic match statement represents a 
     conjunction, and all of the atomic tables together represent 
     a disjunction. Because they represent a disjunction, only 
     one atomic match has to "vote" to execute s1. Thus, we 
     resolve branches that contain both s1 and s2 by replacing 
     them with s1.
 *)
let match_of_if_new exp s1 s2 =
  (* placeholders for true and false branches *)
  let true_branch_call =  scall_sp (Cid.create ["if_branch"]) [value_to_exp (vbool true)] (ty TBool) (Span.default) in
  let false_branch_call = scall_sp (Cid.create ["if_branch"]) [value_to_exp (vbool false)] (ty TBool) (Span.default) in
  let s1_orig, s2_orig = s1, s2 in
  let s1 = true_branch_call in
  let s2 = false_branch_call in 
  (* convert the exp into a list of and expressions *)
  let and_expressions = flatten_disjunction exp in
  (* get the keys of the expression *)
  let ekeys = evars_in_exp exp |> MatchAlgebra.unique_list_of_eq (CoreSyntax.equiv_exp) in
  let keys = CL.map CoreSyntax.exp_to_cid ekeys in
  (* now make a match statement for each and expression. *)
  let smatch_from_conjunction and_expression =
    let atom_exps = flatten_conjunction and_expression in
    let neqs = CL.filter (filter_eop_kind Neq) atom_exps in
    let eqs = CL.filter (filter_eop_kind Eq) atom_exps in
    (* get patterns annotated with variable ids *)
    (* a pattern for each negative branch *)
    let inequality_hit_pats = CL.map (fun neq -> atoms_to_pats keys [neq]) neqs in
    (* one pattern for the positive branch *)
    let equality_hit_pat = atoms_to_pats keys eqs in
    (* one pattern for if the positive branch misses *)
    let nothing_hit_pat = atoms_to_pats keys [] in

    let inequality_branches = CL.map
      (fun pats -> (pats, s2))
      inequality_hit_pats
    in
    let equality_branch = equality_hit_pat, s1 in
    let miss_branch = nothing_hit_pat, s2 in
    (* build the match statement *)
    smatch ekeys (inequality_branches@[equality_branch; miss_branch])
  in 
  let atomic_match_statements = CL.map smatch_from_conjunction and_expressions in
  (* now merge all the atomic match statements together *)
  let smatch = match atomic_match_statements with
    | [] -> error "[match_of_if_new] empty expression in if statement?"
    | stmt::[] -> 
      (* smatch_to_conjunction may return an unreachable 
         "false" branch, which then messes up subsequent phases *)
      MatchAlgebra.optimize_smatch stmt
    | stmt::stmts -> 
      (* multiple statements, must fold. *)
      (* let unoptimized =  *)
        CL.fold_left MatchAlgebra.merge_matches stmt stmts
      (* in   *)
      (* let optimized =
        CL.fold_left MatchAlgebra.merge_matches_deleting_redundant stmt stmts
      in
      let _ = Printf.printf "without redundant rules deleted:\n%s\n" (CorePrinting.stmt_to_string unoptimized) in
      let _ = Printf.printf "with redundant rules deleted:\n%s\n" (CorePrinting.statement_to_string optimized) in
      exit 1;
      optimized *)
  in 
  (* finally, we have to replace the true / false placeholders in each branch with the 
     original statements. 
     How we deal with branches that have multiple statements: 
     If a branch contains at least one true_branch instruction, then we branch to s1.
     Why? Each statement in the merged table comes from a single atomic table. And each 
     atomic table represents one component of a disjunction. So, if we are in a branch 
     where there is at least one true_branch, that means at least one table's conditions 
     are satisfied in that branch ==> at least one component of the disjunction is true. *)
  let unify_branch (pats, stmt) = 
    let stmt_list = unfold_stmts stmt in
    let branch_votes = CL.map 
      (fun stmt -> (match stmt.s with
        | SUnit({e=ECall(_, [{e=EVal({v=VBool branch_bool})}])}) -> branch_bool
        | _ -> error "[match_of_if_new] unexpected statement in branch of merged match"))
      stmt_list
    in
    match (CL.exists (fun b -> b) branch_votes) with
    | true -> (pats, s1_orig)
    | false -> (pats, s2_orig)
  in
  (* finally, eliminate the redundant branches.  *)
  let smatch' = match smatch.s with
    | SMatch(exps, branches) -> {smatch with s=SMatch(exps, CL.map unify_branch branches)}
    | _ -> error "[match_of_if_new] constructed a non smatch in the process of merging smatches..."
  in
  MatchAlgebra.delete_redundant_branches smatch'
;;

(* a new algorithm: convert each conjunction of the if expression 
   into a match statement, then merge all the match statements together.  *)
let rec process_new tds =
    let v = 
        object
            inherit [_] s_map as super
            method! visit_SIf ctx exp s1 s2 = 
            (match_of_if_new exp 
              (super#visit_statement ctx s1) 
              (super#visit_statement ctx s2)).s
        end
    in
    v#visit_tdecls () tds
;;

let process tds =
  let result =
    if Cmdline.cfg.old_ifelim then process_old tds else process_new tds
  in
  result
;;


(* does the output program have the right form? *)
let no_ifs_form ds = 
    let v = object 
        inherit [_] s_iter as super
        val mutable pass = true
        method pass = pass          
        method! visit_SIf _ _ _ _ = 
            pass <- false
        end
    in
    v#visit_tdecls () ds;
    v#pass
;;
