(* match statement algebra 
  This needs some refactoring. Mostly copy / pasted from 
  previous backend. Things can be simplified now. *)

open CoreSyntax
module CL = List


(* helpers to put somewhere else! *)
let cid_of_exp (ex : exp) : Cid.t =
  match ex.e with
  | EVar n -> n
  | _ -> error "could not evaluate expression to a cid"
;;
let cons_uniq_eq eq xs x = if Core.List.mem xs x ~equal:eq then xs else x :: xs
let unique_list_of_eq eq xs = List.rev (Caml.List.fold_left (cons_uniq_eq eq) [] xs)
let unique_expr_list = (unique_list_of_eq equiv_exp)
let unique_stmt_list = (unique_list_of_eq equiv_stmt)

(* the case for a single branch *)
(* type pattern = (Cid.t * pat) list  *)
type pattern = (exp * pat) list 
;;

type condition = {
  negs : pattern list; (* if any of these match, the condition doesn't hold. *)
  pos  : pattern option;
}

let equiv_pattern_ele (e1, p1) (e2, p2) =
  ((equiv_exp e1 e2) && (equiv_pat p1 p2))
;;
let equiv_patterns p1 p2 =
  equiv_list equiv_pattern_ele p1 p2
;;
let equiv_conditions c1 c2 =
  (equiv_list equiv_patterns c1.negs c2.negs)
  && (equiv_options equiv_patterns c1.pos c2.pos)
;;

let keys_of_pattern p : exp list =
  CL.split p |> fst
;;

let keys_of_condition c :exp list =
  ((CL.map keys_of_pattern c.negs) |> CL.flatten)
  @(match c.pos with | None -> [] | Some(p) -> keys_of_pattern p) 
  |> unique_expr_list
;;

type rule = 
  | RHit of   { rpat : pattern; } (* ___SOME___ branch hit. *)
  | RMiss of  { rpat : pattern; }

type conditioned_rule = {
  cs : condition list; (* a list of alternatives *) 
  r : rule; 
}

let string_of_pattern p = 
  CL.map 
    (fun (exp, p) -> ((cid_of_exp exp |> Cid.to_string)^"=="^(CorePrinting.pat_to_string p)))
    p
  |> String.concat ","  
;;


let string_of_negs ns = 
  match ns with 
    | [] -> ""
    | _ -> ((CL.map (string_of_pattern) ns) 
          |> String.concat ";\n")^";"
;;
let string_of_pos p = 
  match p with 
    | None -> ""
    | Some p -> (string_of_pattern p)^";"
;; 
let string_of_condition c = 
  match c.negs with 
  | [] ->   Printf.sprintf "HIT{%s}" (string_of_pos c.pos) 
  | _ ->    Printf.sprintf "MISS{%s}\nHIT{%s}" (string_of_negs c.negs) (string_of_pos c.pos)  
;;

let string_of_rule r = 
  match r with 
    | RMiss{rpat=rpat;} -> "RMiss("^(string_of_pattern rpat)^")"
    | RHit{rpat=rpat;} -> "RHit("^(string_of_pattern rpat)^")"

let string_of_conditioned_rule cr =
  "precondition: "^(CL.map string_of_condition cr.cs |> String.concat "\n")
  ^"rule: "^(string_of_rule cr.r)
;;



(***** Z3 helpers *****)
module Z3Helpers = struct 
  open Z3
  open Solver
  module Z3Bool = Boolean
  module Z3Int = Arithmetic.Integer
  module Z3Bit = BitVector

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

  let eqn_of_core_pat ctx (m_exp : pattern) =
    let var_bw = 32 in (* for now, use 32-bit vectors throughout *)
    (* one term for each field of the pattern *)
    let fold_f (ctx, terms) (m_exp_entry:(exp * CoreSyntax.pat)) =
      match m_exp_entry with
      | var_exp,  PNum(z) -> (
        (* output equation: var == const; *)
        let z3_vid = Z3Bit.mk_const_s ctx (cid_of_exp var_exp |> Cid.to_string) var_bw in 
        let z3_val = Z3Bit.mk_numeral ctx (string_of_int (Z.to_int z)) var_bw in 
        let term = Z3Bool.mk_eq ctx z3_vid z3_val in

        (* let z3_vid = Z3Int.mk_const_s ctx (Cid.to_string vid) in *)
        (* let z3_vint = Z3Int.mk_numeral_i ctx (Integer.to_int vint) in *)
        (* let term = Z3Bool.mk_eq ctx z3_vid z3_vint in *)
        ctx, terms @ [term]
      )
      | var_exp, PBit bits -> (
        (* output equation: var && mask == const; *)
        let vint, mint = bitstring_to_maskedint bits in 

        let z3_vid = Z3Bit.mk_const_s ctx (cid_of_exp var_exp |> Cid.to_string) var_bw in 
        let z3_v   = Z3Bit.mk_numeral   ctx (string_of_int vint) var_bw in 
        let z3_m   = Z3Bit.mk_numeral   ctx (string_of_int mint) var_bw in 
        let z3_lhs = Z3Bit.mk_and ctx z3_vid z3_m in 
        let term = Z3Bool.mk_eq ctx z3_lhs z3_v in
        ctx, terms @ [term]
      )
      | var_exp, PWild -> 
        let z3_vid = Z3Bit.mk_const_s ctx (cid_of_exp var_exp |> Cid.to_string) var_bw in 
        let z3_v   = Z3Bit.mk_numeral   ctx (string_of_int 0) var_bw in 
        let z3_m   = Z3Bit.mk_numeral   ctx (string_of_int 0) var_bw in 
        let z3_lhs = Z3Bit.mk_and ctx z3_vid z3_m in 
        let term = Z3Bool.mk_eq ctx z3_lhs z3_v in
        ctx, terms @ [term]
    in
    let ctx, terms = CL.fold_left fold_f (ctx, []) m_exp in
    let eqn = Z3Bool.mk_and ctx terms in
    ctx, eqn
  ;;


  (* generate a z3 equation from a rule *)
  let eqn_of_rule ctx (r : rule) =
    let match_pattern = match r with 
      | RHit{rpat=rpat;}
      | RMiss{rpat=rpat;} -> rpat
    in 
    eqn_of_core_pat ctx match_pattern
  ;;


  (* can p match after qs have all been checked? *)
  let is_pattern_matchable (p:pattern) (qs : pattern list) =  
    let ctx = mk_context ["model", "true"; "proof", "true"] in
    let ctx, p_eqn = eqn_of_core_pat ctx p in 
    let q_eqns = CL.split (CL.map (eqn_of_core_pat ctx) qs) |> snd in 
    let qs_eqn = Z3Bool.mk_or ctx q_eqns in
    (* negate: we do not match any one of the qs *)
    let not_qs_eqn = Z3Bool.mk_not ctx qs_eqn in
    (* (match r) and (do not match any of the qs)  *)
    let intersect_eqn = Z3Bool.mk_and ctx [not_qs_eqn; p_eqn] in
    let solver = Solver.mk_simple_solver ctx in
    Solver.add solver [intersect_eqn];
    let is_sat = Solver.check solver [] in
    match is_sat with
    | SATISFIABLE ->true
    | UNSATISFIABLE -> false
    | UNKNOWN ->
      Printf.printf "unknown\n";
      error "unknown sat..."    
  ;;



  let is_p_subset_of_q (p : pattern) (q:pattern) : bool = 
    (* p is a subset of q if you cannot reach p after q. 
        that is, if !q && p is satisfiable, then p is not a subset of q. *)
    not (is_pattern_matchable p [q])
  ;;
  let patterns_overlap (p : pattern) (q : pattern) : bool = 
    (* do p and q overlap? that is equivalent to asking if p /\ q is satisfiable *)
    let ctx = mk_context ["model", "true"; "proof", "true"] in
    let ctx, p_eqn = eqn_of_core_pat ctx p in 
    let ctx, q_eqn = eqn_of_core_pat ctx q in 
    let and_eqn = Z3Bool.mk_and ctx [p_eqn; q_eqn] in
    let solver = Solver.mk_simple_solver ctx in
    Solver.add solver [and_eqn];
    let is_sat = Solver.check solver [] in
    match is_sat with
    | SATISFIABLE ->true
    | UNSATISFIABLE -> false
    | UNKNOWN ->
      Printf.printf "unknown\n";
      error "unknown sat..."
  ;;

  (* is rule r feasible given ~(qs) *)
  let new_is_r_still_feasible (r : rule) (qs : rule list) =
    let ctx = mk_context ["model", "true"; "proof", "true"] in
    let ctx, r_eqn = eqn_of_rule ctx r in
    let _, q_eqns = CL.split (CL.map (eqn_of_rule ctx) qs) in
    (* match any one of qs *)
    let qs_eqn = Z3Bool.mk_or ctx q_eqns in
    (* negate: we do not match any one of the qs *)
    let not_qs_eqn = Z3Bool.mk_not ctx qs_eqn in
    (* (match r) and (do not match any of the qs)  *)
    let intersect_eqn = Z3Bool.mk_and ctx [not_qs_eqn; r_eqn] in
    let solver = Solver.mk_simple_solver ctx in
    Solver.add solver [intersect_eqn];
    let is_sat = Solver.check solver [] in
    match is_sat with
    | SATISFIABLE ->true
    | UNSATISFIABLE -> false
    | UNKNOWN ->
      Printf.printf "unknown\n";
      error "unknown sat..."
  ;;

  (* is p && q satisfiable? *)
  let p_and_q (p : pattern) (q : pattern) : bool = 
    let ctx = mk_context ["model", "true"; "proof", "true"] in
    let ctx, p_eqn = eqn_of_core_pat ctx p in
    let ctx, q_eqn = eqn_of_core_pat ctx q in 
    let solver = Solver.mk_simple_solver ctx in
    (* assert p && q *)
    Solver.add solver [Z3Bool.mk_and ctx [p_eqn;q_eqn]];
    (* check satisfiability *)
    let is_sat = Solver.check solver [] in
    match is_sat with
    | SATISFIABLE ->true
    | UNSATISFIABLE -> false
    | UNKNOWN ->
      Printf.printf "unknown\n";
      error "unknown sat..."
  ;;
end

(***** end Z3 helpers *****)

(**** general pat merging helpers ****)

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
      let lastbit = match (Core.Int.(land) i 1) with
        | 0 -> 0
        | 1 -> 1
        | _ -> error "[int_to_bits_rev] impossible case: (i && 1) > 1"
      in 
      lastbit::(int_to_bits_rev (Int.shift_right i 1))
  in 
  CL.rev (int_to_bits_rev (Z.to_int i))
;;

let rec pad_to_w w bs = 
  let bits_to_add = w - (List.length bs) in 
  if (bits_to_add <= 0)
  then (bs)
  else (pad_to_w (w) (0::bs))
;;

let exp_lookup_opt exps_assoc exp = 
  (* we have to use e because identical expressions 
     with different spans will mess up the lookup *)
  let es_assoc = CL.map (fun (exp, v) -> exp.e, v) exps_assoc in
  CL.assoc_opt exp.e es_assoc
;;

let e_eq exp1 exp2 = CoreSyntax.equiv_exp exp1 exp2


let unique_exp_list exps = 
  ShareMemopInputs.unique_list_of_eq e_eq exps
;;
let check_key_uniqueness exps =
 let unique_exps = ShareMemopInputs.unique_list_of_eq e_eq exps in
 if (CL.length exps <> CL.length unique_exps)
 then (error ("[check_key_uniqueness] failed in: "^(CorePrinting.es_to_string exps)))
 else ()
;;


let rec replace eq tuples key new_val =
  match tuples with
  | [] -> []
  | (k, v) :: tuples ->
    if eq k key
    then (k, new_val) :: tuples
    else (k, v) :: replace eq tuples key new_val
;;
let exp_replace = (replace CoreSyntax.equiv_exp)


(* merge pat1 into pat2, producing a new 
   pattern that represents the condition 
   pat1 && pat2. If merging the two patterns 
   produces a rule that cannot match anything, 
 return None. *)
let and_patterns (pat1:pattern) (pat2:pattern) : pattern option = 
  let pat1_es = CL.split pat1 |> fst in
  let pat2_es = CL.split pat2 |> fst in
  (* print_endline "(input check)"; *)
  check_key_uniqueness pat1_es;
  check_key_uniqueness pat2_es;
  let merge_into (pat_opt:pattern option) (new_col: exp * pat) : pattern option = 

    let (new_exp, new_cond) = new_col in 
    let res = match pat_opt with 
      | None -> None (* pat has a conflict, so we can't add a new column. *)
      | Some pat -> (
        match (exp_lookup_opt pat new_exp) with 
          (* the column (variable new_exp) is not in the pattern, so add it. *)
          | None ->
(*             print_endline@@"I have divined that the column: "^(CorePrinting.exp_to_string new_exp)^" ";
            print_endline@@"is NOT in the pattern: "^(string_of_pattern pat); *)
            Some ((new_exp, new_cond)::pat)          
          (* the column (variable new_mid) is in the pattern *)
          | Some (pat_cond) -> (
(*             print_endline@@"[merge_into] the column: "^(CorePrinting.exp_to_string new_exp)^" ";
            print_endline@@"IS in the pattern: "^(string_of_pattern pat); *)

            match (pat_cond, new_cond) with
              (* two bitstrings *)
              | PBit pat_bits, PBit new_bits  -> (
                match and_bitstrings pat_bits new_bits with 
                  | Some bs -> 
                    (* bs is the condition on new_mid, so we want to replace 
                       pat_cond with (new_mid, bs) in pat *)
                    (* pat - pat_cond + (new_mid, bs) *)
                    Some (exp_replace pat new_exp (PBit bs))
                  | None -> None
              )
              | PBit pat_bits, PNum c 
              | PNum c, PBit pat_bits -> (
                (* one bitstring and one constant: 
                   convert the constant to a bitstring and do same as 2 bitstrings *)
                let new_bits = int_to_bitstring (c)
                  |> pad_to_w (CL.length pat_bits) 
                in 
                match and_bitstrings pat_bits new_bits with 
                  | Some bs -> 
                    Some (exp_replace pat new_exp (PBit bs))
                  | None -> None
                )
              (* both wildcards, no change *)
              | PWild, PWild -> Some pat 
              (* new is wildcard, no change *)
              | _, PWild -> Some pat
              (* pat is wc, replace with new condition *)
              | PWild, _ -> 
                Some (exp_replace pat new_exp new_cond)
              | PNum(c), PNum(newc) -> (
                (* if the two conditions are exact and 
                   the same value, then there's no change. 
                   else, the resulting rule is invalid *)
                if (c = newc) then (Some pat)
                else (None)
              )
          )
      )
    in
    res
  in 
  let res_all = CL.fold_left merge_into (Some pat2) pat1 in
  (match (res_all) with 
  | Some res -> 
    (* print_endline "(output check)"; *)
    check_key_uniqueness ((CL.split res) |> fst);

    (* print_endline ("[and_patterns] result: "^(string_of_pattern res)); *)
  | _ -> ();
  );
  res_all
  ;;
  let and_is_sat pat1 pat2 = 
    match (and_patterns pat1 pat2) with 
      | None -> false
      | _ -> true  
  ;;


(* get the match variables from a rule *)
let match_vars_of_rule rule =
  match rule with
  | RHit {rpat=pattern;} ->
    let match_vars, _ = CL.split pattern in
    match_vars
  | RMiss{rpat=pattern;} -> 
    let match_vars, _ = CL.split pattern in
    match_vars
  (* error "[match_vars_of_rule] offpath?" *)
;;

let match_vars_of_rules rules =
  rules
  |> CL.fold_left
       (fun cur_mvs rule ->
         match rule with
         | RHit _ -> cur_mvs @ match_vars_of_rule rule
         | RMiss _ -> cur_mvs@ match_vars_of_rule rule)
       []
  |> MiscUtils.unique_list_of
;;

(* extend pat so that it tests all vars. Assumes pat tests a subset of vars. *)
let extend_pat (vars : exp list) (pat : pattern) : pattern =
  let conds =
    CL.map (fun v -> Caml.Option.value (exp_lookup_opt pat v) ~default:PWild) vars
  in
  CL.combine vars conds
;;

let extend_condition_pats vars c =
  {
    negs = CL.map (extend_pat vars) c.negs;
    pos = match c.pos with
      | None -> None
      | Some(p) -> Some(extend_pat vars p);
  }


let normalize_rules rules = 
  let vars = match_vars_of_rules rules in
  CL.map
    (fun r ->
      match r with
      | RHit{rpat=rpat;} -> 
        let new_pat = extend_pat vars rpat in
        RHit{rpat=new_pat;}
      | RMiss{rpat=rpat;} -> 
        let new_pat = extend_pat vars rpat in 
        RMiss{rpat=new_pat;}
    )
    rules
;;



let and_pattern_list (pat1:pattern) (pat2s:pattern list) : pattern list = 
  CL.filter_map (and_patterns pat1) pat2s
;;

(*** from branchElimination.ml ***)

let condition_rule conditions r = 
  {cs = conditions; r = r;}
;;



let new_precondition () = {
  negs = []; pos = None;
}
let wildcard_condition () = 
  {negs = []; pos = Some ([])}


let rule_to_conditon r = 
  {cs = [new_precondition ()] ; 
   r = r}
;;


let delete_implied_negs c = 
  match c.pos with 
    | None -> c
    | Some pat ->
      (* filter *)
      (* let necessary_negs = CL.filter (and_is_sat pat) c.negs in  *)
      let necessary_negs = CL.filter (Z3Helpers.p_and_q pat) c.negs in 
      {c with negs=necessary_negs}
;;

(* get preconditions is not implemented. CFG dependent *)



(* move all the conditions into the precondition(s), 
   leaving the rule as a wildcard. *)
let normalize_conditioned_rule cr = 
(*   print_endline "[normalize_conditioned_rule] input:";
  print_endline (string_of_conditioned_rule cr); *)
  (* transform the clauses in the condition so that 
     each clause c becomes (c && pat). *)
  let refine_condition_with_pat pat condition = 
    let new_negs = and_pattern_list pat condition.negs in     
    let new_pos =  match condition.pos with 
        | Some pos -> 
          let res = and_patterns pat pos in 
          res
        | None -> 
          print_endline ("[normalize_conditioned_rule] input: "^(string_of_conditioned_rule cr));
          error "[refine_condition_with_pat] no positive clause?"
          (* the precondition doesn't have a feasible branch -- so neither does the generated one? *)
    in 
    { negs = new_negs; pos = new_pos; }
  in
  let new_cr = 
    match (cr.r) with 
      | RHit{rpat=rpat;} -> {
        cs = CL.map (refine_condition_with_pat rpat) cr.cs;
        r = RHit{rpat=[];}
        }
      | RMiss{rpat=rpat;} -> {
        cs = CL.map (refine_condition_with_pat rpat) cr.cs;
        r=RMiss{rpat=[];}        
      }
  in 
(*   print_endline "[normalize_conditioned_rule] output:";
  print_endline (string_of_conditioned_rule new_cr); *)
  new_cr
;;

(* convert a normalized conditional rule 
   into a list of rules. The list 
   of rules will have at most 1 hit rule, 
   at the end. 
   for each condition:    
     make a list of offpaths for the negs
     make an onpath for the positive *)
let to_rule_list cr : rule list = 
  let neg_to_rule n = RMiss{rpat=n;} in 
  let pos_to_rule p = 
    match p with 
      | None -> error "[to_rule_list] positive rule cannot be none by this point."
      | Some pat -> (
(*         print_endline "[pos_to_rule] pat: ";
        print_endline (string_of_pattern pat); *)
        match cr.r with
          | RMiss _ -> RMiss{rpat=pat;}
          | RHit _ -> RHit{rpat=pat;}
      )
  in 
  (* the conditions form a disjunction. *)
  let fold_over_conditions produced_rules condition = 
    match condition.pos with 
      | None -> 
        (* print_endline "POS IS NONE"; *)
        produced_rules (* this condition is unsatisfiable, so we produce nothing. *)
      | _ -> 
        let new_rules = 
          (CL.map neg_to_rule condition.negs)
          @[pos_to_rule condition.pos] 
        in 
        let new_rules = new_rules |> normalize_rules in 
        produced_rules@new_rules
  in 
(*   print_endline ("[to_rule_list] input:");
  print_endline (string_of_conditioned_rule cr); *)
  let rules = 
    CL.fold_left fold_over_conditions [] cr.cs
  in
  (* print_endline ("[to_rule_list] output:"); *)
  (* CL.map string_of_rule rules |> (String.concat "\n") |> print_endline; *)
  rules
;;

let delete_unmatchable rules = 
  let add_rule_if_valid valid_rules candidate_rule =
    match (Z3Helpers.new_is_r_still_feasible candidate_rule valid_rules) with 
    (* | _ -> valid_rules@[candidate_rule] *)
    | true -> valid_rules@[candidate_rule]
    | false -> valid_rules
  in 
  CL.fold_left add_rule_if_valid [] rules 
;;


(* condition every rule and generate a new ruleset with the 
   same "actions / cases". In the new ruleset, 
   the actions will only match if something from the 
 preconditions matched, and something from the rule itself *)
 (* If the input is a rule list that comes from a condition, 
 (i.e., with only one match at the end) then the output 
 will be a rule list that can be put into condition 
 form as well. *)

let condition_rules conditions rules = 
  let pipe rule =  
    rule 
    |>  condition_rule conditions 
    |>  normalize_conditioned_rule
    |>  to_rule_list 
        (* get back a list with at most one hit, at the end. 
           In other words, this is itself a condition. *)
  in 
  let new_rules =     CL.map pipe rules |> CL.flatten  in 
(*   print_endline ("[condition_rules] rule list:");
  CL.map string_of_rule new_rules |> (String.concat "\n") |> print_endline; *)

  let new_rules = 
    new_rules
    (* |> CL.flatten  *)
    (* some cleanup *)
    |> normalize_rules
    |> delete_unmatchable
  in 
  new_rules
;;

(* produce a condition that 
   is equivalent to c1 && c2 *)
let and_conditions c1 c2 = 
(*   print_endline ("[and_conditions]");
  print_endline ("c1:"^(string_of_condition c1));
  print_endline ("c2:"^(string_of_condition c2)); *)
  let c2_rules = 
    (CL.map (fun neg -> RMiss{rpat=neg;}) c2.negs)
    @(match c2.pos with None -> [] | Some pos -> [RHit{rpat=pos;}])
  in 
(*   print_endline@@"c2_rules";
  print_endline
    (CL.map string_of_rule c2_rules |> String.concat "\n"); *)
  let c1_c2_rules = condition_rules [c1] c2_rules in 
(*   print_endline@@"c1_c2_rules";
  print_endline
    (CL.map string_of_rule c1_c2_rules |> String.concat "\n"); *)

  let c1_c2 =
    let fold_rule_into_condition c r = 
      match r with
      | RHit{rpat=rpat;} -> (
        match c.pos with
          | Some _ -> error "[fold_rule_into_condition] trying to fold a positive rule into a condition that already has a positive."
          | None -> {c with pos=Some(rpat);}
      )
      | RMiss{rpat=rpat;} -> 
        {c with negs=c.negs@[rpat];}
    in 
    let res = CL.fold_left 
      fold_rule_into_condition 
      {negs=[]; pos=None;}
      c1_c2_rules
    in
    res
  in 
  (* print_endline("c1_c2:"^(string_of_condition c1_c2)); *)
  c1_c2  
;;


(*** use match algebra to merge match statements ***)

type pattern_branch = {
  pattern : (exp * pat) list;
  bstmt    : statement;
}

type pattern_branches = pattern_branch list ;;

let pattern_branches_of_match m: pattern_branches = 
  match m.s with 
  | SMatch(es, bs) -> (
    let pattern_branch_of_branch (pats, stmt) =
      { pattern = List.combine es pats;
        bstmt = stmt; }
    in 
    List.map pattern_branch_of_branch bs
  )
  | SNoop -> []
  | _ -> error "[pattern_branches_of_match] not a match statement (or noop)"
;;

let keys_of_pattern_branches pbs =
  let foo = CL.map (fun pb -> pb.pattern |> CL.split |> fst) pbs
  |> CL.flatten 
  in
  unique_exp_list foo
;;

let cases_of_pattern_branches pbs =  
  CL.map 
    (fun pb -> 
      (CL.split pb.pattern |> snd, pb.bstmt)
    )
    pbs
;;

(* match statement of a pattern branches list. *)
let smatch_of_pattern_branches pbs =
  smatch 
    (keys_of_pattern_branches pbs)
    (cases_of_pattern_branches pbs)
;;


let can_match_after = Z3Helpers.is_pattern_matchable
;;


(* compute the cross product branch 
   of (b1, b2) and add it into bs 
    The cross product branch is simply: 
      b1 && b2 --> (s1; s2)
      b1 --> s1
      b2 --> s2
    Each of these branches is only added if it is 
    matchable after all rules added before it.*) 
(* find the rules that are the intersection of b1 and b2, if any. *)
let cross_product bs (b1, b2) = 
  (* b1 and b2 may not have the same fields.*)
  let patterns_of_bs bs = List.map (fun b -> b.pattern) bs in 
  let intersect_pattern = and_patterns b1.pattern b2.pattern in 
  (* add the intersection branch after previous intersect branches, if its 
     possible for the intersection branch to match after them. *)
  match intersect_pattern with 
    | Some (pattern) -> (
      match (can_match_after pattern (patterns_of_bs bs)) with 
      | true -> 
        let bstmt = sseq_sp b1.bstmt b2.bstmt Span.default in
        bs@[{pattern; bstmt;}]
      | false -> bs
    )
    | None -> 
      (* the intersection between the two statements cannot 
         match anything.  *)
      (* print_endline ("[cross_product] no intersection pattern."); *)
      bs
;;

let align_keys pb1 pb2 =
  let keys = keys_of_pattern_branches (pb1@pb2) in 
  CL.map 
    (fun pb -> {pb with pattern=extend_pat keys pb.pattern;})
    pb1
  ,CL.map 
    (fun pb -> {pb with pattern=extend_pat keys pb.pattern;})
    pb2

;;


(* delete the pattern branches that cannot be matched in a list *)
let delete_unreachable bs =
  let patterns_of_bs bs = List.map (fun b -> b.pattern) bs in 
  let res = List.fold_left
    (fun bs b -> 
      if (can_match_after b.pattern (patterns_of_bs bs))
      then (bs@[b])
      else (bs)
    )
    []
    bs
  in 
  res
;;


    (* ***deleting redundant branches ***
    we have a statement like this: 
      match (a, b) with 
      | (_, 0) -> x();
      | (_, _) -> x();
  
    the first rule is not needed, because: 
      1) it is a subset of the second rule;
      2) it has the same action as the second rule;
      3) there are no intersecting rules with different actions between the two rules
    
    we want to detect and remove rules like the first rule.
  *)

  
  let is_pattern_branch_redundant (pb :pattern_branch) (pbs : pattern_branch list) =
  (* for each pattern_branch q in pbs:
      # if pb is a subset of q, and pb has the same action as q, then pb is redundant
      if (is_subset(pb, q)):
        if (pb.action == q.action):
          return true
      # if pb and q overlap, and pb has a different action than q, then pb is necessary
      if (intersect(pb, q) != empty):
        if (pb.action != q.action):
          return false *)
    let is_redundant_opt = CL.fold_left
      (fun result_opt q -> (
        match result_opt with 
        | Some(_) -> result_opt
        | None -> (
(*           print_endline ("[is_pattern_branch_redundant] branch:\n" ^ (string_of_pattern pb.pattern));
          print_endline ("[is_pattern_branch_redundant] successor branch:\n" ^ (string_of_pattern q.pattern));
 *)
        (* if pb is a subset of q, and pb has the same stmt as q, then pb is redundant *)
          if (Z3Helpers.is_p_subset_of_q pb.pattern q.pattern) then (
            (* print_endline("[is_pattern_branch_redundant] p is a subset of successor."); *)
            if (CoreSyntax.equiv_stmt pb.bstmt q.bstmt) then (
              (* print_endline("[is_pattern_branch_redundant] p is redundant becuase of successor."); *)
              Some(true)
            (* case: pb is a subset of later rule q, but q has a different statement... *)
            ) else (
                (* print_endline("[is_pattern_branch_redundant] p is necessary because of successor."); *)
                Some(false)
            )
          ) else (
            (* if pb and q overlap, and pb has a different stmt than q, then pb is necessary *)
            if (Z3Helpers.patterns_overlap pb.pattern q.pattern) then (
              (* print_endline("[is_pattern_branch_redundant] p and successor overlap."); *)
              if (not (CoreSyntax.equiv_stmt pb.bstmt q.bstmt)) then (
                (* print_endline("[is_pattern_branch_redundant] p is necessary because of successor."); *)
                Some(false)
              ) else (
                None
              )
            ) 
            (* case: pb and q do not overlap, so we are not yet sure if pb is necessary or redundant. *)
            else (
              None
            )
          )
        )
      )
      )
      None
      pbs
    in
    (* if we did not find any rules in pbs that makes pb redundant, then pb is necessary.*)
    match is_redundant_opt with
    | Some(b) -> b
    | None -> false
  ;;

(* delete the pattern branches that are not redundant, because 
   anything that matches them would match a subsequent branch 
   with the same statement. *)
let delete_redundant bs = 
  let rec delete_redundant_rec bs = 
    match bs with 
    | [] -> []
    | b::bs -> (
      if (is_pattern_branch_redundant b bs) then (
        delete_redundant_rec bs
      ) else (
        b::(delete_redundant_rec bs)
      )
    )
  in 
  delete_redundant_rec bs
;;

let combine_pattern_branches bs1 bs2 = 
  (* first, make sure bs1 and bs2 are over the same fields. *)
  let bs1, bs2 = align_keys bs1 bs2 in 
  match (bs1, bs2) with 
    | [], [] -> []
    | _, [] -> bs1
    | [], _ -> bs2
    | _, _ -> 
      let m2_integrate bs b2 =     
        let m1_integrate bs b1 = 
          cross_product bs (b1, b2)
        in 
        List.fold_left m1_integrate bs bs1
      in 
      let intersect_rules = List.fold_left m2_integrate [] bs2 in 
      (* the original rules must come after _all_ the intersect rules, 
         else they may block some from matching. Also, some rules 
         may not be reachable anymore. *)
      delete_unreachable (intersect_rules@bs1@bs2)
;;

(* combine two match statements into a pattern branch *)
let combine_matches m1 m2 = 
  let bs1 = pattern_branches_of_match m1 in
  let bs2 = pattern_branches_of_match m2 in 
  combine_pattern_branches bs1 bs2
;;

let merge_matches m1 m2 = 
  combine_pattern_branches
    (pattern_branches_of_match m1)
    (pattern_branches_of_match m2)
  |> smatch_of_pattern_branches
;;

(* delete the redundant branches in a match statement *)
let delete_redundant_branches m = 
  (* print_endline ("[delete_redundant_branches] starting statement"^(CorePrinting.statement_to_string m)); *)
  let res = pattern_branches_of_match m
    |> delete_redundant
    |> smatch_of_pattern_branches
  in
  (* print_endline ("[delete_redundant_branches] final statement"^(CorePrinting.statement_to_string res)); *)

  res
;;

(* update pattern branch list bs1, folding in the match statement m2 *)
let fold_match_into_pattern_branches bs1 m2 =
  let bs2 = pattern_branches_of_match m2 in 
  combine_pattern_branches bs1 bs2
;;



