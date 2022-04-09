(* eliminate branch nodes from a control flow graph representation of a DPA program *)
open LLSyntax
open MiscUtils
open DFSyntax
module CL = Caml.List
module MU = MergeUtils
module RS = RuleSolve
open Printf
open DebugPrint

(* move this to Cid.ml? *)
module CidTbl = struct 
  type t = Cid.t
  let compare = Cid.compare
end

exception Error of string

let error s = raise (Error s)
(* logging *)
module DBG = BackendLogging
let outc = ref None
let dprint_endline = ref DBG.no_printf
let start_logging () = DBG.start_mlog __FILE__ outc dprint_endline

let log_rules rules =
  CL.iter (fun r -> DBG.printf outc "%s" (dbgstr_of_rule r)) rules
;;

(* let log_prog cid_decls = DBG.printf outc "----\n%s\n----\n" (str_of_prog cid_decls) ;; *)

let num_keys_of_tid tbl_id cid_decls = 
  let tbl = Cid.lookup cid_decls tbl_id in 
  let keys = keys_of_table tbl in 
  CL.length keys 
;;

let table_stats cid_decls tbl_id = 
  sprintf "table %s with %i keys and %i rules" 
  (Cid.to_string tbl_id)
  (num_keys_of_tid tbl_id cid_decls)
  (CL.length (rules_of_table (Cid.lookup cid_decls tbl_id)))
;;


(**** helpers for eliminating control-flow branches ****)

let delete_unmatchable rules = 
  let add_rule_if_valid valid_rules candidate_rule =
    match (RS.new_is_r_still_feasible candidate_rule valid_rules) with 
    (* | _ -> valid_rules@[candidate_rule] *)
    | true -> valid_rules@[candidate_rule]
    | false -> valid_rules
  in 
  CL.fold_left add_rule_if_valid [] rules 
(*   let new_rules = match CL.length rules with 
    | 0 | 1 -> rules 
    | _ -> CL.fold_left add_rule_if_valid [] rules 
  in 
  new_rules *)
;;

let delete_unmatchable_test () = 
  print_endline ("testing delete unmatchable");
  let a = Cid.create ["a"] in 
  let b = Cid.create ["b"] in   
  let pa = (a, Exact((Integer.of_int 1))) in 
  let pb = (b, Exact((Integer.of_int 2))) in 
  let pat = [pa; pb] in 
  let raid = Cid.create ["rulea"] in 
  let rbid = Cid.create ["ruleb"] in 
  let aaid = Cid.create ["acna"] in 
  let baid = Cid.create ["acnb"] in 
  let ra = Match(raid, pat, aaid) in 
  let rb = Match(rbid, pat, baid) in 

  let all_rules = [ra; rb] in 
  let print_rules rs = 
    print_endline ("-----");
    CL.iter (fun r -> dbgstr_of_rule r |> print_endline) rs;
    print_endline ("-----")
  in 
  print_rules all_rules;
  let matchable_rules = delete_unmatchable all_rules in 
  print_rules matchable_rules;

  exit 1;
;;

(* delete_unmatchable_test ();; *)


let normalize_rules rules = 
  let vars = match_vars_of_rules rules in
  CL.map
    (fun r ->
      match r with
      | Match (r_id, pat, a_id) -> Match (r_id, MU.extend_pat vars pat, a_id)
      | OffPath pat -> OffPath (MU.extend_pat vars pat))
    rules
;;

(* merge pat1 into pat2, producing a new 
   pattern that represents the condition 
   pat1 && pat2. If merging the two patterns 
   produces an inconsistent rule, return 
   None. *)
let and_patterns pat1 pat2 = 
  let merge_into (pat_opt:pattern option) (new_col: mid * condition) : pattern option = 
    let (new_mid, new_cond) = new_col in 
    match pat_opt with 
      | None -> None (* pat has a conflict, so we can't add a new column. *)
      | Some pat -> (
        match (Cid.lookup_opt pat new_mid) with 
          (* the column is not in the pattern, so add it. *)
          | None ->
            Some ((new_mid, new_cond)::pat)          
          (* the column is in the pattern *)
          | Some (pat_cond) -> (
            match (pat_cond, new_cond) with
              (* both wildcards, no change *)
              | Any, Any -> Some pat 
              (* new is wildcard, no change *)
              | _, Any -> Some pat
              (* pat is wc, use new *)
              | Any, _ -> 
                Some (Cid.replace pat new_mid new_cond)
              | Exact(c), Exact(newc) -> (
                (* if the two conditions are exact and 
                   the same value, then there's no change. 
                   else, the resulting rule is invalid *)
                if (c = newc) then (Some pat)
                else (None)
              )
          )
      )
  in 
  CL.fold_left merge_into (Some pat2) pat1
;;

let and_pattern_list (pat1:pattern) (pat2s:pattern list) : pattern list = 
  CL.filter_map (and_patterns pat1) pat2s
;;


(**** path constraint based pass ****)

(* The core of this transformation pass operates on 
  "conditioned rules". A conditioned rule is a rule 
  that only applies when one of its conditions 
  is satisfied. A condition is a list of negative 
  patterns (patterns that must not match) followed 
  by a positive pattern (a pattern that must match). 
  If pos is None, it means the condition cannot be 
  satisfied. This can happen when augmenting a condition 
  with the match guard of a rule, during 
  the normalization step. *)

module PathConstraints = BatMap.Make(CidTbl)

type condition = {
  negs : pattern list;
  pos  : pattern option;
}
let new_precondition () = {
  negs = []; pos = None;
}
let wildcard_condition () = 
  {negs = []; pos = Some ([])}


type conditioned_rule = {
  cs : condition list; (* a list of alternatives *) 
  r : rule; 
}
let rule_to_conditon r = 
  {cs = [new_precondition ()] ; 
   r = r}
;;

let condition_rule conditions r = 
  {cs = conditions; r = r;}
;;

let dbgstr_of_negs ns = 
  match ns with 
    | [] -> "<no negative clauses>"
    | _ -> (CL.map dbgstr_of_pat ns) 
          |> (CL.map (fun s -> "NOT "^s^";"))
          |> String.concat "\n"
;;
let dbgstr_of_pos p = 
  match p with 
    | None -> "<no positive clause>;"
    | Some p -> (dbgstr_of_pat p)^";"
;; 
let dbgstr_of_condition c = 
  sprintf "[\n\t%s;\n\t%s\n]" (dbgstr_of_negs c.negs) (dbgstr_of_pos c.pos)  
;;
let dbgstr_of_conditions cs = 
  "number of conditions: "
  ^(CL.length cs |> string_of_int)^"\n"
  ^(CL.map dbgstr_of_condition cs |> String.concat "\n")
;;

let dbgstr_of_conditioned_rule cr = 
   "******conditioned rule ********\n"
  ^"*****conditions*****\n"
  ^(dbgstr_of_conditions cr.cs)
  ^"\n*****rule*****\n"
  ^(dbgstr_of_rule cr.r)
  ^"\n******end conditioned rule ********"
;;


(* find and delete all the negative clauses that are 
implied by the positive clause and thus not needed. 

input: 
r1: neg: (x = 1;);
r2: pos: (x = 3;);
output: 
delete neg, because neg and pos is false (there is no intersection)

input: 
r1: neg: (x = 1;);
r2: pos: (x = _;);
output: 
keep neg, because neg and pos is true (there is some intersection)

input:
r1: neg: (x = _;);
r2: pos: (x = 1;);
output: 
keep neg. Safe to delete pos as an optimization.

*)
let delete_implied_negs c = 
  match c.pos with 
    | None -> c
    | Some pat ->
      (* filter *)
      let necessary_negs = CL.filter (RS.p_and_q pat) c.negs in 
      {c with negs=necessary_negs}
;;


(* get the conditions between tid's precedessors and tid. 
   At least one of these conditions must apply for 
   any rule in table tid to execute. *)
let get_preconditions cid_decls (pred_tids:oid list) (tid:oid) : condition list = 
  (* does pred_rule point to tid? *)
  let points_to_tid cid_decls pred_rule tid = 
    match pred_rule with 
    | Match _ -> (
      let acn_id = Option.get (new_aid_of_rule pred_rule) in 
      let acn = Cid.lookup cid_decls acn_id in 
      let acn = match acn with 
        | Action(acn) -> acn
        | _ -> error "not an acn"
      in 
      let (_, _, next_tids) = acn in 
      contains next_tids tid
    )
    | _ -> false
  in
  let fold_pred_tbl pre_conditions pred_tid = 
    (* one rule in the table. *)
    let fold_pred_rule (pre_conditions, current_precond) pred_rule = 
      match (points_to_tid cid_decls pred_rule tid) with 
      | true -> 
        (* this branch points to the table,
           it is the positive precondition 
           that ends a single precondition. *)
        let current_precond = 
          {current_precond with pos = Some (rule_pattern (pred_rule))} 
          |> delete_implied_negs
        in 
        !dprint_endline "[get_preconditions] adding precondition:";
        !dprint_endline (dbgstr_of_condition current_precond);
        (pre_conditions@[current_precond], new_precondition ())
      | false -> 
        (* this branch does not point to the table, 
           it is another negative precondition 
           on the precondition currently being built. *)
        let current_precond = {current_precond with negs = current_precond.negs@[rule_pattern (pred_rule)]} in 
        (pre_conditions, current_precond) 
    in 
   let pred_rules = rules_of_table (Cid.lookup cid_decls pred_tid) in 
   let pre_conditions, _ = CL.fold_left 
      fold_pred_rule 
      (pre_conditions, new_precondition ()) 
      pred_rules
    in 
    pre_conditions
  in 
  CL.fold_left fold_pred_tbl [] pred_tids 
;;

(* move all the conditions into the precondition(s), 
   leaving the rule as a wildcard. *)
let normalize_conditioned_rule cr = 
  (* add pat to condition *)
  !dprint_endline "[normalize_conditioned_rule] conditions BEFORE:";
  !dprint_endline (dbgstr_of_conditions cr.cs);
  (* transform the clauses in the condition so that 
     each clause c becomes (c && pat). *)
  let refine_condition_with_pat pat condition = 
    let new_negs = and_pattern_list pat condition.negs in     
    let new_pos =  match condition.pos with 
        | Some pos -> 
          !dprint_endline ("[refine_condition_with_pat] anding two patterns: ");
          !dprint_endline ("[refine_condition_with_pat] pos: ");          
          !dprint_endline (dbgstr_of_pat pos);
          !dprint_endline ("[refine_condition_with_pat] pat: ");          
          !dprint_endline (dbgstr_of_pat pat);
          let res = and_patterns pat pos in 
          (match res with
          | None -> !dprint_endline ("RESULT OF AND IS NONE!");
          | Some res -> !dprint_endline ("Result: "); !dprint_endline (dbgstr_of_pat res);
          );
          res
          (*
            sometimes when we run and_patterns here, we 
            get an infeasible pattern. That is legitimate behavior. 
            We might have code like this, for example. 
            if (x == 1){
              if (x == 2) {
                foo();
              } else {
                bar();
              }
            }
            Right now, we represent infeasible code paths as 
            rules with no positive clause. 
          *)
        | None -> error "[refine_condition_with_pat] no positive clause?"
           (* 
        if there is no positive clause, 
        then there may be now... *)

    in 
    !dprint_endline "[refine_condition_with_pat] original pos";
    !dprint_endline (dbgstr_of_pos condition.pos);
    !dprint_endline "[refine_condition_with_pat] refined pos";
    !dprint_endline (dbgstr_of_pos new_pos);
    { negs = new_negs; pos = new_pos; }
  in
  let new_cr = 
    match (cr.r) with 
      | Match(cid, pat, oid) -> {
        (* pat is the rule's pattern. 
          We want to change the conditioned rule so that 
          pat is a wildcard. To do this, refine_condition_with_pat 
          transforms every clause in cr.cs from c --> c && pat. 
        *)
        cs = CL.map (refine_condition_with_pat pat) cr.cs;
        r = new_rule cid [] oid; (* a wildcard rule *)
      }
      | OffPath(pat) -> {
        cs = CL.map (refine_condition_with_pat pat) cr.cs;
        r = Generators.noop_rule;
      }
  in 
  !dprint_endline "[normalize_conditioned_rule] conditions AFTER:";
  !dprint_endline (dbgstr_of_conditions new_cr.cs);
  new_cr

;;

(* convert a normalized conditional rule 
   into a list of rules. 
   for each condition:    
     make a list of offpaths for the negs
     make an onpath for the positive *)
let to_rule_list cr : rule list = 
  (* TODO: if the rule is unsatisfiable, return an empty list. 
     one way to represent them is for unsatisfiable rules to have 
     no positive clause. *)
  let neg_to_rule n = OffPath(n) in 
  let pos_to_rule p = 
    match p with 
      | None -> !dprint_endline "[to_rule_list] ERROR."; error "[to_rule_list] positive rule cannot be none by this point."
      | Some pat -> (
        match cr.r with
          | OffPath _ -> OffPath(pat)
          | Match(rid, _, aid) -> Match(rid, pat, aid)
      )
  in 
  (* the conditions form a disjunction. *)
  let fold_over_conditions produced_rules condition = 
    !dprint_endline "[to_rule_list] PROCESSING CONDITON: ";
    !dprint_endline (dbgstr_of_condition condition);
    !dprint_endline "\n---------";
    match condition.pos with 
      | None -> produced_rules (* this condition is unsatisfiable, so we produce nothing. *)
      | _ -> 
        let new_rules = 
          (CL.map neg_to_rule condition.negs)
          @[pos_to_rule condition.pos] 
        in 
        let new_rules = new_rules |> normalize_rules in 
        produced_rules@new_rules
  in 

  CL.fold_left fold_over_conditions [] cr.cs
;;


let print_rules rules = 
  CL.iter
    (fun r -> print_endline ((dbgstr_of_rule r)))
    rules  
;;
let print_conditioned_rules crs = 
  CL.iter
    (fun cr -> dbgstr_of_conditioned_rule cr |> print_endline)
    crs
;;
(* condition every rule so that the new ruleset is the 
   cross product of conditions x rules *)
let condition_rules conditions rules = 
  (* for debugging, try conditioning just one rule at a time. *)
(*   print_endline ("---- [condition_rules] input rules ----");
  print_rules rules;
  print_endline ("---- [condition_rules] end input rules ----"); *)
  (* let print_rule_pipe m r = print_endline m; print_rules [r]; r in  *)
  (* let print_conditioned_rule m r = print_endline m; print_conditioned_rules [r]; print_newline (); r in  *)
  let pipe rule =  
    rule 
        (* print_rule_pipe "**** input rule: " rule  *)
    |>  condition_rule conditions 
    (* |>  simplify_preconditions *)
    (* |>  print_conditioned_rule "**** conditioned rule: " *)
    |>  normalize_conditioned_rule
    (* |>  print_conditioned_rule "**** normalized rule: " *)
    |>  to_rule_list 
  in 
  let new_rules = CL.map pipe rules |> CL.flatten in 
(*   print_endline ("[condition_rules] new rules: ");
  CL.iter (fun r -> r |> dbgstr_of_rule |> print_endline) new_rules; *)
  new_rules
;;

let cid_to_string = P4tPrint.str_of_private_oid ;;

let condition_table cid_decls conditions tid = 
  let tbl = Cid.lookup cid_decls tid in 
  !dprint_endline ("\n*** [condition_table] STARTING TABLE: "^(cid_to_string tid));
  !dprint_endline ("****original table****");
  !dprint_endline (str_of_decl tbl);
  !dprint_endline ("\n****preconditions****");
  dbgstr_of_conditions conditions |> !dprint_endline;
  let new_table, noop_acn = match tbl with 
    | Table(tid, rules, something) -> 
      let new_rules = rules
        |> condition_rules conditions (* apply condition to get cross product *)
        |> normalize_rules (* normalize columns *)
        |> delete_unmatchable 
        (* delete rules that cannot possibly be matched *)
      in 
      (* generate the wildcard noop rule *)
      let noop_rule, noop_acn = Generators.concrete_noop [] in 
      let new_rules = new_rules@[noop_rule] in 
(*        print_endline ("******** [condition_table] ********");
      print_endline ("table: "^(cid_to_string tid));
      print_endline (str_of_rules new_rules);
      print_endline ("******** [condition_table] ********");
      print_string (sprintf "... %i rules in new table ... " (CL.length new_rules)); *)

      let new_table = Table (tid, new_rules, something) in 
      new_table, noop_acn
    | _ -> error "[condition_table] not a table."
  in 
  !dprint_endline ("\n***[condition_table] summary for "^(cid_to_string tid));
  !dprint_endline ("****original table****");
  !dprint_endline (str_of_decl tbl);
  !dprint_endline ("\n****preconditions****");
  dbgstr_of_conditions conditions |> !dprint_endline;
  !dprint_endline ("\n****conditioned table**** ");
  !dprint_endline (str_of_decl new_table);
  !dprint_endline ("\n*** [condition_table] END TABLE: "^(cid_to_string tid));
  let cid_decls = Cid.replace cid_decls tid new_table in 
  cid_decls@(dict_of_decls [noop_acn])

(* make OffPath's concrete *)
let concretize_offpaths cid_decls = 
  let fold_rules (new_rules, new_acns) r = 
    match r with 
      | OffPath(pat) -> 
        (* create new rule and action *)
        let r, acn = Generators.concrete_noop pat in 

        new_rules@[r], new_acns@[acn]
      | _ -> new_rules@[r], new_acns
  in 
  let fold_decls processed_cid_decls (oid, decl) = 
    match decl with 
      | Table(tid, rules, something) -> 
        let rules, new_actions = CL.fold_left fold_rules ([], []) rules in 
        let updated_table = Table(tid, rules, something) in 
        processed_cid_decls
        @(dict_of_decls new_actions) 
        @[(oid, updated_table)] 
      | _ -> processed_cid_decls@[(oid, decl)]
  in 
  CL.fold_left fold_decls [] cid_decls
;;

(* 
  input:  tid -- a table (id of a table in cid_decls)
          pred_tids -- the predecessors of tid
  description:        
  transform table tid so that each action "a" only 
  executes when one of the rules that points to a 
  matches not only the rule's conditions, but also 
  one of the conditions from a predecessor action. 
  Also, concretize all the offpaths that may be created.
*)
let enforce_path_constraints_at_table idom tid (cid_decls, pcs) = 
  match is_tbl cid_decls tid with 
  | true -> (
    let pred_tids = pred_tids_of_tid cid_decls tid in 
    match (CL.length pred_tids) with  
    | 0 -> 
      (* this is the root node, there are no preconditions. *)
      let preconditions = [wildcard_condition ()] in 
      let pcs = PathConstraints.add tid preconditions pcs in 
      cid_decls, pcs 
    | 1 -> (
      (* this is a sequence or branch node, preconditions are 
         the same as the predecessor. *)
      let preconditions = get_preconditions 
        cid_decls 
        pred_tids
        tid
      in 
      (* print_string (sprintf "... %i preconditions ... " (CL.length preconditions)); *)
      let new_cid_decls = condition_table 
        cid_decls 
        preconditions 
        tid 
      in 
      (* print_endline ("before concretization for "^(Cid.to_string tid)); *)
      (* let cid_decls_tbls = CL.filter (fun (_, dec) -> is_table dec) new_cid_decls in  *)
      (* print_endline (str_of_cid_decls cid_decls_tbls); *)
      let pcs = PathConstraints.add tid preconditions pcs in 
      let res = concretize_offpaths new_cid_decls in 
      (* print_endline ("after concretization for "^(Cid.to_string tid)); *)
      (* let cid_decls_tbls = CL.filter (fun (_, dec) -> is_table dec) res in  *)
      (* print_endline (str_of_cid_decls cid_decls_tbls); *)
      res, pcs 
    )
    | _ -> (
      (* this is a join node, preconditions are the same as 
         the immediate dominator. 
         Special-casing join nodes is really important 
         because it prevents most conditional tests from 
         propagating through the entire program. It solves 
         the "diamond control flow" problem. *)
(*       let preconditions = get_preconditions 
        cid_decls 
        pred_tids
        tid
      in 
 *)
      let preconditions = PathConstraints.find (idom tid) pcs in 
      let new_cid_decls = condition_table 
        cid_decls 
        preconditions 
        tid 
      in 
      let pcs = PathConstraints.add tid preconditions pcs in 
      let res = concretize_offpaths new_cid_decls in     
      res, pcs 
    )
  )
  | false -> cid_decls, pcs
;;

(**** helpers for deleting no-op tables ****)

(* replace tid with its successor tables in acn_id. acn_id is a predecessor action of tid.*)
let replace_tid_in_pred tbl_id cid_decls pred_aid =
  let succ_tids_of_tbl = succs_of_tid cid_decls tbl_id in
  (* can you be a successor to yourself? oh my. *)
  DBG.printf
    outc
    "[remove_noop_tbl] tbl_id = %s; predecessor of tbl_id = %s \n"
    (P4tPrint.str_of_private_oid tbl_id)
    (P4tPrint.str_of_private_oid pred_aid);
  DBG.printf
    outc
    "[remove_noop_tbl] successors of tbl_id = [%s]\n"
    (CL.map P4tPrint.str_of_private_oid succ_tids_of_tbl |> String.concat ", ");
  let aid, oids, succ_tids_of_pred =
    match Cid.lookup cid_decls pred_aid with
    | Action (aid, oids, succ_tids) -> aid, oids, succ_tids
    | _ -> error "replace_tid_in_pred: pred_aid not an action"
  in
  let fold_replace_tbl_id tids tid =
    match Cid.equals tid tbl_id with
    | false -> tids @ [tid]
    | true -> tids @ succ_tids_of_tbl
  in
  let new_succ_tids_of_pred =
    CL.fold_left fold_replace_tbl_id [] succ_tids_of_pred
  in
  let new_acn = Action (aid, oids, new_succ_tids_of_pred) in
  Cid.replace cid_decls pred_aid new_acn
;;

(* remove a table whose actions are all empty *)
let remove_noop_tbl cid_decls tbl_id =
  (*  
    in the objects: 
      tbl.pred.successor_tbls += tbl.actions.successor_tbls
      tbl.pred.successor_tbls -= tbl
  *)
  DBG.printf
    outc
    "[remove_noop_tbl] tbl_id = %s\n"
    (P4tPrint.str_of_private_oid tbl_id);
  let pred_aids = pred_aids_of_tid cid_decls tbl_id in
  DBG.printf
    outc
    "[remove_noop_tbl] predecessor actions = [%s]\n"
    (CL.map P4tPrint.str_of_private_oid pred_aids |> String.concat ", ");
  let cid_decls =
    CL.fold_left (replace_tid_in_pred tbl_id) cid_decls pred_aids
  in
  (* remove all of tbl_id's actions from cid_decls *)
  let cid_decls =
    CL.fold_left Cid.remove cid_decls (aids_of_tid cid_decls tbl_id)
  in
  (* remove tbl_id from cid_decls *)
  let cid_decls = Cid.remove cid_decls tbl_id in
  cid_decls
;;

let delete_if_noop tbl_id cid_decls =
  match CL.length (oids_of_tid cid_decls tbl_id) with
  | 0 ->
    !dprint_endline
      (sprintf
         "[delete_if_noop] deleting noop table %s!"
         (Cid.to_string tbl_id));
    remove_noop_tbl cid_decls tbl_id (* table has no actions, so delete *)
  | _ -> cid_decls
;;

let visit_node_noop_delete node_id cid_decls =
  DBG.printf
    outc
    "[visit_node_noop_delete] node_id = %s\n"
    (P4tPrint.str_of_private_oid node_id);
  let cid_decls =
    match Cid.exists cid_decls node_id with
    | true ->
      (match is_tbl cid_decls node_id with
      | false -> cid_decls (* not a table? nothing changes. *)
      | true ->
        delete_if_noop node_id cid_decls
        (* a table and its actions might get deleted. *))
    | false -> cid_decls
    (* object doesn't exist? nothing changes.*)
  in
  cid_decls
;;

(* replace branch nodes with tables that test all constraints on their control flow *)
(* basic 2-pass algorithm to eliminate branch nodes:
pass 1: update each table node so that it checks the constraints of its predecessors in the graph. 
pass 2: delete each table node whose actions all do nothing. *)
let eliminate_branch_nodes cid_decls g root_tid =
  (*  We must traverse the dag topologically -- 
		before you can operate on a node, 
		you must operate on all the node's predecessors. *)
  (* compute dominator tree *)
  let idom = Dom.compute_idom g root_tid in 
  let pcs = PathConstraints.empty in 
  DBG.printf
    outc
    "[eliminate_branch_nodes] adding full constraints to all tables \n";
  LLValidate.validate_cid_decls cid_decls "[BranchElimination.eliminate_branch_nodes (start)]";
  !dprint_endline "----cid decls before eliminating control branches ----";
  !dprint_endline (DebugPrint.str_of_cid_decls cid_decls);
  !dprint_endline "----end cid decls before eliminating control branches ----";
  tids_of_declmap cid_decls 
    |> CL.length 
    |> sprintf "Computing execution constraints for every primitive operation table. There are %i tables. Progress: "
    |> print_string
    |> Format.print_flush;

  let cid_decls, _ = Topo.fold (enforce_path_constraints_at_table idom) g (cid_decls, pcs) in
  print_endline " done.";
  !dprint_endline "----cid decls after eliminating control branches ----";
  !dprint_endline (DebugPrint.str_of_cid_decls cid_decls);
  !dprint_endline "----end cid decls after eliminating control branches ----";
  LLValidate.validate_cid_decls
    cid_decls
    "[BranchElimination.eliminate_branch_nodes (end)]";
  DBG.printf outc "[eliminate_branch_nodes] removing noop tables \n";
  let cid_decls = Topo.fold visit_node_noop_delete g cid_decls in
  (* let new_cid_decls, new_g = Topo.fold visit_node g (cid_decls, g) in  *)
  (* rebuild the graph based on new_cid_decls *)
  DBG.printf outc "[eliminate_branch_nodes] rebuilding graph...\n";
  let new_g = DFSyntax.graph_of_declsMap cid_decls in
  cid_decls, new_g
;;

let do_passes df_prog =
  let cid_decls, root_tid, g = DFSyntax.to_tuple df_prog in
  LLValidate.validate_cid_decls cid_decls "[BranchElimination.do_passes (start)]";
  print_endline ("----starting BranchElimination pass----");
  (* log_prog cid_decls; *)
  let new_cid_decls, g = eliminate_branch_nodes cid_decls g root_tid in
  (* log_prog new_cid_decls; *)
  let new_prog = new_cid_decls, root_tid, g in
  DBG.printf outc "[BranchElimination.do_passes (end)] validating output...\n";
  LLValidate.validate_cid_decls cid_decls "BranchElimination.do_passes (end)]";
  (* log_tbl_g_and_ir new_prog "nobranch_table_call"; *)
  (* log_tbl_dot_and_prog new_prog "nobranch_table_call"; *)
  DBG.printf outc "[BranchElimination.do_passes (end)] DONE.\n";
  DFSyntax.from_tuple new_prog df_prog
;;
