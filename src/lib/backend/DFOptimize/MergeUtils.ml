(* helpers for merging tables *)
[@@@ocaml.warning "-17-8-27"]

open Format
open LLSyntax
open MiscUtils
open DFSyntax
module RS = RuleSolve
open DebugPrint
open MiscUtils

(* logging *)
module DBG = BackendLogging
let outc = ref None
let dprint_endline = ref DBG.no_printf
let start_logging () = DBG.start_mlog __FILE__ outc dprint_endline

let log_rules rules =
  CL.iter (fun r -> DBG.printf outc "%s\n" (dbgstr_of_rule r)) rules
;;

let cid_to_string = P4tPrint.str_of_private_oid ;;

let oids_and_next_tids_of_rule cid_decls r =
  match r with
  | Match (_, _, acn_id) ->
    let acn = Cid.lookup cid_decls acn_id in
    let (Action (_, oids, next_tids)) = acn in
    oids, next_tids
  | OffPath _ -> [], []
;;

let is_offpath r =
  match r with
  | Match _ -> false
  | OffPath _ -> true
;;

(* extend a pattern so that it has a column for every var in vars *)
let extend_pat (vars : mid list) (pat : pattern) : pattern =
  let conds =
    CL.map (fun v -> Caml.Option.value (Cid.lookup_opt pat v) ~default:Any) vars
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

(* make sure that all rules in table t have the same columns in the same order *)
let normalize_table_patterns t =
  let (Table (t_id, rules, stg_opt)) = t in
  let vars = match_vars_of_rules rules in
  let rules =
    CL.map
      (fun r ->
        match r with
        | Match (r_id, pat, a_id) -> Match (r_id, extend_pat vars pat, a_id)
        | OffPath pat -> OffPath (extend_pat vars pat))
      rules
  in
  Table (t_id, rules, stg_opt)
;;

(* make sure two tables both have the same columns *)
let normalize_table_pair s t =
  let (Table (s_id, s_rules, s_stg_opt)) = s in
  let (Table (t_id, t_rules, t_stg_opt)) = t in
  let vars = match_vars_of_rules (s_rules @ t_rules) in
  let s_rules =
    CL.map
      (fun r ->
        match r with
        | Match (r_id, pat, a_id) -> Match (r_id, extend_pat vars pat, a_id)
        | OffPath pat -> OffPath (extend_pat vars pat))
      s_rules
  in
  let t_rules =
    CL.map
      (fun r ->
        match r with
        | Match (r_id, pat, a_id) -> Match (r_id, extend_pat vars pat, a_id)
        | OffPath pat -> OffPath (extend_pat vars pat))
      t_rules
  in
  Table (s_id, s_rules, s_stg_opt), Table (t_id, t_rules, t_stg_opt)
;;

(**** intersection of patterns ****)
(* find the intersection of two conditions (columns) in a match pattern, if it exists *)
let intersect_conditions (a_cond : condition) (b_cond : condition)
    : condition option
  =
  match a_cond with
  | Any -> Some b_cond (* a is wildcard, just b*)
  | Exact a_const ->
    (match b_cond with
    | Any -> Some a_cond (* b is wildcard, return a*)
    | Exact b_const ->
      if (* a and b are constants, if they're the same, return either. 
         if they're not the same, there is no intersection in this dimension. *)
         a_const = b_const
      then Some a_cond
      else None)
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
    CL.map (fun (a_cond, b_cond) -> intersect_conditions a_cond b_cond) ab_conds
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
  | true -> Some (CL.combine vars (CL.map Option.get ab_conds))
  (* *)
  | false -> None
;;

(* generate the rule that is the intersection of 
  s_rule : s and t_rule : t, assuming that s and t are parallel tables *)
(* if both rules are offpath, the intersection is offpath. 
     if one rule is on path, the intersection is onpath *)
let gen_intersect_rule s_rule t_rule union_aid =
  let s_pat = pat_of_rule s_rule in
  let t_pat = pat_of_rule t_rule in
  let st_pat = Option.get (intersect_patterns s_pat t_pat) in
  DBG.printf outc "\t\tintersect rule pat: %s\n" (dbgstr_of_pat st_pat);
  let st_rid = Cid.fresh ["r"] in
  let intersect_rule =
    match union_aid with
    | None -> OffPath st_pat (* none means that the intersect rule is offpath*)
    | Some union_aid -> Match (st_rid, st_pat, union_aid)
  in
  intersect_rule
;;

(* for sequential intersection, we know that s_rule is on path 
  (to pass st feasibility check)
  but t_rule may or may not be on path. If its not, 
    we need to add an offpath rule. 
*)

type union_acn_request =
  | Parallel of oid option * rule * rule
  | Sequential of oid option * rule * rule

(* generate a reminder to add a new union action *)

let par_gen_union_acn_request_custom name_fcn s_rule t_rule =
  let s_aid = aid_of_rule s_rule in
  let t_aid = aid_of_rule t_rule in
  match is_offpath s_rule with
  | true ->
    (match is_offpath t_rule with
    (* both offpath -- there is no union rule. *)
    | true -> None, Parallel (None, s_rule, t_rule)
    (* s is offpath -- the union rule is t, which already exists, so there's no request to create *)
    | false -> Some t_aid, Parallel (None, s_rule, t_rule))
  | false ->
    (match is_offpath t_rule with
    (* t is offpath -- the union rule is s, which already exists *)
    | true -> Some s_aid, Parallel (None, s_rule, t_rule)
    (* both are onpath. The union rule is the union of s and t and it must be created. *)
    | false ->
      let new_aid = name_fcn s_aid t_aid in
      Some new_aid, Parallel (Some new_aid, s_rule, t_rule))
;;

let par_gen_union_acn_request = par_gen_union_acn_request_custom Cid.concat

(* for sequential merging, either rule being offpath means 
that the whole s, t sequence is offpath. *)
let seq_gen_union_acn_request_custom name_fcn s_rule t_rule =
  let s_aid = aid_of_rule s_rule in
  let t_aid = aid_of_rule t_rule in
  let new_aid = name_fcn s_aid t_aid in
  (* if either rules are offpath, the intersect rule is off path and there's no union action *)
  match is_offpath s_rule || is_offpath t_rule with
  | true -> None, Sequential (None, s_rule, t_rule)
  | false -> Some new_aid, Sequential (Some new_aid, s_rule, t_rule)
;;

let seq_gen_union_acn_request = seq_gen_union_acn_request_custom Cid.concat

(* generate a union action for parallel merging from two rules (s and t).
  the union action calls all the objects and next tables from either action. 
  assumption: at least one of s and t is on path. if not, there should never be a request to make a union rule *)
let gen_union_acn t_tid cid_decls new_acn_request =
  let new_acn_id, s_rule, t_rule =
    match new_acn_request with
    | Parallel (new_acn_id, s_rule, t_rule) -> new_acn_id, s_rule, t_rule
    | Sequential (new_acn_id, s_rule, t_rule) -> new_acn_id, s_rule, t_rule
  in
  match new_acn_id with
  | None -> cid_decls
  | Some new_acn_id ->
    (match Cid.exists cid_decls new_acn_id with
    | true -> cid_decls
    | false ->
      if is_offpath s_rule && is_offpath t_rule
      then
        error
          "Trying to generate a union action from two offpath rules. This \
           should not happen. ";
      let s_aid = aid_of_rule s_rule in
      let t_aid = aid_of_rule t_rule in
      let s_oids, s_next_tids = oids_and_next_tids_of_rule cid_decls s_rule in
      let t_oids, t_next_tids = oids_and_next_tids_of_rule cid_decls t_rule in
      let next_tids =
        match new_acn_request with
        | Parallel _ -> s_next_tids @ t_next_tids
        | Sequential _ -> t_next_tids
      in
      (* make sure that none of the actions point to t -- this can occur in a parallel merge. *)
      let next_tids =
        CL.filter (fun next_tid -> not (Cid.equals t_tid next_tid)) next_tids
      in
      DBG.printf
        outc
        "[gen_union_acn] new_acn_id=%s s_aid=%s t_aid=%s\n"
        (P4tPrint.str_of_private_oid new_acn_id)
        (P4tPrint.str_of_private_oid s_aid)
        (P4tPrint.str_of_private_oid t_aid);
      let new_acn =
        Action (new_acn_id, unique_list_of (s_oids @ t_oids), next_tids)
      in
      (new_acn_id, new_acn) :: cid_decls)
;;

(************ parallel merging *************)

(* 
new merging algorithm:

start with the rules in s
one at a time, merge in rules from t

for each rule in t:
  for each rule in s:
    - if (s is an added rule): do nothing
    - if (s^t is not feasible): do nothing
    - if (s^t is feasible and s^t covers s): replace s with s^t, note s^t as an added rule
    - if (s^t is feasible and s^t does not cover s): add s^t before s, note s^t as an added rule
  - if t is feasible given the current rules in s, add it to the end 

for sequential merging, we need two changes: 
  1. the last step doesn't happen -- t can only get executed through a rule from s
  2. the feasibility check changes -- s must have table(t) as one of its successor tables

*)

(* merge a rule from t with a rule from s, produce 0, 1, or 2 rules *)
let merge_t_into_s
    gen_union_acn_request
    st_feas_checker
    t_rule
    (new_action_requests, t_derived_s_rules, new_s_rules_so_far)
    s_rule
  =
  DBG.printf
    outc
    "[merge_t_into_s] attempting to merge\n\
     [merge_t_into_s] s_rule: %s\n\
     [merge_t_into_s] t_rule: %s \n"
    (dbgstr_of_rule s_rule)
    (dbgstr_of_rule t_rule);
  (* if s_rule is derived from t, we can't merge t_rule into s_rule *)
  let s_is_t_derived = CL.mem s_rule t_derived_s_rules in
  let more_new_action_requests, more_t_derived_s_rules, more_new_s_rules =
    match s_is_t_derived with
    | true ->
      [], [], [s_rule]
      (* s is derived from a higher priority rule in t (which is mutually exclusive with r_rule) *)
    | false ->
      let st_feas = st_feas_checker s_rule t_rule in
      (* s and t for parallel tables, s then t for sequential tables *)
      (match st_feas with
      | false -> [], [], [s_rule] (* nothing can match both s and t *)
      | true ->
        (* at this point, we are definitely adding an intersect rule (st) -- but we may not keep s. *)
        let union_aid, gen_acn_req = gen_union_acn_request s_rule t_rule in
        let intersect_rule = gen_intersect_rule s_rule t_rule union_aid in
        let s_is_reachable_after_s_t =
          RS.is_r_still_feasible s_rule [s_rule; t_rule]
        in
        (* QUESTION: do we also need to check whether s is feasible with respect to other new_s_rules_so_far? *)
        (match s_is_reachable_after_s_t with
        | false ->
          (* s_and_t covers s, so we want to add only s_and_t, which is derived from t. *)
          DBG.printf
            outc
            "\t[merge_t_into_s]deleting: %s\n"
            (dbgstr_of_rule s_rule);
          DBG.printf
            outc
            "\t[merge_t_into_s]adding: %s\n"
            (dbgstr_of_rule intersect_rule);
          [gen_acn_req], [intersect_rule], [intersect_rule]
        | true ->
          (* s_and_t does not cover s, so we want to add s_and_t, then s. *)
          DBG.printf
            outc
            "\t[merge_t_into_s]adding: %s\n"
            (dbgstr_of_rule intersect_rule);
          DBG.printf
            outc
            "\t[merge_t_into_s]adding: %s\n"
            (dbgstr_of_rule s_rule);
          [gen_acn_req], [intersect_rule], [intersect_rule; s_rule]))
  in
  DBG.printf outc "\n[merge_t_into_s] ---- new_s_rules ----\n";
  log_rules more_new_s_rules;
  DBG.printf outc "\n[merge_t_into_s] ---- end new_s_rules ----\n\n";
  let out_tup =
    ( new_action_requests @ more_new_action_requests
    , t_derived_s_rules @ more_t_derived_s_rules
    , new_s_rules_so_far @ more_new_s_rules )
  in
  out_tup
;;

(* merge a rule from t into all the rules in s *)
let merge_t_into_all_s
    gen_union_acn_request
    st_feas_checker
    t_only_feas_checker
    (new_action_requests, t_derived_s_rules, s_rules)
    t_rule
  =
  (* merge t into each s rule *)
  DBG.printf outc "\n[merge_t_into_all_s] START\n";
  DBG.printf outc "\tt_rule:\n%s\n\ts_rules:\n" (dbgstr_of_rule t_rule);
  log_rules s_rules;
  DBG.printf outc "\n--------\n";
  let new_action_requests, updated_t_derived_s_rules, updated_s_rules =
    CL.fold_left
      (merge_t_into_s gen_union_acn_request st_feas_checker t_rule)
      (new_action_requests, t_derived_s_rules, [])
      s_rules
  in
  (* if we can reach t without going through a rule in s, we need to add t to the end of the new rule set. *)
  let updated_t_derived_s_rules, updated_s_rules =
    match t_only_feas_checker updated_s_rules t_rule with
    | true -> updated_t_derived_s_rules @ [t_rule], updated_s_rules @ [t_rule]
    | false -> updated_t_derived_s_rules, updated_s_rules
  in
  DBG.printf
    outc
    "\n[merge_t_into_all_s]\n\tt_rule:\n%s\n\tUPDATED s_rules:\n"
    (dbgstr_of_rule t_rule);
  log_rules updated_s_rules;
  DBG.printf outc "\n--------\n";
  (* filter out any unreachable rules *)
  let num_rules = CL.length updated_s_rules in
  let updated_s_rules = unique_list_of updated_s_rules in
  let updated_s_rules =
    CL.filter (RS.is_reachable_in_order updated_s_rules) updated_s_rules
  in
  let num_reachable_rules = CL.length updated_s_rules in
  DBG.printf
    outc
    "[merge_t_into_all_s] filtered out %i unreachable rules\n"
    (num_rules - num_reachable_rules);
  DBG.printf outc "[merge_t_into_all_s] reachable rules: \n";
  log_rules updated_s_rules;
  DBG.printf outc "\n[merge_t_into_all_s] END\n";
  new_action_requests, updated_t_derived_s_rules, updated_s_rules
;;

(* merge each rule from t rules into the list of rules s_rules 
return the final rules and any newly generated actions *)
let merge_all_t_into_all_s
    gen_union_acn_request
    st_feas_checker
    t_only_feas_checker
    s_rules
    t_rules
  =
  let new_action_requests, all_t_derived_s_rules, final_s_rules =
    CL.fold_left
      (merge_t_into_all_s
         gen_union_acn_request
         st_feas_checker
         t_only_feas_checker)
      ([], [], s_rules)
      t_rules
  in
  let _ = all_t_derived_s_rules in
  (* don't need to return this *)
  final_s_rules, new_action_requests
;;

(* merge function that can implement either parallel or sequential table merging, 
   based on checker functions passed in *)
let merge_tables
    gen_union_acn_request
    check_st_feas
    check_t_only_feas
    cid_decls
    s_tid
    t_tid
  =
  !dprint_endline "---- [merge tables] ----";
  !dprint_endline
    (DebugPrint.str_of_decls
       [Cid.lookup cid_decls s_tid; Cid.lookup cid_decls t_tid]);
  !dprint_endline "---- [merge tables] ----";
  let [s; t] = CL.map (Cid.lookup cid_decls) [s_tid; t_tid] in
  let (Table (s_id, s_rules, s_stage)) = s in
  let (Table (t_id, t_rules, t_stage)) = t in
  (* normalize the patterns of both tables -- will this help the invalid merging of table groups? *)
  let s, t = normalize_table_pair s t in
  let cid_decls = Cid.replace cid_decls s_id s in
  let cid_decls = Cid.replace cid_decls t_id t in
  let orig_t_aids = aids_of_tid cid_decls t_tid in
  let orig_s_aids = aids_of_tid cid_decls s_tid in
  (* find s_actions -- need to know them for a sequential merge *)

  (* log_prog cid_decls; *)
  let s_acns_map = acn_map_of_tid cid_decls s_id in
  (* merge and generate new actions *)
  let merged_s_rules, new_action_requests =
    (merge_all_t_into_all_s
       gen_union_acn_request
       (check_st_feas s_acns_map t)
       check_t_only_feas)
      s_rules
      t_rules
  in
  (* generate new actions and add them to cid decls*)
  let cid_decls =
    CL.fold_left
      (gen_union_acn t_id)
      cid_decls
      (unique_list_of new_action_requests)
  in
  (* generate s with new rules *)
  let new_s = Table (s_id, merged_s_rules, s_stage) in
  DBG.printf
    outc
    "[merge_tables] finished merging into table %s -- new table has %i rules\n"
    (P4tPrint.str_of_private_oid s_id)
    (CL.length merged_s_rules);
  log_rules merged_s_rules;
  (* normalize patterns in s *)
  let new_s = normalize_table_patterns new_s in
  (* update s in cid_decls *)
  let cid_decls = Cid.replace cid_decls s_id new_s in
  (* remove t from cid_decls *)
  let cid_decls = Cid.remove cid_decls t_id in
  (* delete any actions from t that are no longer used *)
  let new_st_aids = aids_of_tid cid_decls s_id in
  (* let used_in_st = CL.filter (fun orig_aid -> CL.mem orig_aid new_st_aids) (orig_t_aids@orig_s_aids) in  *)
  let unused_in_st =
    CL.filter
      (fun orig_aid -> not (CL.mem orig_aid new_st_aids))
      (orig_t_aids @ orig_s_aids)
  in
  (*[packetin~0, merged_acn~1] *)
  let cid_decls =
    CL.fold_left
      (fun cid_decls unused_aid -> Cid.remove cid_decls unused_aid)
      cid_decls
      unused_in_st
  in
  (* log_prog cid_decls; *)
  (* return updated cid_decls *)
  cid_decls
;;

(**** feasibility checkers that differ depending on whether its a parallel or sequential merge ****)
(* can you execute an action in t without first executing an action in s? *)
(* for a parallel table, you can get to the t_rule if ![st_rules] and t_rule is true *)
let par_t_only_feas st_rules t_rule = RS.is_r_still_feasible t_rule st_rules

(* for a sequential table, its impossible because some s rule must direct you to t.*)
let seq_t_only_feas st_rules t_rule =
  let _, _ = st_rules, t_rule in
  false
;;

(* is it possible to execute s_rule and t_rule? *)
let par_st_feas s_acns_map t s_rule t_rule =
  let _, _ = s_acns_map, t in
  RS.is_intersection_feasible s_rule t_rule
;;

(* QUESTION: do we need to check the higher priority rules in t, for the intersect feasibility? *)

(* 
  is it possible to execute s_rule, then execute t_rule? 
  if s_rule is off path, then it is not possible. 
*)
let seq_st_feas s_acns_map t s_rule t_rule =
  match s_rule with
  | OffPath _ ->
    false (* if s is offpath, then there's no way we can ever get to t. *)
  | Match (_, _, s_acn_id) ->
    DBG.printf
      outc
      "seq_st_feas: s_acn_id = %s\n"
      (P4tPrint.str_of_private_oid s_acn_id);
    let tids_called_from_s_rule = succs_of_aid s_acns_map s_acn_id in
    (match RS.is_intersection_feasible s_rule t_rule with
    (* the intersection must be feasible *)
    | false -> false
    | true ->
      (* the action called by s_rule must have table t as a successor. *)
      CL.mem (tid_of_tbl t) tids_called_from_s_rule)
;;

(**** main parallel and sequential merge functions ****)

(* merge two parallel tables, return updated cid_decls with new s_tid *)
let parallel_merge (cid_decls : declsMap) s_tid t_tid =
  DBG.printf
    outc
    "---PARALLEL MERGE: tables %s and %s---\n"
    (P4tPrint.str_of_private_oid s_tid)
    (P4tPrint.str_of_private_oid t_tid);
  merge_tables
    par_gen_union_acn_request
    par_st_feas
    par_t_only_feas
    cid_decls
    s_tid
    t_tid
;;

(* merge two sequential tables, return updated cid_decls with new s_tid *)
let sequential_merge (cid_decls : declsMap) s_tid t_tid =
  DBG.printf
    outc
    "---SEQUENTIAL MERGE: tables %s and %s---\n"
    (P4tPrint.str_of_private_oid s_tid)
    (P4tPrint.str_of_private_oid t_tid);
  merge_tables
    seq_gen_union_acn_request
    seq_st_feas
    seq_t_only_feas
    cid_decls
    s_tid
    t_tid
;;

(**** sequential condition propagation ****)

(* copy a rule in s, to build the table s'. When we do merge s' t, we want a table with the actions of t, but the path constraints of st.
  If the rule's action doesn't go to table t, replace the rule with an offpath. 
    - this means that the rule can never execute in the new t, because s will never goto t if these constraints are satisfied. 
    - if slicing was done correctly, the rule should already _be_ an offpath in this case.
  If the rule's action does go to t, replace the rule with an empty action 
  (because in the merged table we only want to execute the instructions from t).
*)
let null_action_copy_of_rule cid_decls t_id r =
  match r with
  | Match (r_id, pat, acn_id) ->
    let succ_tids = succs_of_aid cid_decls acn_id in
    (match CL.mem t_id succ_tids with
    | true ->
      (* the rule goes to t -- this is a viable execution that should call nothing from table s.*)
      let new_acn_id = Cid.concat (Cid.fresh [""]) acn_id in
      (* let new_acn_id = Cid.concat acn_id (Cid.fresh ["copy"]) in  *)
      let new_acn = Action (new_acn_id, [], succs_of_aid cid_decls acn_id) in
      let new_r_id = Cid.concat (Cid.fresh [""]) r_id in
      (* let new_r_id = Cid.concat r_id (Cid.fresh ["copy"]) in  *)
      let new_rule = Match (new_r_id, pat, new_acn_id) in
      new_rule, [new_acn]
    | false ->
      (* the rule does not goes to t -- this is not a viable execution. executing this rule in the constrained table t should be impossible. *)
      let new_rule = OffPath pat in
      new_rule, [])
  | OffPath _ -> r, []
;;

let add_acn_to_map cid_decls acn =
  match acn with
  | Action (aid, oids, tids) -> cid_decls @ [aid, acn]
  | _ -> error "not an action"
;;

(* make a copy of the table s_tid, with all actions that go to t set to no-op, and all actions that 
don't go to t set to offpaths. *)
let null_action_copy_of_s_tid cid_decls s_tid t_tid =
  let s = Cid.lookup cid_decls s_tid in
  match s with
  | Table (tbl_id, rules, stage_opt) ->
    (* generate copies of all rules and the new null actions  *)
    let new_rules, new_null_actions_lists =
      CL.split (CL.map (null_action_copy_of_rule cid_decls t_tid) rules)
    in
    let new_null_actions = CL.flatten new_null_actions_lists in
    (* add new rules to cid_decls *)
    let cid_decls = CL.fold_left add_acn_to_map cid_decls new_null_actions in
    (* generate the null table copy *)
    let new_tbl_id = Cid.concat tbl_id (Cid.fresh ["noop_pred"]) in
    let new_tbl = Table (new_tbl_id, new_rules, stage_opt) in
    (* add the table to cid_decls *)
    let cid_decls = cid_decls @ [new_tbl_id, new_tbl] in
    (* return the no-op table id and the updated cid_decls *)
    cid_decls, new_tbl_id
  | _ ->
    error
      (sprintf "couldn't find table: %s\n" (P4tPrint.str_of_private_oid s_tid))
;;

(* change the name of a table to new_tid and update the entry for new_tid in the datastruct *)
let rename_and_update cid_decls new_tid tbl =
  (* set the name of new_tbl = new_tid *)
  let old_tid = tid_of_tbl tbl in
  let renamed_tbl = rename_tbl new_tid tbl in
  let new_cid_decls = Cid.remove cid_decls old_tid in
  let new_cid_decls = new_cid_decls @ [new_tid, renamed_tbl] in
  new_cid_decls
;;

let propagate_condition (cid_decls : declsMap) s_tid t_tid =
  DBG.printf
    outc
    "[propagate conditions] table %s to table %s---\n"
    (P4tPrint.str_of_private_oid s_tid)
    (P4tPrint.str_of_private_oid t_tid);
  (* make a copy of the data structure, with table s_tid having actions replaced by null actions *)
  let noop_s_cid_decls, noop_s_tid =
    null_action_copy_of_s_tid cid_decls s_tid t_tid
  in
  (* merge the tables into st_tid *)
  let noop_s_cid_decls = sequential_merge noop_s_cid_decls noop_s_tid t_tid in
  (* get the st table *)
  let noop_st = Cid.lookup noop_s_cid_decls noop_s_tid in
  (* remove the st' table, add the t table back in. The s table should still be there, because we only operated on a copy. *)
  let updated_cid_decls = rename_and_update noop_s_cid_decls t_tid noop_st in
  (* return the new data structure *)
  updated_cid_decls
;;

type propagation_type =
  | AllMustMatch (* table t executes if all of its conditions hold. *)
  | OneMustMatch

(* table t executes if any of its conditions hold. *)

let propagate_condition_generic prop_type cid_decls s_tid t_tid =
  LLValidate.validate_cid_decls cid_decls "[propagate_condition_generic] start";
  (* make a copy of the data structure, with table s_tid having actions replaced by null actions *)
  let noop_s_cid_decls, noop_s_tid =
    null_action_copy_of_s_tid cid_decls s_tid t_tid
  in
  DBG.printf
    outc
    "[propagate_condition_generic] table %s (noop copy: %s) to table %s\n"
    (P4tPrint.str_of_private_oid s_tid)
    (P4tPrint.str_of_private_oid noop_s_tid)
    (P4tPrint.str_of_private_oid t_tid);
  (* merge the tables into st_tid *)
  let merge_name_fn _ b = b in
  (* let merge_name_fn = Cid.concat in  *)
  let noop_s_cid_decls =
    match prop_type with
    | AllMustMatch ->
      DBG.printf outc "[propagate_condition_generic] AllMustMatch\n";
      (*sequential merge, with different union acton name function*)
      merge_tables
        (seq_gen_union_acn_request_custom merge_name_fn)
        seq_st_feas
        seq_t_only_feas
        noop_s_cid_decls
        noop_s_tid
        t_tid
    | OneMustMatch ->
      DBG.printf outc "[propagate_condition_generic] OneMustMatch\n";
      (*parallel merge, with different union action name function*)
      merge_tables
        (par_gen_union_acn_request_custom merge_name_fn)
        par_st_feas
        par_t_only_feas
        noop_s_cid_decls
        noop_s_tid
        t_tid
  in
  (* get the st table *)
  let noop_st = Cid.lookup noop_s_cid_decls noop_s_tid in
  (* remove the st' table, add the t table back in. The s table should still be there, because we only operated on a copy. *)
  let updated_cid_decls = rename_and_update noop_s_cid_decls t_tid noop_st in
  (* return the new data structure *)
  LLValidate.validate_cid_decls
    updated_cid_decls
    "[propagate_condition_generic] end";
  updated_cid_decls
;;

let preds_tid_ctr = ref 0

let next () =
  preds_tid_ctr := !preds_tid_ctr + 1;
  !preds_tid_ctr
;;

let fresh_preds_tbl cid_decls : oid * declsMap =
  let merged_id = next () in
  let tbl_id = Cid.create_ids [Id.to_id ("merged_preds_tbl", merged_id)] in
  let acn_id = Cid.create_ids [Id.to_id ("merged_preds_acn", merged_id)] in
  let acn = Action (acn_id, [], []) in
  (* a rule to do nothing for any packet.*)
  let def_rule = Match (Cid.fresh ["r"], [], acn_id) in
  let tbl = Table (tbl_id, [def_rule], None) in
  tbl_id, cid_decls @ [acn_id, acn; tbl_id, tbl]
;;

(* 
propagate all the conditions from a set of predecessors (s_tid) to 
a table (t_tid). The new t_tid's rules should enforce all the conditions 
that must be met for it to execute, given its predecessors s_tids.
*)
let merge_pred_conditions cid_decls s_tids t_tid =
  let orig_cid_decls = cid_decls in
  (* 1. make copies of s_tids, offpath the rules that don't lead to t_tid *)
  let fold_f t_tid (cid_decls, new_s_tids) s_tid =
    let cid_decls, new_s_tid =
      null_action_copy_of_s_tid cid_decls s_tid t_tid
    in
    cid_decls, new_s_tids @ [new_s_tid]
  in
  let cid_decls, s_copy_tids =
    CL.fold_left (fold_f t_tid) (cid_decls, []) s_tids
  in
  (* 2. parallel merge all the s_tids, to get a table with rules encoding all the ways control can flow 
  from any one of s_tids to t_tid *)
  let merge_pred_pair cid_decls a_tid b_tid =
    merge_tables
      (par_gen_union_acn_request_custom (fun _ b -> b))
      par_st_feas
      par_t_only_feas
      cid_decls
      a_tid
      b_tid
  in
  (* merge copies of s_tids into merged_pred_tid *)
  let merged_preds_tid, cid_decls = fresh_preds_tbl cid_decls in
  let fold_f merged_tid cid_decls next_pred =
    merge_pred_pair cid_decls merged_tid next_pred
  in
  let cid_decls =
    CL.fold_left (fold_f merged_preds_tid) cid_decls s_copy_tids
  in
  (* 3. propagate conditions from merged_preds_tid to t_tid *)
  let cid_decls =
    propagate_condition_generic AllMustMatch cid_decls merged_preds_tid t_tid
  in
  (* 4. get the new t_tid *)
  let new_t_tbl = Cid.lookup cid_decls t_tid in
  (* 5. replace t_tid in the original cid decls and return *)
  Cid.replace orig_cid_decls t_tid new_t_tbl
;;

(* test runs *)
let run_test test_name case_generator merge_fcn =
  DBG.printf outc "---------testing %s ---------\n" test_name;
  let s, t, decls_map = case_generator () in
  let decls_map_new = merge_fcn decls_map s t in
  let _ = decls_map_new in
  ()
;;

(* 
let test_merge_tables () = 
  (* run_test "parallel merge with simple table pair" gen_simple_table_ex parallel_merge; *)
  run_test "sequential merge with nested if/else tables" gen_nested_if_tbl_ex sequential_merge;
  (* run_test "parallel merge with nested if/else tables" gen_nested_if_tbl_ex parallel_merge; *)
  (* run_test "parallel merge with infeasible path" gen_infeasible_tup_ex parallel_merge; *)

  exit 1

  (* test_nested_if_tables () *)
;;

let test_propagate_conditions () = 
  run_test "sequential merge with nested if/else tables" gen_nested_if_tbl_ex sequential_merge;
  DBG.printf outc "-------------\n";
  run_test "propagate conditions with nested if / else tables" gen_nested_if_tbl_ex propagate_condition;
;; *)


(********** NEW merge utils ************)

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
      | Match (r_id, pat, a_id) -> Match (r_id, extend_pat vars pat, a_id)
      | OffPath pat -> OffPath (extend_pat vars pat))
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
  the normalization step. 
  LEFT OFF HERE: 
    - this is a poor way to do things because of the diamond 
    problem. The more optimal rewrite of BranchElimination.ml
    may change this code significantly, or make it un-necessary.
*)

(* move this to Cid.ml? *)
module CidTbl = struct 
  type t = Cid.t
  let compare = Cid.compare
end

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


(* a bit of conditioned rule algebra: 
   convert the conditions on a rule into a Z3 equation *)
let eqn_of_precondition ctx cr = 
  let eqn_of_condition ctx c = 
    let fold_negs ctx eqns p = 
      let ctx, p_eqn = RS.eqn_of_pat ctx p in 
      let not_p = RS.Z3Bool.mk_not ctx p_eqn in 
      eqns@[not_p]
    in 
    let neg_eqns = CL.fold_left (fold_negs ctx) [] c.negs in 
    let pos_eqns = match c.pos with 
      | None -> []
      | Some p -> 
        let _, pos_eqn = RS.eqn_of_pat ctx p in 
        [pos_eqn]
    in 
    let eqn = RS.Z3Bool.mk_and ctx (neg_eqns@pos_eqns) in 
    eqn
  in
  let condition_eqs = CL.fold_left 
    (fun eqns c -> eqns@[eqn_of_condition ctx c])
    []
    cr.cs
  in 
  RS.Z3Bool.mk_or ctx condition_eqs
;;

(* figure out if the preconditions on a rule 
   actually mean that the rule executes unconditionally, 
   by testing if the negation of the preconditions is 
   feasible. 
   If the negation is not feasible, 
   remove all the preconditions because the rule always 
   executes. *)
(* 
this is not enough. The first branch of a 
program actually looks like this: 

if (event == 1) {
  if (x == 1) {
    foo(); [event == 1 && x == 1]
  }  
  else {
    bar(); [event == 1 && x != 1]
  }
  baz(); // [event == 1]
}

// baz's preconditions is: 
// [event == 1 && x == 1] || [event == 1 && x != 1]
// this should simplify to [event == 1]
// but how do we do that? 
// can we use z3 to find the variables that don't matter? 
// not directly. Have to do it manually... Ugh. 

algorithm: 
  for each variable in any condition: (fcn: get_variables)
    extract the clauses with that variable (fcn: view of a cr?)
    test if the resulting precondition is always true. (fcn: similar to current simplify / test)
    if it is always true, the variable is irrelevant and can be removed. (fcn: remove from cr?)
  - this is sub-optimal. Example: 
    if (x == 1 || y == 1) {
      foo();
    } 
    else { bar(); }
    baz(); 

    [x == 1, y == _; x == _; y = 1;] foo();
    [x != 1, y != 1] bar();
    [(x == 1, y == _; x == _; y = 1;) || (x != 1, y != 1)] baz();    
    we can't eliminate x, if we look at it by itself

algorithm 2: 
  - track the stack of conditions on every table. 
  - when you get to a join node, pop off the last condition. 
    - assumes the most recent condition is the same for all branches in a join node. 
      - is this correct? 

    if (x == 1 || y == 1) {
      foo(); [[x == 1, y == _; x == _; y = 1;]; []]
    } 
    else { bar(); [[x != 1, y != 1]; []] }
    baz(); 
*)
(* the idom-based algorithm makes this unnecessary. *)
let simplify_preconditions cr = 
  print_endline "simplifying preconditions: ";
  print_endline (dbgstr_of_conditions cr.cs);
  let ctx = RS.new_ctx () in 
  let precondition_eqn = eqn_of_precondition ctx cr in 
  let not_preconditions_eqn = RS.Z3Bool.mk_not ctx precondition_eqn in 
  let solver =  Z3.Solver.mk_simple_solver ctx in 
  Z3.Solver.add solver [not_preconditions_eqn];
  let is_sat = Z3.Solver.check solver [] in 
  match is_sat with
    | Z3.Solver.SATISFIABLE -> cr
    | Z3.Solver.UNSATISFIABLE -> {cr with cs = []}
    | Z3.Solver.UNKNOWN -> error "[simplify_conditions] Z3 returned unknown.."  
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

(* 

if (x = 1) { foo(); }
else { bar(); }
baz();

goal: 
want the call to baz to be unconditional. 

So, when we get the preconditions for baz, 
we should test to see if the preconditions cover all possible inputs. 

harder version: 

if (x == 1) { foo(); }
else { bar(); }
if (y == 1) {baz();}

here, the call to baz is not unconditional, but it 
still should not be conditioned on (x == 1). 
We should 



*)

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
let condition_tbl_from_preds idom cid_decls pred_tids pcs tid = 
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
;;
