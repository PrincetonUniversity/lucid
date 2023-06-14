(* Convert all calls to each table t into 
   a single call that is predicated on a 
   "call_num" variable, and followed by a 
   continuation block that executes all the code 
   in each control path that had a table call to t.

   Core algorithm:
    to convert table t:
      traverse the syntax tree until you reach the 
      dominating branch statement of t -- that is, 
      the last branch statement that gets executed 
      before _ALL_ calls to t. (if there is no 
      dominating branch statement, that means the table 
      gets executed unconditionally, and there is only 
      1 call to the table -- so we don't have to do 
      anything.)
      At the dominating branch statement b for t:
        1. replace each table call to t with an assignment 
           of a "table_t_callnum" variable. Cut all the stmts 
           that comes after t and put it in a dictionary 
           that maps the call num to the stmts.
        2. output the transformed dominator branch statement, 
           followed by:
           1) a call to the table, predicated on callnum != 0
           2) a match statement that branches on callnum, 
              executing the sequence of statements that 
              go after each callnum.
*)

(*  
note: 
this pass may break type system ordering (but that might be okay for the layout algorithm -- worth checking)
the scenario is when one branch touches the table and then a global, 
and another branch touches the global but _does not_ touch the table.
The branch that touches the table will have the table_match and the global op moved
to a later part of the code. 
The branch that does _not_ touch the table will have its global op left here, 
before the table.

This is the problematic scenario:
  match x with 
  | 1 -> {
      int result_1 = table_match(t, ...);
      int z = result_1 + 1;
      Array.set(x);
    }
  | 2 -> {
      int result_2 = table_match(t, ...);
      int zz = result_2 + 1;
      Array.set(x);
    }
  | 3 -> {
      Array.set(x);
  }

*)

open CoreSyntax
module TC = TofinoCore
module ZZ = Z

let rec tables_in_prog (tds:TC.tdecls) = 
  match tds with 
  | [] -> []
  | {td=TDGlobal(_, _, {e=ETableCreate(tbl_def); _}); _}::tds' -> 
    tbl_def::(tables_in_prog tds')
  | _::tds' -> tables_in_prog tds'

let tables_matched_in_prog (tds:TC.tdecls) =
  let tbl_ids = ref [] in
  let v =
    object
      inherit [_] TC.s_iter as super
      method! visit_tbl_match _ tm =
        tbl_ids := (tm.tbl |> CoreSyntax.exp_to_id)::(!tbl_ids)
    end
  in
  v#visit_tdecls () tds;
  !tbl_ids |> MiscUtils.unique_list_of
;;

let tables_defined_and_used_in_prog tds =
  let defined_tbls = tables_in_prog tds in
  let used_ids = tables_matched_in_prog tds in
  let defined_and_used_tbls = List.filter
    (fun tdef -> List.exists (fun id -> id = tdef.tid) used_ids)
    defined_tbls
  in
  defined_and_used_tbls
(*   if (List.length defined_tbls) <> (List.length defined_and_used_tbls)
  then (error "some declared tables are not used in the program!")
  else (defined_and_used_tbls)
 *)

;;


let evar_to_id exp = match exp.e with
  | EVar(cid) -> Cid.to_id cid
  | _ -> error "[exp_to_id] exp is not an EVar"
;;

let tblid_equal tid exp =
  Id.equal tid (evar_to_id exp)
    

(* is this statement a call to tid? *)
let is_tbl_call tid stmt =
  match stmt.s with
  | STableMatch(tm) -> 
    tblid_equal tid tm.tbl
  | _ -> false
;;

(* does this statement contain a call to tid? *)
let rec has_tbl_call tid stmt =
  match stmt.s with
  | SIf(_, s1, s2) -> 
    (has_tbl_call tid s1) || (has_tbl_call tid s2)
  | SMatch(_, bs) -> 
    List.fold_left
      (fun acc (_, branch_stmt) -> 
        acc || (has_tbl_call tid branch_stmt))
      false
      bs
  | SSeq(s1, s2) -> 
    (has_tbl_call tid s1) || (has_tbl_call tid s2)    
  | _ -> is_tbl_call tid stmt
;;

(* supporting variables for tables *)
let callnumvar_of_tid tid : (id * ty)  =
  Id.append_string "_callnum" tid, tint 32
;;
let callnumvar_of_table td : (id * ty) =
  Id.append_string "_callnum" td.tid, tint 32
;;

let keyvars_of_table td : (id * ty) list =
  List.mapi 
    (fun i sz -> Id.append_string ("_"^(string_of_int i)^"_key") td.tid, tint sz)
    (ty_of_tbl td).tkey_sizes
;;    
let argvars_of_table td : (id * ty) list =
  List.mapi 
    (fun i ty -> Id.append_string ("_"^(string_of_int i)^"_arg") td.tid, ty)
    (ty_of_tbl td).tparam_tys
;;    
let retvars_of_table td : (id * ty) list =
  List.mapi 
    (fun i ty -> Id.append_string ("_"^(string_of_int i)^"_ret") td.tid, ty)
    (ty_of_tbl td).tret_tys
;; 

let iovars_of_table td : (id * ty) list =
  (callnumvar_of_table td)
  ::(keyvars_of_table td)
  @(argvars_of_table td)
  @(retvars_of_table td)
;;


(* replace the match to tbl td 
   with statements to setup input for branchnum *)
let cut_tbl_match td tbl_match branchnum =
  let set_id, set_ty = callnumvar_of_table td in
  let set_callnum = sassign 
    (Cid.id set_id)
    (vint_exp branchnum (size_of_tint set_ty))
  in 
  let set_keys = List.map2
    (fun (key_id, _) keyarg -> 
      sassign (Cid.id key_id) keyarg)
    (keyvars_of_table td)
    tbl_match.keys
  in
  let set_args = List.map2
    (fun (arg_id, _) arg -> 
      sassign (Cid.id arg_id) arg)
    (argvars_of_table td)
    tbl_match.args
  in
  sequence_stmts (set_callnum::set_keys@set_args)
;;

(* important function: in the given statement: 
   1) prune all the branches that contain table_matches to tbl, 
      replacing each table_match with code to set shared 
      variables for tbl.
   2) add a single match call to tbl after the statement, 
      which uses the shared variables. 
   3) after the match call, add statements to unload the 
      output of the table match back to the appropriate 
      variables, and match statement that executes all 
      the code which was pruned. 

   So, for example: 
   if (b1) {
    int x = table_match(foo, (k), (a));
    bar = x + 1;
   }
   else {
    int y = table_match(foo, (k'), (a'));
    bar = y + 1;
   }
   --> 
   if (b1) {
    foo_callnum = 1; foo_0_key = k; foo_0_arg = a;
   }
   else {
    foo_callnum = 2; foo_0_key = k'; foo_0_arg = a';
   }
   if (foo_callnum != 0) {foo_0_ret = table_match(foo, (foo_0_key), (foo_0_arg))}
   match foo_callnum with
   | 1 -> {
    int x = foo_0_ret;
    bar = x + 1;
   }
   | 2 -> {
    int y = foo_0_ret;
    bar = y + 1;
   }
 *)

(* information about a pruned branch 
   containing a match for a table. *)
type pruned_branch = 
  {
    bcall_num : int;     
    btbl_match : tbl_match;
    bstmts : statement list;
  }
let rec prune_tbl_call_branches (tbl:tbl_def) stmt branchnum  : (statement * pruned_branch list * int) =
  (* remember: this is called from the immediate dominator branch *)
  match stmt.s with
  | SSeq(s1, s2) -> (
   (* sequence -- here, we need to recurse and decide whether to add s2 to the pruned branches 
      note that table calls may only appear in _either_ s1 or s2.*)
    match (has_tbl_call tbl.tid s1, has_tbl_call tbl.tid s2) with
    (* case -- table calls are in s1 *)
    | (true, false) ->   
      (* we are in a subtree of the dominator. So we always have to execute s2 after 
         the table call, even if the table call is itself conditionally executed in s1. *)
      let s1', pruned_branches, branchnum' = prune_tbl_call_branches tbl s1 branchnum in
      (* there might be multiple pruned branches and we want s2 to execute after all of them *) 
      let pruned_branches = List.map
        (fun pruned_branch -> 
          {pruned_branch with bstmts = pruned_branch.bstmts@[s2];})
        pruned_branches
      in
      let s2' = snoop in
      sseq s1' s2', pruned_branches, branchnum'    
    (* case -- table calls are in s2 *)
    | (false, true) -> 
      let s2', pruned_branches, branchnum' = prune_tbl_call_branches tbl s2 branchnum in
      sseq s1 s2', pruned_branches, branchnum'
    (* case -- no table calls *)
    | (false, false) -> sseq s1 s2, [], branchnum
    | (_, _) -> error "[prune_tbl_call_branches] matches to some table appear in sequence."
  )
  | SIf(e, s1, s2) -> 
    (* both branchs of an if may have a table call. So we just need to process 
       them in sequence.  *)
    (* process s1 *)
    let s1', s1_pruned, branchnum' = prune_tbl_call_branches tbl s1 branchnum in
    (* process s2, using the branchnum after s1 *)
    let s2', s2_pruned, branchnum'' = prune_tbl_call_branches tbl s2 branchnum' in
    (* return the if, with both continuations and the branchnum after s2 *)
    sifte e s1' s2', s1_pruned@s2_pruned, branchnum''
  | SMatch(es, bs) -> 
    let (bs', pruned, branchnum') = List.fold_left
      (fun (bs, pruned, branchnum) (pats, stmt) -> 
        let stmt', stmt_pruned, branchnum' = prune_tbl_call_branches tbl stmt branchnum in
        let bs' = bs@[(pats, stmt')] in
        let pruned' = pruned@stmt_pruned in
        (bs', pruned', branchnum'))
      ([], [], branchnum)
      bs
    in
    smatch es bs', pruned, branchnum'
  | STableMatch(tm) -> 
    (* when we reach a table call, replace it with setup stmt and 
       cut the rest of the branch off.*)
    let stmt', branchnum' = 
      if (tblid_equal tbl.tid tm.tbl)
      then (cut_tbl_match tbl tm branchnum, branchnum+1)
      else (stmt, branchnum) 
    in 
    stmt', [{bcall_num=branchnum; btbl_match=tm; bstmts=[];}], branchnum'
  (* for all other statements, do nothing *)
  | _ -> stmt, [], branchnum
;;

let count_tbl_matches tbl stmt = 
  let ct = ref 0 in
  let v =
    object
      inherit [_] s_iter as super
      method! visit_tbl_match _ tm =
        if (tblid_equal tbl.tid tm.tbl)
        then (ct := !ct + 1;)
    end
  in
  v#visit_statement () stmt;
  !ct
;;

(* important method: 
   is this the branch node that is the immediate 
 dominator of all the matches on tbl? *)
let rec is_branch_idom_of_tbl n_calls tbl stmt =
  match stmt.s with
  | STableMatch(_) -> false
  | SSeq _ -> false
  | SAssign _ -> false
  | SNoop -> false
  | SIf(_, s1, s2) -> 
    (* neither sub-branch contains a dominator, 
       and this statement has all the calls to tid. *)
    (* all table calls are in this statement *)
       (n_calls = count_tbl_matches tbl stmt)
    (*  *)
    && (not (contains_idom_branch n_calls tbl s1))
    && (not (contains_idom_branch n_calls tbl s2))
  | SMatch(_, bs) ->
    (* print_endline@@"[is_branch_idom_of_tbl] checking match ("^(CorePrinting.comma_sep CorePrinting.exp_to_string es)^") with..."; *)
    let has_all_calls = (n_calls = count_tbl_matches tbl stmt) in
(*     if (not has_all_calls)
    then(
      print_endline@@"[is_branch_idom_of_tbl] debug for match ("^(CorePrinting.comma_sep CorePrinting.exp_to_string es)^") with...";
      print_endline@@"[is_branch_idom_of_tbl] FALSE: statement does not have all matches to tbl"); *)
    let branch_has_idom = (List.fold_left
        (fun acc (_, bstmt) -> 
          let has_idom = (contains_idom_branch n_calls tbl bstmt) in
(*           if (has_idom)
          then (
            print_endline@@"[is_branch_idom_of_tbl.FOLD] debug for match ("^(CorePrinting.comma_sep CorePrinting.exp_to_string es)^") with...";
            print_endline@@"[is_branch_idom_of_tbl.FOLD] branch with pattern "^(CorePrinting.comma_sep CorePrinting.pat_to_string pat)^" has idom in it"
          ); *)
          acc || has_idom)
        false
        bs)
    in
(*     if (branch_has_idom)
    then ( 
      print_endline@@"[is_branch_idom_of_tbl] debug for match ("^(CorePrinting.comma_sep CorePrinting.exp_to_string es)^") with...";
      print_endline@@"[is_branch_idom_of_tbl] FALSE: a branch of the match has the IDOM in it..."); *)
    let res = has_all_calls && (not branch_has_idom) in
(*     print_endline@@"[is_branch_idom_of_tbl] result for match ("^(CorePrinting.comma_sep CorePrinting.exp_to_string es)^") with...";
    print_endline@@(string_of_bool res); *)
(*     && (List.fold_left
        (fun acc (_, bstmt) -> 
          acc && (not (contains_idom_branch n_calls tbl bstmt)))
        true
        bs)
    in *)
    res
  | _ -> false

and contains_idom_branch n_calls tbl stmt = 
  match stmt.s with
  | SSeq(s1, s2) -> 
       (contains_idom_branch n_calls tbl s1)
    || (contains_idom_branch n_calls tbl s2)
  | SIf(_, s1, s2) -> 
       (contains_idom_branch n_calls tbl s1)
    || (contains_idom_branch n_calls tbl s2)
    || (is_branch_idom_of_tbl n_calls tbl stmt)
  | SMatch(_, bs) -> 
    let is_idom = (is_branch_idom_of_tbl n_calls tbl stmt) in
    let some_branch_contains_idom = 
      (List.fold_left 
        (fun acc (_, bstmt) -> 
          acc || (contains_idom_branch n_calls tbl bstmt))
        false
        bs)    
    in
    is_idom || some_branch_contains_idom
  (* no other statements can contain an idom branch *)
  | _ -> false;;

(* code generators: *)

(* generate the table match statement -- 
   an if statement that executes the match if its callnum var is set. 
   note: since this is placed after if statements are converted to 
   match statements, we generate a match instead of an if. *)
let merged_match_stmt tbldef = 
  let cnum_var, cnum_ty = callnumvar_of_table tbldef in
  let tbl_match = {
    tbl = exp_of_id tbldef.tid tbldef.tty;
    keys = List.map 
      (fun (id, ty) -> exp_of_id id ty)
      (keyvars_of_table tbldef);
    args = List.map 
      (fun (id, ty) -> exp_of_id id ty)
      (argvars_of_table tbldef);
    outs = List.map fst (retvars_of_table tbldef);
    out_tys = None; (* none because the out vars are declared already. *)
    }
  in 
  let branches = [
    ([PNum(Z.of_int(0))], snoop);
    ([PWild], statement (STableMatch(tbl_match)))
  ] in
  let wrapper_stmt = smatch 
    [exp_of_id cnum_var cnum_ty]
    branches 
  in
  {wrapper_stmt with spragma = Some("table", [])}
(*   {(sifte 
    (op_sp Neq [(exp_of_id cnum_var cnum_ty);(vint_exp 0 (size_of_tint cnum_ty))] cnum_ty Span.default)
    (statement (STableMatch(tbl_match)))
    (snoop)) with 
    spragma = Some("table", [])} *)
;;

let pruned_branch_continuations tbl (pruned_branches:pruned_branch list) =
  (* for each pruned branch, make a branch to call after the table that:  
      1. create (SLocal) or assign (SAssign) all the output variables from the match. 
      2. append the other statements. 
      3. has a pattern equal to the pruned branch's table call number *)
  let cnum_var, cnum_ty = callnumvar_of_table tbl in
  let es = [exp_of_id cnum_var cnum_ty] in 
  let bs = List.map
    (fun pruned_branch -> 
      let tbl_match = pruned_branch.btbl_match in
      let outvar_stmts = match tbl_match.out_tys with
        (* assign table outputs to this branch's out vars *)
        | None -> (
          List.map2
            (fun branch_id (tbl_id, tbl_ty) -> 
              sassign (Cid.id branch_id) (exp_of_id tbl_id tbl_ty))
            (tbl_match.outs)
            (retvars_of_table tbl))
        | Some(_) -> (
          List.map2
            (fun branch_id (tbl_id, tbl_ty) -> 
              slocal branch_id tbl_ty (exp_of_id tbl_id tbl_ty))
            (tbl_match.outs)
            (retvars_of_table tbl))
       in
       [PNum((Z.of_int pruned_branch.bcall_num))],
       sequence_stmts (outvar_stmts@pruned_branch.bstmts))
     (pruned_branches)
  in
  smatch es bs
;;

(* main function: merge all matches on table "t" into a 
   single match on "t". Place it immediately after 
   t's dominator branch. The single match to "t" is 
   applied based on a flag (t_call) indicating which call 
   to t was reached. This flag is set in statements
   that replace the original calls to "t". 
   The code in each branch after the match to "t" is cut, 
   and placed after the single match to "t" in a statement 
   that branches on t_call. *)
let rec merge_table_matches tbl n_calls stmt = 
  match stmt.s with
  | SIf(e, s1, s2) -> 
    (* this is the case that matters *)
    if (is_branch_idom_of_tbl n_calls tbl stmt)
    then (
      let stmt', pruned_branches, _ = prune_tbl_call_branches tbl stmt 1 in
      let tbl_call_stmt = merged_match_stmt tbl in
      let continuation = pruned_branch_continuations tbl pruned_branches in
      sseq stmt' (sseq tbl_call_stmt continuation))
    else (
      sifte 
        e 
        (merge_table_matches tbl n_calls s1)
        (merge_table_matches tbl n_calls s2))
  (* all the per-branch processing is done in the prune. *)
  | SMatch(es, bs) -> (
    if (is_branch_idom_of_tbl n_calls tbl stmt)
    then (
      (* print_endline ("[merge_table_matches] found a idom match statement."); *)
      let stmt', pruned_branches, _ = prune_tbl_call_branches tbl stmt 1 in
      let tbl_call_stmt = merged_match_stmt tbl in
      let continuation = pruned_branch_continuations tbl pruned_branches in
      sseq stmt' (sseq tbl_call_stmt continuation)     
    )
    else (
      smatch
        es
        (List.map
          (fun (pat, bstmt) -> 
            pat, merge_table_matches tbl n_calls bstmt)
          bs)))
  | SSeq(s1, s2) -> 
    sseq 
      (merge_table_matches tbl n_calls s1) 
      (merge_table_matches tbl n_calls s2) 
    (* we have reached a table that is not in a branch. 
       That means the table is called unconditionally, 
       which also means there can only be 1 call to the 
       table. So we actually don't need to do anything.  *)
  | STableMatch(_) -> 
    (* print_endline ("[merge_table_matches] reached table match without finding an idom branch."); *)
    stmt
  | _ -> stmt
;;
let process_table stmt tbl =
  (* Process a single table. *)
  let n_calls = count_tbl_matches tbl stmt in
  merge_table_matches tbl n_calls stmt, iovars_of_table tbl
;;

(* this pass produces buggy code when tables are not used in any event *)
let process tds =
(*   print_endline("starting single table match transformation...");
  print_endline("----- prog -----");
  print_endline (TC.tdecls_to_string tds);
 *)  let tbls = tables_defined_and_used_in_prog tds in 
  let main = TC.main tds in
  if (List.length main.main_body > 1)
  then (error "[SingleTableMatch] the main handler in this program is organized into multiple stages, but this pass should be run before staging.");
  (* combine all the table matches for one table at a time *)
  let main_body', tbl_iovars = List.fold_left
    (fun (main_body, tbl_iovars) tbl -> 
      let main_body, new_iovars = process_table main_body tbl in
      main_body, tbl_iovars@new_iovars)
    ((List.hd main.main_body), [])
    tbls
  in 
  let res = TC.update_main 
    tds 
    {main with 
      main_body=[main_body']; 
      shared_locals=main.shared_locals@tbl_iovars} 
  in
(*   print_endline ("finished single table match transformation");
  print_endline("----- prog -----");
  print_endline (TC.tdecls_to_string res); *)
  res
;;

(**** end new table call code ****)

(* updated methods for new tofinocore *)
open TofinoCoreNew


let rec tables_in_prog (tds:tdecls) = 
  match tds with 
  | [] -> []
  | {td=TDGlobal(_, _, {e=ETableCreate(tbl_def); _}); _}::tds' -> 
    tbl_def::(tables_in_prog tds')
  | _::tds' -> tables_in_prog tds'

let tables_matched_in_prog (tds:tdecls) =
  let tbl_ids = ref [] in
  let v =
    object
      inherit [_] s_iter as super
      method! visit_tbl_match _ tm =
        tbl_ids := (tm.tbl |> CoreSyntax.exp_to_id)::(!tbl_ids)
    end
  in
  v#visit_tdecls () tds;
  !tbl_ids |> MiscUtils.unique_list_of
;;


let tables_defined_and_used_in_prog (tds : tdecls) =
  let defined_tbls = tables_in_prog tds in
  let used_ids = tables_matched_in_prog tds in
  let defined_and_used_tbls = List.filter
    (fun tdef -> List.exists (fun id -> id = tdef.tid) used_ids)
    defined_tbls
  in
  defined_and_used_tbls
(*   if (List.length defined_tbls) <> (List.length defined_and_used_tbls)
  then (error "some declared tables are not used in the program!")
  else (defined_and_used_tbls)
 *)

;;
(* this pass produces buggy code when tables are not used in any event *)
let process_core_tds (tds:tdecls) =
(*   print_endline("starting single table match transformation...");
  print_endline("----- prog -----");
  print_endline (TC.tdecls_to_string tds);
 *)  
  let tbls = tables_defined_and_used_in_prog tds in 
  let main = main_handler_of_decls tds in
  let main_body = match main.hdl_body with
    | SFlat(stmt) -> stmt
    | _ -> error "[SingleTableMatch] this should be run before layout"
  in  
  (* combine all the table matches for one table at a time *)
  let main_body', tbl_iovars = List.fold_left
    (fun (main_body, tbl_iovars) tbl -> 
      let main_body, new_iovars = process_table main_body tbl in
      main_body, tbl_iovars@new_iovars)
    (main_body, [])
    tbls
  in
  let main' =  {main with 
    hdl_body = SFlat(main_body');
    hdl_preallocated_vars = (main.hdl_preallocated_vars@tbl_iovars);
    } 
  in
  let tds' = replace_main_handler_of_decls tds main'  in
  tds'
;;

let process_core core_prog = 
  List.map 
    (fun comp -> {comp with comp_decls = process_core_tds comp.comp_decls})
    core_prog
;;