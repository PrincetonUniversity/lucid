(*
  Single assignment form adds a bunch of variables to the backend.
  This puts pressure on the PHV layout stage of the tofino backend.
  It is amplified because we don't do general register allocation
  to reduce the number of local variables.
  Ultimately, single assignment form causes some programs to fail,
  for the current Lucid compiler.
  However, single assignment is necessary in some places. 
  Specifically, it is necessary for variables that are tested in 
  if statements -- should a variable tested by an if statement 
  change at some point p in a branch of that statement, 
  the straightline code generated for everything after p in the 
  branch will test the wrong condition for execution.
  This module converts if statements so that they never test 
  variables modified in their bodies, by making static copies of 
  any such variables immediately before the if statement. 
  Unfortunately, this adds overhead in some common cases -- such as 
  a for loop with an if branch inside of it. 
  
  Transformation pattern (for a single variable in a single if):
  input node: 
    @ifstmt := {s=SIf(@exp, @s1, @s2)}
      where subtree(exp) contains @evar := {e=EVar(@id), ety=@ty}
      and subtree(s1) or subtree(s2) 
        contains @set_var := {s=SAssign(id, _, _)}
  output node:
    sseq 
      (slocal new_id ty evar)
      (ifstmt with evar ~> {e=EVar(new_id), ety=ty})

  note: 
    - a possible optimization is to only apply the transformation 
      if there are more than one statements in either branch. 
      But this cannot be applied currently, because we haven't 
      broken the statements down into primative statements yet... 
*)
open CoreSyntax
open Printf
open BatMap
open Batteries
open InterpHelpers
open MiscUtils
module CL = Caml.List
module DBG = BackendLogging

let outc = ref None
let dprint_endline = ref DBG.no_printf

(* apply single assignment, but only to the variables inside of if / match expressions *)

let count_locals ds =
  let v =
    object
      inherit [_] s_iter as super
      val mutable local_ids = []
      method local_ids = local_ids
      method! visit_SLocal _ id _ _ = local_ids <- id :: local_ids
    end
  in
  v#visit_decls () ds;
  CL.length v#local_ids
;;

(* check if a variable is set in a statement subtree. *)
let assigned_in_stmt (evid_params:((Id.t * Id.t list) list)) query_id stmt =
  let _ = evid_params in 
  let v =
    object
      inherit [_] s_iter as super
      val mutable id_is_set = false
      method id_is_set = id_is_set
      method! visit_SAssign _ id _ = if query_id = id then id_is_set <- true

    end
  in
  v#visit_statement () stmt;
  v#id_is_set
;;

let rec refresh_cid cid =
  match cid with
  | Cid.Id id -> 
    let unique_name = (fst id)^"_branch_test_copy_"^((snd (Id.refresh id)) |> string_of_int) in
    Cid.id (Id.fresh unique_name)
  | Cid.Compound (id, cid) -> Cid.compound (Id.refresh id) (refresh_cid cid)
;;

let assigned_in_one_stmt evid_params query_id stmts =
  CL.map (assigned_in_stmt evid_params query_id) stmts |> CL.fold_left ( || ) false
;;

(* update exp so that if var_subexp is used in any statement in stmts,
  it is replaced with a constant copy. *)
let update_exp_for_one_var evid_params stmts (exp, newvar_decls) var_subexp =
  let var_cid =
    match var_subexp.e with
    | EVar cid -> cid
    | _ -> error "subexp is not an evar."
  in
  let var_id =
    match var_cid with
    | Id id -> id
    | _ -> error "subexp var is a compound identifier."
  in
  (* if the variable is assigned in at least one statement, change it. *)
  (* optimization: the variable should be assigned in one statement, 
     and then there must be at least one other statement *)
  match assigned_in_one_stmt evid_params var_id stmts with
  | true ->
    !dprint_endline
      ("[update_exp_for_one_var] in expression: ("
      ^ Printing.exp_to_string exp
      ^ ")");
    !dprint_endline
      ("[update_exp_for_one_var] changing variable: " ^ Id.to_string var_id);
    let var_ty = var_subexp.ety in
    let new_var_cid = refresh_cid var_cid in
    let new_var_subexp = aexp (EVar new_var_cid) var_ty Span.default in
    let new_exp = replace_in_exp exp var_cid new_var_subexp in
    let new_var_decl =
      slocal (Cid.to_id new_var_cid) var_subexp.ety var_subexp
    in
    new_exp, new_var_decl :: newvar_decls
  (* otherwise, do nothing. *)
  | false -> exp, newvar_decls
;;

(* update a branch statement, so that any vars used in the 
    expression and modified in the statements are replaced 
    with constant copies *)
let update_branch_exp evid_params stmts exp =
  let new_exp, new_var_decls =
    CL.fold_left (update_exp_for_one_var evid_params stmts) (exp, []) (evars_in_exp exp)
  in
  new_exp, new_var_decls
;;

(* transform all the if and match statements so that any variable
  in an if or match expression that is used in the statement branches
of the if / match is replaced by a constant. *)
let const_branch_vars ds =
  DBG.start_mlog __FILE__ outc dprint_endline;
  trans_info "making branch variables single assignment";
  let evid_params = List.filter_map (fun decl -> 
    match decl.d with
    | DEvent(id, _, params) -> Some(id, List.split params |> fst)
    | _ -> None
    )
    ds
  in

  let v =
    object
      inherit [_] s_map as super

      (* only visit handlers. *)
      method! visit_d ctx d =
        match d with
        | DHandler (id, s, body) -> super#visit_DHandler ctx id s body
        | _ -> d

      method! visit_statement ctx statement =
        (* start by visiting the statement subtree *)
        let statement = super#visit_statement ctx statement in
        match statement.s with
        | SIf (exp, st1, st2) ->
          (* core function: update the conditional expression exp,
        so that a single evar subexpression in it, var_subexp,
        is replaced by a fresh, constant copy of the variable.
        Also generate the statement that sets the constant copy. *)
          let new_exp, new_var_decls = update_branch_exp evid_params [st1; st2] exp in
          let new_vars_decl = fold_stmts new_var_decls in
          let new_sif = { statement with s = SIf (new_exp, st1, st2) } in
          (* return the sequence of new_vars_decl, new_if_stmt *)
          sseq new_vars_decl new_sif
        | SMatch (exps, branches) ->
          let _, sts = CL.split branches in
          let new_exps, new_var_declses =
            CL.map (update_branch_exp evid_params sts) exps |> CL.split
          in
          let new_vars_decl = CL.flatten new_var_declses |> fold_stmts in
          let new_smatch = { statement with s = SMatch (new_exps, branches) } in
          sseq new_vars_decl new_smatch
        | _ -> statement
    end
  in
  !dprint_endline
    ("number of local variables before partial ssa: "
    ^ (count_locals ds |> string_of_int));
  let ds = v#visit_decls () ds in
  !dprint_endline
    ("number of local variables after partial ssa: "
    ^ (count_locals ds |> string_of_int));
  trans_info "partial ssa finished";
  ds
;;


  (* "PartialSingleAssignment" is a dumb name. Its just making 
      copies of variables that are tested in if statements and then 
      modified in the bodies of those statements. 
      Shouldn't data dependency analysis handle this? 
      
      The data dependency graph computed later doesn't handle this, because 
      we straightline the code _before_ we compute the data dependency graph.

      2/6/23
      The control / data dependency graph calculation has gotten more 
      robust in last year's refactor, and I don't think this pass is 
      needed any more, which would be wonderful... Example: 

      degenerate example:
      y = 0;
      if (x = 1) {
        x = x + 1;
        if (x = 2) {
          y = 1;
        }
      }
      f(y);
      
      y = 0;
      if (x = 1) {x = x + 1;}
      if (x = 1 && x = 2) {y = 1} // unreachable, will get deleted!
      f(y);

      goal: 
      // stage 1
      y = 0;
      // could we just start using a different x variable at this point in time, 
      // and set it in all the branches?
      if (x = 1) {x2 = x + 1;}e{x2 = x;}
      
      // so... don't change the if exp, but:
      // 1. change all assignments to x in the branches to assignments to x2
      // 2. if there is any branch that does not set x, add a statement that sets 2 = x. 
            for match statements: add a default case that sets x2 = x
      // 3. change all the reads to x after that point to reads of x2
      // 4. change all reads or writes of x after the branch to reads or writes of x2



      



      // original program
      // input: x = 1
      f();
      if (x = 1) {
        x = x + 1; // x = 2
        x = x + 2; // x = 4
      }
      g();
      // output: x = 3
      // --- straightlining without condition variable copying ---
      // phase 1: convert to control flow
      0: f();
      1: if (x = 1) {
      2:  x = x + 1;
      3:  x = x + 2; }
      4: g(); 
      0 -> 1     
      1 -> 2
      1 -> 4
      2 -> 3
      3 -> 4
      // phase 2: calculate conditions on each control flow node
      // note that, the graph still remains the same...
      0: f();
      1: _ -> if (x = 1) {
      2: (x == 1) -> x + 1;
      3: (x == 1) -> x + 2;
      4: _ -> g();
      0 -> 1     
      1 -> 2
      1 -> 4
      2 -> 3
      3 -> 4
      // phase 3: cut out branching control flow nodes, 
         adding new edges as needed to preserve control flow
      0: f();
      2: (x == 1) -> x + 1;
      3: (x == 1) -> x + 2;
      4: _ -> g();
      // 1 was cut out, with new edges from each pair in(1) -> out(1)
      0 -> 2
      0 -> 4     
      2 -> 3
      3 -> 4
      // phase 4: convert to data dependency graph...
      0: f();
      2: (x == 1) -> x + 1; // dependencies: none
      3: (x == 1) -> x + 2; // dependencies: 3 (because of 2 -> 3)
      4: _ -> g();          // dependencies: none
      // so actually... this pass ISNT NEEDED ANYMORE?!
    *)