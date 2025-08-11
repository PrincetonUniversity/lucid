open Batteries
open CoreSyntax
open Collections

(* Hoisting moves statements from inside of a conditional to outside of the conditional if it is safe to do so.

if (x == 1) {
  int y = foo();
  z = y + 2;
}
else {
  z = 7;
}
// transformed into:
int y = foo();
if (x == 1) {
  z = y + 2;
}
else {
  z = 7;
}
*)

(* Return a set of variables which are mutated in this statement. Includes
   variables which are defined in a subordinate scope inside the statement,
   so it requires alpha-renaming to really be useful. *)
let assigned_in_stmt stmt =
  let v =
    object
      inherit [_] s_iter as super
      val mutable assigned = CidSet.empty
      method assigned = assigned
      method! visit_SAssign _ id _ = assigned <- CidSet.add id assigned
      method! visit_SLocal  _ id _ _ = assigned <- CidSet.add (Cid.id id) assigned
    end
  in
  v#visit_statement () (stmt);
  v#assigned
;;

(* variables read in an expression *)
let vars e =
  let v =
    object
      inherit [_] s_iter as super
      val mutable vars = CidSet.empty
      method vars = vars
      method! visit_EVar _ cid = vars <- CidSet.add (cid) vars
    end
  in
  v#visit_exp () e;
  v#vars
;;

(* variables read in a statement *)
let stmt_vars stmt = 
  let v =
    object
      inherit [_] s_iter as super
      val mutable vars = CidSet.empty
      method vars = vars
      method! visit_EVar _ cid = vars <- CidSet.add (cid) vars
      method! visit_SAssign _ _ exp = super#visit_exp () exp
    end
  in
  v#visit_statement () stmt;
  v#vars

(* does a statment contain any of the mutated variables?  *)
let stmt_is_unmutated (mut_vars : CidSet.t) stmt = 
  let vars_in = stmt_vars stmt in
  (CidSet.is_empty (CidSet.inter vars_in mut_vars))
;;

let unmutated_stmts mut_vars stmts = 
  List.filter (stmt_is_unmutated mut_vars) stmts
;;

(* split the stmts_to_place into two lists: 
   non_deps and deps. 
   non_deps are statements that can be placed before the src_stmt, 
   deps are statements that depend on src_stmt and must be placed after it. *)
let stmt_to_deps src_stmt (stmts_to_place: statement list) = 
  let non_deps, deps = List.fold_left
  (fun (non_deps, deps) stmt_to_place -> 
    if (CidSet.is_empty (CidSet.inter (assigned_in_stmt src_stmt) (stmt_vars stmt_to_place)))
      (* if the cidset is empty... nothing in the statment was modified, 
          and we can place before *)
      then (non_deps@[stmt_to_place], deps)
      else (non_deps, deps@[stmt_to_place]))
  ([], [])
  stmts_to_place
  in
  non_deps, deps
;;

(* one "hop" of dependency calculation.
   move all elements of rest that depend on deps into deps *)
let single_hop_deps (rest, deps) =
  let rest, new_deps = List.fold_left
    (fun (rest, new_deps) dependee -> 
      (* update non deps, possibly taking some out, and new_deps, possibly adding some *)
      let rest, new_deps' = stmt_to_deps (dependee) rest in
      rest, new_deps@new_deps'    
      )
    (rest, [])
    deps
  in
  rest, (deps@new_deps)
;;

(* get transitive dependencies, by repeatedly calling single_hop_deps
  until the length of non_deps doesn't change. *)
let transitive_deps (rest, deps) = 
  let rec aux (rest, deps) =
    let rest', deps' = single_hop_deps (rest, deps) in
    if (List.length rest' = List.length rest)
      then (rest', deps')
      else aux (rest', deps')
  in
  aux (rest, deps)
;;

(* pick out the dependencies of a statement, from stmts_to_place, 
   which can be placed before it. *)
let stmt_to_transitive_deps stmt stmts_to_place = 
  stmt_to_deps stmt stmts_to_place |> transitive_deps
;;


let dbg_break : bool ref = ref false ;;
(* traverse the program in reverse. Every time you reach an slocal, 
   remember it and try to move it earlier in the program, 
   to immediately after the last modification of a variable in the slocal's rhs *)
let rec hoist (stmts_to_place : statement list) stmt = 
  let (stmt', stmts_to_place') = match stmt.s with
  | SLocal(_, _, exp) -> (
    (* before hoisting, get all the statements that depend on this one. *)
    (* only hoist atomic operation and hashes *)
    match exp.e with 
    | EOp _ when InterpHelpers.is_atomic exp -> 
      snoop, ( stmt)::stmts_to_place
    | EHash _ when InterpHelpers.is_atomic exp -> 
      snoop, ( stmt)::stmts_to_place
    (* if we are _not_ hoisting this statement, we need to 
       see if any statements to place must be placed after it. *)
    | _ ->       
      let non_deps, deps = stmt_to_transitive_deps stmt stmts_to_place in
      sequence_stmts (stmt::(deps)), non_deps
      
  )
  | SSeq(s1, s2) -> 
    (* walk backwards *)
    let s2, s2_stmts_to_place = hoist stmts_to_place s2 in
    let s1, s1_stmts_to_place = hoist s2_stmts_to_place s1 in
    {stmt with s = SSeq(s1, s2)}, s1_stmts_to_place
    (* that's it *)
  | SIf(e, s1, s2) -> 
    (* hoist whatever you can. 
       Don't place anything from before the if into the if *)
    let s1, stmts_from_s1 = hoist [] s1 in
    let s2, stmts_from_s2 = hoist [] s2 in
    (* tricky part: if any variable from the statements we are
       trying to place is modified in the sif, then we have to place 
       the statement right after. Its because SIf is something that 
       can mutate. *)
    let non_deps, deps = stmt_to_transitive_deps stmt stmts_to_place in
    sequence_stmts ({stmt with s = SIf(e, s1, s2)}::( deps)), (non_deps@stmts_from_s1@stmts_from_s2)
  | SAssign(_, _) -> (
    (* we are walking backwards. If cid is in a statement to place, 
       then cid is the last undefined variable in it -- i.e., no mutations 
       to any cid in the statement happen between here and the original 
       statement's spot. If they did, we would have run into them and 
       removed the statement from the statements_to_place *)
    let non_deps, deps = stmt_to_transitive_deps stmt stmts_to_place 
    in
    sequence_stmts (stmt::( deps)), non_deps
  )
  | SMatch(es, bs) -> (
    let bs, stmts_to_place_branches = List.fold_left 
      (fun (bs, stmts_to_place_branches) (ps, stmt) -> 
        let stmt, to_place = hoist [] stmt in
        (bs@[ps, stmt]), (stmts_to_place_branches@to_place))
      ([], [])
      bs
    in
    let stmts_before, stmts_after = stmt_to_transitive_deps stmt stmts_to_place in
    sequence_stmts ({stmt with s=SMatch(es, bs)}::( stmts_after)), (stmts_before@stmts_to_place_branches)
  )
  (* tuple assign has the same semantics as table match, meaning we don't try to hoist it *)
  | STupleAssign _ -> stmt, stmts_to_place
  (* nothing else changes *)
  | SNoop | SUnit _ | SPrintf _ | SGen _ | SRet _ -> stmt, stmts_to_place
  (* | STableMatch _ -> stmt, stmts_to_place *)
  in
  (stmt', stmts_to_place')
;;

let rec process decls = 
  match decls with
  | [] -> []
  | decl::decls -> (
    match decl.d with
    | DHandler(id, sort, (params, stmt)) ->
      let stmt, pre_stmts = hoist [] stmt in
      let stmt = sequence_stmts (( pre_stmts)@[stmt]) in 
      {decl with d=DHandler(id, sort, (params, stmt))}::(process decls)
    | _ -> decl::(process decls)
  )
  
;;
