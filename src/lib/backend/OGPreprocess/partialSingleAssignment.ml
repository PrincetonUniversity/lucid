(* What???
  Single assignment form adds a bunch of variables to the backend.
  This puts pressure on the PHV layout stage of the tofino backend.
  It is amplified because we don't do general register allocation
  to reduce the number of local variables.
  Ultimately, single assignment form causes some programs to fail,
  for the current Lucid compiler.
  As a temporary workaround, this module puts certain parts
  of a program into single assignment form, so that the
  layout stage doesn't generate semantically incorrect programs.
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
let assigned_in_stmt query_id stmt =
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
  | Cid.Id id -> Cid.id (Id.refresh id)
  | Cid.Compound (id, cid) -> Cid.compound (Id.refresh id) (refresh_cid cid)
;;

let assigned_in_one_stmt query_id stmts =
  CL.map (assigned_in_stmt query_id) stmts |> CL.fold_left ( || ) false
;;

(* update exp so that if var_subexp is used in any statement in stmts,
  it is replaced with a constant copy. *)
let update_exp_for_one_var stmts (exp, newvar_decls) var_subexp =
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
  match assigned_in_one_stmt var_id stmts with
  | true ->
    !dprint_endline
      ("[update_exp_for_one_var] in expression: ("
      ^ Printing.exp_to_string exp
      ^ ")");
    !dprint_endline
      ("[update_exp_for_one_var] changing variable: " ^ Id.to_string var_id);
    let var_ty = ty_of_exp var_subexp in
    let new_var_cid = refresh_cid var_cid in
    let new_var_subexp = aexp (EVar new_var_cid) (Some var_ty) Span.default in
    let new_exp = replace_in_exp exp var_cid new_var_subexp in
    let new_var_decl =
      slocal (Cid.to_id new_var_cid) (ty_of_exp var_subexp) var_subexp
    in
    new_exp, new_var_decl :: newvar_decls
  (* otherwise, do nothing. *)
  | false -> exp, newvar_decls
;;

(* update one expression, so that any vars used in the expression and also the statements
are replaced in the expression with constant copies. *)
let update_one_exp stmts exp =
  let new_exp, new_var_decls =
    CL.fold_left (update_exp_for_one_var stmts) (exp, []) (evars_in_exp exp)
  in
  new_exp, new_var_decls
;;

(* transform all the if and match statements so that any variable
  in an if or match expression that is used in the statement branches
of the if / match is replaced by a constant. *)
let const_branch_vars ds =
  DBG.start_mlog __FILE__ outc dprint_endline;
  trans_info "making branch variables single assignment";
  let v =
    object
      inherit [_] s_map as super

      (* only visit handlers. *)
      method! visit_d ctx d =
        match d with
        | DHandler (id, body) -> super#visit_DHandler ctx id body
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
          let new_exp, new_var_decls = update_one_exp [st1; st2] exp in
          let new_vars_decl = fold_stmts new_var_decls in
          let new_sif = { statement with s = SIf (new_exp, st1, st2) } in
          (* return the sequence of new_vars_decl, new_if_stmt *)
          sseq new_vars_decl new_sif
        | SMatch (exps, branches) ->
          let _, sts = CL.split branches in
          let new_exps, new_var_declses =
            CL.map (update_one_exp sts) exps |> CL.split
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
