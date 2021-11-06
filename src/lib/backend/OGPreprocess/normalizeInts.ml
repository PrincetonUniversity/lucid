(* Balance the subexpressions of commutative 
operation expression trees, then atomize integer expressions.  *)
open Syntax
open InterpHelpers
module DBG = BackendLogging

let outc = ref None
let dprint_endline = ref DBG.no_printf

exception Error of string

let error s = raise (Error s)
let err msg ex = error (msg ^ " " ^ Printing.exp_to_string ex)
let info str = Console.show_message str ANSITerminal.Green "normalizeIntOps"

let dprint_eop exp =
  let op, args, _, _ = unpack_eop exp in
  let flat_args = CL.map (extract_atomic_opargs op) args |> CL.flatten in
  !dprint_endline "[balance_assign_exp] flat args: ";
  CL.iter
    (fun exp ->
      !dprint_endline ("[balance_assign_exp]" ^ Printing.exp_to_string exp))
    flat_args;
  !dprint_endline "---[balance_assign_exp] flat args--- "
;;

(**** transform operation expressions into balanced expression trees ****)
let balance_assign_exp s =
  (*  1. flatten expression based on op --> 
            op [arg1; arg2; ...; argn]
            - each arg is an expression _not_ of type op
        2. build a balanced tree from the list of atoms *)
  (* extract the expression *)
  let exp =
    match s with
    | SLocal (_, _, exp) -> exp
    | SAssign (_, exp) -> exp
    | _ -> error "not an assign or local"
  in
  (* update the exp *)
  let new_exp =
    match op_of_exp exp with
    (* plus is commutative *)
    | Some Plus ->
      dprint_eop exp;
      let new_exp = balance_eop_tree exp in
      !dprint_endline
        ("[balance_assign_exp] original expression:\n"
        ^ Printing.exp_to_string exp);
      !dprint_endline
        ("[balance_assign_exp] balanced expression:\n"
        ^ Printing.exp_to_string new_exp);
      new_exp
    | Some _ -> exp
    | None -> exp
  in
  (* rebuild the statement *)
  match s with
  | SLocal (id, ty, _) -> SLocal (id, ty, new_exp)
  | SAssign (id, _) -> SAssign (id, new_exp)
  | _ -> error "not an assign or local"
;;

let balance_assign_exps ds =
  let v =
    object
      inherit [_] s_map as super

      method! visit_s ctx s =
        (* Question: 
            is it safe to balance every single expression, not just rhs of assign and local? *)
        match s with
        | SAssign _ | SLocal _ -> balance_assign_exp s
        | _ -> super#visit_s ctx s
    end
  in
  v#visit_decls () ds
;;

(**** transform expressions into atomic expressions ****)

(* convert an expression into a variable whose value is 
  the evaluation result of an atomic expression *)
let rec to_immediate exp =
  match is_immediate exp with
  | true -> exp, []
  | false ->
    (match is_atomic exp with
    (* the expression is an atomic op, so we can replace it with a precomputation. *)
    | true ->
      let ty = ty_of_exp exp in
      let var_id = Id.fresh "pc_tmp" in
      let stmt = slocal var_id ty exp in
      let exp = { exp with e = EVar (Cid.id var_id) } in
      exp, [stmt]
    (* convert the expression into an atomic expression, then an immediate *)
    | false ->
      let exp, stmts_a = to_atomic exp in
      let exp, stmts_b = to_immediate exp in
      exp, stmts_a @ stmts_b)

(* convert an expression into an atomic operation. If 
  any of the expressions arguments are not immediates, 
convert them to an immediate first. *)
and to_atomic exp =
  match is_atomic exp with
  | true -> exp, []
  | false ->
    let args = args_of_exp exp in
    (* make all the arguments into immediates *)
    let immediate_args, precompute_stmts =
      CL.map to_immediate args |> CL.split
    in
    let precompute_stmts = CL.flatten precompute_stmts in
    (* return expression that uses immediates and the precompute statement *)
    let exp = replace_args exp immediate_args in
    exp, precompute_stmts
;;

let atomize_int_assigns ds =
  let v =
    object
      inherit [_] s_map as super
      val mutable precompute_stmts : statement list = []
      method precompute_stmts = precompute_stmts

      (* skip memops! *)
      method! visit_DMemop _ id body = DMemop (id, body)

      method! visit_statement ctx stmt =
        match stmt.s with
        | SAssign _ | SLocal _ ->
          precompute_stmts <- [];
          (* recurse on statement tree, passing context as true *)
          let transformed_stmt = super#visit_statement true stmt in
          (* if there are any precompute statements, place them first  *)
          fold_stmts (precompute_stmts @ [transformed_stmt])
        (* for other statement kinds, just recurse *)
        | _ -> super#visit_statement ctx stmt

      method! visit_exp in_assign exp =
        (* We only want to normalize expressions on the rhs of an assignment. 
           By this point in time, all other complex expressions are removed. *)
        match in_assign with
        | true ->
          (* transform the expression into an atomic (if not already) *)
          !dprint_endline
            ("[transform_precompute_exps.visit_exp] exp: "
            ^ Printing.exp_to_string exp);
          let atomic_exp, new_precompute_stmts = to_atomic exp in
          precompute_stmts <- precompute_stmts @ new_precompute_stmts;
          !dprint_endline
            ("[transform_precompute_exps.visit_exp] atomic exp: "
            ^ Printing.exp_to_string atomic_exp);
          atomic_exp
        | false -> exp
    end
  in
  v#visit_decls false ds
;;

let do_passes ds =
  DBG.start_mlog __FILE__ outc dprint_endline;
  let orig_ds = ds in
  let ds = balance_assign_exps ds in
  info "assignments transformed to balanced exps";
  let balanced_ds = ds in
  let ds = atomize_int_assigns ds in
  info "expressions atomized";
  (* exit 1; *)
  !dprint_endline "original program: ";
  !dprint_endline (Printing.decls_to_string orig_ds);
  !dprint_endline "program after exp tree balancing: ";
  !dprint_endline (Printing.decls_to_string balanced_ds);
  !dprint_endline "program after precomputation pass: ";
  !dprint_endline (Printing.decls_to_string ds);
  !dprint_endline "compute expressions simplified.";
  (* exit 1; *)
  ds
;;
