(* Balance the subexpressions of commutative
operation expression trees, then atomize integer expressions.  *)
open CoreSyntax
open InterpHelpers
module DBG = BackendLogging

let silent = ref false
let outc = ref None
let dprint_endline = ref DBG.no_printf

exception Error of string

let error s = raise (Error s)
let err msg ex = error (msg ^ " " ^ Printing.exp_to_string ex)

let info str =
  if not !silent
  then Console.show_message str ANSITerminal.Green "normalizeIntOps"
;;

(**** Pass 1: Balance associative operations so we can compute them in the
       fewest number of stages possible.
 ****)

(* Return true if it is safe to balance trees of this operations; i.e., the
   operation is associative. Only considers operations of type
   int -> int -> int (boolean operations are handled separately) *)
let safe_to_balance = function
  | Plus | SatPlus | BitAnd | BitOr | BitXor -> true
  (* TODO: We could also handle subtraction by turning it into plus a negative *)
  | _ -> false
;;

(* flatten a tree of associative eops into a single eop.
      example:
      a + (b + (c + ((d + e) + f) ) ) ->
      + [a; b; c; d; e; f]
      Recursion stops when it hits an "atom" -- an expression
      that is not of the form EOp(<op>, _) *)
let rec extract_atomic_opargs op exp =
  match exp with
  | { e = EOp (o, args); _ } when o = op ->
    CL.map (extract_atomic_opargs op) args |> CL.flatten (* can recurse *)
  | _ -> [exp]
;;

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

let rec balance_op_list op lst =
  match lst with
  | [] -> error "Tried to balance an empty op list"
  | [e] -> [e]
  | e1 :: e2 :: tl ->
    let balanced_hd = eop_ty op [balance_exp e1; balance_exp e2] e1.ety in
    let balanced_tl = balance_op_list op tl in
    balance_op_list op (balanced_hd :: balanced_tl)

(**** transform operation expressions into balanced expression trees ****)
and balance_exp exp =
  (*  1. flatten expression: nested op --> op [arg1; arg2; ...; argn]
        - each arg is an expression _not_ of form EOp (op, ...)
      2. Iteratively pair up the arguments, then pair up the pairs, etc until
         we have one nested, balanced expression *)
  match op_of_exp exp with
  | Some op when safe_to_balance op ->
    dprint_eop exp;
    let op_list = extract_atomic_opargs op exp in
    let new_exp = balance_op_list op op_list |> List.hd in
    !dprint_endline
      ("[balance_assign_exp] original expression:\n"
      ^ Printing.exp_to_string exp);
    !dprint_endline
      ("[balance_assign_exp] balanced expression:\n"
      ^ Printing.exp_to_string new_exp);
    new_exp
  | _ -> exp
;;

let balance_assign_exps ds =
  let v =
    object
      inherit [_] s_map as super

      method! visit_exp _ exp =
        match exp.e with
        | EOp _ ->
          (* Don't recurse; we do that in balance_op_list *)
          balance_exp exp
        | _ -> super#visit_exp () exp

      method! visit_s ctx s =
        match s with
        | SAssign (id, exp) -> SAssign (id, balance_exp exp)
        | SLocal (id, ty, exp) -> SLocal (id, ty, balance_exp exp)
        | _ -> super#visit_s ctx s
    end
  in
  v#visit_decls () ds
;;

(**** Pass 2: Transform arithmetic expressions into atomic (binary) expressions
      by creating intermediate variables as necessary.
 ****)

(* convert an expression into a variable whose value is
  the evaluation result of an atomic expression *)
let rec to_immediate exp =
  match is_immediate exp with
  | true -> exp, []
  | false ->
    (match is_atomic exp with
     (* the expression is an atomic op, so we can replace it with a precomputation. *)
     | true ->
       let ty = exp.ety in
       let var_id = Id.fresh "to_immediate_tmp" in
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
  let transform_exp exp =
    !dprint_endline
      ("[transform_precompute_exps.visit_exp] exp: "
      ^ Printing.exp_to_string exp);
    let atomic_exp, new_precompute_stmts = to_atomic exp in
    !dprint_endline
      ("[transform_precompute_exps.visit_exp] atomic exp: "
      ^ Printing.exp_to_string atomic_exp);
    atomic_exp, new_precompute_stmts
  in
  let v =
    object
      inherit [_] s_map as super
      val mutable precompute = []

      (* skip memops! *)
      method! visit_DMemop _ m = DMemop m

      method! visit_exp _ exp =
        let new_exp, precompute_stmts = transform_exp exp in
        precompute <- precompute_stmts @ precompute;
        new_exp

      method! visit_statement _ stmt =
        precompute <- [];
        let stmt' = super#visit_statement () stmt in
        let stmt' = sequence_stmts (precompute @ [stmt']) in
        precompute <- [];
        stmt'
    end
  in
  v#visit_decls () ds
;;

let do_passes ds =
  if not !silent then DBG.start_mlog __FILE__ outc dprint_endline;
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
