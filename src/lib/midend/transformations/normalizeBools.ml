(* Transform if expressions into a canonical form that
can easily map to a match action table *)
open CoreSyntax
open Batteries
open InterpHelpers
module CL = Caml.List
open Printf
open Z3
module Z3Int = Arithmetic.Integer
module DBG = BackendLogging

let outc = ref None
let dprint_endline = ref DBG.no_printf

[@@@ocaml.warning "-21"]

let err msg ex = error (msg ^ " " ^ Printing.exp_to_string ex)

let info str =
  Console.show_message str ANSITerminal.Green "If-exp cannonization "
;;

(* helpers that should be moved to the CID module. *)
let rec cid_to_id_string cid =
  match cid with
  | Compound (id, cid) -> Id.to_string id ^ "." ^ cid_to_id_string cid
  | Id id -> Id.to_string id
;;

let cid_from_id_string str =
  let id_strings = String.split_on_char '.' str in
  let fold_f cid_opt id_string =
    let id = Id.of_id_string id_string in
    match cid_opt with
    | Some cid -> Some (Cid.compound id cid)
    | None -> Some (Cid.id id)
  in
  let cid_opt = CL.fold_left fold_f None id_strings in
  match cid_opt with
  | Some cid -> cid
  | None -> error "could not rebuild cid from id string"
;;

(* normalize relational operations in an if statement's branch
expression, so that every expression is of the form
(var <==, !=> constant)
*)
module NormalizeRelops = struct
  (* create a precompute statement of the form var = e1 - e2; *)
  let create_precompute_test e1 op e2 =
    let test_var_id = Id.fresh "if_precomp" in
    let test_var_cid = Cid.Id test_var_id in
    (* create the actual statement *)
    let rhs = aexp (EOp (op, [e1; e2])) e1.ety Span.default in
    (* how big is the temp variable? *)
    let test_var_stmt = slocal test_var_id e1.ety rhs in
    test_var_cid, test_var_stmt
  ;;

  (* normalize a boolean expression in an if. *)
  let rec normalize_exp exp =
    print_endline ("[normalize_exp] " ^ Printing.exp_to_string exp);
    match exp with
    (* operations need to be recursively normalized *)
    | { e = EOp (op, args); espan; ety } -> normalize_erelop op args ety espan
    (* boolean immediates need to be transformed into equality tests *)
    | { e = EVal _; _ } | { e = EVar _; _ } -> normalize_eimmediate exp, []
    | _ -> exp, []

  (* normalize an operation expression in an if *)
  and normalize_erelop (op : op) (args : exp list) ety espan =
    match op with
    (* boolean operations -- recurse on args *)
    | And | Or | Not | Cast _ ->
      let new_args, arg_precompute_stmts = normalize_exps args in
      aexp (EOp (op, new_args)) ety espan, arg_precompute_stmts
    (* relational operators -- precompute depending on form *)
    | Eq | Neq ->
      (match args with
      (* var, int  --> no change *)
      | [{ e = EVar _; _ }; { e = EVal _; _ }] ->
        aexp (EOp (op, args)) ety espan, []
      (* int, var  --> var, int *)
      | [{ e = EVal _; _ }; { e = EVar _; _ }] ->
        aexp (EOp (op, CL.rev args)) ety espan, []
      (* expr1, expr2 --> precompute var_test = expr1 - expr2; var_test == 0; *)
      | [e1; e2] ->
        let test_var_cid, calc_test_var = create_precompute_test e1 Sub e2 in
        let new_e1 = aexp (EVar test_var_cid) e1.ety Span.default in
        (* new_e2 should be the same size as new_e1 *)
        print_endline ("getting from e1: " ^ Printing.exp_to_string e1);
        let new_e2 = value_to_exp (vint 0 (intwidth_of_exp e1)) in
        let new_exp = aexp (EOp (op, [new_e1; new_e2])) ety espan in
        new_exp, [calc_test_var]
      (* optimization case -- int, int --> evaluate here and
          replace with a boolean value, but int, int ops
          should be eliminated earlier *)
      | _ -> error "unexpected args for Eq or Neq")
    | Less ->
      (match args with
      | [e1; e2] ->
        let test_var_cid, calc_test_var = create_precompute_test e2 SatSub e1 in
        let new_e1 = aexp (EVar test_var_cid) e1.ety Span.default in
        let new_e2 = value_to_exp (vint 0 (intwidth_of_exp e1)) in
        let new_exp = aexp (EOp (Neq, [new_e1; new_e2])) ety espan in
        new_exp, [calc_test_var]
      | _ -> error "unexpected args for Less")
    | More ->
      (match args with
      | [e1; e2] ->
        let test_var_cid, calc_test_var = create_precompute_test e1 SatSub e2 in
        let new_e1 = aexp (EVar test_var_cid) e1.ety Span.default in
        let new_e2 = value_to_exp (vint 0 (intwidth_of_exp e1)) in
        let new_exp = aexp (EOp (Neq, [new_e1; new_e2])) ety espan in
        new_exp, [calc_test_var]
      | _ -> error "unexpected args for More")
    | Leq | Geq ->
      error "[normalize_erelop] Leq and Geq should have been eliminated by now"
    (* other operators are the base case, because they cannot have rel ops as leaves *)
    | _ -> aexp (EOp (op, args)) ety espan, []

  (* normalize an immediate boolean expression in an if statement *)
  and normalize_eimmediate exp =
    let rty = raw_ty_of_exp exp in
    match rty with
    (* e --> op (eq, e, true) *)
    | TBool -> eop_ty Eq [exp; eval_bool true] exp.ety
    | _ -> exp

  and normalize_exps exps =
    let fold_f (exps, precompute_stmts) exp =
      let new_exp, new_stmts = normalize_exp exp in
      exps @ [new_exp], precompute_stmts @ new_stmts
    in
    CL.fold_left fold_f ([], []) exps
  ;;

  (* convert the expression in an if statement into:
  1. precompute statements
  2. an expression with atoms of the form: var <eq | neq> int *)
  let normalize_if if_stmt =
    let exp, s1, s2, sspan, precompute_stmts =
      match if_stmt with
      | { s = SIf (exp, s1, s2); sspan } ->
        let new_exp, precompute_stmts = normalize_exp exp in
        new_exp, s1, s2, sspan, precompute_stmts
      | _ -> error "not an if statement"
    in
    let precompute_stmt = fold_stmts precompute_stmts in
    let new_stmt = sseq_sp precompute_stmt (sifte_sp exp s1 s2 sspan) sspan in
    !dprint_endline "original if statement: ";
    !dprint_endline (Printing.stmt_to_string if_stmt);
    !dprint_endline "new if statement: ";
    !dprint_endline (Printing.stmt_to_string new_stmt);
    new_stmt
  ;;

  (* simplify every if statement in the program so that it has the form
    <const_test> [<bool_op> <const_test>]*
    where const_test compares a variable to a constant.  *)
  let normalize_ifs ds =
    let v =
      object
        inherit [_] s_map as super

        (* skip memops! *)
        method! visit_DMemop _ id body = DMemop (id, body)

        method! visit_statement ctx stmt =
          match stmt with
          | { s = SIf _; _ } ->
            (* visit then transform *)
            normalize_if (super#visit_statement ctx stmt)
          | _ -> super#visit_statement ctx stmt
      end
    in
    v#visit_decls () ds
  ;;
end

module NormalizeBoolExps = struct
  (*
    Normalize the boolean expressions that appear inside of an
    if statement so that they are a disjunction of conjunctions.
  *)

  (* Z3 wrappers *)
  let var_ctx = ref []

  let z3_init () =
    var_ctx := [];
    (* reset local context *)
    let ctx = mk_context ["model", "true"; "proof", "true"] in
    ctx
  ;;

  (* convert a boolean lucid expression into a z3 expression
  also builds var_ctx, a map from expression name strings to expression *)
  let rec z3_from_expr ctx exp =
    let _, _ = ctx, exp in
    match exp.e, exp.ety with
    (* variables *)
    | EVar cid, { raw_ty = TInt _ } ->
      let var_id_string = cid_to_id_string cid in
      var_ctx := (var_id_string, exp) :: !var_ctx;
      Z3Int.mk_const_s ctx var_id_string
    | EVar cid, { raw_ty = TBool } ->
      let var_id_string = cid_to_id_string cid in
      var_ctx := (var_id_string, exp) :: !var_ctx;
      Boolean.mk_const_s ctx var_id_string
    (* values -- give values ids and translate them into expressions stored in the context. *)
    | EVal { v = VBool _; _ }, _ ->
      let val_cid = Cid.fresh ["tmp_val_id"] in
      let var_id_string = cid_to_id_string val_cid in
      var_ctx := (var_id_string, exp) :: !var_ctx;
      Boolean.mk_const_s ctx var_id_string
    | EVal { v = VInt _; _ }, _ ->
      let val_cid = Cid.fresh ["tmp_val_id"] in
      let var_id_string = cid_to_id_string val_cid in
      var_ctx := (var_id_string, exp) :: !var_ctx;
      Arithmetic.Integer.mk_const_s ctx var_id_string
    (* operations *)
    | EOp (op, args), _ ->
      (match op with
      (* boolean operations *)
      | And -> Boolean.mk_and ctx (CL.map (z3_from_expr ctx) args)
      | Or -> Boolean.mk_or ctx (CL.map (z3_from_expr ctx) args)
      | Not -> Boolean.mk_not ctx (z3_from_expr ctx (CL.hd args))
      (* relational operations -- must be of the form <var> <relop> <const> *)
      | Eq ->
        let a1, a2 = unpack_binargs args in
        Boolean.mk_eq ctx (z3_from_expr ctx a1) (z3_from_expr ctx a2)
      | Neq ->
        let a1, a2 = unpack_binargs args in
        Boolean.mk_not
          ctx
          (Boolean.mk_eq ctx (z3_from_expr ctx a1) (z3_from_expr ctx a2))
      | Less | More ->
        error
          "got to an less or greater than in z3_from_expr -- this probably \
           means that the clause transformation pass has not run, or went \
           wrong. "
      | Cast _ ->
        z3_from_expr ctx (CL.hd args)
        (* we don't care about the bit width here. *)
      | _ ->
        error
          "Unsupported operation in z3_from_expr -- the clause transformation \
           pass may not have run, or failed.")
      (* statements that should have been eliminated by now. *)
    | EVar _, rty ->
      let expstr = Printing.exp_to_string exp in
      let tystr = Printing.ty_to_string rty in
      sprintf
        "z3_from_expr got a variable that is not an integer or boolean: (%s) : \
         <%s>"
        expstr
        tystr
      |> error
    | EVal _, _ ->
      error "z3_from_expr got a value that is not an integer or boolean..."
    | ECall _, _ ->
      error
        "z3_from_expr got an ecall -- this should have been eliminated by an \
         earlier pass."
    | EHash _, _ ->
      error
        "z3_from_expr got an ehash -- this should have been eliminated by an \
         earlier pass."
  ;;

  (* tell Z3 to convert a boolean expression into disjunctive normal form *)
  let dnf_of_boolexp ctx z3_exp =
    (* no idea why it needs these bool params this way: model, unsatCores, proofs *)
    let cnf_g = Z3.Goal.mk_goal ctx true false false in
    Z3.Goal.add cnf_g [z3_exp];
    (* define the tactic: *)
    (* (apply (then simplify propagate-values propagate-ineqs (repeat (or-else split-clause skip)))) *)
    let split_clause = Z3.Tactic.mk_tactic ctx "split-clause" in
    let skip = Z3.Tactic.mk_tactic ctx "skip" in
    let or_else = Z3.Tactic.or_else ctx split_clause skip in
    let repeat = Z3.Tactic.repeat ctx or_else 9999 in
    let propagate_ineqs = Z3.Tactic.mk_tactic ctx "propagate-ineqs" in
    let propagate_values = Z3.Tactic.mk_tactic ctx "propagate-values" in
    let simplify = Z3.Tactic.mk_tactic ctx "simplify" in
    let and_then =
      Z3.Tactic.and_then ctx simplify propagate_values [propagate_ineqs; repeat]
    in
    (* apply the tactic *)
    let app_res = Z3.Tactic.apply and_then cnf_g None in
    (* get expressions from output *)
    let disjunc_exps =
      List.map Goal.as_expr (Tactic.ApplyResult.get_subgoals app_res)
    in
    (* for the input expression to be true, one of disjunc_exps must be true *)
    disjunc_exps
  ;;

  (**** converting a z3 expression back into a boolean expression in lucid ****)
  let op_and_rty_of_kind z3e_kind =
    (* note: only boolean expressions are currently supported,
    (so we parse only boolean and relational operations
    that produce boolean expressions) *)
    match z3e_kind with
    | Z3enums.OP_AND -> And, TBool
    | Z3enums.OP_OR -> Or, TBool
    | Z3enums.OP_NOT -> Not, TBool
    | Z3enums.OP_EQ -> Eq, TBool
    | _ -> error "unexpected z3e_kind"
  ;;

  let rec exp_of_z3e z3e =
    !dprint_endline (sprintf "converting: %s\n" (Expr.to_string z3e));
    (* convert the z3 expression back into a lucid expression for an if statement *)
    (* this function is a stopgap until we add a boolean elimination pass, which will
      allow us to translate the z3_exp directly into a match statement *)
    (* the expressions that come out of z3 are either variables or operations *)
    let is_var = Expr.is_const z3e in
    match is_var with
    (* lookup expression for variable string in context *)
    | true -> CL.assoc (Expr.to_string z3e) !var_ctx
    (* translate operation, recursing on arguments *)
    | false ->
      (* !(x == y)* --> *)
      let z3e_kind = Expr.get_func_decl z3e |> FuncDecl.get_decl_kind in
      let op, rty = op_and_rty_of_kind z3e_kind in
      let arg_exps = Expr.get_args z3e |> CL.map exp_of_z3e in
      (* convert !(x == y)* --> x != y *)
      (match op, arg_exps with
      | Not, [{ e = EOp (Eq, inner_args); espan; ety }] ->
        { e = EOp (Neq, inner_args); espan; ety }
      (* need to fold and / or ops into a tree *)
      | _ -> fold_commutative_eop (eop_ty op arg_exps (ty rty)))
  ;;

  (* convert a list of disjunctions into a lucid expression *)
  let rec exp_of_disjunct_z3es z3es =
    match z3es with
    | [z3e] -> exp_of_z3e z3e
    | z3e :: z3es ->
      eop_ty Or [exp_of_z3e z3e; exp_of_disjunct_z3es z3es] (ty TBool)
    | [] -> error "unexpected: empty disjunction list."
  ;;

  let dnf_of_if if_stmt =
    let exp, s1, s2, sspan = unpack_if if_stmt in
    !dprint_endline
      (sprintf
         "converting expression to dnf form: %s\n"
         (Printing.exp_to_string exp));
    let ctx = z3_init () in
    let z3_ex = z3_from_expr ctx exp in
    let disjunct_z3es = dnf_of_boolexp ctx z3_ex in
    !dprint_endline
      (sprintf
         "converted expression into a disjunction of %i exprs\n"
         (List.length disjunct_z3es));
    List.iter (fun de -> printf "exp: %s\n" (Expr.to_string de)) disjunct_z3es;
    let new_exp = exp_of_disjunct_z3es disjunct_z3es in
    !dprint_endline
      (sprintf "old expression:  %s\n" (Printing.exp_to_string exp));
    !dprint_endline
      (sprintf "new expression:  %s\n" (Printing.exp_to_string new_exp));
    !dprint_endline
      (sprintf "new expression show: %s\n" (Printing.exp_to_string new_exp));
    let if_stmt = sifte_sp new_exp s1 s2 sspan in
    if_stmt
  ;;

  let dnf_of_ifs ds =
    let v =
      object
        inherit [_] s_map as super

        (* skip memops! *)
        method! visit_DMemop _ id body = DMemop (id, body)

        method! visit_statement ctx stmt =
          match stmt with
          | { s = SIf _; _ } -> dnf_of_if stmt
          | _ -> super#visit_statement ctx stmt
      end
    in
    v#visit_decls () ds
  ;;
end

let do_passes ds =
  DBG.start_mlog __FILE__ outc dprint_endline;
  (* normalize the relational operations inside of if statements, so
     that each atomic boolean is a relational operation of the form
     <var> <!=, ==> <const> *)
  trans_info "Normalizing relational ops...";
  let ds = NormalizeRelops.normalize_ifs ds in
  (* put every if expression into dnf (an "or" of "ands") *)
  trans_info "Normalizing if expressions...";
  let ds = NormalizeBoolExps.dnf_of_ifs ds in
  ds
;;
