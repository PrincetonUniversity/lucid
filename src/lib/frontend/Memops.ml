open Syntax
open SyntaxUtils
open Collections
open Batteries

(** Inference and well-formedness for memops *)

(* Builtin ids for the memory cells. *)
let cell1_id = Builtins.cell1_id
let cell2_id = Builtins.cell2_id
let error_sp = Console.error_position

(* Validate that the expression only uses one of each kind of variable (memory/local),
   doesn't use any other variables, and only uses allowed operations *)
let check_e memvars localvars allowed_op exp =
  let mem cid lst =
    match cid with
    | Compound _ -> false
    | Id id ->
      (match List.find_opt (Id.equals id) lst with
       | None -> false
       | Some _ -> true)
  in
  let rec aux (seen_mem, seen_local) e =
    match e.e with
    | EVal _ | EInt _ -> seen_mem, seen_local
    | EVar cid when mem cid memvars ->
      if not seen_mem
      then true, seen_local
      else
        error_sp
          exp.espan
          "Second use of a memory parameter in a single memop expression"
    | EVar cid when mem cid localvars ->
      if not seen_local
      then seen_mem, true
      else
        error_sp
          exp.espan
          "Second use of a local parameter in a single memop expression"
    | EVar cid ->
      if Cid.equal cid (Id cell1_id) || Cid.equal cid (Id cell2_id)
      then
        error_sp exp.espan
        @@ "Cannot use cell1 and cell2 in an expression, except as the final \
            return value"
      else seen_mem, seen_local
    | EOp (op, [e1; e2]) ->
      if allowed_op op
      then (
        let seen_vars = aux (seen_mem, seen_local) e1 in
        aux seen_vars e2)
      else
        error_sp
          e.espan
          ("Disallowed operation in memop expression: "
          ^ Printing.exp_to_string e)
    | _ ->
      error_sp
        e.espan
        ("Disallowed expression in memop expression: "
        ^ Printing.exp_to_string e)
  in
  ignore @@ aux (false, false) exp
;;

let allowed_bool_op = function
  | Eq | Neq | Less | More | And | Or | Not | Geq | Leq -> true
  | _ -> false
;;

let allowed_int_op = function
  | Plus | Sub | BitAnd | BitOr -> true
  | _ -> false
;;

let check_bool_exp memvars localvars e =
  (* We might have things like `mem1 + local1 <= 3` *)
  let allowed_op b = allowed_bool_op b || allowed_int_op b in
  check_e memvars localvars allowed_op e
;;

let check_int_exp memvars localvars e =
  check_e memvars localvars allowed_int_op e
;;

(* Conditional expressions in complex memops have different requirements: they
   only allow combinations of the two booleans defined at the beginning of the
   expression, plus constant/symbolic variables. Since the user can't define any
   other local variables, this is equivalent to saying they're not allowed to
   use any of the memop parameters *)
let check_conditional param_ids e =
  let allowed_var = function
    | Compound _ -> true
    | Id id ->
      (match List.find_opt (Id.equal id) param_ids with
       | None -> true
       | Some _ -> false)
  in
  let rec aux e =
    match e.e with
    | EVal _ -> ()
    | EVar cid ->
      if not (allowed_var cid)
      then
        error_sp e.espan "Disallowed variable in memop conditional expression"
    | EOp (Not, [e1]) -> aux e1
    | EOp (op, [e1; e2]) ->
      if not (allowed_bool_op op)
      then
        error_sp
          e.espan
          ("Disallowed operation in memop expression" ^ Printing.exp_to_string e);
      aux e1;
      aux e2
    | _ ->
      error_sp
        e.espan
        ("Disallowed expression in memop expression: "
        ^ Printing.exp_to_string e)
  in
  aux e
;;

let extract_simple_body mem1 local1 body =
  let check_int = check_int_exp [mem1] [local1] in
  let check_bool = check_bool_exp [mem1] [local1] in
  match flatten_stmt body with
  | [{ s = SRet (Some e) }] ->
    check_int e;
    MBReturn e
  | [{ s = SIf (e, s1, s2) }] ->
    check_bool e;
    (match flatten_stmt s1, flatten_stmt s2 with
     | [{ s = SRet (Some e1) }], [{ s = SRet (Some e2) }] ->
       check_int e1;
       check_int e2;
       MBIf (e, e1, e2)
     | _ ->
       error_sp body.sspan "Invalid if statement in a memop with two arguments")
  | _ -> error_sp body.sspan "Invalid form for a memop with two arguments"
;;

(* Simplified representation of the statements in the body, as an intermediate form
   while putting together a complex_body *)
type complex_stmt =
  | BoolDef of id * exp
  | CellAssign of id * conditional_return option * conditional_return option
  | ExternCall of cid * exp list
  | LocalRet of conditional_return

let classify_stmts (body : statement) : (complex_stmt * sp) list =
  let classify s =
    let ret =
      match s.s with
      | SLocal (id, { raw_ty = TBool }, e) -> BoolDef (id, e)
      | SUnit { e = ECall (cid, es, _) } -> ExternCall (cid, es)
      | SIf (test1, s1, s2) -> begin
        match flatten_stmt s1, flatten_stmt s2 with
        | [{ s = SRet (Some ret) }], [] -> LocalRet (test1, ret)
        | [{ s = SAssign (id1, ret1) }], [] ->
          CellAssign (id1, Some (test1, ret1), None)
        | [{ s = SAssign (id1, ret1) }], [{ s = SIf (test2, s1', s2') }] ->
        begin
          match flatten_stmt s1', flatten_stmt s2' with
          | [{ s = SAssign (id2, ret2) }], [] ->
            if not (Id.equal id1 id2)
            then
              error_sp s1.sspan
              @@ "If a complex memop has an if/else, both parts must assign to \
                  the same cell variable, not "
              ^ Id.name id1
              ^ " and "
              ^ Id.name id2;
            CellAssign (id1, Some (test1, ret1), Some (test2, ret2))
          | _ -> error_sp s.sspan "Invalid if statement in complex memop"
        end
        | _ -> error_sp s.sspan "Invalid if statement in complex memop"
      end
      | _ -> error_sp s.sspan "Invalid statement type in complex memop"
    in
    ret, s.sspan
  in
  body |> flatten_stmt |> List.map classify
;;

(* Ensure that each cell id appears at most once, and no invalid ids are used *)
let check_cell_ids stmts =
  let counts = (* Seen cell1, Seen cell2 *) ref (false, false) in
  List.iter
    (function
     | BoolDef _, _ | LocalRet _, _ | ExternCall _, _ -> ()
     | CellAssign (id, _, _), sp when Id.equal id cell1_id ->
       if fst !counts
       then
         error_sp sp
         @@ Id.name cell1_id
         ^ " is assigned to in multiple parts of this memop";
       counts := true, snd !counts
     | CellAssign (id, _, _), sp when Id.equal id cell2_id ->
       if snd !counts
       then
         error_sp sp
         @@ Id.name cell2_id
         ^ " is assigned to in multiple parts of this memop";
       counts := fst !counts, true
     | CellAssign (id, _, _), sp ->
       error_sp sp
       @@ Id.name id
       ^ " is not a valid cell identifier (should be cell1 or cell2)")
    stmts
;;

let extract_complex_body mems locals body =
  let check_bool = check_bool_exp mems locals in
  let check_int = check_int_exp mems locals in
  let check_cond = check_conditional (mems @ locals) in
  let check_assign_cr cro =
    let check (cond, e) =
      check_cond cond;
      check_int e
    in
    ignore @@ Option.map check cro
  in
  let check_return_cr (cond, e) =
    check_cond cond;
    match e.e with
    | EVar (Id id)
      when Id.equal id cell1_id
           || Id.equal id cell2_id
           || List.exists (Id.equal id) mems -> ()
    | _ ->
      error_sp e.espan
      @@ "The final return value of a memop may only be one of the variables \
          'cell1', 'cell2', or one of the memory variables"
  in
  let body = classify_stmts body in
  check_cell_ids body;
  let extract_booldef body =
    match body with
    | (BoolDef (id, e), _) :: tl ->
      check_bool e;
      Some (id, e), tl
    | _ -> None, body
  in
  let b1, body = extract_booldef body in
  let b2, body = extract_booldef body in
  let cell1, cell2, body =
    match body with
    | (CellAssign (id1, cr1_1, cr1_2), _)
      :: (CellAssign (_, cr2_1, cr2_2), _)
      :: tl ->
      List.iter check_assign_cr [cr1_1; cr1_2; cr2_1; cr2_2];
      if Id.name id1 = Id.name cell1_id
      then (cr1_1, cr1_2), (cr2_1, cr2_2), tl
      else (cr2_1, cr2_2), (cr1_1, cr1_2), tl
    | (CellAssign (id1, cr1_1, cr1_2), _) :: tl ->
      List.iter check_assign_cr [cr1_1; cr1_2];
      if Id.name id1 = Id.name cell1_id
      then (cr1_1, cr1_2), (None, None), tl
      else (None, None), (cr1_1, cr1_2), tl
    | _ -> (None, None), (None, None), body
  in
  let extern_calls, body =
    List.span
      (function
       | ExternCall _, _ -> true
       | _ -> false)
      body
  in
  let extern_calls =
    List.map
      (function
       | ExternCall (cid, es), _ -> cid, es
       | _ -> failwith "impossible")
      extern_calls
  in
  let ret =
    match body with
    | [] -> None
    | [(LocalRet cr, _)] ->
      check_return_cr cr;
      Some cr
    | (_, sp) :: _ ->
      error_sp sp
      @@ "Unexpected statement in a memop. Are you sure your statements are in \
          order?"
  in
  { b1; b2; cell1; cell2; extern_calls; ret }
;;

let ensure_same_size span params =
  if List.is_empty params
  then Console.error_position span "A memop cannot have 0 arguments!";
  let sizes =
    List.map
      (fun (_, ty) ->
        match TyTQVar.strip_links ty.raw_ty with
        | TInt sz -> sz
        | _ ->
          Console.error_position
            ty.tspan
            "All arguments to a memop must be integers")
      params
  in
  if not (List.for_all (equiv_size (List.hd sizes)) sizes)
  then
    Console.error_position
      span
      " All arguments to a memop must have the same size"
;;

(* Verify that a memop is well-formed, and turn it from a statement into a memop.
   There are three kinds of memop (see Language Features on the wiki for details),
   which can be distinguished by the number of arguments they take. *)
let extract_memop span (params : params) (body : statement) : memop_body =
  ensure_same_size span params;
  List.iter
    (fun (id, _) ->
      if Id.equal id cell1_id || Id.equal id cell2_id
      then error_sp span "Arguments to a memop may not be named cell1 or cell2")
    params;
  match params with
  | [(mem1, _); (local1, _)] -> extract_simple_body mem1 local1 body
  | [(mem1, _); (local1, _); (local2, _)] ->
    MBComplex (extract_complex_body [mem1] [local1; local2] body)
  | [(mem1, _); (mem2, _); (local1, _); (local2, _)] ->
    MBComplex (extract_complex_body [mem1; mem2] [local1; local2] body)
  | _ -> error_sp span "A memop must have exactly 2, 3, or 4 arguments"
;;
