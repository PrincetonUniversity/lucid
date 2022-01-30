open Syntax
open SyntaxUtils
open Collections
open Batteries
open TyperUtil

(** Inference and well-formedness for memops *)

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
    | EVar _ -> seen_mem, seen_local
    | EOp (op, [e1; e2]) ->
      if allowed_op op
      then (
        let seen_vars = aux (seen_mem, seen_local) e1 in
        aux seen_vars e2)
      else
        error_sp
          e.espan
          ("Disallowed operation in memop expression" ^ Printing.exp_to_string e)
    | _ ->
      error_sp
        e.espan
        ("Disallowed expression in memop expression: "
        ^ Printing.exp_to_string e)
  in
  ignore @@ aux (false, false) exp
;;

let allowed_bool_op = function
  | Eq | Neq | Less | More -> true
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
   expression, plus constant/symbolic variables *)
let check_conditional allowed_var e =
  let rec aux e =
    match e.e with
    | EVal _ -> ()
    | EVar cid ->
      if not (allowed_var cid)
      then
        error_sp e.espan "Disallowed variable in memop conditional expression"
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

type simple_body =
  | Return of exp
  | If of exp * exp * exp

(* Boolean condition * return value *)
type conditional_return = exp * exp

type complex_body =
  { b1 : exp option
  ; b2 : exp option
  ; cell1 : conditional_return option * conditional_return option
  ; cell2 : conditional_return option * conditional_return option
  ; ret : conditional_return option
  }

type memop =
  | TwoArg of simple_body
  | ThreeArg of complex_body
  | FourArg of complex_body

let extract_simple_body mem1 local1 body =
  let check_int = check_int_exp [mem1] [local1] in
  let check_bool = check_int_exp [mem1] [local1] in
  match flatten_stmt body with
  | [{ s = SRet (Some e) }] ->
    check_int e;
    Return e
  | [{ s = SIf (e, s1, s2) }] ->
    check_bool e;
    (match flatten_stmt s1, flatten_stmt s2 with
    | [{ s = SRet (Some e1) }], [{ s = SRet (Some e2) }] ->
      check_int e1;
      check_int e2;
      If (e, e1, e2)
    | _ ->
      error_sp body.sspan "Invalid if statement in a memop with two arguments")
  | _ -> error_sp body.sspan "Invalid form for a memop with two arguments"
;;

(* Simplified representation of the statements in the body, as an intermediate form
   while putting together a complex_body *)
type complex_stmt =
  | BoolDef of id * exp
  | CellAssign of id * conditional_return option * conditional_return option
  | LocalRet of conditional_return

let classify_stmts (body : statement) : (complex_stmt * sp) list =
  let classify s =
    let ret =
      match s.s with
      | SLocal (id, { raw_ty = TBool }, e) -> BoolDef (id, e)
      | SIf (test1, s1, s2) ->
        begin
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
                  @@ "If a complex memop has an if/else, both parts must \
                      assign to the same cell variable, not "
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

let cell1_id = Id.create "cell1"
let cell2_id = Id.create "cell2"

(* Ensure that each cell id appears at most once, and no invalid ids are used *)
let check_cell_ids stmts =
  let counts = (* Seen cell1, Seen cell2 *) ref (false, false) in
  List.iter
    (function
      | BoolDef _, _ | LocalRet _, _ -> ()
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
  let check_int = check_int_exp mems locals in
  let check_bool = check_int_exp mems locals in
  let body = classify_stmts body in
  check_cell_ids body;
  let extract_booldef b_ids body =
    match body with
    | (BoolDef (id, e), _) :: tl ->
      check_bool e;
      Some e, id :: b_ids, tl
    | _ -> None, b_ids, body
  in
  let b1, b_ids, body = extract_booldef [] body in
  let b2, b_ids, body = extract_booldef b_ids body in
  let check_cond e =
    check_conditional
      (function
        | Compound _ -> false
        | Id id -> List.mem id b_ids)
      e
  in
  let cell1, cell2, body =
    match body with
    | (CellAssign (id1, cr1_1, cr1_2), _)
      :: (CellAssign (_, cr2_1, cr2_2), _) :: tl ->
      if Id.name id1 = Id.name cell1_id
      then (cr1_1, cr1_2), (cr2_1, cr2_2), tl
      else (cr2_1, cr2_2), (cr1_1, cr1_2), tl
    | (CellAssign (id1, cr1_1, cr1_2), _) :: tl ->
      if Id.name id1 = Id.name cell1_id
      then (cr1_1, cr1_2), (None, None), tl
      else (None, None), (cr1_1, cr1_2), tl
    | _ -> (None, None), (None, None), body
  in
  let ret =
    match body with
    | [] -> None
    | [(LocalRet cr, _)] -> Some cr
    | (_, sp) :: _ ->
      error_sp sp
      @@ "Unexpected statement in a memop. Are you sure your statements are in \
          order?"
  in
  { b1; b2; cell1; cell2; ret }
;;

let ensure_same_size span params =
  if List.is_empty params
  then Console.error_position span "A memop cannot have 0 arguments!";
  let sizes =
    List.map
      (fun (_, ty) ->
        match ty.raw_ty with
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
let extract_memop span (params : params) (body : statement) : memop =
  ensure_same_size span params;
  match params with
  | [(mem1, _); (local1, _)] -> TwoArg (extract_simple_body mem1 local1 body)
  | [(mem1, _); (local1, _); (local2, _)] ->
    ThreeArg (extract_complex_body [mem1] [local1; local2] body)
  | [(mem1, _); (mem2, _); (local1, _); (local2, _)] ->
    FourArg (extract_complex_body [mem1; mem2] [local1; local2] body)
  | _ -> failwith ""
;;
