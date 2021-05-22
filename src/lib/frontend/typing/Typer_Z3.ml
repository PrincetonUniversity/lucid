open Batteries
open Syntax
open SyntaxUtils
open Collections
open Z3
module Z3Int = Arithmetic.Integer

(* Encoding strategy: We encode variables into Z3 as Array-sort Z3 variables.
   We enforce the well-formedness condition that all integer variables are
   nonnegative, as are all valid list entries. Attempting to access an element
   after the end of a list returns -1. *)

type solver =
  { ctx : Z3.context
  ; solver : Z3.Solver.solver
  ; (* Maps Lucid variables to (Z3 Array variable * Z3 length variable) *)
    mutable variables : (Z3.Expr.expr * Z3.Expr.expr) IdMap.t
  }

let default_ctx = mk_context ["auto_config", "true"]

let start_solver ctx =
  { ctx; solver = Solver.mk_simple_solver ctx; variables = IdMap.empty }
;;

let make_forall solver idx_exp body =
  Quantifier.mk_forall_const solver.ctx [idx_exp] body None [] [] None None
  |> Quantifier.expr_of_quantifier
;;

type effect_base =
  | BZero
  | BVar of id

let unwrap e =
  match unwrap_effect e with
  | FZero, lst -> BZero, lst
  | (FVar (QVar id) | FVar (TVar { contents = Unbound (id, _) })), lst ->
    BVar id, lst
  | _ -> failwith "Impossible"
;;

let id_to_z3vars solver id =
  try IdMap.find id solver.variables with
  | _ ->
    let array_var =
      Z3Array.mk_const_s
        solver.ctx
        (Id.to_string id)
        (Z3Int.mk_sort solver.ctx)
        (Z3Int.mk_sort solver.ctx)
    in
    let len_var = Z3Int.mk_const_s solver.ctx (Id.to_string id ^ "_len") in
    solver.variables <- IdMap.add id (array_var, len_var) solver.variables;
    array_var, len_var
;;

let select_from_const solver (lst : int list) idx_exp =
  (* if i = 0 then hd lst else
     if i = 1 then hd (tl lst) else
     ...
     if i >= List.length lst then -1 *)
  let base = Z3Int.mk_numeral_i solver.ctx (-1) in
  List.fold_righti
    (fun idx elt acc ->
      Boolean.mk_ite
        solver.ctx
        (Boolean.mk_eq solver.ctx idx_exp (Z3Int.mk_numeral_i solver.ctx idx))
        (Z3Int.mk_numeral_i solver.ctx elt)
        acc)
    lst
    base
;;

let select_from_var solver var_exp len_exp const_lst idx_exp =
  let select = Z3Array.mk_select solver.ctx var_exp idx_exp in
  if List.is_empty const_lst
  then select
  else (
    (* if i < len - 1 then select(lst, i) else
       if i = len - 1 then select(lst, i) + hd (const_lst) else
       select_from_const(tl const_lst, i-len) *)
    let len_minus_1 =
      Arithmetic.mk_sub solver.ctx [len_exp; Z3Int.mk_numeral_i solver.ctx 1]
    in
    Boolean.mk_ite
      solver.ctx
      (Arithmetic.mk_lt solver.ctx idx_exp len_minus_1)
      select
    @@ Boolean.mk_ite
         solver.ctx
         (Boolean.mk_eq solver.ctx idx_exp len_minus_1)
         (Arithmetic.mk_add
            solver.ctx
            [select; Z3Int.mk_numeral_i solver.ctx (List.hd const_lst)])
         (select_from_const
            solver
            (List.tl const_lst)
            (Arithmetic.mk_sub solver.ctx [idx_exp; len_exp])))
;;

let select solver (base, lst) idx_exp =
  match base with
  | BZero -> select_from_const solver lst idx_exp
  | BVar id ->
    let var_exp, len_exp = id_to_z3vars solver id in
    select_from_var solver var_exp len_exp lst idx_exp
;;

let get_length solver (base, lst) =
  match base with
  | BZero -> Z3Int.mk_numeral_i solver.ctx (List.length lst)
  | BVar id ->
    let _, len_exp = id_to_z3vars solver id in
    Arithmetic.mk_add
      solver.ctx
      [len_exp; Z3Int.mk_numeral_i solver.ctx (List.length lst)]
;;

let encode_constraint solver constr =
  match constr with
  | CLeq (eff1, eff2) ->
    (match unwrap eff1, unwrap eff2 with
    | (BZero, lst1), (BZero, lst2) ->
      Boolean.mk_val solver.ctx (Pervasives.compare lst1 lst2 <= 0)
    | e1, e2 ->
      (* Exists i. (* Note: we skolemize out the exists *)
         select(e1, i) < select(e2, i) &&
         forall j. j < i =>  select(e1, j) = select(e2, j)
      *)
      let idx_var = Z3Int.mk_const_s solver.ctx (Id.to_string (Id.fresh "i")) in
      let select_i =
        Arithmetic.mk_lt
          solver.ctx
          (select solver e1 idx_var)
          (select solver e2 idx_var)
      in
      let j_var = Z3Int.mk_const_s solver.ctx (Id.to_string (Id.fresh "j")) in
      let select_j =
        Boolean.mk_eq
          solver.ctx
          (select solver e1 j_var)
          (select solver e2 j_var)
        |> Boolean.mk_implies
             solver.ctx
             (Arithmetic.mk_lt solver.ctx j_var idx_var)
        |> make_forall solver j_var
      in
      let equal =
        Boolean.mk_eq
          solver.ctx
          (select solver e1 j_var)
          (select solver e2 j_var)
        |> make_forall solver j_var
      in
      Boolean.mk_or
        solver.ctx
        [equal; Boolean.mk_and solver.ctx [select_i; select_j]])
;;

let encode_constraints solver constraints =
  List.map (encode_constraint solver) constraints
;;

(* For each list, make sure
   - each in-bounds list entry is nonnegative
   - each out-of-bounds entry is -1
   - the list is nonempty (length > 0) *)
let ensure_wellformed solver =
  IdMap.iter
    (fun _ (arr_var, len_var) ->
      let idx_var = Z3Int.mk_const_s solver.ctx (Id.to_string (Id.fresh "w")) in
      let in_bounds =
        Boolean.mk_and
          solver.ctx
          [ Arithmetic.mk_ge solver.ctx idx_var (Z3Int.mk_numeral_i solver.ctx 0)
          ; Arithmetic.mk_lt solver.ctx idx_var len_var ]
      in
      let in_bound_body =
        Arithmetic.mk_ge
          solver.ctx
          (select_from_var solver arr_var len_var [] idx_var)
          (Z3Int.mk_numeral_i solver.ctx 0)
      in
      let out_bound_body =
        Boolean.mk_eq
          solver.ctx
          (select_from_var solver arr_var len_var [] idx_var)
          (Z3Int.mk_numeral_i solver.ctx (-1))
      in
      let forall =
        make_forall
          solver
          idx_var
          (Boolean.mk_ite solver.ctx in_bounds in_bound_body out_bound_body)
      in
      let nonempty =
        Arithmetic.mk_gt solver.ctx len_var (Z3Int.mk_numeral_i solver.ctx 0)
      in
      Solver.add solver.solver [forall; nonempty])
    solver.variables
;;

let check_sat_encoded solver encoded_constraints =
  Solver.push solver.solver;
  ensure_wellformed solver;
  Solver.add solver.solver encoded_constraints;
  if Cmdline.cfg.show_queries
  then print_endline @@ "Solver: " ^ Solver.to_string solver.solver;
  let ret =
    match Solver.check solver.solver [] with
    | SATISFIABLE -> true
    | UNKNOWN ->
      failwith "Z3 returned unknown? What kind of program did you write!?"
    | UNSATISFIABLE -> false
  in
  Solver.pop solver.solver 1;
  ret
;;

let check_sat constraints =
  if Cmdline.cfg.show_queries
  then
    print_endline
    @@ "Checking satisfiability of "
    ^ Printing.list_to_string Printing.constraint_to_string constraints;
  (* We could possibly get some performance gains by not restarting the solver
     every time this is called *)
  let solver = start_solver default_ctx in
  let encoded = encode_constraints solver constraints in
  check_sat_encoded solver encoded
;;

exception NoMax

let find_max constraints eff1 eff2 =
  (* If one is obviously bigger, return that *)
  match max_effect eff1 eff2 with
  | Some eff -> eff
  | None ->
    if Cmdline.cfg.show_queries
    then
      print_endline
      @@ Printf.sprintf
           "Finding maximum of %s and %s given %s]"
           (Printing.effect_to_string eff1)
           (Printing.effect_to_string eff2)
           (Printing.list_to_string Printing.constraint_to_string constraints);
    let solver = start_solver default_ctx in
    let encoded_constraints = encode_constraints solver constraints in
    let encoded_1_2 =
      encode_constraints solver [CLeq (eff1, eff2)] @ encoded_constraints
    in
    let encoded_2_1 =
      encode_constraints solver [CLeq (eff2, eff1)] @ encoded_constraints
    in
    (match
       ( check_sat_encoded solver encoded_1_2
       , check_sat_encoded solver encoded_2_1 )
     with
    | true, true ->
      (* Neither effect is necessarily larger than the other. Note that they
          can't be equal or max_effect would have caught it (I think). *)
      raise NoMax
    | true, false -> eff2
    | false, true -> eff1
    | false, false ->
      (* Constraints are UNSAT, doesn't matter what we return *) eff1)
;;

(* Check that constrs1 imply constrs2 *)
let check_implies constrs1 constrs2 =
  if Cmdline.cfg.show_queries
  then
    print_endline
    @@ Printf.sprintf
         "Checking constraints %s imply %s"
         (Printing.list_to_string Printing.constraint_to_string constrs1)
         (Printing.list_to_string Printing.constraint_to_string constrs2);
  let solver = start_solver default_ctx in
  let negate = function
    | CLeq (e1, e2) -> CLeq (FSucc e2, e1)
  in
  let encoded1 = encode_constraints solver constrs1 in
  let encoded2 =
    constrs2
    |> List.map negate
    |> encode_constraints solver
    |> Boolean.mk_or solver.ctx
  in
  check_sat_encoded solver (encoded2 :: encoded1) |> not
;;
