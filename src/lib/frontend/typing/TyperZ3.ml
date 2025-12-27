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

(* To stop queries from getting stuck on
      Z3=4.8.13-14, we: (1) make a new context
      for each query; (2) try multiple logics
      for each query. -jsonch 6/22 *)

type context =
  { ctx : Z3.context
  ; (* Maps Lucid variables to (Z3 Array variable * Z3 length variable) *)
    mutable variables : (Z3.Expr.expr * Z3.Expr.expr) IdMap.t
  }

let z3_global_cfg =
  (* solver 2 is the legacy solver, which seems to have a higher chance of success *)
  Z3.set_global_param "smt.arith.solver" "2"
(* Z3.set_global_param "parallel.enable" "true"; *)
(* Z3.set_global_param "model.compact" "false"; *)
(* Z3.set_global_param "smt.arith.propagation_mode" "2"; *)
(* Z3.set_global_param "verbose" "1"; *)
(* Z3.set_global_param "smt.arith.print_stats" "true"; *)
;;

z3_global_cfg

let cfg = ["model", "true"; "proof", "false"; "timeout", "10000"]
let new_context () = { ctx = mk_context cfg; variables = IdMap.empty }

let set_timeout ctx ms =
  Z3.Params.update_param_value ctx.ctx "timeout" (string_of_int ms)
;;

let make_forall ctx idx_exp body =
  Quantifier.mk_forall_const ctx.ctx [idx_exp] body None [] [] None None
  |> Quantifier.expr_of_quantifier
;;

type effect_base =
  | BZero
  | BVar of id

let unwrap e =
  (* Ignore cids when encoding into Z3. Not sure if it'll be complete,
     and we'll be checking it for all values later anyway. *)
  match unwrap_effect e with
  | FZero, lst -> BZero, List.map snd lst
  | (FVar (QVar id) | FVar (TVar { contents = Unbound (id, _) })), lst ->
    BVar id, List.map snd lst
  | _ -> failwith "Impossible"
;;

let id_to_z3vars ctx id =
  try IdMap.find id ctx.variables with
  | _ ->
    let array_var =
      Z3Array.mk_const_s
        ctx.ctx
        (Id.to_string id)
        (Z3Int.mk_sort ctx.ctx)
        (Z3Int.mk_sort ctx.ctx)
    in
    let len_var = Z3Int.mk_const_s ctx.ctx (Id.to_string id ^ "_len") in
    ctx.variables <- IdMap.add id (array_var, len_var) ctx.variables;
    array_var, len_var
;;

let select_from_const ctx (lst : int list) idx_exp =
  (* if i = 0 then hd lst else
     if i = 1 then hd (tl lst) else
     ...
     if i >= List.length lst then -1 *)
  let base = Z3Int.mk_numeral_i ctx.ctx (-1) in
  List.fold_righti
    (fun idx elt acc ->
      Boolean.mk_ite
        ctx.ctx
        (Boolean.mk_eq ctx.ctx idx_exp (Z3Int.mk_numeral_i ctx.ctx idx))
        (Z3Int.mk_numeral_i ctx.ctx elt)
        acc)
    lst
    base
;;

let select_from_var ctx var_exp len_exp const_lst idx_exp =
  let select = Z3Array.mk_select ctx.ctx var_exp idx_exp in
  if List.is_empty const_lst
  then select
  else (
    (* if i < len - 1 then select(lst, i) else
       if i = len - 1 then select(lst, i) + hd (const_lst) else
       select_from_const(tl const_lst, i-len) *)
    let len_minus_1 =
      Arithmetic.mk_sub ctx.ctx [len_exp; Z3Int.mk_numeral_i ctx.ctx 1]
    in
    Boolean.mk_ite ctx.ctx (Arithmetic.mk_lt ctx.ctx idx_exp len_minus_1) select
    @@ Boolean.mk_ite
         ctx.ctx
         (Boolean.mk_eq ctx.ctx idx_exp len_minus_1)
         (Arithmetic.mk_add
            ctx.ctx
            [select; Z3Int.mk_numeral_i ctx.ctx (List.hd const_lst)])
         (select_from_const
            ctx
            (List.tl const_lst)
            (Arithmetic.mk_sub ctx.ctx [idx_exp; len_exp])))
;;

let select ctx (base, lst) idx_exp =
  match base with
  | BZero -> select_from_const ctx lst idx_exp
  | BVar id ->
    let var_exp, len_exp = id_to_z3vars ctx id in
    select_from_var ctx var_exp len_exp lst idx_exp
;;

let get_length ctx (base, lst) =
  match base with
  | BZero -> Z3Int.mk_numeral_i ctx.ctx (List.length lst)
  | BVar id ->
    let _, len_exp = id_to_z3vars ctx id in
    Arithmetic.mk_add
      ctx.ctx
      [len_exp; Z3Int.mk_numeral_i ctx.ctx (List.length lst)]
;;

let encode_constraint ctx constr =
  match constr with
  | CLeq (eff1, eff2) ->
    (match unwrap eff1, unwrap eff2 with
     | (BZero, lst1), (BZero, lst2) ->
       Boolean.mk_val ctx.ctx (Pervasives.compare lst1 lst2 <= 0)
     | e1, e2 ->
       (* Exists i. (* Note: we skolemize out the exists *)
         select(e1, i) < select(e2, i) &&
         forall j. j < i =>  select(e1, j) = select(e2, j)
      *)
       let idx_var = Z3Int.mk_const_s ctx.ctx (Id.to_string (Id.fresh "i")) in
       let select_i =
         Arithmetic.mk_lt
           ctx.ctx
           (select ctx e1 idx_var)
           (select ctx e2 idx_var)
       in
       let j_var = Z3Int.mk_const_s ctx.ctx (Id.to_string (Id.fresh "j")) in
       let select_j =
         Boolean.mk_eq ctx.ctx (select ctx e1 j_var) (select ctx e2 j_var)
         |> Boolean.mk_implies ctx.ctx (Arithmetic.mk_lt ctx.ctx j_var idx_var)
         |> make_forall ctx j_var
       in
       let equal =
         Boolean.mk_eq ctx.ctx (select ctx e1 j_var) (select ctx e2 j_var)
         |> make_forall ctx j_var
       in
       Boolean.mk_or ctx.ctx [equal; Boolean.mk_and ctx.ctx [select_i; select_j]])
;;

let encode_constraints ctx constraints =
  List.map (encode_constraint ctx) constraints
;;

(* For each list, make sure
   - each in-bounds list entry is nonnegative
   - each out-of-bounds entry is -1
   - the list is nonempty (length > 0) *)
let ensure_wellformed ctx =
  let vars = ref [] in
  IdMap.iter
    (fun _ (arr_var, len_var) ->
      let idx_var = Z3Int.mk_const_s ctx.ctx (Id.to_string (Id.fresh "w")) in
      let in_bounds =
        Boolean.mk_and
          ctx.ctx
          [ Arithmetic.mk_ge ctx.ctx idx_var (Z3Int.mk_numeral_i ctx.ctx 0)
          ; Arithmetic.mk_lt ctx.ctx idx_var len_var ]
      in
      let in_bound_body =
        Arithmetic.mk_ge
          ctx.ctx
          (select_from_var ctx arr_var len_var [] idx_var)
          (Z3Int.mk_numeral_i ctx.ctx 0)
      in
      let out_bound_body =
        Boolean.mk_eq
          ctx.ctx
          (select_from_var ctx arr_var len_var [] idx_var)
          (Z3Int.mk_numeral_i ctx.ctx (-1))
      in
      let forall =
        make_forall
          ctx
          idx_var
          (Boolean.mk_ite ctx.ctx in_bounds in_bound_body out_bound_body)
      in
      let nonempty =
        Arithmetic.mk_gt ctx.ctx len_var (Z3Int.mk_numeral_i ctx.ctx 0)
      in
      vars := !vars @ [forall; nonempty])
    ctx.variables;
  !vars
;;

let solved = ref 0

let check_sat_encoded ctx encoded_constraints =
  (* declare all the variables *)
  let vars = ensure_wellformed ctx in
  (* try multiple logics with timeouts from 100ms to 10s *)
  let logics =
    [ "QF_UFIDL"
    ; "QF_UF"
    ; "QF_RDL"
    ; "QF_IDL"
    ; "QF_LRA"
    ; "QF_LIA"
    ; "QF_UFLIA"
    ; "QF_UFLRA" (* ; "QF_AX" *)
    ; "QF_AUFLIA"
    ; "QF_BV"
    ; "QF_AUFBV"
    ; "QF_ABV"
    ; "QF_UFBV"
    ; "QF_BVRE"
    ; "AUFLIA"
    ; "AUFLIRA"
    ; "AUFNIRA"
    ; "UFNIA"
    ; "UFLRA"
    ; "LRA"
    ; "QF_FP"
    ; "QF_FPBV"
    ; "QF_BVFP"
    ; "QF_S"
    ; "QF_SLIA"
    ; "QF_DT" ]
  in
  let timeouts = [100; 1000; 10000; 100000] in
  let try_with_logic result logic =
    match result with
    | Solver.UNKNOWN ->
      if Config.cfg.show_queries then print_endline ("trying logic: " ^ logic);
      let z3solver = Solver.mk_solver_s ctx.ctx logic in
      Solver.add z3solver (vars @ encoded_constraints);
      let ret = Solver.check z3solver [] in
      if Config.cfg.show_queries
      then (
        print_endline @@ "Solver: " ^ Solver.to_string z3solver;
        match ret with
        | SATISFIABLE | UNSATISFIABLE -> print_endline "* Solver successful"
        | _ -> ());
      Gc.full_major ();
      ret
    | _ -> result
  in
  let try_with_timeout result timeout =
    match result with
    | Solver.UNKNOWN ->
      set_timeout ctx timeout;
      let ret = Caml.List.fold_left try_with_logic Solver.UNKNOWN logics in
      ret
    | _ -> result
  in
  let ret = Caml.List.fold_left try_with_timeout Solver.UNKNOWN timeouts in
  let res =
    match ret with
    | Solver.SATISFIABLE -> true
    | Solver.UNKNOWN ->
      failwith "Z3 returned unknown? What kind of program did you write!?"
    | Solver.UNSATISFIABLE -> false
  in
  solved := !solved + 1;
  if Config.cfg.show_queries
  then
    print_endline @@ "solved count = " ^ string_of_int !solved ^ " constraints";
  res
;;

let check_sat constraints =
  let constraints = prune_constraints constraints in
  if Config.cfg.show_constraints
  then
    print_endline
    @@ "Checking satisfiability of "
    ^ Printing.list_to_string Printing.constraint_to_string constraints;
  let ctx = new_context () in
  let encoded = encode_constraints ctx constraints in
  let ret = check_sat_encoded ctx encoded in
  ret
;;

exception NoMax

let find_max constraints eff1 eff2 =
  let constraints = prune_constraints constraints in
  (* If one is obviously bigger, return that *)
  match max_effect eff1 eff2 with
  | Some eff -> eff
  | None ->
    if Config.cfg.show_constraints
    then
      print_endline
      @@ Printf.sprintf
           "Finding maximum of %s and %s given %s]"
           (Printing.effect_to_string eff1)
           (Printing.effect_to_string eff2)
           (Printing.list_to_string Printing.constraint_to_string constraints);
    let ctx = new_context () in
    let encoded_constraints = encode_constraints ctx constraints in
    let encoded_1_2 =
      encode_constraints ctx [CLeq (eff1, eff2)] @ encoded_constraints
    in
    let encoded_2_1 =
      encode_constraints ctx [CLeq (eff2, eff1)] @ encoded_constraints
    in
    (match
       check_sat_encoded ctx encoded_1_2, check_sat_encoded ctx encoded_2_1
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
  let constrs1 = prune_constraints constrs1 in
  let constrs2 = prune_constraints constrs2 in
  if Config.cfg.show_constraints
  then
    print_endline
    @@ Printf.sprintf
         "Checking constraints %s imply %s"
         (Printing.list_to_string Printing.constraint_to_string constrs1)
         (Printing.list_to_string Printing.constraint_to_string constrs2);
  let ctx = new_context () in
  let negate = function
    | CLeq (e1, e2) -> CLeq (FSucc e2, e1)
  in
  let negate_constraints = function
    | [] -> [CLeq (FSucc FZero, FZero)]
    | lst -> List.map negate lst
  in
  let encoded1 = encode_constraints ctx constrs1 in
  let encoded2 =
    constrs2
    |> negate_constraints
    |> encode_constraints ctx
    |> Boolean.mk_or ctx.ctx
  in
  check_sat_encoded ctx (encoded2 :: encoded1) |> not
;;
