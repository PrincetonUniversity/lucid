open Batteries
open CoreSyntax
open Collections

type level = int

type var_binding =
  { level : level (* Last level this variable was modified at *)
  ; body : exp option
        (* This is Some e iff we know for sure the variable is bound to e *)
  ; is_declared : bool
  ; declared_as : (ty * exp) option (* Original declaration value *)
  }

type env = var_binding IdMap.t

let print_env env =
  print_endline "{";
  IdMap.iter
    (fun id binding ->
      print_endline
      @@ Printf.sprintf
           "%s -> {l:%d; dec:%b; e:%s}"
           (CorePrinting.id_to_string id)
           binding.level
           binding.is_declared
           (match binding.body with
           | None -> "None"
           | Some e -> CorePrinting.exp_to_string e))
    env;
  print_endline "}"
;;

(* Merge the env after a branch of an if/match statement with the prior environment.
   Discard any variables that were defined inside the branch, and add declarations for
   any variables that were mutated *)
let merge_branches base_stmt level prior_env branch_envs =
  let decls_to_add = ref base_stmt in
  let merge =
    IdMap.merge (fun id ao bo ->
        match ao, bo with
        | None, _ -> (* Variable was declared inside the branch, discard *) None
        | _, None -> failwith "Sanity check: should be impossible"
        | Some x, Some y ->
          let is_declared =
            if x.is_declared
            then true
            else if x.level < y.level && x.level = level
            then (
              (* Was originally declared on this level, and modified in the branch *)
              let ty, body =
                match x.body with
                | Some e -> e.ety, e
                | None -> Option.get x.declared_as
              in
              decls_to_add := sseq (slocal id ty body) !decls_to_add;
              true)
            else false
          in
          let level =
            if x.level = y.level then (* Not modified *) x.level else level
          in
          let body =
            match x.body, y.body with
            | Some b1, Some b2 -> if equiv_exp b1 b2 then x.body else None
            | _ -> None
          in
          Some { x with level; is_declared; body })
  in
  let merged = List.fold_left merge prior_env branch_envs in
  !decls_to_add, merged
;;

(* True if it's safe to inline the variable. It's currently considered unsafe
   if the expression contains an array or hash call *)
let should_inline exp =
  (* Rather inefficient but we can optimize if we need to *)
  let v =
    object
      inherit [_] s_iter

      method! visit_ECall r cid _ =
        if Id.name (Cid.first_id cid) = "Array" then r := false

      method! visit_EHash r _ _ = r := false
    end
  in
  let r = ref true in
  v#visit_exp r exp;
  !r
;;

let mk_e v = EVal v

let raw_integer v =
  match v.v with
  | VInt i -> i
  | _ -> error "not integer"
;;

let raw_bool v =
  match v.v with
  | VBool b -> b
  | _ -> error "not boolean"
;;

let rec interp_exp env e =
  match e.e with
  | EVal _ -> e
  | EVar cid ->
    let binding = IdMap.find (Cid.to_id cid) env in
    (match binding.body with
    | None -> e
    | Some e' -> e')
  | ECall (cid, args) ->
    { e with e = ECall (cid, List.map (interp_exp env) args) }
  | EHash (sz, args) ->
    { e with e = EHash (sz, List.map (interp_exp env) args) }
  | EFlood e' -> { e with e = EFlood (interp_exp env e') }
  | EOp (op, args) -> { e with e = interp_op env op args }
  | ETableCreate(_) -> e

(* Mostly copied from InterpCore, could maybe merge the two functions *)
and interp_op env op args =
  let args = List.map (interp_exp env) args in
  (* We could be more aggressive about constant folding, since this won't catch
     e.g. 1 + (x + 1), but it will at least catch fully-constant expressions. *)
  match op, args with
  (* Boolean Ops *)
  | And, [{ e = EVal { v = VBool true } }; e]
  | Or, [{ e = EVal { v = VBool false } }; e]
  | And, [e; { e = EVal { v = VBool true } }]
  | Or, [e; { e = EVal { v = VBool false } }] -> e.e
  | And, [{ e = EVal { v = VBool false } }; _]
  | And, [_; { e = EVal { v = VBool false } }] -> vbool false |> mk_e
  | Or, [{ e = EVal { v = VBool true } }; _]
  | Or, [_; { e = EVal { v = VBool true } }] -> vbool true |> mk_e
  | Not, [{ e = EVal { v = VBool b } }] -> vbool (not b) |> mk_e
  (* Comparison ops *)
  | Eq, [e1; e2] when equiv_exp e1 e2 -> vbool true |> mk_e
  | Neq, [e1; e2] when equiv_exp e1 e2 -> vbool false |> mk_e
  | Eq, [{ e = EVal v1 }; { e = EVal v2 }] when not (equiv_value v1 v2) ->
    vbool false |> mk_e
  | Neq, [{ e = EVal v1 }; { e = EVal v2 }] when not (equiv_value v1 v2) ->
    vbool true |> mk_e
  | Less, [{ e = EVal v1 }; { e = EVal v2 }] ->
    vbool (Integer.lt (raw_integer v1) (raw_integer v2)) |> mk_e
  | More, [{ e = EVal v1 }; { e = EVal v2 }] ->
    vbool (Integer.lt (raw_integer v2) (raw_integer v1)) |> mk_e
  | Leq, [{ e = EVal v1 }; { e = EVal v2 }] ->
    vbool (Integer.leq (raw_integer v1) (raw_integer v2)) |> mk_e
  | Geq, [{ e = EVal v1 }; { e = EVal v2 }] ->
    vbool (Integer.geq (raw_integer v1) (raw_integer v2)) |> mk_e
  (* Arithmetic ops *)
  | Neg, [{ e = EOp (Neg, [e]) }] -> e.e
  | Neg, [_] -> failwith "Not actually supported since all ints are unsigned"
  | Cast size, [{ e = EVal v }] ->
    vinteger (Integer.set_size size (raw_integer v)) |> mk_e
  | Conc, [{ e = EVal v1 }; { e = EVal v2 }] ->
    let v1, v2 = raw_integer v1, raw_integer v2 in
    let sz = Integer.size v1 + Integer.size v2 in
    let v1', v2' = Integer.set_size sz v1, Integer.set_size sz v2 in
    vinteger (Integer.add (Integer.shift_left v1' (Integer.size v2)) v2')
    |> mk_e
  (* Zero right identity *)
  | ( (Plus | SatPlus | Sub | SatSub | LShift | RShift | BitOr | BitXor)
    , [{ e = EVal { v = VInt n } }; e2] )
    when Integer.is_zero n -> e2.e
  (* Zero left identity *)
  | (Plus | SatPlus | BitOr | BitXor), [e1; { e = EVal { v = VInt n } }]
    when Integer.is_zero n -> e1.e
  (* Subtract from zero *)
  | (Sub | SatSub), [e1; { e = EVal { v = VInt n } }] when Integer.is_zero n ->
    (* Could replace with Neg but not sure that actually helps anything *)
    (* interp_op env Neg [e1] *)
    ignore e1;
    EOp (op, args)
  (* Zero left annihilator *)
  | (LShift | RShift | BitAnd), [{ e = EVal ({ v = VInt n } as v) }; _]
    when Integer.is_zero n -> v |> mk_e
  (* Zero right annihilator *)
  | BitAnd, [_; { e = EVal ({ v = VInt n } as v) }] when Integer.is_zero n ->
    v |> mk_e
  (* BitOr with max val -- two branches to suppress a warning *)
  | BitOr, [_; { e = EVal ({ v = VInt n } as v) }]
    when Integer.equal n (Integer.create ~value:(-1) ~size:(Integer.size n)) ->
    { v with v = VInt (Integer.create ~value:(-1) ~size:(Integer.size n)) }
    |> mk_e
  | BitOr, [{ e = EVal ({ v = VInt n } as v) }; _]
    when Integer.equal n (Integer.create ~value:(-1) ~size:(Integer.size n)) ->
    { v with v = VInt (Integer.create ~value:(-1) ~size:(Integer.size n)) }
    |> mk_e
  (* BitAnd with max val *)
  | BitAnd, [e; { e = EVal { v = VInt n } }]
    when Integer.equal n (Integer.create ~value:(-1) ~size:(Integer.size n)) ->
    e.e
  | BitAnd, [{ e = EVal { v = VInt n } }; e]
    when Integer.equal n (Integer.create ~value:(-1) ~size:(Integer.size n)) ->
    e.e
  (* Regular operators on values *)
  | Plus, [{ e = EVal v1 }; { e = EVal v2 }] ->
    vinteger (Integer.add (raw_integer v1) (raw_integer v2)) |> mk_e
  | SatPlus, [{ e = EVal v1 }; { e = EVal v2 }] ->
    let res = Integer.add (raw_integer v1) (raw_integer v2) in
    if Integer.lt res (raw_integer v1)
    then
      vinteger
        (Integer.create ~value:(-1) ~size:(Integer.size (raw_integer v1)))
      |> mk_e
    else vinteger res |> mk_e
  | Sub, [{ e = EVal v1 }; { e = EVal v2 }] ->
    vinteger (Integer.sub (raw_integer v1) (raw_integer v2)) |> mk_e
  | SatSub, [{ e = EVal v1 }; { e = EVal v2 }] ->
    if Integer.lt (raw_integer v1) (raw_integer v2)
    then
      vinteger (Integer.create ~value:0 ~size:(Integer.size (raw_integer v1)))
      |> mk_e
    else vinteger (Integer.sub (raw_integer v1) (raw_integer v2)) |> mk_e
  | LShift, [{ e = EVal v1 }; { e = EVal v2 }] ->
    vinteger
      (Integer.shift_left (raw_integer v1) (raw_integer v2 |> Integer.to_int))
    |> mk_e
  | RShift, [{ e = EVal v1 }; { e = EVal v2 }] ->
    vinteger
      (Integer.shift_right (raw_integer v1) (raw_integer v2 |> Integer.to_int))
    |> mk_e
  | BitAnd, [{ e = EVal v1 }; { e = EVal v2 }] ->
    vinteger (Integer.bitand (raw_integer v1) (raw_integer v2)) |> mk_e
  | BitOr, [{ e = EVal v1 }; { e = EVal v2 }] ->
    vinteger (Integer.bitor (raw_integer v1) (raw_integer v2)) |> mk_e
  | BitXor, [{ e = EVal v1 }; { e = EVal v2 }] ->
    vinteger (Integer.bitxor (raw_integer v1) (raw_integer v2)) |> mk_e
  | BitNot, [{ e = EVal v1 }] ->
    vinteger (Integer.bitnot (raw_integer v1)) |> mk_e
  | Slice (hi, lo), [{ e = EVal v1 }] ->
    vinteger
      (Integer.shift_right (raw_integer v1) lo |> Integer.set_size (hi - lo + 1))
    |> mk_e
  | ( ( Not
      | Neg
      | BitNot
      | And
      | Or
      | Eq
      | Neq
      | Less
      | More
      | Leq
      | Geq
      | Plus
      | Sub
      | SatPlus
      | SatSub
      | BitAnd
      | BitOr
      | BitXor
      | LShift
      | RShift
      | Conc
      | Cast _
      | Slice _ )
    , _ ) -> EOp (op, args)
;;

let interp_gen_ty env = function
  | GSingle eo -> GSingle (Option.map (interp_exp env) eo)
  | GMulti e -> GMulti (interp_exp env e)
  | GPort e -> GPort (interp_exp env e)
;;


let add_builtin_defs level vars env =
  List.fold_left
    (fun acc (id, _) ->
      IdMap.add
        id
        { level; body = None; is_declared = true; declared_as = None }
        acc)
    env
    vars
;;

(* Partially interpret a statement. Takes an environment and the current level
   (i.e. how deeply nested the scope is. Return the interpreted statment and the
   environment after interpreting it. *)
let rec interp_stmt env level s : statement * env =
  let interp_exp = interp_exp env in
  match s.s with
  | SNoop -> s, env
  | SUnit e -> { s with s = SUnit (interp_exp e) }, env
  | SPrintf (str, es) ->
    { s with s = SPrintf (str, List.map interp_exp es) }, env
  | SGen (g, e) -> { s with s = SGen (interp_gen_ty env g, interp_exp e) }, env
  | SRet eopt -> { s with s = SRet (Option.map interp_exp eopt) }, env
  | SSeq (s1, s2) ->
    let s1, env1 = interp_stmt env level s1 in
    let s2, env2 = interp_stmt env1 level s2 in
    let s' =
      if s1.s = SNoop then s2.s else if s2.s = SNoop then s1.s else SSeq (s1, s2)
    in
    { s with s = s' }, env2
  (* Cases where we bind/mutate variables *)
  | SLocal (id, ty, exp) ->
    let exp = interp_exp exp in
    let new_s, body, is_declared =
      if should_inline exp
      then snoop, Some exp, false
      else { s with s = SLocal (id, ty, exp) }, None, true
    in
    let new_binding =
      { level; body; is_declared; declared_as = Some (ty, exp) }
    in
    new_s, IdMap.add id new_binding env
  | SAssign (id, exp) ->
    let exp = interp_exp exp in
    let old_binding = IdMap.find id env in
    let new_s, new_binding =
      (* Using a match instead of if because it makes the code easier to read *)
      match should_inline exp with
      | true ->
        (* Can completely remove the statement if we're on the declaration level *)
        let new_s =
          if old_binding.level = level then SNoop else SAssign (id, exp)
        in
        new_s, { old_binding with level; body = Some exp }
      | false ->
        if old_binding.is_declared || old_binding.level <> level
        then SAssign (id, exp), { old_binding with level; body = None }
        else
          (* If we're on declaration level and it hasn't been declared yet,
             we can replace the assignment with a declaration instead *)
          ( SLocal (id, exp.ety, exp)
          , { old_binding with body = None; is_declared = true } )
    in
    { s with s = new_s }, IdMap.add id new_binding env
  (* Cases where we branch *)
  | SIf (test, s1, s2) ->
    let test = interp_exp test in
    (match test with
    | { e = EVal { v = VBool b } } ->
      if b then interp_stmt env level s1 else interp_stmt env level s2
    | _ ->
      let s1, env1 = interp_stmt env (level + 1) s1 in
      let s2, env2 = interp_stmt env (level + 1) s2 in
      let base_stmt = { s with s = SIf (test, s1, s2) } in
      merge_branches base_stmt level env [env1; env2])
  | SMatch (es, branches) ->
    let es = List.map interp_exp es in
    (* TODO: Precompute the match as much as possible *)
    let branches, envs =
      List.map
        (fun (p, stmt) ->
          let stmt', env' = interp_stmt env (level + 1) stmt in
          (p, stmt'), env')
        branches
      |> List.split
    in
    let base_stmt = { s with s = SMatch (es, branches) } in
    merge_branches base_stmt level env envs
  | STableMatch(tm) -> 
    let keys' = List.map interp_exp tm.keys in
    let args' = List.map interp_exp tm.args in
    {s with s= STableMatch({tm with keys=keys'; args=args';})}, env
;;

let interp_body builtin_tys env (params, stmt) =
  let level = 1 in
  let builtins =
    let open Builtins in
    (ingr_port_id, builtin_tys.ingr_port_ty) :: (this_id, builtin_tys.this_ty) :: builtin_vars
  in
  let env =
    env |> add_builtin_defs level builtins |> add_builtin_defs level params
  in
  params, fst (interp_stmt env level stmt)
;;

let interp_decl builtin_tys env d =
  let add_dec env id =
    IdMap.add
      id
      { level = 0; body = None; is_declared = true; declared_as = None }
      env
  in
  match d.d with
  | DGlobal (id, ty, e) ->
    let e = interp_exp env e in
    let env = add_dec env id in
    env, { d with d = DGlobal (id, ty, e) }
  | DMemop (id, params, body) ->
    let env = add_dec env id in
    (* TODO: Maybe interp in the body? Maybe that's handled later? *)
    env, { d with d = DMemop (id, params, body) }
  | DHandler (id, body) ->
    let env = add_dec env id in
    env, { d with d = DHandler (id, interp_body builtin_tys env body) }
  | DEvent (id, _, _) | DExtern (id, _) ->
    let env = add_dec env id in
    env, d
  | DAction(acn) -> 
    let env = add_dec env acn.aid in
    let level = 1 in 
    let acn_env = 
      env 
      |> add_builtin_defs level acn.aconst_params
      |> add_builtin_defs level acn.aparams
    in
    let abody = List.map (interp_exp acn_env) acn.abody in
    env, {d with d = DAction({acn with abody})}
;;

let interp_prog ds = ds
(* FIXME: In addition to keeping declarations for variables which are mutated, we also
   need to keep mutations which might or might not get overwritten later.
   For example, the following program prints 8 when in(1,2) is called, but should
   print 10:

   event in(int<<8>> i, int<<8>> j) {
     int<<'a>> k = 2;
     if (j == 2) { k = 3; }
     i = k;
     k = 5;
     if (j == 7) { k = 4; }
     printf("%d", i+j + k);
   }
*)
(* let builtins =
    let open Builtins in
    [recirc_id, recirc_ty; self_id, self_ty]
  in
  let env = ref (IdMap.empty |> add_builtin_defs 0 builtins) in
  List.map
    (fun d ->
      let env', d = interp_decl !env d in
      env := env';
      d)
    ds *)
