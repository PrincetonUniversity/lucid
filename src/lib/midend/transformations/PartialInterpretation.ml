open Batteries
open CoreSyntax
open Collections

(* The information we might have about the value of a variable during
   partial interpretation. A partial value might be:
   - A known value v
   - An unknown value, to which we assign a unique integer identifier.
     These represent event arguments, the result of Array/hash operations,
     and the result of modifying a variable inside a conditional.
   - An expression which we weren't able to fully evaluate because it
     contains one or more variables; the partial value includes both the
     expression itself and the value of each of those variables at the time.
     Storing the original variable values allows us to check if they have
     changed since the point in the program where the expression was written.
*)
(* TODO: We could probably make this simpler by just storing the time at
   which each variable was last modified, rather than storing the entire partial
   value for each dependency inside the PExp. The rule is then that we inline a
   PExp only if all its dependencies were not modified since the PExp was created. *)
type partial_value =
  | PValue of value
  | PUnknown of int
  | PExp of exp * (id * partial_value) list

let unknown_value_counter = ref (-1)

let new_unknown () =
  incr unknown_value_counter;
  PUnknown !unknown_value_counter
;;

let rec partial_value_to_string = function
  | PValue v -> CorePrinting.value_to_string v
  | PUnknown n -> "Unk" ^ string_of_int n
  | PExp (e, pvals) ->
    Printf.sprintf
      "%s where %s"
      (CorePrinting.exp_to_string e)
      (CorePrinting.list_to_string
         (fun (id, pval) ->
           Id.to_string id ^ " = " ^ partial_value_to_string pval)
         pvals)
;;

type env = partial_value IdMap.t

let print_env env =
  print_endline "{";
  IdMap.iter
    (fun id pval ->
      print_endline
      @@ Printf.sprintf
           "%s -> %s"
           (CorePrinting.id_to_string id)
           (partial_value_to_string pval))
    env;
  print_endline "}"
;;

(* Since partial values should only be equal if they are literal copies of each
   other, this function is probably overkill. But I'm writing it anyway just in
   case something changes later *)
let rec equiv_partial_value pv1 pv2 =
  match pv1, pv2 with
  | PValue v1, PValue v2 -> equiv_value v1 v2
  | PUnknown n1, PUnknown n2 -> n1 = n2
  | PExp (e1, pvs1), PExp (e2, pvs2) ->
    equiv_exp e1 e2
    && (* Note: This assumes the lists have the same order. We could sort beforehand.
          but that's probably overkill since it's very unlikely we'll have two equivalent
          expressions which only differ in list order. In fact, I think it's impossible. *)
    equiv_list
      (fun (id1, pv1) (id2, pv2) ->
        Id.equal id1 id2 && equiv_partial_value pv1 pv2)
      pvs1
      pvs2
  | (PValue _ | PUnknown _ | PExp _), _ -> false
;;

let add_builtin_defs vars env =
  List.fold_left (fun acc (id, _) -> IdMap.add id (new_unknown ()) acc) env vars
;;

(* Merge two environments from different branches of a program. Variables which
   only appear in one env are local to that branch, and so are discarded. Variables
   which have different values in the two envs are replaced with a fresh unknown
   value *)
let merge_two_envs env1 env2 =
  IdMap.merge
    (fun _ v1o v2o ->
      match v1o, v2o with
      | None, _ | _, None ->
        (* Variable was declared inside only one env, discard *) None
      | Some v1, Some v2 ->
        if equiv_partial_value v1 v2 then Some v1 else Some (new_unknown ()))
    env1
    env2
;;

let merge_envs envs = List.reduce merge_two_envs envs

let variable_extractor =
  object
    inherit [_] s_iter as super

    method! visit_EVar acc cid =
      if not (IdSet.mem (Cid.to_id cid) !acc)
      then acc := IdSet.add (Cid.to_id cid) !acc
  end
;;

(* Get a list of all the variables that appear in an expression *)
let extract_variables ?(acc = IdSet.empty) exp =
  let acc = ref acc in
  variable_extractor#visit_exp acc exp;
  !acc
;;

let contains_hash_or_call exp =
  let ret = ref false in
  let checker =
    object
      inherit [_] s_iter as super
      method! visit_EHash _ _ _ = ret := true
      method! visit_ECall _ _ _ _= ret := true
    end
  in
  checker#visit_exp () exp;
  !ret
;;

(* Create the appropriate type of partial value to represent an expression.
   Always return unknown for hashes and function values to avoid inlining them *)
let extract_partial_value (env : env) exp =
  if contains_hash_or_call exp
  then new_unknown ()
  else (
    match exp.e with
    | EVal v -> PValue v
    | EHash _ | ECall _ -> new_unknown ()
    | _ ->
      let vars = extract_variables exp in
      let var_values =
        List.map (fun id -> id, IdMap.find id env) (IdSet.to_list vars)
      in
      PExp (exp, var_values))
;;

(* Replace the variable with its definition in the environment, if allowable.
   A variable can be inlined if its definition is either:
   - a known value (PValue)
   - a PExp, all of whose dependencies are unchanged since its definition *)
let try_inline_variable env id =
  match IdMap.find id env with
  | PValue v -> Some (EVal v)
  | PUnknown _ -> None
  | PExp (e, pvs) ->
    if List.for_all
         (fun (id, pv) -> equiv_partial_value pv (IdMap.find id env))
         pvs
    then Some e.e
    else None
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
    (match try_inline_variable env (Cid.to_id cid) with
     | Some e' -> { e with e = e' }
     | None -> e)
  | ECall (cid, args, u) ->
    { e with e = ECall (cid, List.map (interp_exp env) args, u) }
  | EHash (sz, args) ->
    { e with e = EHash (sz, List.map (interp_exp env) args) }
  | EFlood e' -> { e with e = EFlood (interp_exp env e') }
  | EOp (op, args) -> { e with e = interp_op env op args }
  | ETableCreate _ -> e
  | ERecord(fields) -> {
    e with e = ERecord (List.map (fun (id, e) -> (id, interp_exp env e)) fields)
  }
  | EProj(rec_exp, id) -> (
    let rec_exp = interp_exp env rec_exp in
    match rec_exp.e with
    | ERecord(fields) -> interp_exp env (List.assoc id fields)
    | _ -> {e with e = EProj(rec_exp, id)}
  )

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
  | Neg, [{ e = EVal v1 }] ->
    (* Compute 0 - v1 *)
    let v1 = raw_integer v1 in
    vinteger (Integer.sub (Integer.create ~value:0 ~size:(Integer.size v1)) v1)
    |> mk_e
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
  (* patterns *)
  | PatExact, [{ e = EVal { v = VInt n } }] ->
    vpat (int_to_bitpat (Integer.to_int n) (Integer.size n)) |> mk_e
  | PatMask, [{ e = EVal { v = VInt n } }; { e = EVal { v = VInt m } }] ->
    vpat
      (int_mask_to_bitpat
         (Integer.to_int n)
         (Integer.to_int m)
         (Integer.size n))
    |> mk_e
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
      | Slice _
      | PatExact
      | PatMask )
    , _ ) -> EOp (op, args)
;;

let interp_gen_ty env = function
  | GSingle eo -> GSingle (Option.map (interp_exp env) eo)
  | GMulti e -> GMulti (interp_exp env e)
  | GPort e -> GPort (interp_exp env e)
;;

(* interpet constant match statements *)
let bitmatch bits n =
  let open Z in
  let bits = List.rev bits in
  let two = Z.of_int 2 in
  let rec aux bits n =
    match bits with
    | [] -> n = zero
    | hd :: tl ->
      aux tl (shift_right n 1)
      &&
      (match hd with
       | 0 -> rem n two = zero
       | 1 -> rem n two <> zero
       | _ -> true)
  in
  aux bits n
;;

let matches_pat vs ps =
  if ps = [PWild]
  then true
  else
    List.for_all2
      (fun v p ->
        let v = v.v in
        match p, v with
        | PWild, _ -> true
        | PNum pn, VInt n -> Z.equal (Integer.value n) pn
        | PBit bits, VInt n -> bitmatch bits (Integer.value n)
        | _ -> false)
      vs
      ps
;;

let exp_to_value_opt exp =
  match exp.e with
  | EVal value -> Some value
  | _ -> None
;;

let rec exp_to_values exps =
  match exps with
  | [] -> Some []
  | exp :: exps ->
    let value = exp_to_value_opt exp in
    let values = exp_to_values exps in
    (match value, values with
     | Some value, Some values -> Some (value :: values)
     | _ -> None)
;;

let elim_const_smatch stmt =
  match stmt.s with
  | SMatch (es, bs) ->
    let v_opts = exp_to_values es in
    (match v_opts with
     | None -> stmt
     | Some vs ->
       let first_match =
         try List.find (fun (pats, _) -> matches_pat vs pats) bs with
         | _ ->
           error
             "[PartialInterpretation.elim_const_smatch] Match statement did \
              not match any branch!"
       in
       let stmt' = snd first_match in
       stmt')
  | _ -> stmt
;;

(* Partially interpret a statement. Return the interpreted statment and the
   environment after interpreting it. *)
let rec interp_stmt env s : statement * env =
  (* (match s.s with
   | SNoop | SSeq _ -> ()
   | _ -> print_endline @@ "Interping " ^ CorePrinting.stmt_to_string s); *)
  let interp_exp = interp_exp env in
  let should_inline () =
    match Pragma.find_sprag_args "noinline" [] s.spragmas with
    | Some _ -> false
    | _ -> true
  in
  match s.s with
  | SNoop -> s, env
  | SUnit e -> { s with s = SUnit (interp_exp e) }, env
  | SPrintf (str, es) ->
    { s with s = SPrintf (str, List.map interp_exp es) }, env
  | SGen (g, e) -> { s with s = SGen (interp_gen_ty env g, interp_exp e) }, env
  | SRet eopt -> { s with s = SRet (Option.map interp_exp eopt) }, env
  | STableInstall (id, entries) ->
    let entries =
      List.map
        (fun entry ->
          { entry with
            ematch = List.map interp_exp entry.ematch
          ; eargs = List.map interp_exp entry.eargs
          })
        entries
    in
    { s with s = STableInstall (id, entries) }, env
  | SSeq (s1, s2) ->
    let s1, env1 = interp_stmt env s1 in
    let s2, env2 = interp_stmt env1 s2 in
    (* make sure to carry the pragmas and other annotations on statements *)
    let stmt' =
      if s1.s = SNoop
      then s2
      else if s2.s = SNoop
      then s1
      else { s with s = SSeq (s1, s2) }
    in
    stmt', env2
  (* let s' =
      if s1.s = SNoop then s2.s else if s2.s = SNoop then s1.s else SSeq (s1, s2)
    in
    { s with s = s' }, env2 *)
  (* Cases where we bind/mutate variables *)
  | SLocal (id, ty, exp) ->
    let exp = interp_exp exp in
    let pv =
      if should_inline () then extract_partial_value env exp else new_unknown ()
    in
    let new_s = { s with s = SLocal (id, ty, exp) } in
    new_s, IdMap.add id pv env
  | SAssign (id, exp) ->
    let exp = interp_exp exp in
    let pv =
      if should_inline () then extract_partial_value env exp else new_unknown ()
    in
    let new_s = { s with s = SAssign (id, exp) } in
    new_s, IdMap.add (Cid.to_id id) pv env
  | STableMatch tm ->
    let keys = List.map interp_exp tm.keys in
    let args = List.map interp_exp tm.args in
    let env =
      List.fold_left
        (fun env id -> IdMap.add id (new_unknown ()) env)
        env
        tm.outs
    in
    { s with s = STableMatch { tm with keys; args } }, env
  | SIf (test, s1, s2) ->
    let test = interp_exp test in
    (match test with
     | { e = EVal { v = VBool b } } ->
       if b then interp_stmt env s1 else interp_stmt env s2
     | _ ->
       let s1, env1 = interp_stmt env s1 in
       let s2, env2 = interp_stmt env s2 in
       let base_stmt = { s with s = SIf (test, s1, s2) } in
       base_stmt, merge_envs [env; env1; env2])
  | SMatch (es, branches) ->
    (* expand wildcard rules to have one wildcard for each es *)
    let branches = List.map 
      (fun (ps, stmt) -> 
        let ps = match ps with 
        | [PWild] -> List.init (List.length es) (fun _ -> PWild)
        | _ -> ps in
        (ps, stmt)) branches 
    in
    let es = List.map interp_exp es in
    let rec process_branch (ps, stmt) =       
      (*  1. bind the event parameters
          2. interp the statement
          3. unbind the event parameters *)
      (* We need to bind all the params in matching events.
         We just bind them all to unknown for now. *)
      let event_param_bindings = List.fold_left2 
        (fun event_param_bindings pat exp -> 
          match pat, exp.e with 
          | PEvent(_, params), _ -> 
              event_param_bindings@(List.map (fun (id, _) -> id, new_unknown ()) params)
          | _ -> event_param_bindings)
        []
        ps
        es
      in
      let env = List.fold_left
        (fun env (id, pv) -> IdMap.add id pv env)
        env
        event_param_bindings
      in
      let stmt', env' = interp_stmt env stmt in
      let env' = List.fold_left
        (fun env id -> IdMap.remove id env)
        env'
        (List.split event_param_bindings |> fst)
      in
      (ps, stmt'), env'
    in
    let branches, envs = List.map process_branch branches |> List.split in
    
    let base_stmt = { s with s = SMatch (es, branches) } in
    (* if the match statement's key expression is a constant,
       compute the branch and replace the match statement with it *)
    elim_const_smatch base_stmt, merge_envs (env :: envs)
;;

(* After we do partial interpretation of a handler body, there will likely be many
   variables which are now unused, because all their uses got inlined. Remove
   definitions and mutations for such variables.

   Strategy: Live variable analysis. Walk _backwards_ through the statement, keeping
   track of which variables we see used. If we get to a mutation or definition of
   a variable we haven't seen, we can safely remove it.
*)
let remove_unused_variables stmt =
  (* We should never remove an expression involving a function call, even if
     the result is unused *)
  let cannot_remove_e e =
    let acc = ref false in
    let v =
      object
        inherit [_] s_iter
        method! visit_ECall _ _ _ _ = acc := true
      end
    in
    v#visit_exp () e;
    !acc
  in
  let rec aux live_vars stmt =
    match stmt.s with
    | SSeq (s1, s2) ->
      (* Visit s2 first, since we're going backwards *)
      let live_vars, s2' = aux live_vars s2 in
      let live_vars, s1' = aux live_vars s1 in
      live_vars, { stmt with s = SSeq (s1', s2') }
    (* For most statements, we just collect the variables that are used
       inside it *)
    | SNoop | SPrintf _ | SRet _ | SUnit _ | SGen _ | STableInstall _ ->
      let acc = ref live_vars in
      variable_extractor#visit_statement acc stmt;
      !acc, stmt
    (* When we see a variable assignment or declaration, remove it if
       the variable it refers to is not alive. Never remove function calls
       (or hashes?) because they might have side effects.
    *)
    | SLocal (id, _, e) ->
      let live_vars', stmt' =
        if IdSet.mem id live_vars || cannot_remove_e e
        then (
          (* The variable is no longer live (because we're now before it was
             defined in the first place!) *)
          let live_vars = IdSet.remove id live_vars in
          extract_variables ~acc:live_vars e, stmt)
        else live_vars, { stmt with s = SNoop }
      in
      live_vars', stmt'
    | SAssign (id, e) ->
      (* Same as SLocal but we don't unalive the variable if it was already live *)
      let live_vars', stmt' =
        if IdSet.mem (Cid.to_id id) live_vars || cannot_remove_e e
        then (
          let live_vars = IdSet.add (Cid.to_id id) live_vars in
          extract_variables ~acc:live_vars e, stmt)
        else live_vars, { stmt with s = SNoop }
      in
      live_vars', stmt'
    (* We can't remove the variables that are declared as part of a table match
         without changing semantics, so just make sure we unalive them and gather
         any arguments to the match *)
    | STableMatch tm ->
      let live_vars =
        let acc = ref live_vars in
        variable_extractor#visit_statement acc stmt;
        !acc
      in
      let live_vars =
        List.fold_left (fun acc id -> IdSet.remove id acc) live_vars tm.outs
      in
      live_vars, stmt
    (* Code inside a branch can add live variables, but it can't remove them
       (since they were used outside the branch, and so weren't defined inside it) *)
    | SIf (test, s1, s2) ->
      let live_vars0 = extract_variables ~acc:live_vars test in
      let live_vars1, s1 = aux live_vars s1 in
      let live_vars2, s2 = aux live_vars s2 in
      let live_vars' =
        IdSet.union live_vars0 (IdSet.union live_vars1 live_vars2)
      in
      live_vars', { stmt with s = SIf (test, s1, s2) }
    | SMatch (es, branches) ->
      let live_vars0 =
        List.fold_left (fun acc e -> extract_variables ~acc e) live_vars es
      in
      let live_varses, branches =
        List.map
          (fun (p, stmt) ->
            let live_vars, stmt' = aux live_vars stmt in
            live_vars, (p, stmt'))
          branches
        |> List.split
      in
      let live_vars' = List.fold_left IdSet.union live_vars0 live_varses in
      live_vars', { stmt with s = SMatch (es, branches) }
  in
  aux IdSet.empty stmt |> snd
;;

(* Two passes:
   1.) Normal partial interpretation, which performs constant folding, removes
       unused if-branches, and replaces variables with their definition where possible.
   2.) Remove any variables which are now unused (because all their uses got inlined) *)
let interp_body builtin_tys env (params, stmt) =
  let builtins =
    let open Builtins in
    (ingr_port_id, builtin_tys.ingr_port_ty)
    :: (this_id, Builtins.this_ty)
    :: builtin_vars
  in
  let env = env |> add_builtin_defs builtins |> add_builtin_defs params in
  let stmt' = interp_stmt env stmt |> fst |> remove_unused_variables in
  params, stmt'
;;

let interp_decl builtin_tys env d =
  let add_dec env id = IdMap.add id (new_unknown ()) env in
  match d.d with
  | DGlobal (id, ty, e) ->
    let e = interp_exp env e in
    let env = add_dec env id in
    env, { d with d = DGlobal (id, ty, e) }
  | DMemop { mid; mparams; mbody } ->
    let env = add_dec env mid in
    (* TODO: Maybe interp in the body? Maybe that's handled later? *)
    env, { d with d = DMemop { mid; mparams; mbody } }
  | DHandler (id, s, body) ->
    (* let env = add_dec env id in *)
    env, { d with d = DHandler (id, s, interp_body builtin_tys env body) }
  | DFun(id, ty, body) -> 
    env, { d with d = DFun (id, ty, interp_body builtin_tys env body) }

  | DEvent (id, _, _, _) | DExtern (id, _) ->
    let env = add_dec env id in
    env, d
  | DAction acn ->
    let env = add_dec env acn.aid in
    let acn_env =
      env |> add_builtin_defs acn.aconst_params |> add_builtin_defs acn.aparams
    in
    let abody = List.map (interp_exp acn_env) acn.abody in
    env, { d with d = DAction { acn with abody } }
  | DParser _ -> env, d
;;

let interp_prog ds =
  let builtins =
    let open Builtins in
    [recirc_id, recirc_ty; self_id, self_ty]
  in
  let env = ref (IdMap.empty |> add_builtin_defs builtins) in
  List.map
    (fun d ->
      let env', d = interp_decl Builtins.tofino_builtin_tys !env d in
      env := env';
      d)
    ds
;;
