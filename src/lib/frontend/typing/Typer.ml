open Syntax
open SyntaxUtils
open Batteries
open Collections
open Printing
open TyperUtil
open TyperUnify
open TyperInstGen
open TyperModules

let mk_ty rty = ty_eff rty (fresh_effect ())
let inst ty = instantiator#visit_ty (fresh_maps ()) ty
let auto_count = ref (-1)

let fresh_auto () =
  incr auto_count;
  Id.create ("auto" ^ string_of_int !auto_count)
;;

let check_constraints span err_str (env : env) end_eff constraints =
  match env.ret_ty, IdMap.is_empty env.indices with
  | None, true ->
    (* In this case, we're in a handler, and not inside a loop. This means we're
       not doing inference, we're just checking that the user's constraints
       imply the function's constraints. *)
    if not (TyperZ3.check_implies env.constraints constraints)
    then
      error_sp span
      @@ err_str
      ^ " in a handler may violate global order. (Did you include all the \
         necessary constraints when declaring the event?)";
    { env with current_effect = end_eff }
  | _ ->
    (* In this case, either we're not in a handler or we're in a loop body.
       We should record the new constraints and make sure they're still
       satisfiable *)
    let new_constraints = constraints @ env.constraints in
    if not (TyperZ3.check_sat new_constraints)
    then error_sp span @@ err_str ^ " violates the global order";
    { env with current_effect = end_eff; constraints = new_constraints }
;;

let infer_value v =
  let vty =
    match v.v with
    | VBool _ -> TBool
    | VInt n -> TInt (IConst (Integer.size n))
    | VGroup _ -> TGroup
    | VGlobal _ | VEvent _ -> failwith "Cannot write values of these types"
    | VPat (PBit bs) -> TPat (IConst (List.length bs))
    | VPat (PWild) -> TPat (fresh_size ())
    | VPat (_) -> failwith "patterns besides bitstrings and wilds should not appear in a value"
  in
  { v with vty = Some (mk_ty vty) }
;;

let infer_pattern env p =
  match p with
  | PWild -> (fresh_type ()).raw_ty
  | PVar (cid, span) ->
    (* Basically the same process as typing an EVar *)
    (lookup_var span env cid).raw_ty
    |> instantiator#visit_raw_ty (fresh_maps ())
  | PNum _ -> TInt (fresh_size ())
  | PBit ps -> TInt (IConst (List.length ps))
  | PEvent (_) -> TEvent
;;


(* cast a function as effectless *)
let rec remove_effects ty = 
  let v = object (_) 
    inherit [_] s_map
    method! visit_func_ty _ func_ty = 
      (* drop any constraints on the function *)
      { func_ty with 
        constraints = ref [];
        end_eff = func_ty.start_eff;}
  end
  in
  v#visit_ty () ty
;;

let rec infer_exp (env : env) (e : exp) : env * exp =
  (* print_endline @@ "Inferring " ^ exp_to_string e;
  (match e.ety with 
    | None -> ()
    | Some(ety) -> print_endline@@"Listed type: "^(Printing.ty_to_string ety)); *)
  match e.e with
  | EVar cid ->
    let inst t = instantiator#visit_ty (fresh_maps ()) t in
    let t = lookup_var e.espan env cid in
    env, Some (inst t) |> wrap e
  | EVal v ->
    let v = infer_value v in
    env, { e with e = EVal v; ety = v.vty }
  | EInt (z, szo) ->
    ( env
    , (match szo with
       | None ->
         let new_size = fresh_size () in
         { e with
           e = EInt (z, Some new_size)
         ; ety = Some (mk_ty @@ TInt new_size)
         }
       | Some sz ->
         validate_size e.espan env sz;
         Some (mk_ty @@ TInt sz) |> wrap e) )
  | ESizeCast (sz, _) -> env, Some (mk_ty @@ TInt sz) |> wrap e
  | EOp (op, args) -> infer_op env e.espan op args
  | EHash (size, es) ->
    if List.length es < 2
    then error_sp e.espan "Must pass at least two arguments to hash";
    validate_size e.espan env size;
    let env, inf_es = infer_exps env es in
    let hd = List.hd inf_es in
    unify_ty hd.espan (Option.get hd.ety) (mk_ty @@ TInt (fresh_size ()));
    env, { e with e = EHash (size, inf_es); ety = Some (mk_ty @@ TInt size) }
  | EFlood e1 ->
    let env, inf_e, inf_ety = infer_exp env e1 |> textract in
    unify_ty e.espan inf_ety (mk_ty @@ TInt (fresh_size ()));
    env, { e with e = EFlood inf_e; ety = Some (mk_ty @@ TGroup) }
  | ECall (f, args, unordered) -> (
    let _, _, inferred_fty =
      (* Get type of f as if we used the var rule for the function *)
      infer_exp env { e with e = EVar f } |> textract
    in
    
    (* if we are in unordered mode, cast everything as effectless *)
    (* let inferred_fty = if (Cmdline.cfg.unordered) then  *)
    let inferred_fty = if (unordered) then 
        remove_effects inferred_fty
      else inferred_fty 
    in

    match inferred_fty.raw_ty with 
    | TFun _ -> 
      let env, inferred_args = infer_exps env args in
      let fty : func_ty =
        { arg_tys = List.map (fun arg -> Option.get arg.ety) inferred_args
        ; ret_ty = fresh_type ()
        ; start_eff = env.current_effect
        ; end_eff = fresh_effect ()
        ; constraints = ref []
        }
      in
      unify_raw_ty e.espan (TFun fty) inferred_fty.raw_ty;
      let new_env =
        check_constraints e.espan "Function call" env fty.end_eff
        @@ !(fty.constraints)
      in
      new_env, { e with e = ECall (f, inferred_args, unordered); ety = Some fty.ret_ty }
    | TActionConstr(acn_ctor_ty) -> (
      let env, inferred_args = infer_exps env args in
      let fty : acn_ctor_ty =
        { aconst_param_tys = List.map (fun arg -> Option.get arg.ety) inferred_args
        ; aacn_ty = {
            aarg_tys = List.init (List.length acn_ctor_ty.aacn_ty.aarg_tys) (fun _ -> fresh_type ());
            aret_tys = List.init (List.length acn_ctor_ty.aacn_ty.aret_tys) (fun _ -> fresh_type ());}
        }
      in
      unify_raw_ty e.espan (TActionConstr fty) inferred_fty.raw_ty;
      let aacn_ty = {
        aarg_tys = List.map strip_links fty.aacn_ty.aarg_tys;
        aret_tys = List.map strip_links fty.aacn_ty.aret_tys;
        }
      in
      let acn_ty = ty@@TAction (aacn_ty) in
      let acn_ty = strip_links acn_ty in
      env, { e with e = ECall (f, inferred_args, unordered); ety = Some (acn_ty) }
    )
    | _ -> error_sp e.espan "Cannot call non-function"
  )
  | EProj (e, label) ->
    let env, inf_e = infer_exp env e in (* infer the type of the _record_ *)
    let expected_ty, entries = (* expected_ty is the expected type of the _record_ *)
      match Option.map inst @@ StringMap.find_opt label env.record_labels with
      | Some ({ raw_ty = TRecord lst } as ty) -> ty, lst
      | Some _ -> failwith "Impossible, I hope"
      | None -> error_sp e.espan @@ "Unknown label " ^ label ^" in record exp: "^(Printing.exp_to_string e)
    in
    unify_ty e.espan expected_ty (Option.get inf_e.ety);
    let e_effect = (Option.get inf_e.ety).teffect in
    let idx, (_, raw_ret_ty) = List.findi (fun _ (l, _) -> l = label) entries in
    let ret_ty =
      ty_eff raw_ret_ty (wrap_effect e_effect [None, 0; None, idx])
    in
    env, { e with e = EProj (inf_e, label); ety = Some ret_ty }
  | ERecord entries ->
    let labels, es = List.split entries in
    let env, inf_es = infer_exps env es in
    let expected_ty =
      match StringMap.find_opt (List.hd labels) env.record_labels with
      | Some ty ->
        if (not env.in_global_def) && is_global ty
        then
          error_sp e.espan "Cannot dynamically create values of a global type"
        else inst ty
        | None -> error_sp e.espan @@ "Unknown label " ^ List.hd labels ^" in record exp: "^(Printing.exp_to_string e)
      in
    let inf_ety =
      TRecord
        (List.map2 (fun l e -> l, (Option.get e.ety).raw_ty) labels inf_es)
      |> mk_ty
    in

    unify_ty e.espan expected_ty inf_ety;
    let inf_entries = List.combine labels inf_es in
    env, { e with e = ERecord inf_entries; ety = Some expected_ty }
  | EWith (base, entries) ->
    let labels, es = List.split entries in
    let expected_ty =
      match StringMap.find_opt (List.hd labels) env.record_labels with
      | Some ty ->
        if (not env.in_global_def) && is_global ty
        then
          error_sp e.espan "Cannot dynamically create values of a global type"
        else inst ty
      | None -> error_sp e.espan @@ "Unknown label " ^ List.hd labels
    in
    let env, inf_base, inf_basety = infer_exp env base |> textract in
    unify_raw_ty e.espan expected_ty.raw_ty inf_basety.raw_ty;
    let env, inf_es = infer_exps env es in
    let inf_entries = List.combine labels inf_es in
    let expected_entries =
      match TyTQVar.strip_links expected_ty.raw_ty with
      | TRecord entries -> entries
      | _ -> failwith "impossible"
    in
    List.iter
      (fun (l, e) ->
        let expected =
          try List.assoc l expected_entries with
          | Not_found ->
            Console.error_position e.espan
            @@ "Label "
            ^ l
            ^ " does not belong to the same type as label "
            ^ List.hd labels
        in
        unify_raw_ty e.espan (Option.get e.ety).raw_ty expected)
      inf_entries;
    env, { e with e = EWith (inf_base, inf_entries); ety = Some expected_ty }
  | ETuple es ->
    let env, inf_es = infer_exps env es in
    let tuple_eff = fresh_effect () in
    List.iteri
      (fun i e' ->
        if (not env.in_global_def) && is_global (Option.get e'.ety)
        then
          error_sp
            e'.espan
            "Cannot dynamically create tuples containing global types"
        else (
          (* WARNING: This is a hack that needs to be fixed if users can write tuples. 
              If we have a tuple that contains a get operation to another tuple, 
              the effect of the get op will be a projection to the element 
              of the inner tuple. In the current representation I don't think there's a 
              way to unify something like "the effect of the 5th element of the outer tuple is 
              equal to the 2nd element of some other tuple". And expressions like this can 
              be generated by the compiler -- e.g., when recordElimination turns records 
              into tuples. *)
          let expected = match (e'.e) with 
            | EOp(TGet(_, idx), _) -> wrap_effect tuple_eff [None, 0; None, idx]
            | _ -> wrap_effect tuple_eff [None, 0; None, i] 
          in
          let derived = (Option.get e'.ety).teffect in           
          unify_effect e.espan expected derived
        )
      )
      inf_es;
    let final_ety =
      ty_eff (TTuple (List.map (fun e -> (Option.get e.ety).raw_ty) inf_es)) tuple_eff
    in
    env, { e with e = ETuple inf_es; ety = Some final_ety }


  | EVector es ->
    let env, inf_es = infer_exps env es in
    let ety = fresh_type () in
    List.iteri
      (fun i e' ->
        if (not env.in_global_def) && is_global (Option.get e'.ety)
        then
          error_sp
            e'.espan
            "Cannot dynamically create vectors containing global types"
        else (
          let expected =
            { ety with teffect = wrap_effect ety.teffect [None, 0; None, i] }
          in
          unify_ty e.espan expected (Option.get e'.ety)))
      inf_es;
    let final_ety = TVector (ety.raw_ty, IConst (List.length es)) |> mk_ty in
    env, { e with e = EVector inf_es; ety = Some final_ety }
  | EIndex (e1, IUser (Id idx)) ->
    let env, inf_e1, inf_e1ty = infer_exp env e1 |> textract in
    let renamed_idx, expected_length =
      match IdMap.find_opt idx env.indices with
      | Some x -> x
      | None ->
        error_sp e.espan
        @@ "Index "
        ^ id_to_string idx
        ^ " was not declared in a for loop or comprehension"
    in
    let length = fresh_size () in
    let entry_ty = fresh_type () in
    let expected_ety =
      ty_eff (TVector (entry_ty.raw_ty, length)) (fresh_effect ())
    in
    unify_ty e1.espan inf_e1ty expected_ety;
    if List.exists
         (function
          | Some id, _ -> Id.equal id renamed_idx
          | _ -> false)
         (unwrap_effect inf_e1ty.teffect |> snd)
    then
      error_sp e.espan
      @@ Printf.sprintf
           "Index variable %s was already used as an index to a list \
            containing this one"
           (id_to_string renamed_idx);
    (try unify_size e.espan length expected_length with
     | CannotUnify ->
       error_sp e.espan
       @@ Printf.sprintf
            "Index %s has maximum value %s, but the list has length %s. These \
             values should be identical."
            (id_to_string idx)
            (size_to_string length)
            (size_to_string expected_length));
    let ety =
      Some { entry_ty with teffect = FIndex (renamed_idx, inf_e1ty.teffect) }
    in
    env, { e with e = EIndex (inf_e1, IUser (Id idx)); ety }
  | EIndex (e1, idx) ->
    let i =
      match idx with
      | IConst i -> i
      | _ ->
        error_sp e.espan
        @@ "Index "
        ^ size_to_string idx
        ^ " is neither a variable nor a constant."
    in
    let env, inf_e1, inf_e1ty = infer_exp env e1 |> textract in
    let length = fresh_size () in
    let entry_ty = fresh_type () in
    let expected_ety =
      ty_eff (TVector (entry_ty.raw_ty, length)) (fresh_effect ())
    in
    unify_ty e1.espan inf_e1ty expected_ety;
    (match STQVar.strip_links length with
     | IConst n ->
       if i >= n
       then
         error_sp e.espan
         @@ Printf.sprintf "Invalid index %d: list has length %d" i n
     | sz ->
       error_sp e.espan
       @@ "Invalid indexing operation: list must have a known length, not "
       ^ size_to_string sz);
    let ety =
      Some
        { entry_ty with
          teffect = wrap_effect inf_e1ty.teffect [None, 0; None, i]
        }
    in
    env, { e with e = EIndex (inf_e1, idx); ety }
  | EComp (e1, idx, sz) ->
    validate_size e.espan env sz;
    let renamed_idx = Id.freshen idx in
    (* Effect before starting the loop *)
    let initial_effect = env.current_effect in
    (* Abstract starting effect for typechecking the loop body *)
    let alpha_start = fresh_effect ~name:"start" () in
    let env1, inf_e1, inf_ety =
      infer_exp
        { env with
          indices = IdMap.add idx (renamed_idx, sz) env.indices
        ; current_effect = alpha_start
        }
        e1
      |> textract
    in
    unify_effect
      e1.espan
      inf_ety.teffect
      (FIndex (renamed_idx, fresh_effect ()));
    let alpha_start_id =
      match FTQVar.strip_links alpha_start with
      | FVar (TVar { contents = Unbound (id, _) }) -> id
      | _ -> failwith "impossible"
    in
    (* Effect at the end of the loop, in terms of renamed_idx and start_effect *)
    let end_effect = env1.current_effect in
    (* Constraints that we got from the loop body *)
    let loop_constraints = drop_constraints env env1 in
    (* Helper function for substituting renamed_idx and start_effect into the end_effect *)
    let subst_env i start = (renamed_idx, i), (alpha_start_id, start) in
    (* Helper function that does the substitution *)
    let inst_constraints i start =
      List.map (subst_loop#visit_constr (subst_env i start)) loop_constraints
    in
    (* Effect at start of loop iteration 0 *)
    let start_effect_0 = initial_effect in
    (* Effect at start of loop iteration 1 *)
    let start_effect_1 =
      subst_loop#visit_effect (subst_env 0 start_effect_0) end_effect
    in
    let c0 = inst_constraints 0 start_effect_0 in
    let c1 = inst_constraints 1 start_effect_1 in
    (* Effect after finishing execution of the loop *)
    let final_effect =
      if equiv_effect end_effect alpha_start
      then initial_effect
      else FSucc (drop_indexes renamed_idx end_effect)
    in
    (* Env after finishing execution of the loop *)
    let new_env =
      check_constraints
        e.espan
        "Comprehension "
        env
        final_effect
        (c0 @ c1 @ env.constraints)
    in
    ( new_env
    , { e with
        e = EComp (inf_e1, idx, sz)
      ; ety = Some (mk_ty (TVector (inf_ety.raw_ty, sz)))
      } )
  | EStmt (s, e1) ->
    let env, inf_s = infer_statement env s in
    let env, inf_e1, inf_e1ty = infer_exp env e1 |> textract in
    env, { e with e = EStmt (inf_s, inf_e1); ety = Some inf_e1ty }
  | ETableCreate ecreate ->
    let env, inf_tsize = infer_exp env ecreate.tsize in
    let unify_arg_tys sp msg tys1 tys2 =
      if List.length tys1 <> List.length tys2
      then error_sp sp ("wrong number of match-time arguments " ^ msg);
      List.iter2 (unify_ty e.espan) tys1 tys2
    in
    (* for actions, we check the action args and return types *)
    (* expected types come from table type *)
    let exp_atys, exp_rty =
      match ecreate.tty.raw_ty with
      | TTable trec -> trec.tparam_tys, trec.tret_tys
      | _ -> error "expected table type"
    in
    (* inferred types come from actions passed as arguments *)
    let env, inf_acns = infer_exps env ecreate.tactions in
    let check_acn_ctor_ty e_inf_acn =
      let inf_atys, inf_rty =
        match (Option.get e_inf_acn.ety).raw_ty with
        | TActionConstr {aacn_ty = {aarg_tys; aret_tys} } -> aarg_tys, aret_tys
        | _ -> error "not an action"
      in
      (* unify runtime arg and return types *)
      unify_arg_tys
        e_inf_acn.espan
        "in action assigned to table"
        exp_atys
        inf_atys;
      unify_arg_tys
        e_inf_acn.espan
        "in return of action assigned to table"
        exp_rty
        inf_rty
    in
    List.iter check_acn_ctor_ty inf_acns;
    (* infer types of default action args *)
    let def_cid, def_args, flag = unpack_default_action ecreate.tdefault.e in
    let env, inf_def_args = infer_exps env def_args in
    (* type check the default action's const args *)
    let expected_def_arg_tys =
      match (lookup_var e.espan env def_cid).raw_ty with
      | TActionConstr a -> a.aconst_param_tys
      | _ -> error_sp e.espan "the default action does not have type TActionConstr"
    in
    let inf_def_arg_tys =
      List.map (fun exp -> Option.get exp.ety) inf_def_args
    in
    unify_arg_tys
      e.espan
      ("provided to default action \"" ^ Printing.cid_to_string def_cid ^ "\"")
      expected_def_arg_tys
      inf_def_arg_tys;
    (* check that the default action is one of the table's actions *)
    let tbl_acn_cids =
      List.map
        (fun exp ->
          match exp.e with
          | EVar cid -> cid
          | _ -> error_sp exp.espan "table actions must be a list of action ids")
        inf_acns
    in
    if not (List.exists (Cid.equal def_cid) tbl_acn_cids)
    then
      error_sp
        e.espan
        ("default action ("
        ^ Printing.cid_to_string def_cid
        ^ ") is not an action assigned to the table.");
    (* The constructor expression must have an unbound effect, or it may not
       unify with the type of the declaration. Note that, we cannot infer the
       full table type from the constructor expression, because the constructor
       expression doesn't know what the table keys are. *)
    let ety = { ecreate.tty with teffect = fresh_effect () } in
    (* return typed table with typed action args *)
    ( env
    , { e with
        e =
          ETableCreate
            { ecreate with
              tactions = inf_acns
            ; tsize = inf_tsize
            (* note that the default action expression is currently typed as TVoid, because the type 
               never matters except for earlier in this checking branch, where it is obtained from elsewhere *)
            ; tdefault = {ecreate.tdefault with e=ECall(def_cid, inf_def_args, flag); ety=Some(ty TVoid)}
            }
      ; ety = Some ety
      } )
  | ETableMatch tr ->
    let new_env, new_tr, ret_ty = infer_tblmatch env tr e.espan in
    let ret_ty =
      match ret_ty with
      | [ret_ty] -> ret_ty
      | _ ->
        error
          "table apply expression has multiple return types. This should be \
           impossible."
    in
    let new_e = ETableMatch new_tr in
    new_env, { e with e = new_e; ety = Some ret_ty }
  

and infer_op env span op args =
  let env, ty, new_args =
    match op, args with
    | Not, [e] ->
      let env, inf_e, inf_ety = infer_exp env e |> textract in
      unify_raw_ty span inf_ety.raw_ty TBool;
      env, mk_ty TBool, [inf_e]
    | Neg, [e] ->
      let env, inf_e, inf_ety = infer_exp env e |> textract in
      unify_raw_ty span inf_ety.raw_ty TBool;
      env, mk_ty (TInt (fresh_size ())), [inf_e]
    | BitNot, [e] ->
      let tsize = fresh_size () in
      let env, inf_e, inf_ety = infer_exp env e |> textract in
      unify_raw_ty span inf_ety.raw_ty (TInt tsize);
      env, mk_ty (TInt (fresh_size ())), [inf_e]
    | (And | Or), [e1; e2] ->
      let env, inf_e1, inf_ety1 = infer_exp env e1 |> textract in
      let env, inf_e2, inf_ety2 = infer_exp env e2 |> textract in
      unify_raw_ty span inf_ety1.raw_ty TBool;
      unify_raw_ty span inf_ety2.raw_ty TBool;
      env, mk_ty TBool, [inf_e1; inf_e2]
    | (Eq | Neq), [e1; e2] ->
      let env, inf_e1, inf_ety1 = infer_exp env e1 |> textract in
      let env, inf_e2, inf_ety2 = infer_exp env e2 |> textract in
      unify_ty span inf_ety1 inf_ety2;
      env, mk_ty TBool, [inf_e1; inf_e2]
    | (Less | More | Leq | Geq), [e1; e2] ->
      let tsize = fresh_size () in
      let env, inf_e1, inf_ety1 = infer_exp env e1 |> textract in
      let env, inf_e2, inf_ety2 = infer_exp env e2 |> textract in
      unify_raw_ty span inf_ety1.raw_ty (TInt tsize);
      unify_raw_ty span inf_ety2.raw_ty (TInt tsize);
      env, mk_ty TBool, [inf_e1; inf_e2]
    | (Plus | Sub | SatPlus | SatSub | BitAnd | BitOr | BitXor), [e1; e2] ->
      let tsize = fresh_size () in
      let env, inf_e1, inf_ety1 = infer_exp env e1 |> textract in
      let env, inf_e2, inf_ety2 = infer_exp env e2 |> textract in
      unify_raw_ty span inf_ety1.raw_ty (TInt tsize);
      unify_raw_ty span inf_ety2.raw_ty (TInt tsize);
      env, mk_ty @@ TInt tsize, [inf_e1; inf_e2]
    | (LShift | RShift), [e1; e2] ->
      let tsize = fresh_size () in
      let env, inf_e1, inf_ety1 = infer_exp env e1 |> textract in
      let env, inf_e2, inf_ety2 = infer_exp env e2 |> textract in
      unify_raw_ty span inf_ety1.raw_ty (TInt tsize);
      unify_raw_ty span inf_ety2.raw_ty (TInt (fresh_size ()));
      env, mk_ty @@ TInt tsize, [inf_e1; inf_e2]
    | Conc, [e1; e2] ->
      let tsize1 = fresh_size () in
      let tsize2 = fresh_size () in
      let env, inf_e1, inf_ety1 = infer_exp env e1 |> textract in
      let env, inf_e2, inf_ety2 = infer_exp env e2 |> textract in
      unify_raw_ty span inf_ety1.raw_ty (TInt tsize1);
      unify_raw_ty span inf_ety2.raw_ty (TInt tsize2);
      env, mk_ty @@ TInt (add_sizes tsize1 tsize2), [inf_e1; inf_e2]
    | Cast out_size, [e] ->
      let tsize = fresh_size () in
      let env, inf_e, inf_ety = infer_exp env e |> textract in
      unify_raw_ty span inf_ety.raw_ty (TInt tsize);
      env, mk_ty @@ TInt out_size, [inf_e]
    | Slice (hi, lo), [e] ->
      (* FIXME: To be sound, we should also require that the size is at most hi.
                But we can't express that at the moment. *)
      (match try_subtract_sizes hi lo with
       | None ->
         error_sp span
         @@ Printf.sprintf
              "Invalid slice arguments: %s is not obviously larger than %s"
              (size_to_string hi)
              (size_to_string lo)
       | Some diff ->
         let tsize = fresh_size () in
         let env, inf_e, inf_ety = infer_exp env e |> textract in
         unify_raw_ty span inf_ety.raw_ty (TInt tsize);
         env, mk_ty @@ TInt (ISum ([diff], 1)), [inf_e])
    | TGet (size, idx), [e] ->
      let env, inf_e, inf_ety = infer_exp env e |> textract in
      let expected_rtys = List.init size (fun _ -> (fresh_type ()).raw_ty) in
      let expected_ty = mk_ty @@ TTuple expected_rtys in
      unify_ty span inf_ety expected_ty;
      if idx < 0 || idx >= size
      then error_sp span "Invalid index in TGet operator";
      let final_ty =
        ty_eff
          (List.nth expected_rtys idx)
          (wrap_effect inf_ety.teffect [None, 0; None, idx])
      in
      env, final_ty, [inf_e]
    | PatExact, [e] ->
      (* infer operand type *)
      let env, inf_e, inf_ety = infer_exp env e |> textract in
      (* check to make sure it is int of some size *)
      let tsize = fresh_size () in
      unify_raw_ty span inf_ety.raw_ty (TInt tsize);
      (* result type is a TPat of that size, with typed operand *)
      env, mk_ty @@ TPat tsize, [inf_e]
    | PatMask, [e1; e2] ->
      let tsize = fresh_size () in
      let env, inf_e1, inf_ety1 = infer_exp env e1 |> textract in
      let env, inf_e2, inf_ety2 = infer_exp env e2 |> textract in
      unify_raw_ty span inf_ety1.raw_ty (TInt tsize);
      unify_raw_ty span inf_ety2.raw_ty (TInt tsize);
      env, mk_ty @@ TPat tsize, [inf_e1; inf_e2]
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
        | TGet _
        | PatExact
        | PatMask )
      , _ ) ->
      error_sp span
      @@ "Wrong number of arguments to operator: "
      ^ e_to_string (EOp (op, args))
  in
  env, { e = EOp (op, new_args); ety = Some ty; espan = span }

and infer_exps env es =
  let env, es' =
    List.fold_left
      (fun (env, es') e ->
        let env', e' = infer_exp env e in
        env', e' :: es')
      (env, [])
      es
  in
  env, List.rev es'

and infer_action_args env sp (acn_args : exp list) (expected_arg_tys : ty list) =
  let _, inf_acn_args = infer_exps env acn_args in
  List.iter2
    (fun inf_e expect_ty ->
      match inf_e.ety with
      | Some ty -> try_unify_ty sp ty expect_ty
      | None -> error_sp sp "could not infer type of action argument")
    inf_acn_args
    expected_arg_tys;
  inf_acn_args

and infer_keys env sp (inf_keysizes : ty list) keys =
  let inf_keys = List.map (infer_exp env) keys |> List.split |> snd in
  if List.length inf_keysizes <> List.length keys
  then error_sp sp "Key has incorrect number of fields for table_type.";
  List.iter2
    (fun key_exp inf_keysz ->
      let keysz =
        match key_exp.ety with
        | None -> error_sp key_exp.espan "Could not infer type"
        | Some ty -> ty
      in
      unify_ty sp keysz inf_keysz)
    inf_keys
    inf_keysizes;
  inf_keys

(* for table return types *)
and tup_of_tys tys =
  match tys with
  | [t] -> t.raw_ty
  | _ -> TTuple (List.map (fun ty -> ty.raw_ty) tys)

and infer_tblmatch (env : env) (tr : tbl_match) sp : env * tbl_match * ty list =
  let etbl = tr.tbl in
  (* infer table type, which looks it up from context *)
  let _, inf_etbl = infer_exp env etbl in
  let tblty = Option.get inf_etbl.ety in
  (* try_unify_rty sp ((Option.get inf_etbl.ety).raw_ty) tr.tty.raw_ty; *)
  (* get information from inferred table type *)
  let inf_keysize, inf_arg_rtys, inf_ret_ty =
    match inf_etbl.ety with
    | Some ty ->
      (match TyTQVar.strip_links ty.raw_ty with
       | TTable trec ->
         trec.tkey_sizes, trec.tparam_tys, tup_of_tys trec.tret_tys
       | t ->
         error_sp
           sp
           ("table_match arg is not a table: " ^ Printing.raw_ty_to_string t))
    | _ -> error_sp sp "table_match 1st arg has no type"
  in
  let key_args, acn_args = tr.keys, tr.args in
  (* type check key args *)
  let inf_keys = infer_keys env sp inf_keysize key_args in
  (* type check action args *)
  let inf_acn_args = infer_action_args env sp acn_args inf_arg_rtys in
  (* the actions and case statements have already been type checked at creation time.*)

  (* check effects *)
  (* inferred type of the match statement --
       a function call with:
        1st arg is declared table type.
        next args are key types, remaining args are action arg types.
       start effect is fresh, table arg effect is fresh, end effect is table arg effect+1,
       constraints are that start effect is equal to table arg effect.   *)
  let base_apply_fty =
    let tbl_eff = FVar (QVar (Id.fresh "eff")) in
    let start_eff = FVar (QVar (Id.fresh "eff")) in
    let base_tblty = ty_eff tblty.raw_ty tbl_eff in
    (* note: its okay to use inferred keys/acn args because they have been
         checked against inferred table, which has been checked against declared table. *)
    let base_key_tys = List.map (fun ekey -> Option.get ekey.ety) inf_keys in
    let base_arg_tys =
      List.map (fun earg -> Option.get earg.ety) inf_acn_args
    in
    (* hack: put return types at end of arg types *)
    { arg_tys = (base_tblty :: base_key_tys) @ base_arg_tys
    ; ret_ty = ty inf_ret_ty
    ; start_eff
    ; end_eff = FSucc tbl_eff
    ; constraints = ref [CLeq (start_eff, tbl_eff)]
    }
  in
  (* the inferred type is a copy of the base type. *)
  let inf_apply_fty =
    instantiator#visit_ty (fresh_maps ()) (ty (TFun base_apply_fty))
  in
  (* the actual type is a call with 1st arg of INFERRED type of the table variable.
       This is important because the inferred type will have the concrete effect corresponding
       to where the variable was declared. *)
  (* this can be cleaner. inferred type should come entirely from inferred variables.
       declared / expected type should come entirely (modulo key types) from declared type.
       but this seems correct for now. *)
  let base_key_tys = List.map (fun ekey -> Option.get ekey.ety) inf_keys in
  let base_arg_tys = List.map (fun earg -> Option.get earg.ety) inf_acn_args in
  let expected_fty =
    { (* hack: put return types at end of arg types *)
      arg_tys = (Option.get inf_etbl.ety :: base_key_tys) @ base_arg_tys
    ; ret_ty = ty inf_ret_ty
    ; start_eff = env.current_effect
    ; end_eff = fresh_effect ()
    ; constraints = ref []
    }
  in
  (* unify inferred and actual types *)
  unify_raw_ty sp (TFun expected_fty) inf_apply_fty.raw_ty;
  let new_env =
    check_constraints sp "Table match" env expected_fty.end_eff
    @@ !(expected_fty.constraints)
  in
  let new_tr =
    { tbl = inf_etbl
    ; keys = inf_keys
    ; args = inf_acn_args
    ; outs = tr.outs
    ; out_tys = tr.out_tys
    }
  in
  let ret_tys =
    match expected_fty.ret_ty.raw_ty with
    | TTuple raw_tys -> List.map ty raw_tys
    | _ -> [expected_fty.ret_ty]
  in
  new_env, new_tr, ret_tys

and infer_statement (env : env) (s : statement) : env * statement =
  (*(match s.s with
  | SSeq _ | SNoop -> ()
  | _ -> print_endline @@ "Inferring " ^ stmt_to_string s);*)
  let env, stmt =
    match s.s with
    | SNoop -> env, s.s
    | SUnit e ->
      let env, inf_e, _ = infer_exp env e |> textract in
      env, SUnit inf_e
    | SRet eopt ->
      let err str = error_sp s.sspan @@ str ^ ": " ^ stmt_to_string s in
      let return env =
        { env with
          returned = true
        ; ret_effects = env.current_effect :: env.ret_effects
        }
      in
      begin
        match env.ret_ty, eopt with
        | None, _ -> err "Return outside of function body"
        | Some { raw_ty = TVoid }, None -> return env, SRet None
        | Some { raw_ty = TVoid }, Some _ ->
          err "Nonempty return inside void function body"
        | Some _, None -> err "Empty return inside non-void function body"
        | Some ty, Some e ->
          let env, inf_e, inf_ety = infer_exp env e |> textract in
          unify_ty s.sspan ty inf_ety;
          return env, SRet (Some inf_e)
      end
    | SLocal (id, ty, e) ->
      let env, inf_e, ety = infer_exp env e |> textract in
      unify_ty s.sspan ty ety;
      (match TyTQVar.strip_links ety.raw_ty with
       | TVoid ->
         error_sp s.sspan
         @@ "Cannot assign result of void function to variable: "
         ^ stmt_to_string s
       | _ -> ());
      let env = add_locals env [id, ty] in
      env, SLocal (id, ty, inf_e)
    | SAssign (id, e) ->
      let env, inf_e, ety = infer_exp env e |> textract in
      (match IdMap.find_opt id env.locals with
       | Some rty -> unify_ty s.sspan rty ety
       | None ->
         (match lookup_var s.sspan env (Id id) with
          | _ ->
            error_sp s.sspan @@ "Assignment to constant variable " ^ Id.name id));
      (match TyTQVar.strip_links ety.raw_ty with
       | TVoid ->
         error_sp s.sspan
         @@ "Cannot assign result of void function to variable: "
         ^ stmt_to_string s
       | _ -> ());
      env, SAssign (id, inf_e)
    (* assignment to already declared tuple *)
    | STupleAssign({ids; tys=None; exp}) -> (
      (* type the exp *)
      let env, inf_e, ety = infer_exp env exp |> textract in
      let etys = unpack_tuple_rty (ety.raw_ty) in
      (* make sure no inner types are TVoid *)
      List.iter (fun ty -> match TyTQVar.strip_links ty with
        | TVoid -> error_sp s.sspan "Cannot assign result of void function to variable"
        | _ -> ()) etys;
      (* make sure the number of ids matches the number of types in the tuple *)
      if (List.length ids) <> (List.length etys) then (
        error_sp s.sspan "Incorrect number of variables in tuple assignment";
      );
      (* look up the types of the ids *)
      let id_tys = List.map (fun id -> 
        match (IdMap.find_opt id env.locals) with 
        | Some ty -> ty.raw_ty
        | None -> error_sp s.sspan @@ "Variable " ^ Id.name id ^ " not declared") ids 
      in
      (* unify id_tys with etys *)
      List.iter2 (unify_raw_ty s.sspan) id_tys etys;
      (* return the statement *)
      env, STupleAssign({ids; tys=None; exp=inf_e})
    )
    | STupleAssign({ids; tys=Some(tys); exp}) -> (
      (* type the exp *)
      let env, inf_e, ety = infer_exp env exp |> textract in
      let etys = unpack_tuple_rty (ety.raw_ty) in
      (* make sure no inner types are TVoid *)
      List.iter (fun ty -> match TyTQVar.strip_links ty with
        | TVoid -> error_sp s.sspan "Cannot assign result of void function to variable"
        | _ -> ()) etys;
      (* make sure the number of ids matches the number of types in the tuple *)
      if (List.length ids) <> (List.length etys) then (
        error_sp s.sspan "Incorrect number of variables in tuple assignment";
      );
      (* unify id_tys with tys *)
      List.iter2 (unify_raw_ty s.sspan) (List.map (fun ty -> ty.raw_ty) tys) etys;
      (* return the statement *)
      env, STupleAssign({ids; tys=Some(tys); exp=inf_e})
    )
    | SPrintf (str, es) ->
      let expected_tys = extract_print_tys s.sspan str in
      if List.length expected_tys <> List.length es
      then error_sp s.sspan "Incorrect number of arguments to printf statement";
      let env, inf_es = infer_exps env es in
      List.iter2
        (fun e ty -> unify_raw_ty s.sspan (Option.get e.ety).raw_ty ty)
        inf_es
        expected_tys;
      env, SPrintf (str, inf_es)
    | SIf (e, s1, s2) ->
      let env, inf_e, inf_ety = infer_exp env e |> textract in
      unify_raw_ty e.espan TBool inf_ety.raw_ty;
      let env1, inf_s1 = infer_statement env s1 in
      let env2, inf_s2 = infer_statement env s2 in
      let env =
        { env with
          returned = env1.returned && env2.returned
        ; constraints = drop_constraints env env1 @ env2.constraints
        ; ret_effects = drop_ret_effects env env1 @ env2.ret_effects
        }
      in
      let current_effect =
        match env1.returned, env2.returned with
        | true, true -> env.current_effect
        | false, true -> env1.current_effect
        | true, false -> env2.current_effect
        | false, false ->
          (try
             TyperZ3.find_max
               env.constraints
               env1.current_effect
               env2.current_effect
           with
           | TyperZ3.NoMax ->
             Console.error_position
               s.sspan
               "Unable to determine which globals have been used at end of if \
                statement; neither branch ends obviously later than the other.")
      in
      { env with current_effect }, SIf (inf_e, inf_s1, inf_s2)
    | SGen (g, e) ->
      let env, inf_e, ety = infer_exp env e |> textract in
      unify_raw_ty s.sspan ety.raw_ty TEvent;
      let env, inf_g =
        match g with
        | GSingle None -> env, g
        | GSingle (Some loc) ->
          let env, inf_loc, lty = infer_exp env loc |> textract in
          unify_raw_ty s.sspan lty.raw_ty (TInt (fresh_size ()));
          env, GSingle (Some inf_loc)
        | GMulti loc ->
          let env, inf_loc, lty = infer_exp env loc |> textract in
          unify_raw_ty s.sspan lty.raw_ty TGroup;
          env, GMulti inf_loc
        | GPort loc ->
          let env, inf_loc, lty = infer_exp env loc |> textract in
          (* the egress port expression should be the same type as the 
            ingress port builtin *)
          let inst t = instantiator#visit_ty (fresh_maps ()) t in
          let t = inst (lookup_var e.espan env (Cid.id Builtins.ingr_port_id)) in
          unify_raw_ty s.sspan lty.raw_ty t.raw_ty;
          env, GPort inf_loc
      in
      env, SGen (inf_g, inf_e)
    | SSeq (s1, s2) ->
      let env, inf_s1 = infer_statement env s1 in
      let env, inf_s2 = infer_statement env s2 in
      env, SSeq (inf_s1, inf_s2)
    | SMatch (es, bs) ->
      let env, inf_es = infer_exps env es in
      let env, inf_bs =
        infer_branches
          env
          s
          (List.map (fun e -> (e.ety |> Option.get).raw_ty) inf_es)
          bs
      in
      env, SMatch (inf_es, inf_bs)
    | STableMatch tm ->
      let new_env, new_tm, _ = infer_tblmatch env tm s.sspan in
      let new_env =
        match new_tm.out_tys with
        | Some out_tys ->
          (* table_match declares new locals *)
          add_locals new_env (List.combine new_tm.outs out_tys)
        | None -> new_env
      in
      new_env, STableMatch new_tm
    | STableInstall (etbl, entries) ->
      (* infer table type, which looks it up from context *)
      let _, inf_etbl = infer_exp env etbl in
      let env, inf_entries =
        infer_entries env s.sspan (Option.get inf_etbl.ety) entries
      in
      env, STableInstall (inf_etbl, inf_entries)
    | SLoop (s1, idx, sz) ->
      validate_size s.sspan env sz;
      let renamed_idx = Id.freshen idx in
      (* Effect before starting the loop *)
      let initial_effect = env.current_effect in
      (* Abstract starting effect for typechecking the loop body *)
      let alpha_start = fresh_effect ~name:"start" () in
      let env1, inf_s1 =
        infer_statement
          { env with
            indices = IdMap.add idx (renamed_idx, sz) env.indices
          ; current_effect = alpha_start
          }
          s1
      in
      let alpha_start_id =
        match FTQVar.strip_links alpha_start with
        | FVar (TVar { contents = Unbound (id, _) }) -> id
        | _ -> failwith "impossible"
      in
      (* Effect at the end of the loop, in terms of renamed_idx and start_effect *)
      let end_effect = env1.current_effect in
      (* Constraints that we got from the loop body *)
      let loop_constraints = drop_constraints env env1 in
      (* Helper function for substituting renamed_idx and start_effect into the end_effect *)
      let subst_env i start = (renamed_idx, i), (alpha_start_id, start) in
      (* Helper function that does the substitution *)
      let inst_constraints i start =
        List.map (subst_loop#visit_constr (subst_env i start)) loop_constraints
      in
      (* Effect at start of loop iteration 0 *)
      let start_effect_0 = initial_effect in
      (* Effect at start of loop iteration 1 *)
      let start_effect_1 =
        subst_loop#visit_effect (subst_env 0 start_effect_0) end_effect
      in
      let c0 = inst_constraints 0 start_effect_0 in
      let c1 = inst_constraints 1 start_effect_1 in
      (* Effect after finishing execution of the loop *)
      let final_effect =
        if equiv_effect end_effect alpha_start
        then initial_effect
        else FSucc (drop_indexes renamed_idx end_effect)
      in
      (* Env after finishing execution of the loop *)
      let new_env =
        check_constraints
          s.sspan
          "Loop "
          env
          final_effect
          (c0 @ c1 @ env.constraints)
      in
      new_env, SLoop (inf_s1, idx, sz)
  in
  env, { s with s = stmt }

(* check / infer types of entries in a table install statement *)
and infer_entries (env : env) sp tbl_ty entries =
  (* get key sizes *)
  let key_sizes =
    match (TyTQVar.strip_links tbl_ty.raw_ty) with
    | TTable tbl_ty -> tbl_ty.tkey_sizes
    | _ -> error_sp sp ("first argument to table_install is not a table:\n"^(Printing.raw_ty_to_string tbl_ty.raw_ty))
  in
  let ty_to_size (ty : ty) = 
    match ty.raw_ty with 
    | TInt(sz) -> sz
    | TBool -> IConst(1)
    | _ -> error_sp sp "[ty_to_size] expected an int or bool, but got something else"
  in
  let expected_pat_rawtys = List.map (fun sz -> TPat (ty_to_size sz)) key_sizes in
  (* do inference and checks for a single entry *)
  let infer_entry env entry =
    (* type the patterns *)
    let env, inf_ematch =
      if List.length entry.ematch <> List.length expected_pat_rawtys
      then
        error_sp
          sp
          "an entry has the wrong number of patterns based on this table's key."
      else (
        let env, inf_epats_rev =
          List.fold_left
            (fun (env, inf_epats_rev) (epat, expected_epat_rawty) ->
              (* infer the expression's type *)
              let env, inf_epat, inf_epat_ty = infer_exp env epat |> textract in
              (* unify that type with the expected type *)
              unify_raw_ty epat.espan expected_epat_rawty inf_epat_ty.raw_ty;
              (* return the new environment and pat *)
              env, inf_epat :: inf_epats_rev)
            (env, [])
            (List.combine entry.ematch expected_pat_rawtys)
        in
        env, List.rev inf_epats_rev)
    in
    (* type the constant action parameters *)
    let action_cid, action_args, flag = unpack_default_action entry.eaction.e in
    let param_tys =
      match (lookup_var sp env action_cid).raw_ty with
      | TActionConstr acn_ctor_ty -> acn_ctor_ty.aconst_param_tys
      | _ -> error_sp sp "table entry does not refer to an action."
    in
    (* infer types of action args *)
    let env, inf_eargs = infer_exps env action_args in
    let inf_arg_tys = List.map (fun arg -> Option.get arg.ety) inf_eargs in
    (* "unify", inferred args with params (make sure they are equiv) *)
    List.iter2 (unify_ty sp) inf_arg_tys param_tys;
    (* return new env and entry with typed patterns and args *)
    (* note that the action call's type is not currently checked *)
    let eaction = {entry.eaction with e=ECall(action_cid, inf_eargs, flag); ety = Some(ty TVoid)} in
    let entry = {entry with ematch = inf_ematch; eaction;} in
    env, entry
  in
  let env', entries_rev =
    List.fold_left
      (fun (env, entries_rev) entry ->
        let env', entry' = infer_entry env entry in
        env', entry' :: entries_rev)
      (env, [])
      entries
  in
  let entries = List.rev entries_rev in
  env', entries

and infer_branches (env : env) s etys branches =
  let drop_constraints = drop_constraints env in
  let drop_ret_effects = drop_ret_effects env in
  let check_pats pats =
    match pats with
    | [PWild] -> ()
    | _ when List.length pats <> List.length etys ->
      error_sp
        s.sspan
        "A branch of this match statement has the wrong number of patterns"
    | _ ->
      List.iter2 (unify_raw_ty s.sspan) etys (List.map (infer_pattern env) pats)
  in
  let add_pat_params env pat = 
    match pat with 
    | PEvent (_, params) -> (add_locals env params)
    | _ -> env 
  in
  let add_pats_params env pats = (List.fold_left add_pat_params env pats) in
  let infer_branch (pats, s) =
    check_pats pats;
    let env1, inf_s = infer_statement (add_pats_params env pats) s in
    env1, (pats, inf_s)
  in
  let returned, current_effect, constraints, ret_effects, inf_bs =
    List.fold_left
      (fun (returned, acc_eff, acc_constraints, acc_ret_effects, branches) b ->
        let env1, inf_b = infer_branch b in
        let constrs = drop_constraints env1 @ acc_constraints in
        let ret_effects = drop_ret_effects env1 @ acc_ret_effects in
        let eff =
          if env1.returned
          then acc_eff
          else (
            try TyperZ3.find_max constrs env1.current_effect acc_eff with
            | TyperZ3.NoMax ->
              Console.error_position
                s.sspan
                "Unable to determine which global variables have been used \
                 after match statement.")
        in
        returned && env1.returned, eff, constrs, ret_effects, inf_b :: branches)
      (true, env.current_effect, env.constraints, env.ret_effects, [])
      branches
  in
  ( { env with returned; current_effect; constraints; ret_effects }
  , List.rev inf_bs )
;;

let infer_body env (params, s) =
  let env = add_locals env params in
  let env, s = infer_statement env s in
  env, (params, s)
;;

let infer_memop env params mbody =
  (* Memops.ml has already enforced most of the restrictions on the form of the
     memop during parsing. Now we just need to make sure the different parts
     have the right type. *)
  let expected_tint = snd (List.hd params) in
  let check_e env expected e =
    let _, inf_e, inf_ety = infer_exp env e |> textract in
    try_unify_ty e.espan inf_ety expected;
    inf_e
  in
  let check_bool env e = check_e env (ty TBool) e in
  let check_int env e = check_e env expected_tint e in
  let env = add_locals env params in
  match mbody with
  | MBReturn e -> MBReturn (check_int env e)
  | MBIf (e1, e2, e3) ->
    MBIf (check_bool env e1, check_int env e2, check_int env e3)
  | MBComplex body ->
    let check_b env (id, e) = id, check_bool env e in
    let b1 = Option.map (check_b env) body.b1 in
    let b2 = Option.map (check_b env) body.b2 in
    let bs =
      [b1; b2]
      |> List.filter_map (fun x -> x)
      |> List.map (fun (id, _) -> id, ty TBool)
    in
    let env = add_locals env bs in
    let infer_cr env (e1, e2) = check_bool env e1, check_int env e2 in
    let infer_cell (cro1, cro2) =
      Option.map (infer_cr env) cro1, Option.map (infer_cr env) cro2
    in
    let cell1 = infer_cell body.cell1 in
    let cell2 = infer_cell body.cell2 in
    let env =
      add_locals
        env
        [Builtins.cell1_id, expected_tint; Builtins.cell2_id, expected_tint]
    in
    let extern_calls =
      let unpack_ecall = function
        | { e = ECall (cid, es, _) } -> cid, es
        | _ -> failwith "impossible"
      in
      List.map
        (fun (cid, es) ->
          let env', call = infer_exp env (call_sp cid es Span.default) in
          if not (equiv_effect env.current_effect env'.current_effect)
          then
            Console.error
            @@ "Function "
            ^ cid_to_string cid
            ^ " cannot be called inside a memop";
          unpack_ecall call)
        body.extern_calls
    in
    let ret = Option.map (infer_cr env) body.ret in
    MBComplex { b1; b2; cell1; cell2; extern_calls; ret }
;;

(* Check that the event id has already been defined, and that it has the
   expected paramters, and return an instantiated version of the constraints.
   Expects that params has already been instantiated. *)
let retrieve_constraints env span id params =
  match IdMap.find_opt id env.current_modul.vars with
  | Some
      { raw_ty =
          TFun
            { ret_ty = { raw_ty = TEvent }
            ; constraints = { contents = constraints }
            ; arg_tys
            }
      } ->
    let maps = fresh_maps () in
    let params2 = List.map (instantiator#visit_ty maps) arg_tys in
    let constraints = List.map (instantiator#visit_constr maps) constraints in
    let _ =
      (* FIXME: This isn't quite sufficient -- it won't catch e.g.
           an event which takes Array.t<<'a>>, but a hander which takes Array.t<<32>> *)
      try
        try_unify_lists
          (fun (_, ty1) ty2 -> unify_ty span ty1 ty2)
          params
          params2
      with
      | CannotUnify ->
        error_sp
          span
          (Printf.sprintf
             "Event %s was declared with arguments (%s) but its handler takes \
              arguments (%s)."
             (id_to_string id)
             (comma_sep (fun ty -> ty_to_string ty) params2)
             (comma_sep (fun (_, ty) -> ty_to_string ty) params))
    in
    constraints
  | Some ty ->
    error_sp span
    @@ Printf.sprintf
         "Handler declared for event %s, but %s has non-event type %s."
         (id_to_string id)
         (id_to_string id)
         (ty_to_string ty)
  | None ->
    error_sp span
    @@ Printf.sprintf
         "Handler %s has no corresponding event declaration."
         (id_to_string id)
;;

let infer_parser_action env (action, span) =
  match action with
  (* no typing to do in pskip *)
  | PSkip _ -> env, (action, span)
  | PAssign (lexp, exp) ->
    let env, inf_lexp, inf_lty = infer_exp env lexp |> textract in
    let env, inf_exp, inf_ety = infer_exp env exp |> textract in
    unify_ty exp.espan inf_lty inf_ety;
    env, (PAssign (inf_lexp, inf_exp), span)
  (* read and local have the same type rules. Read is just a 
     convenience tag with a specific form. *)
  | PRead (var_id, var_ty, pkt_exp) -> 
    (* ty id = read(pkt_exp); *)
    let env, inf_pkt_exp, inf_pkt_ety = infer_exp env pkt_exp |> textract in
    (* pkt_exp should have type bitstring *)
    unify_ty inf_pkt_exp.espan inf_pkt_ety (ty TBitstring) ;
    (* assume read always returns the right type, so 
       just add id to the env *)
    let env = add_locals env [var_id, var_ty] in
    env, (PRead(var_id, var_ty, inf_pkt_exp), span)
  | PLocal(id, ty, exp) -> 
    let env, inf_exp, inf_ety = infer_exp env exp |> textract in
    unify_ty exp.espan ty inf_ety;
    let env = add_locals env [id, ty] in
    env, (PLocal(id, ty, inf_exp), span)
;;

let rec infer_parser_step env (step, span) =
  match step with
  | PDrop -> step, span
  | PGen exp ->
    let _, inf_exp, inf_ety = infer_exp env exp |> textract in
    unify_raw_ty span inf_ety.raw_ty TEvent;
    PGen inf_exp, span
  | PCall exp ->
    (* Similar to checking an ECall, but we look for the function in the parser environment *)
    (match exp.e with
     | ECall (cid, args, _) ->
       let params = lookup_parser span env cid in
       let _, inf_args = infer_exps env args in
       List.iter2
         (fun (_, pty) arg ->
           unify_raw_ty arg.espan pty.raw_ty (Option.get arg.ety).raw_ty)
         params
         inf_args;
       let exp' = call_sp cid inf_args span in
       let exp' = { exp' with ety = Some (mk_ty TEvent) } in
       PCall exp', span
     | _ ->
       error_sp
         span
         "Parser bodies can only read, skip, generate, match, or call another \
          parser")
  | PMatch (exp, branches) ->
    let env, inf_exp, inf_ety = infer_exp env exp |> textract in
    let branches =
      List.map
        (fun (pat, block) ->
          let pty = infer_pattern env pat in
          unify_raw_ty span inf_ety.raw_ty pty;
          pat, infer_parser_block env block)
        branches
    in
    PMatch (inf_exp, branches), span

and infer_parser_block env (actions, step) =
  let env, inf_actions_rev =
    List.fold_left
      (fun (env, acc) action ->
        let env, action = infer_parser_action env action in
        env, action :: acc)
      (env, [])
      actions
  in
  let inf_step = infer_parser_step env step in
  List.rev inf_actions_rev, inf_step
;;

let rec infer_declaration
  (builtin_tys : Builtins.builtin_tys)
  (env : env)
  (effect_count : effect)
  (d : decl)
  : env * effect * decl
  =
  (* print_endline @@ "Inferring decl " ^ decl_to_string d; *)
  let d = subst_TNames env d in
  (* print_endline @@ "After subst_TNames "^ decl_to_string d; *)
  let env, effect_count, new_d =
    match d.d with
    | DSize (id, szo) ->
      let _ = Option.map (validate_size d.dspan env) szo in
      define_size id env, effect_count, d.d
    | DGlobal (id, ty, e) ->
      enter_level ();
      let _, inf_e, inf_ety =
        infer_exp { env with in_global_def = true } e |> textract
      in
      leave_level ();
      let ty = { ty with teffect = effect_count } in
      unify_ty d.dspan inf_ety ty;
      let ty = generalizer#visit_ty () ty in
      let env = define_const id ty env in
      env, FSucc effect_count, DGlobal (id, ty, inf_e)
    | DConst (id, ty, e) ->
      enter_level ();
      let _, inf_e, inf_ety = infer_exp env e |> textract in
      leave_level ();
      unify_ty d.dspan ty inf_ety;
      let ty = generalizer#visit_ty () ty in
      if is_global ty
      then
        error_sp d.dspan
        @@ "Type "
        ^ ty_to_string ty
        ^ " is global and must be created via a global declaration";
      let env = define_const id ty env in
      env, effect_count, DConst (id, ty, inf_e)
    | DExtern (id, ty) ->
      if is_global ty
      then
        error_sp ty.tspan
        @@ "Type "
        ^ ty_to_string ty
        ^ " is global and cannot be declared extern";
      let env = define_const id ty env in
      env, effect_count, DExtern (id, ty)
    | DSymbolic (id, ty) ->
      if is_global ty
      then
        error_sp ty.tspan
        @@ "Type "
        ^ ty_to_string ty
        ^ " is global and cannot be declared symbolic";
      let env = define_const id ty env in
      env, effect_count, DSymbolic (id, ty)
    | DEvent (id, annot, sort, constr_specs, params) ->
      let constrs, _ =
        spec_to_constraints env d.dspan FZero params constr_specs
      in
      let env = define_const id (mk_event_ty constrs params) env in
      env, effect_count, DEvent (id, annot, sort, constr_specs, params)
    | DHandler (id, s, body) ->
      enter_level ();
      let constraints = retrieve_constraints env d.dspan id (fst body) in
      let _, inf_body =
        let starting_env =
          { env with current_effect = FZero; constraints }
          |> define_const Builtins.this_id Builtins.this_ty
          |> define_const Builtins.ingr_port_id builtin_tys.ingr_port_ty
        in
        infer_body starting_env body
      in
      leave_level ();
      let inf_body = generalizer#visit_body () inf_body in
      env, effect_count, DHandler (id, s, inf_body)
    | DFun (id, ret_ty, constr_specs, body) ->
      (* a function declaration needs to have all the 
         local builtins available to it as well. *)
      let env' = env 
        |> define_const Builtins.this_id Builtins.this_ty 
        |> define_const Builtins.ingr_port_id builtin_tys.ingr_port_ty
      in
      enter_level ();
      let start_eff = fresh_effect () in
      let constraints, end_eff =
        spec_to_constraints env' d.dspan start_eff (fst body) constr_specs
      in
      let ret_effects =
        match end_eff with
        | None -> []
        | Some eff -> [eff]
      in
      let end_eff, constraints, ret_effects, inf_body =
        let fun_env, inf_body =
          infer_body
            { env' with
              current_effect = start_eff
            ; ret_ty = Some ret_ty
            ; constraints
            ; ret_effects
            }
            body
        in
        if (not fun_env.returned) && ret_ty.raw_ty <> TVoid
        then Console.error_position d.dspan "Non-void function may not return!";
        ( fun_env.current_effect
        , fun_env.constraints
        , fun_env.ret_effects
        , inf_body )
      in
      let end_eff =
        try
          List.fold_left (TyperZ3.find_max constraints) end_eff ret_effects
        with
        | TyperZ3.NoMax ->
          Console.error_position (snd body).sspan
          @@ "Unable to determine which globals have been used at end of \
              function; no control path obviously finishes at a later stage \
              than the others : "
          ^ Printing.stmt_to_string (snd body)
      in
      leave_level ();
      if not (TyperZ3.check_sat constraints)
      then
        error_sp d.dspan
        @@ "Function "
        ^ Id.name id
        ^ " violates ordering constraints";
      let fty : func_ty =
        { arg_tys = List.map (fun (_, ty) -> ty) (fst inf_body)
        ; ret_ty
        ; start_eff
        ; end_eff
        ; constraints = ref constraints
        }
        |> generalizer#visit_func_ty ()
      in
      let inf_body = generalizer#visit_body () inf_body in
      let env = define_const id (mk_ty @@ TFun fty) env in
      env, effect_count, DFun (id, ret_ty, constr_specs, inf_body)
    | DMemop (id, params, memop_body) ->
      enter_level ();
      let inf_body = infer_memop env params memop_body in
      leave_level ();
      let sz =
        match snd (List.hd params) with
        | { raw_ty = TInt sz } -> sz
        | _ -> failwith "Memops.ml should make this impossible"
      in
      let tmem =
        TMemop (List.length params, sz) |> generalizer#visit_raw_ty ()
      in
      let env = define_const id (ty tmem) env in
      env, effect_count, DMemop (id, params, inf_body)
    | DUserTy (id, sizes, ty) ->
      let new_env =
        let new_env = define_user_ty id sizes ty env in
        match ty.raw_ty with
        | TRecord lst ->
          let record_labels =
            List.fold_left
              (fun acc (l, _) -> StringMap.add l ty acc)
              new_env.record_labels
              lst
          in
          { new_env with record_labels }
        | TTable ttbl ->
          (* want to do something about the inner types ?*)
          let inf_tparam_tys =
            List.map
              (fun stated_ty ->
                (* let raw_ty = stated_ty.raw_ty in *)
                let inf_ty = inst stated_ty in
                (* not sure what this does... *)
                try_unify_ty stated_ty.tspan stated_ty inf_ty;
                inf_ty)
              ttbl.tparam_tys
          in
          let inf_tret_tys =
            List.map
              (fun stated_ty ->
                let inf_ty = inst stated_ty in
                (* not sure what this does... *)
                try_unify_ty stated_ty.tspan stated_ty inf_ty;
                inf_ty)
              ttbl.tret_tys
          in
          let inf_ty =
            { ty with
              raw_ty =
                TTable
                  { ttbl with
                    tparam_tys = inf_tparam_tys
                  ; tret_tys = inf_tret_tys
                  }
            }
          in
          define_user_ty id sizes inf_ty env
        | _ -> new_env
      in
      new_env, effect_count, DUserTy (id, sizes, ty)
    | DConstr (id, ty, params, e) ->
      enter_level ();
      let _, inf_e, inf_ety =
        let locals =
          List.fold_left
            (fun acc (id, ty) -> IdMap.add id ty acc)
            env.locals
            params
        in
        let inf_env = { env with in_global_def = true; locals } in
        infer_exp inf_env e |> textract
      in
      leave_level ();
      unify_ty d.dspan ty inf_ety;
      let fty =
        (* If called at top level the start/end effects don't matter; otherwise,
           the constructor doesn't involve any global stuff, so it's stateless. *)
        let eff = fresh_effect () in
        { arg_tys = List.map snd params
        ; ret_ty = ty
        ; start_eff = eff
        ; end_eff = eff
        ; constraints = ref []
        }
        |> generalizer#visit_func_ty ()
      in
      let env = define_constructor id fty env in
      env, effect_count, DConstr (id, ty, params, inf_e)
    | DModule (id, intf, ds) ->
      let m_env, effect_count, ds =
        let subenv =
          { env with
            current_modul = empty_modul
          ; parents = env.current_modul :: env.parents
          }
        in
        List.fold_left
          (fun (env, effect_count, ds) d ->
            let env, effect_count, d =
              infer_declaration builtin_tys env effect_count d
            in
            env, effect_count, d :: ds)
          (subenv, effect_count, [])
          ds
      in
      let ds = List.rev ds in
      let env =
        if List.is_empty intf
        then define_submodule id m_env.current_modul env
        else add_interface d.dspan env id intf m_env.current_modul
      in
      let env = { env with record_labels = m_env.record_labels } in
      (* print_endline @@ "After module " ^ id_to_string id ^ ", env is";
      print_endline @@ modul_to_string ~show_defs:false env.current_modul; *)
      env, effect_count, DModule (id, intf, ds)
    | DModuleAlias (id1, e, cid1, cid2) ->
      let env, inf_e, inf_ety = infer_exp env e |> textract in
      unify_raw_ty e.espan inf_ety.raw_ty TBool;
      let m =
        let m1 = lookup_module d.dspan env cid1 in
        let m2 = lookup_module d.dspan env cid2 in
        ensure_equiv_modul d.dspan m1 m2;
        re_abstract_modul id1 m1
      in
      ( define_submodule id1 m env
      , effect_count
      , DModuleAlias (id1, inf_e, cid1, cid2) )
    | DAction(id, ret_ty, (params, action_body)) -> 
      let check_e env expected_ty e =
        let _, inf_e, inf_ety = infer_exp env e |> textract in
        try_unify_ty e.espan inf_ety expected_ty;
        inf_e
      in
      (* check and infer action body expression types *)
      enter_level ();
      let action_env = add_locals env params in
      let inf_action_body = List.map2 (check_e action_env) ret_ty action_body in
      leave_level ();
      (* add variable to type env *)
      let acn_ty =
        TAction
          { aarg_tys = List.map (fun (_, ty) -> ty) params
          ; aret_tys = ret_ty
          }
      in
      let env = define_const id (mk_ty @@ acn_ty) env in
      (* return action with type annotations *)
      ( env
      , effect_count
      , DAction (id, ret_ty, (params, inf_action_body)) )
    | DActionConstr (id, ret_ty, const_params, (params, action_body)) ->
      let check_e env expected_ty e =
        let _, inf_e, inf_ety = infer_exp env e |> textract in
        try_unify_ty e.espan inf_ety expected_ty;
        inf_e
      in
      enter_level ();
      (* add const and dynamic params to locals *)
      let action_env = add_locals env (const_params @ params) in
      (* check and infer action body expression types *)
      let inf_action_body = List.map2 (check_e action_env) ret_ty action_body in
      leave_level ();
      (* add variable to type env *)
      let acn_ctor_ty =
        TActionConstr
          { aconst_param_tys = List.map (fun (_, ty) -> ty) const_params
          ; aacn_ty = {aarg_tys = List.map (fun (_, ty) -> ty) params
          ; aret_tys = ret_ty};
          }
      in
      let env = define_const id (mk_ty @@ acn_ctor_ty) env in
      ( env
      , effect_count
      , DActionConstr (id, ret_ty, const_params, (params, inf_action_body)) )
    | DParser (id, params, parser) ->
      enter_level ();
      (* a parser may branch on the ingress port *)
      let ingress_port_param = (Builtins.ingr_port_id, builtin_tys.ingr_port_ty) in
      let parser_env =
        add_locals env (ingress_port_param::params) 
        |> define_parser Builtins.lucid_parse_id [(Id.create "pkt", ty TBitstring)]
      in
      
      let inf_parser = infer_parser_block parser_env parser in
      let env = define_parser id params env in
      env, effect_count, DParser (id, params, inf_parser)
  in
  let new_d = { d with d = new_d } in
  Wellformed.check_qvars new_d;
  env, effect_count, new_d
;;

let ensure_fully_typed ds =
  let v =
    object (self)
      inherit [_] s_iter

      method! visit_value _ v =
        match v.vty with
        | Some _ -> self#visit_v () v.v
        | None ->
          error_sp v.vspan
          @@ Printf.sprintf
               "Internal error: value %s has no type!"
               (Printing.value_to_string v)

      method! visit_exp _ exp =
        match exp.ety with
        | Some _ -> self#visit_e () exp.e
        | None ->
          error_sp exp.espan
          @@ Printf.sprintf
               "Internal error: expression %s has no type!"
               (Printing.exp_to_string exp)
    end
  in
  v#visit_decls () ds
;;



let infer_prog (builtin_tys : Builtins.builtin_tys) (decls : decls) : decls =
  let decls = instantiate_prog decls in  
  let (env : env) = default_env in
  let infer_d (env, count, ds) d =
    let env, count, d = infer_declaration builtin_tys env count d in
    env, count, d :: ds
  in
  let _, _, inf_decls = List.fold_left infer_d (env, FZero, []) decls in
  ensure_fully_typed inf_decls;
  let inf_decls = unsubst_TAbstracts inf_decls in
  (* exit 1; *)
  let decls = List.rev inf_decls in
  (* now, run through module-specific type checkers *)
  List.iter (fun typer -> typer decls) (Builtins.builtin_typers);
  (* Tables.table_type_checker decls; *)
  decls
;;
