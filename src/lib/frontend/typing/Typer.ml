open Syntax
open SyntaxUtils
open Batteries
open Collections
open Printing
open TyperUtil
open TyperUnify
open TyperInstGen
open TyperModules

let infer_value v =
  let vty =
    match v.v with
    | VBool _ -> TBool
    | VInt n -> TInt (IConst (Integer.size n))
    | VGlobal _ | VEvent _ | VGroup _ ->
      failwith "Cannot write values of these types"
  in
  { v with vty = Some vty }
;;

let infer_pattern p =
  match p with
  | PWild -> fresh_type ()
  | PNum _ -> TInt (fresh_size ())
  | PBit ps -> TInt (IConst (List.length ps))
;;

let rec infer_exp (env : env) (e : exp) : env * exp =
  match e.e with
  | EVar cid ->
    let inst t = instantiator#visit_raw_ty (fresh_maps ()) t in
    ( env
    , (match CidMap.find_opt cid env.locals with
      | Some t -> Some (inst t) |> wrap e
      | None ->
        (match CidMap.find_opt cid env.consts with
        | Some t -> Some (inst t) |> wrap e
        | None -> error_sp e.espan ("Unbound variable " ^ Cid.to_string cid))) )
  | EVal v -> env, (infer_value v).vty |> wrap e
  | EInt (z, szo) ->
    ( env
    , (match szo with
      | None ->
        let new_size = fresh_size () in
        { e with e = EInt (z, Some new_size); ety = Some (TInt new_size) }
      | Some sz ->
        validate_size e.espan env sz;
        Some (TInt sz) |> wrap e) )
  | EOp (op, args) -> infer_op env e.espan op args
  | EHash (size, es) ->
    if List.length es < 2
    then error_sp e.espan "Must pass at least two arguments to hash";
    validate_size e.espan env size;
    let env, inf_es = infer_exps env es in
    let hd = List.hd inf_es in
    unify_ty hd.espan (Option.get hd.ety) (TInt (IConst 32));
    env, { e with e = EHash (size, es); ety = Some (TInt size) }
  | ECall (f, args) ->
    let _, _, inferred_fty =
      (* Get type of f as if we used the var rule for the function *)
      infer_exp env { e with e = EVar f } |> textract
    in
    let env, inferred_args = infer_exps env args in
    let fty : func_ty =
      { arg_tys = List.map (fun arg -> Option.get arg.ety) inferred_args
      ; ret_ty = fresh_type ()
      ; start_eff = env.current_effect
      ; end_eff = fresh_effect ()
      ; constraints = ref []
      }
    in
    unify_ty e.espan (TFun fty) inferred_fty;
    let new_env =
      match env.ret_ty with
      | None ->
        (* Only None when we're inside a handler. In this case, we're
           not doing inference, we're just checking that the user's constraints
           imply the function's constraints *)
        if not (Typer_Z3.check_implies env.constraints !(fty.constraints))
        then
          error_sp
            e.espan
            "Function call in a handler may violate global order. (Did you \
             include all the necessary constraints when declaring the event?)";
        { env with current_effect = fty.end_eff }
      | Some _ ->
        (* If it's Some, we're in a function or memop. We should record the new
           constraints and make sure they're still satisfiable *)
        let new_constraints = !(fty.constraints) @ env.constraints in
        if not (Typer_Z3.check_sat new_constraints)
        then error_sp e.espan "Function call violates the global order";
        { env with current_effect = fty.end_eff; constraints = new_constraints }
    in
    new_env, { e with e = ECall (f, inferred_args); ety = Some fty.ret_ty }
  | EProj (e, label) ->
    let env, inf_e = infer_exp env e in
    let expected_gty_cid =
      match StringMap.find_opt label env.global_labels with
      | Some x -> x
      | None -> error_sp e.espan @@ "Unknown label " ^ label
    in
    let sizes, labels =
      match CidMap.find_opt expected_gty_cid env.global_tys with
      | Some (sizes, params) ->
        let sizes = List.map (fun id -> IVar (QVar id)) sizes in
        let maps = fresh_maps () in
        let sizes = List.map (instantiator#visit_size maps) sizes in
        sizes, (inst_ivars maps.size_map)#visit_params () params
      | None ->
        error_sp e.espan "Somehow had label entry for a nonexistent type?"
    in
    let ret_eff = fresh_effect () in
    let expected_gty = TGlobal ((expected_gty_cid, sizes), ret_eff) in
    unify_ty e.espan expected_gty (Option.get inf_e.ety);
    let idx, (_, ty) =
      List.findi (fun _ (id, _) -> Id.name id = label) labels
    in
    let rty =
      match ty.raw_ty with
      | TGlobal (gty, _) -> TGlobal (gty, wrap_effect ret_eff [0; idx])
      | x -> x
    in
    env, { e with e = EProj (inf_e, label); ety = Some rty }
  | ERecord entries ->
    let labels, es = List.split entries in
    let env, inf_es = infer_exps env es in
    let expected_gty_cid =
      match StringMap.find_opt (List.hd labels) env.global_labels with
      | Some x -> x
      | None -> error_sp e.espan @@ "Unknown label " ^ List.hd labels
    in
    let sizes, expected_labels =
      match CidMap.find_opt expected_gty_cid env.global_tys with
      | Some (sizes, params) ->
        let sizes = List.map (fun id -> IVar (QVar id)) sizes in
        let maps = fresh_maps () in
        let sizes = List.map (instantiator#visit_size maps) sizes in
        sizes, (inst_ivars maps.size_map)#visit_params () params
      | None ->
        error_sp e.espan "Somehow had label entry for a nonexistent type?"
    in
    verify_record_entries env e.espan expected_gty_cid labels expected_labels;
    let inf_entries = List.combine labels inf_es in
    List.iter
      (fun (l_id, ty) ->
        let e = List.assoc (Id.name l_id) inf_entries in
        unify_ty e.espan (Option.get e.ety) ty.raw_ty)
      expected_labels;
    let ret_eff = fresh_effect () (* This should never actually get used *) in
    let ret_ty = TGlobal ((expected_gty_cid, sizes), ret_eff) in
    env, { e with e = ERecord inf_entries; ety = Some ret_ty }

and infer_op env span op args =
  let env, ty, new_args =
    match op, args with
    | Not, [e] ->
      let env, inf_e, inf_ety = infer_exp env e |> textract in
      unify_ty span inf_ety TBool;
      env, TBool, [inf_e]
    | (And | Or), [e1; e2] ->
      let env, inf_e1, inf_ety1 = infer_exp env e1 |> textract in
      let env, inf_e2, inf_ety2 = infer_exp env e2 |> textract in
      unify_ty span inf_ety1 TBool;
      unify_ty span inf_ety2 TBool;
      env, TBool, [inf_e1; inf_e2]
    | (Eq | Neq), [e1; e2] ->
      let env, inf_e1, inf_ety1 = infer_exp env e1 |> textract in
      let env, inf_e2, inf_ety2 = infer_exp env e2 |> textract in
      unify_ty span inf_ety1 inf_ety2;
      env, TBool, [inf_e1; inf_e2]
    | (Less | More | Leq | Geq), [e1; e2] ->
      let tsize = fresh_size () in
      let env, inf_e1, inf_ety1 = infer_exp env e1 |> textract in
      let env, inf_e2, inf_ety2 = infer_exp env e2 |> textract in
      unify_ty span inf_ety1 (TInt tsize);
      unify_ty span inf_ety2 (TInt tsize);
      env, TBool, [inf_e1; inf_e2]
    | (Plus | Sub | SatSub | BitAnd | BitOr), [e1; e2] ->
      let tsize = fresh_size () in
      let env, inf_e1, inf_ety1 = infer_exp env e1 |> textract in
      let env, inf_e2, inf_ety2 = infer_exp env e2 |> textract in
      unify_ty span inf_ety1 (TInt tsize);
      unify_ty span inf_ety2 (TInt tsize);
      env, TInt tsize, [inf_e1; inf_e2]
    | (LShift | RShift), [e1; e2] ->
      let tsize = fresh_size () in
      let env, inf_e1, inf_ety1 = infer_exp env e1 |> textract in
      let env, inf_e2, inf_ety2 = infer_exp env e2 |> textract in
      unify_ty span inf_ety1 (TInt tsize);
      unify_ty span inf_ety2 (TInt (fresh_size ()));
      env, TInt tsize, [inf_e1; inf_e2]
    | Conc, [e1; e2] ->
      let tsize1 = fresh_size () in
      let tsize2 = fresh_size () in
      let env, inf_e1, inf_ety1 = infer_exp env e1 |> textract in
      let env, inf_e2, inf_ety2 = infer_exp env e2 |> textract in
      unify_ty span inf_ety1 (TInt tsize1);
      unify_ty span inf_ety2 (TInt tsize2);
      env, TInt (add_sizes tsize1 tsize2), [inf_e1; inf_e2]
    | Cast out_size, [e] ->
      let tsize = fresh_size () in
      let env, inf_e, inf_ety = infer_exp env e |> textract in
      unify_ty span inf_ety (TInt tsize);
      env, TInt out_size, [inf_e]
    | Slice (hi, lo), [e] ->
      (* FIXME: To be sound, we should also require that the size is at most hi.
                But we can't express that at the moment. *)
      if lo < 0 || lo > hi
      then error_sp span "Bad arguments to slice operator"
      else (
        let tsize = fresh_size () in
        let env, inf_e, inf_ety = infer_exp env e |> textract in
        unify_ty span inf_ety (TInt tsize);
        env, TInt (IConst (hi - lo + 1)), [inf_e])
    | _ ->
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
;;

let rec infer_statement (env : env) (s : statement) : env * statement =
  (* (match s.s with
  | SSeq _ | SNoop -> ()
  | _ -> print_endline @@ "Inferring " ^ stmt_to_string s); *)
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
        | Some TVoid, None -> return env, SRet None
        | Some TVoid, Some _ -> err "Nonempty return inside void function body"
        | Some _, None -> err "Empty return inside non-void function body"
        | Some ty, Some e ->
          let env, inf_e, inf_ety = infer_exp env e |> textract in
          unify_ty s.sspan ty inf_ety;
          return env, SRet (Some inf_e)
      end
    | SLocal (id, ty, e) ->
      let env, inf_e, ety = infer_exp env e |> textract in
      unify_ty s.sspan ty.raw_ty ety;
      (match TyTQVar.strip_links ety with
      | TVoid ->
        error_sp s.sspan
        @@ "Cannot assign result of void function to variable: "
        ^ stmt_to_string s
      | _ -> ());
      ( { env with locals = CidMap.add (Id id) ty.raw_ty env.locals }
      , SLocal (id, ty, inf_e) )
    | SAssign (id, e) ->
      let env, inf_e, ety = infer_exp env e |> textract in
      (match CidMap.find_opt (Id id) env.locals with
      | Some rty -> unify_ty s.sspan rty ety
      | None ->
        (match CidMap.find_opt (Id id) env.consts with
        | Some _ ->
          error_sp s.sspan @@ "Assignment to constant variable " ^ Id.name id
        | None ->
          error_sp s.sspan @@ "Assignment to unbound variable " ^ Id.name id));
      (match TyTQVar.strip_links ety with
      | TVoid ->
        error_sp s.sspan
        @@ "Cannot assign result of void function to variable: "
        ^ stmt_to_string s
      | _ -> ());
      env, SAssign (id, inf_e)
    | SPrintf (str, es) ->
      let expected_tys = extract_print_tys s.sspan str in
      if List.length expected_tys <> List.length es
      then error_sp s.sspan "Incorrect number of arguments to printf statement";
      let env, inf_es = infer_exps env es in
      List.iter2
        (fun e ty -> unify_ty s.sspan (Option.get e.ety) ty)
        inf_es
        expected_tys;
      env, SPrintf (str, inf_es)
    | SIf (e, s1, s2) ->
      let env, inf_e, inf_ety = infer_exp env e |> textract in
      unify_ty e.espan TBool inf_ety;
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
             Typer_Z3.find_max
               env.constraints
               env1.current_effect
               env2.current_effect
           with
          | Typer_Z3.NoMax ->
            Console.error_position
              s.sspan
              "Unable to determine which globals have been used at end of if \
               statement; neither branch ends obviously later than the other.")
      in
      { env with current_effect }, SIf (inf_e, inf_s1, inf_s2)
    | SGen (b, e) ->
      let env, inf_e, ety = infer_exp env e |> textract in
      unify_ty s.sspan ety (TEvent b);
      env, SGen (b, inf_e)
    | SSeq (s1, s2) ->
      let env, inf_s1 = infer_statement env s1 in
      let env, inf_s2 = infer_statement env s2 in
      env, SSeq (inf_s1, inf_s2)
    | SMatch (es, bs) ->
      let env, inf_es = infer_exps env es in
      let env, inf_bs =
        infer_branches env s (List.map (fun e -> e.ety |> Option.get) inf_es) bs
      in
      env, SMatch (inf_es, inf_bs)
  in
  env, { s with s = stmt }

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
    | _ -> List.iter2 (unify_ty s.sspan) etys (List.map infer_pattern pats)
  in
  let infer_branch (pats, s) =
    check_pats pats;
    let env1, inf_s = infer_statement env s in
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
            try Typer_Z3.find_max constrs env1.current_effect acc_eff with
            | Typer_Z3.NoMax ->
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
  let locals =
    List.fold_left
      (fun acc (id, ty) -> CidMap.add (Id id) ty.raw_ty acc)
      env.locals
      params
  in
  let env, s = infer_statement { env with locals } s in
  env, (params, s)
;;

let infer_memop span env (params, s) =
  (* First, make sure we have the right number/type of arguments *)
  let arg1size = fresh_size () in
  let arg1ty = TInt arg1size in
  let arg2ty = fresh_type () in
  let env, id1, id2 =
    match params with
    | [(id1, ty1); (id2, ty2)] ->
      let locals = CidMap.add (Id id1) ty1.raw_ty env.locals in
      let locals = CidMap.add (Id id2) ty2.raw_ty locals in
      unify_ty ty1.tspan ty1.raw_ty arg1ty;
      unify_ty ty2.tspan ty2.raw_ty arg2ty;
      { env with locals }, id1, id2
    | _ -> error_sp span "Wrong number of parameters to memop"
  in
  (* Do regular typechecking of the body *)
  let _, inf_s = infer_statement { env with ret_ty = Some arg1ty } s in
  (* Do grammar checking of the body *)
  let check_return e =
    ignore
    @@ check_e
         (Id id1)
         (Id id2)
         (* TODO: There are more ops that are allowed in return statements, add them here *)
           (function
           | Plus | Sub -> true
           | _ -> false)
         (false, false)
         e
  in
  (match inf_s.s with
  | SRet (Some e) -> check_return e
  | SIf (test, { s = SRet (Some e1) }, { s = SRet (Some e2) }) ->
    check_test id1 id2 test;
    check_return e1;
    check_return e2
  | _ -> error_sp span "Invalid grammar for body of memop");
  arg1size, arg2ty, (params, inf_s)
;;

(* Check that the event id has already been defined, and that it has the
   expected paramters, and return an instantiated version of the constraints.
   Expects that params has already been instantiated. *)
let retrieve_constraints env span id params =
  match CidMap.find_opt (Id id) env.handlers with
  | None ->
    error_sp span
    @@ Printf.sprintf
         "Handler %s has no corresponding event declaration."
         (id_to_string id)
  | Some (constraints, params2) ->
    let maps = fresh_maps () in
    let params2 = instantiator#visit_params maps params2 in
    let constraints = List.map (instantiator#visit_constr maps) constraints in
    let _ =
      try
        unify_lists
          (fun (_, ty1) (_, ty2) -> unify_ty span ty1.raw_ty ty2.raw_ty)
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
             (comma_sep (fun (_, ty) -> ty_to_string ty) params2)
             (comma_sep (fun (_, ty) -> ty_to_string ty) params))
    in
    constraints
;;

let rec infer_declaration (env : env) (effect_count : effect) (d : decl)
    : env * effect * decl
  =
  let env, effect_count, new_d =
    match d.d with
    | DSize (id, size) ->
      validate_size d.dspan env size;
      ( { env with sizes = CidSet.add (Id id) env.sizes } |> def KSize id
      , effect_count
      , d.d )
    | DGlobal (id, (ty_id, ty_sizes), constr_id, args) ->
      List.iter (validate_size d.dspan env) ty_sizes;
      let _, inf_args = infer_exps env args in
      let constr_sizes, constr_arg_tys =
        match CidMap.find_opt constr_id env.constructors with
        | None ->
          error_sp d.dspan @@ "Unbound constructor " ^ cid_to_string constr_id
        | Some (ret_id, sizes, arg_tys) ->
          if not (Cid.equals ret_id ty_id)
          then
            error_sp d.dspan
            @@ Printf.sprintf
                 "Constructor %s returns type %s, not %s"
                 (cid_to_string constr_id)
                 (cid_to_string ret_id)
                 (cid_to_string ty_id)
          else if List.length sizes <> List.length ty_sizes
          then
            error_sp d.dspan
            @@ "Wrong number of size arguments to type "
            ^ cid_to_string ty_id
          else (
            let maps = fresh_maps () in
            let subst rty =
              let v =
                object
                  inherit [_] s_map

                  method! visit_IUser _ cid =
                    match cid with
                    | Compound _ -> IUser cid
                    | Id id ->
                      if List.mem id sizes then IVar (QVar id) else IUser cid
                end
              in
              instantiator#visit_raw_ty maps (v#visit_raw_ty () rty)
            in
            let sizes =
              sizes
              |> List.map (fun id -> IVar (QVar id))
              |> List.map (instantiator#visit_size maps)
            in
            sizes, List.map subst arg_tys)
      in
      (try unify_lists (unify_size d.dspan) ty_sizes constr_sizes with
      | CannotUnify ->
        error_sp
          d.dspan
          "Return type of constructor has wrong number of size arguments");
      (try
         unify_lists
           (fun ty e -> unify_ty e.espan ty (Option.get e.ety))
           constr_arg_tys
           inf_args
       with
      | CannotUnify ->
        error_sp d.dspan
        @@ Printf.sprintf
             "Incorrect argument types to constructor: expected %s but got %s"
             (Printing.list_to_string Printing.raw_ty_to_string constr_arg_tys)
             (Printing.list_to_string
                (fun e -> Printing.raw_ty_to_string (Option.get e.ety))
                inf_args));
      let gty = ty_id, ty_sizes in
      let inf_ty = TGlobal (gty, effect_count) in
      ensure_concrete (ty inf_ty);
      let env =
        { env with consts = CidMap.add (Id id) inf_ty env.consts }
        |> def KConst id
      in
      env, FSucc effect_count, DGlobal (id, gty, constr_id, inf_args)
    | DConst (id, ty, e) ->
      let _, inf_e, inf_ety = infer_exp env e |> textract in
      unify_ty d.dspan ty.raw_ty inf_ety;
      (match ty.raw_ty with
      | TGlobal ((cid, _), _) when has_global_entry env cid ->
        error_sp d.dspan
        @@ "Type "
        ^ cid_to_string cid
        ^ " is global and must be created via a global declaration"
      | _ -> ensure_concrete ~check_effects:false ty);
      let env =
        { env with consts = CidMap.add (Id id) ty.raw_ty env.consts }
        |> def KConst id
      in
      env, effect_count, DConst (id, ty, inf_e)
    | DExtern (id, ty) ->
      (match TyTQVar.strip_links ty.raw_ty with
      | TGlobal ((cid, _), _) when has_global_entry env cid ->
        error_sp ty.tspan
        @@ "Type "
        ^ cid_to_string cid
        ^ " is global and cannot be declared extern"
      | _ -> ());
      ensure_concrete ty;
      let env =
        { env with consts = CidMap.add (Id id) ty.raw_ty env.consts }
        |> def KConst id
      in
      env, effect_count, DExtern (id, ty)
    | DGroup (id, es) ->
      let _, inf_args = infer_exps env es in
      (* Locations are just ints for now *)
      List.iter
        (fun e -> unify_ty d.dspan (Option.get e.ety) (TInt (IConst 32)))
        inf_args;
      let env =
        { env with consts = CidMap.add (Id id) TGroup env.consts }
        |> def KConst id
      in
      env, effect_count, DGroup (id, inf_args)
    | DEvent (id, sort, constr_specs, params) ->
      (* List.iter (fun (_, ty) -> ensure_concrete ~event:true ty) params; *)
      let constrs, _ =
        spec_to_constraints env d.dspan FZero params constr_specs
      in
      let env =
        { env with
          handlers = CidMap.add (Id id) (constrs, params) env.handlers
        ; consts = CidMap.add (Id id) (mk_event_ty constrs params) env.consts
        }
        |> def KConst id
        |> def KHandler id
      in
      env, effect_count, DEvent (id, sort, constr_specs, params)
    | DHandler (id, body) ->
      enter_level ();
      let body = instantiator#visit_body (fresh_maps ()) body in
      let constraints = retrieve_constraints env d.dspan id (fst body) in
      let _, inf_body =
        infer_body
          { env with
            current_effect = FZero
          ; consts =
              CidMap.add (Id Builtins.this_id) Builtins.this_ty env.consts
          ; constraints
          }
          body
      in
      leave_level ();
      (* Unlike functions, events/handlers must _not_ be polymorphic, except
         for global arguments *)
      (* I think this is more subtle -- polymorphism in a handler is ok so
         long as all polymorphic things appear in a TGlobal somewhere, since those
         sizes will be concretized when we do global arg elimination *)
      (* TODO: Actually check for that *)
      (* List.iter (fun (_, ty) -> ensure_concrete ~event:true ty) (fst body); *)
      env, effect_count, DHandler (id, inf_body)
    | DFun (id, ret_ty, constr_specs, body) ->
      enter_level ();
      let start_eff = fresh_effect () in
      let ret_ty, body =
        let maps = fresh_maps () in
        instantiator#visit_ty maps ret_ty, instantiator#visit_body maps body
      in
      let constraints, end_eff =
        spec_to_constraints env d.dspan start_eff (fst body) constr_specs
      in
      let ret_effects =
        match end_eff with
        | None -> []
        | Some eff -> [eff]
      in
      let end_eff, constraints, ret_effects, inf_body =
        let fun_env, inf_body =
          infer_body
            { env with
              current_effect = start_eff
            ; ret_ty = Some ret_ty.raw_ty
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
          List.fold_left (Typer_Z3.find_max constraints) end_eff ret_effects
        with
        | Typer_Z3.NoMax ->
          Console.error_position (snd body).sspan
          @@ "Unable to determine which globals have been used at end of \
              function; no control path obviously finishes at a later stage \
              than the others : "
          ^ Printing.stmt_to_string (snd body)
      in
      leave_level ();
      (* Make sure that all paths lead to a return statement *)
      if not (Typer_Z3.check_sat constraints)
      then
        error_sp d.dspan
        @@ "Function "
        ^ Id.name id
        ^ " violates ordering constraints";
      let fty : func_ty =
        { arg_tys = List.map (fun (_, ty) -> ty.raw_ty) (fst inf_body)
        ; ret_ty = ret_ty.raw_ty
        ; start_eff
        ; end_eff
        ; constraints = ref constraints
        }
      in
      let fty = generalizer#visit_raw_ty () (TFun fty) in
      let env =
        { env with consts = CidMap.add (Id id) fty env.consts } |> def KConst id
      in
      env, effect_count, DFun (id, ret_ty, constr_specs, inf_body)
    | DMemop (id, body) ->
      enter_level ();
      let body = instantiator#visit_body (fresh_maps ()) body in
      let inf_size, inf_ty, inf_memop = infer_memop d.dspan env body in
      leave_level ();
      let tmem = generalizer#visit_raw_ty () (TMemop (inf_size, inf_ty)) in
      let env =
        { env with consts = CidMap.add (Id id) tmem env.consts }
        |> def KConst id
      in
      env, effect_count, DMemop (id, inf_memop)
    | DGlobalTy (id, size_ids, params) ->
      (* TODO: Should validate that params don't contain any unbound qvars *)
      let ensure_exists (_, ty) =
        match ty.raw_ty with
        | TGlobal ((cid, _), _) ->
          if not (CidMap.mem cid env.global_tys)
          then error_sp ty.tspan @@ "Unknown global type " ^ cid_to_string cid
        | _ -> ()
      in
      List.iter ensure_exists params;
      let env =
        { env with
          global_tys = CidMap.add (Id id) (size_ids, params) env.global_tys
        }
        |> def KGlobalTy id
      in
      let env =
        List.fold_left
          (fun acc (l, _) -> add_global_label acc d.dspan id l)
          env
          params
      in
      env, effect_count, d.d
    | DConstr { constr_id; ty_id; size_args; params; body } ->
      let expected_sizes, labels =
        match CidMap.find_opt ty_id env.global_tys with
        | None ->
          error_sp d.dspan @@ "Unbound global type " ^ cid_to_string ty_id
        | Some (expected, labels) ->
          if List.length expected <> List.length size_args
          then
            error_sp
              d.dspan
              "Constructor takes a different number of size arguments than its \
               associated type"
          else if List.length body <> List.length labels
          then error_sp d.dspan "Constructor defines the wrong number of labels"
          else expected, labels
      in
      validate_constr_body
        (List.map Cid.id expected_sizes)
        (List.map Cid.id size_args)
        labels
        body;
      let body =
        let locals =
          List.fold_left
            (fun acc (id, ty) -> CidMap.add (Id id) ty.raw_ty acc)
            env.locals
            params
        in
        let sizes =
          List.fold_left
            (fun acc id -> CidSet.add (Id id) acc)
            env.sizes
            size_args
        in
        List.map
          (infer_declaration { env with locals; sizes } effect_count)
          body
        |> List.map (fun (_, _, d) -> d)
      in
      let env =
        { env with
          constructors =
            CidMap.add
              (Id constr_id)
              (ty_id, size_args, List.map (fun (_, ty) -> ty.raw_ty) params)
              env.constructors
        }
        |> def KConstr constr_id
      in
      env, effect_count, DConstr { constr_id; ty_id; size_args; params; body }
    | DModule (id, intf, ds) ->
      wellformed_interface env intf;
      let m_env, effect_count, ds =
        List.fold_left
          (fun (env, effect_count, ds) d ->
            let env, effect_count, d = infer_declaration env effect_count d in
            env, effect_count, d :: ds)
          ({ env with module_defs = KindSet.empty }, effect_count, [])
          ds
      in
      let ds = List.rev ds in
      let env = add_module_defs id intf env m_env in
      env, effect_count, DModule (id, intf, ds)
  in
  env, effect_count, { d with d = new_d }
;;

let infer_prog (decls : decls) : decls =
  let (env : env) = default_env in
  let infer_d (env, count, ds) d =
    let env, count, d = infer_declaration env count d in
    env, count, d :: ds
  in
  let _, _, inf_decls = List.fold_left infer_d (env, FZero, []) decls in
  List.rev inf_decls
;;
