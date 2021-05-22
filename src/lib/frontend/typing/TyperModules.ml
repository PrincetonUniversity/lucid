open Syntax
open SyntaxUtils
open Collections
open Batteries
open TyperUtil

let def k id env =
  { env with module_defs = KindSet.add (k, Id id) env.module_defs }
;;

let prefixer =
  object (self)
    inherit [_] s_map

    method! visit_IUser (f, kset) cid =
      if KindSet.mem (KSize, cid) kset then IUser (f cid) else IUser cid

    method! visit_global_ty (f, kset) (cid, sizes) =
      let sizes = List.map (self#visit_size (f, kset)) sizes in
      if KindSet.mem (KGlobalTy, cid) kset then f cid, sizes else cid, sizes
  end
;;

(* After going through a module body, add all the new definitions to the old
   environment, but with the module id as a prefix *)
let add_all_module_defs m_id old_env m_env =
  let prefix cid = Compound (m_id, cid) in
  let safe_prefix k cid =
    if KindSet.mem (k, cid) m_env.module_defs then prefix cid else cid
  in
  let prefix_rty = prefixer#visit_raw_ty (prefix, m_env.module_defs) in
  let prefix_params = prefixer#visit_params (prefix, m_env.module_defs) in
  let prefixed_maps =
    KindSet.fold
      (fun (k, cid) acc ->
        match k with
        | KSize -> { acc with sizes = CidSet.add (prefix cid) acc.sizes }
        | KConst ->
          let ty = CidMap.find cid m_env.consts |> prefix_rty in
          { acc with consts = CidMap.add (prefix cid) ty acc.consts }
        | KHandler ->
          let handler =
            let x, params = CidMap.find cid m_env.handlers in
            x, prefix_params params
          in
          { acc with handlers = CidMap.add (prefix cid) handler acc.handlers }
        | KConstr ->
          let new_constr =
            let cid, x, argtys = CidMap.find cid m_env.constructors in
            safe_prefix KGlobalTy cid, x, List.map prefix_rty argtys
          in
          { acc with
            constructors = CidMap.add (prefix cid) new_constr acc.constructors
          }
        | KGlobalTy ->
          let gty =
            let x, params = CidMap.find cid m_env.global_tys in
            x, prefix_params params
          in
          { acc with global_tys = CidMap.add (prefix cid) gty acc.global_tys })
      m_env.module_defs
      old_env
  in
  { prefixed_maps with
    module_defs =
      KindSet.union
        old_env.module_defs
        (KindSet.map (fun (k, id) -> k, prefix id) m_env.module_defs)
  ; global_labels = StringMap.map (safe_prefix KGlobalTy) m_env.global_labels
  }
;;

(* Ensure the interface actually matches the declarations in the module body *)
let rec validate_interface prefix env interface =
  let err_id span kind_str id =
    error_sp span
    @@ Printf.sprintf
         "No %s named %s defined in module body"
         kind_str
         (Printing.id_to_string id)
  in
  let equiv_tys mapping ty1 ty2 =
    let subst = subst_ivars mapping in
    equiv_raw_ty
      (subst#visit_raw_ty () ty1.raw_ty)
      (subst#visit_raw_ty () ty2.raw_ty)
  in
  List.iter
    (fun spec ->
      match spec.ispec with
      | InSize id ->
        if not (KindSet.mem (KSize, prefix id) env.module_defs)
        then err_id spec.ispan "size" id
      | InVar (id, ty) ->
        if not (KindSet.mem (KConst, prefix id) env.module_defs)
        then err_id spec.ispan "value" id;
        ensure_concrete ~check_effects:false ty;
        let actual_ty = CidMap.find (prefix id) env.consts in
        let equiv =
          (* Hack to get around the fact that if ty is a global type, it'll have
             a dummy effect. *)
          let insted =
            TyperInstGen.instantiator#visit_raw_ty
              (TyperInstGen.fresh_maps ())
              ty.raw_ty
          in
          try
            TyperUnify.unify_ty spec.ispan insted actual_ty;
            true
          with
          | _ -> false
        in
        if not equiv
        then
          error_sp spec.ispan
          @@ Printf.sprintf
               "Value %s has type %s, but interface specified type %s"
               (Printing.id_to_string id)
               (Printing.raw_ty_to_string actual_ty)
               (Printing.ty_to_string ty)
      | InGlobalTy (id, size_ids, params) ->
        if not (KindSet.mem (KGlobalTy, prefix id) env.module_defs)
        then err_id spec.ispan "global type" id;
        let actual_sizes, actual_params =
          CidMap.find (prefix id) env.global_tys
        in
        let mapping =
          List.fold_left2
            (fun acc id1 id2 -> CidMap.add id1 id2 acc)
            CidMap.empty
            (List.map Cid.id actual_sizes)
            (List.map Cid.id size_ids)
        in
        if List.length size_ids <> List.length actual_sizes
        then
          error_sp spec.ispan
          @@ Printf.sprintf
               "Global type %s takes %d size arguments, but interface \
                specified %d"
               (Printing.id_to_string id)
               (List.length actual_sizes)
               (List.length size_ids);
        if (not (List.is_empty params))
           && not
                (List.for_all2
                   (fun (id1, ty1) (id2, ty2) ->
                     Id.equal id1 id2 && equiv_tys mapping ty1 ty2)
                   params
                   actual_params)
        then
          error_sp spec.ispan
          @@ Printf.sprintf
               "Definition of global type %s does not match the one in the body"
               (Printing.id_to_string id)
      | InConstr (id, ret_cid, size_ids, params) ->
        if not (KindSet.mem (KConstr, prefix id) env.module_defs)
        then err_id spec.ispan "constructor" id;
        let actual_ret_cid, actual_sizes, actual_params =
          CidMap.find (prefix id) env.constructors
        in
        let mapping =
          List.fold_left2
            (fun acc id1 id2 -> CidMap.add id1 id2 acc)
            CidMap.empty
            (List.map Cid.id actual_sizes)
            (List.map Cid.id size_ids)
        in
        if (not (Cid.equal ret_cid actual_ret_cid))
           || List.length size_ids <> List.length actual_sizes
        then
          error_sp spec.ispan
          @@ Printf.sprintf
               "Constructor %s in interface has return type %s, but in module \
                body has type %s"
               (Printing.id_to_string id)
               (Printing.gty_to_string
                  (ret_cid, List.map (fun id -> IUser (Id id)) size_ids))
               (Printing.gty_to_string
                  ( actual_ret_cid
                  , List.map (fun id -> IUser (Id id)) actual_sizes ));
        if not
             (List.for_all2
                (fun (_, ty1) ty2 -> equiv_tys mapping ty1 (ty ty2))
                params
                actual_params)
        then
          error_sp spec.ispan
          @@ Printf.sprintf
               "Constructor %s takes different arguments in interface than in \
                body"
               (Printing.id_to_string id)
      | InFun (id, ret_ty, specs, params) ->
        if not (KindSet.mem (KConst, prefix id) env.module_defs)
        then err_id spec.ispan "function" id;
        let fty = CidMap.find (prefix id) env.consts in
        let func_ty : func_ty =
          match fty with
          | TFun f -> f
          | _ ->
            error_sp spec.ispan
            @@ Printf.sprintf
                 "Definition of %s in module body has non-function type %s"
                 (Printing.id_to_string id)
                 (Printing.raw_ty_to_string fty)
        in
        let start_eff = func_ty.start_eff in
        let new_params =
          List.map2 (fun (id, _) rty -> id, ty rty) params func_ty.arg_tys
        in
        let spec_constraints, end_eff =
          spec_to_constraints env spec.ispan start_eff new_params specs
        in
        let sufficient_constraints =
          let constrs =
            match end_eff with
            | None ->
              (* If not specified then start_eff must equal end_eff *)
              CLeq (start_eff, func_ty.end_eff)
              :: CLeq (func_ty.end_eff, start_eff)
              :: !(func_ty.constraints)
            | Some eff -> CLeq (func_ty.end_eff, eff) :: !(func_ty.constraints)
          in
          Typer_Z3.check_implies spec_constraints constrs
        in
        if not sufficient_constraints
        then
          error_sp spec.ispan
          @@ Printf.sprintf
               "Constraints in interface (for function %s) are weaker than the \
                constraints in the module body."
               (Printing.id_to_string id);
        let expected_func_ty =
          TFun
            { arg_tys = List.map (fun (_, ty) -> ty.raw_ty) params
            ; ret_ty = ret_ty.raw_ty
            ; start_eff
            ; end_eff = func_ty.end_eff
            ; constraints = ref !(func_ty.constraints)
            }
        in
        if not (equiv_raw_ty (TFun func_ty) expected_func_ty)
        then
          error_sp spec.ispan
          @@ Printf.sprintf
               "Function %s has different parameters or return type in \
                interface than in body"
               (Printing.id_to_string id)
      | InEvent (id, cspecs, params) ->
        if not (KindSet.mem (KConst, prefix id) env.module_defs)
        then err_id spec.ispan "event" id;
        if not (KindSet.mem (KHandler, prefix id) env.module_defs)
        then
          error_sp spec.ispan
          @@ Printf.sprintf
               "Event %s has no corresponding handler definition in module body"
               (Printing.id_to_string id);
        let fty = CidMap.find (prefix id) env.consts in
        let func_ty : func_ty =
          match fty with
          | TFun f -> f
          | _ ->
            error_sp spec.ispan
            @@ Printf.sprintf
                 "Definition of %s in module body has non-event type %s"
                 (Printing.id_to_string id)
                 (Printing.raw_ty_to_string fty)
        in
        let new_params =
          try
            List.map2 (fun (id, _) rty -> id, ty rty) params func_ty.arg_tys
          with
          | Invalid_argument _ ->
            error_sp spec.ispan
            @@ Printf.sprintf
                 "Event %s has different parameters in interface than in body"
                 (Printing.id_to_string id)
        in
        let spec_constraints, _ =
          spec_to_constraints env spec.ispan func_ty.start_eff new_params cspecs
        in
        if not (Typer_Z3.check_implies spec_constraints !(func_ty.constraints))
        then
          error_sp spec.ispan
          @@ Printf.sprintf
               "Event %s has different constraints in interface than in body"
               (Printing.id_to_string id);
        let expected_fty =
          TFun
            { arg_tys = List.map (fun (_, ty) -> ty.raw_ty) params
            ; ret_ty = TEvent false
            ; start_eff = func_ty.start_eff
            ; end_eff = func_ty.start_eff
            ; constraints = ref !(func_ty.constraints)
            }
        in
        if not (equiv_raw_ty (TFun func_ty) expected_fty)
        then
          error_sp spec.ispan
          @@ Printf.sprintf
               "Event %s has different parameters in interface than in body"
               (Printing.id_to_string id)
      | InModule (m_id, intf) ->
        validate_interface (fun id -> Compound (m_id, prefix id)) env intf)
    interface
;;

(* Add all the definitions from the interface into the environment, with the
   appropriate prefix *)
let rec add_interface m_id old_env interface =
  let prefix id = Cid.create_ids [m_id; id] in
  let prefix_c cid = Compound (m_id, cid) in
  let prefix_rty env rty =
    prefixer#visit_raw_ty (prefix_c, env.module_defs) rty
  in
  let prefix_params env params =
    prefixer#visit_params (prefix_c, env.module_defs) params
  in
  let new_env =
    List.fold_left
      (fun env spec ->
        match spec.ispec with
        | InSize id ->
          { env with sizes = CidSet.add (prefix id) env.sizes } |> def KSize id
        | InVar (id, ty) ->
          { env with
            consts =
              CidMap.add (prefix id) (prefix_rty env ty.raw_ty) env.consts
          }
          |> def KConst id
        | InGlobalTy (id, size_ids, params) ->
          { env with
            global_tys =
              CidMap.add
                (prefix id)
                (size_ids, prefix_params env params)
                env.global_tys
          ; global_labels =
              List.fold_left
                (fun acc (l, _) -> StringMap.add (Id.name l) (prefix id) acc)
                env.global_labels
                params
          }
          |> def KGlobalTy id
        | InConstr (id, ret_ty, size_ids, params) ->
          { env with
            constructors =
              CidMap.add
                (prefix id)
                ( prefix_c ret_ty
                , size_ids
                , List.map (fun (_, ty) -> prefix_rty env ty.raw_ty) params )
                env.constructors
          }
          |> def KConstr id
        | InFun (id, ret_ty, specs, params) ->
          let start_eff = FVar (QVar (Id.fresh "start")) in
          let constraints, end_eff =
            spec_to_constraints env ret_ty.tspan start_eff params specs
          in
          let end_eff =
            match end_eff with
            | Some eff -> eff
            | None -> start_eff
          in
          let fty =
            { arg_tys = List.map (fun (_, ty) -> ty.raw_ty) params
            ; ret_ty = ret_ty.raw_ty
            ; start_eff
            ; end_eff
            ; constraints = ref constraints
            }
          in
          { env with
            consts =
              CidMap.add (prefix id) (prefix_rty env (TFun fty)) env.consts
          }
          |> def KConst id
        | InEvent (id, cspecs, params) ->
          let constrs, _ =
            spec_to_constraints env spec.ispan FZero params cspecs
          in
          let handlers =
            CidMap.add
              (prefix id)
              (constrs, prefix_params env params)
              env.handlers
          in
          let consts =
            CidMap.add
              (prefix id)
              (mk_event_ty constrs (prefix_params env params))
              env.consts
          in
          let env =
            { env with handlers; consts } |> def KConst id |> def KHandler id
          in
          env
        | InModule (id, intf) -> add_interface id env intf)
      { old_env with module_defs = KindSet.empty }
      interface
  in
  let safe_prefix k cid =
    if KindSet.mem (k, cid) new_env.module_defs then prefix_c cid else cid
  in
  { new_env with
    module_defs =
      KindSet.union old_env.module_defs
      @@ KindSet.map (fun (k, id) -> k, prefix_c id) new_env.module_defs
  ; global_labels = StringMap.map (safe_prefix KGlobalTy) new_env.global_labels
  }
;;

(* Ensure the interface doesn't reference any hidden information, e.g.
   any types that aren't in the interface *)
let rec wellformed_interface env interface =
  let validate_ty env ty =
    match ty.raw_ty with
    | TGlobal ((id, sizes), _) ->
      (match CidMap.find_opt id env.global_tys with
      | None -> error_sp ty.tspan @@ "Unknown type " ^ Printing.ty_to_string ty
      | Some (sizes2, _) ->
        if List.length sizes <> List.length sizes2
        then
          error_sp ty.tspan
          @@ "Wrong number of size arguments to type "
          ^ Printing.cid_to_string id)
    | _ -> ()
  in
  let validate_params env params =
    List.iter (fun (_, ty) -> validate_ty env ty) params
  in
  let _ =
    List.fold_left
      (fun env spec ->
        match spec.ispec with
        | InSize _ -> env
        | InVar (_, ty) ->
          validate_ty env ty;
          env
        | InGlobalTy (id, sizes, params) ->
          validate_params env params;
          { env with
            global_tys = CidMap.add (Id id) (sizes, params) env.global_tys
          }
        | InConstr (_, cid, sizes, params) ->
          validate_params env params;
          (match CidMap.find_opt cid env.global_tys with
          | None ->
            error_sp spec.ispan
            @@ "Unknown type "
            ^ Printing.cid_to_string cid
            ^ (Printing.comma_sep Printing.id_to_string sizes
              |> Printing.wrap "<<" ">>")
          | Some (sizes2, _) ->
            if List.length sizes <> List.length sizes2
            then
              error_sp spec.ispan
              @@ "Wrong number of size arguments to type "
              ^ Printing.cid_to_string cid);
          env
        | InFun (_, rty, cspecs, params) ->
          validate_ty env rty;
          validate_params env params;
          let _ =
            (* Just getting through this function ensures the constraints are valid *)
            spec_to_constraints env spec.ispan (fresh_effect ()) params cspecs
          in
          env
        | InEvent (_, cspecs, params) ->
          validate_params env params;
          let _ =
            (* Just getting through this function ensures the constraints are valid *)
            spec_to_constraints env spec.ispan (fresh_effect ()) params cspecs
          in
          env
        | InModule (id, intf) ->
          wellformed_interface env intf;
          add_interface id env intf)
      env
      interface
  in
  ()
;;

let add_module_defs m_id interface old_env m_env =
  if List.is_empty interface
  then add_all_module_defs m_id old_env m_env
  else (
    validate_interface Cid.id m_env interface;
    add_interface m_id old_env interface)
;;
