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

    method! visit_TName (f, kset) cid sizes b =
      let sizes = List.map (self#visit_size (f, kset)) sizes in
      if KindSet.mem (KUserTy, cid) kset
      then TName (f cid, sizes, b)
      else TName (cid, sizes, b)
  end
;;

(* After going through a module body, add all the new definitions to the old
   environment, but with the module id as a prefix *)
let add_all_module_defs m_id old_env m_env =
  let prefix cid = Compound (m_id, cid) in
  let prefix_ty = prefixer#visit_ty (prefix, m_env.module_defs) in
  let prefix_params = prefixer#visit_params (prefix, m_env.module_defs) in
  let prefixed_maps =
    KindSet.fold
      (fun (k, cid) acc ->
        match k with
        | KSize -> { acc with sizes = CidSet.add (prefix cid) acc.sizes }
        | KConst ->
          let ty = CidMap.find cid m_env.consts |> prefix_ty in
          { acc with consts = CidMap.add (prefix cid) ty acc.consts }
        | KHandler ->
          let handler =
            let x, params = CidMap.find cid m_env.handlers in
            x, prefix_params params
          in
          { acc with handlers = CidMap.add (prefix cid) handler acc.handlers }
        | KConstr ->
          let new_fty =
            let fty = CidMap.find cid m_env.constructors in
            prefixer#visit_func_ty (prefix, m_env.module_defs) fty
          in
          { acc with
            constructors = CidMap.add (prefix cid) new_fty acc.constructors
          }
        | KUserTy ->
          (* Non-abstract user types have already been replaced,
             so don't have to do anything here *)
          acc)
      m_env.module_defs
      old_env
  in
  { prefixed_maps with
    module_defs =
      KindSet.union
        old_env.module_defs
        (KindSet.map (fun (k, id) -> k, prefix id) m_env.module_defs)
  ; record_labels = StringMap.map prefix_ty m_env.record_labels
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
  let compare_tfuns span kind_str id (expected : func_ty) (declared : func_ty) =
    if not (equiv_ty ~ignore_effects:true expected.ret_ty declared.ret_ty)
    then
      error_sp span
      @@ Printf.sprintf
           "%s %s returns type %s, but interface specifies return type %s."
           kind_str
           (Printing.id_to_string id)
           (Printing.ty_to_string expected.ret_ty)
           (Printing.ty_to_string declared.ret_ty);
    if List.length expected.arg_tys <> List.length declared.arg_tys
       || List.exists2
            (fun ty1 ty2 -> not (equiv_ty ~ignore_effects:true ty1 ty2))
            expected.arg_tys
            declared.arg_tys
    then
      error_sp span
      @@ Printf.sprintf
           "%s %s takes arguments %s but interface specifies arguments %s"
           kind_str
           (Printing.id_to_string id)
           (Printing.list_to_string Printing.ty_to_string expected.arg_tys)
           (Printing.list_to_string Printing.ty_to_string declared.arg_tys)
  in
  let subst ty = subst_user_tys#visit_ty env ty in
  let normalize ty = (normalizer ())#visit_ty () ty in
  List.iter
    (fun spec ->
      match spec.ispec with
      | InSize id ->
        if not (KindSet.mem (KSize, prefix id) env.module_defs)
        then err_id spec.ispan "size" id
      | InVar (id, ty) ->
        if not (KindSet.mem (KConst, prefix id) env.module_defs)
        then err_id spec.ispan "value" id;
        let ty = ty |> subst |> normalize in
        if is_global ty
        then
          error_sp spec.ispan "Cannot define global variables inside a module";
        let actual_ty = CidMap.find (prefix id) env.consts |> normalize in
        if not (equiv_ty ~ignore_effects:true ty actual_ty)
        then
          error_sp spec.ispan
          @@ Printf.sprintf
               "Value %s has type %s, but interface specified type %s"
               (Printing.id_to_string id)
               (Printing.ty_to_string actual_ty)
               (Printing.ty_to_string ty)
      | InTy (id, sizes, tyo, b) ->
        if not (KindSet.mem (KUserTy, prefix id) env.module_defs)
        then err_id spec.ispan "user type" id;
        let actual_ty, actual_sizes = CidMap.find (prefix id) env.user_tys in
        if List.length sizes <> List.length actual_sizes
        then
          error_sp spec.ispan
          @@ Printf.sprintf
               "Type %s takes %d size arguments, but interface specified %d"
               (Printing.id_to_string id)
               (List.length actual_sizes)
               (List.length sizes);
        if b <> is_global actual_ty
        then
          error_sp spec.ispan
          @@ Printf.sprintf
               "Type %s has%s global components and must%s be declared global \
                in interface"
               (Printing.id_to_string id)
               (if b then " no" else "")
               (if b then " not" else "");
        (match tyo with
        | None -> ()
        | Some ty ->
          let ty =
            { ty with
              raw_ty =
                ReplaceUserTys.subst_sizes
                  ty.tspan
                  (Id id)
                  ty.raw_ty
                  (ReplaceUserTys.extract_ids ty.tspan sizes)
                  actual_sizes
            }
            |> subst
            |> normalize
          in
          if not (equiv_ty ~ignore_effects:true ty actual_ty)
          then
            error_sp spec.ispan
            @@ Printf.sprintf
                 "Definition of global type %s does not match the one in the \
                  body"
                 (Printing.id_to_string id))
      | InConstr (id, ret_ty, params) ->
        if not (KindSet.mem (KConstr, prefix id) env.module_defs)
        then err_id spec.ispan "constructor" id;
        let ret_ty = subst ret_ty in
        let params = List.map (fun (id, ty) -> id, subst ty) params in
        let actual_fty =
          CidMap.find (prefix id) env.constructors |> normalize_tfun
        in
        let declared_fty =
          { arg_tys = List.map snd params
          ; ret_ty (* Last three entries don't matter *)
          ; start_eff = fresh_effect ()
          ; end_eff = fresh_effect ()
          ; constraints = ref []
          }
          |> normalize_tfun
        in
        compare_tfuns spec.ispan "Constructor" id actual_fty declared_fty
      | InFun (id, ret_ty, specs, params) ->
        if not (KindSet.mem (KConst, prefix id) env.module_defs)
        then err_id spec.ispan "function" id;
        let ret_ty = subst ret_ty in
        let params = List.map (fun (id, ty) -> id, subst ty) params in
        let fty = CidMap.find (prefix id) env.consts in
        let func_ty : func_ty =
          match fty.raw_ty with
          | TFun f -> f |> normalize_tfun
          | _ ->
            error_sp spec.ispan
            @@ Printf.sprintf
                 "Definition of %s in module body has non-function type %s"
                 (Printing.id_to_string id)
                 (Printing.ty_to_string fty)
        in
        let start_eff = func_ty.start_eff in
        let new_params =
          List.map2 (fun (id, _) ty -> id, ty) params func_ty.arg_tys
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
          TyperZ3.check_implies spec_constraints constrs
        in
        if not sufficient_constraints
        then
          error_sp spec.ispan
          @@ Printf.sprintf
               "Constraints in interface (for function %s) are weaker than the \
                constraints in the module body."
               (Printing.id_to_string id);
        let expected_func_ty =
          { arg_tys = List.map snd params
          ; ret_ty
          ; start_eff
          ; end_eff = func_ty.end_eff
          ; constraints = ref !(func_ty.constraints)
          }
          |> normalize_tfun
        in
        compare_tfuns spec.ispan "Function" id expected_func_ty func_ty
      | InEvent (id, cspecs, params) ->
        if not (KindSet.mem (KConst, prefix id) env.module_defs)
        then err_id spec.ispan "event" id;
        if not (KindSet.mem (KHandler, prefix id) env.module_defs)
        then
          error_sp spec.ispan
          @@ Printf.sprintf
               "Event %s has no corresponding handler definition in module body"
               (Printing.id_to_string id);
        let params = List.map (fun (id, ty) -> id, subst ty) params in
        let fty = CidMap.find (prefix id) env.consts in
        let func_ty : func_ty =
          match fty.raw_ty with
          | TFun f -> f |> normalize_tfun
          | _ ->
            error_sp spec.ispan
            @@ Printf.sprintf
                 "Definition of %s in module body has non-event type %s"
                 (Printing.id_to_string id)
                 (Printing.ty_to_string fty)
        in
        let new_params =
          try List.map2 (fun (id, _) ty -> id, ty) params func_ty.arg_tys with
          | Invalid_argument _ ->
            error_sp spec.ispan
            @@ Printf.sprintf
                 "Event %s has different parameters in interface than in body"
                 (Printing.id_to_string id)
        in
        let spec_constraints, _ =
          spec_to_constraints env spec.ispan func_ty.start_eff new_params cspecs
        in
        if not (TyperZ3.check_implies spec_constraints !(func_ty.constraints))
        then
          error_sp spec.ispan
          @@ Printf.sprintf
               "Event %s has different constraints in interface than in body"
               (Printing.id_to_string id);
        let expected_fty =
          { arg_tys = List.map snd params
          ; ret_ty = ty @@ TEvent false
          ; start_eff = func_ty.start_eff
          ; end_eff = func_ty.start_eff
          ; constraints = ref !(func_ty.constraints)
          }
          |> normalize_tfun
        in
        compare_tfuns spec.ispan "Event" id expected_fty func_ty
      | InModule (m_id, intf) ->
        validate_interface (fun id -> Compound (m_id, prefix id)) env intf)
    interface
;;

(* Add all the definitions from the interface into the environment, with the
   appropriate prefix *)
let rec add_interface m_id old_env interface =
  let prefix id = Cid.create_ids [m_id; id] in
  let prefix_c cid = Compound (m_id, cid) in
  let prefix_ty env ty = prefixer#visit_ty (prefix_c, env.module_defs) ty in
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
            consts = CidMap.add (prefix id) (prefix_ty env ty) env.consts
          }
          |> def KConst id
        | InTy (id, sizes, tyo, b) ->
          begin
            match tyo with
            | None ->
              let ty = ty @@ TName (prefix id, sizes, b) in
              { env with
                user_tys = CidMap.add (prefix id) (ty, sizes) env.user_tys
              }
              |> def KUserTy id
            | Some ty ->
              let ty = prefix_ty env ty in
              let record_labels =
                match ty.raw_ty with
                | TRecord lst ->
                  List.fold_left
                    (fun acc (l, _) -> StringMap.add l ty acc)
                    env.record_labels
                    lst
                | _ -> env.record_labels
              in
              { env with
                user_tys = CidMap.add (prefix id) (ty, sizes) env.user_tys
              ; record_labels
              }
              |> def KUserTy id
          end
        | InConstr (id, ret_ty, params) ->
          let fty =
            { arg_tys = List.map (prefix_ty env % snd) params
            ; ret_ty = prefix_ty env ret_ty
            ; start_eff = fresh_effect ()
            ; end_eff = fresh_effect ()
            ; constraints = ref []
            }
          in
          { env with
            constructors = CidMap.add (prefix id) fty env.constructors
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
            { arg_tys = List.map (prefix_ty env % snd) params
            ; ret_ty = prefix_ty env ret_ty
            ; start_eff
            ; end_eff
            ; constraints = ref constraints
            }
          in
          { env with
            consts = CidMap.add (prefix id) (ty (TFun fty)) env.consts
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
  { new_env with
    module_defs =
      KindSet.union old_env.module_defs
      @@ KindSet.map (fun (k, id) -> k, prefix_c id) new_env.module_defs
      (* FIXME: Not sure why this is here; it seems wrong *)
      (*; record_labels = StringMap.map (safe_prefix KGlobalTy) new_env.record_labels *)
  }
;;

(* Ensure the interface doesn't reference any hidden information, e.g.
   any types that aren't in the interface *)
let rec wellformed_interface env interface =
  let validate_ty env ty =
    match ty.raw_ty with
    | TName (cid, sizes, _) ->
      (match CidMap.find_opt cid env.user_tys with
      | None -> error_sp ty.tspan @@ "Unknown type " ^ Printing.ty_to_string ty
      | Some (_, sizes2) ->
        if List.length sizes <> List.length sizes2
        then
          error_sp ty.tspan
          @@ "Wrong number of size arguments to type "
          ^ Printing.cid_to_string cid)
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
        | InTy (id, sizes, tyo, _) ->
          let ty =
            match tyo with
            | None -> ty TVoid (* Doesn't actually matter *)
            | Some ty ->
              validate_ty env ty;
              ty
          in
          { env with user_tys = CidMap.add (Id id) (ty, sizes) env.user_tys }
        | InConstr (_, ret_ty, params) ->
          validate_params env params;
          validate_ty env ret_ty;
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
