open Syntax
open SyntaxUtils
open Collections
open Batteries
open TyperUtil

let rec modul_of_interface span env interface =
  let aux acc intf =
    match intf.ispec with
    | InSize id -> { acc with sizes = IdSet.add id acc.sizes }
    | InVar (id, ty) -> { acc with vars = IdMap.add id ty acc.vars }
    | InTy (id, sizes, tyo, b) ->
      let ty =
        match tyo with
        | Some ty ->
          ty
          (* FIXME: We need to ensure these TNames are always unique. Maybe. *)
        | None -> ty @@ TName (Id id, sizes, b)
      in
      { acc with user_tys = IdMap.add id (sizes, ty) acc.user_tys }
    | InConstr (id, ty, params) ->
      let start_eff = fresh_effect () in
      let fty =
        { arg_tys = List.map snd params
        ; ret_ty = ty
        ; start_eff
        ; end_eff = start_eff
        ; constraints = ref []
        }
        |> normalize_tfun
      in
      { acc with constructors = IdMap.add id fty acc.constructors }
    | InFun (id, ret_ty, constrs, params) ->
      let start_eff = fresh_effect () in
      let constrs, end_eff =
        spec_to_constraints env span start_eff params constrs
      in
      let end_eff = Option.default start_eff end_eff in
      let fty =
        { arg_tys = List.map snd params
        ; ret_ty
        ; start_eff
        ; end_eff
        ; constraints = ref constrs
        }
        |> normalize_tfun
      in
      { acc with vars = IdMap.add id (ty @@ TFun fty) acc.vars }
    | InEvent (id, constrs, params) ->
      let start_eff = fresh_effect () in
      let constrs, _ = spec_to_constraints env span start_eff params constrs in
      let fty =
        { arg_tys = List.map snd params
        ; ret_ty = ty TEvent
        ; start_eff
        ; end_eff = start_eff
        ; constraints = ref constrs
        }
        |> normalize_tfun
      in
      { acc with vars = IdMap.add id (ty @@ TFun fty) acc.vars }
    | InModule (id, interface) ->
      { acc with
        submodules =
          IdMap.add id (modul_of_interface span env interface) acc.submodules
      }
  in
  List.fold_left aux empty_modul interface
;;

(* Replace TNames with their definitions, according to the map provided in env *)
(* let subst_interface_tys target sizes ty modul =
  let v =
    object
      inherit [_] s_map as super

      method! visit_ty (target, sizes', ty') ty =
        match ty.raw_ty with
        | TName (Id id, sizes, _) when Id.equal id target ->
          let replaced_ty =
            ReplaceUserTys.subst_sizes
              ty.tspan
              (Id id)
              ty'.raw_ty
              (ReplaceUserTys.extract_ids ty.tspan sizes')
              sizes
          in
          { ty with raw_ty = replaced_ty; teffect = ty'.teffect }
        | _ -> super#visit_ty (target, sizes', ty') ty
    end
  in
  let env = target, sizes, ty in
  let rec subst_modul modul =
    { modul with
      vars = IdMap.map (v#visit_ty env) modul.vars
    ; user_tys =
        (* FIXME: The handling of sizes here might be wrong *)
        IdMap.map (fun (_, ty) -> sizes, v#visit_ty env ty) modul.user_tys
    ; constructors = IdMap.map (v#visit_func_ty env) modul.constructors
    ; submodules = IdMap.map subst_modul modul.submodules
    }
  in
  subst_modul modul
;; *)

let rec equiv_modul m1 m2 =
  let cmp_user_tys (szs1, ty1) (szs2, ty2) =
    let szs1, ty1 =
      let norm = normalizer () in
      List.map (norm#visit_size ()) szs1, norm#visit_ty () ty1
    in
    let szs2, ty2 =
      let norm = normalizer () in
      List.map (norm#visit_size ()) szs2, norm#visit_ty () ty2
    in
    List.length szs1 = List.length szs2 && equiv_ty ty1 ty2
  in
  let cmp_ftys fty1 fty2 = equiv_raw_ty (TFun fty1) (TFun fty2) in
  IdSet.equal m1.sizes m2.sizes
  && IdMap.equal equiv_ty m1.vars m2.vars
  && IdMap.equal cmp_user_tys m1.user_tys m2.user_tys
  && IdMap.equal cmp_ftys m1.constructors m2.constructors
  && IdMap.equal equiv_modul m1.submodules m2.submodules
;;

let rec compatible_interface span intf_modul modul =
  let open Printing in
  let diff = IdSet.diff intf_modul.sizes modul.sizes in
  if not (IdSet.is_empty diff)
  then
    error_sp span
    @@ "Size "
    ^ id_to_string (IdSet.choose diff)
    ^ " appears in interface but not module body";
  IdMap.iter
    (fun id ty ->
      match IdMap.find_opt id modul.vars with
      | Some ty' when equiv_ty ty ty' -> ()
      | Some ty' ->
        error_sp span
        @@ Printf.sprintf
             "%s has type %s in interface but type %s in module body"
             (id_to_string id)
             (ty_to_string ty)
             (ty_to_string ty')
      | None ->
        error_sp span
        @@ id_to_string id
        ^ " is declared in module interface but does not appear in the body")
    intf_modul.vars;
  IdMap.iter
    (fun id fty ->
      match IdMap.find_opt id modul.constructors with
      | Some fty' when equiv_raw_ty (TFun fty) (TFun fty') -> ()
      | Some fty' ->
        error_sp span
        @@ Printf.sprintf
             "Constructor %s has type %s in interface but type %s in module \
              body"
             (id_to_string id)
             (func_to_string fty)
             (func_to_string fty')
      | None ->
        error_sp span
        @@ "Constructor "
        ^ id_to_string id
        ^ " is declared in module interface but does not appear in the body")
    intf_modul.constructors;
  IdMap.iter
    (fun id (sizes, ty) ->
      match IdMap.find_opt id modul.user_tys with
      | None ->
        error_sp span
        @@ "Type "
        ^ id_to_string id
        ^ " is declared in module interface but does not appear in the body"
      | Some (sizes', ty') ->
        if List.length sizes <> List.length sizes'
        then
          error_sp span
          @@ Printf.sprintf
               "Type %s has %d size parameters in interface but %d in body"
               (id_to_string id)
               (List.length sizes)
               (List.length sizes');
        (match ty.raw_ty with
        | TName (Id id', _, _) when Id.equal id id' -> ()
        | _ ->
          if not (equiv_ty ty ty')
          then
            error_sp span
            @@ Printf.sprintf
                 "Type %s has different definition in interface than in body"
                 (id_to_string id)))
    intf_modul.user_tys;
  IdMap.iter
    (fun id m ->
      match IdMap.find_opt id modul.submodules with
      | Some m' -> compatible_interface span m m'
      | None ->
        error_sp span
        @@ "Module "
        ^ id_to_string id
        ^ " is declared in module interface but does not appear in the body")
    intf_modul.submodules
;;
