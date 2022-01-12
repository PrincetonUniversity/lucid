open Syntax
open SyntaxUtils
open Collections
open Batteries
open TyperUtil

(* Goes through each decl and substitutes each bound TName for whatever it's
   bound to. If it runs into a type declaration in an interface, it adds it to
   the environment before continuing. If it runs into an abstract type declaration,
   it first explicitly binds it to a TAbstract before continuing. *)
let subst_TNames env d =
  let v =
    object (self)
      inherit [_] s_map as super

      method! visit_ty env ty =
        { ty with raw_ty = lookup_TName ty.tspan !env ty.raw_ty }

      method! visit_DUserTy env id sizes ty =
        let ty = self#visit_ty env ty in
        env := define_user_ty id sizes ty !env;
        DUserTy (id, sizes, ty)

      method! visit_InTy env id sizes tyo b =
        let tyo' =
          match tyo with
          | Some ty -> Some (self#visit_ty env ty)
          | None -> Some (TAbstract (Id (Id.freshen id), sizes, b) |> ty)
        in
        env := define_user_ty id sizes (Option.get tyo') !env;
        InTy (id, sizes, tyo', b)

      method! visit_DModule env id interface ds =
        let old_env = !env in
        let ret = super#visit_DModule env id interface ds in
        env := old_env;
        ret

      method! visit_interface env interface =
        let old_env = !env in
        let ret = super#visit_interface env interface in
        env := old_env;
        ret

      method! visit_TQVar dummy tqv =
        match tqv with
        | TVar { contents = Link x } -> self#visit_raw_ty dummy x
        | _ -> TQVar tqv

      method! visit_IVar dummy tqv =
        match tqv with
        | TVar { contents = Link x } -> self#visit_size dummy x
        | _ -> IVar tqv

      method! visit_FVar dummy tqv =
        match tqv with
        | TVar { contents = Link x } -> self#visit_effect dummy x
        | _ -> FVar tqv
    end
  in
  v#visit_decl (ref env) d
;;

let rec modul_of_interface span env interface =
  let aux acc intf =
    match intf.ispec with
    | InSize id -> { acc with sizes = IdSet.add id acc.sizes }
    | InVar (id, ty) -> { acc with vars = IdMap.add id ty acc.vars }
    | InTy (id, sizes, tyo, _) ->
      let ty =
        match tyo with
        | Some ty -> ty
        | None -> failwith "Internal error: should be replaced by subst_TNames"
      in
      { acc with user_tys = IdMap.add id (sizes, ty) acc.user_tys }
    | InConstr (id, ty, params) ->
      let start_eff = FVar (QVar (Id.fresh "eff")) in
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
      let start_eff = FVar (QVar (Id.fresh "eff")) in
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
      let start_eff = FVar (QVar (Id.fresh "eff")) in
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

(* Go through a module and consistently replace each abstract type which is
   defined in the body. For use in checking equivalence/interface validity *)
let replace_abstract_type (target : cid) (replacement : sizes * ty) modul =
  let v =
    object
      inherit [_] s_map as super

      method! visit_ty (target, (sizes', ty')) ty =
        match ty.raw_ty with
        | TAbstract (cid, sizes, _) ->
          let replaced_ty =
            ReplaceUserTys.subst_sizes
              ty.tspan
              cid
              ty'.raw_ty
              (ReplaceUserTys.extract_ids ty.tspan sizes')
              sizes
          in
          { ty with raw_ty = replaced_ty }
        | _ -> super#visit_ty (target, (sizes', ty')) ty
    end
  in
  let rec replace_modul env modul =
    { modul with
      vars = IdMap.map (v#visit_ty env) modul.vars
    ; constructors = IdMap.map (v#visit_func_ty env) modul.constructors
    ; user_tys =
        IdMap.map (fun (sz, ty) -> sz, v#visit_ty env ty) modul.user_tys
    ; submodules = IdMap.map (replace_modul env) modul.submodules
    }
  in
  let env = target, replacement in
  replace_modul env modul
;;

let rec equiv_modul m1 m2 =
  (* For each abstract type declared in m1, ensure that m2 also declares an abstract
     type with the same name, and replace each occurrence of that abstract type in
     m1 with the definition in m2 (so they can be compared directly later *)
  let m1 =
    IdMap.fold
      (fun id (_, ty) acc ->
        match ty.raw_ty with
        | TAbstract (Id id1, _, b) when Id.name id = Id.name id1 ->
          (match IdMap.find_opt id m2.user_tys with
          | Some (sizes', ({ raw_ty = TAbstract (_, _, b') } as ty'))
            when b = b' -> replace_abstract_type (Id id1) (sizes', ty') acc
          | _ -> (* We'll return false later *) acc)
        | _ -> (* Not an abstract type, don't need to replace *) acc)
      m1.user_tys
      m1
  in
  let cmp_user_tys (szs1, ty1) (szs2, ty2) =
    (* I think this is overkill but I don't think it's wrong *)
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

let rec ensure_compatible_interface span intf_modul modul =
  let open Printing in
  (* For each abstract type declared in the interface, endsure the body has a
     corresponding type declared, and replace the version in the interface with
     the body's definition *)
  let intf_modul =
    IdMap.fold
      (fun id (_, ty) acc ->
        match ty.raw_ty with
        | TAbstract (Id id1, _, b) when Id.name id = Id.name id1 ->
          (match IdMap.find_opt id modul.user_tys with
          | Some (sizes', ty') ->
            if (b && is_global ty') || ((not b) && is_not_global ty')
            then replace_abstract_type (Id id1) (sizes', ty') acc
            else acc
          | _ -> (* We'll return false later *) acc)
        | _ -> (* Not an abstract type, don't need to replace *) acc)
      intf_modul.user_tys
      intf_modul
  in
  let check_func_tys id fty1 fty2 =
    if List.length fty1.arg_tys <> List.length fty2.arg_tys
    then
      error_sp span
      @@ Printf.sprintf
           "%s takes %d arguments in interface but %d in body"
           (id_to_string id)
           (List.length fty1.arg_tys)
           (List.length fty2.arg_tys);
    List.iter2i
      (fun n ty1 ty2 ->
        if not (equiv_ty ty1 ty2)
        then
          error_sp ty1.tspan
          @@ Printf.sprintf
               "Argument %d to %s has type %s in interface but %s in module \
                body"
               n
               (id_to_string id)
               (ty_to_string ty1)
               (ty_to_string ty2))
      fty1.arg_tys
      fty2.arg_tys;
    if not (equiv_ty fty1.ret_ty fty2.ret_ty)
    then
      error_sp span
      @@ Printf.sprintf
           "%s returns %s in interface but %s in body"
           (id_to_string id)
           (ty_to_string fty1.ret_ty)
           (ty_to_string fty2.ret_ty)
  in
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
      (* Note: This won't work right if we get more functional later *)
      | Some { raw_ty = TFun body_fty } ->
        (* Gotta handle functions differently, since the constraints in the
           interface may be less restrictive. Need to ensure:
           1. Constraints in the interface imply constraints in the body
           2. End effect in interface is >= end effect in body *)
        let intf_fty =
          match ty.raw_ty with
          | TFun fty -> normalize_tfun fty
          | _ ->
            error_sp span
            @@ Printf.sprintf
                 "%s has type non-function type %s in interface but function \
                  type in module body"
                 (id_to_string id)
                 (ty_to_string (normalize_ty ty))
        in
        let body_fty = normalize_tfun body_fty in
        let sufficient_constraints =
          let rhs =
            match !(body_fty.constraints) with
            | [] -> []
            | lst -> CLeq (body_fty.end_eff, intf_fty.end_eff) :: lst
          in
          TyperZ3.check_implies !(intf_fty.constraints) rhs
        in
        if not sufficient_constraints
        then
          error_sp span
          @@ Printf.sprintf
               "Constraints in interface (for function %s) are weaker than the \
                constraints in the module body."
               (Printing.id_to_string id);
        check_func_tys id intf_fty body_fty
      | Some ty' when equiv_raw_ty ty.raw_ty ty'.raw_ty -> ()
      | Some ty' ->
        error_sp span
        @@ Printf.sprintf
             "%s has type %s in interface but type %s in module body"
             (id_to_string id)
             (ty_to_string (normalize_ty ty))
             (ty_to_string @@ normalize_ty ty')
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
        if not (equiv_raw_ty ty.raw_ty ty'.raw_ty)
        then
          error_sp span
          @@ Printf.sprintf
               "Type %s has is defined as %s in interface but %s in body"
               (id_to_string id)
               (ty_to_string ty)
               (ty_to_string ty'))
    intf_modul.user_tys;
  IdMap.iter
    (fun id m ->
      match IdMap.find_opt id modul.submodules with
      | Some m' -> ensure_compatible_interface span m m'
      | None ->
        error_sp span
        @@ "Module "
        ^ id_to_string id
        ^ " is declared in module interface but does not appear in the body")
    intf_modul.submodules
;;

let add_interface span env id interface modul =
  let intf_modul = modul_of_interface span env interface in
  ensure_compatible_interface span intf_modul modul;
  define_submodule id intf_modul env
;;
