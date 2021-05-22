open Batteries
open Syntax
open Collections

module GMap = Map.Make (struct
  type t = cid * string

  let compare = Pervasives.compare
end)

let subst =
  object
    inherit [_] s_map as super

    method! visit_IUser env cid =
      match cid with
      | Compound _ -> IUser cid
      | Id id ->
        (match IdMap.find_opt id (fst env) with
        | None -> IUser cid
        | Some x -> x)

    method! visit_exp env e =
      match e.e with
      | EVar cid ->
        begin
          match CidMap.find_opt cid (snd env) with
          | None -> e
          | Some x -> x
        end
      | _ -> super#visit_exp env e
  end
;;

type env =
  { global_tys : ((string * ty) list * id list) CidMap.t
  ; global_renaming : cid GMap.t
  ; constructors : (id list * params * decl list) CidMap.t
  }

let empty_env =
  { global_tys = CidMap.empty
  ; global_renaming = GMap.empty
  ; constructors = CidMap.empty
  }
;;

let is_builtin cid =
  Cid.names cid = ["Array"; "t"] || Cid.names cid = ["Counter"; "t"]
;;

let replacer =
  object (self)
    inherit [_] s_map as super

    method rename_d env base_id l f =
      let label = Id.name l in
      let new_id = Id.fresh (Id.name base_id ^ "_" ^ label) in
      ( { env with
          global_renaming =
            GMap.add (Id base_id, label) (Id new_id) env.global_renaming
        }
      , f new_id )

    method replace_decl env d =
      match d.d with
      | DGlobalTy (id, size_ids, params) ->
        ( { env with
            global_tys =
              CidMap.add
                (Id id)
                (List.map (fun (id, ty) -> Id.name id, ty) params, size_ids)
                env.global_tys
          }
        , [] )
      | DConstr { constr_id; size_args; params; body } ->
        ( { env with
            constructors =
              CidMap.add
                (Id constr_id)
                (size_args, params, body)
                env.constructors
          }
        , [] )
      | DGlobal (id, (gty, size_args), constr_id, constr_args)
        when not (is_builtin gty) ->
        let size_ids, params, ds = CidMap.find constr_id env.constructors in
        let size_map =
          List.fold_left2
            (fun acc id sz -> IdMap.add id sz acc)
            IdMap.empty
            size_ids
            size_args
        in
        let constr_args =
          List.map (subst#visit_exp (size_map, CidMap.empty)) constr_args
        in
        let arg_map =
          List.fold_left2
            (fun acc (id, _) e -> CidMap.add (Id id) e acc)
            CidMap.empty
            params
            constr_args
        in
        let ds = subst#visit_decls (size_map, arg_map) ds in
        let env, ds =
          List.fold_left
            (fun (env, ds) d ->
              let env, d' =
                match d.d with
                | DConst (l, ty, exp) ->
                  self#rename_d env id l (fun id -> DConst (id, ty, exp))
                | DGlobal (l, gty, cid, args) ->
                  let f id = DGlobal (id, gty, cid, args) in
                  self#rename_d env id l f
                | _ -> failwith "Impossible"
              in
              env, { d with d = d' } :: ds)
            (env, [])
            ds
        in
        self#replace_decls env (List.rev ds)
      | DConst (_, { raw_ty = TGlobal _ }, _)
      | DExtern (_, { raw_ty = TGlobal _ }) ->
        failwith
          "Elimination for record consts and externs is not yet implemented"
      | _ -> env, [self#visit_decl env d]

    method replace_decls env ds =
      let env, ds =
        List.fold_left
          (fun (env, ds) d ->
            let env, d = self#replace_decl env d in
            env, d :: ds)
          (env, [])
          ds
      in
      env, List.concat (List.rev ds)

    method! visit_decls env ds = snd @@ self#replace_decls env ds

    method replace_vardefs env s id e =
      let builtin_types =
        List.map
          (fun id -> Compound (id, Id (Id.create "t")))
          Builtins.builtin_modules
      in
      match TyTQVar.strip_links (Option.get e.ety) with
      | TGlobal ((cid, size_args), _) when not (List.mem cid builtin_types) ->
        let labels =
          let labels, size_ids = CidMap.find cid env.global_tys in
          let size_map =
            List.fold_left2
              (fun acc id sz -> IdMap.add id sz acc)
              IdMap.empty
              size_ids
              size_args
          in
          List.map
            (fun (l, ty) -> l, subst#visit_ty (size_map, CidMap.empty) ty)
            labels
        in
        let env, s =
          List.fold_left
            (fun (env, acc) (l, ty) ->
              let new_id = Id.fresh (Id.name id ^ "_" ^ l) in
              let new_def =
                let proj =
                  { (proj_sp e l s.sspan) with ety = Some ty.raw_ty }
                in
                match s.s with
                | SLocal _ -> slocal_sp new_id ty proj s.sspan
                | _ -> sassign_sp new_id proj s.sspan
              in
              ( { env with
                  global_renaming =
                    GMap.add (Id id, l) (Id new_id) env.global_renaming
                }
              , sseq_sp new_def acc s.sspan ))
            (env, snoop_sp s.sspan)
            labels
        in
        self#replace_statement env s
      | _ -> env, super#visit_statement env s

    method replace_statement env s =
      match s.s with
      | SSeq (s1, s2) ->
        let env, s1 = self#replace_statement env s1 in
        let env, s2 = self#replace_statement env s2 in
        env, { s with s = SSeq (s1, s2) }
      | SIf (e, s1, s2) ->
        let _, s1 = self#replace_statement env s1 in
        let _, s2 = self#replace_statement env s2 in
        env, { s with s = SIf (self#visit_exp env e, s1, s2) }
      | SMatch (es, bs) ->
        let es = List.map (self#visit_exp env) es in
        let bs =
          List.map (fun (p, s) -> p, snd (self#replace_statement env s)) bs
        in
        env, { s with s = SMatch (es, bs) }
      | SLocal (id, _, e) | SAssign (id, e) -> self#replace_vardefs env s id e
      | _ -> env, super#visit_statement env s

    method! visit_statement env s = snd @@ self#replace_statement env s

    method! visit_EProj env e label =
      let e' = self#visit_exp env e in
      match e'.e with
      | EVar cid ->
        let renamed_id = GMap.find (cid, label) env.global_renaming in
        EVar renamed_id
      | ERecord entries ->
        let exp = self#visit_exp env (List.assoc label entries) in
        exp.e
      | _ ->
        Console.error_position e.espan
        @@ "Unknown global variable "
        ^ Printing.exp_to_string e
  end
;;

let eliminate_prog ds = replacer#visit_decls empty_env ds
