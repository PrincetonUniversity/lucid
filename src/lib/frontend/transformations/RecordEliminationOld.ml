open Batteries
open Syntax
open SyntaxUtils
open Collections

(* This file isn't currently used, but I'm keeping it around in case
   we want to switch to it to make debugging easier. We can't use it in
   a program that contains vectors, though. *)

(* Maps a variable name and a label to the new variable representing that label
   of the old variable *)
module GMap = Map.Make (struct
  type t = cid * string

  let compare = Pervasives.compare
end)

type env = cid GMap.t

let mk_label_var env id label =
  let new_id = Id.fresh (Id.name id ^ "_" ^ label) in
  GMap.add (Id id, label) (Id new_id) env, new_id
;;

(* Transforms a definition (DGlobal, SLocal, etc.) of a record-type variable into
   several definitions of the same kind, each corresponding to one label of the
   record. Takes the id of the record-type variable, its type, and the expression
   it was to be bound to. Also takes a function for reconstructing the definition
   given those three things.  *)
let rec split_definition env constr id ty e =
  match TyTQVar.strip_links ty.raw_ty with
  | TRecord entries ->
    let env, lvars =
      List.fold_left
        (fun (env, lvars) (label, ty) ->
          let env, new_id = mk_label_var env id label in
          let eproj =
            { e = EProj (e, label)
            ; ety = Some { ty with teffect = FVar (QVar (Id.fresh "eff")) }
            ; espan = e.espan
            }
          in
          let env, defs = split_definition env constr new_id ty eproj in
          env, defs :: lvars)
        (env, [])
        entries
    in
    env, List.concat (List.rev lvars)
  | _ ->
    let ty = { ty with teffect = FVar (QVar (Id.fresh "eff")) } in
    env, [constr id ty e]
;;

let replacer =
  object (self)
    inherit [_] s_map as super

    method replace_decl env d =
      match d.d with
      | DUserTy _ ->
        (* Don't need these anymore, might as well eliminate them now. *)
        env, []
      | DGlobal (id, ty, e) ->
        let constr id ty e =
          if is_global ty
          then { d with d = DGlobal (id, ty, e) }
          else { d with d = DConst (id, ty, e) }
        in
        let env, ds = split_definition env constr id ty e in
        env, super#visit_decls env ds
      | DConst (id, ty, e) ->
        let constr id ty e = { d with d = DConst (id, ty, e) } in
        let env, ds = split_definition env constr id ty e in
        env, super#visit_decls env ds
      | DEvent (id, sort, specs, params) ->
        let constr id ty _ = id, ty in
        let pss =
          List.map
            (fun (id, ty) ->
              snd
              @@ split_definition env constr id ty (var_sp (Id id) Span.default))
            params
        in
        let ps = List.flatten pss in
        (* We can get away with not changing specs because this transformation
           happens after global arg elimination. If it didn't, we'd have to
           adjust any record-typed variables in the spec *)
        env, [{ d with d = DEvent (id, sort, specs, ps) }]
      | DHandler (id, (params, body)) ->
        let constr id ty _ = id, ty in
        let f_env, pss =
          List.fold_left
            (fun (env, pss) (id, ty) ->
              let env, ps =
                split_definition env constr id ty (var_sp (Id id) Span.default)
              in
              env, ps :: pss)
            (env, [])
            params
        in
        let ps = List.flatten (List.rev pss) in
        env, [super#visit_decl f_env { d with d = DHandler (id, (ps, body)) }]
      | _ -> env, [super#visit_decl env d]

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

    method replace_statement env s =
      (* Can't use the regular method because we want to update the env *)
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
      | SNoop | SUnit _ | SGen _ | SRet _ | SPrintf _ ->
        (* Non-recursive *)
        env, super#visit_statement env s
      | SLocal (id, ty, e) ->
        let constr id ty e = { s with s = SLocal (id, ty, e) } in
        let env, ss = split_definition env constr id ty e in
        let s = List.fold_left sseq (List.hd ss) (List.tl ss) in
        env, super#visit_statement env s
      | SAssign (id, e) ->
        let constr id _ e = { s with s = SAssign (id, e) } in
        let env, ss = split_definition env constr id (Option.get e.ety) e in
        let s = List.fold_left sseq (List.hd ss) (List.tl ss) in
        env, super#visit_statement env s
      | SLoop _ -> error "Loops should be eliminated before records"

    method! visit_statement env s = snd @@ self#replace_statement env s

    method! visit_EProj env e label =
      let e' = self#visit_exp env e in
      match e'.e with
      | EVar cid ->
        let renamed_id = GMap.find (cid, label) env in
        EVar renamed_id
      | ERecord entries ->
        let exp = self#visit_exp env (List.assoc label entries) in
        exp.e
      | EWith (base, entries) ->
        let exp =
          match List.assoc_opt label entries with
          | Some e -> self#visit_exp env e
          | None -> self#visit_exp env { e with e = EProj (base, label) }
        in
        exp.e
      | _ ->
        Console.error_position e.espan
        @@ "Unknown record variable "
        ^ Printing.exp_to_string e

    method! visit_ECall env cid args =
      let rec flatten_exp e =
        match TyTQVar.strip_links (Option.get e.ety).raw_ty with
        | TRecord entries ->
          List.map
            (fun (l, ty) ->
              { e with e = EProj (e, l); ety = Some ty } |> flatten_exp)
            entries
          |> List.concat
        | _ -> [e]
      in
      let args = List.map flatten_exp args |> List.concat in
      ECall (cid, List.map (super#visit_exp env) args)
  end
;;

let eliminate_prog ds = replacer#visit_decls GMap.empty ds
