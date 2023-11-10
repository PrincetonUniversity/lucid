open Batteries
open Syntax
open Collections

(* Move all code from modules to toplevel, creating a single flat program.
   Expects alpha-renaming and module aliasing to be done first. *)

type subst_env =
  { sizes : id CidMap.t
  ; types : id CidMap.t
  ; vars : id CidMap.t
  }

let empty_env =
  { sizes = CidMap.empty; types = CidMap.empty; vars = CidMap.empty }
;;

let subst =
  let v =
    object (self)
      inherit [_] s_map

      method! visit_EVar env x =
        match CidMap.find_opt x env.vars with
        | None -> EVar x
        | Some x' -> EVar (Id x')

      method! visit_IUser env x =
        match CidMap.find_opt x env.sizes with
        | None -> IUser x
        | Some x' -> IUser (Id x')

      method! visit_TName env cid sizes b =
        let sizes = List.map (self#visit_size env) sizes in
        let cid =
          match CidMap.find_opt cid env.types with
          | None -> cid
          | Some x -> Id x
        in
        TName (cid, sizes, b)

      method! visit_ETableCreate env tty tactions tsize tdefault =
        let tactions = List.map (self#visit_exp env) tactions in
        let tdefault_cid, tdefault_args, flag = match tdefault.e with 
        | ECall(tdefault_cid, tdefault_args, flag) -> tdefault_cid, tdefault_args, flag
        | _ -> error "internal error: default table action in constructor is not a call"
        in

        let tdefault_args =
          List.map (self#visit_exp env) tdefault_args
        in
        (* rename the default action cid *)
        let tdefault_cid =
          match CidMap.find_opt tdefault_cid env.vars with
          | None -> tdefault_cid
          | Some tdefault_cid' -> Id tdefault_cid'
        in
        ETableCreate
          { tty; tactions; tsize; tdefault = {tdefault with e=ECall(tdefault_cid, tdefault_args, flag)}}
          
      method! visit_STableInstall env etbl entries =
        let etbl = self#visit_exp env etbl in
        let entries =
          List.map
            (fun entry ->
              { entry with
                ematch = List.map (self#visit_exp env) entry.ematch
              ; eaction =
                  (match CidMap.find_opt (Cid.id entry.eaction) env.vars with
                   | None -> entry.eaction
                   | Some new_action_id -> new_action_id)
              ; eargs = List.map (self#visit_exp env) entry.eargs
              })
            entries
        in
        STableInstall (etbl, entries)

      method! visit_ECall env x args u =
        let args = List.map (self#visit_exp env) args in
        let x =
          match CidMap.find_opt x env.vars with
          | None -> x
          | Some x' -> Id x'
        in
        ECall (x, args, u)
    end
  in
  v
;;

let flat_prefix p id = Id.create (Id.name p ^ "_" ^ Id.name id)

let add_definitions prefix env ds =
  let rec aux prefix env d =
    let add_entry map id =
      CidMap.add (Compound (prefix, Id id)) (flat_prefix prefix id) map
    in
    match d.d with
    | DConst (id, _, _)
    | DExtern (id, _)
    | DSymbolic (id, _)
    | DFun (id, _, _, _)
    | DMemop (id, _, _)
    | DEvent (id, _, _, _, _)
    | DHandler (id, _, _)
    | DConstr (id, _, _, _)
    | DGlobal (id, _, _) -> { env with vars = add_entry env.vars id }
    | DSize (id, _) -> { env with sizes = add_entry env.sizes id }
    | DUserTy (id, _, _) -> { env with types = add_entry env.types id }
    | DAction (id, _, _, _) -> { env with vars = add_entry env.vars id }
    | DParser (id, _, _) -> { env with vars = add_entry env.vars id }
    | DModuleAlias _ -> failwith "Should be eliminated before this"
    | DModule (id, _, ds) ->
      let env' = List.fold_left (aux id) empty_env ds in
      let prefix_entries map =
        CidMap.fold (fun _ v acc -> add_entry acc v) map CidMap.empty
      in
      let merge =
        CidMap.merge (fun _ x y ->
          match y with
          | None -> x
          | _ -> y)
      in
      let env =
        { sizes = prefix_entries env'.sizes |> merge env.sizes
        ; types = prefix_entries env'.types |> merge env.types
        ; vars = prefix_entries env'.vars |> merge env.vars
        }
      in
      env
  in
  List.fold_left (aux prefix) env ds
;;

let rec replace_module env m_id ds =
  let wrap decl d = [{ decl with d }] in
  let prefix id = flat_prefix m_id id in
  let add_entry map id = CidMap.add (Id id) (prefix id) map in
  let env, ds' =
    List.fold_left
      (fun (env, ds) d ->
        let d = subst#visit_decl env d in
        let env, d =
          match d.d with
          | DConst (id, x, y) ->
            ( { env with vars = add_entry env.vars id }
            , DConst (prefix id, x, y) |> wrap d )
          | DExtern (id, x) ->
            ( { env with vars = add_entry env.vars id }
            , DExtern (prefix id, x) |> wrap d )
          | DSymbolic (id, x) ->
            ( { env with vars = add_entry env.vars id }
            , DSymbolic (prefix id, x) |> wrap d )
          | DFun (id, x, y, z) ->
            ( { env with vars = add_entry env.vars id }
            , DFun (prefix id, x, y, z) |> wrap d )
          | DMemop (id, x, y) ->
            ( { env with vars = add_entry env.vars id }
            , DMemop (prefix id, x, y) |> wrap d )
          | DEvent (id, x, y, z, w) ->
            ( { env with vars = add_entry env.vars id }
            , DEvent (prefix id, x, y, z, w) |> wrap d )
          | DHandler (id, x, y) ->
            ( { env with vars = add_entry env.vars id }
            , DHandler (prefix id, x, y) |> wrap d )
          | DConstr (id, x, y, z) ->
            ( { env with vars = add_entry env.vars id }
            , DConstr (prefix id, x, y, z) |> wrap d )
          | DGlobal (id, x, y) ->
            ( { env with vars = add_entry env.vars id }
            , DGlobal (prefix id, x, y) |> wrap d )
          | DSize (id, x) ->
            ( { env with sizes = add_entry env.sizes id }
            , DSize (prefix id, x) |> wrap d )
          | DUserTy (id, x, y) ->
            ( { env with types = add_entry env.types id }
            , DUserTy (prefix id, x, y) |> wrap d )
          | DAction (id, x, y, z) ->
            ( { env with vars = add_entry env.vars id }
            , DAction (prefix id, x, y, z) |> wrap d )
          | DParser (id, x, y) ->
            ( { env with vars = add_entry env.vars id }
            , DParser (prefix id, x, y) |> wrap d )
          | DModule (id, _, ds) ->
            let _, ds = replace_module env id ds in
            replace_module env m_id ds
          | DModuleAlias _ -> failwith "Should be eliminated before this"
        in
        env, d :: ds)
      (env, [])
      ds
  in
  let env = add_definitions m_id env ds in
  env, List.concat @@ List.rev ds'
;;

let replace_decl env d =
  match d.d with
  | DModule (id, _, ds) -> replace_module env id ds
  | _ -> env, [subst#visit_decl env d]
;;

let eliminate_prog ds =
  let _, ds =
    List.fold_left
      (fun (env, ds) d ->
        let env, d = replace_decl env d in
        env, d :: ds)
      (empty_env, [])
      ds
  in
  List.concat @@ List.rev ds
;;
