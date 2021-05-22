open Batteries
open Syntax
open Collections

type kind = [%import: TyperUtil.kind]

module KindSet = TyperUtil.KindSet

(*** Do alpha-renaming to ensure that variable names are globally unique. Do the
    same for size names ***)

type env =
  { var_map : Cid.t CidMap.t
  ; size_map : Cid.t CidMap.t
  ; ty_map : Cid.t CidMap.t
  ; active : int (* Tells us which map to do lookups in at any given point *)
  ; in_constr : bool
  ; module_defs : KindSet.t
  }

let empty_env =
  { var_map = CidMap.empty
  ; size_map = CidMap.empty
  ; ty_map = CidMap.empty
  ; active = 0
  ; in_constr = false
  ; module_defs = KindSet.empty
  }
;;

(* After going through a module body, add all the new definitions to the old
   environment, but with the module id as a prefix *)
let add_module_defs m_id old_env m_env =
  let prefix cid = Compound (m_id, cid) in
  let prefixed_maps =
    KindSet.fold
      (fun (k, cid) acc ->
        match k with
        | KSize ->
          let size = CidMap.find cid m_env.size_map |> prefix in
          { acc with size_map = CidMap.add (prefix cid) size acc.size_map }
        | KConstr | KConst ->
          let x = CidMap.find cid m_env.var_map |> prefix in
          { acc with var_map = CidMap.add (prefix cid) x acc.var_map }
        | KGlobalTy ->
          let x = CidMap.find cid m_env.ty_map |> prefix in
          { acc with ty_map = CidMap.add (prefix cid) x acc.ty_map }
        | KHandler -> acc)
      m_env.module_defs
      old_env
  in
  { prefixed_maps with
    module_defs =
      KindSet.union
        old_env.module_defs
        (KindSet.map (fun (k, id) -> k, prefix id) m_env.module_defs)
  }
;;

(* Unfortunately, scope isn't baked into the structure of our syntax the way it
   is in OCaml. This means that we need to maintain a global environment instead
   of threading it through function calls. This also means that we need to reset
   that environment at the end of each scope. A scope is created every time we
   recurse into a statement, except in SSeq. *)
let rename prog =
  let v =
    object (self)
      inherit [_] s_map as super

      val mutable env : env =
        let open Builtins in
        (* Builtin stuff doesn't get renamed *)
        let var_map =
          List.fold_left
            (fun env id -> CidMap.add (Id id) (Id id) env)
            CidMap.empty
            (start_id :: this_id :: List.map fst builtin_vars)
        in
        let builtin_cids =
          List.map fst (Arrays.constructors @ Counters.constructors)
          @ List.map
              (fun (gf : InterpState.State.global_fun) -> gf.cid)
              builtin_defs
        in
        let var_map =
          List.fold_left
            (fun env cid -> CidMap.add cid cid env)
            var_map
            builtin_cids
        in
        let ty_map =
          List.fold_left
            (fun env cid -> CidMap.add cid cid env)
            CidMap.empty
            [Arrays.t_id; Counters.t_id]
        in
        { empty_env with var_map; ty_map }

      method freshen_any active x =
        let new_x = Cid.fresh (Cid.names x) in
        (match active with
        | 0 ->
          env
            <- { env with
                 module_defs = KindSet.add (KConst, x) env.module_defs
               ; var_map = CidMap.add x new_x env.var_map
               }
        | 1 ->
          env
            <- { env with
                 module_defs = KindSet.add (KSize, x) env.module_defs
               ; size_map = CidMap.add x new_x env.size_map
               }
        | _ ->
          env
            <- { env with
                 module_defs = KindSet.add (KGlobalTy, x) env.module_defs
               ; ty_map = CidMap.add x new_x env.ty_map
               });
        new_x

      method freshen_var x = self#freshen_any 0 (Id x) |> Cid.to_id

      method freshen_size x = self#freshen_any 1 (Id x) |> Cid.to_id

      method freshen_ty x = self#freshen_any 2 (Id x) |> Cid.to_id

      method lookup x =
        let map =
          match env.active with
          | 0 -> env.var_map
          | 1 -> env.size_map
          | _ -> env.ty_map
        in
        match CidMap.find_opt x map with
        | Some x -> x
        | _ -> failwith @@ "Renaming: Lookup failed: " ^ Cid.to_string x

      method activate_var () = env <- { env with active = 0 }

      method activate_size () = env <- { env with active = 1 }

      method activate_ty () = env <- { env with active = 2 }

      method! visit_ty dummy ty =
        let old = env in
        self#activate_ty ();
        let ret = super#visit_ty dummy ty in
        env <- { env with active = old.active };
        ret

      method! visit_global_ty dummy (cid, sizes) =
        let old = env in
        self#activate_ty ();
        let cid = self#lookup cid in
        env <- { env with active = old.active };
        cid, List.map (self#visit_size dummy) sizes

      method! visit_size dummy size =
        let old = env in
        self#activate_size ();
        let ret = super#visit_size dummy size in
        env <- { env with active = old.active };
        ret

      (*** Replace variable uses. Gotta be careful not to miss any cases later
           on so we don't accidentally rewrite extra things ***)
      method! visit_id _ x = self#lookup (Id x) |> Cid.to_id

      method! visit_cid _ c = self#lookup c

      (*** Places we bind new variables ***)
      method! visit_SLocal dummy x ty body =
        let replaced_body = self#visit_exp dummy body in
        let new_ty = self#visit_ty dummy ty in
        let new_x = self#freshen_var x in
        SLocal (new_x, new_ty, replaced_body)

      method! visit_body dummy (params, body) =
        let old_env = env in
        let new_params =
          List.map
            (fun (id, ty) -> self#freshen_var id, self#visit_ty dummy ty)
            params
        in
        let new_body = self#visit_statement dummy body in
        env <- old_env;
        new_params, new_body

      (* Since many declarations have special behavior, we'll just override
         visit_d. *)
      method! visit_d dummy d =
        (* print_endline @@ "Working on:" ^ Printing.d_to_string d; *)
        match d with
        | DGlobal (x, gty, cid, args) ->
          let replaced_gty = self#visit_global_ty dummy gty in
          let replaced_cid = self#visit_cid dummy cid in
          let replaced_args = List.map (self#visit_exp dummy) args in
          let new_x = if env.in_constr then x else self#freshen_var x in
          DGlobal (new_x, replaced_gty, replaced_cid, replaced_args)
        | DSize (x, size) ->
          let replaced_size = self#visit_size dummy size in
          let new_x = self#freshen_size x in
          DSize (new_x, replaced_size)
        | DMemop (x, body) ->
          let replaced_body = self#visit_body dummy body in
          let new_x = self#freshen_var x in
          DMemop (new_x, replaced_body)
        | DEvent (x, s, cspecs, params) ->
          let old_env = env in
          let new_params =
            List.map
              (fun (id, ty) -> self#freshen_var id, self#visit_ty dummy ty)
              params
          in
          let new_cspecs = List.map (self#visit_constr_spec dummy) cspecs in
          env <- old_env;
          let new_x = self#freshen_var x in
          DEvent (new_x, s, new_cspecs, new_params)
        | DHandler (x, body) ->
          (* Note that we require events to be declared before their handler *)
          DHandler (self#lookup (Id x) |> Cid.to_id, self#visit_body dummy body)
        | DFun (f, rty, cspecs, (params, body)) ->
          let old_env = env in
          let new_rty = self#visit_ty dummy rty in
          let new_params =
            List.map
              (fun (id, ty) -> self#freshen_var id, self#visit_ty dummy ty)
              params
          in
          let new_cspecs = List.map (self#visit_constr_spec dummy) cspecs in
          let new_body = self#visit_statement dummy body in
          env <- old_env;
          let new_f = self#freshen_var f in
          DFun (new_f, new_rty, new_cspecs, (new_params, new_body))
        | DConst (x, ty, exp) ->
          let new_exp = self#visit_exp dummy exp in
          let new_ty = self#visit_ty dummy ty in
          let new_x = if env.in_constr then x else self#freshen_var x in
          DConst (new_x, new_ty, new_exp)
        | DExtern (x, ty) ->
          let new_ty = self#visit_ty dummy ty in
          let new_x = self#freshen_var x in
          DExtern (new_x, new_ty)
        | DGroup (x, es) ->
          let new_es = List.map (self#visit_exp dummy) es in
          let new_x = self#freshen_var x in
          DGroup (new_x, new_es)
        | DGlobalTy (id, ids, params) ->
          let orig_env = env in
          let new_ids = List.map self#freshen_size ids in
          let new_params =
            List.map (fun (id, ty) -> id, self#visit_ty dummy ty) params
          in
          env <- orig_env;
          let new_id = self#freshen_ty id in
          DGlobalTy (new_id, new_ids, new_params)
        | DConstr { constr_id; ty_id; size_args; params; body } ->
          let orig_env = env in
          let size_args = List.map self#freshen_size size_args in
          let params =
            List.map
              (fun (id, ty) -> self#freshen_var id, self#visit_ty dummy ty)
              params
          in
          env <- { env with in_constr = true };
          let body = self#visit_decls dummy body in
          env <- orig_env;
          env <- { env with in_constr = false };
          (* Not sure why this is needed but it is *)
          self#activate_ty ();
          let ty_id = self#lookup ty_id in
          self#activate_var ();
          let constr_id = self#freshen_var constr_id in
          DConstr { constr_id; ty_id; size_args; params; body }
        | DModule (id, intf, body) ->
          let orig_env = env in
          env <- { env with module_defs = KindSet.empty };
          let body = self#visit_decls dummy body in
          let intf = self#visit_interface dummy intf in
          let new_env = add_module_defs id orig_env env in
          env <- new_env;
          DModule (id, intf, body)

      (*** Places we enter a scope ***)
      method! visit_SIf dummy test left right =
        let orig_env = env in
        let test' = self#visit_exp dummy test in
        let left' = self#visit_statement dummy left in
        env <- orig_env;
        let right' = self#visit_statement dummy right in
        env <- orig_env;
        SIf (test', left', right')

      (*** Special Cases ***)
      method! visit_TFun dummy func =
        let orig_env = env in
        let new_arg_tys = List.map (self#visit_raw_ty dummy) func.arg_tys in
        let new_ret_ty = self#visit_raw_ty dummy func.ret_ty in
        env <- orig_env;
        TFun { func with arg_tys = new_arg_tys; ret_ty = new_ret_ty }

      method! visit_params dummy params =
        (* Don't rename parameters unless they're part of a body declaration *)
        List.map (fun (id, ty) -> id, self#visit_ty dummy ty) params

      (* Declaration-like things where we don't rename parts of them *)
      method! visit_InGlobalTy dummy id size_ids params =
        let orig_env = env in
        self#activate_ty ();
        let id = self#visit_id dummy id in
        self#activate_size ();
        let size_ids = List.map self#freshen_size size_ids in
        self#activate_var ();
        let params = self#visit_params dummy params in
        env <- orig_env;
        InGlobalTy (id, size_ids, params)

      method! visit_InConstr dummy id ret_id size_ids params =
        let orig_env = env in
        let id = self#visit_id dummy id in
        self#activate_ty ();
        let ret_id = self#visit_cid dummy ret_id in
        self#activate_size ();
        let size_ids = List.map self#freshen_size size_ids in
        self#activate_var ();
        let params = self#visit_params dummy params in
        env <- orig_env;
        InConstr (id, ret_id, size_ids, params)

      method! visit_InModule dummy id intf =
        InModule (id, self#visit_interface dummy intf)

      method! visit_InFun dummy id rty cspecs params =
        let new_id = self#visit_id dummy id in
        let old_env = env in
        let new_rty = self#visit_ty dummy rty in
        let new_params =
          List.map
            (fun (id, ty) -> self#freshen_var id, self#visit_ty dummy ty)
            params
        in
        let new_cspecs = List.map (self#visit_constr_spec dummy) cspecs in
        env <- old_env;
        InFun (new_id, new_rty, new_cspecs, new_params)

      method! visit_InEvent dummy id cspecs params =
        let new_id = self#visit_id dummy id in
        let old_env = env in
        let new_params =
          List.map
            (fun (id, ty) -> self#freshen_var id, self#visit_ty dummy ty)
            params
        in
        let new_cspecs = List.map (self#visit_constr_spec dummy) cspecs in
        env <- old_env;
        InEvent (new_id, new_cspecs, new_params)

      (* Ids inside tqvars aren't variable IDs and shouldn't be renamed *)
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

      method rename prog =
        self#activate_var ();
        let renamed = self#visit_decls () prog in
        env, renamed
    end
  in
  v#rename prog
;;
