open Syntax
open SyntaxUtils
open Batteries
open Collections

(*** Replace abstract types with their definitions. We run
     this after module elimination, to eliminate all abstract types
     except the builtin ones (Arrays and Counters) ***)

(* Go through raw_ty and replace the qvars corresponding to size_vars with the
   corresponding entry in sizes. *)
let subst =
  object
    inherit [_] s_map

    method! visit_IVar (env : size IdMap.t) tqv =
      match STQVar.strip_links (IVar tqv) with
      | IVar (QVar id) -> begin
        match IdMap.find_opt id env with
        | Some sz -> sz
        | None -> IVar tqv
      end
      | _ -> IVar tqv
  end
;;

let subst_sizes span tyname raw_ty size_vars sizes =
  let mapping =
    try List.combine size_vars sizes with
    | Invalid_argument _ ->
      Console.error_position span
      @@ Printf.sprintf
           "Type %s expects %d arguments, but was given %d."
           (Printing.cid_to_string tyname)
           (List.length size_vars)
           (List.length sizes)
  in
  if List.is_empty mapping (* Not polymorphic *)
  then raw_ty
  else (
    let map =
      List.fold_left
        (fun acc (id, sz) -> IdMap.add id sz acc)
        IdMap.empty
        mapping
    in
    subst#visit_raw_ty map raw_ty)
;;

(* subst for type arguments of builtin module types *)
let subst_ty = 
  object
    inherit [_] s_map
    method! visit_TQVar (env : raw_ty IdMap.t) tqv =      
      match TyTQVar.strip_links (TQVar tqv) with
      | TQVar (QVar id) -> begin
        match IdMap.find_opt id env with
        | Some ty -> ty
        | None -> TQVar tqv
      end
      | _ -> TQVar tqv
  end
;;
let subst_ty_args span tname raw_ty ty_vars arg_tys = 
  let mapping = 
    try List.combine ty_vars arg_tys with
    | Invalid_argument _ ->
      Console.error_position span
      @@ Printf.sprintf
           "Type %s expects %d arguments, but was given %d."
           (Printing.cid_to_string tname)
           (List.length ty_vars)
           (List.length arg_tys)
  in
  if List.is_empty mapping (* Not polymorphic *)
  then raw_ty
  else (
    let map =
      List.fold_left
        (fun acc (id, ty) -> IdMap.add id ty acc)
        IdMap.empty
        mapping
    in
    subst_ty#visit_raw_ty map raw_ty)
;;




(* Maps each type name to its definition, as well as a list of sizes to unify
   the size arguments with. The ids are the QVar ids for any polymorphic size args.
   Also keep a list of type ids to unify the type arguments with, if any. *)
type env =
  { mapping : (raw_ty * id list * id list) CidMap.t
  ; module_defs : CidSet.t
  }

let base_env () : env ref =
  let mk_entry t_id n_sizes global n_tyargs =
    let size_ids = List.init n_sizes (fun _ -> Id.fresh "sz") in
    let sizes = List.map (fun id -> IVar (QVar id)) size_ids in
    let ty_ids = List.init n_tyargs (fun _ -> Id.fresh "tyarg") in
    let tys = List.map (fun id -> (TQVar (QVar id))) ty_ids in
    TName (t_id, sizes, global, tys), size_ids, ty_ids
  in
  let mapping =
    List.fold_left
      (fun acc (t_id, sizes, global, ty_args) ->
        CidMap.add t_id (mk_entry t_id sizes global ty_args) acc)
      CidMap.empty
      Builtins.builtin_type_info
  in
  ref { mapping; module_defs = CidSet.empty }
;;

let extract_ids span sizes =
  List.map
    (function
     | IVar (QVar id) -> id
     | _ ->
       Console.error_position
         span
         "User types must be declared with polymorphic size variables \
          (beginning with a tick")
    sizes
;;

(* add an entry for a user-defined type. Since user types cannot have 
   type arguments, the entry ends with an empty list ((rty, size_vars, [])) *)
let add_entry env id rty size_vars =
  env
    := { mapping = CidMap.add (Id id) (rty, size_vars, []) !env.mapping
       ; module_defs = CidSet.add (Id id) !env.module_defs
       }
;;

let prefixer =
  object (self)
    inherit [_] s_map

    method! visit_TName (f, mdefs) cid sizes b tyargs =
      let sizes = List.map (self#visit_size (f, mdefs)) sizes in
      if CidSet.mem cid mdefs
      then TName (f cid, sizes, b, tyargs)
      else TName (cid, sizes, b, tyargs)
  end
;;

let add_module_defs orig_env m_env m_id =
  let prefix cid = Compound (m_id, cid) in
  let mapping =
    CidSet.fold
      (fun cid acc ->
        let rty, sizes, ty_args = CidMap.find cid m_env.mapping in
        let rty = prefixer#visit_raw_ty (prefix, m_env.module_defs) rty in
        CidMap.add (prefix cid) (rty, sizes, ty_args) acc)
      m_env.module_defs
      orig_env.mapping
  in
  let module_defs =
    CidSet.union orig_env.module_defs (CidSet.map prefix m_env.module_defs)
  in
  { mapping; module_defs }
;;

let replacer =
  object (self)
    inherit [_] s_map as super

    (* FIXME: Currently this ignores the print_as field (which is probably fine),
      since that mechanism seems to do more harm than good anyway *)
    method! visit_raw_ty (env : env ref) raw_ty =
      match raw_ty with
      | TName (cid, sizes, _, arg_tys) -> begin
        match CidMap.find_opt cid !env.mapping with
        | None -> Console.error @@ "Unknown type " ^ Printing.cid_to_string cid
        | Some (raw_ty, size_vars, ty_vars) ->
          print_endline("[replacer] about to call subst_sizes");
          print_endline ("sizes = "^(Printing.sizes_to_string sizes));
          print_endline ("size_vars = "^(Printing.comma_sep Printing.id_to_string size_vars));
          let raw_ty = subst_sizes Span.default cid raw_ty size_vars sizes in
          subst_ty_args Span.default cid raw_ty ty_vars arg_tys
      end
      | _ -> super#visit_raw_ty env raw_ty

    method! visit_decl env d =
      match d.d with
      | DUserTy (id, sizes, ty) ->
        let sizes' = extract_ids d.dspan sizes in
        let ty = self#visit_ty env ty in
        add_entry env id ty.raw_ty sizes';
        { d with d = DUserTy (id, sizes, ty) }
      | DModule (id, intf, decls) ->
        let orig_env = !env in
        env := { orig_env with module_defs = CidSet.empty };
        let decls = self#visit_decls env decls in
        let intf =
          if List.is_empty intf
          then []
          else (
            env := { orig_env with module_defs = CidSet.empty };
            self#visit_interface env intf)
        in
        env := add_module_defs orig_env !env id;
        { d with d = DModule (id, intf, decls) }
      | _ -> super#visit_decl env d

    method! visit_interface_spec env ispec =
      match ispec.ispec with
      | InTy (id, sizes, tyo, b) ->
        let sizes' = extract_ids ispec.ispan sizes in
        let tyo = Option.map (self#visit_ty env) tyo in
        let ret_ty =
          match tyo with
          | Some ty -> ty.raw_ty
          (* TODO: double check the empty list for type args *)
          | None -> TName (Id id, sizes, b, [])
        in
        add_entry env id ret_ty sizes';
        { ispec with ispec = InTy (id, sizes, tyo, b) }
      | InModule (id, intf) ->
        let orig_env = !env in
        env := { orig_env with module_defs = CidSet.empty };
        let intf = self#visit_interface env intf in
        env := add_module_defs orig_env !env id;
        { ispec with ispec = InModule (id, intf) }
      | _ -> super#visit_interface_spec env ispec
  end
;;

let replace_prog ds = 
  let be = base_env () in
  (* (match CidMap.find_opt (Cid.create ["Table"; "t"]) !be.mapping with
  | Some (_, size_vars) ->
    print_endline ("in base_env -- size_vars = "^(Printing.comma_sep Printing.id_to_string size_vars));
    | _ -> ());
  exit 1; *)
  replacer#visit_decls (base_env ()) ds
