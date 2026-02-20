open Syntax
open SyntaxUtils
open Collections
open Batteries

(* Region-like levels for efficient implementation of type generalization
   code following http://okmij.org/ftp/ML/generalization.html#levels
*)
(* Since we currently only generalize at DFuns, the whole level setup is
   overkill, but it'll be handy if we get more functional later on. *)
let current_level = ref 0
let enter_level () = incr current_level
let leave_level () = decr current_level
let level () = !current_level
let level_reset () = current_level := 0

(* Create new unbound TVars of each type *)
let fresh_tvar name = TVar (ref (Unbound (Id.fresh name, level ())))
let fresh_size ?(name = "sz") () = IVar (fresh_tvar name)
let fresh_effect ?(name = "eff") () = FVar (fresh_tvar name)

let fresh_type ?(name = "a") () =
  ty_eff (TQVar (fresh_tvar name)) (fresh_effect ())
;;

let error_sp sp msg = Console.error_position sp msg
let strip_links ty = { ty with raw_ty = TyTQVar.strip_links ty.raw_ty }

type modul =
  { sizes : IdSet.t
  ; vars : ty IdMap.t
  ; parsers : params IdMap.t
  ; user_tys : (sizes * ty) IdMap.t
  ; constructors : func_ty IdMap.t
  ; submodules : modul IdMap.t
  }

let empty_modul =
  { sizes = IdSet.empty
  ; vars = IdMap.empty
  ; parsers = IdMap.empty
  ; user_tys = IdMap.empty
  ; constructors = IdMap.empty
  ; submodules = IdMap.empty
  }
;;

let lookup_modul_size id m = IdSet.find_opt id m.sizes
let lookup_modul_var id m = IdMap.find_opt id m.vars
let lookup_modul_parser id m = IdMap.find_opt id m.parsers
let lookup_modul_ty id m = IdMap.find_opt id m.user_tys
let lookup_modul_constr id m = IdMap.find_opt id m.constructors
let lookup_modul_submodule id m = IdMap.find_opt id m.submodules

type env =
  { (*** Global information ***)
    current_modul : modul
  ; parents : modul list
  ; record_labels : ty StringMap.t (* Maps labels to the gty with that label *)
  ; (*** Information we use while typechecking function/handler bodies ***)
    locals : ty IdMap.t
  ; current_effect : effect
  ; (* Maps vector index vars to their max size and an alpha-renamed cid *)
    indices : (id * size) IdMap.t
  ; constraints : constr list
  ; ret_ty : ty option (* Some iff we're in a function body *)
  ; ret_effects : effect list (* All the effects we `return`ed at *)
  ; returned : bool (* If we ran into a return statement *)
  ; in_global_def : bool (* True if we're able to use constructors *)
  }

let empty_env =
  { locals = IdMap.empty
  ; parents = []
  ; indices = IdMap.empty
  ; current_effect = FZero
  ; constraints = []
  ; ret_ty = None
  ; ret_effects = []
  ; returned = false
  ; record_labels = StringMap.empty
  ; in_global_def = false
  ; current_modul = empty_modul
  }
;;

let define_size id (env : env) =
  { env with
    current_modul =
      { env.current_modul with sizes = IdSet.add id env.current_modul.sizes }
  }
;;

let define_const id ty (env : env) =
  { env with
    current_modul =
      { env.current_modul with vars = IdMap.add id ty env.current_modul.vars }
  }
;;

let define_constructor id fty (env : env) =
  { env with
    current_modul =
      { env.current_modul with
        constructors = IdMap.add id fty env.current_modul.constructors
      }
  }
;;

let define_user_ty id sizes ty (env : env) =
  { env with
    current_modul =
      { env.current_modul with
        user_tys = IdMap.add id (sizes, ty) env.current_modul.user_tys
      }
  }
;;

let define_parser id params (env : env) =
  { env with
    current_modul =
      { env.current_modul with
        parsers = IdMap.add id params env.current_modul.parsers
      }
  }
;;

let define_submodule id m (env : env) =
  { env with
    current_modul =
      { env.current_modul with
        submodules = IdMap.add id m env.current_modul.submodules
      }
  }
;;

let default_env =
  let modules =
    List.map
      (fun (id, tys, defs, constructors) ->
        let vars =
          List.fold_left
            (fun acc (r : InterpState.global_fun) ->
              IdMap.add (Cid.last_id r.cid) r.ty acc)
            IdMap.empty
            defs
        in
        let constructors =
          List.fold_left
            (fun acc (cid, fty) -> IdMap.add (Cid.last_id cid) fty acc)
            IdMap.empty
            constructors
        in
        let user_tys =
          List.fold_left
            (fun acc (tid, sizes, ty) -> IdMap.add tid (sizes, ty) acc)
            IdMap.empty
            tys
        in
        id, { empty_modul with vars; constructors; user_tys })
        (List.map LibraryInterface.sigty_to_tup (Builtins.builtin_modules))
  in
  let submodules =
    List.fold_left
      (fun acc (id, modul) -> IdMap.add id modul acc)
      IdMap.empty
      modules
  in
  let global_vars =
    List.fold_left
      (fun acc (id, ty) -> IdMap.add id ty acc)
      IdMap.empty
      Builtins.builtin_vars
  in
  let current_modul =
    { empty_env.current_modul with submodules; vars = global_vars }
  in
  { empty_env with current_modul }
;;

(* Helper function, you probably don't want to call this directly. We expect
   f to be one of the "lookup_modul_xxx" functions from above. *)
let lookup_any span lookup env cid =
  match cid with
  | Id id ->
    (* Walk up through parents, checking at each step *)
    List.find_map_opt (lookup id) (env.current_modul :: env.parents)
  | Compound (id, cid) ->
    (* Walk up until we find a module with the appropriate name *)
    let starting_module =
      List.find_map_opt
        (fun m -> IdMap.find_opt id m.submodules)
        (env.current_modul :: env.parents)
    in
    let starting_module =
      match starting_module with
      | Some x -> x
      | None ->
        Console.error_position span
        @@ "Unknown module "
        ^ Printing.id_to_string id
    in
    (* Now walk down through that module's submodules until we hit the end of the cid *)
    let final_id, prefix = Cid.to_ids_prefix cid in
    let _, final_modul =
      List.fold_left
        (fun (path, m) id ->
          match IdMap.find_opt id m.submodules with
          | Some m -> id :: path, m
          | None ->
            Console.error_position span
            @@ "Unknown module "
            ^ BatString.concat
                "."
                (List.rev_map Printing.id_to_string (id :: path)))
        ([id], starting_module)
        prefix
    in
    (* Finally, do the appropriate lookup in the module we ended up at *)
    lookup final_id final_modul
;;

let size_exists span env cid =
  match lookup_any span lookup_modul_size env cid with
  | None -> false
  | _ -> true
;;

let lookup_ty span env cid =
  match lookup_any span lookup_modul_ty env cid with
  | Some x -> x
  | None ->
    Console.error_position span @@ "Unknown type " ^ Printing.cid_to_string cid
;;

let lookup_parser span env cid =
  match lookup_any span lookup_modul_parser env cid with
  | Some x -> x
  | None ->
    Console.error_position span
    @@ "Unknown parser "
    ^ Printing.cid_to_string cid
;;

let lookup_var span env cid =
  let local_val =
    match cid with
    | Id id -> IdMap.find_opt id env.locals
    | _ -> None
  in
  match local_val with
  | Some t -> t
  | None ->
    let lookup_fun id m =
      match lookup_modul_var id m with
      | Some t -> Some t
      | None ->
        (match lookup_modul_constr id m with
         | None -> None
         | Some t ->
           if env.in_global_def || not (is_global t.ret_ty)
           then Some (ty (TFun t))
           else
             error_sp
               span
               "Cannot call global constructor except in global definitions or \
                other constructors")
    in
    (match lookup_any span lookup_fun env cid with
     | Some t -> t
     | None -> error_sp span @@ "Unbound variable " ^ Printing.cid_to_string cid)
;;

let lookup_module span env cid =
  match lookup_any span lookup_modul_submodule env cid with
  | Some m -> m
  | None ->
    Console.error_position span
    @@ "Unknown module "
    ^ Printing.cid_to_string cid
;;

let add_locals env bindings =
  let locals =
    List.fold_left (fun acc (id, ty) -> IdMap.add id ty acc) env.locals bindings
  in
  { env with locals }
;;

(* Drops the last n constraints in the second environment and returns
   the rest. For use after if/match statments, where the result after
   each side constains all the constraints from the original env plus
   maybe some more. This is a quick way of preventing duplicates. *)
let drop_constraints (env : env) (env' : env) =
  List.take
    (List.length env'.constraints - List.length env.constraints)
    env'.constraints
;;

let drop_ret_effects (env : env) (env' : env) =
  List.take
    (List.length env'.ret_effects - List.length env.ret_effects)
    env'.ret_effects
;;

let wrap e ety = { e with ety }

let textract (env, e) =
  match e.ety with
  | None -> failwith "internal error (textract)"
  | Some ty -> env, e, ty
;;

(* Construct the type for an event creation function given its constraints
   and parameters *)
let mk_event_ty constrs params =
  let eff = FVar (QVar (Id.fresh "start")) in
  ty
  @@ TFun
       { arg_tys = List.map snd params
       ; ret_ty = ty TEvent
       ; start_eff = eff
       ; end_eff = eff
       ; constraints = ref constrs
       }
;;

(* Given a printf statement, return the list of expected types for its arguments
   based on the formatters in the string *)
let extract_print_tys span (s : string) =
  let rec aux acc chars =
    match chars with
    | [] -> acc
    | '\\' :: '%' :: tl -> aux acc tl
    | '%' :: 'b' :: tl -> aux (TBool :: acc) tl
    | '%' :: 'd' :: tl -> aux (TInt (fresh_size ()) :: acc) tl
    | '%' :: _ -> error_sp span "Invalid % in printf string"
    | _ :: tl -> aux acc tl
  in
  List.rev @@ aux [] (String.to_list s)
;;


let rec validate_size span env size =
  match STQVar.strip_links size with
  | IUser cid ->
    if not (size_exists span env cid)
    then error_sp span @@ "Unknown size " ^ Printing.cid_to_string cid
  | ISum (sizes, n) ->
    if n < 0 then error_sp span @@ "Size sum had negative number?";
    List.iter (validate_size span env) sizes
  | IConst _ | IVar _ -> ()
  | ITup(sizes) -> 
    List.iter (validate_size span env) sizes
;;

(* Turn a user's constraint specification into a list of actual constraints
   on the effects of the parameters, as well as a starter list of ending effects
*)
let spec_to_constraints (env : env) sp start_eff (params : params) specs =
  let lookup_effect env cid =
    if Cid.names cid = ["start"]
    then start_eff
    else (
      let ty = lookup_var sp env cid in
      if not (is_global ty)
      then
        error_sp sp
        @@ "Variable "
        ^ Printing.cid_to_string cid
        ^ " is not obviously global and cannot appear in a constraint."
      else ty.teffect)
  in
  let env =
    List.fold_left
      (fun env (id, ty) -> { env with locals = IdMap.add id ty env.locals })
      env
      params
  in
  let constraints =
    List.concat
    @@ List.map
         (function
          | CSpec lst ->
            let left = List.take (List.length lst - 1) lst in
            let right = List.tl lst in
            List.map2
              (fun (cid1, cmp) (cid2, _) ->
                let left_eff =
                  match cmp with
                  | SpecLess -> FSucc (lookup_effect env cid1)
                  | SpecLeq -> lookup_effect env cid1
                in
                CLeq (left_eff, lookup_effect env cid2))
              left
              right
          | CEnd _ -> [])
         specs
  in
  let end_eff =
    let ends =
      List.filter_map
        (function
         | CEnd id -> Some (FSucc (lookup_effect env id))
         | CSpec _ -> None)
        specs
    in
    match ends with
    | [] -> None
    | [hd] -> Some hd
    | _ -> error_sp sp @@ "Cannot specify more than one end constraint"
  in
  constraints, end_eff
;;

let add_record_label env span recty l =
  let l = Id.name l in
  match StringMap.find_opt l env.record_labels with
  | None -> { env with record_labels = StringMap.add l recty env.record_labels }
  | Some ty ->
    error_sp span
    @@ Printf.sprintf
         "The label %s already exists in type %s"
         l
         (Printing.ty_to_string ty)
;;

type loop_subst = (id * int) * (id * effect)

let subst_loop =
  object (self)
    inherit [_] s_map

    method! visit_FIndex (env : loop_subst) id eff =
      let eff = self#visit_effect env eff in
      let target, n = fst env in
      if Id.equal id target
      then if n = 0 then FProj eff else FSucc (FProj eff)
      else FIndex (id, eff)

    method! visit_FVar (env : loop_subst) tqv =
      let target, eff = snd env in
      match tqv with
      | TVar { contents = Unbound (id, _) } ->
        if Id.equal id target then eff else FVar tqv
      | _ -> FVar (self#visit_tqvar self#visit_effect env tqv)
  end
;;

let drop_indexes target eff =
  let rec aux lst =
    match lst with
    | [] -> []
    | (Some id, _) :: _ when Id.equal id target -> []
    | hd :: tl -> hd :: aux tl
  in
  let base, lst = unwrap_effect eff in
  let lst = aux lst in
  wrap_effect base lst
;;

let lookup_TName span env rty =
  match TyTQVar.strip_links rty with
  | TName (cid, sizes, _) ->
    let sizes', ty = lookup_ty span env cid in
    let replaced_ty =
      (* replace size ids in sizes' with sizes *)
      ReplaceUserTys.subst_sizes
        span
        cid
        ty.raw_ty
        (ReplaceUserTys.extract_ids span sizes') 
        sizes
    in

    replaced_ty
  | rty -> rty
;;

let rec modul_to_string ?(show_defs = true) m =
  let open Printing in
  Printf.sprintf
    "{\n\
     sizes = [%s]\n\
     vars = {%s}\n\
     parsers = {%s}\n\
     user_tys = {%s}\n\
     constructors = {%s}\n\
     submodules = {%s}\n\
     }"
    (IdSet.fold (fun s acc -> id_to_string s ^ ", " ^ acc) m.sizes "")
    (IdMap.fold
       (fun id ty acc ->
         let str =
           if show_defs
           then id_to_string id ^ " -> " ^ ty_to_string ty
           else id_to_string id
         in
         str ^ ", " ^ acc)
       m.vars
       "")
    (IdMap.fold
       (fun id params acc ->
         let str =
           if show_defs
           then id_to_string id ^ " -> " ^ params_to_string params
           else id_to_string id
         in
         str ^ ", " ^ acc)
       m.parsers
       "")
    (IdMap.fold
       (fun id (_, ty) acc ->
         let str =
           if show_defs
           then id_to_string id ^ " -> " ^ ty_to_string ty
           else id_to_string id
         in
         str ^ ", " ^ acc)
       m.user_tys
       "")
    (IdMap.fold
       (fun id ty acc ->
         let str =
           if show_defs
           then id_to_string id ^ " -> " ^ func_to_string ty
           else id_to_string id
         in
         str ^ ", " ^ acc)
       m.constructors
       "")
    (IdMap.fold
       (fun id m acc ->
         let str =
           if show_defs
           then id_to_string id ^ " -> " ^ modul_to_string m
           else id_to_string id
         in
         str ^ ", " ^ acc)
       m.submodules
       "")
;;

(* Note: causes an unbound variable error if the variable is unbound *)
let is_const env span id =
  match IdMap.find_opt id env.locals with
  | Some _ -> false
  | None ->
    (match lookup_var span env (Id id) with
     | _ -> true)
;;
