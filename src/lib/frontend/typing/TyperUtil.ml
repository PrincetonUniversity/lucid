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
let fresh_type ?(name = "a") () = TQVar (fresh_tvar name)
let error_sp sp msg = Console.error_position sp msg

(* Make sure a type is fully determined -- no QVars or unbound TVars.
   Events/Handlers are allowed to be polymorphic inside global types, though. *)
let ensure_concrete ?(check_effects = true) ty =
  let v =
    object (self)
      inherit [_] s_iter

      method! visit_Unbound _ _ _ _ =
        error_sp ty.tspan @@ "Unable to fully determine type of this expression"

      method! visit_QVar _ _ _ =
        error_sp ty.tspan
        @@ "Encountered QVar in a type that should be concrete"

      method! visit_TGlobal _ gty eff =
        self#visit_global_ty () gty;
        if check_effects then self#visit_effect () eff
    end
  in
  v#visit_ty () ty
;;

(* Not really kinds, but rather everything we have a different map for in the
   environment *)
type kind =
  | KSize
  | KConst
  | KHandler
  | KConstr
  | KGlobalTy

module KindSet = Set.Make (struct
  type t = kind * cid

  let compare = Pervasives.compare
end)

type env =
  { (* Maps for all the different kinds of variables we have in a program *)
    sizes : CidSet.t
  ; locals : raw_ty CidMap.t
  ; consts : raw_ty CidMap.t (* Constant variables *)
  ; (* Maps for special "variables" like handlers and constructors *)
    handlers : (constr list * params) CidMap.t
  ; constructors : (cid * id list * (* constr args *) raw_ty list) CidMap.t
  ; global_tys : (id list (* size args *) * params) CidMap.t
  ; global_labels : cid StringMap.t (* Maps labels to the gty with that label *)
  ; (* Track the things we've defined in the current module body *)
    module_defs : KindSet.t
  ; (* Information we use while typechecking function/handler bodies *)
    current_effect : effect
  ; constraints : constr list
  ; ret_ty : raw_ty option (* Some iff we're in a function body *)
  ; ret_effects : effect list (* All the effects we `return`ed at *)
  ; returned : bool (* If we ran into a return statement *)
  }

let empty_env =
  { sizes = CidSet.empty
  ; handlers = CidMap.empty
  ; locals = CidMap.empty
  ; consts = CidMap.empty
  ; current_effect = FZero
  ; constraints = []
  ; ret_ty = None
  ; ret_effects = []
  ; returned = false
  ; constructors = CidMap.empty
  ; global_tys = CidMap.empty
  ; global_labels = StringMap.empty
  ; module_defs = KindSet.empty
  }
;;

let default_env =
  let builtins =
    List.fold_left
      (fun acc (r : InterpState.State.global_fun) -> CidMap.add r.cid r.ty acc)
      CidMap.empty
      Builtins.builtin_defs
  in
  let consts =
    List.fold_left
      (fun acc (id, ty) -> CidMap.add (Id id) ty acc)
      builtins
      Builtins.builtin_vars
  in
  let constructors =
    List.fold_left
      (fun acc (cid, ty) -> CidMap.add cid ty acc)
      CidMap.empty
      (Arrays.constructors @ Counters.constructors)
  in
  let global_tys =
    CidMap.empty
    |> CidMap.add Arrays.t_id Arrays.sizes_labels
    |> CidMap.add Counters.t_id Counters.sizes_labels
  in
  { empty_env with consts; constructors; global_tys }
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

(** Inference and well-formedness for memops. They have a lot of restrictions
    on them: They must have exactly two arguments, an int<<'a>> and a 'b, plus
    their bodies follow a restricted grammar *)

(* An expression in a memop may use an unlimited number of binops, but
   only some are allowed. Furthermore, each parameter can appear only once in the
   expression; all other arguments must be constants. *)
let check_e cid1 cid2 allowed (seen1, seen2) exp =
  let rec aux (seen1, seen2) e =
    match e.e with
    | EVal _ | EInt _ -> seen1, seen2
    | EVar cid when Cid.equals cid cid1 ->
      if not seen1
      then true, seen2
      else
        error_sp
          exp.espan
          ("Parameter "
          ^ Cid.to_string cid
          ^ " appears more than once in memop expression")
    | EVar cid when Cid.equals cid cid2 ->
      if not seen2
      then seen1, true
      else
        error_sp
          exp.espan
          ("Parameter "
          ^ Cid.to_string cid
          ^ " appears more than once in memop expression")
    | EVar _ -> seen1, seen2
    | EOp (op, [e1; e2]) ->
      if allowed op
      then (
        let seen_vars = aux (seen1, seen2) e1 in
        aux seen_vars e2)
      else
        error_sp
          e.espan
          ("Disallowed operation in memop expression" ^ Printing.exp_to_string e)
    | _ ->
      error_sp
        e.espan
        ("Disallowed expression in memop expression: "
        ^ Printing.exp_to_string e)
  in
  aux (seen1, seen2) exp
;;

(* Similar to check_return, except the test of the body also conditionals *)
let check_test id1 id2 exp =
  let check_e = check_e (Id id1) (Id id2) in
  let allowed = function
    | Plus | Sub -> true
    | _ -> false
  in
  match exp.e with
  | EOp ((Eq | Neq | Less | More), [e1; e2]) ->
    let seen_vars1 = check_e allowed (false, false) e1 in
    ignore @@ check_e allowed seen_vars1 e2
  | _ -> ignore @@ check_e allowed (false, false) exp
;;

(* Construct the type for an event creation function given its constraints
   and parameters *)
let mk_event_ty constrs params =
  let eff = FVar (QVar (Id.fresh "start")) in
  TFun
    { arg_tys = List.map (fun (_, ty) -> ty.raw_ty) params
    ; ret_ty = TEvent false (* Events are singlecast by default *)
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
    if not (CidSet.mem cid env.sizes)
    then error_sp span @@ "Unknown size " ^ Printing.cid_to_string cid
  | ISum (sizes, n) ->
    if n < 0 then error_sp span @@ "Size sum had negative number?";
    List.iter (validate_size span env) sizes
  | IConst _ | IVar _ -> ()
;;

(* Turn a user's constraint specification into a list of actual constraints
   on the effects of the parameters, as well as a starter list of ending effects
*)
let spec_to_constraints (env : env) sp start_eff (params : params) specs =
  let rec extract_effect x rty =
    match rty with
    | TGlobal (_, eff) -> eff
    | TQVar (TVar { contents = Link ty }) -> extract_effect x ty
    | ty ->
      error_sp sp
      @@ Printf.sprintf
           "Parameter %s was used in a constraint, but its type %s is not a \
            global type"
           x
           (Printing.raw_ty_to_string ty)
  in
  let lookup_effect env cid =
    if Cid.names cid = ["start"]
    then start_eff
    else (
      match CidMap.find_opt cid env.locals with
      | Some ty -> extract_effect (Printing.cid_to_string cid) ty
      | None ->
        (match CidMap.find_opt cid env.consts with
        | Some ty -> extract_effect (Printing.cid_to_string cid) ty
        | _ -> error_sp sp @@ "Unbound variable %s" ^ Printing.cid_to_string cid))
  in
  let env =
    List.fold_left
      (fun env (id, ty) ->
        { env with locals = CidMap.add (Id id) ty.raw_ty env.locals })
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

let add_global_label env span g_id l =
  let l = Id.name l in
  let g_id = Id g_id in
  match StringMap.find_opt l env.global_labels with
  | None -> { env with global_labels = StringMap.add l g_id env.global_labels }
  | Some id ->
    error_sp span
    @@ Printf.sprintf
         "The label %s already exists in type %s"
         l
         (Printing.cid_to_string id)
;;

let subst_ivars mapping =
  let v =
    object
      inherit [_] s_map

      val mapping : cid CidMap.t = mapping

      method! visit_IUser _ cid =
        match CidMap.find_opt cid mapping with
        | None -> IUser cid
        | Some cid' -> IUser cid'
    end
  in
  v
;;

let inst_ivars mapping =
  let v =
    object
      inherit [_] s_map

      val mapping : size tqvar IdMap.t = !mapping

      method! visit_IUser _ cid =
        match cid with
        | Compound _ -> IUser cid
        | Id id ->
          (match IdMap.find_opt id mapping with
          | None -> IUser cid
          | Some x -> IVar x)
    end
  in
  v
;;

let validate_constr_body expected_sizes size_args labels body =
  let err span s exp got =
    error_sp span
    @@ Printf.sprintf
         "Declaration in constructor has wrong %s: expected %s but got %s"
         s
         exp
         got
  in
  let subst =
    subst_ivars
      (List.fold_left2
         (fun acc id1 id2 -> CidMap.add id1 id2 acc)
         CidMap.empty
         size_args
         expected_sizes)
  in
  List.iter2
    (fun d (id, ty) ->
      match d.d, ty.raw_ty with
      | (DGlobal (id2, _, _, _) | DConst (id2, _, _)), _
        when not (Id.equal id id2) ->
        err d.dspan "name" (Id.name id) (Id.name id2)
      | DGlobal (_, gty2, _, _), TGlobal (gty1, _) ->
        if gty1 <> subst#visit_global_ty () gty2
        then
          err
            d.dspan
            "type"
            (Printing.gty_to_string gty1)
            (Printing.gty_to_string gty2)
      | DGlobal (_, gty2, _, _), ty ->
        err
          d.dspan
          "type"
          (Printing.raw_ty_to_string ty)
          (Printing.gty_to_string gty2)
      | DConst (_, ty2, _), _ ->
        if not (equiv_ty ty (subst#visit_ty () ty2))
        then
          err
            d.dspan
            "type"
            (Printing.ty_to_string ty)
            (Printing.ty_to_string ty2)
      | _ ->
        error_sp
          d.dspan
          "Only global and constant declarations are allowed in constructor \
           bodies")
    body
    labels
;;

(* Only meant to be called on global types. Tells us if it recursively contains
   any global entries *)
let has_global_entry env ty_cid =
  let rec aux ty =
    match TyTQVar.strip_links ty.raw_ty with
    | TGlobal ((cid, _), _) ->
      let _, params = CidMap.find cid env.global_tys in
      List.exists (fun (_, ty) -> aux ty) params || List.is_empty params
      (* Assume all abstract types are global *)
    | _ -> false
  in
  aux (ty (TGlobal ((ty_cid, []), FZero)))
;;

(* Ensure that a record entry has entries for all labels, and no entries for
   nonexistent labels. Also ensure that this type doesn't contain any globals. *)
let verify_record_entries env span ty_cid labels expected_params =
  if List.is_empty expected_params
  then
    error_sp span
    @@ "Type "
    ^ Printing.cid_to_string ty_cid
    ^ " is abstract and cannot be created directly";
  if has_global_entry env ty_cid
  then
    error_sp span
    @@ "Type "
    ^ Printing.cid_to_string ty_cid
    ^ " contains globals, and so cannot be created dynamically";
  let rec aux l1 l2 =
    match l1, l2 with
    | [], [] -> ()
    | hd1 :: _, [] ->
      error_sp span
      @@ Printf.sprintf
           "Label %s does not belong to type %s"
           hd1
           (Printing.cid_to_string ty_cid)
    | [], hd2 :: _ ->
      error_sp span @@ Printf.sprintf "Missing definition for label %s" hd2
    | hd1 :: tl1, hd2 :: tl2 ->
      if hd1 = hd2
      then aux tl1 tl2
      else if List.mem hd1 tl2
      then error_sp span @@ Printf.sprintf "Missing definition for label %s" hd2
      else
        error_sp span
        @@ Printf.sprintf
             "Label %s does not belong to type %s"
             hd1
             (Printing.cid_to_string ty_cid)
  in
  aux
    (List.sort Pervasives.compare labels)
    (List.sort
       Pervasives.compare
       (List.map (fun (l_id, _) -> Id.name l_id) expected_params))
;;
