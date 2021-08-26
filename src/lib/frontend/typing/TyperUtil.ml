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

(* Not really kinds, but rather everything we have a different map for in the
   environment *)
type kind =
  | KSize
  | KConst
  | KHandler
  | KConstr
  | KUserTy

module KindSet = Set.Make (struct
  type t = kind * cid

  let compare = Pervasives.compare
end)

type env =
  { (* Maps for all the different kinds of variables we have in a program *)
    sizes : CidSet.t
  ; locals : ty CidMap.t
  ; consts : ty CidMap.t (* Constant variables *)
  ; (* Maps vector index vars to their max size and an alpha-renamed cid *)
    indices : (id * size) IdMap.t
  ; (* Maps for special "variables" like handlers and constructors *)
    handlers : (constr list * params) CidMap.t
  ; constructors : func_ty CidMap.t
  ; user_tys : (ty * sizes) CidMap.t
  ; record_labels : ty StringMap.t (* Maps labels to the gty with that label *)
  ; (* Track the things we've defined in the current module body *)
    module_defs : KindSet.t
  ; (* Information we use while typechecking function/handler bodies *)
    current_effect : effect
  ; constraints : constr list
  ; ret_ty : ty option (* Some iff we're in a function body *)
  ; ret_effects : effect list (* All the effects we `return`ed at *)
  ; returned : bool (* If we ran into a return statement *)
  ; in_global_def : bool (* True if we're able to use constructors *)
  }

let empty_env =
  { sizes = CidSet.empty
  ; handlers = CidMap.empty
  ; locals = CidMap.empty
  ; consts = CidMap.empty
  ; indices = IdMap.empty
  ; current_effect = FZero
  ; constraints = []
  ; ret_ty = None
  ; ret_effects = []
  ; returned = false
  ; constructors = CidMap.empty
  ; user_tys = CidMap.empty
  ; record_labels = StringMap.empty
  ; module_defs = KindSet.empty
  ; in_global_def = false
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
  { empty_env with consts; constructors }
;;

let lookup_var span env cid =
  match CidMap.find_opt cid env.locals with
  | Some t -> t
  | None ->
    (match CidMap.find_opt cid env.consts with
    | Some t -> t
    | None ->
      (match CidMap.find_opt cid env.constructors with
      | Some t ->
        if env.in_global_def
        then ty (TFun t)
        else
          error_sp
            span
            "Cannot call constructor except in global definitions or other \
             constructors"
      | None -> error_sp span ("Unbound variable " ^ Cid.to_string cid)))
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
  ty
  @@ TFun
       { arg_tys = List.map snd params
       ; ret_ty = ty (TEvent false) (* Events are singlecast by default *)
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
  let lookup_effect env cid =
    if Cid.names cid = ["start"]
    then start_eff
    else (
      let ty =
        match CidMap.find_opt cid env.locals with
        | Some ty -> ty
        | None ->
          (match CidMap.find_opt cid env.consts with
          | Some ty -> ty
          | _ -> error_sp sp @@ "Unbound variable " ^ Printing.cid_to_string cid)
      in
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
      (fun env (id, ty) ->
        { env with locals = CidMap.add (Id id) ty env.locals })
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

(* Replace all user types with their definition -- even abstract ones.
   For use when validating module interfaces -- everywhere else, this is already
   done by ReplaceUserTys.ml. Expects the interface to be well-formed. *)
(* FIXME: This currently doesn't fully replace the effect. It should work fine
   as long as the effect is a QVar, though. *)
let subst_user_tys =
  object
    inherit [_] s_map as super

    method! visit_ty env ty =
      match ty.raw_ty with
      | TName (cid, sizes, _) ->
        begin
          match CidMap.find_opt cid env.user_tys with
          | None -> (* Must be a builtin *) ty
          | Some (ty', sizes') ->
            let replaced_ty =
              ReplaceUserTys.subst_sizes
                ty.tspan
                cid
                ty'.raw_ty
                (ReplaceUserTys.extract_ids ty.tspan sizes')
                sizes
            in
            { ty with raw_ty = replaced_ty; teffect = ty'.teffect }
        end
      | _ -> super#visit_ty env ty
  end
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
      | TVar { contents = Unbound (id, _) } when Id.equal id target -> eff
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
