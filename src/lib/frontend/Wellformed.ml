open Syntax
open SyntaxUtils
open Batteries
open Collections
open Printing

(* This file contains any well-formedness conditions that aren't guaranteed by
   the syntax of the language *)

(*
   Checks we do before typechecking:
   - No globals declared inside modules
   - No constructors/global declarations for non-global types
   - Only certain types allowed as externs (just ints? I guess bools too?)
   - All events have either one or two handlers declared: one in ingress and one in egress, 
     which must be in the same scope as them.
   - All sizes in symbolic declarations are either concrete or symbolic themselves

   Checks we do during typechecking:
   - No dynamic global creation
   - QVars should only sometimes be allowed in declarations (defined in this file)
   - Constraint specifications only reference global types

  Checks we do after typechecking:
   - Payload restrictions:
     - At most one parameter to each event has (or contains) type Payload.t
     - Payload.parse() is only called inside parsers, and Payload.empty() inside handlers

   Checks we should do at some point (TODO):
   - Check for accidental unification of different user-declared QVars
   - Ensure users only define one level of nested records at a time, or just
     make record handling recursive during typechecking
   - No non-constructor function calls in global variable definitions or constructors
*)

(* Run a bunch of simple checks:
   - Ensure that no globals are declared within a module
   - Ensure that that each global/constructor actually has a global type
   - Ensure that each Extern has int/bool type
*)
let check_decls ds =
  let rec check_decl in_module d =
    match d.d with
    | DGlobal (id, ty, _) ->
      if in_module
      then
        Console.error_position
          d.dspan
          "All globals must be declared at toplevel, not with in a module.";
      if not (is_global ty)
      then
        Console.error_position d.dspan
        @@ Printf.sprintf
             "Global variable %s declared with non-global type %s"
             (id_to_string id)
             (ty_to_string ty)
    | DConstr (id, ty, _, _) ->
      ignore (id, ty)
      (* This restriction isn't actually necessary, I don't think. *)
      (* if not (is_global ty)
             then
               Console.error_position d.dspan
               @@ Printf.sprintf
                    "Constructor %s returns non-global type %s"
                    (id_to_string id)
                    (ty_to_string ty) *)
    | DExtern (_, ty) ->
      (match TyTQVar.strip_links ty.raw_ty with
       | TInt _ | TBool -> ()
       | TFun { arg_tys } ->
         List.iter
           (fun ty ->
             match TyTQVar.strip_links ty.raw_ty with
             | TInt _ | TBool -> ()
             | _ ->
               Console.error_position d.dspan
               @@ Printf.sprintf
                    "Arguments to an extern function must have type int or \
                     bool, not %s"
                    (ty_to_string ty))
           arg_tys
       | _ ->
         Console.error_position d.dspan
         @@ Printf.sprintf
              "Externs must have type int or bool, not %s"
              (ty_to_string ty))
    | DModule (_, _, decls) -> List.iter (check_decl true) decls
    | _ -> ()
  in
  List.iter (check_decl false) ds
;;

(* Next up: Check that symbolic declarations don't use any non-symbolic sizes.
   We could remove this constraint if we wanted by inlining concrete sizes immediately,
   but that's possible tricky if the sizes appear inside of module. *)
let check_symbolics ds =
  let checker =
    object (self)
      inherit [_] s_iter

      method! visit_IUser env cid =
        match cid with
        | Id id when IdSet.mem id !env -> ()
        | _ -> raise @@ Failure (Printing.cid_to_string cid)

      method! visit_decl env d =
        match d.d with
        | DSize (id, None) -> env := IdSet.add id !env
        | DSymbolic (id, ty) ->
          (try self#visit_ty env ty with
           | Failure s ->
             Console.error
             @@ "Unknown or non-symbolic size "
             ^ s
             ^ " appears in type of symbolic "
             ^ Printing.id_to_string id)
        | _ -> ()
    end
  in
  checker#visit_decls (ref IdSet.empty) ds
;;

(* Next up: make sure each event has exactly one handler defined, which must be
   in the same scope. Also ensure that we don't have two events with the same
   name in a given scope.
   To support egress handlers, we check that each event has a handler defined in 
   either ingress or egress, and that every handler has an event.
    *)
let rec match_handlers ?(m_cid = None) (ds : decls) =
  let m_str =
    match m_cid with
    | None -> ""
    | Some cid -> Cid.to_string cid ^ "."
  in
  let aux (events, handlers, egress_handlers) d =
    match d.d with
    | DEvent (id, _, _, _, _) ->
      if IdSet.mem id events
      then
        Console.error_position d.dspan
        @@ "Event "
        ^ m_str
        ^ id_to_string id
        ^ " is declared twice in this scope."
      else IdSet.add id events, handlers, egress_handlers
    | DHandler(id, HEgress, _) ->
      if IdSet.mem id egress_handlers
      then
        Console.error_position d.dspan
        @@ "Egress handler "
        ^ m_str
        ^ id_to_string id
        ^ " is declared twice in this scope."
      else events, handlers, IdSet.add id egress_handlers
    | DHandler (id, _, _) ->
      if IdSet.mem id handlers
      then
        Console.error_position d.dspan
        @@ "Handler "
        ^ m_str
        ^ id_to_string id
        ^ " is declared twice in this scope."
      else events, IdSet.add id handlers, egress_handlers
    | DModule (id, _, ds) ->
      let m_cid =
        match m_cid with
        | None -> Some (Id id)
        | Some cid -> Some (Compound (id, cid))
      in
      match_handlers ~m_cid ds;
      events, handlers, egress_handlers
    | _ -> events, handlers, egress_handlers
  in
  let events, handlers, egress_handlers = List.fold_left aux (IdSet.empty, IdSet.empty, IdSet.empty) ds in
  (* make sure that every event has a handler (in either ingress or egress)
     and every handler has an event *)
  let eminush = IdSet.diff events (IdSet.union handlers egress_handlers) in
  let h_minus_e = IdSet.diff handlers events in
  let h_egr_minus_e = IdSet.diff egress_handlers events in
  match IdSet.choose_opt eminush, IdSet.choose_opt h_minus_e, IdSet.choose_opt h_egr_minus_e with
  | Some id, _, _ ->
    Console.error
    @@ Printf.sprintf
         "Event %s has no corresponding handler definition"
         (m_str ^ id_to_string id)
  | _, Some id, _ ->
    Console.error
    @@ Printf.sprintf
         "Handler %s has no corresponding event definition"
         (m_str ^ id_to_string id)
  | _, _, Some id ->
    Console.error
    @@ Printf.sprintf
         "Egress Handler %s has no corresponding event definition"
         (m_str ^ id_to_string id)
  | None, None, None -> ()
;;

(* Checking restrictions on Payloads
   TODO: Maybe we need to add a restriction that payloads can't be conditionally
   assigned to? Like events, we really want to inline all of them during compilation,
   since they're not "really" first-class values. Maybe even that you can't declare
   local variables or function arguments of type payload.
 *)

(* FIXME: This doesn't account for things happening inside modules *)
let check_payloads ds =
  let v =
    object (self)
      inherit [_] s_iter as super

      (** Since payloads might appear in compount types, we track those that do
          so we can detect multiple payload parameters to events **)
      val mutable saw_payload = false

      val mutable user_payload_tys = CidSet.empty

      method! visit_raw_ty () rty =
        match TyTQVar.strip_links rty with
        | TName (cid, _, _)
          when Cid.equal cid Payloads.t_id || CidSet.mem cid user_payload_tys ->
          saw_payload <- true
        | rty -> super#visit_raw_ty () rty

      method contains_payload ty =
        match TyTQVar.strip_links ty.raw_ty with
        | TName (cid, _, _) ->
          Cid.equal cid Payloads.t_id || CidSet.mem cid user_payload_tys
        | rty ->
          saw_payload <- false;
          self#visit_raw_ty () rty;
          saw_payload

      method! visit_DUserTy () id _ ty =
        if self#contains_payload ty
        then user_payload_tys <- CidSet.add (Cid.id id) user_payload_tys

      (** Ensure that Payload.parse is only called inside parsers, and
          Payload.empty is only called outside parsers *)
      val mutable in_parser = false

      method! visit_exp () e =
        match e.e with
        | ECall (cid, args) ->
          List.iter (self#visit_exp ()) args;
          if in_parser && Cid.equal cid Payloads.payload_empty_cid
          then
            Console.error_position e.espan
            @@ "Payload.empty should only be called outside parsers";
          if (not in_parser) && Cid.equal cid Payloads.payload_parse_cid
          then
            Console.error_position e.espan
            @@ "Payload.parse should only be called inside parsers"
        | _ -> super#visit_exp () e

      method! visit_decl () d =
        match d.d with
        | DEvent (id, _, _, _, params) ->
          (* Ensure the event has at most one payload-type argument *)
          let params_containing_payload =
            List.fold_left
              (fun acc (id, ty) ->
                if self#contains_payload ty then id :: acc else acc)
              []
              params
          in
          if List.length params_containing_payload > 1
          then
            Console.error_position d.dspan
            @@ Printf.sprintf
                 "Multiple parameters to event %s have or contain the type \
                  Payload.t: %s"
                 (Printing.id_to_string id)
                 (Printing.list_to_string Printing.id_to_string
                 @@ List.rev params_containing_payload)
        | DParser _ ->
          in_parser <- true;
          super#visit_decl () d;
          in_parser <- false
        | _ -> super#visit_decl () d
    end
  in
  v#visit_decls () ds
;;

let pre_typing_checks ds =
  check_decls ds;
  match_handlers ds;
  check_symbolics ds;
  check_payloads ds
;;

(*** QVar checking. This is run on each decl after its type is inferred, and makes
     sure all types, sizes, and effects are appropriately determined (i.e. it makes
     sure that polymorphism only appears in places where it is supported). Different
     decls have different restrictions for where QVars can appear *)

(* Does easy checks, where we can classify the allowed QVars by kind *)
let basic_qvar_checker =
  object
    inherit [_] s_iter as super
    val mutable span = Span.default

    method! visit_Unbound _ _ _ _ =
      Console.error_position span
      @@ "Internal error: declaration was improperly generalized."

    method! visit_QVar _ _ _ =
      Console.error_position span
      @@ "Unable to fully determine the type of this expression. If you used \
          auto or any polymorphic variables, try writing out explicitly what \
          they should be."

    method! visit_FVar (check_effects, check_sizes) tqv =
      if check_effects then super#visit_FVar (check_effects, check_sizes) tqv

    method! visit_IVar (check_effects, check_sizes) tqv =
      if check_sizes then super#visit_IVar (check_effects, check_sizes) tqv

    method! visit_ty env ty =
      span <- ty.tspan;
      super#visit_ty env ty

    (* table types are always allowed to have QVars *)
    method! visit_TTable _ _ = ()
    method! visit_exp _ _ = ()

    method! visit_decl env d =
      span <- d.dspan;
      super#visit_decl env d

    method! visit_interface_spec env ispec =
      span <- ispec.ispan;
      super#visit_interface_spec env ispec
  end
;;

(* For checking user type declarations: there's a preset list of size
   variables that are allowed *)
let preset_qvar_checker =
  object
    inherit [_] s_iter as super
    val mutable span = Span.default

    method! visit_Unbound _ _ _ _ =
      Console.error_position span
      @@ "Internal error: declaration was improperly generalized."

    method! visit_QVar _ _ _ =
      Console.error_position span
      @@ "Unable to fully determine the type of this expression. If you used \
          auto or any polymorphic variables, try writing out explicitly what \
          they should be."

    method! visit_FVar _ _ = (* Never check effects *) ()

    method! visit_IVar allowed_sizes tqv =
      if not (List.mem (STQVar.strip_links (IVar tqv)) allowed_sizes)
      then super#visit_IVar allowed_sizes tqv

    method! visit_ty env ty =
      span <- ty.tspan;
      super#visit_ty env ty

    method! visit_exp _ _ = ()

    method! visit_decl env d =
      span <- d.dspan;
      super#visit_decl env d

    method! visit_interface_spec env ispec =
      span <- ispec.ispan;
      super#visit_interface_spec env ispec
  end
;;

(* For checking events and handlers. Polymorphic sizes are allowed only if they
   appear at some point inside an argument of global type, because such arguments
   will eventually be eliminated, and the sizes will then be concretized. *)
let event_qvar_checker =
  object (self)
    inherit [_] s_iter as super
    val mutable span = Span.default

    method! visit_Unbound _ _ _ _ =
      Console.error_position span
      @@ "Internal error: declaration was improperly generalized."

    method! visit_QVar _ _ _ =
      Console.error_position span
      @@ "Unable to fully determine the type of this expression. If you used \
          auto or any polymorphic variables, try writing out explicitly what \
          they should be."

    method! visit_FVar _ _ = (* Never check effects *) ()

    method! visit_IVar (in_gty, sizes) tqv =
      if in_gty
      then sizes := STQVar.strip_links (IVar tqv) :: !sizes
      else if not (List.mem (STQVar.strip_links (IVar tqv)) !sizes)
      then super#visit_IVar (in_gty, sizes) tqv

    method! visit_ty env ty =
      span <- ty.tspan;
      super#visit_ty env ty

    method! visit_params (_, sizes) params =
      List.iter (fun (_, ty) -> self#visit_ty (is_global ty, sizes) ty) params
  end
;;

(* Ensure that the declaration isn't more general than it should be. Different
   decls have different requirements for where they allow QVars to appear. *)
let rec check_qvars d =
  match d.d with
  | DFun _ | DMemop _ | DModuleAlias _ -> (* No restrictions *) ()
  | DAction _ -> ()
  | DGlobal _ ->
    (* None allowed at all *) basic_qvar_checker#visit_decl (true, true) d
  | DSize _ | DSymbolic _ | DConst _ | DExtern _ | DParser _ ->
    (* Only allowed in effect *) basic_qvar_checker#visit_decl (false, true) d
  | DConstr _ ->
    (* Allowed in both sizes and effects *)
    basic_qvar_checker#visit_decl (false, false) d
  | DUserTy (_, sizes, _) ->
    (* Effects always ok, sizes only if they appear in the declared size list *)
    preset_qvar_checker#visit_decl (List.map STQVar.strip_links sizes) d
  | DEvent (_, _, _, _, params) | DHandler (_, _, (params, _)) ->
    (* Effects always ok, sizes only allowed if they appear inside at least one global-type argument *)
    event_qvar_checker#visit_params (false, ref []) params
  | DModule (_, intf, ds) ->
    List.iter check_qvars_intf intf;
    List.iter check_qvars ds

(* These don't actually have to be mutually recursive, I just want to put this
   below check_qvars *)
and check_qvars_intf ispec =
  match ispec.ispec with
  | InFun _ -> ()
  | InSize _ | InVar _ ->
    (* Only allowed in effect *)
    basic_qvar_checker#visit_interface_spec (false, true) ispec
  | InConstr _ ->
    (* Allowed in both sizes and effects *)
    basic_qvar_checker#visit_interface_spec (false, false) ispec
  | InTy (_, sizes, _, _) ->
    (* Like DUserTy *)
    preset_qvar_checker#visit_interface_spec
      (List.map STQVar.strip_links sizes)
      ispec
  | InEvent (_, _, params) ->
    (* Like DEvent *)
    event_qvar_checker#visit_params (false, ref []) params
  | InModule (_, intf) -> List.iter check_qvars_intf intf
;;
