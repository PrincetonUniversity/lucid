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
   - All events have exactly one handler declared, which must be in the same scope as them.

   Checks we do during typechecking:
   - No dynamic global creation
   - QVars should only sometimes be allowed in declarations (defined in this file) -- TODO
   - Constraint specifications only reference global types

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
      if not (is_global ty)
      then
        Console.error_position d.dspan
        @@ Printf.sprintf
             "Constructor %s returns non-global type %s"
             (id_to_string id)
             (ty_to_string ty)
    | DExtern (_, ty) ->
      begin
        match TyTQVar.strip_links ty.raw_ty with
        | TInt _ | TBool -> ()
        | _ ->
          Console.error_position d.dspan
          @@ Printf.sprintf
               "Externs must have type int or bool, not %s"
               (ty_to_string ty)
      end
    | DModule (_, _, decls) -> List.iter (check_decl true) decls
    | _ -> ()
  in
  List.iter (check_decl false) ds
;;

(* Next up: make sure each event has exactly one handler defined, which must be
   in the same scope. Also ensure that we don't have two events with the same
   name in a given scope *)

let rec match_handlers ?(m_cid = None) (ds : decls) =
  let m_str =
    match m_cid with
    | None -> ""
    | Some cid -> Cid.to_string cid ^ "."
  in
  let aux (events, handlers) d =
    match d.d with
    | DEvent (id, sort, _, _) ->
      if IdSet.mem id events
      then
        Console.error_position d.dspan
        @@ "Event "
        ^ m_str
        ^ id_to_string id
        ^ " is declared twice in this scope."
      else if sort = EExit
      then events, handlers
      else IdSet.add id events, handlers
    | DHandler (id, _) ->
      if IdSet.mem id handlers
      then
        Console.error_position d.dspan
        @@ "Handler "
        ^ m_str
        ^ id_to_string id
        ^ " is declared twice in this scope."
      else events, IdSet.add id handlers
    | DModule (id, _, ds) ->
      let m_cid =
        match m_cid with
        | None -> Some (Id id)
        | Some cid -> Some (Compound (id, cid))
      in
      match_handlers ~m_cid ds;
      events, handlers
    | _ -> events, handlers
  in
  let events, handlers = List.fold_left aux (IdSet.empty, IdSet.empty) ds in
  let eminush = IdSet.diff events handlers in
  let hminuse = IdSet.diff handlers events in
  match IdSet.choose_opt eminush, IdSet.choose_opt hminuse with
  | Some id, _ ->
    Console.error
    @@ Printf.sprintf
         "Event %s has no corresponding handler definition"
         (m_str ^ id_to_string id)
  | _, Some id ->
    Console.error
    @@ Printf.sprintf
         "Handler %s has no corresponding event definition"
         (m_str ^ id_to_string id)
  | None, None -> ()
;;

let pre_typing_checks ds =
  check_decls ds;
  match_handlers ds
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
  | DFun _ | DMemop _ -> (* No restrictions *) ()
  | DGlobal _ ->
    (* None allowed at all *) basic_qvar_checker#visit_decl (true, true) d
  | DSize _ | DConst _ | DGroup _ | DExtern _ ->
    (* Only allowed in effect *) basic_qvar_checker#visit_decl (false, true) d
  | DConstr _ ->
    (* Allowed in both sizes and effects *)
    basic_qvar_checker#visit_decl (false, false) d
  | DUserTy (_, sizes, _) ->
    (* Effects always ok, sizes only if they appear in the declared size list *)
    preset_qvar_checker#visit_decl (List.map STQVar.strip_links sizes) d
  | DEvent (_, _, _, params) | DHandler (_, (params, _)) ->
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
