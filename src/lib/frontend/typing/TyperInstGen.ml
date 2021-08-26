(** Instantiation (for the variable rule). Consistently replace all QVars with
    Unbound TVars. *)
open Syntax

open Collections
open Batteries
open TyperUtil

type inst_maps =
  { size_map : size tqvar IdMap.t ref
  ; effect_map : effect tqvar IdMap.t ref
  ; ty_map : raw_ty tqvar IdMap.t ref
  }

let fresh_maps () =
  { size_map = ref IdMap.empty
  ; effect_map = ref IdMap.empty
  ; ty_map = ref IdMap.empty
  }
;;

let instantiator =
  (* Replace each QVar with a new unbound TVar, doing so consistently if we
   encounter it more than once. *)
  let inst_QVar (type a) (map : a tqvar IdMap.t ref) id =
    match IdMap.find_opt id !map with
    | Some x -> x
    | None ->
      let new_var = fresh_tvar (Id.name id) in
      map := IdMap.add id new_var !map;
      new_var
  in
  object (self)
    inherit [_] s_map

    method! visit_IVar maps tqv =
      match tqv with
      | QVar id -> IVar (inst_QVar maps.size_map id)
      | TVar ({ contents = tvar } as r) ->
        let tvar' = self#visit_tyvar self#visit_size maps tvar in
        (* Make sure _not_ to create a new ref! *)
        r := tvar';
        IVar (TVar r)

    method! visit_FVar maps tqv =
      match tqv with
      | QVar id -> FVar (inst_QVar maps.effect_map id)
      | TVar ({ contents = tvar } as r) ->
        let tvar' = self#visit_tyvar self#visit_effect maps tvar in
        r := tvar';
        FVar (TVar r)

    method! visit_TQVar maps tqv =
      match tqv with
      | QVar id -> TQVar (inst_QVar maps.ty_map id)
      | TVar ({ contents = tvar } as r) ->
        let tvar' = self#visit_tyvar self#visit_raw_ty maps tvar in
        r := tvar';
        TQVar (TVar r)
  end
;;

(** Generalization (for function declarations). Consistently replace all unbound
    TVars (at a higher level than the current one) with QVars *)
let generalizer =
  let generalize_tqvar constr gen tqv =
    match tqv with
    (* If we ever make Lucid functional we'll need to uncomment the when clause here.
       But for now it's fine. We could also take the "let should not be generalized"
       philosophy, and only generalize at the top level, in which case we wouldn't
       need levels in the first place *)
    | TVar ({ contents = Unbound (name, _) } as r) (* when l > !current_level *)
      ->
      let qvar = constr (QVar name) in
      r := Link qvar;
      qvar
    | TVar { contents = Link x } -> gen x
    | _ -> constr tqv
  in
  object (self)
    inherit [_] s_map

    method! visit_IVar _ tqv =
      generalize_tqvar (fun x -> IVar x) (self#visit_size ()) tqv

    method! visit_FVar _ tqv =
      generalize_tqvar (fun x -> FVar x) (self#visit_effect ()) tqv

    method! visit_TQVar _ tqv =
      generalize_tqvar (fun x -> TQVar x) (self#visit_raw_ty ()) tqv
  end
;;

let rec instantiate_prog ds =
  List.map
    (fun d ->
      enter_level ();
      let d =
        match d.d with
        (* No point instantiating if there aren't any things to unify
           the sub-parts with. *)
        | DUserTy _ | DExtern _ | DSize _ | DEvent _ -> d
        (* For modules, don't instantiate the inferface, for the same reason *)
        | DModule (id, intf, ds) ->
          leave_level ();
          (* Hack to avoid going more than one level down *)
          let d' = DModule (id, intf, instantiate_prog ds) in
          enter_level ();
          { d with d = d' }
        | _ -> instantiator#visit_decl (fresh_maps ()) d
      in
      leave_level ();
      d)
    ds
;;
