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
    | TVar { contents = Unbound (name, l) } when l > !current_level ->
      constr (QVar name)
    | TVar { contents = Link x } -> gen x
    | QVar id ->
      Printf.printf
        "Unexpected: qvar %s appears during generalization\n"
        (Printing.id_to_string id);
      constr tqv
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
