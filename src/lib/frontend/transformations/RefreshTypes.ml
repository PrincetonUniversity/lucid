(* Reset effect annotations on types inside handler and function bodies.
   This is a temporary patch / hack for type checking to work in 
   certain phases of the frontend (from TableInlining to MonomorphicEventArgs),
   After a typing pass, types in handler bodies carry resolved effects with
   specific index variable IDs. These conflict with fresh variables created
   by a subsequent typing pass. This pass replaces those teffect fields with
   fresh effect variables, allowing a subsequent type checker to re-derive
   effects from program structure.

   Top-level declarations (globals, events, user types) are left untouched
   since their effects carry meaningful semantic information (e.g., global
   ordering). *)
open Syntax
open TyperUtil

let effect_refresher =
  object
    inherit [_] s_map as super

    method! visit_ty () ty =
      { (super#visit_ty () ty) with teffect = fresh_effect () }
  end
;;

let refresh_prog ds =
  List.map
    (fun d ->
      match d.d with
      | DHandler (id, sort, body) ->
        let body' = effect_refresher#visit_body () body in
        { d with d = DHandler (id, sort, body') }
      | DFun (id, rty, cs, body) ->
        let body' = effect_refresher#visit_body () body in
        { d with d = DFun (id, rty, cs, body') }
      | _ -> d)
    ds
;;