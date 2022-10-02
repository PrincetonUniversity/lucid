open Batteries
open Syntax
open SyntaxUtils
open Collections

(*let replacer = 
  object (self)
    inherit [_] s_map as super

    method! visit_DSpec env id alphabet size event_spec = 
      super#visit_DSpec env id alphabet size event_spec
      (*let states_id = Id.create "states" in
      let states_size = 1 in
      DGlobal(states_id, 
        ty (TName(Cid.fresh ["states"], [IConst(32)], true)),
        exp(ECall(Compound(Id.create "Array", Id.create "create"), 1)))*)
  end
  ;;*)
let process_prog ds = 
  (*let ds = replacer#visit_decls (ref CidMap.empty) in *)
  List.filter(
    function 
    | {d = DVarRegex _} -> false
    | _ -> true) ds
;; 