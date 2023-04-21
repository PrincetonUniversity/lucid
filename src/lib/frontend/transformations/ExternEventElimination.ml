(* Replace exit events with regular events and noop handlers *)

open Syntax

let rec process ds =
  match ds with
  | dec :: ds ->
    (match dec.d with
     | DEvent (evid, EExit, constrs, eparams) ->
       let hdl = decl (DHandler (evid, HData, (eparams, snoop))) in
       let new_ev =
         { dec with d = DEvent (evid, EBackground, constrs, eparams) }
       in
       new_ev :: hdl :: process ds
     | _ -> dec :: process ds)
  | [] -> []
;;
