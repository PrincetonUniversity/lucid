(* ensure that every event defined in the ingress component has a 
   handler in the egress component. For each event that does not, 
   define a default egress handler that generates the same event. *)

open CoreSyntax
open TofinoCoreNew
open BackendLogging
open Collections

let add_egress_handler prog : prog =
  (* get the ingress events *)
  let ingress_events = List.filter_map 
      (fun tdecl -> match tdecl.td with
      | TDEvent (event) -> Some(event)
      | _ -> None)
      (find_component_by_id prog (Id.create "ingress")).comp_decls
  in
  (* add all the ingress events to egress, if they aren't already there. 
     Then ensure that there is an egress handler for every event and add one if not.  *)
  prog

;;