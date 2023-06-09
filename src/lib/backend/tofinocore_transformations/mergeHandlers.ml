(* 
  merge the handlers of each component.
  The input event type becomes an event union of all the handler input events.
  The output event type becomes and event union of all the handler output events.
  The handler's body is merged by adding a match statement, with a case for 
  each input event's tag in the input event union.
*)

open CoreSyntax
open TofinoCoreNew

let merge_handlers prog : prog = prog


