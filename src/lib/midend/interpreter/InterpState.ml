(* Network-wide state in the interpreter. *)
(* 
  The interpreter's state is a mutable array of switches, 
  plus a current time.
  Each switch has a pipeline with mutable local state in it. 
  The switches have queues, but they are currently immutable, 
  and so to update a queue, the switch must call a helper 
  from here (InterpState) that updates the queue.
  We may want to refactor the switch objects to have 
  mutable queues, to simplify the architecture.
  - But the switch will still need to get references to 
    other switches from here.
*)
open Batteries
open InterpSwitch

exception Error of string
let error s = raise (Error s)



