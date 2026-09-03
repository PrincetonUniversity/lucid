(* interpreter-specific extensions to core syntax: 
internal value and event types *)
open CoreSyntax




(* internal event location in simulated network *)
type loc = {
  switch : int option;
  port : int;
}

(* an internal event in the interpreter *)
type ievent = {
  sevent : event_val;
  sloc : loc; (* we always know where an event is located *)
  stime : int;
  squeue_order : int; (* tiebreaker for two events queued at the same time *)
}

let to_internal_event ev loc time = 
  { sevent = ev
  ; sloc = loc
  ; stime = time
  ; squeue_order = 0;
  }

let get_loc (ev : ievent) = 
  ev.sloc
;;
let update_port (ev : ievent) port' = 
  {ev with sloc = {ev.sloc with port = port'}}
;;

let get_port (ev : ievent) =  ev.sloc.port
;;

let delay (ev : ievent) = ev.sevent.edelay
;;

let timestamp ievent = ievent.stime
;;

let set_timestamp ievent ts = 
  {ievent with stime = ts}
;;
let loc (switch, port) = {switch; port}

let internal_event_to_string ievent = 
  CorePrinting.event_to_string ievent.sevent
;;

