(* interpreter-specific extensions to core syntax: 
internal value and event types *)
open CoreSyntax
(* open InterpControl *)

(* values used in interpreter contexts. 'nst is network state *)
type 'nst ival =
  | V of value
  | F of 'nst code

and 'nst code = 'nst -> int (* switch *) -> 'nst ival list -> 'nst ival

and memop

and 'nst handler =
'nst -> int (* switch *) -> int (* port *) -> event_val -> unit

let extract_ival iv =
  match iv with
  | V v -> v
  | F _ -> failwith "IVal not a regular value"
;;

let ival_to_string v =
  match v with
  | V v -> CorePrinting.value_to_string v
  | F _ -> "<function>"
;;


(* internal event location in simulated network *)
type loc = {
  switch : int option;
  port : int;
}

(* an internal event in the interpreter *)
type ievent = {
  sevent : event_val;
  sloc : loc; (*we always know where an event is located *)
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


(* an input from the user *)


(* type interp_input =
  | IEvent of {iev : event_val; ilocs : loc list; itime : int}
  | IControl of {ictl : control_val; ilocs : loc list; itime : int}
;;
let ievent iev ilocs itime = IEvent{iev; ilocs; itime}
let icontrol ictl ilocs itime = IControl{ictl; ilocs; itime}
let interp_input_to_time interp_input = 
  match interp_input with
  | IEvent({itime}) -> itime
  | IControl({itime}) -> itime
;;

let input_locs interp_input = 
  match interp_input with
  | IEvent({ilocs}) -> ilocs
  | IControl({ilocs}) -> ilocs
;; *)