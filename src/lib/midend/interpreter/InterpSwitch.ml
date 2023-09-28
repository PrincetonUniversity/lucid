(* models a single switch in the interpreter *)

open CoreSyntax
open InterpSyntax
open InterpControl
open Batteries
module Env = Collections.CidMap
module IntMap = Map.Make (Int)


(* input queue for a single switch *)
module EventQueue = BatHeap.Make (struct
  (* time, event, port *)
  type t = internal_event
  let compare t1 t2 = 
    (* compare stime and use squeue_order as a tiebreaker *)
    if (timestamp t1) = (timestamp t2)
      then Pervasives.compare t1.squeue_order t2.squeue_order
      else        
        Pervasives.compare (timestamp t1) (timestamp t2)
end)

module CommandQueue = BatHeap.Make (struct
  type t = (control_val * int)
  let compare t1 t2 = Pervasives.compare (snd t1) (snd t2)
end)

(* stats counter for a switch *)
type stats_counter =
{ entries_handled : int
; total_handled : int
}

type gress = 
  | Ingress
  | Egress

type 'nst state = 
  { 
    swid : int
  ; global_env : 'nst InterpSyntax.ival Env.t
  ; command_queue : CommandQueue.t
  ; ingress_queue : EventQueue.t
  ; egress_queue : EventQueue.t
  ; pipeline : Pipeline.t
  ; exits : (InterpSyntax.internal_event * int option * int) Queue.t
  ; drops : (internal_event * int) Queue.t
  ; retval : value option ref
  ; counter : stats_counter ref
  ; save_update : ('nst -> 'nst state -> unit)
  }

let empty_counter = { entries_handled = 0; total_handled = 0 }


let create swid update_f =
  { swid = swid
  ; global_env = Env.empty
  ; pipeline = Pipeline.empty ()
  ; command_queue = CommandQueue.empty
  ; ingress_queue = EventQueue.empty
  ; egress_queue = EventQueue.empty
  ; exits = Queue.create ()
  ; drops = Queue.create ()
  ; retval = ref None
  ; counter = ref empty_counter
  ; save_update = update_f
  }
;;

(* let copy_state st =
  { st with
    pipeline = Pipeline.copy st.pipeline
  ; exits = Queue.copy st.exits
  }
;; *)

let mem_env cid state = Env.mem cid state.global_env
let lookup k state = 
  try Env.find k state.global_env with
  | Not_found -> error ("missing variable: " ^ Cid.to_string k)


let add_global cid v st =
  if Env.mem cid st.global_env
  then error ("global variable " ^ Cid.to_string cid ^ "  already defined")
  else
    { st with global_env = Env.add cid v st.global_env }
;;

let log_exit port event current_time st = 
  Queue.push (event, port, current_time) st.exits
;;
let log_drop event current_time st = 
  Queue.push (event, current_time) st.drops
;;

let update_counter event_sort st= 
  let new_counter = match event_sort with
  | EPacket -> 
    {entries_handled = !(st.counter).entries_handled + 1;
     total_handled = !(st.counter).total_handled + 1}
  | _ ->
    {!(st.counter) with total_handled = !(st.counter).total_handled + 1}
  in
  st.counter := new_counter
;;

let n_queued_for_time queued_events stime = 
  List.length (List.filter (fun e -> (timestamp e) = stime) queued_events)
;;

(* push an event to an ingress at a different switch *)
let push_to_ingress nst st internal_event stime sport =
  let squeue_order = n_queued_for_time (EventQueue.elems st.ingress_queue) stime in
  let internal_event = {
    internal_event with   
    sloc = loc (None,sport);
    squeue_order;
    stime
  } in
  let st' = {st with ingress_queue=EventQueue.add internal_event st.ingress_queue} in
  st.save_update nst st' 
;;
(* push an event from an ingress queue to an egress queue. Here, sport is the output port of the switch *)
let push_to_egress nst st internal_event stime sport =
  (* if there's already an event in the queue with the same time, we want to 
     make sure this one gets popped after it. So we increment the queue_spot. *)
  let squeue_order = n_queued_for_time (EventQueue.elems st.egress_queue) stime in
  let internal_event = {internal_event with squeue_order; sloc = loc (None,sport); stime} in
  (* let internal_event = set_timestamp internal_event stime in *)
  let st' = {st with egress_queue=EventQueue.add internal_event st.egress_queue} in
  st.save_update nst st' 
;;

let push_to_commands nst st control_val stime = 
  let st' = {st with command_queue=CommandQueue.add (control_val, stime) st.command_queue} in
  st.save_update nst st'
;;

let next_q_ele (fsize, fmin, fdel, ftime) q cur_time = 
  let sz = fsize q in
  if sz = 0
    then None
    else (
      let ele = fmin q in
      if (ftime ele) > cur_time
        then None
        else (
          Some (fdel q, ele)
        )
    )
;;
let command_queue_fs = (CommandQueue.size, CommandQueue.find_min, CommandQueue.del_min, snd)

let next_command current_time st = 
  match (next_q_ele command_queue_fs st.command_queue current_time) with
  | None -> None
  | Some (q, (control_val, time)) -> Some ({st with command_queue = q;}, control_val, time)
;;

let event_queue_fs = (EventQueue.size, EventQueue.find_min, EventQueue.del_min, timestamp)

let next_ingress_event current_time st = 
  match (next_q_ele event_queue_fs st.ingress_queue current_time) with
  | None -> None
  | Some (q, (iev)) -> Some ({st with ingress_queue = q;}, iev.sevent, get_port iev, timestamp iev)
;;

let next_egress_event current_time st = 
  match (next_q_ele event_queue_fs st.egress_queue current_time) with
  | None -> None
  | Some (q, (iev)) -> Some ({st with egress_queue = q;}, iev.sevent, get_port iev, timestamp iev)



let next_event current_time st = 
  let igr_result, egr_result = next_ingress_event current_time st, next_egress_event current_time st in
  match igr_result, egr_result with
  | Some (st, event, port, _), None -> Some (st, [event, port, Ingress])
  | None, Some (st, event, port, _) -> Some (st, [event, port, Egress])
  | Some (st1, event1, port1, t1), Some (st2, event2, port2, t2) -> (
    if (t1 = t2) then 
      (
        (* taking from both ingress and egress *)
        let st = {st1 with egress_queue = st2.egress_queue} in
        Some (st, [event1, port1, Ingress; event2, port2, Egress])
    )
    else
    if (t1 < t2)
      then Some (st1, [event1, port1, Ingress])
      else Some (st2, [event2, port2, Egress] ))
  | None, None -> None
;;

let next_time st = 
  let next_time_ingress = if (EventQueue.size st.ingress_queue = 0) then None else Some (EventQueue.find_min st.ingress_queue |>timestamp) in
  let next_time_egress  = if (EventQueue.size st.egress_queue = 0) then None else Some (EventQueue.find_min st.egress_queue|> timestamp) in
  let next_time_command = if (CommandQueue.size st.command_queue = 0) then None else Some (CommandQueue.find_min st.command_queue |> snd) in
  let next_times = List.filter_map (fun x -> x) [next_time_ingress; next_time_egress; next_time_command] in
  match next_times with 
  | [] -> None
  | _ -> Some(List.min next_times)
;;

(* we need a few more egress helpers to keep event arrival times the same 
in the new (9/2023) version of the interpreter with the egress queues. *)
let ready_egress_events current_time st = 
  (* pop events out of the queue for current time *)
  let rec _all_egress_events st = 
    match next_egress_event current_time st with
    | Some (st, event, port, _) -> 
      let st', rest = _all_egress_events st in
      st', (event, port, Egress) :: rest
    | None -> st, []
  in
  _all_egress_events st
;;

let ready_control_commands nst st current_time = 
  (* pop events out of the queue for current time *)
  let rec _all_control_commands st = 
    match next_command current_time st with
    | Some (st, event, _) -> 
      let st', rest = _all_control_commands st in
      st', event :: rest
    | None -> st, []
  in
  let st', control_vals = _all_control_commands st in
  st.save_update nst st';
  control_vals
;;


let all_egress_events st = 
  let all_elems = EventQueue.elems st.egress_queue in
  let all_elems = List.map 
    (fun switch_ev -> 
      (switch_ev.sevent, get_port switch_ev, timestamp switch_ev, Egress))
      all_elems
  in
  {st with egress_queue = EventQueue.empty}, all_elems
;;


  



(* pipeline wrappers *)
let get_table_entries stage st=
  Pipeline.get_table_entries ~stage st.pipeline
;;

let install_table_entry  stage entry st =
  Pipeline.install_table_entry ~stage ~entry st.pipeline
;;

let update  stage idx getop setop st  =
  Pipeline.update ~stage ~idx ~getop ~setop st.pipeline
;;

let update_complex stage idx memop st=
  Pipeline.update_complex ~stage ~idx ~memop st.pipeline
;;

(* printers *)
let queue_sizes st = 
  Printf.sprintf "ingress: %d, egress: %d"
  (EventQueue.size st.ingress_queue) (EventQueue.size st.egress_queue)
;;

let stats_counter_to_string counter =
  Printf.sprintf
    "\n packet events handled: %d\n total events handled: %d\n"
    counter.entries_handled
    counter.total_handled
;;

let event_queue_to_string q =
  if EventQueue.size q = 0
  then "[ ]"
  else
    Printf.sprintf "[\n%s  ]"
    @@ (q
       |> EventQueue.to_list (* No BatHeap.fold :( *)
       |> List.fold_left
            (fun acc internal_event ->
              Printf.sprintf
                "%s    %dns: %s at port %d\n"
                acc
                (timestamp internal_event)
                (CorePrinting.event_to_string internal_event.sevent)
                (get_port internal_event))
            "")
;;

let exits_to_string s =
  if Queue.is_empty s
  then "[ ]"
  else
    Printf.sprintf "[\n%s  ]"
    @@ Queue.fold
         (fun acc (event, port, time) ->
           Printf.sprintf
             "%s    %s at port %d, t=%d\n"
             acc
             (CorePrinting.event_to_string  event.sevent)
             (Option.default (-1) port)
             time)
         ""
         s
;;

let drops_to_string s =
  if Queue.is_empty s
  then "[ ]"
  else
    Printf.sprintf "[\n%s  ]"
    @@ Queue.fold
         (fun acc (event, time) ->
           Printf.sprintf
             "%s    %s, t=%d\n"
             acc
             (CorePrinting.event_to_string event.sevent)
             time)
         ""
         s
;;

let env_to_string ival_to_string env =
  if Env.is_empty env
  then "{ }"
  else
    Printf.sprintf "{\n%s  }"
    @@ Env.fold
         (fun id v acc ->
           let kstr = Cid.to_string id in
           acc ^ "    " ^ kstr ^ " = " ^ ival_to_string v ^ ";\n")
         env
         ""
;;

let to_string
  ival_to_string
?(show_vars = false)
?(show_pipeline = true)
?(show_queue = true)
?(show_exits = true)
st
=
let show b title str =
  if not b
  then ""
  else
    Printf.sprintf
      "\n %s : %s%s\n"
      title
      (String.make (8 - String.length title) ' ')
      str
in
let vars = show show_vars "Env" @@ env_to_string ival_to_string st.global_env in
let pipeline =
  show show_pipeline "Pipeline" @@ Pipeline.to_string ~pad:"  " st.pipeline
in
let queue =
  show show_queue "Events" @@ event_queue_to_string st.ingress_queue
in
let exits = show show_exits "Exits" @@ exits_to_string st.exits in
let drops = show show_exits "Drops" @@ drops_to_string st.drops in
let stats = stats_counter_to_string !(st.counter) in
"{\n" ^ vars ^ pipeline ^ queue ^ exits ^ drops ^ stats ^ "\n}"
;;