(* Per-switch state in the interpreter. *)

open CoreSyntax
open InterpSyntax
open InterpJson
open InterpControl
open Batteries
module Env = Collections.CidMap
open InterpSocket

module IntMap = InterpSim.IntMap
(* maps port numbers to socket datatypes *)
type socket_map = InterpSocket.t IntMap.t

(* input queue for a single switch *)
module EventQueue = BatHeap.Make (struct
  (* time, event, port *)
  type t = ievent
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

(* topology-related datatypes that should be combined 
into a proper "location" type *)
type gress = 
  | Ingress
  | Egress

type ingress_destination = 
  | Port of int
  | Switch of int
  | PExit of int
  

and 'nst state = 
  { 
    swid : int
  ; config : InterpSim.simulation_config
  ; global_env : 'nst ival Env.t
  ; command_queue : CommandQueue.t
  ; ingress_queue : EventQueue.t
  ; egress_queue : EventQueue.t
  ; pipeline : Pipeline.t                         
  ; exits : (ievent * int option * int) Queue.t
  ; drops : (ievent * int) Queue.t
  ; retval : value option ref
  ; counter : stats_counter ref
  ; sockets : socket_map
  ; hdlrs : 'nst handler Env.t
  ; egress_hdlrs : 'nst handler Env.t
  ; event_sorts : event_sort Env.t
  ; event_signatures  : (Cid.t * CoreSyntax.ty list) InterpSim.IntMap.t
  ; global_names : SyntaxGlobalDirectory.dir
  ; sws : 'nst state Array.t ref (* a reference to the array of switches in the nw *)
  ; global_time : int ref (* shared global time *)
  }

and 'nst handler =
  'nst -> int (* switch *) -> int (* port *) -> event_val -> unit


let empty_counter = { entries_handled = 0; total_handled = 0 }
;;

let lookup_switch self swid = 
  !(self.sws).(swid)
;;
let save_update self sw = 
  !(self.sws).(sw.swid) <- sw 
;;


let create ?(with_sockets=false) start_time_ref event_sorts event_signatures config swid =
  (* construct socket map *)
  let sockets = if with_sockets then (
    List.fold_left 
      (fun ifmap (intf:SoftSwitchConfig.interface) -> 
        let socket = InterpSocket.create intf.switch intf.port intf.interface in
        IntMap.add intf.port socket ifmap)
      IntMap.empty
      SoftSwitchConfig.cfg.interface  
    )
    else IntMap.empty
  in
  { swid
  ; config
  ; global_env = Env.empty
  ; pipeline = Pipeline.empty ()
  ; command_queue = CommandQueue.empty
  ; ingress_queue = EventQueue.empty
  ; egress_queue = EventQueue.empty
  ; exits = Queue.create ()
  ; drops = Queue.create ()
  ; retval = ref None
  ; counter = ref empty_counter
  ; sockets
  ; hdlrs = Env.empty
  ; egress_hdlrs = Env.empty
  ; event_sorts
  ; event_signatures
  ; global_names = SyntaxGlobalDirectory.empty_dir
  ; sws = ref (Array.of_list [])
  ; global_time = start_time_ref (* shared global time *)
  }
;;



(* set the switch array reference *)
let set_sws (self : 'nst state) sws = 
  {self with sws = ref sws;}
;;

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

let get_sockets st : InterpSocket.t list = IntMap.bindings st.sockets |> List.map snd
;;


(* generate an event to stdio or the exit log *)
let log_exit port (ievent:ievent) current_time st = 
  if InterpConfig.cfg.interactive
    then (
      InterpJson.event_exit_to_json 
        st.swid 
        (Some(port))
        ievent.sevent 
        current_time
      |> print_endline)
    else Queue.push (ievent, Some(port), current_time) st.exits
;;

let emit_or_log_exit port (ievent:ievent) current_time st = 
  match IntMap.find_opt port st.sockets with
    | None -> log_exit port ievent current_time st
    | Some(socket) -> InterpSocket.send_event socket ievent.sevent
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
  save_update st' st' 
;;
(* push an event from an ingress queue to an egress queue. Here, sport is the output port of the switch *)
let push_to_egress nst st internal_event stime sport =
  (* if there's already an event in the queue with the same time, we want to 
     make sure this one gets popped after it. So we increment the queue_spot. *)
  let squeue_order = n_queued_for_time (EventQueue.elems st.egress_queue) stime in
  let internal_event = {internal_event with squeue_order; sloc = loc (None,sport); stime} in
  (* let internal_event = set_timestamp internal_event stime in *)
  let st' = {st with egress_queue=EventQueue.add internal_event st.egress_queue} in
  save_update st' st' 
;;

let push_to_commands nst st control_val stime = 
  let st' = {st with command_queue=CommandQueue.add (control_val, stime) st.command_queue} in
  save_update st' st'
;;

(** input loading **)
let load_interp_input nst st port interp_input = 
  match interp_input with
  | IEvent({iev; itime}) -> 
    let internal_event = to_internal_event iev {switch=Some st.swid; port} itime in
    push_to_ingress nst st internal_event itime port
  | IControl({ictl; itime}) -> 
    push_to_commands nst st ictl itime
;;


let gtime self = 
  !(self.global_time)
;;

(* event movement functions *)

let ingress_receive nst st send_time arrival_time port (ievent : ievent)  =
if Random.int 100 < st.config.drop_chance
  then (log_drop ievent send_time st)
  else (push_to_ingress nst st ievent arrival_time port)     
;;

let egress_receive nst st arrival_time port ievent = 
  push_to_egress nst st ievent arrival_time port
;;

(* calculate when an event arrives at an input queue *)
let calc_arrival_time nst (src_sw : 'nst state) dst_id desired_delay = 
  let propagate_delay =
    if src_sw.swid = dst_id
    then
      src_sw.config.propagate_delay
      + Random.int src_sw.config.random_propagate_range
    else 0
  in
  gtime src_sw 
  (* src_sw.utils.get_time nst *)
    + max desired_delay src_sw.config.generate_delay
    + propagate_delay
    + Random.int src_sw.config.random_delay_range
;;

(* val ingress_send : 'nst -> 'nst state -> ingress_destination -> event_val -> unit *)
let ingress_send (nst : 'nst) (src_sw : 'nst state) ingress_destination event_val = 
  match ingress_destination with
    | Switch sw -> 
      let dst_sw = lookup_switch src_sw sw in
      let send_time = gtime src_sw in
      (* let send_time = src_sw.utils.get_time nst in *)

      let arrive_time = calc_arrival_time nst src_sw dst_sw.swid event_val.edelay in
      let ievent = to_internal_event event_val {switch = Some dst_sw.swid; port = 0} arrive_time in
      ingress_receive nst dst_sw send_time arrive_time 0 ievent
    | PExit port -> 
      let send_time = gtime src_sw in
      (* let send_time = src_sw.utils.get_time nst in *)
      let ievent = to_internal_event event_val {switch = Some src_sw.swid; port = port} send_time in
      emit_or_log_exit port ievent send_time src_sw
    | Port port -> (* NOTE: generate_port goes through an egress for the port *)
      let dst_id, _ = InterpSim.lookup_dst src_sw.config.links (src_sw.swid, port) in 
      let timestamp = calc_arrival_time nst src_sw dst_id (event_val.edelay) in
      let ievent = to_internal_event event_val {switch = Some src_sw.swid; port = port} timestamp in
      egress_receive nst src_sw timestamp port ievent
;;

let egress_send nst src_sw out_port event_val = 
  let dst_id, dst_port = InterpSim.lookup_dst src_sw.config.links (src_sw.swid, out_port) in
  let time = gtime src_sw in
  (* let time = src_sw.utils.get_time nst in *)

  (* dst -1 means "somewhere outside of the lucid network" *)
  if (dst_id = -1)
    then (
      let ievent = to_internal_event event_val {switch = Some src_sw.swid; port = out_port} time in
      emit_or_log_exit out_port ievent time src_sw)
    else (
      let dst_sw = lookup_switch src_sw dst_id in
      let ievent = to_internal_event event_val {switch = Some dst_id; port = dst_port} time in        
      (* note that send and arrival times are currently the same -- we model 0-latency egress, for now *)
      ingress_receive nst dst_sw time time dst_port ievent)
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
  save_update st' st';
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

let env_to_string env =
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
let vars = show show_vars "Env" @@ env_to_string st.global_env in
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