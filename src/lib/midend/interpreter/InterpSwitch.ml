(* Per-switch state in the interpreter, and all operations that touch a single
   switch in isolation: its queues, globals, pipeline, mailbox, and printing.

   A switch has no knowledge of how events move between switches -- that is the
   job of [InterpNetwork], which depends on this module (never the reverse). *)
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
  | PFlood of int

(* An event a switch wants to send, recorded in its mailbox/outbox. The
   network drains these and performs delivery -- this separates "generating a
   message" (a pure switch operation) from "moving a message" (the fabric's
   job). The two variants mirror the two send paths: a generate in an ingress
   handler vs. an egress handler. *)
type send_intent =
  | FromIngress of ingress_destination * event_val
  | FromEgress  of int (* out_port *) * event_val


type state =
  {
    swid : int
  ; config : InterpSim.simulation_config
  ; global_env : ival Env.t
  ; command_queue : CommandQueue.t
  ; ingress_queue : EventQueue.t
  ; egress_queue : EventQueue.t
  ; pipeline : Pipeline.t
  ; exits : (ievent * int option * int) Queue.t
  ; drops : (ievent * int) Queue.t
  ; retval : value option ref
  ; counter : stats_counter ref
  ; sockets : socket_map
  ; hdlrs :  handler Env.t
  ; egress_hdlrs : handler Env.t
  ; event_sorts : event_sort Env.t
  ; event_signatures  : (Cid.t * CoreSyntax.ty list) InterpSim.IntMap.t
  ; global_names : SyntaxGlobalDirectory.dir
  ; outbox : send_intent list ref (* the mailbox: events generated, not yet delivered *)
  ; global_time : int ref (* shared global time *)
  }

(* values used in interpreter contexts. *)
(* a handler runs a switch's event code: its effects are local to that switch
   (outgoing events go to the mailbox, the pipeline mutates in place), so it
   takes just the switch state -- not the network. *)
and  handler = state -> int (* port *) -> event_val -> unit

(* code inside the program may mutate switch state (first arg) *)
and code = state -> ival list -> ival

and ival =
  | V of value
  | F of (cid option *  code)

let f (cid: cid) (code: code) = F(Some(cid), code)
let anonf (code: code) = F(None, code)

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


type global_fun =
  { cid : Cid.t
  ; body : code
  ; ty : Syntax.ty
  }

let gfun_cid (gf : global_fun) : Cid.t =
  gf.cid
;;

let empty_counter = { entries_handled = 0; total_handled = 0 }
;;

let create ?(softswitch_mode=false) ?(interfaces=None) start_time_ref event_sorts event_signatures config swid =
  (* in softswitch mode, we take the socket config from the global SwitchConfig map *)
  let sockets =
    if softswitch_mode then
      List.fold_left
        (fun ifmap (intf:SwitchConfig.interface) ->
          let socket = InterpSocket.create intf.switch intf.port intf.interface in
          IntMap.add intf.port socket ifmap)
        IntMap.empty
        SwitchConfig.cfg.interface
    else (
      (* in simulation mode, create the sockets from the interfaces map *)
      let my_intfs = match interfaces with
        | Some(intfs) -> List.nth intfs swid |> snd
        | None -> []
      in
      List.fold_left
        (fun ifmap (port_id, interface_name) ->
          let socket = InterpSocket.create swid port_id interface_name in
          IntMap.add port_id socket ifmap)
        IntMap.empty
        my_intfs
    )
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
  ; outbox = ref []
  ; global_time = start_time_ref (* shared global time *)
  }
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

(* mailbox: record an event the switch wants to send. The network performs the
   actual delivery later, during its drain phase. The outbox is a ref so this
   fits the interpreter's in-place style (like retval/counter) and needs no
   state threading through interp_statement. *)
let emit (st : state) (intent : send_intent) : unit =
  st.outbox := intent :: !(st.outbox)
;;

(* how many events are already queued at [stime] -- a stable tiebreaker for
   events that arrive at the same time. *)
let n_queued_for_time queued_events stime =
  List.length (List.filter (fun e -> (timestamp e) = stime) queued_events)
;;

(* enqueue an event into this switch's ingress queue (pure). *)
let enqueue_ingress st iev stime sport : state =
  let squeue_order = n_queued_for_time (EventQueue.elems st.ingress_queue) stime in
  let iev = { iev with sloc = loc (None, sport); squeue_order; stime } in
  { st with ingress_queue = EventQueue.add iev st.ingress_queue }
;;

(* enqueue an event into this switch's egress queue (pure). *)
let enqueue_egress st iev stime sport : state =
  let squeue_order = n_queued_for_time (EventQueue.elems st.egress_queue) stime in
  let iev = { iev with squeue_order; sloc = loc (None, sport); stime } in
  { st with egress_queue = EventQueue.add iev st.egress_queue }
;;

(* enqueue a control command into this switch's command queue (pure). *)
let enqueue_command st control_val stime : state =
  { st with command_queue = CommandQueue.add (control_val, stime) st.command_queue }
;;

(* record a dropped event (mutates the shared drops queue). *)
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

let gtime self =
  !(self.global_time)
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

(* drain the control-command queue for [current_time]; returns the updated
   switch state and the commands (the caller writes the state back). *)
let ready_control_commands st current_time =
  let rec _all_control_commands st =
    match next_command current_time st with
    | Some (st, event, _) ->
      let st', rest = _all_control_commands st in
      st', event :: rest
    | None -> st, []
  in
  _all_control_commands st
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
