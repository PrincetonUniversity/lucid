(* The network "fabric" of the interpreter.

   This module moves events between switches and performs the external I/O
   (sockets, stdio exit). It is the single "delivery" phase of the actor-model
   interpreter: a switch [emit]s generated events into its mailbox (a pure,
   local operation -- see InterpSwitch), and the network [drain]s those
   mailboxes here, routing each event to a peer switch's queue or out an
   interface.

   [InterpNetwork] depends on [InterpSwitch], never the reverse -- a switch has
   no knowledge of the network. *)
open CoreSyntax
open InterpSyntax
open InterpJson
open InterpControl
open Batteries
open InterpSocket
open InterpSwitch

(* the network is just the array of switch states. This is the network's view;
   the per-switch core (InterpSwitch / InterpCore) only ever sees a single
   [state], never this. *)
type network_state = state array


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

(* send an event out a port: to a bound socket if there is one, otherwise
   log/print it as an exit from the simulated network. This is the only place
   the interpreter performs external I/O. *)
let emit_or_log_exit port (ievent:ievent) current_time st =
  match IntMap.find_opt port st.sockets with
    | None -> log_exit port ievent current_time st
    | Some(socket) -> InterpSocket.send_event socket ievent.sevent
;;

(* load external input into a switch's queues; returns the new switch state. *)
let load_interp_input st port interp_input : state =
  match interp_input with
  | IEvent({iev; itime}) ->
    let iev = to_internal_event iev {switch = Some st.swid; port} itime in
    enqueue_ingress st iev itime port
  | IControl({ictl; itime}) ->
    enqueue_command st ictl itime
;;

(* an event arrives at a switch's ingress: it may be dropped (the link drop
   model) or enqueued. Returns the new switch state. *)
let ingress_receive st send_time arrival_time port (ievent : ievent) : state =
  if Random.int 100 < st.config.drop_chance
  then (log_drop ievent send_time st; st)
  else enqueue_ingress st ievent arrival_time port
;;

(* calculate when an event arrives at an input queue *)
let calc_arrival_time (src_sw : state) (dst_id: location option) desired_delay =
  let propagate_delay =
    if src_sw.swid = Option.default (-1) dst_id
    then
      src_sw.config.propagate_delay
      + Random.int src_sw.config.random_propagate_range
    else 0
  in
  gtime src_sw
    + max desired_delay src_sw.config.generate_delay
    + propagate_delay
    + Random.int src_sw.config.random_delay_range
;;

(* deliver an event generated in an ingress handler at switch [src], writing
   the result directly into the live network array. *)
let deliver_ingress (net : network_state) src ingress_destination event_val : unit =
  let src_sw = net.(src) in
  match ingress_destination with
    | Switch dst ->
      let send_time = gtime src_sw in
      let arrive_time = calc_arrival_time src_sw (Some dst) event_val.edelay in
      let ievent = to_internal_event event_val {switch = Some dst; port = 0} arrive_time in
      net.(dst) <- ingress_receive net.(dst) send_time arrive_time 0 ievent
    | PFlood port ->
      let send_time = gtime src_sw in
      let ievent = to_internal_event event_val {switch = Some src_sw.swid; port} send_time in
      emit_or_log_exit port ievent send_time src_sw
    | Port port -> (* generate_port goes through this switch's egress for the port *)
      let dst_id_opt = InterpSim.lookup_dst_switch src_sw.config.links (src_sw.swid, port) in
      let timestamp = calc_arrival_time src_sw dst_id_opt event_val.edelay in
      let ievent = to_internal_event event_val {switch = Some src_sw.swid; port} timestamp in
      net.(src) <- enqueue_egress net.(src) ievent timestamp port
;;

(* deliver an event generated in an egress handler at switch [src]. *)
let deliver_egress (net : network_state) src out_port event_val : unit =
  let src_sw = net.(src) in
  let dst_opt = InterpSim.lookup_dst src_sw.config.links (src_sw.swid, out_port) in
  let time = gtime src_sw in
  match dst_opt with
  | None ->
    let ievent = to_internal_event event_val {switch = Some src_sw.swid; port = out_port} time in
    emit_or_log_exit out_port ievent time src_sw
  | Some (dst_id, dst_port) ->
    let ievent = to_internal_event event_val {switch = Some dst_id; port = dst_port} time in
    (* send and arrival times are the same -- 0-latency egress, for now *)
    net.(dst_id) <- ingress_receive net.(dst_id) time time dst_port ievent
;;

(* deliver one mailbox intent from switch [src] into the live network array. *)
let deliver (net : network_state) ~(src : int) (intent : send_intent) : unit =
  match intent with
  | FromIngress (dest, event_val) -> deliver_ingress net src dest event_val
  | FromEgress (out_port, event_val) -> deliver_egress net src out_port event_val
;;

(* deliver everything switch [swid] has queued in its mailbox, then clear it.
   This is the common case: only the switch whose handler just ran has intents,
   so the event loop drains that one switch rather than scanning the array. *)
let drain_switch (net : network_state) (swid : int) : unit =
  let sw = net.(swid) in
  List.iter (deliver net ~src:swid) (List.rev !(sw.outbox));
  sw.outbox := []
;;

(* drain every switch's mailbox (a full sweep over the network). *)
let drain (net : network_state) : unit =
  Array.iteri (fun swid _ -> drain_switch net swid) net
;;
