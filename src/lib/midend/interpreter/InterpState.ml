(* Interpreter context + helpers for data structure interpretation. *)
open CoreSyntax
open InterpSyntax
open Batteries
module Env = Collections.CidMap
module IntMap = Map.Make (Int)

open InterpSwitch

module State = struct

  (* Maps switch -> port -> (switch * port) *)
  type topology = (int * int) IntMap.t IntMap.t

  let empty_topology num_switches (recirc_ports : int list) =
    List.fold_left2
      (fun acc swid recirc_port -> IntMap.add swid 
        (* add the recirculation port mapping *)
        (IntMap.add recirc_port (swid, recirc_port) IntMap.empty) acc)
      IntMap.empty
      (List.init num_switches (fun n -> n))
      recirc_ports
  ;;

  type config =
    { max_time : int
    ; default_input_gap : int
    ; generate_delay : int
    ; propagate_delay : int
    ; random_seed : int
    ; random_delay_range : int
    ; random_propagate_range : int
    ; drop_chance : int
    }

  type network_state =
    { current_time : int
    ; config : config
    ; event_sorts : event_sort Env.t
    ; event_signatures  : (Cid.t * CoreSyntax.ty list) IntMap.t
    ; handlers : handler Env.t
    ; egress_handlers : handler Env.t
    ; links : topology
    ; switches : switch array
    ; actions : action Env.t
    ; global_names : SyntaxGlobalDirectory.dir
    }
    and handler = network_state InterpSyntax.handler
    and switch = network_state InterpSwitch.state
  
  type global_fun =
    { cid : Cid.t
    ; body : network_state InterpSyntax.code
    ; ty : Syntax.ty
        (* Not CoreSyntax.ty, since this is needed only for type inference *)
    }

  let create config : network_state =
    { current_time = -1
    ; config
    ; event_sorts = Env.empty
    ; event_signatures = IntMap.empty
    ; handlers = Env.empty
    ; egress_handlers = Env.empty
    ; switches = Array.of_list []
    ; links = empty_topology 0 []
    ; actions = Env.empty
    ; global_names = SyntaxGlobalDirectory.empty_dir
    }
  ;;

  (* switch wrappers *)

  let sw nst swid = nst.switches.(swid)

  let mem_env swid cid nst = InterpSwitch.mem_env cid nst.switches.(swid)
  let lookup swid k nst = InterpSwitch.lookup k nst.switches.(swid)


  let sw_update nst swid update_f = 
    nst.switches.(swid) <- update_f nst.switches.(swid)
  ;;

  
  let add_global swid cid v nst = sw_update nst swid (InterpSwitch.add_global cid v)
  ;;

  let add_global_function (g : global_fun) nst =
    Array.modify
      (fun st ->
        if InterpSwitch.mem_env g.cid st
        then
          error ("global variable " ^ Cid.to_string g.cid ^ "  already defined")
        else InterpSwitch.add_global g.cid (F g.body) st)
      nst.switches
  ;;

  (* let lookup_handler cid nst =
    try Some (Env.find cid nst.handlers) with
    | Not_found -> error ("missing handler: " ^ Cid.to_string cid)
  ;; *)

  let lookup_action cid nst =
    try Env.find cid nst.actions with
    | Not_found -> error ("missing action: " ^ Cid.to_string cid)
  ;;

  let add_handler cid lam nst =
    { nst with handlers = Env.add cid lam nst.handlers }
  ;;
  let add_egress_handler cid lam nst =
    { nst with egress_handlers = Env.add cid lam nst.egress_handlers }
  ;;

  let add_action cid action nst =
    { nst with actions = Env.add cid action nst.actions }
  ;;

  let log_exit swid port event nst =
    InterpSwitch.log_exit  port event nst.current_time (sw nst swid)
  ;;

  let log_drop swid event nst =
    InterpSwitch.log_drop  event nst.current_time (sw nst swid)
  ;;

  let update_counter swid event nst =
    let st = nst.switches.(swid) in
    let event_sort = Env.find event.eid nst.event_sorts in
    InterpSwitch.update_counter event_sort st
  ;;

  (* Maps switch * port -> switch * port according to the topology *)
  let lookup_dst nst (sw, p) =
    match IntMap.find_opt sw nst.links with
    | Some map ->
      (match IntMap.find_opt p map with
       | None -> 
        -1, p
       | Some ret -> ret)
    (* the switch ID does not exist in the topology... *)
    | None ->
      error @@ "lookup_dst error -- Invalid switch id " ^ string_of_int sw
  ;;



  let queue_sizes nst = 
    let size_strs = Array.mapi (fun i st -> "switch:"^(string_of_int i)^" "^InterpSwitch.queue_sizes st) nst.switches in
    "------\n"^
    String.concat "\n"  (Array.to_list size_strs)
    ^"\n------"
  ;;
  type push_loc = 
  | Port of int
  | Switch of int
  | PExit of int


  (*** moving events around ***)
  (* this is currently (9/2023) more complicated than it should be, because 
     right now we are trying to preserve the same timing behavior as the 
     interpreter had before there were egress queues. 
     Also, this push_loc stuff needs to be refactored. *)

  (* push a single event to a single switch's queue. Called by interpCore for 
     events that the program itself generates. *)

  (* for now, we just model delay from ingress to ingress, and apply it 
     when the packet leaves the ingress pipeline. *)
  let next_ingress_arrival_time src_id dst_id desired_delay nst = 
    let propagate_delay =
      if src_id = dst_id
      then
        nst.config.propagate_delay
        + Random.int nst.config.random_propagate_range
      else 0
    in
    nst.current_time
      + max desired_delay nst.config.generate_delay
      + propagate_delay
      + Random.int nst.config.random_delay_range
  ;;

  let ingress_receive arrival_time dst_id port (ievent : internal_event_val) nst =
    let st = nst.switches.(dst_id) in
    if Random.int 100 < nst.config.drop_chance
    then (InterpSwitch.log_drop  ievent nst.current_time st)
    else (nst.switches.(dst_id) <- InterpSwitch.push_to_ingress  ievent arrival_time port st;)
  ;;

  (* push a single event to the switch's egress queue. Called by local methods 
     moving events from ingress to egress.*)
  let egress_receive arrival_time swid port (ievent : internal_event_val) nst =
    let st = nst.switches.(swid) in
    nst.switches.(swid) <- InterpSwitch.push_to_egress ievent arrival_time port st;
  ;;
  (* send out of an ingress *)
  let ingress_send src_id out_port ievent nst = 
    (* (match ievent with 
    | IEvent(eval) -> 
      print_endline ("pushing out of ingress: "^(CorePrinting.event_to_string eval));
    | _ -> ());  *)
    match out_port with
      (* "generate directly to a switch" skips egress.
         Its a depreciated feature. *)
      | Switch sw -> 
        let dst_id, dst_port = sw, 0 in
        let timestamp = next_ingress_arrival_time src_id dst_id (InterpSyntax.delay ievent) nst in
        ingress_receive timestamp dst_id dst_port ievent nst
      (* "generate some negative number port" prints an exit events. 
         Its just a hacky way of logging floods. *)
      | PExit port -> 
        (* print_endline ("\tits an exit event with port "^(string_of_int port)); *)
        log_exit src_id (Some port) ievent nst
      (* port locations go to egress. 
         Port locations are the only type of location that should exist once the interp is cleaned up *)
      | Port port -> 
        let dst_id, _ = lookup_dst nst (src_id, port) in 
        let timestamp = next_ingress_arrival_time src_id dst_id (InterpSyntax.delay ievent) nst in
        egress_receive timestamp src_id port ievent nst
  ;;

  (* send out of an egress. Note that we don't call egress_send, because 
     we don't want to compute another arrival time. We model an egress that is 
     instant -- it queues at the next ingress immediately. *)
  let egress_send src_id out_port ievent nst = 
    (* print_endline@@"pushing event out of egress: "
    ^(match ievent with 
    | IEvent(eval) -> 
      CorePrinting.event_to_string eval
      | _ -> "<control or packet event>"); *)

    let dst_id, dst_port = lookup_dst nst (src_id, out_port) in
    (* dst -1 means "somewhere outside of the lucid network" *)
    if (dst_id = -1) then 
      log_exit src_id (Some out_port) ievent nst
    else

    (* print_endline@@"\tsrc_id="
    ^(string_of_int src_id)^"out_port="^(string_of_int out_port)
    ^"dst_id="^(string_of_int dst_id)^" dst_port="^(string_of_int dst_port); *)
    let t = nst.current_time in
    nst.switches.(dst_id) <- InterpSwitch.push_to_ingress  ievent ( t) dst_port (nst.switches.(dst_id));
  ;;


  (*** pushing input events into the network ***)
  (* push an event to one switch *)
  let push_singleloc_event nst internal_event = 
    let switch_id, port = match internal_event.sloc with
      | [loc] -> (
        match loc.switch with
        | Some sw -> sw, loc.port
        | None -> error "push_singleloc_event: no switch id") 
      | _ -> error "push_singleloc_event: multiple locations"
    in
    let time = internal_event.stime in
    let sws = nst.switches in
    sws.(switch_id) <- InterpSwitch.push_to_ingress 
      internal_event.sevent time port (sws.(switch_id))
  ;;
  (* Push an event to a list of entry points *)
  let push_multiloc_event nst internal_event =
    let single_switch_events = 
      List.map (fun loc -> {internal_event with sloc = [loc]}) internal_event.sloc 
    in
    List.iter (push_singleloc_event nst) single_switch_events
  ;;

  (* push a located event to multiple switches where it should appear *)
  let push_multiloc_events nst (located_events : internal_event list)  =
    List.iter (push_multiloc_event nst) located_events 
  ;;


  (*** some other helpers ***)
  let next_event swid nst =
    let st = nst.switches.(swid) in
    match InterpSwitch.next_event ( nst.current_time) st with
      | None -> None
      | Some(st', epgs) -> 
        (* print_endline ("-----");
        print_endline@@"[next_event] got "^(string_of_int (List.length epgs));
        print_endline@@"queues before:\n"^
        (queue_sizes nst); *)
        nst.switches.(swid) <- st';
        (* print_endline ("-----");
        print_endline@@"queues after:\n"^
        (queue_sizes nst);
        print_endline ("-----"); *)
        Some(epgs)
  ;;

  let drain_egress swid nst = 
    let st = nst.switches.(swid) in
    let st', evs = InterpSwitch.drain_egress ( nst.current_time) st in
    nst.switches.(swid) <- st';
    evs
  ;;

  let final_egress_drain swid nst = 
    let st = nst.switches.(swid) in
    let st', evs = InterpSwitch.final_egress_drain st in
    nst.switches.(swid) <- st';
    evs
  ;;

  let next_time nst =
    Array.fold_left
      (fun acc st ->
        match acc, InterpSwitch.next_time st with
        | None, x | x, None -> x
        | Some x, Some y -> Some (min x y))
      None
      nst.switches
  ;;

  let ival_to_string v =
    match v with
    | V v -> CorePrinting.value_to_string v
    | F _ -> "<function>"
    | P pv -> InterpSyntax.payload_to_string pv
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
  let event_queue_to_string = InterpSwitch.event_queue_to_string 
  let exits_to_string = InterpSwitch.exits_to_string 
  let drops_to_string = InterpSwitch.drops_to_string
  let stats_counter_to_string = InterpSwitch.stats_counter_to_string

  let st_to_string = (InterpSwitch.to_string ival_to_string)

  let nst_to_string
    ?(show_vars = false)
    ?(show_pipeline = true)
    ?(show_queue = true)
    ?(show_exits = true)
    nst
    =
    Array.fold_lefti
      (fun acc idx st ->
        Printf.sprintf "%s\nSwitch %d : %s" acc idx
        @@ st_to_string ~show_vars ~show_pipeline ~show_queue ~show_exits st)
      ""
      nst.switches
  ;;
end

let extract_ival iv =InterpSyntax.extract_ival iv
;;
