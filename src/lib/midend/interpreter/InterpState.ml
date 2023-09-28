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
  let ports_to_neighbors topology sw = 
    match IntMap.find_opt sw topology with
    | Some map ->       
      IntMap.fold (fun port (dst_sw, _) acc -> (port, dst_sw)::acc) map []
    | None -> []
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

  let pipe nst swid = nst.switches.(swid).pipeline

  let mem_env swid cid nst = InterpSwitch.mem_env cid nst.switches.(swid)
  let lookup swid k nst = InterpSwitch.lookup k nst.switches.(swid)


  (* update the state of a switch. Pass this callback to 
     a switch so that it can update itself without having to know 
     about the network. *)
  let save_update nst sw = nst.switches.(sw.swid) <- sw;    
  ;;
  let add_global swid cid v nst = 
    save_update nst (InterpSwitch.add_global cid v nst.switches.(swid))
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

  (*** moving events around ***)
  let to_internal_event ev loc time = 
    { sevent = ev
    ; sloc = loc
    ; stime = time
    ; squeue_order = 0;
    }


  (* calculate when an event should arrive at the next switch's ingress. 
     The calculation should be improved to add ingres -> egress queue time and then 
      propagation time in two separate steps. Also, the names are confusing and why 
     would propagation delay only apply to recirculating events? *)
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

  let ingress_receive arrival_time swid port (ievent : internal_event) nst =
    let st = nst.switches.(swid) in
    if Random.int 100 < nst.config.drop_chance
    then (InterpSwitch.log_drop ievent nst.current_time st)
    else (InterpSwitch.push_to_ingress nst st ievent arrival_time port)     
  ;;

  (* push a single event to the switch's egress queue. Called by local methods 
     moving events from ingress to egress.*)
  let egress_receive arrival_time swid port (ievent : internal_event) nst =
    let st = nst.switches.(swid) in
    InterpSwitch.push_to_egress nst st ievent arrival_time port;
  ;;
  (* send out of an ingress *)
  type ingress_destination = 
  | Port of int
  | Switch of int
  | PExit of int
  let ingress_send src_id ingress_destination event_val nst = 
    match ingress_destination with
      | Switch sw -> 
        let timestamp = next_ingress_arrival_time src_id sw (event_val.edelay) nst in
        let ievent = to_internal_event event_val {switch = Some sw; port = 0} timestamp in
        ingress_receive timestamp sw 0 ievent nst
      | PExit port -> 
        let ievent = to_internal_event event_val {switch = Some src_id; port = port} nst.current_time in
        log_exit src_id (Some port) ievent nst
      | Port port -> 
        let dst_id, _ = lookup_dst nst (src_id, port) in 
        let timestamp = next_ingress_arrival_time src_id dst_id (event_val.edelay) nst in
        let ievent = to_internal_event event_val {switch = Some src_id; port = port} timestamp in
        egress_receive timestamp src_id port ievent nst
  ;;

  (* send out of an egress. We model an egress that is 
     instant -- it queues at the next ingress immediately. *)
  let egress_send src_id out_port event_val nst = 
    let dst_id, dst_port = lookup_dst nst (src_id, out_port) in
    (* dst -1 means "somewhere outside of the lucid network" *)
    if (dst_id = -1) 
      then 
        let ievent = to_internal_event event_val {switch = Some src_id; port = out_port} nst.current_time in
        log_exit src_id (Some out_port) ievent nst
      else 
        let ievent = to_internal_event event_val {switch = Some dst_id; port = dst_port} nst.current_time in        
        ingress_receive nst.current_time dst_id dst_port ievent nst
  ;;


  (*** pushing input events into the network ***)

  (* load an interpreter input into the network environment *)
  let load_interp_input nst interp_input = 
    let sws = nst.switches in
    match interp_input with 
    | IEvent({iev; ilocs; itime}) -> 
      List.iter 
        (fun loc -> 
          let switch = match loc.switch with 
            | None -> error "input event not associated with a switch"
            | Some(switch) -> switch
          in
          let internal_event = to_internal_event iev loc itime in
          (InterpSwitch.push_to_ingress nst sws.(switch) internal_event itime loc.port)
        )
        ilocs      
    | IControl({ictl; ilocs; itime}) -> 
      List.iter 
        (fun loc -> 
          let switch = match loc.switch with 
            | None -> error "input event not associated with a switch"
            | Some(switch) -> switch
          in
          (InterpSwitch.push_to_commands nst sws.(switch) ictl itime)
        )
        ilocs
  ;;

  let load_interp_inputs nst interp_inputs = 
    List.iter (load_interp_input nst) interp_inputs

  (*** some other helpers ***)
  let next_ready_event swid nst =
    let st = nst.switches.(swid) in
    match InterpSwitch.next_event ( nst.current_time) st with
      | None -> None
      | Some(st', epgs) -> 
        (* print_endline ("-----");
        print_endline@@"[next_event] got "^(string_of_int (List.length epgs));
        print_endline@@"queues before:\n"^
        (queue_sizes nst); *)
        save_update nst st';
        (* print_endline ("-----");
        print_endline@@"queues after:\n"^
        (queue_sizes nst);
        print_endline ("-----"); *)
        Some(epgs)
  ;;

  let ready_egress_events swid nst = 
    let st = nst.switches.(swid) in
    let st', evs = InterpSwitch.ready_egress_events nst.current_time st in
    save_update nst st';
    evs
  ;;
  let ready_control_commands swid nst = 
    let st = nst.switches.(swid) in
    InterpSwitch.ready_control_commands nst st nst.current_time
  ;;
  let all_egress_events swid nst = 
    let st = nst.switches.(swid) in
    let st', evs = InterpSwitch.all_egress_events st in
    save_update nst st';
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
