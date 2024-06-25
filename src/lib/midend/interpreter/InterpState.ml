(* Network-wide state in the interpreter. *)
open CoreSyntax
open InterpSyntax
open Batteries
module Env = Collections.CidMap
module IntMap = Map.Make (Int)

open InterpConfig
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

  type network_state =
    { current_time : int
    ; config : InterpConfig.config
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
  let save_update nst sw = nst.switches.(sw.swid) <- sw;;

  let lookup_switch nst swid = nst.switches.(swid);;

  let current_time nst = nst.current_time;;

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

  (* calculate when an event should arrive at the next switch's ingress. 
     The calculation should be improved to add ingres -> egress queue time and then 
      propagation time in two separate steps. Also, the names are confusing and why 
     would propagation delay only apply to recirculating events? *)
    let calc_arrival_time nst src_id dst_id desired_delay = 
    let propagate_delay =
      if src_id = dst_id
      then
        (nst : network_state).config.propagate_delay
        + Random.int nst.config.random_propagate_range
      else 0
    in
    nst.current_time
      + max desired_delay nst.config.generate_delay
      + propagate_delay
      + Random.int nst.config.random_delay_range
  ;;


  let network_utils = { save_update; lookup_dst; lookup_switch; get_time=current_time; calc_arrival_time }
  ;;

  (* updating and accessing globals defined for all switches *)
  let add_global swid cid v nst = 
    save_update nst (InterpSwitch.add_global cid v nst.switches.(swid))
  ;;
  let add_global_function (g : global_fun) nst =
    Array.modify
      (fun st ->
        if InterpSwitch.mem_env g.cid st
        then
          error ("global variable " ^ Cid.to_string g.cid ^ "  already defined")
        else InterpSwitch.add_global g.cid (F (Some(g.cid), g.body)) st)
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

  let update_counter swid event nst =
    let st = nst.switches.(swid) in
    let event_sort = Env.find event.eid nst.event_sorts in
    InterpSwitch.update_counter event_sort st
  ;;

  (* load an interpreter input into the network environment *)
  let load_interp_input nst interp_input = 
    let locs = InterpJson.input_locs interp_input in
    List.iter (fun loc -> 
      let swid = match loc.switch with 
        | None -> error "input event not associated with a switch"
        | Some(switch) -> switch
      in
      InterpSwitch.load_interp_input nst (lookup_switch nst swid) loc.port interp_input)
      locs
  ;;

  let load_interp_inputs nst interp_inputs = 
    List.iter (load_interp_input nst) interp_inputs


  let next_time nst =
    Array.fold_left
      (fun acc st ->
        match acc, InterpSwitch.next_time st with
        | None, x | x, None -> x
        | Some x, Some y -> Some (min x y))
      None
      nst.switches
  ;;

  (*** these are all temporary helpers until we clean up the behavior of egress queues ***)
  let next_ready_event swid nst =
    let st = nst.switches.(swid) in
    match InterpSwitch.next_event ( nst.current_time) st with
      | None -> None
      | Some(st', epgs) -> 
        save_update nst st';
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

  let nst_to_string
    ?(show_vars = false)
    ?(show_pipeline = true)
    ?(show_queue = true)
    ?(show_exits = true)
    nst
    =
    let base_str = Array.fold_lefti
      (fun acc idx st ->
        Printf.sprintf "%s\nSwitch %d : %s" acc idx
        @@ InterpSwitch.to_string ~show_vars ~show_pipeline ~show_queue ~show_exits st)
      ""
      nst.switches
    in
    if Cmdline.cfg.json || Cmdline.cfg.interactive
      then InterpJson.interp_report_json "final_state" base_str None
      else base_str
  ;;
end

let extract_ival iv =InterpSyntax.extract_ival iv
;;
