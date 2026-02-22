(* The main interpreter library. Does initialization, user-polling, and event execution / simulator update loops. *)
open Batteries
open Yojson.Basic
open CoreSyntax
open InterpSyntax
open InterpJson
open InterpState
open InterpControl
open CoreSyntaxGlobalDirectory
open InterpStdio

module Env = Collections.CidMap

(* Temporary functions from InterpState during refactor. *)
let save_update nst sw = 
  let open InterpSwitch in
  nst.switches.(sw.swid) <- sw;;
let next_ready_event swid nst =
  let st = nst.switches.(swid) in
  match InterpSwitch.next_event !( st.global_time) st with
    | None -> None
    | Some(st', epgs) -> 
      save_update nst st';
      Some(epgs)
;;

let ready_egress_events swid nst = 
  let st = nst.switches.(swid) in
  let st', evs = InterpSwitch.ready_egress_events !(st.global_time) st in
  save_update nst st';
  evs
;;

let ready_control_commands swid nst = 
  let st = nst.switches.(swid) in
  InterpSwitch.ready_control_commands nst st !(st.global_time)
;;

let all_egress_events swid nst = 
  let st = nst.switches.(swid) in
  let st', evs = InterpSwitch.all_egress_events st in
  save_update nst st';
  evs
;;


(* load an interpreter input into the network environment *)
let load_interp_input nst interp_input = 
  let locs = InterpJson.input_locs interp_input in
  List.iter (fun loc -> 
    let swid = match loc.switch with 
      | None -> error "input event not associated with a switch"
      | Some(switch) -> switch
    in
    InterpSwitch.load_interp_input nst (nst.switches.(swid)) loc.port interp_input)
    locs
;;

let load_interp_inputs nst interp_inputs = 
  List.iter (load_interp_input nst) interp_inputs


let event_sorts events = Env.map (fun (a, _, _) -> a) events
;;
let event_signatures events = List.fold_left
  (fun acc (evid, event) -> 
  let (_, num_opt, arg_tys) = event in
  (match num_opt with 
  | None -> print_endline ("event has no number: " ^ (Cid.to_string evid)); 
  | _ -> ());
  let num = Option.get num_opt in
  InterpSim.IntMap.add num (evid, arg_tys) acc)
    InterpSim.IntMap.empty
    (Env.bindings events)

(* initial state is before declarations are parsed and loaded into handlers *)
let initial_state ?(with_sockets=false) (pp : Preprocess.t) (spec : InterpSpec.t) =
  let global_time = ref (-1) in
  let empty_state = InterpState.create() in
  let switches = Array.init 
    spec.simconfig.num_switches (InterpSwitch.create 
        ~with_sockets:with_sockets 
        global_time
        (event_sorts pp.events) 
        (event_signatures pp.events) 
        spec.simconfig) 
  in
  let nst = { empty_state with switches } in
  (* give all the switches references to the switches Array *)
  Array.iteri 
    (fun swid _ -> (
       switches.(swid) <- InterpSwitch.set_sws switches.(swid) switches))      
    nst.switches
  ;  
  nst
;;

(* nst.switches.(sw.swid) <- sw *)

(* initialize misc per-switch state, 
   right now called after process_decls runs, 
   but does not need to be. *)
let init_switches nst (spec : InterpSpec.t) ds = 
  let add_global i cid x nst = 
    let sw_st = nst.switches.(i) in
    let sw_st = InterpSwitch.add_global cid x sw_st in
    nst.switches.(i) <- sw_st;
  in
  let add_global_function (g : global_fun) nst =
    Array.modify
      (fun st ->
        if InterpSwitch.mem_env g.cid st
        then
          error ("global variable " ^ Cid.to_string g.cid ^ "  already defined")
        else InterpSwitch.add_global g.cid (F (Some(g.cid), g.body)) st)
      nst.switches
  in
  (* Add builtins (per switch) *)
  List.iter
    (fun f -> add_global_function f nst)
    Builtins.builtin_defs;
  (* Add externs (per switch) *)
  List.iteri
    (fun i exs -> Env.iter (fun cid v -> add_global i cid (V v) nst) exs)
    spec.externs;
  (* Add foreign functions (per switch) *)
  Env.iter
    (fun cid fval ->
      Array.iteri (fun i _ -> add_global i cid fval nst) nst.switches)
    spec.extern_funs;
  (* Add error-raising function for background event parser (per switch) *)
  let bg_parse_cid = Cid.id Builtins.lucid_parse_id in 
  let bg_parse_fun = InterpParsing.lucid_parse_fun in
  Array.iteri
    (fun swid _ -> add_global swid bg_parse_cid (anonf bg_parse_fun) nst)
    nst.switches
  ;
  (* Initialize global names *)
  Array.iteri
    (fun swid _ -> nst.switches.(swid) <- {nst.switches.(swid) with global_names = CoreSyntaxGlobalDirectory.build_coredirectory ds})
    nst.switches
  ;
  (* push interpreter inputs to ingress and control command queues *)
  load_interp_inputs nst spec.events;
  nst
;;

(* Initialize interpreter with discrete time simulator *)
let initialize renaming spec_file ds =
  let pp, ds = Preprocess.preprocess ds in
  let spec = InterpSpec.parse pp renaming spec_file in
  let nst = initial_state pp spec in
  let nst = InterpCore.process_decls nst ds in
  (* initialize the global name directory *)
  (* let nst =
    { nst with global_names = CoreSyntaxGlobalDirectory.build_coredirectory ds }
  in *)
  let nst = init_switches nst spec ds in
  nst, pp, spec
;;

(*** innermost functions of interpretation loop: execute one
     event or control op, then call a function to continue interpretation ***)
let execute_event
  print_log
  swid
  (nst : network_state)
  (event : CoreSyntax.event_val)
  port
  gress
  =
  let st = nst.switches.(swid) in
  let handlers, gress_str = match gress with 
  | InterpSwitch.Egress ->st.egress_hdlrs, " egress "
  | InterpSwitch.Ingress -> 
    (* try using the switch's hdlrs field instead of the global handlers one *)
    st.hdlrs, ""
    (* nst.handlers, "" *)
  in
  match Env.find_opt event.eid handlers with
  (* if we found a handler, run it *)
  | Some handler ->
    if print_log
    then
      if InterpConfig.cfg.json || InterpConfig.cfg.interactive
      then
        `Assoc
          [ ( "event_arrival"
            , `Assoc
                [ "switch", `Int swid
                ; "port", `Int port
                ; "time", `Int !(st.global_time)
                ; "event", `String (CorePrinting.event_to_string event) ] ) ]
        |> Yojson.Basic.pretty_to_string
        |> print_endline
      else
        Printf.printf
          "t=%d: Handling %s%sevent %s at switch %d, port %d\n"
          !(st.global_time)
          gress_str
          (match Env.find event.eid nst.switches.(swid).event_sorts with
          | EPacket -> "packet "
          | _ -> "")
          (CorePrinting.event_to_string event)
          swid
          port;
    handler nst swid port event  
  (* if we didn't find a handler, that's an error for ingress but okay for egress. *)
  | None -> (
    match gress with
    | InterpSwitch.Egress -> 
      (* add propagation delay and push to destination *)
      (* run a default handling statement that re-serializes the event and 
         pushes it to the next switch.*)
      let builtin_env = Env.add (Id (Builtins.ingr_port_id)) (InterpSyntax.V (C.vint port 32)) Env.empty in
      (* print_endline@@"t="^(string_of_int nst.current_time)^" running default egress handler for event " ^ Cid.to_string event.eid ^ " at switch " ^ (string_of_int swid) ^ " port " ^ (string_of_int port); *)
      let default_handler_body = 
        C.SGen(C.GPort(C.vint_exp port 32), C.value_to_exp {v=C.VEvent(event); vty=C.tevent; vspan=Span.default})
      in      
      ignore@@InterpCore.interp_statement nst HEgress swid builtin_env (C.statement default_handler_body)
    | InterpSwitch.Ingress ->    
      error @@ "No handler for event " ^ Cid.to_string event.eid )
;;

let execute_main_parser print_log swidx port (nst: network_state) (pkt_ev : (CoreSyntax.event_val)) = 
  let sw_st = nst.switches.(swidx) in
  let payload_val = List.hd pkt_ev.data in
  (* main takes 2 arguments, port and payload. Port is implicit. *)
  let main_args = [InterpSyntax.V (C.vint port 32); InterpSyntax.V payload_val] in
  let main_parser = InterpSwitch.lookup (Cid.id Builtins.main_parse_id) sw_st in

  match main_parser with 
    | F (_, parser_f) -> (
      if print_log
        then
          if InterpConfig.cfg.json || InterpConfig.cfg.interactive
          then
            `Assoc
              [ ( "packet_arrival"
                , `Assoc
                    [ "switch", `Int swidx
                    ; "port", `Int port
                    ; "time", `Int !(sw_st.global_time)
                    ; "bytes", `String (CorePrinting.value_to_string payload_val) ] ) ]
            |> Yojson.Basic.pretty_to_string
            |> print_endline
          else
            Printf.printf
              "t=%d: Parsing packet %s at switch %d, port %d\n"
              !(sw_st.global_time)
              (CorePrinting.value_to_string payload_val)
              swidx
              port;
      let event_val = parser_f nst swidx main_args |> extract_ival in
      match event_val.v with 
      | VEvent(event_val) -> execute_event print_log swidx nst event_val port (InterpSwitch.Ingress)
      | VBool(false) -> () (* Its okay to not generate an event. That will happen for drops. *)
      (* error "main parser did not generate an event" *)
      | _ -> error "main parser did not return an event or a no event signal")
    | _ -> error "the global named 'main' is not a parser"
;;   

let execute_control swidx (nst : network_state) (ctl_ev : control_val) =
  (* construst a helper to install the table entry, as InterpControl 
     can't call back up to InterpCore *)
  let do_tbl_install tbl_cid (cmd : entry_install_cmd) =
    let etbl = exp (EVar tbl_cid) (ty@@TBool) in
    let ekey = exp 
      (ETuple(cmd.imatch)) 
      (ty@@TTuple(List.map (fun exp -> exp.ety.raw_ty) cmd.imatch) ) in
    let emask = exp
      (ETuple(cmd.imask))
      (ty@@TTuple(List.map (fun exp -> exp.ety.raw_ty) cmd.imask) ) in
    let eaction_constr = exp 
      (EVar(Cid.id cmd.iaction))
      (ty@@TBool) (* note: type is bogus *)      
    in
    let eaction_constr_args = exp
      (ETuple(cmd.iargs))
      (ty@@TTuple(List.map (fun exp -> exp.ety.raw_ty) cmd.iargs)) in
    let eargs = [etbl; ekey; emask; eaction_constr; eaction_constr_args] in
    let ecall = C.exp (ECall(Cid.create ["Table"; "install_ternary"],eargs, false)) (ty TBool) in
    InterpCore.interp_exp nst swidx Env.empty ecall
  in
  InterpControl.handle_control 
    do_tbl_install
    nst.switches.(swidx).pipeline
    nst.switches.(swidx).global_names 
    ctl_ev    
;;


let run_event_tup print_log idx nst ((event:CoreSyntax.event_val), port, gress) = 
  if (not event.eserialized) then 
    execute_event print_log idx nst event port gress
  else 
  execute_main_parser print_log idx port nst event
;;

let run_event_at_time print_log idx nst (ievent, port, event_time, gress) = 
  (* let nst = {nst with current_time=event_time} in *)
  let st = nst.switches.(idx) in
  st.global_time := event_time;
  
  run_event_tup print_log idx nst (ievent, port, gress)
;;

let execute_interp_event
  print_log
  simulation_callback
  idx
  (nst : network_state)
  events
  =
  List.iter (run_event_tup print_log idx nst) events;
  simulation_callback ((idx + 1) mod Array.length nst.switches) nst
;;

(* run all the egress events from all of the switches *)
let execute_ready_egress_events print_log (nst:network_state) = 
  Array.iteri 
    (fun swid _ -> 
        let egr_evs = ready_egress_events swid nst in
        List.iter (run_event_tup print_log swid nst) egr_evs;
    )
  nst.switches
;;

let finish_egress_events print_log (nst:network_state) = 
  Array.iteri 
    (fun swid _ -> 
        let egr_evs = all_egress_events swid nst in
        List.iter (run_event_at_time print_log swid nst) egr_evs;
    )
  nst.switches
;;

(* execute all the control commands at the switch *)
let execute_ready_controls swid (nst : network_state) =
  List.iter 
    (execute_control swid nst) 
    (ready_control_commands swid nst)
;;

let advance_current_time next_event_time (nst: network_state) = 
  (* advance the current time of the network to the time of the
     next queued event, unless the event is queued at the 
     current time, in which case we advance the current time 
     by 1.
     If you run this at the start of processing an event from each 
     switch, it models being able to process 1 event per time unit. *)
  nst.switches.(0).global_time := max next_event_time (!(nst.switches.(0).global_time) + 1);
  nst
  (* {nst with current_time = max next_event_time (nst.current_time + 1)} *)
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
let rec execute_sim_step idx nst = 
  (* execute a step of the simulation *)
  match next_time nst with
  (* no time at which some switch has a thing to do? then nothing will ever change. *)
  | None -> nst
  | Some next_event_time -> (
    (* special processing when you are at the first switch *)
    let nst = if (idx = 0) 
      then (
        let nst = advance_current_time next_event_time nst in
        execute_ready_egress_events InterpConfig.cfg.show_interp_events nst;
        nst)
      else nst
    in
    if !(nst.switches.(idx).global_time) > nst.switches.(idx).config.max_time
      then nst
      else (
        (* run any control events *)
        execute_ready_controls idx nst;
        (* check for ingress and egress events with time < nst.current_time *)
        match next_ready_event idx nst with
        | Some (epgs) -> execute_interp_event InterpConfig.cfg.show_interp_events execute_sim_step idx nst epgs
        | None -> execute_sim_step ((idx + 1) mod Array.length nst.switches) nst
      )    
  )
;;

let simulate (nst : network_state) =
  if (not InterpConfig.cfg.json) && not InterpConfig.cfg.interactive
  then (* there must be at least 1 switch, and all random seeds are the same *)
    Console.report
    @@ "Using random seed: "
    ^ string_of_int nst.switches.(0).config.random_seed
    ^ "\n";
  Random.init nst.switches.(0).config.random_seed;
  let nst = execute_sim_step 0 nst in
  (* drain all the egresses one last time to get everything into an ingress queue for logging *)
  finish_egress_events InterpConfig.cfg.show_interp_events nst;
  nst
;;

(** interactive mode (stdio) implementation
  Interactive mode behavior notes:
    - Input:
      - expects every event to be a json dictionary on its own line
      - waits for eof
    - Execution:
      - starts polling stdin after max_time has elapsed
      - polls stdin for new events once per time unit
      - events on stdin execute at time = max(current_ts, event.timestamp)
    - Output:
      - prints each exit event to stdout as a json, one event per line
      - all printfs in the program print to stderr
**)

type event_getter = int -> InterpJson.interp_input list

(* interp events until max_time is reached
   or there are no more events to interpret *)

let load_new_events nst event_getter_opt = 
  match event_getter_opt with
  | None -> ()
  | Some(event_getter) -> 
    load_interp_inputs nst (event_getter nst) ;
;;
(* execute a step of the simulation running in interactive mode *)
let rec execute_interactive_sim_step event_getter_opt max_time idx nst = 
  let next_step_continuation = execute_interactive_sim_step event_getter_opt max_time in
  match next_time nst with
  (* no time at which some switch has a thing to do? then nothing will ever change. *)
  | None -> nst
  | Some t -> 
    let nst = if (idx = 0)
      then (
        execute_ready_egress_events InterpConfig.cfg.show_interp_events nst;
        load_new_events nst event_getter_opt;
        advance_current_time t nst)
      else nst
    in
    if max_time > -1 && !(nst.switches.(0).global_time) > max_time
      then nst
    else 
      match next_ready_event idx nst with
      | Some (epgs) ->
        execute_ready_controls idx nst;
        execute_interp_event InterpConfig.cfg.show_interp_events next_step_continuation idx nst epgs
      (* if there's no next event, move to the next switch *)
      | None -> next_step_continuation ((idx + 1) mod Array.length nst.switches) nst
;;


let run pp renaming (spec : InterpSpec.t) (nst : network_state) =
  (* read a single event, blocking until it appears. This is used to 
  read in new events after the interpreter has finished all of its 
  current work. *)
  let get_input_blocking = get_stdio_input true in
  (* read up to n immediately available events from stdin, 
  ignoring end of file. This is used to read in new events 
  while the interpreter is still working on a previous event. *)
  let get_input_nonblocking (nst: network_state) =
    let located_events =
      List.filter_map
        (fun _ ->
          match get_stdio_input false pp renaming spec.simconfig.num_switches !(nst.switches.(0).global_time)  with
          | Events e -> Some e
          | _ -> None)
        (MiscUtils.range 0 spec.simconfig.num_switches)
    in
    List.flatten located_events  
  in
  let rec poll_loop (nst : network_state) =
    (* wait for a single event or command *)
    let input =
      get_input_blocking pp renaming spec.simconfig.num_switches !(nst.switches.(0).global_time)
    in
    match input with
    | End -> (* no more input, but we want to finish up the egress events to generate everything *)
      execute_ready_egress_events true nst;
      nst
    | NoEvents -> poll_loop nst
    | Events evs -> 
      load_interp_inputs nst evs;
      (* interpret all the queued events, using event_getter to poll for more events
           in between iterations. *)
      let nst = execute_interactive_sim_step (Some get_input_nonblocking) (-1) 0 nst in
      poll_loop nst
  in
  Random.init nst.switches.(0).config.random_seed;
  (* interp events with no event getter to initialize network, then run the polling loop *)
  let nst = execute_interactive_sim_step None nst.switches.(0).config.max_time 0 nst in
  poll_loop nst  
;;


(*** SOFTWARE SWITCH ***)
let initialize_softswitch renaming ds =
  let pp, ds = Preprocess.preprocess ds in
  (* create a single-switch configuration for the simulator *)
  let spec = InterpSpec.default pp renaming in 
  let nst = initial_state ~with_sockets:true pp spec in
  let nst = InterpCore.process_decls nst ds in (* load declarations, which modifies state *)
  let nst = init_switches nst spec ds in
  nst, pp, spec
;;

(* get packet from socket or stdio *)
let get_stdio_input_blocking = 
  get_stdio_input true 
;;
(* read up to n immediately available events from stdin, 
ignoring end of file. This is used to read in new events 
while the interpreter is still working on a previous event. 
The important thing is ignoring eof, so interp exits on a turn. *)
let get_stdio_input_nonblocking pp renaming (spec : InterpSpec.t) (nst: network_state) =
  let located_events =
    List.filter_map
      (fun _ ->
        match get_stdio_input false pp renaming spec.simconfig.num_switches !(nst.switches.(0).global_time)  with
        | Events e -> Some e
        | _ -> None)
      (MiscUtils.range 0 spec.simconfig.num_switches)
  in
  List.flatten located_events  
;;

(* execute a step of the software switch *)
let rec execute_softswitch_step idx nst = 
  let current_time = Int64.to_int (Int64.of_float (Unix.gettimeofday () *. 1e9)) in
  nst.switches.(0).global_time := current_time;
  (* let nst = {nst with current_time} in  *)
  execute_ready_controls idx nst;
  match next_time nst with
  | None -> nst
  | Some next_t -> 
    if next_t > !(nst.switches.(0).global_time) then ( (* delayed event not yet ready *)
      nst 
    ) else ( (* there is an event ready to dispatch *)
      match next_ready_event idx nst with
      | Some (epgs) ->
        execute_interp_event InterpConfig.cfg.show_interp_events execute_softswitch_step idx nst epgs
      | None -> execute_softswitch_step idx nst
    )
;;

(* main loop for software switch *)
let run_softswitch pp renaming (spec : InterpSpec.t) (nst : network_state) =
  (*  assume there's only 1 switch (at index 0) *)
  let all_sockets = InterpSwitch.get_sockets (Array.get nst.switches 0) in
  let cur_sock_idx = ref 0 in
  let n_sockets = List.length(all_sockets) in
  let poll_sockets pp renaming (spec : InterpSpec.t) nst = 
    if !cur_sock_idx == n_sockets then (* read from stdio last *)
      (
        cur_sock_idx := 0;
        get_stdio_input_nonblocking pp renaming spec nst
      )
    else (
      let sock = List.nth all_sockets (!cur_sock_idx) in
      cur_sock_idx := (!cur_sock_idx + 1 );
      InterpSocket.read_event_batch sock
    )
  in
  let rec poll_loop (nst : network_state) =
    (* 1. poll for events *)
    let input = poll_sockets pp renaming spec nst in
    (* 2. load any new events *)
    load_interp_inputs nst input;
    (* 3. run the interpreter to process one or more events *)
    let nst = execute_softswitch_step 0 nst in
    (* 4. recurse *)
    poll_loop nst
  in
  Random.init nst.switches.(0).config.random_seed;
  (* interp events with no event getter to initialize network, then run the polling loop *)
  (* let nst = execute_softswitch_step 0 nst in *)
  poll_loop nst  
;;
