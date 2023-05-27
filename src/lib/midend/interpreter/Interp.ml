open Batteries
open Yojson.Basic
open Syntax
open InterpSyntax
open InterpState
open CoreSyntaxGlobalDirectory

let initial_state (pp : Preprocess.t) (spec : InterpSpec.t) =
  let nst =
    { (State.create spec.config) with
      event_sorts = Env.map fst pp.events
    ; switches = Array.init spec.num_switches (fun _ -> State.empty_state ())
    ; links = spec.links
    }
  in
  (* Add builtins *)
  List.iter
    (fun f -> State.add_global_function f nst)
    (System.defs @ Events.defs @ Counters.defs @ Arrays.defs @ PairArrays.defs);
  (* Add externs *)
  List.iteri
    (fun i exs -> Env.iter (fun cid v -> State.add_global i cid (V v) nst) exs)
    spec.externs;
  (* Add foreign functions *)
  Env.iter
    (fun cid fval ->
      Array.iteri (fun i _ -> State.add_global i cid fval nst) nst.switches)
    spec.extern_funs;
  (* push located events to switch queues *)
  List.iter
    (fun { ievent = event; ilocs = locs } ->
      List.iter (fun loc -> State.push_interp_event loc event nst) locs)
    (*         (fun loc  -> let (loc, port) = loc_to_tup loc in State.push_input_event loc port event nst)
        locs) *)
    spec.events;
  nst
;;

let initialize renaming spec_file ds =
  let pp, ds = Preprocess.preprocess ds in
  (* Also initializes the Python environment *)
  let spec = InterpSpec.parse pp renaming spec_file in
  let nst = initial_state pp spec in
  let nst = InterpCore.process_decls nst ds in
  (* initialize the global name directory *)
  let nst =
    { nst with global_names = CoreSyntaxGlobalDirectory.build_coredirectory ds }
  in
  nst, pp, spec
;;

(*** innermost functions of interpretation loop: execute one
     event or control op, then call a function to continue interpretation ***)
let execute_event
  print_log
  idx
  (nst : State.network_state)
  (event : CoreSyntax.event)
  port
  =
  match Env.find_opt event.eid nst.handlers with
  | None -> error @@ "No handler for event " ^ Cid.to_string event.eid
  | Some handler ->
    if print_log
    then
      if Cmdline.cfg.json || Cmdline.cfg.interactive
      then
        `Assoc
          [ ( "event_arrival"
            , `Assoc
                [ "switch", `Int idx
                ; "port", `Int port
                ; "time", `Int nst.current_time
                ; "event", `String (CorePrinting.event_to_string event) ] ) ]
        |> Yojson.Basic.pretty_to_string
        |> print_endline
      else
        Printf.printf
          "t=%d: Handling %sevent %s at switch %d, port %d\n"
          nst.current_time
          (match Env.find event.eid nst.event_sorts with
           | EPacket -> "packet "
           | _ -> "")
          (CorePrinting.event_to_string event)
          idx
          port;
    handler nst idx port event
;;

let execute_control swidx (nst : State.network_state) (ctl_ev : control_event) =
  InterpControl.handle_control nst swidx ctl_ev
;;

let execute_interp_event
  print_log
  simulation_callback
  idx
  (nst : State.network_state)
  ievent
  port
  =
  (match ievent with
   | IEvent event -> execute_event print_log idx nst event port
   | IControl ctl_ev -> execute_control idx nst ctl_ev);
  simulation_callback ((idx + 1) mod Array.length nst.switches) nst
;;

let simulate (nst : State.network_state) =
  if (not Cmdline.cfg.json) && not Cmdline.cfg.interactive
  then
    Console.report
    @@ "Using random seed: "
    ^ string_of_int nst.config.random_seed
    ^ "\n";
  Random.init nst.config.random_seed;
  let rec interp_events idx nst =
    match State.next_time nst with
    | None -> nst
    | Some t ->
      (* Increment the current time *)
      let nst =
        if idx = 0
        then { nst with current_time = max t (nst.current_time + 1) }
        else nst
      in
      if nst.current_time > nst.config.max_time
      then nst
      else (
        match State.next_event idx nst with
        | None -> interp_events ((idx + 1) mod Array.length nst.switches) nst
        | Some (ievent, port) ->
          execute_interp_event true interp_events idx nst ievent port)
  in
  let nst = interp_events 0 nst in
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

type event_getter = int -> InterpSyntax.located_event list

(* interp events until max_time is reached
   or there are no more events to interpret *)
let rec interp_events event_getter_opt max_time idx nst =
  match State.next_time nst with
  | None -> nst
  | Some t ->
    let nst =
      if idx = 0
      then (
        (* when index is 0, poll for new events *)
        match event_getter_opt with
        | None -> { nst with current_time = max t (nst.current_time + 1) }
        | Some (event_getter : event_getter) ->
          (* if there's an event getter, check for new events *)
          State.push_located_events (event_getter nst.current_time) nst;
          { nst with current_time = max t (nst.current_time + 1) })
      else nst
    in
    if max_time > -1 && nst.current_time > max_time
    then nst
    else (
      match State.next_event idx nst with
      | None ->
        interp_events
          event_getter_opt
          max_time
          ((idx + 1) mod Array.length nst.switches)
          nst
      | Some (event, port) ->
        execute_interp_event
          false
          (interp_events event_getter_opt max_time)
          idx
          nst
          event
          port)
;;

let sighdl s =
  print_endline ("got signal " ^ string_of_int s);
  if s != -14 then exit 1
;;

(* this type should be refactored out *)
type interactive_mode_input =
  | Process of located_event list
  | Continue
  | End

(* create a pipe for control commands *)
let create_control_pipe ctl_pipe_name =
  prerr_endline ("using control pipe: " ^ ctl_pipe_name);
  let exists =
    try
      Unix.access ctl_pipe_name [Unix.F_OK];
      true
    with
    | Unix.Unix_error _ -> false
  in
  (* we can mkfifo if file is fifo or does not exist *)
  let okay_to_mkfifo =
    if not exists
    then true
    else (
      let stats = Unix.stat ctl_pipe_name in
      if stats.st_kind <> Unix.S_FIFO
      then
        error
        @@ "the control pipe file "
        ^ ctl_pipe_name
        ^ " already exists, and is not a named pipe. "
        ^ " please delete it or use another file."
      else true)
  in
  if okay_to_mkfifo
  then (
    try Unix.mkfifo ctl_pipe_name 0o664 with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    | e -> raise e)
  else error "could not create named pipe to controller";
  let ctl_fd = Unix.openfile ctl_pipe_name [Unix.O_RDONLY; Unix.O_NONBLOCK] 0 in
  ctl_fd
;;

(* Run the interpreter in interactive mode. *)
let run pp renaming (spec : InterpSpec.t) (nst : State.network_state) =
  let all_fds =
    match spec.ctl_pipe_name with
    | Some ctl_pipe_name -> [Unix.stdin; create_control_pipe ctl_pipe_name]
    | None -> [Unix.stdin]
  in
  (* get a single input from either stdin or the control pipe *)
  let get_input pp renaming num_switches current_time twait =
    (* poll stdin and the control pipe for input *)
    let read_fds, _, _ =
      try Unix.select all_fds [] [] twait with
      | Unix.Unix_error (err, fname, arg) ->
        (match err with
         | Unix.EBADF ->
           ( []
           , []
           , [] (* supposed to happen when stdin closes, but not sure of that.*)
           )
         | _ -> error @@ "[get_input] unix error: " ^ fname ^ "(" ^ arg ^ ")")
    in
    (* this part sould be cleaned up, but it may be a bit delicate with the file descriptors. *)
    (* if stdin has input available, read it *)
    if List.mem Unix.stdin read_fds
    then (
      try
        let ev_str = input_line stdin in
        let ev_json = Yojson.Basic.from_string ev_str in
        let located_events =
          InterpSpec.parse_interp_event_list
            pp
            renaming
            num_switches
            current_time
            ev_json
        in
        (* let located_event = InterpSpec.parse_interp_input pp renaming num_switches current_time ev_json in  *)
        Process located_events
      with
      | _ ->
        End
        (* if reading from stdin fails, we want to exit *)
        (* if there are any other input pipes (i.e., the control pipe), read a command from it *))
    else if List.length all_fds = 2 && List.mem (List.nth all_fds 1) read_fds
    then (
      try
        let ctl_fd = List.nth all_fds 1 in
        let ev_str = input_line (Unix.in_channel_of_descr ctl_fd) in
        let ev_json = Yojson.Basic.from_string ev_str in
        let located_events =
          InterpSpec.parse_interp_event_list
            pp
            renaming
            num_switches
            current_time
            ev_json
        in
        (* let located_event = InterpSpec.parse_interp_input pp renaming num_switches current_time ev_json in  *)
        Process located_events
      with
      (* if reading from anything besides stdin fails, we don't want to exit because there's still stdin.. *)
      (* TODO -- lol clean that up *)
      | End_of_file -> Continue
      | e ->
        let _ = e in
        error "error reading from control pipe (NOT an EOF)")
    else Continue
  in
  (* get up to n inputs. For any inputs that are events,
     queue the events for processing. For any inputs that are
     commands, execute them immediately. *)
  let get_events_nonblocking pp renaming num_switches n current_time =
    let located_events =
      List.filter_map
        (fun _ ->
          match get_input pp renaming num_switches current_time 0.0 with
          | Process e -> Some e
          | _ -> None)
        (MiscUtils.range 0 n)
    in
    List.flatten located_events
  in
  (* wait for 1 event or eof *)
  let get_input_blocking pp renaming num_switches current_time =
    let res = get_input pp renaming num_switches current_time (-1.0) in
    res
  in
  (* function to get a batch of events that arrive while interpreter is executing *)
  let event_getter =
    get_events_nonblocking pp renaming spec.num_switches spec.num_switches
  in
  let rec poll_loop (nst : State.network_state) =
    (* wait for a single event or command *)
    let input =
      get_input_blocking pp renaming spec.num_switches nst.current_time
    in
    match input with
    | End -> nst (* end *)
    | Continue -> poll_loop nst
    | Process loc_evs ->
      (* add the events to queues in the network simulator *)
      List.iter
        (fun loc_ev -> State.push_interp_events loc_ev.ilocs loc_ev.ievent nst)
        loc_evs;
      (* interpret all the queued events, using event_getter to poll for more events
           in between iterations. *)
      let nst = interp_events (Some event_getter) (-1) 0 nst in
      poll_loop nst
  in
  Random.init nst.config.random_seed;
  (* interp events with no event getter to initialize network *)
  let nst = interp_events None nst.config.max_time 0 nst in
  poll_loop nst
;;
