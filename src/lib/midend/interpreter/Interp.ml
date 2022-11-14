open Batteries
open Syntax
open InterpState

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
  (* Add events *)
  List.iter
    (fun (event, locs) ->
      List.iter
        (fun (loc, port) -> State.push_input_event loc port event nst)
        locs)
    spec.events;
  nst
;;

let initialize renaming spec_file ds =
  let pp, ds = Preprocess.preprocess ds in
  (* Also initializes the Python environment *)
  let spec = InterpSpec.parse pp renaming spec_file in
  let nst = initial_state pp spec in
  let nst = InterpCore.process_decls nst ds in
  nst, pp, spec
;;

let simulate (nst : State.network_state) =
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
        | Some (event, port) ->
          (match Env.find_opt event.eid nst.handlers with
          | None -> error @@ "No handler for event " ^ Cid.to_string event.eid
          | Some handler ->
            Printf.printf
              "t=%d: Handling %sevent %s at switch %d, port %d\n"
              nst.current_time
              (match Env.find event.eid nst.event_sorts with
              | EEntry _ -> "entry "
              | _ -> "")
              (CorePrinting.event_to_string event)
              idx
              port;
            handler nst idx port event);
          interp_events ((idx + 1) mod Array.length nst.switches) nst)
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

type event_getter = int -> InterpSpec.located_event list

(* interp events until max_time is reached 
   or there are no more events to interpret *)
let rec interp_events event_getter_opt idx max_time nst = 
  match State.next_time nst with
  | None -> nst
  | Some t ->
    let nst =
      if idx = 0
      then (
        (* when index is 0, poll for new events *)
        match event_getter_opt with 
        | None ->  
          { nst with current_time = max t (nst.current_time + 1) }
        | Some event_getter -> 
          (* if there's an event getter, check for new events *)
          State.push_located_events 
            (event_getter nst.current_time)
            nst
          ;
          { nst with current_time = max t (nst.current_time + 1) }
      )
        else nst
    in
    if ((max_time > -1) && (nst.current_time > max_time))
    then nst
    else (
      match State.next_event idx nst with
      | None -> interp_events event_getter_opt ((idx + 1) mod Array.length nst.switches) max_time nst
      | Some (event, port) ->
        (match Env.find_opt event.eid nst.handlers with
        | None -> error @@ "No handler for event " ^ Cid.to_string event.eid
        | Some handler ->
          if (not Cmdline.cfg.interactive)
          then 
            Printf.printf
              "t=%d: Handling %sevent %s at switch %d, port %d\n"
              nst.current_time
              (match Env.find event.eid nst.event_sorts with
              | EEntry _ -> "entry "
              | _ -> "")
              (CorePrinting.event_to_string event)
              idx
              port;
            handler nst idx port event);
        interp_events event_getter_opt ((idx + 1) mod Array.length nst.switches) max_time nst)
;;

let sighdl s =
  print_endline ("got signal "^(string_of_int s));
  if (s != -14) then (exit 1)
;;


type 'a inp = 
  | IEvent of 'a
  | ICmdStr of string 
  | Continue
  | End

let handle_cmd cmdstr = 
  (* execute a command string *)
  (* left off here. Implement table update command. *)
  print_endline ("executing command: "^cmdstr)
;;

(* Run the interpreter in interactive mode. *)
let run pp renaming (spec:InterpSpec.t) (nst : State.network_state) =
  let all_fds = match spec.ctl_pipe_name with 
    | Some (ctl_pipe_name) -> 
      print_endline ("using control pipe: "^ctl_pipe_name);
      let exists = try
        Unix.access ctl_pipe_name [Unix.F_OK];
        true
      with Unix.Unix_error _ -> false
      in
      (* we can mkfifo if file is fifo or does not exist *) 
      let okay_to_mkfifo = 
        if (not exists) then 
          true
        else
          let stats = Unix.stat ctl_pipe_name in
          if (stats.st_kind <> Unix.S_FIFO) then
            error
              @@"the control pipe file "
              ^ctl_pipe_name
              ^" already exists, and is not a named pipe. "
              ^" please delete it or use another file."
          else
            true
      in
      (if (okay_to_mkfifo) then 
        (try Unix.mkfifo ctl_pipe_name 0o664 with
          | Unix.Unix_error(Unix.EEXIST, _, _) -> ()
          | e ->  raise e)
      else
        error "could not create named pipe to controller");
      let ctl_fd = Unix.openfile 
        ctl_pipe_name 
        [Unix.O_RDONLY; Unix.O_NONBLOCK] 
        0 
      in
      [Unix.stdin; ctl_fd]
    | None -> [Unix.stdin]
  in
  (* open control pipe, reading and nonblocking *)
  let get_input pp renaming num_switches current_time twait = 
    let read_fds, _, _ = 
      try (Unix.select all_fds [] [] twait)
      with (Unix.Unix_error(err, fname, arg)) -> (
        match err with 
        | Unix.EBADF -> print_endline "here?"; [], [], [] (* happens when stdin has closed *)
        | _ -> error @@ "[get_input] unix error: " ^ fname ^ "("^ arg ^")"
      ) 
    in 
    if List.mem Unix.stdin read_fds
    then try 
        let ev_str = input_line stdin in 
        let ev_json = Yojson.Basic.from_string ev_str in 
        let ev_internal = InterpSpec.parse_event pp renaming num_switches current_time ev_json in 
        IEvent (ev_internal)
      with
        | _ -> End (* when we can't read events from stdin, we're done *)
    else if ((List.length all_fds) = 2) && (List.mem (List.nth all_fds 1) read_fds)
    then try 
        let ctl_fd = (List.nth all_fds 1) in 
        let cmd = input_line (Unix.in_channel_of_descr ctl_fd) in 
        ICmdStr(cmd)
      with
        | End_of_file -> Continue 
          (* end of file just means a control client closed -- its okay *)
        | e -> let _ = e in error "error reading from control pipe (NOT an EOF)"
    (* there's nothing to read right now, but there may be in the future *)
    else Continue
  in

  (* get up to n inputs. process the commands and return the events. 
     used while the interpreter's event processing loop is running. *)
  let get_events_nonblocking pp renaming num_switches n current_time = 
    let located_events = List.filter_map 
      (fun _ -> 
          match (get_input pp renaming num_switches current_time 0.0) with
          | IEvent(e) -> Some(e)
          | ICmdStr(s) -> handle_cmd s; None
          | _ -> None)
      (MiscUtils.range 0 n)
    in 
    located_events
  in

  (* wait for 1 event or eof *)
  let get_input_blocking pp renaming num_switches current_time = 
    let res = get_input pp renaming num_switches current_time (-1.0) in
    res
  in

  (* function to get events that arrive while interpreter is executing *)
  let event_getter = get_events_nonblocking pp renaming spec.num_switches spec.num_switches in 

  (* poll_loop waits for more events on stdin 
     after the interpreter has finished all of 
     its input events *)
  let rec poll_loop (nst : State.network_state) = 
    (* get a single event or eof *)
    let input = get_input_blocking 
      pp 
      renaming 
      spec.num_switches 
      nst.current_time 
    in 
    match input with
    | End -> nst (* end *)
    | Continue -> poll_loop nst        
    | IEvent (ev, locs) -> 
        State.push_located_event locs ev nst;
        let nst = interp_events (Some event_getter) 0 (-1) nst in
        poll_loop nst        
    (* control command -- execute it then continue *)
    | ICmdStr(cmd_str) -> 
      print_endline ("executing control command: "^(cmd_str));
        poll_loop nst        
  in

  Random.init nst.config.random_seed;
  (* interp events with no event getter to initialize network *)
  let nst = interp_events None 0 (nst.config.max_time) nst in 
  poll_loop nst
;;
