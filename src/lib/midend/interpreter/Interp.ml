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


(* Run the interpreter in interactive mode. *)
let run pp renaming (spec:InterpSpec.t) (nst : State.network_state) =
  let get_event pp renaming num_switches current_time twait = 
    let read_fds, _, _ = 
      try (Unix.select [Unix.stdin] [] [] twait)
      with (Unix.Unix_error(err, fname, arg)) -> (
        match err with 
        | Unix.EBADF -> [], [], [] (* happens when stdin has closed *)
        | _ -> error @@ "[get_event] unix error: " ^ fname ^ "("^ arg ^")"
      ) 
    in 
    match read_fds with 
      | [_] -> (
          try (
            let ev_str = input_line stdin in 
            let ev_json = Yojson.Basic.from_string ev_str in 
            let ev_internal = InterpSpec.parse_event pp renaming num_switches current_time ev_json in 
            Some (ev_internal)
          )
          (* eof --> nothing to read *)
          with 
            | End_of_file -> None
            | e -> let _ = e in None (* Bad file descriptor*)
      )
      | _ -> None
  in

  (* get up to n events if they are available *)
  let get_events_nonblocking pp renaming num_switches n current_time = 
    let located_events = List.filter_map 
      (fun _ -> get_event pp renaming num_switches current_time 0.0)
      (MiscUtils.range 0 n)
    in 
    located_events
  in

  (* wait for 1 event or eof *)
  let get_event_blocking pp renaming num_switches current_time = 
    get_event pp renaming num_switches current_time (-1.0)
  in

  (* function to get events that arrive while interpreter is executing *)
  let event_getter = get_events_nonblocking pp renaming spec.num_switches spec.num_switches in 

  (* poll_loop waits for more events on stdin 
     after the interpreter has finished all of 
     its input events *)
  let rec poll_loop (nst : State.network_state) = 
    (* get a single event or eof *)
    let located_ev_opt = get_event_blocking 
      pp 
      renaming 
      spec.num_switches 
      nst.current_time 
    in 
    match located_ev_opt with
    | None -> nst (* end *)
    | Some (ev, locs) -> 
        State.push_located_event locs ev nst;
        let nst = interp_events (Some event_getter) 0 (-1) nst in
        poll_loop nst        
  in

  Random.init nst.config.random_seed;
  (* interp events with no event getter to initialize network *)
  let nst = interp_events None 0 (nst.config.max_time) nst in 
  poll_loop nst
;;
