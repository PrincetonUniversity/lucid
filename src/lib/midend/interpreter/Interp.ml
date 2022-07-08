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

let simulate_inner (nst : State.network_state) = 
  let rec interp_events idx nst =
    match State.next_time nst with
    | None -> nst
    | Some t ->
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
            if (Cmdline.cfg.verbose)
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
          interp_events ((idx + 1) mod Array.length nst.switches) nst)
  in
  let nst = interp_events 0 nst in
  nst
;;

let simulate (nst : State.network_state) =
  Console.report
  @@ "Using random seed: "
  ^ string_of_int nst.config.random_seed
  ^ "\n";
  Random.init nst.config.random_seed;
  simulate_inner nst 
;;


(* run the interpreter in interactive mode:
  1. execute all the events in the spec file. 
  2. poll stdin for new events until it closes.
  3. print exit events to stdout. *)
let run pp renaming spec (nst : State.network_state) =
   Random.init nst.config.random_seed;
  (* step 1: run the startup events *)
  let nst = simulate_inner nst in 

  (* step 2: poll for events and interpret them as they arrive from stdin *)
  let rec poll_loop (nst : State.network_state) = 
    (* parse the event, give it a time of max(time, current_time) *)
    (* print_endline "[poll_loop] polling for new event"; *)
    let next_ev_opt = InterpStream.get_event_blocking 
      pp 
      renaming 
      spec 
      nst.current_time 
    in 
    match next_ev_opt with 
      | Some(ev, locs) -> 
        (* update interpreter state (push) *)        
        State.push_input_events locs ev nst;
        (* run the interpreter until there are no more events to process *)
        let nst = simulate_inner nst in 
        poll_loop nst
      | _ -> (* EOF *) nst
  in 
  (* return final state *)
  poll_loop nst
;;
