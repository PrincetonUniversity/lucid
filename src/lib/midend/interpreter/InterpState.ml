(* Interpreter context + helpers for data structure interpretation. *)
open CoreSyntax
open Batteries
module Env = Collections.CidMap
module IntMap = Map.Make (Int)

module State = struct
  module EventQueue = BatHeap.Make (struct
    type t = int * event * int

    let compare (t1, _, _) (t2, _, _) = Pervasives.compare t1 t2
  end)

  type stats_counter =
    { pkts_handled : int
    ; total_handled : int
    }

  let empty_counter = { pkts_handled = 0; total_handled = 0 }

  (* Maps switch -> port -> (switch * port) *)
  type topology = (int * int) IntMap.t IntMap.t

  let empty_topology num_switches =
    List.fold_left
      (fun acc n -> IntMap.add n IntMap.empty acc)
      IntMap.empty
      (List.init num_switches (fun n -> n))
  ;;

  type config =
    { max_time : int
    ; generate_delay : int
    ; propagate_delay : int
    ; random_seed : int
    ; random_delay_range : int
    ; random_propagate_range : int
    }

  type network_state =
    { current_time : int
    ; config : config
    ; handlers : handler Env.t
    ; links : topology
    ; switches : state array
    }

  and state =
    { global_env : ival Env.t
    ; event_queue : EventQueue.t
    ; pipeline : Pipeline.t
    ; exits : (event * int option * int) Queue.t
    ; retval : value option ref
    ; counter : stats_counter ref
    }

  and ival =
    | V of value
    | F of code

  and code = network_state -> int (* switch *) -> ival list -> value

  and memop

  and handler =
    network_state -> int (* switch *) -> int (* port *) -> event -> unit

  type global_fun =
    { cid : Cid.t
    ; body : code
    ; ty : Syntax.ty
          (* Not CoreSyntax.ty, since this is needed only for type inference *)
    }

  let copy_state st =
    { st with
      pipeline = Pipeline.copy st.pipeline
    ; exits = Queue.copy st.exits
    }
  ;;

  let create config : network_state =
    { current_time = -1
    ; config
    ; handlers = Env.empty
    ; switches = Array.of_list []
    ; links = empty_topology 0
    }
  ;;

  let empty_state () =
    { global_env = Env.empty
    ; pipeline = Pipeline.empty ()
    ; event_queue = EventQueue.empty
    ; exits = Queue.create ()
    ; retval = ref None
    ; counter = ref empty_counter
    }
  ;;

  let mem_env swid cid nst = Env.mem cid nst.switches.(swid).global_env

  let lookup swid k nst =
    try Env.find k nst.switches.(swid).global_env with
    | Not_found -> error ("missing variable: " ^ Cid.to_string k)
  ;;

  let lookup_handler cid nst =
    try Some (Env.find cid nst.handlers) with
    | Not_found -> error ("missing handler: " ^ Cid.to_string cid)
  ;;

  let add_global swid cid v nst =
    let st = nst.switches.(swid) in
    if Env.mem cid st.global_env
    then error ("global variable " ^ Cid.to_string cid ^ "  already defined")
    else
      nst.switches.(swid)
        <- { st with global_env = Env.add cid v st.global_env }
  ;;

  let add_global_function (g : global_fun) nst =
    Array.modify
      (fun st ->
        if Env.mem g.cid st.global_env
        then
          error ("global variable " ^ Cid.to_string g.cid ^ "  already defined")
        else { st with global_env = Env.add g.cid (F g.body) st.global_env })
      nst.switches
  ;;

  let add_handler cid lam nst =
    { nst with handlers = Env.add cid lam nst.handlers }
  ;;

  let log_exit swid port event nst =
    Queue.push (event, port, nst.current_time) nst.switches.(swid).exits
  ;;

  let update_counter swid _ nst =
    let st = nst.switches.(swid) in
    let new_counter =
      (* match Env.find event.eid nst.event_sorts with
      | EEntry _ ->
        { pkts_handled = !(st.counter).pkts_handled + 1
        ; total_handled = !(st.counter).total_handled + 1
        }
      | _ -> *)
      { !(st.counter) with total_handled = !(st.counter).total_handled + 1 }
    in
    st.counter := new_counter
  ;;

  let push_event swid port event nst =
    let st = nst.switches.(swid) in
    let t =
      nst.current_time
      + max event.edelay nst.config.generate_delay
      + Random.int nst.config.random_delay_range
    in
    let event_queue = EventQueue.add (t, event, port) st.event_queue in
    nst.switches.(swid) <- { st with event_queue }
  ;;

  (* Like push_event, but doesn't add any artificial delays *)
  let push_input_event swid port event nst =
    let st = nst.switches.(swid) in
    let event_queue =
      EventQueue.add (event.edelay, event, port) st.event_queue
    in
    nst.switches.(swid) <- { st with event_queue }
  ;;

  let next_event swid nst =
    let q = nst.switches.(swid).event_queue in
    if EventQueue.size q = 0
    then None
    else (
      let t, event, port = EventQueue.find_min q in
      if t > nst.current_time
      then None
      else (
        nst.switches.(swid)
          <- { (nst.switches.(swid)) with event_queue = EventQueue.del_min q };
        Some (event, port)))
  ;;

  let next_time_st st =
    let q = st.event_queue in
    if EventQueue.size q = 0
    then None
    else (
      let t, _, _ = EventQueue.find_min q in
      Some t)
  ;;

  let next_time nst =
    Array.fold_left
      (fun acc st ->
        match acc, next_time_st st with
        | None, x | x, None -> x
        | Some x, Some y -> Some (min x y))
      None
      nst.switches
  ;;

  let update_switch swid stage idx getop setop nst =
    Pipeline.update ~stage ~idx ~getop ~setop nst.switches.(swid).pipeline
  ;;

  let update_switch_complex swid stage idx memop nst =
    Pipeline.update_complex ~stage ~idx ~memop nst.switches.(swid).pipeline
  ;;

  (* Maps switch * port -> switch * port according to the topology *)
  let lookup_dst nst (sw, p) =
    match IntMap.find_opt sw nst.links with
    | Some map ->
      (match IntMap.find_opt p map with
      | None -> -1, p
      | Some ret -> ret)
    | None -> error @@ "Invalid switch id " ^ string_of_int sw
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

  let event_queue_to_string q =
    if EventQueue.size q = 0
    then "[ ]"
    else
      Printf.sprintf "[\n%s  ]"
      @@ (q
         |> EventQueue.to_list (* No BatHeap.fold :( *)
         |> List.fold_left
              (fun acc (t, event, port) ->
                Printf.sprintf
                  "%s    %dns: %s at port %d\n"
                  acc
                  t
                  (CorePrinting.event_to_string event)
                  port)
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
               (CorePrinting.event_to_string event)
               (Option.default (-1) port)
               time)
           ""
           s
  ;;

  let stats_counter_to_string counter =
    Printf.sprintf
      (* "\n packet events handled: %d\n total events handled: %d\n"
         counter.pkts_handled *)
      "\n total events handled: %d\n"
      counter.total_handled
  ;;

  let st_to_string
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
      show show_queue "Events" @@ event_queue_to_string st.event_queue
    in
    let exits = show show_exits "Exits" @@ exits_to_string st.exits in
    let stats = stats_counter_to_string !(st.counter) in
    "{\n" ^ vars ^ pipeline ^ queue ^ exits ^ stats ^ "\n}"
  ;;

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

let extract_ival iv =
  match iv with
  | State.V v -> v
  | State.F _ -> failwith "IVal not a regular value"
;;
