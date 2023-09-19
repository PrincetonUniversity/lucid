(* models a single switch in the interpreter *)

open CoreSyntax
open InterpSyntax
open Batteries
module Env = Collections.CidMap
module IntMap = Map.Make (Int)


(* input queue for a single switch *)
module EventQueue = BatHeap.Make (struct
  (* time, event, port *)
  type t = switch_event

  let compare t1 t2 = Pervasives.compare t1.stime t2.stime

  (* type t = int * event * int *)
  (* let compare (t1, _, _) (t2, _, _) = Pervasives.compare t1 t2 *)
end)

(* stats counter for a switch *)
type stats_counter =
{ entries_handled : int
; total_handled : int
}

type 'nst state = 
  { global_env : 'nst InterpSyntax.ival Env.t
  ; event_queue : EventQueue.t
  ; pipeline : Pipeline.t
  ; exits : (InterpSyntax.interp_event * int option * int) Queue.t
  ; drops : (interp_event * int) Queue.t
  ; retval : value option ref
  ; counter : stats_counter ref
  }


let empty_counter = { entries_handled = 0; total_handled = 0 }

let copy_state st =
  { st with
    pipeline = Pipeline.copy st.pipeline
  ; exits = Queue.copy st.exits
  }
;;

let empty_state () =
  { global_env = Env.empty
  ; pipeline = Pipeline.empty ()
  ; event_queue = EventQueue.empty
  ; exits = Queue.create ()
  ; drops = Queue.create ()
  ; retval = ref None
  ; counter = ref empty_counter
  }
;;

let mem_env cid state = Env.mem cid state.global_env
let lookup k state = 
  try Env.find k state.global_env with
  | Not_found -> error ("missing variable: " ^ Cid.to_string k)


let add_global cid v st =
  if Env.mem cid st.global_env
  then error ("global variable " ^ Cid.to_string cid ^ "  already defined")
  else
    { st with global_env = Env.add cid v st.global_env }
;;

let log_exit st port event current_time = 
  Queue.push (event, port, current_time) st.exits
;;
let log_drop st event current_time = 
  Queue.push (event, current_time) st.drops
;;

let update_counter st event_sort = 
  let new_counter = match event_sort with
  | EPacket -> 
    {entries_handled = !(st.counter).entries_handled + 1;
     total_handled = !(st.counter).total_handled + 1}
  | _ ->
    {!(st.counter) with total_handled = !(st.counter).total_handled + 1}
  in
  st.counter := new_counter
;;

let push_event st sevent stime sport =
  {st with event_queue=EventQueue.add {sevent; stime; sport} st.event_queue}
;;

let next_event st current_time = 
  let q = st.event_queue in
  if EventQueue.size q = 0
  then None
  else (
    let switch_ev = EventQueue.find_min q in
    let t, event, port = switch_ev.stime, switch_ev.sevent, switch_ev.sport in
    if t > current_time
    then None
    else (
      let q = EventQueue.del_min q in
      Some ({st with event_queue = q;}, event, port)
    )
  )
;;

let next_time st = 
  let q = st.event_queue in
  if EventQueue.size q = 0
  then None
  else (
    let { stime = t; _ } = EventQueue.find_min q in
    Some t)
;;

let get_table_entries st stage =
  Pipeline.get_table_entries ~stage st.pipeline
;;

let install_table_entry st stage entry =
  Pipeline.install_table_entry ~stage ~entry st.pipeline
;;

let update st stage idx getop setop =
  Pipeline.update ~stage ~idx ~getop ~setop st.pipeline
;;

let update_complex st stage idx memop =
  Pipeline.update_complex ~stage ~idx ~memop st.pipeline
;;



let stats_counter_to_string counter =
  Printf.sprintf
    "\n packet events handled: %d\n total events handled: %d\n"
    counter.entries_handled
    counter.total_handled
;;

let event_queue_to_string q =
  if EventQueue.size q = 0
  then "[ ]"
  else
    Printf.sprintf "[\n%s  ]"
    @@ (q
       |> EventQueue.to_list (* No BatHeap.fold :( *)
       |> List.fold_left
            (fun acc { stime = t; sevent = event; sport = port } ->
              Printf.sprintf
                "%s    %dns: %s at port %d\n"
                acc
                t
                (interp_event_to_string event)
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
             (InterpSyntax.interp_event_to_string event)
             (Option.default (-1) port)
             time)
         ""
         s
;;

let drops_to_string s =
  if Queue.is_empty s
  then "[ ]"
  else
    Printf.sprintf "[\n%s  ]"
    @@ Queue.fold
         (fun acc (event, time) ->
           Printf.sprintf
             "%s    %s, t=%d\n"
             acc
             (InterpSyntax.interp_event_to_string event)
             time)
         ""
         s
;;

let env_to_string ival_to_string env =
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

let to_string
  ival_to_string
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
let vars = show show_vars "Env" @@ env_to_string ival_to_string st.global_env in
let pipeline =
  show show_pipeline "Pipeline" @@ Pipeline.to_string ~pad:"  " st.pipeline
in
let queue =
  show show_queue "Events" @@ event_queue_to_string st.event_queue
in
let exits = show show_exits "Exits" @@ exits_to_string st.exits in
let drops = show show_exits "Drops" @@ drops_to_string st.drops in
let stats = stats_counter_to_string !(st.counter) in
"{\n" ^ vars ^ pipeline ^ queue ^ exits ^ drops ^ stats ^ "\n}"
;;