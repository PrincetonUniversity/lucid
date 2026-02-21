(* Network-wide state in the interpreter. *)
(* 
  The interpreter's state is a mutable array of switches, 
  plus a current time.
  Each switch has a pipeline with mutable local state in it. 
  The switches have queues, but they are currently immutable, 
  and so to update a queue, the switch must call a helper 
  from here (InterpState) that updates the queue.
  We may want to refactor the switch objects to have 
  mutable queues, to simplify the architecture.
  - But the switch will still need to get references to 
    other switches from here.
*)
open CoreSyntax
open InterpSyntax
open Batteries
module Env = Collections.CidMap

open InterpSwitch
open InterpSim


type network_state =
  { current_time : int
  ; switches : switch array
  }

and handler = network_state InterpSyntax.handler
and switch = network_state InterpSwitch.state

type global_fun =
  { cid : Cid.t
  ; body : network_state InterpSyntax.code
  ; ty : Syntax.ty
  }

let create () : network_state =
  { current_time = -1
  ; switches = Array.of_list []
  }
;;


(* switch wrappers *)

let sw nst swid = nst.switches.(swid)

let pipe nst swid = nst.switches.(swid).pipeline

(* environment operations *)
let mem_env swid cid nst = InterpSwitch.mem_env cid nst.switches.(swid)
let lookup swid k nst = InterpSwitch.lookup k nst.switches.(swid)

(* update the state of a switch. Used for queues.
   Provided to switches. *)
let save_update nst sw = nst.switches.(sw.swid) <- sw;;

let lookup_switch nst swid = nst.switches.(swid);;

let current_time nst = nst.current_time;;

let network_utils = { save_update; lookup_switch; get_time=current_time;}
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

let update_counter swid event nst =
  let st = nst.switches.(swid) in
  let event_sort = Env.find event.eid nst.switches.(swid).event_sorts in
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
  if InterpConfig.cfg.json || InterpConfig.cfg.interactive
    then InterpJson.interp_report_json "final_state" base_str None
    else base_str
;;

let gfun_cid (gf : global_fun) : Cid.t = 
  gf.cid
;;
