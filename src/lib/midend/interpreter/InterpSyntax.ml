(* syntax extensions for the interpreter, mainly related 
   to the input stream, which consists of 
   events located at specific ports in the network 
   and interpreter control commands *)

open CoreSyntax
open Yojson.Basic


(* interpreter state events *)
type control_e = 
  | ArraySet of string * int * (int list)
  | ArraySetRange of string * int * int * (int list)
  | ArrayGet of string * int
  | ArrayGetRange of string * int * int
  | Print of string
  | Noop

type control_event = {
  ctl_cmd : control_e;
  ctl_edelay : int;
}

type interp_event =
  | IEvent of event
  | IControl of control_event

(* event queued for execution at a switch *)
type switch_event = {
  sevent : interp_event;
  stime : int;
  sport : int;
}

type loc = {
  switch : int;
  port : int;
}
(* input to the interpreter from the user *)
type located_event = {
  ievent : interp_event;
  ilocs : loc list;
}

(* constructors *)
let loc (switch, port) = {switch; port}
let ievent ev = IEvent(ev)
let ilocated_event (ev, locs) =
  {ievent = IEvent(ev); ilocs = List.map loc locs}
;;
let ilocated_control (ctl_ev, locs) = 
  {ievent = IControl(ctl_ev); ilocs = List.map loc locs}
;;

let assoc_to_vals keys lst =
  List.map
    (fun aname -> List.assoc aname lst)
    keys
;;    
let json_to_intval v =
  match v with 
    | `Int v -> v
    | `String v -> int_of_string v        
    | _ -> error "[json_to_intval] got a value that is not an int or string"
;;

let parse_array_set lst = 
  let arglist = match (List.assoc "args" lst) with
    | `List lst -> lst
    | `Assoc lst -> assoc_to_vals ["array"; "index"; "value"] lst
    | _ -> error "err"
  in
  match arglist with
    | [`String a; idx; `List vs] -> 
      ArraySet(a, json_to_intval idx, List.map json_to_intval vs)
    | _ -> error "[parse_array_set] args must be an assoc or list"
;;

let parse_array_setrange lst = 
  let arglist = match (List.assoc "args" lst) with
    | `List lst -> lst
    | `Assoc lst -> assoc_to_vals ["array"; "start"; "end"; "value"] lst
    | _ -> error "err"
  in
  match arglist with
    | [`String a; s; e; `List vs] -> 
      ArraySetRange(a, json_to_intval s, json_to_intval e, List.map json_to_intval vs)
    | _ -> error "[parse_array_set] args must be an assoc or list"

let parse_array_get lst =
  let arglist = match (List.assoc "args" lst) with
    | `List lst -> lst
    | `Assoc lst -> assoc_to_vals ["array"; "index"] lst
    | _ -> error "[parse_array_get] args must be an assoc or list"
  in
  match arglist with
    | [`String a; v] -> ArrayGet(a, json_to_intval v)
    | _ -> error "[malformed array.get args]"
;;
let parse_array_getrange lst =
  let arglist = match (List.assoc "args" lst) with
    | `List lst -> lst
    | `Assoc lst -> assoc_to_vals ["array"; "start"; "end"] lst
    | _ -> error "[parse_array_getrange] args must be an assoc or list"
  in
  match arglist with
    | [`String a; s; e] -> ArrayGetRange(a, json_to_intval s, json_to_intval e)
    | _ -> error "[malformed array.get args]"
;;

let parse_control_e lst =
  match (List.assoc "name" lst) with
    | (`String "Array.set") -> parse_array_set lst      
    | (`String "Array.setrange") -> parse_array_setrange lst      
    | (`String "Array.get") -> parse_array_get lst
    | (`String "Array.getrange") -> parse_array_getrange lst
    | _ -> error "unknown control command"
    (* | None -> error "control command has no name field" *)
;;
(* printing *)
let control_event_to_string control_e =
  match control_e with
  | ArraySet(arrname, idx, newvals) -> 
    Printf.sprintf 
      "Array.set(%s, %i, %s);" 
      arrname 
      idx 
      (Printf.sprintf "[%s]" (List.map string_of_int newvals |> String.concat ","))
  | ArraySetRange(arrname, s, e, newvals) -> 
    Printf.sprintf 
      "Array.setrange(%s, %i, %i, %s);" 
      arrname 
      s
      e 
      (Printf.sprintf "[%s]" (List.map string_of_int newvals |> String.concat ","))
  | ArrayGet(arrname, idx) -> 
    Printf.sprintf "Array.get(%s, %i);" arrname idx
  | _ -> "<control event without printer>"
;;

(* destructors *)
let loc_to_tup loc = (loc.switch, loc.port)
let locs_to_tups = List.map loc_to_tup

let interp_event_delay (iev : interp_event) = match iev with
  | IEvent(ev) -> ev.edelay
  | IControl(ev) -> ev.ctl_edelay
;;

let located_event_delay (lev:located_event) = 
  interp_event_delay lev.ievent
;;
let interp_event_to_string ievent =
  match ievent with
  | IEvent(e) -> CorePrinting.event_to_string e
  | IControl(_) -> "(Control event (printer not implemented))"
;;