(* syntax extensions for the interpreter, mainly related 
   to the input stream, which consists of 
   events located at specific ports in the network 
   and interpreter control commands *)
open CoreSyntax
open Yojson.Basic
open Str

(* control events are interpreter builtins -- state update commands *)
type control_e = 
  | ArraySet of string * value * (value list)
  | ArraySetRange of string * value * value * (value list)
  | ArrayGet of string * value
  | ArrayGetRange of string * value * value
  | TableInstall of string * tbl_entry
  | Print of string
  | Noop

type control_event = {
  ctl_cmd : control_e;
  ctl_edelay : int;
}

(* a packet event is an unparsed event, 
   which gets transformed into an event_val 
   by a parser. *)

(* packet values are used in other places, e.g., 
   ivals in InterpState. *)
type packet_val = {
  pbuf : string;  (* hex string *)
  plen : int; (* length of buf *)
  ppos : int; (* current offset in buf *)
}

let packet_val str = {
  pbuf = str;
  plen = String.length str;
  ppos = 0;
}

type packet_event = {
  pkt_val : packet_val;
  pkt_edelay : int;
}

let packet_event pkt_val pkt_edelay = 
  {pkt_val; pkt_edelay}
;;

type interp_event =
  | IEvent of event_val
  | IControl of control_event
  | IPacket of packet_event

let ievent ev = IEvent(ev)
let icontrol ev = IControl(ev)
let ipacket ev = IPacket(ev)

type loc = {
  switch : int;
  port : int;
}
let loc (switch, port) = {switch; port}

(* a located event represents an event on input, 
   after parsing but before queueing *)
type located_event = {
  ievent : interp_event;
  ilocs : loc list;
}

let located_event (ev, locs) =
  {ievent = IEvent(ev); ilocs = List.map loc locs}
;;
let located_control (ctl_ev, locs) = 
  {ievent = IControl(ctl_ev); ilocs = List.map loc locs}
;;
let located_packet (pkt_ev, locs) = 
  {ievent = IPacket(pkt_ev); ilocs = List.map loc locs}
;;


(* a switch_event is how we represent the event in a queue at a switch *)
type switch_event = {
  sevent : interp_event;
  stime : int;
  sport : int;
}



(* constructors *)

let assoc_to_vals keys lst =
  List.map
    (fun aname -> List.assoc aname lst)
    keys
;;    

(*** parsing json event inputs ***)

module JP = struct
  let to_size v =
    match v with 
      | `Int v -> v
      | `String v -> int_of_string v        
      | _ -> error "[json_to_intval] got a value that is not an int or string"
  ;;

  (* json parsing module *)
  let to_vint v = 
    match v with 
      (* 32-bit int *)
      | `Int v -> vint v 32
      | `String vstr -> (
        match vstr with
        | "true" -> vbool true
        | "false" -> vbool false
        | _ -> (
        (* x<<size>> *)
        let v, sz = if (String.contains vstr '<')
          then (
            Scanf.sscanf vstr "%d<<%d>>" (fun x y -> x,y))
          else (
            Scanf.sscanf vstr "%d" (fun x -> x, 32)
          )
        in
        vint v sz
        )
      )
        | _ -> error "[to_vint] got a value that is not an int or string"
  ;;
  let to_eval_int v = v |> to_vint |> value_to_exp ;;



  (* parse string to value. Value may have optional size. 
     Value may be a wildcard for table pattern (_). *)
  let string_to_val_components str =
    let re_val_size = Str.regexp "\\(.*\\)<<\\(.*\\)>>" in
    let re_val = Str.regexp "\\(.*\\)" in
    try 
      let _ = Str.search_forward re_val_size str 0 in
      (Str.matched_group 1 str, Str.matched_group 2 str)
    with _ -> (
      try 
        let _ = Str.search_forward re_val str 0 in
        Str.matched_group 1 str, "32"
      with _ -> (
        error "invalid form for pattern value string"
      )
    )
  ;;
end

let parse_array_set lst = 
  let arglist = match (List.assoc "args" lst) with
    | `List lst -> lst
    | `Assoc lst -> assoc_to_vals ["array"; "index"; "value"] lst
    | _ -> error "err"
  in
  match arglist with
    | [`String a; idx; `List vs] -> 
      ArraySet(a, JP.to_vint idx, List.map JP.to_vint vs)
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
      ArraySetRange(a, JP.to_vint s, JP.to_vint e, List.map JP.to_vint vs)
    | _ -> error "[parse_array_set] args must be an assoc or list"

let parse_array_get lst =
  let arglist = match (List.assoc "args" lst) with
    | `List lst -> lst
    | `Assoc lst -> assoc_to_vals ["array"; "index"] lst
    | _ -> error "[parse_array_get] args must be an assoc or list"
  in
  match arglist with
    | [`String a; v] -> ArrayGet(a, JP.to_vint v)
    | _ -> error "[malformed array.get args]"
;;
let parse_array_getrange lst =
  let arglist = match (List.assoc "args" lst) with
    | `List lst -> lst
    | `Assoc lst -> assoc_to_vals ["array"; "start"; "end"] lst
    | _ -> error "[parse_array_getrange] args must be an assoc or list"
  in
  match arglist with
    | [`String a; s; e] -> ArrayGetRange(a, JP.to_vint s, JP.to_vint e)
    | _ -> error "[malformed array.get args]"
;;

let parse_pat patjson =
  match patjson with
  | `String s -> (
    match Str.split (Str.regexp "&&&") s with
    (* no mask, just base *)
    | [b] -> (
      let v, sz = JP.string_to_val_components b in
      let v, sz = String.trim v, String.trim sz in 
      let sz = int_of_string sz in
      match v with
        | "_" -> vwild sz
        | vint -> vpat (int_to_bitpat (int_of_string vint) sz))
    (* base and mask. Note: use size from base. *)
    | [b; m] -> (
      let v, sz = JP.string_to_val_components b in
      let m, _  = JP.string_to_val_components m in
      let v, sz, m = String.trim v, String.trim sz, String.trim m in 
      vpat (int_mask_to_bitpat (int_of_string v) (int_of_string m) (int_of_string sz))      
    )
    (* something else -- fail. *)
    | _ -> error "malformed pattern string";
  )
  (* int -- assume 32-bit *)
  | `Int i -> vpat (int_to_bitpat i 32)
  | _ -> error "not a pattern string"
;;

let parse_entry entrylst =
  let key = match (List.assoc "key" entrylst) with
    | `List patlst -> List.map parse_pat patlst |> List.map value_to_exp
    | _ -> error "[parse_entry] key is in wrong format"
  in
  let action = match (List.assoc "action" entrylst) with
    | `String acn_name -> 
      Id.create acn_name
    | _ -> error "[parse_entry] action name is wrong format"
  in
  let priority = match (List.assoc_opt "priority" entrylst) with
    | Some(v) -> JP.to_size v
    | _ -> 0
  in
  let action_args = match (List.assoc "args" entrylst) with
    | `List argslst -> 
      List.map JP.to_eval_int argslst
    | _ -> error "[parse_entry] action args are in wrong format"
  in
  {
    eprio= priority;
    ematch=key;
    eaction=action;
    eargs = action_args;
  }
;;

let parse_str_field lst field = match (List.assoc field lst) with
  | `String s -> s
  | _ -> error "[parse_str_field] not a string value"
;;

(*
  Table.install format: 
  {"type":"command", "name": "Table.install", 
    "args":{"table":"mytbl", "pattern":["4<<32>> &&& 3<<32>>", "_<<32>>"], "action":"hit_acn", "args":[3], "priority":1}    
    }*)
let parse_table_install lst =
  let tblname, entry = match (List.assoc "args" lst) with
    | `Assoc entrylst -> 
      parse_str_field entrylst "table", parse_entry entrylst
    | _ -> error "[parse_table_install] args is wrong type"
  in
  TableInstall(tblname, entry)
;;

let parse_control_e lst =
  match (List.assoc "name" lst) with
    | (`String "Array.set") -> parse_array_set lst      
    | (`String "Array.setrange") -> parse_array_setrange lst      
    | (`String "Array.get") -> parse_array_get lst
    | (`String "Array.getrange") -> parse_array_getrange lst
    | (`String "Table.install") -> parse_table_install lst
    | _ -> error "unknown control command"
    (* | None -> error "control command has no name field" *)
;;


(*** event parsing (plus helpers ripped out of InterpSpec)***)
(*** these are parsing helper functions that have been ripped out
    of InterpSpec.ml and butchered to remove CiRcULar DePENdencIeS!1!! *)


let rename env err_str id_str =
  match Collections.CidMap.find_opt (Cid.from_string id_str) env with
  | Some id -> id
  | None -> error @@ Printf.sprintf "Unknown %s %s" err_str id_str
;;
    

let parse_port str =
  match String.split_on_char ':' str with
  | [id; port] ->
    (try int_of_string id, int_of_string port with
     | _ -> error "Incorrect format for link entry!")
  | _ -> error "Incorrect format for link entry!"
;;

let parse_int err_str (j) =
  match j with
  | `Int n -> n
  | _ -> error @@ "Non-integer value for " ^ err_str
;;

let rec parse_value payloads_t_id err_str ty j =
  match j, ty.raw_ty with
  | `Int n, TInt size -> vint n size
  | `Int n, TName (cid, _, _) when Cid.equal cid payloads_t_id -> vint n 32
  | `Bool b, TBool -> vbool b
  | `List lst, TGroup ->
    vgroup (List.map (fun n -> parse_int "group value definition" n) lst)
  | _ ->
    error
    @@ err_str
    ^ " specification had wrong or unexpected argument "
    ^ to_string j
;;


let parse_locations default_port num_switches lst =
  let locations =
    match List.assoc_opt "locations" lst with
    | Some (`List lst) ->
      List.map
        (function
         | `String str ->
           let sw, port = parse_port str in
           if sw < 0 || sw >= num_switches
           then
             error
             @@ "Cannot specify event at nonexistent switch "
             ^ string_of_int sw;
           if port < 0 || port >= 255
           then
             error
             @@ "Cannot specify event at nonexistent port "
             ^ string_of_int port;
           sw, port
         | _ -> error "Event specification had non-string location")
        lst
    | None -> [0, default_port]
    | _ -> error "Event specification has non-list locations field"
  in
  locations
;;

(* control locations are just a switch number, no port *)
let parse_control_locations num_switches lst = 
  let locations =
    match List.assoc_opt "locations" lst with
    | Some (`List lst) ->
      List.map
        (function
        | `Int sw ->
          if sw < 0 || sw >= num_switches
          then
            error
            @@ "Cannot specify control command at nonexistent switch "
            ^ string_of_int sw;
          sw
        | _ -> error "Control command specification had non-int location")
        lst
    | None -> [0]
    | _ -> error "control command specification has non-list locations field"
  in
  locations
;;


let parse_delay cur_ts lst =
  match List.assoc_opt "timestamp" lst with
  | Some (`Int n) -> n
  | None -> cur_ts
  | _ -> error "Event specification had non-integer delay field"
;;



(* get the name of the event, given a renaming function that I am not allowed to know. *)
let get_eid var_map lst =
  match List.assoc_opt "name" lst with
  | Some (`String id) -> rename var_map "event" id
  | None -> error "Event specification missing name field"
  | _ -> error "Event specification had non-string type for name field"
;;

(* get event data  *)
let get_data (payloads_t_id, var_map) event_json events = 
  let eid = get_eid var_map event_json in
  let data =
    match List.assoc_opt "args" event_json with
    | Some (`List lst) -> 
      (* let _, tys = InterpSyntax.get_event_tys eid pp.events in *)
      let _, tys = Collections.CidMap.find eid events in
      (try List.map2 (parse_value payloads_t_id "Event") tys lst with
      | Invalid_argument _ ->
        error
        @@ Printf.sprintf
             "Event specification for %s had wrong number of arguments"
             (Cid.to_string eid))
    | None -> error "Event specification missing args field"
    | _ -> error "Event specification had non-list type for args field"
  in
  data
;;

(* outermost parser... *)
    (* Find the event name, accounting for the renaming pass, and get its
       sort and argument types *)
    (* Determine if the record is an event or a control command.
       "type" == "command" -> command
       "type" == "event" -> event
       - if there is no type, it is an event *)
    (* Parse the arguments into values, and make sure they have the right types.
       At the moment only integer and boolean arguments are supported *)

let parse_located_event 
  payloads_t_id
  var_map
  events
  num_switches
  cur_ts
  default_port
  event : located_event = 
  match event with
  | `Assoc event_json ->
    (match List.assoc_opt "type" event_json with
     | Some (`String "event")
     | None -> 
      (* located user event event *)
      let eid = get_eid var_map event_json in
      let data = get_data (payloads_t_id, var_map) event_json events in
      let edelay = parse_delay cur_ts event_json in
      let locations = parse_locations default_port num_switches event_json in
      located_event ({ eid; data; edelay }, locations)
  
     | Some (`String "command") -> 
      (* located control/command event *)
      let edelay = parse_delay cur_ts event_json in
      let locations = parse_control_locations num_switches event_json in
      let control_event =
        { ctl_cmd = parse_control_e event_json; ctl_edelay = edelay }
      in
      located_control (control_event, List.map (fun sw -> sw, 0) locations)
     | Some (`String "packet") -> 
       let pkt_edelay = parse_delay cur_ts event_json in
       let locations = parse_locations default_port num_switches event_json in
       let pkt_bytes = match List.assoc_opt "bytes" event_json with
        | Some (`String s) -> s
        | _ -> error "packet event must have a bytes field"
       in
       let pkt_event = packet_event
          (packet_val pkt_bytes)
          pkt_edelay
       in
       located_packet (pkt_event, locations)
     | Some _ ->
       error "unknown interpreter input type (expected event or command)"
    )
  | _ -> error "Non-assoc type for interpreter input"
;;

(* printing *)
let control_event_to_string control_e =
  match control_e with
  | ArraySet(arrname, idx, newvals) -> 
    Printf.sprintf 
      "Array.set(%s, %s, %s);" 
      arrname 
      (CorePrinting.value_to_string idx) 
      (Printf.sprintf "[%s]" (List.map CorePrinting.value_to_string newvals |> String.concat ","))
  | ArraySetRange(arrname, s, e, newvals) -> 
    Printf.sprintf 
      "Array.setrange(%s, %s, %s, %s);" 
      arrname 
      (CorePrinting.value_to_string s)
      (CorePrinting.value_to_string e) 
      (Printf.sprintf "[%s]" (List.map CorePrinting.value_to_string newvals |> String.concat ","))
  | ArrayGet(arrname, idx) -> 
    Printf.sprintf "Array.get(%s, %s);" arrname (CorePrinting.value_to_string idx)
  | _ -> "<control event without printer>"
;;

let packet_val_to_string packet_val = packet_val.pbuf
;;


let packet_event_to_string packet_event = 
  packet_val_to_string (packet_event.pkt_val)
;;

(* destructors *)
let loc_to_tup loc = (loc.switch, loc.port)
let locs_to_tups = List.map loc_to_tup

let interp_event_delay (iev : interp_event) = match iev with
  | IEvent(ev) -> ev.edelay
  | IControl(ev) -> ev.ctl_edelay
  | IPacket(ev) -> ev.pkt_edelay
;;

let located_event_delay (lev:located_event) = 
  interp_event_delay lev.ievent
;;
let interp_event_to_string ievent =
  match ievent with
  | IEvent(e) -> CorePrinting.event_to_string e
  | IControl(_) -> "(Control event (printer not implemented))"
  | IPacket(_) -> "(Packet event (printer not implemented))"
;;