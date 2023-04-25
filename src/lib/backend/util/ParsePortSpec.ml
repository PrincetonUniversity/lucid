(* parse the port specification file *)
(* 
  file format:
"recirc_dpid" : 196;
"internal_ports" : {<port dpid>:<port speed in gb/s>, ...};
"external_ports" : {<port dpid>:<port speed in gb/s>, ...};
"port_events" : [[<port dpid : int>,<event name : string>]]
*)

open Core
open Yojson.Basic.Util

exception Error of string
let error s = raise (Error s)


type port = {dpid:int; speed:int;}

let p dpid speed = {dpid; speed}

type port_config = {
  recirc_dpid : int;
  internal_ports : port list;
  external_ports : port list;
  port_events  : (int * string) list;
}

let default_port_config = {
  recirc_dpid = 196;
  internal_ports = [];
  external_ports = [p 128 10; p 129 10; p 130 10; p 131 10];
  port_events = [];
}


let string_of_port p =
  (string_of_int p.dpid)^"@"^(string_of_int p.speed)^"Gb/s"
;;

let string_of_portconfig ps =
  "recirc dpid: "^(string_of_int ps.recirc_dpid)^"\n"
  ^"lucid internal ports: "^((Caml.List.map string_of_port ps.internal_ports) |> Caml.String.concat ", ")^"\n"
  ^"lucid external ports: "^((Caml.List.map string_of_port ps.external_ports) |> Caml.String.concat ", ")^"\n"
  ^"port-event bindings: "^((Caml.List.map (fun (p, e) -> "("^(string_of_int p)^" : "^e^")") ps.port_events) |> Caml.String.concat ", ")
;;

let check recirc_dpid internal_dpids external_dpids (bound_dpids : int list) =
  (* returns false if any integer in bound_dpids 
     is in the list (recirc_dpid::internal_dpids@external_dpids)  *)
  let all_dpids = recirc_dpid :: (internal_dpids @ external_dpids) in
  not (Caml.List.exists (fun dpid -> Caml.List.mem dpid all_dpids) bound_dpids)
;;

let parse fn_opt =
  match fn_opt with 
  | None -> 
    print_endline "------- port configuration -------";
    print_endline (string_of_portconfig default_port_config);
    default_port_config
  | Some(fn) -> (    
    let json = In_channel.read_all fn 
      |> Yojson.Basic.from_string (* parse from string *)
    in

    let parse_port_list js =    
      Caml.List.map (fun (dpid_str, speed_js) -> 
          let port = int_of_string dpid_str in
          let speed = speed_js |> to_int in
          p port speed)
        (to_assoc js)
    in

    let parse_port_events (port_events_json : Yojson.Basic.t) : (int * string) list =
      let port_bindings = port_events_json |> to_list in
      Caml.List.map (function
        | `List [`Int port; `String event] -> (port, event)
        | _ -> failwith "Invalid port binding format")
      port_bindings
    in
    let port_events =
      try
        member "port_events" json |> parse_port_events
      with
      | Yojson.Json_error _ | Type_error _ -> []
    in

    let recirc_dpid =
      try
        member "recirc_dpid" json |> to_int
      with
      | Yojson.Json_error _ | Type_error _ -> 0
    in
    let internal_ports = parse_port_list (member "internal_ports" json) in
    let external_ports = parse_port_list (member "external_ports" json) in
    let port_to_dpid p = p.dpid in
    let ps_to_dpids ps = Caml.List.map port_to_dpid ps in
    let valid_conf = check 
      recirc_dpid 
      (ps_to_dpids internal_ports) 
      (ps_to_dpids external_ports) 
      (Caml.List.map fst port_events) 
    in
    (if (not (valid_conf))
    then (error "port configuration is not valid. Please make sure that all ports bound to events are not declared as recirc, internal, or external ports.")
  );

    let res = {recirc_dpid;internal_ports;external_ports; port_events} in
    res
  )
;;