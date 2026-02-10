(* Simple port specifications for the tofino. 
   Each port has a "speed"; config is a list of ports
   and the id of the recirculation port. *)
open Core
open Yojson.Basic.Util

exception Error of string
let error s = raise (Error s)

type port = {dpid:int; speed:int;}

let p dpid speed = {dpid; speed}

type port_config = {
  recirc_dpid : int;
  external_ports : port list;
}

let string_of_port p =
  (string_of_int p.dpid)^"@"^(string_of_int p.speed)^"Gb/s"
;;

let string_of_portconfig ps =
  "recirc dpid: "^(string_of_int ps.recirc_dpid)^"\n"
  ^"switch ports: "^((Caml.List.map string_of_port ps.external_ports) |> Caml.String.concat ", ")^"\n"
;;
let create front_panel_ports recirc_dpid = 
  let external_ports = Caml.List.map (fun (dpid, speed) -> p dpid speed) front_panel_ports in
  {recirc_dpid; external_ports;}
;;

(* json format:
"recirc_dpid" : 196;
"external_ports" : {<port dpid>:<port speed in gb/s>, ...}; *)
let parse fn =
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
  let recirc_dpid =
    try
      member "recirc_dpid" json |> to_int
    with
    | Yojson.Json_error _ | Type_error _ -> 0
  in
  let external_ports = parse_port_list (member "external_ports" json) in

  let res = {recirc_dpid;external_ports;} in
  res  
;;