(* parse the port specification file *)
(* 
  file format:
"recirc_dpid" : 196;
"internal_ports" : {<port dpid>:<port speed in gb/s>, ...};
"external_ports" : {<port dpid>:<port speed in gb/s>, ...};
*)

open Core
open Yojson.Basic.Util


type port = {dpid:int; speed:int;}

let p dpid speed = {dpid; speed}

type port_config = {
  recirc_dpid : int;
  internal_ports : port list;
  external_ports : port list;
}

let default_port_config = {
  recirc_dpid = 196;
  internal_ports = [];
  external_ports = [p 128 10; p 129 10; p 130 10; p 131 10];
}


let string_of_port p =
  (string_of_int p.dpid)^"@"^(string_of_int p.speed)^"Gb/s"
;;

let string_of_portconfig ps =
  "recirc dpid: "^(string_of_int ps.recirc_dpid)^"\n"
  ^"lucid internal ports: "^((Caml.List.map string_of_port ps.internal_ports) |> Caml.String.concat ", ")^"\n"
  ^"lucid external ports: "^((Caml.List.map string_of_port ps.external_ports) |> Caml.String.concat ", ")
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

    let recirc_dpid = member "recirc_dpid" json |> to_int in 
    let internal_ports = parse_port_list (member "internal_ports" json) in
    let external_ports = parse_port_list (member "external_ports" json) in
    let res = {recirc_dpid;internal_ports;external_ports} in
    res
  )
;;