(* socket IO for the interpreter *)
(* Model after interpStdio.ml *)


open Rawlink
open Cstruct
open Yojson.Basic
open InterpSyntax
open CoreSyntax
open InterpJson
open InterpDeparsing


type t = {
  switch : int;     (* switch id *)
  port   : int;     (* port id *)
  ifname : string ; (* interface name *)
  link : Rawlink.t  (* FD wrapper *)
}

let create switch port ifname : t =
  let link = Rawlink.open_link ~promisc:true ifname in
  (* drain any packets queued between socket() and bind() in rawlink,
     since AF_PACKET with ETH_P_ALL captures from all interfaces
     until bind() restricts it to the target interface *)
  ignore (Rawlink.drain_buffered link);
  let fd = Rawlink.get_fd link in
  let rec drain () =
    let ready, _, _ = Unix.select [fd] [] [] 0.0 in
    match ready with
    | [] -> ()
    | _ -> ignore (Rawlink.read_packet link); drain ()
  in
  drain ();
  { switch; port; ifname; link }
;;
(* IO operations *)
let send t buf = Rawlink.send_packet t.link buf ;;
let read t = Rawlink.read_packet t.link ;;
let read_nonblock self =
    if Rawlink.has_buffered_packet self.link then (
      (* packet already buffered, read won't block *)
      Some(read self)
    )
    else (
      (* check if fd is ready with 0 timeout (non-blocking poll) *)
      let fd = Rawlink.get_fd self.link in
      let ready, _, _ = Unix.select [fd] [] [] 0.0 in
      match ready with
      | [] -> None  (* no packet available *)
      | _ ->  Some(read self)
    )
;;

let read_batch_nonblock self =
  let fd = Rawlink.get_fd self.link in
  let ready, _, _ = Unix.select [fd] [] [] 0.0 in
  match ready with
  | [] -> []
  | _ ->
    let first = Rawlink.read_packet self.link in
    let rest = Rawlink.drain_buffered self.link in
    first :: rest
;;


(* create an event from timestamp, location, and buf *)
let event_create timestamp locations buf =
  let bytes = hexstr_to_vbits (Cstruct.to_hex_string buf) in
  let pkt_event = packet_event bytes 0 in
  let ev = ievent pkt_event locations timestamp in
  ev
;;

(* create a packet buffer from a CoreSyntax event value *)
let event_to_packetbuf (ev : event_val) = 
  let serialized_event = InterpDeparsing.serialize_event_eth ev in 
  let vbits = 
    match serialized_event.data with 
    | [vbits] -> extract_bits vbits
    | _ -> error "[InterpSocket.ml] Interpreter fault: event serialized wrong"
  in
  let hex_str = BitString.bits_to_hexstr vbits |> Cstruct.of_hex in
  hex_str
;;


(*** Public API ***)
(* read a batch of event values wrapped in an interp_input, nonblocking *)
let read_event_batch (self : t) : interp_input list = 
  let timestamp = Int64.to_int (Int64.of_float (Unix.gettimeofday () *. 1e9)) in
  let locations = [{switch=Some(self.switch); port=self.port}] in
  let bufs = read_batch_nonblock self in
  List.map (fun buf -> event_create timestamp locations buf) bufs
;;


(* send an event value *)
let send_event self ev = send self (event_to_packetbuf ev) ;;

(* experimental: send an event to stdout as a json *)
let send_event_to_stdio_as_json (self : t) (ev : event_val) = 
  let base_event = event_to_json ev in  
  let locs = `List [`String (Printf.sprintf "%i:%i" self.switch self.port)] in
  let evjson = `Assoc(base_event@["locations", locs]) in
  Yojson.Basic.to_string evjson
;;


(* 
let test_rawlink ifname =
  let intf = create 0 0 ifname in 
  let buf = read intf in
  
  let first = Cstruct.get_uint8 buf 0 in
  Cstruct.set_uint8 buf 0 ((first + 1) mod 256);

  Printf.printf "Got a packet with %d bytes, echoing...%!\n" (buf.len);
  send intf buf;
  ()
;; *)