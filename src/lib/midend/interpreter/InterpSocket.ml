(* socket IO for the interpreter *)
(* Model after interpStdio.ml *)


open Rawlink
open Cstruct

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
(* create an interface from a name *)
let create switch port ifname : t = 
  let link = Rawlink.open_link ~promisc:true ifname in  
  { switch; port; ifname; link }
;;

(* IO operations*)
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

(* read an input event, blocking when block = true *)
let read_event_opt (self :t) block current_time = 
  (* timestamp passed from interpreter loop *)
  let timestamp = current_time in
  (* location defined by link *)
  let locations = [{switch=Some(self.switch); port=self.port}] in
  (* blocking: just read a buf and cast to bytes *)
  if (block) then (
    let buf = read self in
    Some(event_create timestamp locations buf)
  ) else ( (* non blocking *)
    match read_nonblock self with 
    | None -> None 
    | Some(buf) -> Some(event_create timestamp locations buf)
  )
;;  

(* send an event value *)
let send_event t ev = send t (event_to_packetbuf ev) ;;

let test_rawlink ifname =
  let intf = create 0 0 ifname in 
  let buf = read intf in
  
  let first = Cstruct.get_uint8 buf 0 in
  Cstruct.set_uint8 buf 0 ((first + 1) mod 256);

  Printf.printf "Got a packet with %d bytes, echoing...%!\n" (buf.len);
  send intf buf;
  ()
;;