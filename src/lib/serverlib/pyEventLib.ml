(* event parsers in python *)
open Batteries
open Syntax
open SyntaxUtils
open Printing

(* input type for many printers *)
type eventsig = {
  id : id;
  num : int;
  params : (id * CoreSyntax.ty) list;
}
type eventsigs = eventsig list
let null_event = {id=Id.fresh "NONE"; num=0; params=[]}

(* printing helpers *)




let evrec_to_string evrec =
  Printf.sprintf "event %s(%s);" 
    (CorePrinting.id_to_string evrec.id)
    (CorePrinting.params_to_string evrec.params)
;;


let sp n = String.make n ' '
let ln_sep ls = String.concat "\n" ls
let comma_sep f ls = List.map f ls |> String.concat ", "
let line_sep f ls =
  List.map f ls |> String.concat "\n"
let sp_sep f ls = 
  List.map f ls |> String.concat " "

let indent n str = 
  String.split_on_char '\n' str 
  |> List.map (fun s -> (sp n)^s)
  |> String.concat "\n"
;;

let indenttl n str = 
  match (String.split_on_char '\n' str) with 
  | [] -> ""
  | hd::tl -> hd::List.map (fun s -> (sp n)^s) tl |> String.concat "\n"
;;

let imports = [%string {|
import socket, time, os, struct
from collections import namedtuple
|}]

let bits_to_padded_bytes bits = 
  if ((bits mod 8) == 0)
  then (bits / 8)
  else ((bits / 8) + 1)
;;

let bridge_header_fmt evsigs = 
  (* bitfields padded to byte aligned field, plus 
     1 byte for another event id *)
  let len = (bits_to_padded_bytes (List.length evsigs)) in
  "B"^(string_of_int len)^"s"
;;

(* headers for lucid ethernet packets. The wireevent's 
   size depends on the number of events. *)
let lucid_pkt_hdrs evsigs = [%string {| 
lucid_etype = 0x666
EthHdr = namedtuple("EthHdr", "dst_addr src_addr etype")
EthHdr.fmt = "!6s6sH"
WireEvHdr = namedtuple("WireEv", "event_id port_event_id event_bitvec_pad")
WireEvHdr.fmt = "!B%{bridge_header_fmt evsigs}"
|}]
;;

let parsers = [%string {|
# static
def parse_header(data, HdrDef):
    # return parsed fields and remaining data.
    if (len(data) < struct.calcsize(HdrDef.fmt)):
        return None, []
    return HdrDef( *struct.unpack(HdrDef.fmt, data[0:struct.calcsize(HdrDef.fmt)])), data[struct.calcsize(HdrDef.fmt):]
def deparse_header(hdr, HdrDef, payload):
  return struct.pack(HdrDef.fmt, *hdr) + payload

# more static code -- parsers and deparsers
def parse_eventpacket(pktbuf):
  eth, payload = parse_header(pktbuf, EthHdr)
  if (eth.etype == lucid_etype):
    wireev, payload = parse_header(payload, WireEvHdr)
    for HdrDef in events:
      if (wireev.event_id == HdrDef.id):
        event, payload = parse_header(payload, HdrDef)
        return event, payload
    # if we got here, its an unknown event type
    return None

def deparse_eventpacket(event, payload):
  # smac and dmac are fixed for now
  smac = b'\x01'*6
  dmac = b'\x02'*6 
  # deparse event header
  for HdrDef in events:
    if (event.id == HdrDef.id):
      # add event header
      pktbuf = deparse_header(event, HdrDef, payload)
      # add event metadata header -- includes the bridged header
      wireev = WireEvHdr(event.id, 0, b'\x00'*(struct.calcsize(WireEvHdr.fmt)-2))
      pktbuf = deparse_header(wireev, WireEvHdr, pktbuf)
      # finally add the lucid ethernet hdr
      lucid_eth = EthHdr(dmac, smac, lucid_etype)
      pktbuf = deparse_header(lucid_eth, EthHdr, pktbuf)
      return pktbuf
  # if we got here, its an unknown event type
  return None
|}]


(* lucid type to a struct format specifier in python *)
let ty_to_fmt (ty:CoreSyntax.ty) =
  match ty.raw_ty with
  | TInt(8) -> "B"
  | TInt(16) -> "H"
  | TInt(32) -> "I"
  | TInt(n_bits) -> (
    (* anything larger than 32-bits is a byte string *)
    if ((n_bits mod 8) == 0)
    then ((string_of_int (n_bits / 8))^"s")
    else (error "all parameters in events with a server library must have sizes that are multiples of 8 bits"))
  | _ -> error "all parameters in events for servers must be integers"
;;
let tys_to_fmt tys =
  List.map ty_to_fmt tys |> String.concat ""
;;

let event_name es = fst (es.id) ;;
let param_name (id, _) = fst id ;;

let event_tuples (ess:eventsigs) : string =
  let event_tuple es = [%string {|
%{event_name es} = namedtuple("%{event_name es}", "%{sp_sep param_name es.params}")
%{event_name es}.fmt = "!%{tys_to_fmt (List.split es.params |> snd)}"
%{event_name es}.id  = %{string_of_int es.num}
%{event_name es}.name = %{event_name es}
|}]
  in
  line_sep event_tuple ess
;;
let event_tuple_list ess = [%string {|
events = [%{comma_sep event_name ess}]
|}]
;;

let socket_helpers = [%string {|
############ raw socket helpers ############
def open_socket(iface):
  s = socket.socket(socket.PF_PACKET, socket.SOCK_RAW, socket.htons(0x0003))
  s.bind((iface, 0))
  return s

def tx_pkt(s, pkt):
  s.send(pkt)

def rx_pkt(s):
  # get the next incoming packet
  pkt, addr = s.recvfrom(2048)
  (intf, proto, pkttype, hatype, addr) = addr
  while (pkttype == socket.PACKET_OUTGOING):
    pkt, (intf, proto, pkttype, hatype, addr) = s.recvfrom(2048)
  return pkt

def close_socket(s):
  s.close()

############ end raw socket helpers ############
|}]
;;

let dpid_helper = [%string {|
# mapping based on p4tapp assignment
def dpid_to_veth(dpid):
  return ("veth%i"%(dpid * 2 + 1))
|}]
;;

let lib es = [
  imports;
  lucid_pkt_hdrs es;
  parsers;
  event_tuples es;
  event_tuple_list es;
  socket_helpers;
  dpid_helper
]
  |> String.concat "\n"
;;

let syntax_decls_to_evrecs ds =
  let rec f num ds = 
    match ds with
    | [] -> []
    | d::ds -> (
      match d.d with
      | DEvent(id, _, _, params) -> 
      (* use coresyntax types for params *)
      let params = SyntaxToCore.translate_params params in
      {id=id; num=num; params=params}::(f (num +1) ds)
      | _ -> f num ds
    )
  in
  List.rev (f 1 ds)
;;

let coresyntax_decls_to_evrecs (ds:CoreSyntax.decls) = 
  let rec f num (ds:CoreSyntax.decls) = 
    match ds with
    | [] -> []
    | d::ds -> (
      match d.d with
      | CoreSyntax.DEvent(id, _, params) -> 

        {id=id; num=num; params=params}::(f (num +1) ds)
      | _ -> f num ds
    )
  in
  List.rev (f 1 ds)
;;

(* translators from both syntax and coresyntax *)
let syntax_to_pyeventlib ds =
  (* well formed and type checks *)
  Wellformed.pre_typing_checks ds;
  let ds = Typer.infer_prog Builtins.tofino_builtin_tys ds in
  (* get event info needed for compilation *)
  let evrecs = syntax_decls_to_evrecs ds in
  lib evrecs
;;

let coresyntax_to_pyeventlib cds = 
  let evrecs = coresyntax_decls_to_evrecs cds in
  lib evrecs
;;