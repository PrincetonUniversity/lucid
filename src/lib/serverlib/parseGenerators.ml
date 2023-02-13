(* generate event parsers for endhost 
   programs in c and python *)
open Batteries
open Syntax
open SyntaxUtils
open Printing


(* input type for many printers *)
type eventsig = {
  id : id;
  num : int;
  params : (id * ty) list;
}
type eventsigs = eventsig list
let null_event = {id=Id.fresh "NONE"; num=0; params=[]}

(* printing helpers *)


let evrec_to_string evrec =
  Printf.sprintf "event %s(%s);" 
    (Printing.id_to_string evrec.id)
    (Printing.params_to_string evrec.params)
;;


let sp n = String.make n ' '
let ln_sep ls = String.concat "\n" ls
let comma_sep f ls = List.map f ls |> String.concat ", "
let line_sep f ls =
  List.map f ls |> String.concat "\n"

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

let event_type_t = "event_type"
let event_payload_t = "event_payload"
let eventpacket_t = "eventpacket"
let eventpkt_parser_name = "eventpkt_parse"
let eventpkt_size_name = "eventpkt_size"
let eventpkt_deparse_name = "eventpkt_deparse"
;;

(**** generators for components of library ****)

(* toplevel -- includes *)
let includes = 
[%string {|
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
|}]

let ty_to_cstring ty =
  match ty.raw_ty with
  | TInt(sz) -> (
    match (extract_size sz) with
      | 8 -> "uint8_t"
      | 16 -> "uint16_t"
      | 32 -> "uint32_t"
      | _ -> error "all parameters in events with a server library must be 8, 16, or 32-bit ints"
  )
  | _ -> error "all parameters in events with a server library must be 8, 16, or 32-bit ints"

;;

(* toplevel -- event id enumerator *)
let event_enum es = 
  let event_num_str e = 
    [%string "const uint8_t %{id_to_string e.id |> String.uppercase_ascii} = %{string_of_int e.num};"]
  in
  let event_num_strs n es = 
    line_sep event_num_str es |> indent n
  in
[%string {|
typedef uint8_t %{event_type_t};
%{event_num_strs 0 (null_event::es)}
|}]
;;

(* toplevel -- event structure *)
let event_struct e = 
  let idstr = id_to_string e.id in   
  let str_of_param (fid, fty) = 
    [%string {|%{ty_to_cstring fty} %{id_to_string fid};|}]
  in
[%string {|
typedef struct %{idstr} {
  %{line_sep str_of_param e.params |> indenttl 2}
} __attribute__((__packed__)) %{idstr};
|}]
;;


(* toplevel *)
let event_structs es = 
  line_sep event_struct es
;;

(* toplevel *)
let const_structs = 
[%string {|
#define ETYPE_LUCID 0x666
// parsed header on a lucid packet
typedef struct eth_hdr {
  uint8_t dst[6];
  uint8_t src[6];
  uint16_t etype;
} __attribute__((__packed__)) eth_hdr;

// parsed header on a lucid packet
typedef struct lucid_shim_hdr {
  uint8_t event_type;
  uint16_t _pad;
} __attribute__((__packed__)) lucid_shim_hdr;   

// internal representation of lucid packet's payload
// (after the event)
typedef struct %{event_payload_t} {
  uint8_t* data;
  size_t len;
} %{event_payload_t};
|}]
;;


(* toplevel *)
let eventpacket_struct es =
  let union_field_str e = 
    [%string{|%{id_to_string e.id} %{id_to_string e.id};|}]
  in
[%string{|
// internal representation of a lucid packet
// containing an event + payload.
typedef struct %{eventpacket_t} {
  event_type event_type;
  union {
    %{line_sep union_field_str es |> indenttl 4}
  };
  %{event_payload_t} payload;
} %{eventpacket_t};
|}]
;;

(* toplevel *)
let eventpkt_parse es = 

let eventparse_case e = 
  let evid = id_to_string e.id in 
  let to_caps = String.uppercase_ascii in 
  [%string 
"\
case %{ evid |> to_caps}:{\
  %{evid}* ev = (%{evid}*) _buf;\
  p->%{evid} = *ev;\
  _buf  += sizeof(%{evid});\
  _buflen -= sizeof(%{evid});\
  break;  \
}\
"]
in

[%string {|
// parse a packet buffer buf of size buflen into the eventpacket p,
// using pointers (event data and payloads are pointers to original buffer)
int %{eventpkt_parser_name}(uint8_t* buf, size_t buflen, %{eventpacket_t}* p) {
  uint8_t* _buf = buf;
  size_t _buflen = buflen;
  if (buflen < sizeof(eth_hdr)){
    printf("[eventpkt_parse] error: buffer too small for eth header\n");
    return 0;
  } 
  eth_hdr* e = (eth_hdr*) _buf;
  if (e->etype != ETYPE_LUCID) {
    printf("[eventpkt_parse] error: not a lucid packet\n");
    return 0;
  }

  _buf  += sizeof(eth_hdr);
  _buflen -= sizeof(eth_hdr);
  lucid_shim_hdr* lcd_shim = (lucid_shim_hdr*) _buf;

  p->event_type=lcd_shim->event_type;
  switch (lcd_shim->event_type) {
    case NONE:{
      break;    
    }
    %{line_sep eventparse_case es |> indent 4}
    default: {
      printf("[eventpkt_parse] error: unknown event type\n");
      return 0;
    }
  }
  p->payload.data=_buf;
  p->payload.len=_buflen;
  return 1;
}
|}]
;;


(* toplevel *)
let eventpkt_size es = 
let eventpkt_size_case e = 
  let evid = id_to_string e.id in 
  let to_caps = String.uppercase_ascii in 
[%string 
"\
case %{ evid |> to_caps}:{\
  size = size + sizeof(%{evid});\
  break;\
}\
"]
in
[%string {|
// Calculate the size required for a packet that stores the given event packet
size_t %{eventpkt_size_name}(%{eventpacket_t}* p) {
  size_t size = sizeof(eth_hdr) + sizeof(lucid_shim_hdr) + p->payload.len;
  switch(p->event_type) {
    case NONE: {break;}
    %{line_sep eventpkt_size_case es |> indent 4}
    default: {
      printf("[eventpkt_size] error: unknown event type\n");
      return 0;
    }    
  }
  return size;
}
|}]
;;



(* toplevel *)
let eventpkt_deparse es = 

let eventpkt_deparse_case e = 
  let evid = id_to_string e.id in
  let evconst = evid |> String.uppercase_ascii in
  [%string{|
case (%{evconst}): {
  %{evid}* ev = (%{evid}*) _buf;
  *ev = p->%{evid};
  _buf += sizeof(%{evid});
  break;
}
|}]
in

  [%string {| 
// serialize an eventpacket to packet stored in buf, with 
// length buflen. requirement: buflen >= eventpacket_size(p)
int %{eventpkt_deparse_name}(uint8_t* buf, size_t buflen, 
  uint8_t dmac[6], uint8_t smac[6], eventpacket* p) {
  if (buflen < eventpkt_size(p)) {
      printf("[eventpkt_deparse] error: output buffer too small\n");    
      return 0;
  }
  uint8_t* _buf = buf;

  eth_hdr* eth = (eth_hdr*) _buf;
  memcpy(eth->dst, dmac, 6);
  memcpy(eth->src, smac, 6);
  eth->etype=ntohs(ETYPE_LUCID);
  _buf += sizeof(eth_hdr);  

  lucid_shim_hdr* lsh = (lucid_shim_hdr *)_buf;
  lsh->event_type = p->event_type;
  lsh->_pad = 0;
  _buf += sizeof(lucid_shim_hdr); 
  switch (p->event_type) {
    case (NONE): {
      break;
    }
    %{line_sep eventpkt_deparse_case es |> indent 4}
    default: {
        printf("[eventpkt_deparse] error: unknown event type\n");
        return 0;     
    }
  }
  memcpy(_buf, p->payload.data, p->payload.len);
  return 1;
}

|}]
;;


let lib es = [
  includes; 
  event_enum es; 
  event_structs es; 
  const_structs; 
  eventpacket_struct es; 
  eventpkt_parse es; 
  eventpkt_size es;  
  eventpkt_deparse es;
]
  |> String.concat "\n"
;;

let decls_to_evrecs ds =
  let rec f num ds = 
    match ds with
    | [] -> []
    | d::ds -> (
      match d.d with
      | DEvent(id, _, _, params) -> {id=id; num=num; params=params}::(f (num +1) ds)
      | _ -> f num ds
    )
  in
  List.rev (f 1 ds)
;;

let generate_c ds =
  (* well formed and type checks *)
  Wellformed.pre_typing_checks ds;
  let ds = Typer.infer_prog Builtins.interp_builtin_tys ds in
  (* get event info needed for compilation *)
  let evrecs = decls_to_evrecs ds in
  line_sep evrec_to_string evrecs |> print_endline;
  lib evrecs
;;