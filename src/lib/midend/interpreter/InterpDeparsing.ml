open Batteries
open CoreSyntax

(*** deparse / serialization ***)
(* serialize a value and append it to the bitstring *)
let pwrite (p:BitString.bits) (v:value) : BitString.bits = 
  match v.v with
  | VInt(z) -> 
    let vval = Z.to_int z.value in
    let size = Z.to_int z.size in
    let pbits = BitString.int_to_bits size vval in
    BitString.concat p pbits
  | VBool(b) -> 
    let i = match b with | true -> 1 | false -> 0 in
    BitString.concat p (BitString.int_to_bits 1 i)
  | VBits(bs) -> BitString.concat p bs
  | _ -> error "[pwrite] unsupported type"
;;


(*  Serialize a packet event to a bitstring.
    Packet events have no tag or encaspulation headers. 
    The values are just serialized directly. *)
let serialize_packet_event event_val = 
  (* serialize all the event arguments to a single bitstring *)
  let packet_bits = List.fold_left pwrite [] event_val.data in 
  (* tag with metadata *)
  {event_val with eid=Cid.create ["bytes"]; data=[vbits packet_bits]; eserialized=true;}
;;

(*  Serialize a background event to a bitstring.
    A background event has an optional header (e.g., eth), 
    an event tag (16 bit), and then the values serialized. *)
let serialize_background_event lucid_hdrs event_val = 
  let evnum = match event_val.evnum with 
    | None -> error "cannot serialize a background event with no number"
    | Some(evnum) -> evnum 
  in
  let all_data = lucid_hdrs@[evnum]@event_val.data in
  let packet_bits = List.fold_left pwrite [] all_data in 
  {event_val with eid=Cid.create ["bytes"]; data=[vbits packet_bits]; eserialized=true;}
;;

let translate_vint (v : Syntax.value) = 
  match v.v with 
  | Syntax.VInt({value; size}) -> CoreSyntax.vint (Z.to_int value) (Z.to_int size)
  | _ -> error "not a vint"
;;  

let lucid_eth_fields = 
  [vint 1 48; vint 2 48; Constants.lucid_ety_value |> translate_vint]
;;
(* serialize any event as an ethernet packet. 
   we need to know if this event is a background event or a packet event. 
   If its a packet event, then we must not add the wrapper or event type tag. *)
let serialize_event_eth event_val = 
  if (event_val.eserialized) then ( event_val )
  (* not serialized means its a background event *)
  else ( serialize_background_event lucid_eth_fields event_val )
;;
