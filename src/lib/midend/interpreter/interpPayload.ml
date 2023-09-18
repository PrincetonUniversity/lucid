open Batteries
open CoreSyntax
open InterpSyntax
open InterpState


(* bitstring helpers *)
let read (pv:BitString.bits) n : (int * BitString.bits) = 
  let i = BitString.read_msb n pv in
  let pbits = match BitString.advance n pv with
    | Some(bs) -> bs
    | None -> failwith "[parser packet_read] attempted to read past end of packet"
  in
  (i, pbits)
;;
let peek pv n : int = 
  BitString.read_msb n pv
;;

let skip pv n : BitString.bits = 
  let pbits = match BitString.advance n pv with
    | Some(bs) -> bs
    | None -> failwith "[parser packet_skip] attempted to advance past end of packet"
  in
  pbits
;;

(*** parser action implementations ***)

(* read something of the given type from the packet. Return a CoreSyntax value. *)
let pread pkt ty : (CoreSyntax.value * BitString.bits) = 
  match ty.raw_ty with
  | TInt(size) -> 
    let (i, pkt) = read pkt size in
    (vint i size), pkt
  | TBool -> (
    let (i, pkt) = read pkt 1 in
    match i with 
    | 0 -> (vbool false), pkt
    | 1 -> (vbool true), pkt
    | _ -> error "[pread] invalid result from packet_read"
  )
  | _ -> error "[pread] unsupported type"
;;
let ppeek pkt ty = 
  match ty.raw_ty with 
  | TInt(size) -> vint (peek pkt size) size
  | TBool -> (
    match (peek pkt 1) with 
    | 0 -> vbool false
    | 1 -> vbool true
    | _ -> error "[ppeek] invalid result from packet_peek"
  )
  | _ -> error "[ppeek] unsupported type"
;;

let padvance pkt ty =
  match ty.raw_ty with
  | TInt(size) -> skip pkt size
  | TBool -> skip pkt 1
  | _ -> error "[padvance] unsupported type"
;;

(*** deparse / serialization ***)
(* append a single value to a payload *)
let pwrite (pkt:BitString.bits) (v:value) : BitString.bits = 
  match v.v with
  | VInt(z) -> 
    let vval = Z.to_int z.value in
    let size = Z.to_int z.size in
    let pbits = BitString.int_to_bits size vval in
    BitString.concat pkt pbits
  | VBool(b) -> 
    let i = match b with | true -> 1 | false -> 0 in
    BitString.concat pkt (BitString.int_to_bits 1 i)
  | _ -> error "[pwrite] unsupported type"
;;

(* serialize a packet event to a payload *)
let serialize_packet_event event_val = 
  let header = List.fold_left pwrite [] event_val.data in 
  let packet = match event_val.epayload with
    | Some(p) -> BitString.concat header p
    | None -> header
  in
  packet
;;

(* Serialize a background event into a payload. *)
let serialize_background_event lucid_hdrs event_val = 
  let evnum = match event_val.evnum with 
    | None -> error "cannot serialize a background event with no number"
    | Some(evnum) -> evnum 
  in
  let all_data = 
    lucid_hdrs
    @event_val.data
    @[evnum]
  in
  let header = List.fold_left pwrite [] all_data in 
  (* finally add the payload *)
  let packet = match event_val.epayload with
  | Some(p) -> (BitString.concat header p)
  | None -> header
in
packet
;;

let lucid_parse_fun (nst: State.network_state) swid args = 
  let _, _, _ = nst, swid, args in
  let payload = match args with
    | [_; payload] -> payload
    | _ -> error "unexpected args"
  in
  match payload with 
  | InterpState.State.P(p) -> 
    let event_num, payload = pread 
      p 
      (Builtins.lucid_eventnum_ty |> SyntaxToCore.translate_ty) 
    in
    let event_num_int = match event_num.v with 
      | VInt(v) -> v.value |> Z.to_int
      | _ -> error "event number is not a value?"
    in
    
    (* look up the event signature *)
    let event_cid, param_tys = match IntMap.find_opt event_num_int nst.event_signatures with
    | Some(cid, tys) -> cid, tys
    | None ->
      print_endline ("----event number directory----");
      IntMap.iter (fun k v -> print_endline ("event num: "^(string_of_int k)^" event id: "^(Cid.to_string (fst v))   )) nst.event_signatures;
      error ("parsed an event tag int that doesn't correspond to a known event: "^(string_of_int event_num_int));
    in
    (* now parse all the arguments in order *)
    let payload, args = List.fold_left 
      (fun (payload, args) ty ->
        let arg, payload = pread payload ty in
        payload, args@[arg])
      (payload, [])
      param_tys
    in
    (* now construct the event value *)
    vevent {
      eid = event_cid;
      data = args;
      edelay = 0;
      epayload = Some(payload);
      evnum = Some(event_num);
    }
  | _ -> error "lucid builtin background event parser called without payload arg"
;;
