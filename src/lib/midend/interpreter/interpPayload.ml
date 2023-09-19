open Batteries
open CoreSyntax
open InterpSyntax
open InterpState

let is_payload_ty ty = equiv_ty ty (Payloads.payload_ty |> SyntaxToCore.translate_ty)


(*** parser action implementations ***)

(* read something of the given type from the packet. Return a CoreSyntax value. *)
let pread p ty : (CoreSyntax.value * BitString.bits) = 
  let size = match ty.raw_ty with 
    | TInt(size) -> size | TBool -> 1
    | _ -> error ("[pread] unsupported type ("^(CorePrinting.ty_to_string ty)^")")
  in
  match (BitString.pop_msb size p) with
  | Some(i, bits) -> (vint i size, bits)
  | None -> error "[pread] attempted to read past end of packet"
;;

let ppeek p ty = 
  let size = match ty.raw_ty with 
    | TInt(size) -> size | TBool -> 1
    | _ -> error "[ppeek] unsupported type"
  in
  match (BitString.pop_msb size p) with
  | Some(i, _) -> vint i size
  | None -> error "[ppeek] attempted to read past end of packet"
;;

let padvance p ty =
  let size = match ty.raw_ty with 
    | TInt(size) -> size | TBool -> 1
    | _ -> error "[padvance] unsupported type"
  in
  match (BitString.pop_msb size p) with
  | Some(_, bits) -> bits
  | None -> error "[padvance] attempted to read past end of packet"
;;

(*** deparse / serialization ***)
(* append a single value to a payload *)
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
  (* payload value -- ignore for now, we use implicit *)
  | VPat(_) when (is_payload_ty v.vty) -> 
    BitString.concat p (CoreSyntax.vpat_to_payload v)
  | _ -> error "[pwrite] unsupported type"
;;

(* serialize a packet event to a payload *)
let serialize_packet_event event_val = 
  let header = List.fold_left pwrite [] event_val.data in 
  header
;;

(* Serialize a background event into a payload *)
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
  header
;;

(* the lucid parser for background events *)
let lucid_parse_fun (nst: State.network_state) swid args = 
  let _, _, _ = nst, swid, args in
  let payload = match args with
    | [_; payload] -> payload
    | _ -> error "unexpected args"
  in
  let payload = match payload with 
    | InterpSyntax.V(vpat) -> CoreSyntax.vpat_to_payload vpat
    | _ -> error "lucid builtin background event parser called without payload arg"
  in
  let event_num, payload = pread payload
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
  let _, args = List.fold_left
    (fun (payload, argvs) ty ->
      if (is_payload_ty ty) then
        (* when we reach the payload argument, put the entire rest of the payload in it. *)
        [], argvs@[payload_to_vpat payload]
      else 
      let arg, payload = pread payload ty in
      payload, argvs@[arg])
    (payload, [])
    param_tys
  in
  (* now construct the event value, only passing it a payload if it has a payload arg *)
  let res = vevent {
    eid = event_cid;
    data = args;
    edelay = 0;
    evnum = Some(event_num);
  }
  in 
  res
;;
