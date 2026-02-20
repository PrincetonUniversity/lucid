(***
  This module is for translating between CoreSyntax values and bitstrings, 
  mainly for the interpreter's parser and event serialization.
***)

open Batteries
open CoreSyntax
open InterpSyntax
open InterpState
let is_payload_ty ty = equiv_ty ty (Payloads.payload_ty |> SyntaxToCore.translate_ty)

(*** Helper functions used inside of parsers ***)

(* read something of the given type from the packet. Return a CoreSyntax value. *)
let pread (p: value) ty : (value * value) = 
  let size = match ty.raw_ty with 
    | TInt(Sz size) -> size | TBool -> 1
    | _ -> error ("[pread] unsupported type ("^(CorePrinting.ty_to_string ty)^")")
  in
  match (BitString.pop_msb size (extract_bits p)) with
  | Some(i, bits) -> (vint i size, (vbits bits))
  | None -> error "[pread] attempted to read past end of packet"
;;

let ppeek p ty = 
  let size = match ty.raw_ty with 
    | TInt(Sz size) -> size | TBool -> 1
    | _ -> error "[ppeek] unsupported type"
  in
  match (BitString.peek_msb size (extract_bits p)) with
  | Some(i) -> vint i size
  | None -> error "[ppeek] attempted to read past end of packet"
;;

let padvance p ty =
  let size = match ty.raw_ty with 
    | TInt(Sz size) -> size | TBool -> 1
    | _ -> error "[padvance] unsupported type"
  in
  match (BitString.pop_msb size (extract_bits p)) with
  | Some(_, bits) -> vbits bits
  | None -> error "[padvance] attempted to read past end of packet"
;;

(* parse parse a list of types from a payload into a list of values *)
let parse_args (p:value) arg_tys = 
  let _, args = List.fold_left
    (fun ((payload:CoreSyntax.value), argvs) ty ->
      if (is_payload_ty ty) then
        (* (vbits []) will cause an error if there's anything
           after a payload (as expected) *)
        (vbits []), argvs@[payload]
      else 
      let arg, payload = pread payload ty in
      payload, argvs@[arg])
    (p, [])
    arg_tys
  in
  args
;;


let lucid_parse_fun (nst: network_state) _(*swid*) args = 
  (* payload is a VBits value *)
  let payload = match args with
    | [_; InterpSyntax.V(payload)] -> payload
    | _ -> error "unexpected args to lucid builtin parser"
  in
  (* read the event tag *)
  let event_num, payload = pread payload
    (Builtins.lucid_eventnum_ty |> SyntaxToCore.translate_ty) 
  in
  let event_num_int = match event_num.v with 
    | VInt(v) -> v.value |> Z.to_int
    | _ -> error "event number is not a value?"
  in
  (* look up the event signature *)
  let event_cid, param_tys = match InterpSim.IntMap.find_opt event_num_int nst.event_signatures with
  | Some(cid, tys) -> cid, tys
  | None ->
    print_endline ("----event number directory----");
    InterpSim.IntMap.iter (fun k v -> print_endline ("event num: "^(string_of_int k)^" event id: "^(Cid.to_string (fst v))   )) nst.event_signatures;
    error ("parsed an event tag int that doesn't correspond to a known event: "^(string_of_int event_num_int));
  in
  (* parse arguments from bitstring *)
  let args = parse_args payload param_tys in
  (* construct the event value, only passing it a payload if it has a payload arg *)
  let res = vevent {
    eid = event_cid;
    data = args;
    edelay = 0;
    evnum = Some(event_num);
    eserialized = false;
  }
  in 
  InterpSyntax.V(res)
;;
