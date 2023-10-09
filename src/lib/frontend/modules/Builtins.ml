open Syntax

(* Builtin variables *)
let self_id = Id.create "self"
let self_ty = ty (TInt (IConst 32))
let recirc_id = Id.create "recirculation_port"
let recirc_ty = ty (TInt (IConst 32))

(* the lucid ethernet header is the tag for background events *)
let lucid_ety_id = Id.create "LUCID_ETHERTY"
let lucid_ety_ty = ty (TInt (IConst 16))
let lucid_ety_int = 666
let lucid_ety_value = vint lucid_ety_int 16
let lucid_eventnum_ty = ty (TInt (IConst 16))

(* used as first argument to hash, for checksum in (de)parser *)
let checksum_id = Id.create "checksum"
let checksum_ty = TInt (IConst 32) |> ty

let this_ty = ty TEvent
let builtin_vars =
  [self_id, self_ty; recirc_id, recirc_ty; lucid_ety_id, lucid_ety_ty; checksum_id, checksum_ty]
;;

let builtin_type_info =
  [ Arrays.t_id, Arrays.sizes, Arrays.global
  ; Counters.t_id, Counters.sizes, Counters.global
  ; PairArrays.t_id, PairArrays.sizes, PairArrays.global
  ; Payloads.t_id, Payloads.sizes, Payloads.global
  ; Packet.t_id, Packet.sizes, Packet.global
  ]
;;

(* Builtin modules *)
let builtin_modules =
  [ Arrays.signature
  ; Counters.signature
  ; Events.signature
  ; System.signature
  ; PairArrays.signature
  ; Payloads.signature
  ; Packet.signature 

  ]
;;

let builtin_defs =
  Arrays.defs
  @ Counters.defs
  @ Events.defs
  @ System.defs
  @ PairArrays.defs
  @ Payloads.defs
  @ Packet.defs
;;


(* Builtin local vars *)
let this_id = Id.create "this"

(* Builtin local vars with
   platform-dependent types *)
let ingr_port_id = Id.create "ingress_port"

type builtin_tys =
  { ingr_port_ty : Syntax.ty }

let interp_builtin_tys =
  { ingr_port_ty =
      ty_sp (TQVar (QVar (Id.create "auto_ingress_port"))) Span.default}
;;

let tofino_builtin_tys =
  { ingr_port_ty = TInt (IConst 9) |> ty; }
;;

(* Used in constraints *)
let start_id = Id.create "start"

(* Used in memops *)
let cell1_id = SyntaxUtils.cell1_id
let cell2_id = SyntaxUtils.cell2_id

(* used in the parser *) 
let lucid_parse_id = Id.create "do_lucid_parsing"
let packet_arg_id = Id.create "packet"
let main_parse_id = Id.create "main"