open Syntax

(* Builtin variables *)
let self_id = Id.create "self"
let self_ty = ty (TInt (IConst 32))
let recirc_id = Id.create "recirculation_port"
let recirc_ty = ty (TInt (IConst 32))
let lucid_ety_id = Id.create "LUCID_ETHERTY"

(* FIXME: Just guessing: we should probably have this be configurable *)
let lucid_ety_ty = ty (TInt (IConst 16))
let lucid_ety_int = 666
let lucid_ety_value = vint lucid_ety_int 16

(* used as first argument to hash, for checksum in deparser *)
let checksum_id = Id.create "checksum"
let checksum_ty = TInt (IConst 32) |> ty


let builtin_vars =
  [self_id, self_ty; recirc_id, recirc_ty; lucid_ety_id, lucid_ety_ty; checksum_id, checksum_ty]
;;

let builtin_type_info =
  [ Arrays.t_id, Arrays.sizes, Arrays.global
  ; Counters.t_id, Counters.sizes, Counters.global
  ; PairArrays.t_id, PairArrays.sizes, PairArrays.global
  ; Payloads.t_id, Payloads.sizes, Payloads.global ]
;;

(* Building modules *)
let builtin_modules =
  [ Arrays.signature
  ; Counters.signature
  ; Events.signature
  ; System.signature
  ; PairArrays.signature
  ; Payloads.signature ]
;;

let builtin_defs =
  Arrays.defs
  @ Counters.defs
  @ Events.defs
  @ System.defs
  @ PairArrays.defs
  @ Payloads.defs
;;

(* Builtin local vars with
   platform-dependent types *)
let this_id = Id.create "this"
let ingr_port_id = Id.create "ingress_port"

type builtin_tys =
  { ingr_port_ty : Syntax.ty
  ; this_ty : Syntax.ty
  }

let interp_builtin_tys =
  { ingr_port_ty =
      ty_sp (TQVar (QVar (Id.create "auto_ingress_port"))) Span.default
  ; this_ty = TEvent |> ty
  }
;;

let tofino_builtin_tys =
  { ingr_port_ty = TInt (IConst 9) |> ty; this_ty = TEvent |> ty }
;;

(* Used in constraints *)
let start_id = Id.create "start"

(* Used in memops *)
let cell1_id = SyntaxUtils.cell1_id
let cell2_id = SyntaxUtils.cell2_id
let lucid_parse_id = Id.create "do_lucid_parsing"

