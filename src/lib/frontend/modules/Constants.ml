open Syntax
(* constant builtins with minimal dependencies *)

(* Builtin variables *)
let self_id = Id.create "self"
let self_ty = ty (TInt (IConst 32))
let recirc_id = Id.create "recirculation_port"
let recirc_ty = ty (TInt (IConst 32))

(* Background events are encapsulated in eth packets over the wire. *)
let lucid_ety_id = Id.create "LUCID_ETHERTY"
let lucid_ety_ty = ty (TInt (IConst 16))
let lucid_ety_int = 666
let lucid_ety_value = vint lucid_ety_int 16
(* Background events have a shim header after eth, for event type tag.  *)
let lucid_eventnum_ty = ty (TInt (IConst 16))

(* used as first argument to hash, for checksum in (de)parser *)
let checksum_id = Id.create "checksum"
let checksum_ty = TInt (IConst 32) |> ty

let this_ty = ty TEvent
let builtin_vars =
  [self_id, self_ty; recirc_id, recirc_ty; lucid_ety_id, lucid_ety_ty; checksum_id, checksum_ty]
;;


(* Builtin local vars *)
let this_id = Id.create "this"

(* Builtin local vars with
   platform-dependent types *)
let ingr_port_id = Id.create "ingress_port"

type builtin_tys =
  { ingr_port_ty : Syntax.ty }

let interp_builtin_tys =
  (*  note that this is a TVar, which means that all the uses of ingress_port_size 
      will be unified to the same value. This is important for the typechecker to 
      ensure that the size of the ingress port is consistent across the program.
      If it was a QVar, as are most of the reference sizes and types, then it 
      would represent a constant that could take a different size _every time it was 
      used_.       
  *)
  let ingr_port_size_tvar =  TVar (ref (Unbound (Id.fresh "ingress_port_size", 0))) in
  { ingr_port_ty = ty_sp (TInt ((IVar ingr_port_size_tvar))) Span.default }
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
(* an alternative to main, which starts with 
   a parsed ethernet packet. *)
let eth_parse_id = Id.create "eth_main"