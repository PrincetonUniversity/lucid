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
  ; Tables.t_id, Tables.sizes, Tables.global
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
  ; Tables.signature
  ]
;;
let builtin_tycid_to_ty = List.filter_map 
  (fun (libsig: LibraryInterface.sigty) -> 
    match libsig.m_tys with 
    | [] -> None
    | [t_id, _, ty] -> 
      Some(Cid.create_ids [libsig.m_id; t_id], ty)
    | _ -> error "unexpected: builtin library with multiple constructors"
      (* this is fine to change, but the callers of this function need to adjust *)
  )
  builtin_modules
;;

let builtin_defs =
  Arrays.defs
  @ Counters.defs
  @ Events.defs
  @ System.defs
  @ PairArrays.defs
  @ Payloads.defs
  @ Packet.defs
  @ Tables.defs
;;

let gfun_cid (gf : InterpState.State.global_fun) : Cid.t = 
  gf.cid
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