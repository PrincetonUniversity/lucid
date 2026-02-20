open Syntax
include Constants
(* builtin constants plus builtin modules (Arrays, etc.) *)
(* separated into 2 modules because the modules depend on interp state *)

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

