open Batteries
open Syntax
open InterpState

let packet_name = "Packet"
let packet_id = Id.create packet_name

let packet_error fun_name msg =
  error (packet_name ^ ": " ^ fun_name ^ ": " ^ msg)
;;

let module_id = packet_id
let t_id = Cid.create_ids [packet_id; Id.create "t"]
let packet_ty = ty @@ TName (t_id, [], false)
let sizes = 0
let ty_args = 0
let global = false

(* Not a global type, so no global constructor *)
let constructors = []


(* auto _ = Packet.parse(Packet.t p); *)
let packet_parse_name = "parse"
let packet_parse_id = Id.create packet_parse_name
let packet_parse_cid = Cid.create_ids [packet_id; packet_parse_id]
let packet_parse_ty = 
  let start_eff = FVar (QVar (Id.fresh "eff")) in
  ty
  @@ TFun
       { arg_tys = [packet_ty]
       ; ret_ty = ty (TQVar (QVar (Id.fresh "parse_ret")))
       ; start_eff
       ; end_eff = start_eff
       ; constraints = ref []
       }
;;
let packet_parse_error msg = packet_error packet_parse_name msg

let packet_parse_fun nst swnum args =
  let _, _, _ = nst, swnum, args in
  packet_parse_error "Packet.parse should never be called outside of parsers"
;;

let defs : global_fun list =
  [ { cid = packet_parse_cid; body = packet_parse_fun; ty = packet_parse_ty } ]
;;

let signature =
  LibraryInterface.tup_to_sigty
    (module_id, [Cid.last_id t_id, [], packet_ty], defs, constructors)
;;