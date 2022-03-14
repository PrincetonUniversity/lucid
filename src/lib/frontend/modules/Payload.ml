(* Interpretation of packet payloads. *)
open Batteries
open Syntax
open InterpState

(* Generic Payload defs *)
let payload_name = "Payload"
let payload_id = Id.create payload_name
let module_id = payload_id
let t_id = Cid.create_ids [payload_id; Id.create "t"]
let sizes_labels = [], []

(* Constructor *)
let payload_create_id = Cid.create_ids [payload_id; Id.create "create"]

let payload_create_sig =
  let start_eff = FVar (QVar (Id.fresh "eff")) in
  { arg_tys = []
  ; ret_ty = ty (TName (t_id, [], true))
  ; start_eff
  ; end_eff = start_eff
  ; constraints = ref []
  }
;;

let constructors = [payload_create_id, payload_create_sig]
let defs : State.global_fun list = []

let signature =
  ( module_id
  , [Cid.last_id t_id, [], TName (t_id, [], true) |> ty]
  , defs
  , constructors )
;;
