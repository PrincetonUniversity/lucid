(* Interpretation of stateful payloads. *)
open Batteries
open Syntax
open InterpState

(* Generic Payload defs *)
let payload_name = "Payload"
let payload_id = Id.create payload_name

let payload_error fun_name msg =
  error (payload_name ^ ": " ^ fun_name ^ ": " ^ msg)
;;

let module_id = payload_id
let t_id = Cid.create_ids [payload_id; Id.create "t"]
let payload_ty = ty @@ TName (t_id, [], false)

(* Not a global type, so no global constructor *)
let constructors = []

(* Payload.empty *)
let payload_empty_name = "empty"
let payload_empty_id = Id.create payload_empty_name
let payload_empty_cid = Cid.create_ids [payload_id; payload_empty_id]
let payload_empty_error msg = payload_error payload_empty_name msg

let payload_empty_ty =
  let start_eff = FVar (QVar (Id.fresh "eff")) in
  ty
  @@ TFun
       { arg_tys = []
       ; ret_ty = payload_ty
       ; start_eff
       ; end_eff = start_eff
       ; constraints = ref []
       }
;;

let dummy_memop = State.F (fun _ _ args -> extract_ival (List.hd args))
let setop = State.F (fun _ _ args -> extract_ival (List.nth args 1))
let dummy_int = State.V (CoreSyntax.vinteger (Integer.of_int 0))

let payload_empty_fun _ _ args =
  match args with
  | [] -> CoreSyntax.vint_sp (Integer.create ~size:32 ~value:0) Span.default
  | _ ->
    payload_empty_error "Incorrect number or type of arguments to Payload.empty"
;;

let defs : State.global_fun list =
  [{ cid = payload_empty_cid; body = payload_empty_fun; ty = payload_empty_ty }]
;;

let signature =
  module_id, [Cid.last_id t_id, [], payload_ty], defs, constructors
;;
