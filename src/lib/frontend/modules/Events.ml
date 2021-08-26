(* Useful eventtem functions *)

open Batteries
open Syntax
open InterpState

(* Generic Event defs *)
let event_name = "Event"
let event_id = Id.create event_name
let module_id = event_id
let event_error fun_name msg = error (event_name ^ ": " ^ fun_name ^ ": " ^ msg)

(* Event.delay *)
let event_delay_name = "delay"
let event_delay_id = Id.create event_delay_name
let event_delay_cid = Cid.create_ids [event_id; event_delay_id]
let event_delay_error msg = event_error event_delay_name msg

let delay_fun err _ _ args =
  let open State in
  match args with
  | [V { v = VEvent event }; V { v = VInt delay }] ->
    let delay = Integer.to_int delay in
    if delay < 0
    then err "Interpreter: Cannot create an event with negative delay"
    else vevent { event with edelay = delay }
  | _ -> err "Incorrect number of type of events to Event.delay"
;;

let event_delay_fun = delay_fun event_delay_error

let delay_ty b =
  let eff = FVar (QVar (Id.fresh "eff")) in
  ty
  @@ TFun
       { arg_tys = [ty @@ TEvent b; ty @@ TInt (IConst 32)]
       ; ret_ty = ty @@ TEvent b
       ; start_eff = eff
       ; end_eff = eff
       ; constraints = ref []
       }
;;

let event_delay_ty = delay_ty false

(* Event.mdelay *)
let event_mdelay_name = "mdelay"
let event_mdelay_id = Id.create event_mdelay_name
let event_mdelay_cid = Cid.create_ids [event_id; event_mdelay_id]
let event_mdelay_error msg = event_error event_mdelay_name msg
let event_mdelay_ty = delay_ty true
let event_mdelay_fun = delay_fun event_mdelay_error

(* Event.sslocate *)
let event_sslocate_name = "sslocate"
let event_sslocate_id = Id.create event_sslocate_name
let event_sslocate_cid = Cid.create_ids [event_id; event_sslocate_id]
let event_sslocate_error msg = event_error event_sslocate_name msg

let locate_fun err nst _ args =
  (* Hack to make the types work *)
  let err str = failwith (err str) in
  let open State in
  let event, locations =
    match args with
    | [V { v = VEvent event }; V { v = VInt location }] -> event, [location]
    | [V { v = VEvent event }; V { v = VGroup locations }] -> event, locations
    | _ -> err "Incorrect number of type of events to Event.locate"
  in
  List.iter
    (fun location ->
      let location = Integer.to_int location in
      if location < 0 || location >= Array.length nst.switches
      then err @@ Printf.sprintf "invalid location %d" location)
    locations;
  vevent { event with elocations = locations }
;;

let event_sslocate_fun = locate_fun event_sslocate_error

let locate_ty in_b out_b =
  let eff = FVar (QVar (Id.fresh "eff")) in
  let argty = if out_b then TGroup else TInt (IConst 32) in
  ty
  @@ TFun
       { arg_tys = [ty @@ TEvent in_b; ty argty]
       ; ret_ty = ty @@ TEvent out_b
       ; start_eff = eff
       ; end_eff = eff
       ; constraints = ref []
       }
;;

let event_sslocate_ty = locate_ty false false

(* Event.smlocate *)
let event_smlocate_name = "smlocate"
let event_smlocate_id = Id.create event_smlocate_name
let event_smlocate_cid = Cid.create_ids [event_id; event_smlocate_id]
let event_smlocate_error msg = event_error event_smlocate_name msg
let event_smlocate_fun = locate_fun event_smlocate_error
let event_smlocate_ty = locate_ty false true

(* Event.mslocate *)
let event_mslocate_name = "mslocate"
let event_mslocate_id = Id.create event_mslocate_name
let event_mslocate_cid = Cid.create_ids [event_id; event_mslocate_id]
let event_mslocate_error msg = event_error event_mslocate_name msg
let event_mslocate_fun = locate_fun event_mslocate_error
let event_mslocate_ty = locate_ty true false

(* Event.mmlocate *)
let event_mmlocate_name = "mmlocate"
let event_mmlocate_id = Id.create event_mmlocate_name
let event_mmlocate_cid = Cid.create_ids [event_id; event_mmlocate_id]
let event_mmlocate_error msg = event_error event_mmlocate_name msg
let event_mmlocate_fun = locate_fun event_mmlocate_error
let event_mmlocate_ty = locate_ty true true

let defs : State.global_fun list =
  [ { cid = event_delay_cid; body = event_delay_fun; ty = event_delay_ty }
  ; { cid = event_mdelay_cid; body = event_mdelay_fun; ty = event_mdelay_ty }
  ; { cid = event_sslocate_cid
    ; body = event_sslocate_fun
    ; ty = event_sslocate_ty
    }
  ; { cid = event_smlocate_cid
    ; body = event_smlocate_fun
    ; ty = event_smlocate_ty
    }
  ; { cid = event_mslocate_cid
    ; body = event_mslocate_fun
    ; ty = event_mslocate_ty
    }
  ; { cid = event_mmlocate_cid
    ; body = event_mmlocate_fun
    ; ty = event_mmlocate_ty
    } ]
;;
