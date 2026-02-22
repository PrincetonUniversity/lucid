(* Useful eventtem functions *)

open Batteries
open Syntax
open InterpSwitch

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

let event_delay_fun _ _ args =
  let open CoreSyntax in
  let open InterpSyntax in 
  match args with
  | [V { v = VEvent event }; V { v = VInt delay }] ->
    let delay = Integer.to_int delay in
    V (if delay < 0
    then
      event_error
        event_delay_name
        "Interpreter: Cannot create an event with negative delay"
    else vevent { event with edelay = delay })
  | _ ->
    event_error event_delay_name "Incorrect number of arguments to Event.delay"
;;

let event_delay_ty =
  let eff = FVar (QVar (Id.fresh "eff")) in
  ty
  @@ TFun
       { arg_tys = [ty TEvent; ty @@ TInt (IConst 32)]
       ; ret_ty = ty TEvent
       ; start_eff = eff
       ; end_eff = eff
       ; constraints = ref []
       }
;;

let defs : global_fun list =
  [{ cid = event_delay_cid; body = event_delay_fun; ty = event_delay_ty }]
;;

let signature = 
  LibraryInterface.tup_to_sigty
  (module_id, [], defs, [])
