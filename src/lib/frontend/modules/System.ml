(* Useful system functions *)
open Batteries
open Syntax
open InterpState

(* Generic Sys defs *)
let sys_name = "Sys"
let sys_id = Id.create sys_name
let module_id = sys_id
let sys_error fun_name msg = error (sys_name ^ ": " ^ fun_name ^ ": " ^ msg)
let sys_time_name = "time"
let sys_time_id = Id.create sys_time_name
let sys_time_cid = Cid.create_ids [sys_id; sys_time_id]
let sys_time_error msg = sys_error sys_time_name msg

let sys_time_ty =
  let start_eff = FVar (QVar (Id.fresh "eff")) in
  ty
  @@ TFun
       { arg_tys = []
       ; ret_ty = ty @@ TInt (IConst 32)
       ; start_eff
       ; end_eff = start_eff
       ; constraints = ref []
       }
;;

let sys_time_fun (nst : State.network_state) _ args =
  match args with
  | [] -> vinteger (Integer.create ~value:nst.current_time ~size:32)
  | _ -> sys_time_error "takes no parameters"
;;

(* Sys.random *)
let sys_random_name = "random"
let sys_random_id = Id.create sys_random_name
let sys_random_cid = Cid.create_ids [sys_id; sys_random_id]
let sys_random_error msg = sys_error sys_random_name msg

let sys_random_ty =
  let start_eff = FVar (QVar (Id.fresh "eff")) in
  ty
  @@ TFun
       { arg_tys = []
       ; ret_ty = ty @@ TInt (IConst 32)
       ; start_eff
       ; end_eff = start_eff
       ; constraints = ref []
       }
;;

let sys_random_fun _ _ args =
  match args with
  | [] ->
    (* Random.bits only generates 30 random bits, so we need 2 more *)
    let randval =
      Random.bits ()
      |> Z.of_int
      |> Z.( * ) (Z.of_int 2)
      |> Z.( + ) (if Random.bool () then Z.one else Z.zero)
      |> Z.( * ) (Z.of_int 2)
      |> Z.( + ) (if Random.bool () then Z.one else Z.zero)
    in
    vinteger (Integer.create_z ~value:randval ~size:32)
  | _ -> sys_random_error "takes no parameters"
;;

let defs : State.global_fun list =
  [ { cid = sys_time_cid; body = sys_time_fun; ty = sys_time_ty }
  ; { cid = sys_random_cid; body = sys_random_fun; ty = sys_random_ty } ]
;;
