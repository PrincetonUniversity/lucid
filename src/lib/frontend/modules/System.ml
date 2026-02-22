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

let sys_time_fun (nst : network_state) _ args =
  let open CoreSyntax in
  match args with
  | [] -> InterpSyntax.V(vinteger (Integer.create ~value:!(nst.switches.(0).global_time) ~size:32))
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
  let open CoreSyntax in
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
    InterpSyntax.V(vinteger (Integer.create_z ~value:randval ~size:32))
  | _ -> sys_random_error "takes no parameters"
;;

(* Sys.checksum *)
(* let sys_checksum_name = "checksum"
let sys_checksum_id = Id.create sys_checksum_name
let sys_checksum_cid = Cid.create_ids [sys_id; sys_checksum_id]
let sys_checksum_error msg = sys_error sys_checksum_name msg
let sys_checksum_ty =
  let start_eff = FVar (QVar (Id.fresh "eff")) in
  ty
  @@ TFun
       { arg_tys = [ty @@ TInt (IConst 16)]
       ; ret_ty = ty @@ TInt (IConst 16)
       ; start_eff
       ; end_eff = start_eff
       ; constraints = ref []
       }
;;
let sys_checksum_fun _ _ args =
  (* TODO: implement checksum. Placeholder for now: 0 *)
  let _ = args in 
  let open CoreSyntax in
  vinteger (Integer.create_z ~value:(Z.of_int 0) ~size:16)
;; *)



(* Sys.dequeue_depth *)
let sys_dequeue_depth_name = "dequeue_depth"
let sys_dequeue_depth_id = Id.create sys_dequeue_depth_name
let sys_dequeue_depth_cid = Cid.create_ids [sys_id; sys_dequeue_depth_id]
let sys_dequeue_depth_error msg = sys_error sys_dequeue_depth_name msg


let sys_dequeue_depth_fun _ _ args =
  let open CoreSyntax in
  match args with
  | [] ->
    (* return 0 for now *)
    InterpSyntax.V(default_vint 32)
  | _ -> sys_dequeue_depth_error "takes no parameters"
;;
let sys_dequeue_depth_ty =
  let start_eff = FVar (QVar (Id.fresh "eff")) in
  ty
  @@ TFun
       { arg_tys = []
       ; ret_ty = ty_sp (TQVar (QVar (Id.create "auto_deq_depth"))) Span.default
       ; start_eff
       ; end_eff = start_eff
       ; constraints = ref []
       }
;;

(* directory of Sys methods *)
let defs : global_fun list =
  [ { cid = sys_time_cid; body = sys_time_fun; ty = sys_time_ty }
  ; { cid = sys_random_cid; body = sys_random_fun; ty = sys_random_ty } 
  ; { cid = sys_dequeue_depth_cid; body = sys_dequeue_depth_fun; ty = sys_dequeue_depth_ty}
  (* ; { cid = sys_checksum_cid; body = sys_checksum_fun; ty = sys_checksum_ty}  *)
  
  ]
;;


let signature = 
  LibraryInterface.tup_to_sigty
  (module_id, [], defs, [])
