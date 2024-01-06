(* Interpretation of stateful payloads. *)
open Batteries
open Syntax
open InterpState

(*  some helpers for readability *)
let effectless_fun_ty arg_tys ret_ty =
  let start_eff = FVar (QVar (Id.fresh "eff")) in
  ty
  @@ TFun
       { arg_tys
       ; ret_ty
       ; start_eff
       ; end_eff = start_eff
       ; constraints = ref []
       }
;;

let tynum = ref (-1)
let fresh_ty tyid_base =  
  incr tynum;
  let ty_id = Id.create (tyid_base ^ string_of_int !tynum) in
  ty_sp (TQVar (QVar ty_id)) Span.default
;;
let fresh_size base_id = 
  incr tynum;
  let size_id = Id.create ( base_id ^ string_of_int !tynum) in
   (IVar (QVar size_id))
;;

(* Generic Payload defs *)
let payload_name = "Payload"
let payload_id = Id.create payload_name

let payload_error fun_name msg =
  error (payload_name ^ ": " ^ fun_name ^ ": " ^ msg)
;;

let module_id = payload_id
let t_id = Cid.create_ids [payload_id; Id.create "t"]
let payload_ty = ty @@ TName (t_id, [], false)
let sizes = 0
let ty_args = 0

let global = false

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

(* Just use ints to represent payloads in the interpreter. We could make a new
   type if we really wanted to distinguish them better *)
(* Lets use a pattern value for now. *)
let payload_empty_fun _ _ args =
  match args with
  | [] -> InterpSyntax.V({(CoreSyntax.vpat []) with vty = (SyntaxToCore.translate_ty payload_ty)})
    
    (* CoreSyntax.vint_sp (Integer.create ~size:32 ~value:0) Span.default *)
  | _ ->
    payload_empty_error "Incorrect number or type of arguments to Payload.empty"
;;

(* Payload.parse *)
(* Right now, parsing is a builtin. But it would be nice to move it out to a 
   module like this. Requires some refactoring of the core ir. *)
let payload_parse_name = "parse"
let payload_parse_id = Id.create payload_parse_name
let payload_parse_cid = Cid.create_ids [payload_id; payload_parse_id]
let payload_parse_ty = effectless_fun_ty [ty TBitstring] payload_ty
let payload_parse_error msg = payload_error payload_parse_name msg

let payload_parse_fun _ _ args =
  (* at this point, Payload.parse is just a wrapper that stores 
     whatever bitstring is left at the end of packet processing. *)
  let open InterpSyntax in
  let open CoreSyntax in
  match args with 
  | [V{v}] -> InterpSyntax.V(value v)
  | _ -> payload_parse_error "Payload.parse called with wrong args"
;;

(* Payload.read *)
let payload_read_name = "read"
let payload_read_id = Id.create payload_read_name
let payload_read_cid = Cid.create_ids [payload_id; payload_read_id]

let payload_read_ty = effectless_fun_ty [payload_ty] (fresh_ty "payload_read_ret") ;;
  
let payload_read_error msg = payload_error payload_read_name msg

let payload_read_fun _ _ _ =
  payload_read_error "Payload.read is not implemented yet"
;;


(* Payload.skip *)
(* Payload.skip : (pkt : Payload.t) -> (n : int) -> unit 
    skip n bytes in pkt *)
let payload_skip_name = "skip"
let payload_skip_id = Id.create payload_skip_name
let payload_skip_cid = Cid.create_ids [payload_id; payload_skip_id]
let payload_skip_ty = effectless_fun_ty [payload_ty; ty (TInt(fresh_size "payload_skip_arg"))   ] (ty TVoid) ;;
let payload_skip_error msg = payload_error payload_skip_name msg
let payload_skip_fun _ _ _ =
  payload_skip_error "Payload.skip is not implemented yet"
;;

(* Payload.peek *)
let payload_peek_name = "peek"
let payload_peek_id = Id.create payload_peek_name
let payload_peek_cid = Cid.create_ids [payload_id; payload_peek_id]

let payload_peek_ty = effectless_fun_ty [payload_ty] (fresh_ty "payload_peek_ret") ;;

let payload_peek_error msg = payload_error payload_peek_name msg

let payload_peek_fun _ _ _ =
  payload_peek_error "Payload.peek is not implemented yet"
;;



let defs : State.global_fun list =
  [ { cid = payload_empty_cid; body = payload_empty_fun; ty = payload_empty_ty }
  ; { cid = payload_parse_cid; body = payload_parse_fun; ty = payload_parse_ty }
  ; {cid = payload_read_cid; body = payload_read_fun; ty = payload_read_ty}
  ; {cid = payload_peek_cid; body = payload_peek_fun; ty = payload_peek_ty}
  ]
;;

let signature =
  module_id, [Cid.last_id t_id, [], payload_ty], defs, constructors
;;
