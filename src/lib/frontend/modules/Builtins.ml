open Syntax

(* Builtin variables *)
let self_id = Id.create "self"
let self_ty = ty (TInt (IConst 32))
let recirc_id = Id.create "recirculation_port"
let recirc_ty = ty (TInt (IConst 32))
let builtin_vars = [self_id, self_ty; recirc_id, recirc_ty]
let builtin_type_ids = [Arrays.t_id; Counters.t_id]

(* Building modules *)
let builtin_modules =
  [Arrays.signature; Counters.signature; Events.signature; System.signature]
;;

let builtin_defs = Arrays.defs @ Counters.defs @ Events.defs @ System.defs

(* Not a global var *)
let this_id = Id.create "this"
let this_ty = TEvent |> ty
let ingr_port_id = Id.create "ingress_port"
let ingr_port_ty = TInt (IConst 32) |> ty

(* Used in constraints *)
let start_id = Id.create "start"

(* Used in memops *)
let cell1_id = SyntaxUtils.cell1_id
let cell2_id = SyntaxUtils.cell2_id
