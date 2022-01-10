open InterpState
open Syntax

val module_id : Id.t
val defs : State.global_fun list

val signature:
  Id.t * (Id.t * sizes * ty) list * State.global_fun list * (Cid.t * func_ty) list
[@@ocamlformat "disable"]
