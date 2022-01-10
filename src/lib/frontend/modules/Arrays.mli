open InterpState
open Syntax

val module_id : Id.t
val t_id : Cid.t
val sizes_labels : Id.t list * params
val constructors : (Cid.t * func_ty) list
val defs : State.global_fun list

val signature:
   Id.t * (Id.t * sizes * ty) list * State.global_fun list * (Cid.t * func_ty) list
[@@ocamlformat "disable"]
