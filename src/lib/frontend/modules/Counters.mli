open InterpState

val module_id : Id.t
val t_id : Cid.t
val sizes_labels : Id.t list * Syntax.params
val constructors : (Cid.t * Syntax.func_ty) list
val defs : State.global_fun list
