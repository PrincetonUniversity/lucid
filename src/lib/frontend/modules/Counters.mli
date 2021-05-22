open InterpState

val module_id : Id.t
val t_id : Cid.t
val sizes_labels : Id.t list * Syntax.params
val constructors : (Cid.t * (Cid.t * Id.t list * Syntax.raw_ty list)) list
val defs : State.global_fun list
