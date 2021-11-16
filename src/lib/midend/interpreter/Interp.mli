open CoreSyntax
open InterpState

val initialize : Renaming.env -> string -> decl list -> State.network_state
val simulate : State.network_state -> State.network_state
