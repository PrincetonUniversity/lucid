open CoreSyntax
open InterpState

val initialize : Renaming.env -> string -> decl list -> State.network_state * Preprocess.t * InterpSpec.t
val simulate : State.network_state -> State.network_state
val run : Preprocess.t -> Renaming.env -> InterpSpec.t -> State.network_state -> State.network_state