open CoreSyntax
open InterpState

val initialize : Renaming.env -> string -> decl list -> network_state * Preprocess.t * InterpSpec.t
val simulate : network_state -> network_state
val run : Preprocess.t -> Renaming.env -> InterpSpec.t -> network_state -> network_state
val initialize_softswitch : Renaming.env -> decl list -> network_state * Preprocess.t * InterpSpec.t
val run_softswitch : Preprocess.t -> Renaming.env -> InterpSpec.t -> network_state -> network_state