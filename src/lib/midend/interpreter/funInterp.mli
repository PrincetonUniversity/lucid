open CoreSyntax
open InterpState


val init_function : decl list -> State.network_state * id * params (* initialize the interpreter to run a single function *)
val run_function : State.network_state -> id -> params -> int list -> int list (* run a single function with given arguments *)
    (* interpret a single-function program *)
