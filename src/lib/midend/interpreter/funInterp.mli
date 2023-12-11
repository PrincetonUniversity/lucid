open CoreSyntax
open InterpState

(* interpret a single-function program *)
type fctx
val init_function : decl list -> fctx 
    (* initialize the interpreter to run a single function *)
val run_function : fctx -> int list -> int list 
    (* run a single function with given arguments *)
