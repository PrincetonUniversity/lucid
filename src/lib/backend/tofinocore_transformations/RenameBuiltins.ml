(* rename builtins in a tofinocore program. 
    1. EVar ingress_port -> EVar (some header of main)
    2. ECall (Sys.ts())  -> EVar (some header of main)
    3. ECall (Sys.random()) -> Hmm can't really eliminate this. Its a weird hash op. 
       okay so needs to be back in the translation to IR I guess. 
*)