(* control program generation for the tofino *)

(* Steps:
  1. replace all control handlers with handlers that 
     use generate_port to send the same event to the CPU.
  2. generate fixed libraries
  3. translate control handler bodies into functions. *)

open CoreSyntax

(* make a statement: 
  generate(evid(evargs)) *)
let forward_to_control ctlport ctlportwidth id params  = 
  let ev_exp = call_sp
    (Cid.id id)
    (List.map (fun (id,ty) -> exp_of_id id ty) params)
    (ty TEvent)
    Span.default
  in
  gen_sp
    (GPort(vint_exp ctlport ctlportwidth))
    ev_exp
    Span.default
;;

let _split_program ctlport ctlportwidth (ctl_ds, data_ds) decl =
  match decl.d with
  (* control handlers get transformed into 
     handlers that forward to the control plane  *)
  | DHandler(id, HControl, body) -> 
    let (params, _) = body in
    let stmt' = forward_to_control ctlport ctlportwidth id params in
    let data_handler = {decl with d=DHandler(id, HData, (params, stmt'))} in
    let control_handler = decl in
    ctl_ds@[control_handler], data_ds@[data_handler]
  (* everything else gets put into both programs *)
  | _ ->
    ctl_ds@[decl], data_ds@[decl]
;;

(* split a program into data and control plane 
   components. 
   - The control plane component 
   contains all declarations but only control handlers.  
   - The data plane component contains all declarations, 
     but with control handlers replaced with handlers that 
     simply forward the event to the CPU *)
let split_program ctlport ctlportwidth (ds:decls) =
  List.fold_left 
    (_split_program ctlport ctlportwidth)
    ([], [])
    ds
;;

(* now, we need to generate the program. 
  The control and data plane component need to agree on:
    - ids of globals and actions
    - ids and iids of events / handlers *)

