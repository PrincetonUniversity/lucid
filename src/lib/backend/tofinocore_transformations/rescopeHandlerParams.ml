(* functions to rescope handler parameters after 
  event / handler transformations.   
  Rescopes both the input parameters and the output parameters (generate args)
*)

open CoreSyntax
open TofinoCoreNew
open BackendLogging

(* left off here *)
(* scope all the parameter variables that appear in a statement. *)
let rec scope_outputs (stmt : statement) = 
  match stmt.s with
  | SAssign _ -> failwith "TODO -- check if its an event variable that gets generated"
  | SLocal _ -> failwith "TODO -- check if its an event variable that gets generated"
  | SGen _ -> failwith "TODO -- case ECall: rename event id with prefix of out"
  | _ -> failwith "TODO: I think these are all noop cases"
;;

(* scope all the parameter variables. I think this is just about 
   expressions.  *)
let rec scope_inputs (stmt : statement) = 
  let _ = stmt in 
  (* use visitor to visit evars. If an evar's cid is a parameter, 
     prefix with in arg *)
  failwith "not done"
;;

let update_handler_scopes (hdl : handler) : handler =
  match hdl with
  | HParams _ -> hdl 
  | HEvent _ -> hdl

;;

let update_scopes prog : prog = 
  
  
  prog