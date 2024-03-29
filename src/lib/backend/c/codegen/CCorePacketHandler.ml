(* implementation of the toplevel packet handler function *)
open CCoreSyntax
(* the packet handler is a fixed function that uses the compiler generated functions: 
    - parse_event
    - handle_event
    - deparse_event
*)

(* find a type definition based on its id *)
let rec find_ty_opt ty_cid decls = 
  match decls with 
  | [] -> None
  | decl::decls -> (
    match decl.d with 
    | DTy(cid, Some(ty)) -> 
      if (Cid.equal cid ty_cid) then Some(ty) else (find_ty_opt ty_cid decls)
    | _ -> find_ty_opt ty_cid decls
  )
;;


(* placeholder. We probably don't even need to read anything? *)
let process decls = 
  print_endline ("generating packet handler");
  exit 1;  
  
  decls