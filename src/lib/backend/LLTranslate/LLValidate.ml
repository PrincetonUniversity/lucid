(* Check for bugs in the control graph *)
open LLSyntax
module CL = Caml.List

exception Error of string

let error s = raise (Error s)

(* is there a table in cid decls that self loops?  *)
let get_self_loop_tables cid_decls =
  let if_self_loop (cid, decl) =
    match decl with
    | Table _ ->
      (match
         Caml.List.exists
           (Cid.equals cid)
           (DFSyntax.succs_of_tid cid_decls cid)
       with
      | true -> Some (cid, decl)
      | false -> None)
    | _ -> None
  in
  CL.filter_map if_self_loop cid_decls
;;

let no_self_loop_tables cid_decls loc_str =
  let self_loopers = get_self_loop_tables cid_decls in
  match CL.length self_loopers with
  | 0 -> ()
  | _ ->
    print_endline
      ("self looping tables at "
      ^ loc_str
      ^ "\n"
      ^ DebugPrint.str_of_cid_decls self_loopers);
    error "SELF LOOP"
;;

let validate_cid_decls cid_decls loc_str = no_self_loop_tables cid_decls loc_str
