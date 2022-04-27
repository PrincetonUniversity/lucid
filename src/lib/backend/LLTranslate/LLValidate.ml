(* Check for bugs in the control graph *)
open LLSyntax
open Printf
module CL = Caml.List

exception Error of string

let error s = raise (Error s)

let silent = ref false;;

(* is there a table in cid decls that self loops?  *)
let get_self_loop_tables cid_decls =
  let if_self_loop (cid, decl) =
    match decl with
    | Table _ ->
      (match
         Caml.List.exists (Cid.equals cid) (DFSyntax.succs_of_tid cid_decls cid)
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

(* is there more than one root table (table with 
   no predecessor action?) *)
let only_one_root cid_decls loc_str = 
  let get_root_tables cid_decls = 
    let is_root_table (cid, decl) = 
      match decl with 
        | (Table _) -> (
          match ((DFSyntax.pred_aids_of_tid cid_decls cid) |> CL.length) with 
            | 0 -> true 
            | _ -> false
        )
        | _ -> false
    in 
    CL.filter is_root_table cid_decls
  in 
  match (get_root_tables cid_decls |> CL.length) with 
    | 0 | 1 -> (
      print_endline ("[assertion passed] only one root at "^loc_str)
    )
    | _ -> (
      error ("[ASSERTION FAILED] more than 1 root table at "^loc_str)
    )
;;

let validate_cid_decls cid_decls loc_str = 
  if (not !silent) 
  then (
    no_self_loop_tables cid_decls loc_str;
    only_one_root cid_decls loc_str
  )
;;
