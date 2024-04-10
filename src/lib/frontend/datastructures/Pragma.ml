(*pragmas that are used throughout the compiler. 
  Generally, a pragma is attached to some node in the
  syntax by an earlier pass and then used in a later 
  pass. For example, there's a pragma that function 
  inlining puts on a local variable declaration that 
  indicates it is safe to not initialize that variable 
  when translating to P4 or C. *)
  open Cid
  type cid = [%import: (Cid.t[@opqaue])]

  type t = 
  | PNoInitLocal 
    (* this statement declares 
       a local variable that does not need to be 
       initialized.  *)
  | POverlay of Cid.t * Cid.t
    (* two variables declared in this handler should be overlaid *)
  | PStringPrag of string * string list
  (* PStringPrag holds a pragma name string and 
    a list of string arguments. This is mainly 
    for pragmas that are temporary or in development. 
    Once a pragma is stable, it should be given a 
    proper constructor. *)
     
let sprag name args = PStringPrag(name, args)

let sprag_equal pname pargs prag = 
  match prag with 
  | PStringPrag(name, args) -> 
      (pname = name) & (List.for_all2 (=) pargs args)
  | _ -> false
;;
let sprag_name_equal pname prag = 
  match prag with 
  | PStringPrag(name, _) -> (pname = name)
  | _ -> false
;;
let find_sprag_args pname pargs prags =
  List.find_opt (sprag_equal pname pargs) prags
;;

let find_sprag pname prags = 
  match List.find_opt (sprag_name_equal pname) prags with 
  | Some(PStringPrag(name, args)) -> Some(name, args)
  | _ -> None
;;

let exists_sprag pname = 
  List.exists (sprag_name_equal pname)
;;
let exists_sprag_args pname pargs = 
  List.exists (sprag_equal pname pargs)
;;

let exists_pnolocal = 
  List.exists (fun prag -> match prag with 
    PNoInitLocal -> true | _ -> false)
;;

let to_string prag = 
  match prag with
  | PNoInitLocal -> "@pnoinitlocal"
  | POverlay(cid1, cid2) -> Printf.sprintf 
    "@overlay(%s, %s)"
    (Cid.to_string cid1)
    (Cid.to_string cid2)
  | PStringPrag(name, args) -> Printf.sprintf 
    "@%s(%s)"
    name
    (String.concat ", " args)    
;;

let to_strings prags = 
  List.map to_string prags |> String.concat ", "