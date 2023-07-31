(*pragmas that are used throughout the compiler. 
  Generally, a pragma is attached to some node in the
  syntax by an earlier pass and then used in a later 
  pass. For example, there's a pragma that function 
  inlining puts on a local variable declaration that 
  indicates it is safe to not initialize that variable 
  when translating to P4 or C. *)
type t = 
  | PNoInitLocal 
    (* this statement declares 
       a local variable that does not need to be 
       initialized.  *)
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

let find_sprag pname pargs prags =
  List.find_opt (sprag_equal pname pargs) prags
;;

let exists_sprag pname pargs = 
  List.exists (sprag_equal pname pargs)
;;

let exists_pnolocal = 
  List.exists (fun prag -> match prag with 
    PNoInitLocal -> true | _ -> false)
;;

let to_string prag = 
  match prag with
  | PNoInitLocal -> "@pnoinitlocal"
  | PStringPrag(name, args) -> Printf.sprintf 
    "@%s(%s)"
    name
    (String.concat ", " args)    
;;

let to_strings prags = 
  List.map to_string prags |> String.concat ", "