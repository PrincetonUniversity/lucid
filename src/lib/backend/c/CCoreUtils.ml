open CCoreSyntax
open CCoreExceptions

(* [xs with [n] := x] *)
let replace n x xs = 
  let rec replace' n x xs acc = 
    match xs with 
    | [] -> List.rev acc
    | y::ys -> 
      if n = 0 then 
        List.rev_append (x::acc) ys
      else 
        replace' (n-1) x ys (y::acc)
  in
  replace' n x xs []
;;
let id = Id.create

let is_smatch statement = match statement.s with 
  | SMatch _ -> true 
  | _ -> false
;;

(* monomorphization and code gen sometimes needs type names *)
let ty_to_namestr ty = match ty.raw_ty with 
  | TInt _ | TBool | TAbstract _ -> CCorePPrint.ty_to_string ~use_abstract_name:true ty
  | _ -> err_expected_ty ty "to convert a type to a string for a generated function, the type must be an int, bool, or abstract"
;;
let cid_for_ty cid ty = 
  Cid.str_cons_plain (ty_to_namestr ty) cid
;;
