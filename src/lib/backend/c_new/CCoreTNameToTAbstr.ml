(* 
  convert all user-defined TNames into TAbstracts, 
  which carry the concrete type in their value.
*)

open CCoreSyntax 

let rec get_ty_defs decls = 
  match decls with 
  | [] -> []
  | ({d=DTy(cid, Some(ty))})::decls -> 
    (ty, cid) :: get_ty_defs decls
  | _::decls -> get_ty_defs decls
;;

let transform_ty ty_def_assoc ty = 
  let concrete_ty_cid_opt = List.find_opt 
    (fun (declared_concrete_ty, _) -> 
      equiv_tys declared_concrete_ty ty)
      ty_def_assoc
  in
  match concrete_ty_cid_opt with
  | Some (_, cid) -> tabstract_cid cid ty
  | None -> ty
;;  
let process decls = 
  let ty_def_assoc = get_ty_defs decls in
  CCoreTransformers.subst_ty#visit_decls (transform_ty ty_def_assoc) decls  
