(* CCore system function implementations (time, random, and hasn)  *)
open CCoreSyntax
open CCoreUtils
let sys_time = 
  dfun_foriegn
    System.sys_time_cid
    (tint 32)
    []
    "{ return 0;// TODO! }"


let port_ty = tint ((!CCoreConfig.cfg).port_id_size)
;;

let sys_flood = 
  dfun_foriegn
    (cid"sys_flood")
    (port_ty)
    [id"port", port_ty]
    "{ return port + 10000;// TODO! }"
;;
let flood_call_replacer exp = 
  if (is_ecall_cid exp (cid"flood")) then (
    let _, args = extract_ecall exp in 
    let flood_ty = tfun [port_ty] port_ty in
    ecall (efunref (cid"sys_flood") flood_ty) args
  )
  else exp
;;
let group_ty_replacer ty = 
  if (is_tbuiltin tgroup_cid ty) then
    port_ty
  else ty
;;

    

let process decls = 
  sys_time::sys_flood:: (*add the functions, replace group types with port types *)
  ( decls 
    |> CCoreTransformers.subst_ty#visit_decls group_ty_replacer
    |> CCoreTransformers.subst_exp#visit_decls flood_call_replacer
  )
