(* CCore system function implementations (time, random, and hasn)  *)
open CCoreSyntax
open CCoreExceptions
open CCoreUtils
open Str

(* for now, group type is an alias for port type *)
let new_group_ty = tint ((!CCoreConfig.cfg).port_id_size) ;;
let group_ty_replacer ty = 
  if (is_tbuiltin tgroup_cid ty) then
    new_group_ty
  else ty
;;

(* Sys.time *)
let sys_time = 
  dfun_foriegn
    System.sys_time_cid
    (tint 32)
    []
    "{ return 0; /*TODO!*/ }"

(* Flood was a core builtin, now a Sys function *)
let sys_flood = 
  dfun_foriegn
    (cid"flood")
    (new_group_ty)
    [id"port", new_group_ty]
    "{ return port + 10000;/* TODO!*/ }"
;;
(* let flood_call_replacer exp = 
  if (is_ecall_cid exp (cid"flood")) then (
    let _, args = extract_ecall exp in 
    let flood_ty = tfun [new_group_ty] new_group_ty in
    ecall (efunref (cid"sys_flood") flood_ty) args
  )
  else exp
;; *)

let hash_fun size = 
  dfun_foriegn 
    (cid("hash_"^(string_of_int size)))
    (tint size)
    [id"seed", tint 32; id"str", tref (tint 8); id"length", tint 32]
{|{
  int hashValue = seed;
  for (int i = 0; i < length; i++) {
      hashValue += str[i];
  }
  return hashValue;
}|}


let process decls = 
  sys_time::sys_flood:: (*add the functions, replace group types with port types *)
  hash_fun 32::
  ( decls 
    |> CCoreTransformers.subst_ty#visit_decls group_ty_replacer
    (* |> CCoreTransformers.subst_exp#visit_decls flood_call_replacer *)
  )