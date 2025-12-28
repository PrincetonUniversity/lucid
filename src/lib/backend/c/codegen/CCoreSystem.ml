(* CCore system function implementations (time, random, and hasn)  *)
open CCoreSyntax
open CCoreExceptions
open CCoreUtils
open Str

(* for now, group type is an alias for port type *)
let port_ty = tint (Config.c_cfg.port_id_size) ;;

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
    (port_ty)
    [cid"port", port_ty]
    "{ return port + 10000;/* TODO!*/ }"
;;

let hash_fun size = 
  dfun_foriegn 
    (cid("hash_"^(string_of_int size)))
    (tint size)
    [cid"seed", tint 32; cid"str", tref (tint 8); cid"length", tint 32]
{|{
  int hashValue = seed;
  for (int i = 0; i < length; i++) {
      hashValue += str[i];
  }
  return hashValue;
}|}


let process decls = 
  sys_time::sys_flood:: (*add the functions, replace group types with port types *)
  hash_fun 32::decls
