(* CCore system function implementations (time, random, and hasn)  *)
open CCoreSyntax
open CCoreExceptions
open CCoreUtils
open Str

(* for now, group type is an alias for port type *)
let port_ty = tint (CConfig.c_cfg.port_id_size) ;;

(* Sys.time() is not a function here: CCoreHandlers rewrites each call to a read of the
   current event's meta.timestamp, which the driver stamps at dequeue (see
   CCoreSyntax.event_timestamp / replace_sys_time). *)

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
    [cid"seed", tint 32; cid"str", tref (tint 8); cid"len_bits", tint 32]
{|{
  // len_bits is the value's bit width. Sum the whole bytes, then add the last
  // partial byte masked to its valid low bits (the value is stored little-endian,
  // so the leftover bits live in the low end of the final byte). Placeholder hash
  // (a byte sum) -- collisions are not a concern yet.
  int hashValue = seed;
  uint32_t full_bytes = len_bits / 8;
  for (uint32_t i = 0; i < full_bytes; i++) {
      hashValue += str[i];
  }
  uint32_t rem = len_bits % 8;
  if (rem != 0) {
      hashValue += str[full_bytes] & ((1 << rem) - 1);
  }
  return hashValue;
}|}


let process decls =
  sys_flood:: (*add the functions, replace group types with port types *)
  hash_fun 32::decls
