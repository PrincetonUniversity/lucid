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


(* ---- hash<16>(checksum, ...): the standard internet (RFC 1071) checksum ----
   Computed over wire-format bit-concatenation of args 
   (network ordering, no padding). A partial final byte / odd final byte 
   is zero-padded on the right, per the RFC. Note the interpreter 
   does not handle this edge case correctly (in calc_crc16_csum it does
   a left pad). Each arg sig gets a C function. Helpers for bit packing 
   and ones complement. *)
let checksum_helpers = dforiegn {|
/* write the low n bits of v into buf at bit offset `off`, MSB-first.
   buf must be pre-zeroed (bits are only OR'd in). */
static void csum_put_bits(uint8_t* buf, uint32_t off, uint64_t v, int n) {
    for (int i = 0; i < n; i++) {
        if ((v >> (n - 1 - i)) & 1) {
            uint32_t b = off + i;
            buf[b >> 3] |= (uint8_t)(1u << (7 - (b & 7)));
        }
    }
}
/* RFC 1071 ones-complement sum of buf[0..nbytes), 16-bit big-endian words;
   an odd final byte is the high byte of its word (right zero-padding). */
static uint16_t csum16_fold(const uint8_t* buf, uint32_t nbytes) {
    uint32_t sum = 0;
    for (uint32_t i = 0; i + 1 < nbytes; i += 2) sum += ((uint32_t)buf[i] << 8) | buf[i+1];
    if (nbytes & 1) sum += (uint32_t)buf[nbytes-1] << 8;
    while (sum >> 16) sum = (sum & 0xFFFF) + (sum >> 16);
    return (uint16_t)~sum;
}|}
;;

(* one checksum function per distinct field-type signature: pack the fields
   into a zeroed stack buffer at their static wire offsets, then fold. *)
let mk_csum_fun fcid ftys =
  let widths = List.map sizeof_ty ftys in
  let total = List.fold_left (+) 0 widths in
  let nbytes = (total + 7) / 8 in
  let params = List.mapi (fun i fty -> cid ("f"^string_of_int i), fty) ftys in
  let puts, _ = List.fold_left2
    (fun (lines, off) i w ->
      (Printf.sprintf "  csum_put_bits(buf, %d, (uint64_t)f%d, %d);" off i w)::lines, off + w)
    ([], 0) (List.init (List.length widths) (fun i -> i)) widths
  in
  dfun_foriegn fcid (tint 16) params
    (Printf.sprintf "{\n  uint8_t buf[%d] = {0};\n%s\n  return csum16_fold(buf, %d);\n}"
      nbytes (String.concat "\n" (List.rev puts)) nbytes)
;;

let process_checksums decls =
  let checksum_cid = Cid.id Builtins.checksum_id in
  (* signature key -> (function cid, function ty); keyed on the printable
     field kinds so bool and int<1> args get distinct functions *)
  let csum_tbl = Hashtbl.create 4 in
  let sig_key ftys = String.concat "_"
    (List.map (fun t -> if is_tbool t then "b" else "i"^string_of_int (sizeof_ty t)) ftys)
  in
  let visitor = object (_) inherit [_] s_map as super
    method! visit_exp () exp =
      match exp.e with
      (* match the whole hash op before recursing, so the seed var below it
         isn't flagged by the stray-checksum-var case *)
      | EOp(Hash size, [{e=EVar seed_cid; _}; arg]) when Cid.equal seed_cid checksum_cid ->
        let arg = super#visit_exp () arg in
        if size <> 16 then
          ty_err "the checksum builtin is 16 bits wide -- use hash<16>(checksum, ...)";
        let fields = match arg.e with
          | ETuple es -> es
          | _ -> [arg]
        in
        List.iter (fun f ->
          if not (is_tint f.ety || is_tbool f.ety) then
            ty_err "checksum args must be ints or bools (flatten compound args into fields)")
          fields;
        let ftys = List.map (fun f -> f.ety) fields in
        let fcid, fty = match Hashtbl.find_opt csum_tbl (sig_key ftys) with
          | Some (fcid_fty, _) -> fcid_fty
          | None ->
            let fcid = cid ("checksum16_"^string_of_int (Hashtbl.length csum_tbl)) in
            let fty = tfun ftys (tint 16) in
            Hashtbl.replace csum_tbl (sig_key ftys) ((fcid, fty), ftys);
            fcid, fty
        in
        ecall (efunref fcid fty) fields
      | EVar var_cid when Cid.equal var_cid checksum_cid ->
        ty_err "the checksum builtin may only be used as a hash seed: hash<16>(checksum, ...)"
      | _ -> super#visit_exp () exp
  end
  in
  let decls = List.map (visitor#visit_decl ()) decls in
  let csum_funs = Hashtbl.fold
    (fun _ ((fcid, _), ftys) acc -> mk_csum_fun fcid ftys::acc)
    csum_tbl []
  in
  match csum_funs with
  | [] -> decls
  | _ -> checksum_helpers::csum_funs @ decls
;;

let process decls =
  sys_flood:: (*add the functions, replace group types with port types *)
  hash_fun 32::(process_checksums decls)
