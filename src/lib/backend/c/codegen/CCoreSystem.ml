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

(* ---- hash<size>(seed, ...): every seed but `checksum` ----
   A hash is computed over its args' wire-format bit concatenation (MSB-first,
   no padding), packed at the call site (mk_hash_args_fun), so that its input
   doesn't depend on C struct layout. hash_64 is the hash over that bit string.
   Seed 1 is the identity hash (as in the interpreter): the string's low bits.
   Every other seed is a placeholder byte sum for now, so its values are
   specific to this backend. *)
let hash_fun =
  dfun_foriegn
    (cid "hash_64")
    (tint 64)
    [cid"seed", tint 32; cid"buf", tref (tint 8); cid"len_bits", tint 32]
{|{
  if (seed == 1) {
    // the identity hash: the low 64 bits of the bit string (the caller keeps
    // the low <size> bits)
    uint64_t v = 0;
    for (uint32_t i = len_bits > 64 ? len_bits - 64 : 0; i < len_bits; i++)
      v = (v << 1) | ((buf[i >> 3] >> (7 - (i & 7))) & 1);
    return v;
  }
  // placeholder hash (a byte sum of the zero-padded bit string)
  uint64_t h = seed;
  for (uint32_t i = 0; i < (len_bits + 7) / 8; i++) h += buf[i];
  return h;
}|}
;;

(* ---- hash<16>(checksum, ...): the standard internet (RFC 1071) checksum ----
   Computed over wire-format bit-concatenation of args 
   (network ordering, no padding). A partial final byte / odd final byte 
   is zero-padded on the right, per the RFC. Note the interpreter 
   does not handle this edge case correctly (in calc_crc16_csum it does
   a left pad). Each arg sig gets a C function. Helpers for bit packing 
   and ones complement. *)
let put_bits_helper = dforiegn {|
/* write the low n bits of v into buf at bit offset `off`, MSB-first.
   buf must be pre-zeroed (bits are only OR'd in). Packs hash and checksum args. */
static void put_bits(uint8_t* buf, uint32_t off, uint64_t v, int n) {
    for (int i = 0; i < n; i++) {
        if ((v >> (n - 1 - i)) & 1) {
            uint32_t b = off + i;
            buf[b >> 3] |= (uint8_t)(1u << (7 - (b & 7)));
        }
    }
}|}
;;

let checksum_helpers = dforiegn {|
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

(* C statements that pack fields f0, f1, ... (of types ftys) into a zeroed
   stack buffer `buf` at their static wire offsets; returns (lines, bits, bytes) *)
let pack_fields ftys =
  let widths = List.map sizeof_ty ftys in
  let total = List.fold_left (+) 0 widths in
  let nbytes = (total + 7) / 8 in
  let puts, _ = List.fold_left2
    (fun (lines, off) i w ->
      (Printf.sprintf "  put_bits(buf, %d, (uint64_t)f%d, %d);" off i w)::lines, off + w)
    ([], 0) (List.init (List.length widths) (fun i -> i)) widths
  in
  Printf.sprintf "  uint8_t buf[%d] = {0};\n%s" (max nbytes 1) (String.concat "\n" (List.rev puts)),
  total, nbytes
;;

let field_params ftys = List.mapi (fun i fty -> cid ("f"^string_of_int i), fty) ftys

(* one checksum function per distinct field-type signature: pack the fields,
   then fold. *)
let mk_csum_fun fcid ftys =
  let pack, _, nbytes = pack_fields ftys in
  dfun_foriegn fcid (tint 16) (field_params ftys)
    (Printf.sprintf "{\n%s\n  return csum16_fold(buf, %d);\n}" pack nbytes)
;;

(* one hash function per (seed type, size, field types): pack the fields, hash
   the bit string with hash_64, keep the low [size] bits *)
let mk_hash_args_fun fcid seed_ty size ftys =
  let pack, total, _ = pack_fields ftys in
  let mask = if size >= 64 then "" else Printf.sprintf " & 0x%LxULL" (Int64.pred (Int64.shift_left 1L size)) in
  dfun_foriegn fcid (tint size) ((cid "seed", seed_ty)::field_params ftys)
    (Printf.sprintf "{\n%s\n  return hash_64((uint32_t)seed, buf, %d)%s;\n}" pack total mask)
;;

(* lucidcc's implementation of hash: each hash op becomes a call to a generated
   function for its signature, which packs the args into their wire-format bit
   string and then computes the checksum (seed `checksum`) or hash_64 (any
   other seed). *)
let process_hashes decls =
  let checksum_cid = Cid.id Builtins.checksum_id in
  let ty_key t = if is_tbool t then "b" else "i"^string_of_int (sizeof_ty t) in
  (* signature key -> ((function cid, function ty), make the function); keyed on
     the printable field kinds so bool and int<1> args get distinct functions *)
  let funs = Hashtbl.create 4 in
  let fun_for key mk_cid fty mk_fun =
    match Hashtbl.find_opt funs key with
    | Some ((fcid, fty), _) -> fcid, fty
    | None ->
      let fcid = mk_cid (Hashtbl.length funs) in
      Hashtbl.replace funs key ((fcid, fty), mk_fun fcid);
      fcid, fty
  in
  let fields_of what arg =
    let fields = match arg.e with ETuple es -> es | _ -> [arg] in
    List.iter (fun f ->
      if not (is_tint f.ety || is_tbool f.ety) then
        ty_err (what^" args must be ints or bools (flatten compound args into fields)"))
      fields;
    fields
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
        let fields = fields_of "checksum" arg in
        let ftys = List.map (fun f -> f.ety) fields in
        let key = "csum_" ^ String.concat "_" (List.map ty_key ftys) in
        let fcid, fty = fun_for key
          (fun n -> cid ("checksum16_"^string_of_int n)) (tfun ftys (tint 16))
          (fun fcid -> mk_csum_fun fcid ftys)
        in
        ecall (efunref fcid fty) fields
      | EOp(Hash size, [seed; arg]) ->
        let seed = super#visit_exp () seed in
        let arg = super#visit_exp () arg in
        let fields = fields_of "hash" arg in
        let ftys = List.map (fun f -> f.ety) fields in
        let key = Printf.sprintf "hash%d_%s_%s" size (ty_key seed.ety)
          (String.concat "_" (List.map ty_key ftys)) in
        let fcid, fty = fun_for key
          (fun n -> cid ("hash_args_"^string_of_int n)) (tfun (seed.ety::ftys) (tint size))
          (fun fcid -> mk_hash_args_fun fcid seed.ety size ftys)
        in
        ecall (efunref fcid fty) (seed::fields)
      | EVar var_cid when Cid.equal var_cid checksum_cid ->
        ty_err "the checksum builtin may only be used as a hash seed: hash<16>(checksum, ...)"
      | _ -> super#visit_exp () exp
  end
  in
  let decls = List.map (visitor#visit_decl ()) decls in
  let gen = Hashtbl.fold (fun key (_, f) acc -> (key, f)::acc) funs [] |> List.sort compare in
  let is_csum (k, _) = String.length k > 5 && String.sub k 0 5 = "csum_" in
  let csums, hashes = List.partition is_csum gen in
  match gen with
  | [] -> decls
  | _ ->
    [put_bits_helper]
    @ (if csums = [] then [] else checksum_helpers::List.map snd csums)
    @ (if hashes = [] then [] else hash_fun::List.map snd hashes)
    @ decls
;;

let process decls =
  sys_flood:: (*add the functions, replace group types with port types *)
  process_hashes decls
;;
