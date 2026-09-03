(* simple bitstrings, used to represent unparsed packet payloads.
   Represented as an immutable byte string plus a bit length.
   Bits are stored MSB-first: bit i lives in byte (i/8), at mask (0x80 lsr (i mod 8)).
   Canonical form invariant: `bstr` is exactly (blen+7)/8 bytes and any pad bits
   in the final byte are zero. Every operation returns a canonical value, so
   structural equality on `bits` is semantic equality. *)
type bits =
  { bstr : string
  ; blen : int (* length in bits *)
  }

let empty = { bstr = ""; blen = 0 }
let length bits = bits.blen

(* read bit i (0-indexed from the MSB); assumes i < blen *)
let get_bit bs i =
  (Char.code (String.unsafe_get bs.bstr (i lsr 3)) lsr (7 - (i land 7))) land 1
;;

(* build a canonical bits of length len whose ith bit is f i *)
let init_bits len f =
  let nbytes = (len + 7) / 8 in
  let b = Bytes.make nbytes '\000' in
  for i = 0 to len - 1 do
    if f i = 1
    then
      Bytes.unsafe_set
        b
        (i lsr 3)
        (Char.unsafe_chr (Char.code (Bytes.unsafe_get b (i lsr 3)) lor (0x80 lsr (i land 7))))
  done;
  { bstr = Bytes.unsafe_to_string b; blen = len }
;;

let hex_char_to_int c =
  match c with
  | '0' .. '9' -> Char.code c - Char.code '0'
  | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
  | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
  | _ -> failwith "[hex_to_bits] Invalid hex character"
;;

(* take a string of hex numbers with no delimiters and
   convert it into a bitstring. *)
let hexstr_to_bits (str : String.t) : bits =
  let n = String.length str in
  let b = Bytes.make ((n + 1) / 2) '\000' in
  for i = 0 to n - 1 do
    let v = hex_char_to_int (String.get str i) in
    let cur = Char.code (Bytes.get b (i lsr 1)) in
    let nv = if i land 1 = 0 then cur lor (v lsl 4) else cur lor v in
    Bytes.set b (i lsr 1) (Char.chr nv)
  done;
  { bstr = Bytes.unsafe_to_string b; blen = 4 * n }
;;

let char_to_bits c = hexstr_to_bits (String.make 1 c)

let bits_to_hexstr (bits : bits) : string =
  if bits.blen mod 4 <> 0 then failwith "[bits_to_hexstr] bits must be a multiple of 4";
  String.init (bits.blen / 4) (fun i ->
    let byte = Char.code (String.get bits.bstr (i lsr 1)) in
    let v = if i land 1 = 0 then byte lsr 4 else byte land 0xf in
    "0123456789abcdef".[v])
;;

(* raw byte string conversions. of_byte_string is where packet payloads enter;
   because the representation is bytes, both directions are (at most) one copy. *)
let of_byte_string (s : string) : bits = { bstr = s; blen = 8 * String.length s }

let to_byte_string (bits : bits) : string =
  if bits.blen mod 8 <> 0 then failwith "[to_byte_string] bits must be a multiple of 8";
  bits.bstr
;;

(* print as a bitstring *)
let to_string bits : string =
  String.init bits.blen (fun i -> if get_bit bits i = 1 then '1' else '0')
;;

(* convert an unsigned integer to a bitstring *)
let int_to_bits width n : bits =
  let b = Bytes.make ((width + 7) / 8) '\000' in
  let x = ref n in
  for j = 0 to width - 1 do
    if !x land 1 = 1
    then begin
      let i = width - 1 - j in
      Bytes.set b (i lsr 3) (Char.chr (Char.code (Bytes.get b (i lsr 3)) lor (0x80 lsr (i land 7))))
    end;
    x := !x lsr 1
  done;
  { bstr = Bytes.unsafe_to_string b; blen = width }
;;

let bits_to_int (bits : bits) : int =
  let r = ref 0 in
  for i = 0 to bits.blen - 1 do
    r := (!r lsl 1) lor get_bit bits i
  done;
  !r
;;

(* advance to the nth bit, return new string *)
let advance n bits : bits option =
  if n < 0 || n > bits.blen
  then None
  else if n land 7 = 0
  then (
    (* byte-aligned fast path: copy the remaining bytes. pad bits of the
       final byte are unchanged, so canonical form holds. *)
    let len = bits.blen - n in
    Some { bstr = String.sub bits.bstr (n lsr 3) ((len + 7) / 8); blen = len })
  else (
    (* unaligned: rebuild bit-by-bit *)
    let len = bits.blen - n in
    Some (init_bits len (fun i -> get_bit bits (i + n))))
;;

(* read n bits to unsigned int without advancing. *)
let peek_msb n bits : int option =
  if n > bits.blen
  then None
  else (
    let r = ref 0 in
    for i = 0 to n - 1 do
      r := (!r lsl 1) lor get_bit bits i
    done;
    Some !r)
;;

(* read n bits to unsigned int and advance. *)
let pop_msb n bits : (int * bits) option =
  match advance n bits, peek_msb n bits with
  | Some bits', Some v -> Some (v, bits')
  | _ -> None
;;

(* concat 2 bitstrings *)
let concat bits1 bits2 : bits =
  if bits1.blen land 7 = 0
  then { bstr = bits1.bstr ^ bits2.bstr; blen = bits1.blen + bits2.blen }
  else
    init_bits (bits1.blen + bits2.blen) (fun i ->
      if i < bits1.blen then get_bit bits1 i else get_bit bits2 (i - bits1.blen))
;;

(* conversions to/from lists of 0/1 ints, for compile-time translation passes *)
let to_ints (bits : bits) : int list = List.init bits.blen (fun i -> get_bit bits i)

let of_ints (is : int list) : bits =
  let arr = Array.of_list is in
  Array.iter (fun i -> if i <> 0 && i <> 1 then failwith "[of_ints] invalid bit int") arr;
  init_bits (Array.length arr) (fun i -> arr.(i))
;;
