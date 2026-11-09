(* The record is exposed so ppx_import can re-export it in CoreSyntax,
   but treat it as abstract: construct values only through this interface.
   Invariant: bstr is exactly (blen+7)/8 bytes, MSB-first, pad bits zero.
   Values are canonical, so structural equality is semantic equality. *)
type bits =
  { bstr : string
  ; blen : int (* length in bits *)
  }

val empty : bits
val length : bits -> int
val char_to_bits : char -> bits
val hexstr_to_bits : string -> bits
val bits_to_hexstr : bits -> string
val of_byte_string : string -> bits
val to_byte_string : bits -> string
val to_string : bits -> string
val advance : int -> bits -> bits option
val peek_msb : int -> bits -> int option
val pop_msb : int -> bits -> (int * bits) option
val concat : bits -> bits -> bits
(* peek_msb / pop_msb as Z.t, for fields of 63 bits or more *)
val peek_msb_z : int -> bits -> Z.t option
val pop_msb_z : int -> bits -> (Z.t * bits) option
val int_to_bits : int -> int -> bits
(* the low [width] bits of a Z.t, for values that don't fit an OCaml int *)
val z_to_bits : int -> Z.t -> bits
val bits_to_int : bits -> int
val to_ints : bits -> int list
val of_ints : int list -> bits
