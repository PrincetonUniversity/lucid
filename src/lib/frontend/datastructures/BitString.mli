type bit = |B0|B1

type bits = bit list

val empty : bits
val char_to_bits : char -> bits
val hexstr_to_bits : string -> bits
val bits_to_hexstr : bits -> string
val to_string : bits -> string
val read_msb : int -> bits -> int
val advance :  int -> bits -> bits option
val pop_msb : int -> bits -> (int * bits) option
val concat  : bits -> bits -> bits
val int_to_bits : int -> int -> bits
val bits_to_int : bits -> int