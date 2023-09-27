(* simple bitstrings, used to represent unparsed packet payloads *)
type bit = |B0|B1
type bits = bit list

let empty = []
let char_to_bits c =
  match c with
  | '0' -> [B0; B0; B0; B0]
  | '1' -> [B0; B0; B0; B1]
  | '2' -> [B0; B0; B1; B0]
  | '3' -> [B0; B0; B1; B1]
  | '4' -> [B0; B1; B0; B0]
  | '5' -> [B0; B1; B0; B1]
  | '6' -> [B0; B1; B1; B0]
  | '7' -> [B0; B1; B1; B1]
  | '8' -> [B1; B0; B0; B0]
  | '9' -> [B1; B0; B0; B1]
  | 'a' | 'A' -> [B1; B0; B1; B0]
  | 'b' | 'B' -> [B1; B0; B1; B1]
  | 'c' | 'C' -> [B1; B1; B0; B0]
  | 'd' | 'D' -> [B1; B1; B0; B1]
  | 'e' | 'E' -> [B1; B1; B1; B0]
  | 'f' | 'F' -> [B1; B1; B1; B1]
  | _ -> failwith "[hex_to_bits] Invalid hex character"
;;
(* take a string of hex numbers with no delimiters and 
   convert it into a bit list. *)
let rec hexstr_to_bits (str:String.t) : bits = 
  match str with 
  | "" -> []    
  | _ -> 
    let c = String.get str 0 in
    let remaining_str = String.sub str 1 ((String.length str)-1) in
    (char_to_bits c) @ (hexstr_to_bits remaining_str)
;;

let rec bits_to_hexstr (bits:bits) : string = 
  match bits with 
  | [] -> ""
  | b1::b2::b3::b4::bs -> 
    let c = match (b1,b2,b3,b4) with 
      | (B0,B0,B0,B0) -> '0'
      | (B0,B0,B0,B1) -> '1'
      | (B0,B0,B1,B0) -> '2'
      | (B0,B0,B1,B1) -> '3'
      | (B0,B1,B0,B0) -> '4'
      | (B0,B1,B0,B1) -> '5'
      | (B0,B1,B1,B0) -> '6'
      | (B0,B1,B1,B1) -> '7'
      | (B1,B0,B0,B0) -> '8'
      | (B1,B0,B0,B1) -> '9'
      | (B1,B0,B1,B0) -> 'a'
      | (B1,B0,B1,B1) -> 'b'
      | (B1,B1,B0,B0) -> 'c'
      | (B1,B1,B0,B1) -> 'd'
      | (B1,B1,B1,B0) -> 'e'
      | (B1,B1,B1,B1) -> 'f'
    in
    String.make 1 c ^ (bits_to_hexstr bs)
  | _ -> failwith "[bits_to_hexstr] bits must be a multiple of 4"
;;
(* print as a bitstring *)
let rec to_string bits : string = 
  match bits with 
  | [] -> ""
  | B1::bits -> "1" ^ (to_string bits)
  | B0::bits -> "0" ^ (to_string bits)
;;

(* convert an unsigned integer to a bitstring *)
let rec int_to_bits_rev width n : bits = 
  if (width = 0) then []
  else 
    let b = match (n land 1) with 
      | 0 -> B0
      | 1 -> B1
      | _ -> failwith "[int_to_bits] invalid result from n land 1"
    in
    b::(int_to_bits_rev (width-1) (n lsr 1))
;;

let int_to_bits width n = 
  List.rev (int_to_bits_rev width n)
;;
let rec bits_to_int (bits:bits) : int = 
  match bits with
  | [] -> 0
  | b::bs -> 
    let v = match b with 
      | B1 -> 1 lsl (List.length bs)
      | B0 -> 0
    in
    v lor (bits_to_int bs)
;;

(* read n most significant bits into an unsigned int *)
let rec read_msb n bits: int = 
  match bits with 
  | [] -> 0
  | b::bs -> 
    if (n = 0) then 0
    else 
    let v = match b with 
      | B1 -> 1 lsl ((n-1))
      | B0 -> 0
    in
    v lor (read_msb (n-1) bs )
;; 

(* advance to the nth bit *)
let rec advance n bits : bits option =
  match n with
  | 0 -> Some(bits)
  | _ -> (
    match bits with
    | [] -> None
    | _::bs -> advance (n-1) bs
  )    
;;

let pop_msb n bits : (int * bits) option = 
  match advance n bits with 
  | None -> None
  | Some(bits') -> Some(read_msb n bits, bits')
;;

(* concat 2 bitstrings *)
let rec concat bits1 bits2 : bits = 
  match bits1 with 
  | [] -> bits2
  | b::bs -> b::(concat bs bits2)