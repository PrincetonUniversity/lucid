(* helpers to generate all bit vectors of certain lengths. Not currently used anywhere *)
open TofinoCore
open InterpHelpers
open P4TofinoSyntax
module T = P4TofinoSyntax 
module C = TofinoCore
module CS = CoreSyntax
module CL = 

(* bitvec helpers for some combinatorial logic *)  
let rec zeros len =
  match len with
  | 0 -> []
  | 1 -> [0]
  | _ -> 0 :: zeros (len - 1)
;;

let rec bv_ones len =
  match len with
  | 0 -> []
  | 1 -> [1]
  | _ -> 1 :: bv_ones (len - 1)
;;
type bv = int list

(* generate all bitvectors of length len that have n elements set to 1 *)
let rec gen_bv len n : bv list =
  match len = n with
  | true -> [bv_ones len]
  | false ->
    (match n with
    | 0 -> [zeros len]
    | _ ->
      let suffixes_with_one = gen_bv (len - 1) (n - 1) in
      let suffixes_with_zero = gen_bv (len - 1) n in
      let prepend v suffix = v :: suffix in
      let result_with_one = CL.map (prepend 1) suffixes_with_one in
      let result_with_zero = CL.map (prepend 0) suffixes_with_zero in
      result_with_one @ result_with_zero)
;;


let rec wildcard_up_to_first_one pats = 
  match pats with 
  | [] -> []
  | T.PNum(0)::pats -> T.PWild::(wildcard_up_to_first_one pats)
  | pats -> (* if it doesn't start with 0, stop *) pats
;;
let wildcard_after_last_one pats =
  pats |> List.rev  |> wildcard_up_to_first_one |> List.rev
;;
let pats_of_bitvec bv =
  List.map pnum bv
;;

let rec nth_pos pos x lst =
  match lst with
  | [] -> error "[find_first] could not find element in list."
  | hd :: lst -> if hd = x then pos else nth_pos (pos + 1) x lst
;;
let first_pos = nth_pos 0
(* find the last position (index) in lst that has value x *)
let last_pos x lst =
  let from_end = CL.rev lst |> first_pos x in
  CL.length lst - 1 - from_end
;;
let str_of_bitvec bv = CL.map string_of_int bv |> String.concat ";"

