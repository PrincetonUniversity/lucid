type t = string * int [@@deriving show]

(* Should be a character which the NV parser doesn't allow in identifiers *)
let delim = "~"
let counter = ref 0

let next () =
  counter := !counter + 1;
  !counter
;;

let reset () = counter := 0
let fresh s = s, next ()
let create s = s, 0
let name (s, _) = s
let to_id (s, i) = s, i
let from_id (s, i) = s, i
let to_string (s, i) = s ^ delim ^ string_of_int i
let to_string_delim d (s, i) = s ^ d ^ string_of_int i
let prepend_string p (s, i) = p ^ s, i
let append_string p (s, i) = s ^ p, i
let refresh (s, _) = fresh s
let freshen = refresh

let of_id_string s =
  try
    let v, i = BatString.rsplit s ~by:"~" in
    v, int_of_string i
  with
  | _ -> failwith @@ Printf.sprintf "of_var_string: %s has wrong format" s
;;

let equal (s1, i1) (s2, i2) = s1 = s2 && i1 = i2
let equals = equal
let equal_names (s1, _) (s2, _) = s1 = s2

(* let compare (s1, i1) (s2, i2) =
 *   let s = compare s1 s2 in
 *   if s = 0 then
 *     compare i1 i2
 *   else s *)

let compare (s1, i1) (s2, i2) =
  let i = compare i1 i2 in
  if i = 0 then compare s1 s2 else i
;;
