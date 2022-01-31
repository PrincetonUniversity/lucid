type t =
  { size : Z.t
  ; value : Z.t
  }

let modulo = Big_int_Z.mod_big_int
let pow2 n = Z.pow (Z.of_int 2) (Z.to_int n)

let mod_by_size (x : t) : t =
  { size = x.size; value = modulo x.value (pow2 x.size) }
;;

let of_string (s : string) : t =
  let lst = List.map Z.of_string @@ Str.split (Str.regexp "u") s in
  match lst with
  | [] -> failwith "Integer.of_string: This is literally impossible"
  | [value] -> mod_by_size { size = Z.of_int 32; value }
  | [value; size] -> mod_by_size { size; value }
  | _ -> failwith "Integer.of_string: Too many values"
;;

let of_bv_string (s : string) : t =
  (* print_endline @@ "of_bv_string: given " ^ s; *)
  let size =
    match String.sub s 0 2 with
    | "#b" -> Z.of_int @@ (String.length s - 2)
    | "#x" -> Z.of_int @@ (4 * (String.length s - 2))
    | _ -> failwith @@ "Integer.of_bv_string: Unrecognized bv format: " ^ s
  in
  (* Replace "#b00101" with "0b00101", etc *)
  let value = Z.of_string @@ "0" ^ Str.string_after s 1 in
  { size; value }
;;

let of_int (n : int) : t =
  mod_by_size { size = Z.of_int 32; value = Z.of_int n }
;;

let create ~(value : int) ~(size : int) : t =
  mod_by_size { size = Z.of_int size; value = Z.of_int value }
;;

let create_64 ~(value : Int64.t) ~(size : int) : t =
  mod_by_size { size = Z.of_int size; value = Z.of_int64 value }
;;

let create_z ~(value : Z.t) ~(size : int) : t =
  mod_by_size { size = Z.of_int size; value }
;;

let size x = Z.to_int x.size
let value x = x.value
let to_int x = Z.to_int x.value
let to_string x = Z.to_string x.value ^ "u" ^ Z.to_string x.size
let to_p4_string x = Z.to_string x.size ^ "w" ^ Z.to_string x.value
let value_string x = Z.to_string x.value
let size_string x = Z.to_string x.size
let same_size x y = Z.equal x.size y.size

let check x y =
  if not (same_size x y)
  then
    failwith
    @@ Printf.sprintf
         "Integer bit sizes did not match (%s vs %s)"
         (to_string x)
         (to_string y)
;;

let add x y =
  check x y;
  let value = Z.add x.value y.value in
  mod_by_size @@ { size = x.size; value }
;;

let sub x y =
  check x y;
  let value = Z.sub x.value y.value in
  mod_by_size @@ { size = x.size; value }
;;

let bitand x y =
  check x y;
  mod_by_size @@ { x with value = Z.logand x.value y.value }
;;

let bitor x y =
  check x y;
  mod_by_size @@ { x with value = Z.logor x.value y.value }
;;

let bitxor x y =
  check x y;
  mod_by_size @@ { x with value = Z.logxor x.value y.value }
;;

let bitnot x = mod_by_size @@ { x with value = Z.lognot x.value }

let shift_left (x : t) (n : int) =
  let value = Z.shift_left x.value n in
  mod_by_size @@ { size = x.size; value }
;;

let shift_right (x : t) (n : int) =
  let value = Z.shift_right x.value n in
  mod_by_size @@ { size = x.size; value }
;;

let set_size (n : int) t = mod_by_size @@ { t with size = Z.of_int n }

let pred x =
  let value = Z.sub x.value Z.one in
  mod_by_size @@ { size = x.size; value }
;;

let succ x =
  let value = Z.add x.value Z.one in
  mod_by_size @@ { size = x.size; value }
;;

let max_int (sz : int) =
  let sz = Z.of_int sz in
  let v = pow2 sz in
  { size = sz; value = Z.pred v }
;;

let lt x y =
  check x y;
  Z.lt x.value y.value
;;

let leq x y =
  check x y;
  Z.leq x.value y.value
;;

let gt x y =
  check x y;
  Z.gt x.value y.value
;;

let geq x y =
  check x y;
  Z.geq x.value y.value
;;

let equal x y = x.value = y.value && x.size = y.size

let compare x y =
  let cmp = Z.compare x.value y.value in
  if cmp <> 0 then cmp else Z.compare x.size y.size
;;

let is_zero x = x.value = Z.zero

let concat x y =
  let sz = size x + size y in
  let x', y' = set_size sz x, set_size sz y in
  add (shift_left x' (size y)) y'
;;
