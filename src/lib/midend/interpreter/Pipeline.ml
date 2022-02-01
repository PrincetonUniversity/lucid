open Batteries
open CoreSyntax

(* Bool is true if this is a paired array *)
type stage = zint array * bool

let split_integer n =
  let sz = Integer.size n / 2 in
  let upper = Integer.shift_right n sz |> Integer.set_size sz in
  let lower = Integer.set_size sz n in
  upper, lower
;;

type t =
  { arrs : stage array
  ; current_stage : int ref
  }

let empty () =
  { arrs = Array.make 0 (Array.make 0 (Integer.of_int 0), false)
  ; current_stage = ref 0
  }
;;

let append_stage ~(width : int) ~(length : int) ~(pair : bool) t =
  let full_width = if pair then width * 2 else width in
  let new_arr = Array.make length (Integer.create 0 full_width) in
  { t with arrs = Array.append t.arrs @@ Array.make 1 (new_arr, pair) }
;;

let of_globals (gs : (int * int * bool) list) =
  List.fold_left
    (fun acc (width, length, pair) -> append_stage ~width ~length ~pair acc)
    (empty ())
    gs
;;

let reset_stage t = t.current_stage := 0

let copy t =
  { arrs = Array.map (fun (arr, o) -> Array.copy arr, o) t.arrs
  ; current_stage = ref !(t.current_stage)
  }
;;

let length t = Array.length t.arrs

let to_string ?(pad = "") t =
  if Array.length t.arrs = 0
  then "[ ]"
  else (
    let str =
      t.arrs
      |> Array.mapi (fun idx (arr, pair) ->
             let print_entry n =
               if not pair
               then Integer.to_string n
               else (
                 let upper, lower = split_integer n in
                 Printf.sprintf
                   "(%s, %s)"
                   (Integer.to_string upper)
                   (Integer.to_string lower))
             in
             Printf.sprintf
               "%s%d : %s\n"
               (pad ^ pad)
               idx
               (Printing.list_to_string print_entry (Array.to_list arr)))
      |> Array.fold_left ( ^ ) ""
    in
    Printf.sprintf "[\n%s%s]" str pad)
;;

let validate_update stage idx t =
  if stage < 0 || idx < 0
  then failwith "Pipeline Error: Stage or index value is negative";
  if stage < !(t.current_stage)
  then
    failwith
      "Pipeline Error: Attempted to access global out-of-order. The type \
       system should have caught this, so notify the developer.";
  if stage >= Array.length t.arrs
  then
    failwith
      "Pipeline Error: Attempted to access nonexistent global. This should be \
       impossible.";
  let arr, pair = t.arrs.(stage) in
  if idx >= Array.length arr
  then
    failwith
    @@ Printf.sprintf
         "Pipeline Error: Index %d is invalid for global number %d."
         idx
         stage;
  arr, pair
;;

let update
    ~(stage : int)
    ~(idx : int)
    ~(getop : zint -> 'a)
    ~(setop : zint -> zint)
    (t : t)
  =
  let arr, pair = validate_update stage idx t in
  if pair
  then failwith @@ "Pipeline Error: Tried to use Array.update on a paired array";
  let orig_val = arr.(idx) in
  arr.(idx) <- setop orig_val;
  t.current_stage := stage + 1;
  getop orig_val
;;

let update_complex
    ~(stage : int)
    ~(idx : int)
    ~(memop : zint -> zint -> zint * zint * 'a)
    (t : t)
  =
  let arr, pair = validate_update stage idx t in
  let orig_val = arr.(idx) in
  let new_val, ret =
    match pair with
    | true ->
      let upper, lower = split_integer orig_val in
      let new_upper, new_lower, ret = memop upper lower in
      Integer.concat new_upper new_lower, ret
    | false ->
      let new_val, _, ret =
        memop orig_val (Integer.create ~value:0 ~size:(Integer.size orig_val))
      in
      new_val, ret
  in
  arr.(idx) <- new_val;
  t.current_stage := stage + 1;
  ret
;;
