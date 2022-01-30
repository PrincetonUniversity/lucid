open Batteries
open CoreSyntax

type t =
  { arrs : zint array array
  ; current_stage : int ref
  }

let empty () =
  { arrs = Array.make 0 (Array.make 0 (Integer.of_int 0))
  ; current_stage = ref 0
  }
;;

let append_stage ~(width : int) ~(length : int) t =
  let new_arr = Array.make length (Integer.create 0 width) in
  { t with arrs = Array.append t.arrs (Array.make 1 new_arr) }
;;

let of_globals (gs : (int * int) list) =
  let arrs =
    gs
    |> List.map (fun (width, length) ->
           Array.make length (Integer.create 0 width))
    |> Array.of_list
  in
  { arrs; current_stage = ref 0 }
;;

let reset_stage t = t.current_stage := 0

let copy t =
  { arrs = Array.map Array.copy t.arrs; current_stage = ref !(t.current_stage) }
;;

let length t = Array.length t.arrs

let to_string ?(pad = "") t =
  if Array.length t.arrs = 0
  then "[ ]"
  else (
    let str =
      t.arrs
      |> Array.mapi (fun idx arr ->
             Printf.sprintf
               "%s%d : %s\n"
               (pad ^ pad)
               idx
               (Printing.list_to_string Integer.to_string (Array.to_list arr)))
      |> Array.fold_left ( ^ ) ""
    in
    Printf.sprintf "[\n%s%s]" str pad)
;;

let update
    ~(stage : int)
    ~(idx : int)
    ~(getop : zint -> 'a)
    ~(setop : zint -> zint)
    (t : t)
  =
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
  let arr = t.arrs.(stage) in
  if idx >= Array.length arr
  then
    failwith
    @@ Printf.sprintf
         "Pipeline Error: Index %d is invalid for global number %d."
         idx
         stage;
  let orig_val = arr.(idx) in
  arr.(idx) <- setop orig_val;
  t.current_stage := stage + 1;
  getop orig_val
;;
