open Batteries
open Printf
module CL = Caml.List
(* misc util functions *)

let cons_uniq xs x = if List.mem x xs then xs else x :: xs
let unique_list_of xs = List.rev (List.fold_left cons_uniq [] xs)

let list_remove xs s = CL.filter
  (fun x -> x<>s)
  xs
;; 

(* do lists l1 and l2 intersect? *)
let intersect l1 l2 =
  List.fold_left
    (fun acc x -> if List.exists (fun y -> y = x) l1 then true else acc)
    false
    l2
;;

(* l1 - l2 *)
let list_sub l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

(* do l1 and l2 contain the same elements? *)
let test_set_eq l1 l2 =
  match CL.length l1 = CL.length l2 with
  | false -> false
  | true ->
    let fold_f is_eq l1_entry = is_eq && CL.mem l1_entry l2 in
    CL.fold_left fold_f true l1
;;

let pair_fold ts pair_acc s = pair_acc @ CL.map (fun t -> s, t) ts
let get_all_pairs ss ts = CL.fold_left (pair_fold ts) [] ss

let rec remove_unordered_pair_dups pairs =
  match pairs with
  | [] -> []
  | [x] -> [x]
  | (a, b) :: pairs ->
    (match CL.mem (a, b) pairs || CL.mem (b, a) pairs with
    | true -> remove_unordered_pair_dups pairs
    | false -> (a, b) :: remove_unordered_pair_dups pairs)
;;

let get_unique_unordered_pairs ss ts =
  get_all_pairs ss ts |> remove_unordered_pair_dups
;;

let rec find_first_dup l =
  match l with
  | [] -> None
  | hd :: tl ->
    (match List.exists (( = ) hd) tl with
    | true -> Some hd
    | false -> find_first_dup tl)
;;

