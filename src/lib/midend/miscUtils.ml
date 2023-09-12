open Batteries
open Printf
module CL = Caml.List

exception Error of string

let error s = raise (Error s)

(* misc util functions *)

(* (s, e] *)
let range s e = List.init (e - s) (fun x -> s + x)

(* Take a list of lists of objects of the form 
      [[a1; a2; ... aj]; [b1; b2; ... bk]; ... [c1; c2; ... cl]]
    Return a list of all possible combinations of the form 
      [[a1; b1; ... c1]; [a1; b1; ... c2]; [a1; b1; ... ck]; ... [a1; b2; ... c1] ... [aj; bk; ... cl]] *)
let rec all_combinations (stss : 'a list list) : 'a list list =
  match stss with
  | [] -> []
  (* all combinations of a list is a list of singletons *)
  | [sts] -> CL.map (fun st -> [st]) sts
  (* all combinations of a singleton list with subsequent lists --> 
       prepend the singleton to all lists. *)
  | [st] :: stss ->
    let combos = all_combinations stss in
    CL.map (fun (combo : 'a list) -> st :: combo) combos
  (* all combinations with every element of first list, separately, then flatten. *)
  | sts :: stss ->
    CL.map (fun st -> all_combinations ([st] :: stss)) sts |> CL.flatten
;;

let sum xs = CL.fold_left ( + ) 0 xs
let cons_uniq xs x = if List.mem x xs then xs else x :: xs
let unique_list_of xs = List.rev (List.fold_left cons_uniq [] xs)

let cons_uniq_eq eq xs x = let open Core in if List.mem xs x ~equal:eq then xs else x :: xs
let unique_list_of_eq eq xs = List.rev (Caml.List.fold_left (cons_uniq_eq eq) [] xs)



module Seq = Core.Sequence
let seq_cons_unique eq xs x = 
  let found = (Seq.mem ~equal:eq xs x) in
  if found then xs else (Seq.append xs (Seq.singleton x))
;;
let unique_seq_of eq xs =
  Seq.fold
    xs
    ~init:Seq.empty
    ~f:(seq_cons_unique eq)
;;

let has_dup xs = 
  (xs |> unique_list_of |> CL.length) <> (xs |> CL.length)
;;

let list_remove xs x = CL.filter (fun x_c -> x_c <> x) xs
let remove x xs = list_remove xs x

(* replace the binding in assoc for k with (k, v). If there is 
   no binding, append (k, v) to the end. *)
let rec replace_or_app (k, v) assoc =
  match assoc with
  | [] -> [k, v]
  | (hd_k, hd_v) :: tl ->
    (match String.equal hd_k k with
    | true -> (k, v) :: tl
    | false -> (hd_k, hd_v) :: replace_or_app (k, v) tl)
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

let list_eq l1 l2 =
  match list_sub l1 l2, list_sub l2 l1 with
  | [], [] -> true
  | _ -> false
;;

let contains l1 ele = CL.exists (fun e -> e = ele) l1

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

let find_one_in_list fcn lst =
   let rec loop acc = function
      (* at the end of lst, so check if we have found something *)
     | [] -> (match acc with
              | [] -> None
              | [x] -> Some x
              | _ -> failwith "Multiple matches found")
     (* processing the list, so update the accumulator *)
     | x::xs -> if fcn x then loop (x::acc) xs else loop acc xs
   in loop [] lst
;;


let id i = Id.create i