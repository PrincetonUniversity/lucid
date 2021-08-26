(* inline and remove all consts *)

open Syntax
open Cid
open Batteries
open Printf
module CL = Caml.List

(* eliminate first const and recurse if any others remain. *)
let rec eliminate_consts ds =
  (* get assoc of const:expr *)
  print_endline "------- finding consts ------- ";
  let map_f dec =
    match dec.d with
    | ConstVar (id, _, exp) -> Some (Cid.id id, exp)
    | _ -> None
  in
  let const_tups = CL.filter_map map_f ds in
  match const_tups with
  | [] -> ds
  | (const_cid, const_exp) :: _ ->
    (* eliminate const_cid const_exp *)
    print_endline ("eliminating const: " ^ Printing.cid_to_string const_cid);
    (* replace every const var exp with its value exp *)
    let v =
      object
        inherit [_] s_map as super

        method! visit_d ctx d =
          (match d with
          | ConstVar _ -> print_endline ("entering: " ^ Printing.d_to_string d)
          | _ -> ());
          let new_d = super#visit_d ctx d in
          (match d with
          | ConstVar _ -> print_endline ("leaving: " ^ Printing.d_to_string d)
          | _ -> ());
          new_d

        method! visit_EVar ctx cid =
          match Cid.equals const_cid cid with
          | true ->
            print_endline
              ("FOUND USE: "
              ^ Printing.cid_to_string const_cid
              ^ " --> "
              ^ Printing.exp_to_string const_exp);
            const_exp.e (* instance of target const -- replace with value *)
          | _ -> super#visit_EVar ctx cid
        (* not instance of tgt -- do not replace *)
      end
    in
    let ds = v#visit_decls () ds in
    (* delete the const declaration *)
    let filter_f dec =
      match dec.d with
      | ConstVar (id, _, _) -> not (Cid.equals (Cid.id id) const_cid)
      | _ -> true
    in
    let ds = CL.filter filter_f ds in
    (* recurse for remaining consts *)
    eliminate_consts ds
;;
