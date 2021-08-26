(* eliminate non-call unit statements *)

open Syntax
open Cid
open Batteries
open Printf
module CL = Caml.List

let eliminate_noncall_units ds =
  let v =
    object
      inherit [_] s_map as super

      method! visit_SUnit ctx unit_exp =
        match unit_exp.e with
        | ECall _ -> super#visit_SUnit ctx unit_exp
        | _ -> SNoop
    end
  in
  v#visit_decls () ds
;;

(* temporary: delete cast operations. *)
let delete_casts ds =
   let v =
    object
      inherit [_] s_map as super

      method! visit_EOp ctx op exps =
        match op with
        | Cast _ -> 
          print_endline "FOUND A CAST";
          super#visit_EOp ctx op exps
        (* (CL.hd exps).e *)
        | _ -> super#visit_EOp ctx op exps
    end
  in
  v#visit_decls () ds
;;

(* temporary: delete print statements *)
let delete_prints ds =
  let v =
    object
      inherit [_] s_map as super

      method! visit_SPrintf _ _ _ = SNoop
    end
  in
  v#visit_decls () ds
;;

(* temporary: replace gt / lt with equality *)
let replace_ineqs ds =
  let v =
    object
      inherit [_] s_map as super

      method! visit_EOp ctx op exps =
        match op with
        | Less | More | Leq | Geq -> super#visit_EOp ctx Eq exps
        | _ -> super#visit_EOp ctx op exps
    end
  in
  v#visit_decls () ds
;;
