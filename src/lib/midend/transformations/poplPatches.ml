(* eliminate non-call unit statements *)

open CoreSyntax
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

(* delete print statements *)
let delete_prints ds =
  let v =
    object
      inherit [_] s_map as super
      method! visit_SPrintf _ _ _ = SNoop
    end
  in
  v#visit_decls () ds
;;