(* convert all booleans into 1-bit ints *)
open Syntax
open Printf
open Batteries
open InterpHelpers
module CL = Caml.List

(* Give all the spans in a program a unique id. *)
let cur_span = ref 0

let refresh_span sp =
  cur_span := !cur_span + 1;
  { sp with Span.spid = !cur_span }
;;

let make_unique_spans ds =
  let v =
    object
      inherit [_] s_map as super

      method! visit_sp _ sp = refresh_span sp
    end
  in
  v#visit_decls () ds
;;
