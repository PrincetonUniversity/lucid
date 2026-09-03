(* Network-wide state in the interpreter. *)
(* 
  Depreciated. Network-wide state is simply an Array of switches in InterpSwitch.ml
*)
open Batteries
open InterpSwitch

exception Error of string
let error s = raise (Error s)



