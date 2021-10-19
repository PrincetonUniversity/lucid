open Syntax
open Batteries

let inline_exp e =
  match e with
  | EVal _ | EInt _ | EVar _ -> snoop, e
  | _ -> snoop, e
;;

let eliminate_prog x = x
