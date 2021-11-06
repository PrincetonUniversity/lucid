(* convert: 
  (x >= y) --> (x > y || x == y) 
  (x <= y) --> (x < y || x == y) *)
open Syntax

let transform ds =
  let v =
    object
      inherit [_] s_map as super

      method! visit_EOp ctx op exps =
        match op with
        | Leq ->
          super#visit_EOp
            ctx
            Or
            [ exp_sp (EOp (Less, exps)) Span.default
            ; exp_sp (EOp (Eq, exps)) Span.default ]
        | Geq ->
          super#visit_EOp
            ctx
            Or
            [ exp_sp (EOp (More, exps)) Span.default
            ; exp_sp (EOp (Eq, exps)) Span.default ]
        | _ -> super#visit_EOp ctx op exps
    end
  in
  v#visit_decls () ds
;;
