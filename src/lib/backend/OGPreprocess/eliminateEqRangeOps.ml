(* convert:
  (x >= y) --> (x > y || x == y)
  (x <= y) --> (x < y || x == y) *)
open CoreSyntax

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
            [exp (EOp (Less, exps)) (ty TBool); exp (EOp (Eq, exps)) (ty TBool)]
        | Geq ->
          super#visit_EOp
            ctx
            Or
            [exp (EOp (More, exps)) (ty TBool); exp (EOp (Eq, exps)) (ty TBool)]
        | _ -> super#visit_EOp ctx op exps
    end
  in
  v#visit_decls () ds
;;
