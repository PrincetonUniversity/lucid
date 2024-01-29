open FCoreSyntax

(* LEFT OFF HERE: simple printing for the FCore IR  *)

(* 
type op =  | And | Or | Not
          | Eq  | Neq | Less| More  | Leq | Geq
          | Neg | Plus| Sub | SatPlus | SatSub
          | BitAnd  | BitOr | BitXor | BitNot | LShift | RShift
          | Slice of int * int
          | PatExact | PatMask
          | Hash of size
          | Cast of size 
          | Conc
          | Project of id | Get of int (* record and tuple ops *)   
*)

let id_to_string id = fst id
let rec exp_to_string exp = 
  failwith "not done"
(* translate to valid ocaml *)
and eop_to_string op args raw_ty = 
  match op, args, raw_ty with 
  | And, [e1; e2], TBool -> Printf.sprintf "(%s && %s)" (exp_to_string e1) (exp_to_string e2)
  | Or, [e1; e2], TBool -> Printf.sprintf "(%s || %s)" (exp_to_string e1) (exp_to_string e2)
  | Not, [e1], TBool -> Printf.sprintf "(not%s)" (exp_to_string e1)
  | Eq, [e1; e2], _ -> Printf.sprintf "(%s = %s)" (exp_to_string e1) (exp_to_string e2)
  | Neq, [e1; e2], _ -> Printf.sprintf "(%s <> %s)" (exp_to_string e1) (exp_to_string e2)
  | Less, [e1; e2], _ -> Printf.sprintf "(%s < %s)" (exp_to_string e1) (exp_to_string e2)
  | More, [e1; e2], _ -> Printf.sprintf "(%s > %s)" (exp_to_string e1) (exp_to_string e2)
  | Leq, [e1; e2], _ -> Printf.sprintf "(%s <= %s)" (exp_to_string e1) (exp_to_string e2)
  | Geq, [e1; e2], _ -> Printf.sprintf "(%s >= %s)" (exp_to_string e1) (exp_to_string e2)
  | Neg, [e1], _ -> Printf.sprintf "(-%s)" (exp_to_string e1)
  | Plus, [e1; e2], _ -> Printf.sprintf "(%s + %s)" (exp_to_string e1) (exp_to_string e2)
  | Sub, [e1; e2], _ -> Printf.sprintf "(%s - %s)" (exp_to_string e1) (exp_to_string e2)
  | SatPlus, [e1; e2], _ -> Printf.sprintf "(%s |+| %s)" (exp_to_string e1) (exp_to_string e2)
  | SatSub, [e1; e2], _ -> Printf.sprintf "(%s |-| %s)" (exp_to_string e1) (exp_to_string e2)
  | BitAnd, [e1; e2], _ -> Printf.sprintf "(%s & %s)" (exp_to_string e1) (exp_to_string e2)
  | BitOr, [e1; e2], _ -> Printf.sprintf "(%s | %s)" (exp_to_string e1) (exp_to_string e2)
  | BitXor, [e1; e2], _ -> Printf.sprintf "(%s ^ %s)" (exp_to_string e1) (exp_to_string e2)
  | BitNot, [e1], _ -> Printf.sprintf "(~%s)" (exp_to_string e1)
  | LShift, [e1; e2], _ -> Printf.sprintf "(%s << %s)" (exp_to_string e1) (exp_to_string e2)
  | RShift, [e1; e2], _ -> Printf.sprintf "(%s >> %s)" (exp_to_string e1) (exp_to_string e2)
  | Slice (i, j), [e1], _ -> Printf.sprintf "(%s[%d:%d])" (exp_to_string e1) i j
  | PatExact, [e1], _ -> Printf.sprintf "(exact_pat(%s))" (exp_to_string e1)
  | PatMask, [e1; e2], _ -> Printf.sprintf "(mask_pat(%s, %s))" (exp_to_string e1) (exp_to_string e2)
  | Hash((s)), args, _ -> Printf.sprintf "(hash<%s>(%s))" (string_of_int s) (List.map exp_to_string args |> String.concat ", ")
  | Cast((s)), [e1], _ -> Printf.sprintf "(int<%s>(%s))" (string_of_int s) (exp_to_string e1)
  | Conc, [e1; e2], _ -> Printf.sprintf "(%s @ %s)" (exp_to_string e1) (exp_to_string e2)
  | Project(id), [e1], _ -> Printf.sprintf "(%s.%s)" (exp_to_string e1) (id_to_string id)
  | Get(i), [e1], _ -> Printf.sprintf "(%s[%d])" (exp_to_string e1) i
  | _, _, _ -> failwith "invalid operation expression"
;;

