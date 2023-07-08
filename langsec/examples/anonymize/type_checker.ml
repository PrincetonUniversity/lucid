type id = string
type security = High | Low
let top = High
let bot = Low

module Env = Map.Make(String)
let env : 'a Env.t = Env.empty
exception TypeError of string

type t =
| Bool of security
| Int of security
| Arr of t * t * security

let topbool = Bool High
let topint = Int High
let toparr arg res = Arr (arg, res, top)

type expr = 
| Var of id
| Bool of bool
| Int of int
| If of expr * expr * expr
| Func of id * t * expr * security
| Call of expr * expr
| And of expr * expr
| Or of expr * expr
| Not of expr
| Plus of expr * expr
| Minus of expr * expr
| Multiply of expr * expr
| Divide of expr * expr
| Let of id * expr * expr

let lookup (env: 'a Env.t) (id: id): t =
  Env.find id env

let update (env: 'a Env.t) (id: id) (t: t): 'a Env.t =
  Env.add id t env

let sjoin (s1: security) (s2: security): security =
  match (s1, s2) with
  | (Low, Low) -> Low
  | (_, _) -> High

let smeet (s1: security) (s2: security): security =
  match (s1, s2) with
  | (High, High) -> High
  | (_, _) -> Low

let sget (t: t): security = 
  match t with
  | Bool s -> s
  | Int s -> s
  | Arr (arg, result, s) -> s

let rec join (t1: t) (t2: t): t = 
  match (t1, t2) with
  | (Bool s1, Bool s2) -> Bool (sjoin s1 s2)
  | (Int s1, Int s2) -> Int (sjoin s1 s2)
  | (Arr (targ1, tresult1, s1), Arr (targ2, tresult2, s2)) -> (
      let rec meet (t1: t) (t2: t): t =
        match (t1, t2) with
        | (Bool s1, Bool s2) -> Bool (smeet s1 s2)
        | (Int s1, Int s2) -> Int (smeet s1 s2)
        | (Arr (targ1, tresult1, s1), Arr (targ2, tresult2, s2)) -> (
            let targ = join targ1 targ2 in 
            let tresult = meet tresult1 tresult2 in 
            let slevel = sjoin s1 s2 in 
            Arr (targ, tresult, slevel))
        | (_, _) -> raise (TypeError "Types do not match") in
      let targ = meet targ1 targ2 in 
      let tresult = join tresult1 tresult2 in 
      let slevel = smeet s1 s2 in 
      Arr (targ, tresult, slevel))
  | (_, _) -> raise (TypeError "Types do not match") 

let lteq (s1: security) (s2: security): bool =
  match (s1, s2) with
  | (High, Low) -> false
  | (_, _) -> true

let eq (s1: security) (s2: security): bool =
  match (s1, s2) with
  | (High, High) -> true
  | (Low, Low) -> true
  | (_, _) -> false

let rec equal (t1: t) (t2: t): bool =
  match (t1, t2) with
  | (Bool s1, Bool s2) -> eq s1 s2 
  | (Int s1, Int s2) -> eq s1 s2
  | (Arr (targ1, tresult1, s1), Arr (targ2, tresult2, s2)) -> (
      (equal targ1 targ2) && (equal tresult1 tresult2) && eq s1 s2)
  | (_, _) -> raise (TypeError "Types do not match")

let rec less_than_equal (t1: t) (t2: t): bool =
  match (t1, t2) with
  | (Bool s1, Bool s2) -> lteq s1 s2 
  | (Int s1, Int s2) -> lteq s1 s2
  | (Arr (targ1, tresult1, s1), Arr (targ2, tresult2, s2)) -> (
      (less_than_equal targ1 targ2) && (less_than_equal tresult1 tresult2) && lteq s1 s2)
  | (_, _) -> raise (TypeError "Types do not match")

let rec typecheck (env: 'a Env.t) (expr: expr): t = 
  match expr with
  | Var x -> (
      try lookup env x with 
      not_Found -> raise (TypeError "Free variable"))
  | Bool x -> Bool bot
  | Int x -> Int bot
  | If (condition, if_expr, else_expr) -> (
      less_than_equal (typecheck env condition) topbool;
      let if_type = typecheck env if_expr in 
      let else_type = typecheck env else_expr in
      join if_type else_type)
  | Func (x, arg_type, func_body, s) -> 
      let result_type = typecheck (update env x arg_type) func_body in
      Arr (arg_type, result_type, s)
  | Call (func, arg) -> (
      match (typecheck env func) with
      | Arr (arg_type, result_type, security) -> (
          let targ = typecheck env arg in 
          let sarg = sget targ in
          less_than_equal targ arg_type;
          if lteq sarg security then result_type 
          else raise (TypeError "Security levels do not match");)
      | _ -> raise (TypeError "Not a function"))
  | And (expr1, expr2) -> (
      let t1 = typecheck env expr1 in 
      let t2 = typecheck env expr2 in 
      less_than_equal t1 topbool;
      less_than_equal t2 topbool;
      join t1 t2)
  | Or (expr1, expr2) -> (
      let t1 = typecheck env expr1 in 
      let t2 = typecheck env expr2 in 
      less_than_equal t1 topbool;
      less_than_equal t2 topbool;
      join t1 t2)
  | Not x -> (
      let t1 = typecheck env x in  
      less_than_equal t1 topbool;
      t1)
  | Plus (expr1, expr2) -> (
      let t1 = typecheck env expr1 in 
      let t2 = typecheck env expr2 in 
      less_than_equal t1 topint;
      less_than_equal t2 topint;
      join t1 t2)
  | Minus (expr1, expr2) -> (
      let t1 = typecheck env expr1 in 
      let t2 = typecheck env expr2 in 
      less_than_equal t1 topint;
      less_than_equal t2 topint;
      join t1 t2)
  | Multiply (expr1, expr2) -> (
      let t1 = typecheck env expr1 in 
      let t2 = typecheck env expr2 in 
      less_than_equal t1 topint;
      less_than_equal t2 topint;
      join t1 t2)
  | Divide (expr1, expr2) -> (
      let t1 = typecheck env expr1 in 
      let t2 = typecheck env expr2 in 
      less_than_equal t1 topint;
      less_than_equal t2 topint;
      join t1 t2)
  | Let (x, expr1, expr2) -> (
      let expr1_type = typecheck env expr1 in
      typecheck (update env x expr1_type) expr2)

let print_security (s:security) =
  match s with
  | High -> "High"
  | Low -> "Low"

let rec print_type (t:t) = 
  match t with
  | Bool s -> "Bool " ^ (print_security s)
  | Int s -> "Int " ^ (print_security s)
  | Arr (arg, result, s) -> (print_type arg) ^ " -> " ^ (print_type result) ^ " : " ^ (print_security s)

let pretty_print (env: 'a Env.t): unit =
  let bindings = Env.bindings env in
  let rec iter ls = 
    match ls with
    | [] -> print_endline "End of bindings"
    | head :: tail -> (
        match head with
        | (id, t) -> print_endline (id ^ (print_type t));
        iter tail) in 
  iter bindings

let env = update env "x" (Int Low)
let env = update env "y" (Int High)
let env = update env "true" (Bool High)
let env = update env "false" (Bool Low)
let env = update env "fizz" (Arr (Int High, Bool Low, Low))
let env = update env "buzz" (Arr (Int High, Bool Low, High))
let e1 = (If (Var "true", Var "fizz", Var "buzz"))
let e2 = Call (Var "z", Var "y")
let _ = print_endline (print_type (typecheck env (Let ("z", e1, e2))))
    
(* Type inferencing notes *)
(* fun x -> fun y -> x + 2 + (if x then 3 else 4) *)
(* x: 'a <- TVar created by 'fresh' function
   y: 'b 
   unify (lookup x) Int 
    - mutate TVar so it equals Int
    OR
    - generate equation 'a = Int
   unify (lookup x) Bool 
    - unification fails b/c TVar is Int 
    OR
    - generate equation 'a = Bool *)

(* let f = (fun x -> x) in 
if (f true) then (f 1) else (f 2) *)
(* x: 'a 
   unify (arg_type of f) Bool 
   - mutate 'a = Bool 
   unify (arg_type of f) Int 
   - unification fails b/c 'a != Int *)

(* f has quantified type: for all q. q -> q 
    - generalization of 'a -> 'a 
    - careful about what you can generalize (if variable already appears in env)
  in (f true), replace q's with fresh type 'c
    - f has type 'c -> 'c 
    - 'c gets specialized as Bool
  in (f 1), replace q's with fresh type 'd
    - f has type 'd -> 'd 
    - 'd gets specialized to Int 
  in (f 2), replace q's with fresh type 'e
    - f has type 'e -> 'e 
    - 'e gets specialized to Int *)