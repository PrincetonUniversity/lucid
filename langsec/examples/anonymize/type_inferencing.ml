type id = string
type security = High | Low | Unknown
let top = High
let bot = Low

type level = int
let curr_level = ref 1
let reset_level (): unit = curr_level := 1
let enter_level () = incr curr_level
let exit_level () = decr curr_level

let gensym_counter = ref 0
let reset_gensym (): unit = gensym_counter := 0

let gensym (): id = 
  incr gensym_counter; 
  "'a" ^ string_of_int !gensym_counter

module Env = Map.Make(String)
let env : 'a Env.t = Env.empty
exception TypeError of string
exception UnificationError of string

type t =
| TVar of tv ref
| QVar of id * security
| Arr of t * t * security
| Bool of security
| Int of security
and tv = Unbound of id * security * level | Link of t

type expr = 
| Var of id
| Func of id * expr
| Call of expr * expr
| Let of id * expr * expr
| At of expr * security       (* Ascription: `expr` at security level `security` *)
| Bool of bool
| Int of int
| If of expr * expr * expr
| And of expr * expr
| Or of expr * expr
| Not of expr
| Plus of expr * expr
| Minus of expr * expr
| Multiply of expr * expr
| Divide of expr * expr

let rec print_type (t: t): string = 
  match t with
  | TVar tvref -> (
      match !tvref with
      | Unbound (name, s, level) -> "(" ^ name ^ " " ^ sprint s ^ ")"
      | Link typ -> "(" ^ print_type typ ^ ")")
  | QVar (id, s) -> "(QVar " ^ id ^ " " ^ sprint s ^ ")"
  | Arr (targ, tres, s) -> "(" ^ print_type targ ^ " -> " ^ print_type tres ^ ", " ^ sprint s ^ ")"
  | Bool s -> "(Bool " ^ sprint s ^ ")"
  | Int s -> "(Int " ^ sprint s ^ ")"
and sprint (s: security): string = 
  match s with
  | High -> "H"
  | Low -> "L"
  | Unknown -> "U"

let lookup (env: 'a Env.t) (id: id): t =
  Env.find id env

let update (env: 'a Env.t) (id: id) (t: t): 'a Env.t =
  Env.add id t env

let sfresh (): security =
  Unknown

let fresh (s: security): t =
  TVar (ref (Unbound (gensym (), s, !curr_level)))

let reset (): unit = 
  reset_gensym ();
  reset_level ()

let rec equal (t1: t) (t2: t): bool =
  match (t1, t2) with
  | (TVar {contents = Link t1}, t2) -> equal t1 t2
  | (t1, TVar {contents = Link t2}) -> equal t1 t2
  | (TVar {contents = Unbound (x, s1, _)}, TVar {contents = Unbound (y, s2, _)}) -> (
      String.equal x y && (sequal s1 s2)) 
  | (QVar (x, s1), QVar (y, s2)) -> String.equal x y && (sequal s1 s2)
  | (Arr (targ1, tres1, s1), Arr (targ2, tres2, s2)) -> (
      (equal targ1 targ2) && (equal tres1 tres2) && (sequal s1 s2))
  | (Bool s1, Bool s2) -> sequal s1 s2
  | (Int s1, Int s2) -> sequal s1 s2
  | (_, _) -> false
and sequal (s1: security) (s2: security): bool =
  match (s1, s2) with 
  | (High, High) -> true
  | (Low, Low) -> true
  | (Unknown, Unknown) -> true
  | (_, _) -> false

let rec less_than_equal (t1: t) (t2: t): bool =
  match (t1, t2) with
  | (TVar {contents = Link t1}, t2) -> less_than_equal t1 t2
  | (t1, TVar {contents = Link t2}) -> less_than_equal t1 t2
  | (TVar {contents = Unbound (x, s1, _)}, TVar {contents = Unbound (y, s2, _)}) -> (
      String.equal x y && (lteq s1 s2)) 
  | (QVar (x, s1), QVar (y, s2)) -> String.equal x y && (lteq s1 s2)
  | (Arr (targ1, tresult1, s1), Arr (targ2, tresult2, s2)) -> (
      (less_than_equal targ1 targ2) && (less_than_equal tresult1 tresult2) && lteq s1 s2)
  | (Bool s1, Bool s2) -> lteq s1 s2 
  | (Int s1, Int s2) -> lteq s1 s2
  | (_, _) -> false
and lteq (s1: security) (s2: security): bool =
  match (s1, s2) with
  | (High, Low) -> false
  | (_, _) -> true

let inst (t: t): t =
  let rec loop (env: 'a Env.t) (typ: t): t * 'a Env.t =
    match typ with
    | QVar (name, s) -> (
        try (lookup env name, env)
        with Not_found ->
          let tv = fresh s in
          (tv, update env name tv))
    | TVar {contents = Link typ'} -> loop env typ'
    | Arr (targ, tres, s) -> 
        let (targ, env) = loop env targ in
        let (tres, env) = loop env tres in
        (Arr (targ, tres, s), env)
    | typ' -> (typ', env) in
  fst (loop Env.empty t)

let rec occurs (tv: tv ref) (t: t): unit = 
  match t with
  | TVar ({contents = Unbound (name, s, level)} as tvref) -> (
      if tv_equal tv tvref then raise (UnificationError ("Occurs check failed"));
      let min_level = 
      match !tv with
      | Unbound (_, security, lev) -> min lev level 
      | _ -> level in 
      tvref := Unbound (name, s, min_level))
  | TVar ({contents = Link typ} as tvref) -> (
      if tv_equal tv tvref then raise (UnificationError ("Occurs check failed"));
      occurs tv typ)
  | Arr (t1, t2, s) -> occurs tv t1; occurs tv t2
  | _ -> ()
and tv_equal (tv1: tv ref) (tv2: tv ref): bool =
  match (!tv1, !tv2) with
  | (Unbound (x, s1, l1), Unbound (y, s2, l2)) -> (String.equal x y) && (sequal s1 s2)
  | (Link t1, Link t2) -> equal t1 t2
  | (_, _) -> false

let rec supdate (t: t) (s: security): t =
  match t with
  | TVar {contents = Unbound (name, _, level)} -> TVar {contents = Unbound (name, s, level)}
  | TVar {contents = Link t} -> TVar {contents = Link (supdate t s)}
  | QVar (name, _) -> QVar (name, s)
  | Arr (targ, tres, _) -> Arr (targ, tres, s)
  | Bool _ -> Bool s
  | Int _ -> Int s

let sjoin (s1: security) (s2: security): security =
  match (s1, s2) with
  | (Low, Low) -> Low
  | (Unknown, s) -> s
  | (s, Unknown) -> s
  | (_, _) -> High

let smeet (s1: security) (s2: security): security =
  match (s1, s2) with
  | (High, High) -> High
  | (Unknown, s) -> s
  | (s, Unknown) -> s
  | (_, _) -> Low

let rec sget (t: t): security = 
  match t with
  | TVar {contents = Unbound (_, s, _)} -> s
  | TVar {contents = Link t} -> sget t
  | QVar (_, s) -> s
  | Arr (arg, result, s) -> s
  | Bool s -> s
  | Int s -> s

let rec join (t1: t) (t2: t): t = 
  match (t1, t2) with
  | (Arr (targ1, tresult1, s1), Arr (targ2, tresult2, s2)) -> (
      let targ = meet targ1 targ2 in 
      let tresult = join tresult1 tresult2 in 
      let slevel = sjoin s1 s2 in 
      Arr (targ, tresult, slevel))
  | (Bool s1, Bool s2) -> Bool (sjoin s1 s2)
  | (Int s1, Int s2) -> Int (sjoin s1 s2)
  | (_, _) -> raise (TypeError "Types cannot be joined")
and meet (t1: t) (t2: t): t =
  match (t1, t2) with
  | (Arr (targ1, tresult1, s1), Arr (targ2, tresult2, s2)) -> (
      let targ = join targ1 targ2 in 
      let tresult = meet tresult1 tresult2 in 
      let slevel = smeet s1 s2 in 
      Arr (targ, tresult, slevel))
  | (Bool s1, Bool s2) -> Bool (smeet s1 s2)
  | (Int s1, Int s2) -> Int (smeet s1 s2)
  | (_, _) -> raise (TypeError "Types cannot meet")

let rec screep (t: t) (s: security): t =
  match t with
  | TVar {contents = Unbound (name, security, level)} -> TVar {contents = Unbound (name, sjoin security s, level)}
  | TVar {contents = Link t} -> TVar {contents = Link (screep t s)}
  | QVar (name, security) -> QVar (name, sjoin security s)
  | Arr (targ, tres, security) -> Arr (targ, tres, sjoin security s)
  | Bool security -> Bool (sjoin security s)
  | Int security -> Int (sjoin security s)
  
let rec unify (free: t) (typ: t): unit =
  if greater_than_equal typ free then raise (UnificationError "Cannot unify security levels")
  else if less_than_equal typ free then ()
  else match (free, typ) with
  | (TVar {contents = Link t1}, t2) -> unify t1 t2
  | (t1, TVar {contents = Link t2}) -> unify t1 t2
  | (TVar ({contents = Unbound (_, s, _)} as tv), t2) -> (
      occurs tv t2; 
      let sunified = sunify (sget free) (sget typ) in 
      tv := Link (supdate t2 sunified)) 
  | (t1, TVar ({contents = Unbound (_, s, _)} as tv)) -> (
      occurs tv t1; 
      let sunified = sunify (sget free) (sget typ) in 
      tv := Link (supdate t1 sunified)) 
  | (Arr (targ1, tres1, s1), Arr (targ2, tres2, s2)) -> (
      unify targ1 targ2; unify tres1 tres2; unify free typ)
  | (_, _) -> raise (UnificationError "Cannot unify types")
and sunify (sfree: security) (slevel: security): security =
  if sequal sfree slevel then sfree
  else match (sfree, slevel) with
  | (High, Low) -> Low
  | (Unknown, s) -> s
  | (s, Unknown) -> s
  | (_, _) -> raise (UnificationError "Cannot unify security levels")
and greater_than_equal (free: t) (typ: t): bool = 
  if (equal (supdate free Unknown) (supdate typ Unknown)) && not (lteq (sget free) (sget typ))
  then true
  else false

let rec typecheck (env: 'a Env.t) (expr: expr): t =
  match expr with
  | Var x -> inst (lookup env x)
  | Func (x, e) -> (
      let x_type = fresh (sfresh ()) in 
      let e_type = typecheck (update env x x_type) e in 
      Arr (x_type, e_type, sfresh ()))
  | Call (e1, e2) -> (
      let func_type = typecheck env e1 in 
      let arg_type = typecheck env e2 in 
      let res_type = fresh (sfresh ()) in 
      unify (Arr (arg_type, res_type, sfresh ())) func_type;
      res_type)
  | Let (x, e1, e2) -> (
      enter_level ();
      let e1_type = typecheck env e1 in 
      exit_level ();
      typecheck (update env x e1_type) e2)
  | At (e, s) -> (
      match e with
      | Var x -> ( 
        let x_type = inst (lookup env x) in 
        let new_env = update env x (supdate x_type s) in 
        typecheck new_env e)
      | _ -> raise (TypeError "Ascription can only be used with variables"))
  | Bool e -> Bool bot
  | Int e -> Int bot
  | If (condition, if_expr, else_expr) -> (
      let cond_type = typecheck env condition in 
      unify cond_type (Bool Unknown);
      let if_type = typecheck env if_expr in 
      let else_type = typecheck env else_expr in
      screep (join if_type else_type) (sget cond_type))
  | And (e1, e2) -> (
      let t1 = typecheck env e1 in 
      let t2 = typecheck env e2 in 
      unify t1 (Bool Unknown);
      unify t2 (Bool Unknown);
      join t1 t2)
  | Or (e1, e2) -> (
      let t1 = typecheck env e1 in 
      let t2 = typecheck env e2 in 
      unify t1 (Bool Unknown);
      unify t2 (Bool Unknown);
      join t1 t2)
  | Not e -> (
    let t1 = typecheck env e in 
    unify t1 (Bool Unknown);
    t1)
  | Plus (e1, e2) -> (
      let t1 = typecheck env e1 in 
      let t2 = typecheck env e2 in 
      unify t1 (Int Unknown);
      unify t2 (Int Unknown);
      join t1 t2)
  | Minus (e1, e2) -> (
      let t1 = typecheck env e1 in 
      let t2 = typecheck env e2 in 
      unify t1 (Int Unknown);
      unify t2 (Int Unknown);
      join t1 t2)
  | Multiply (e1, e2) -> (
      let t1 = typecheck env e1 in 
      let t2 = typecheck env e2 in 
      unify t1 (Int Unknown);
      unify t2 (Int Unknown);
      join t1 t2)
  | Divide (e1, e2) -> (
      let t1 = typecheck env e1 in 
      let t2 = typecheck env e2 in 
      unify t1 (Int Unknown);
      unify t2 (Int Unknown);
      join t1 t2)
  
let rec generalize (t: t): t =
  match t with
  | TVar {contents = Unbound (name, s, level)} -> if level > !curr_level then QVar (name, s) else t
  | TVar {contents = Link typ} -> generalize typ
  | Arr (targ, tres, s) -> Arr (generalize targ, generalize tres, s)
  | typ -> typ 

(* fun x -> let y = fun z -> z in y *)
let _ = print_endline (print_type (typecheck env (Func ("x", Let ("y", Func ("z", Var "z"), Var "y")))))
(* fun x -> let y = x in y *)
let _ = print_endline (print_type (typecheck env (Func ("x", Let ("y", Var "x", Var "y")))))
(* fun x -> let y = fun z -> x in y *)
let _ = print_endline (print_type (typecheck env (Func ("x", Let ("y", Func ("z", Var "x"), Var "y")))))

(* Ascription *)
(* fun x -> let y = fun z -> z at High in y *)
let _ = print_endline (print_type (typecheck env (Func ("x", Let ("y", Func ("z", At (Var "z", High)), Var "y")))))

let env = update env "x" (Bool High)
let env = update env "y" (Bool Low)
let t = typecheck env (Func ("y", Var "x"))
let env = update env "fizz" t
let _ = print_endline (print_type (typecheck env (Call (Var "fizz", Var "y"))))
let env = update env "fizz" (typecheck env (Call (Var "fizz", Var "y")))
let _ = print_endline (print_type (typecheck env (Call (Var "fizz", Var "x"))))