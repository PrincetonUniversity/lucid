type id = string
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
| QVar of id
| Arr of t * t
and tv = Unbound of id * level | Link of t

type expr = 
| Var of id
| Func of id * expr
| Call of expr * expr
| Let of id * expr * expr

let lookup (env: 'a Env.t) (id: id): t =
  Env.find id env

let update (env: 'a Env.t) (id: id) (t: t): 'a Env.t =
  Env.add id t env

let fresh (): t =
  TVar (ref (Unbound (gensym (), !curr_level)))

let reset (): unit = 
  reset_gensym ();
  reset_level ()

let rec equal (t1: t) (t2: t): bool =
  let tv_equal (tv1: tv ref) (tv2: tv ref): bool =
    match (!tv1, !tv2) with
    | (Unbound (x, l1), Unbound (y, l2)) -> String.equal x y
    | (Link t1, Link t2) -> equal t1 t2
    | (_, _) -> false in
  match (t1, t2) with
  | (TVar tv1, TVar tv2) -> tv_equal tv1 tv2
  | (QVar x, QVar y) -> String.equal x y
  | (Arr (targ1, tres1), Arr (targ2, tres2)) -> (equal targ1 targ2) && (equal tres1 tres2)
  | (_, _) -> false

let tveq (tv1: tv ref) (tv2: tv ref): bool =
  match (!tv1, !tv2) with
  | (Unbound (x, l1), Unbound (y, l2)) -> String.equal x y
  | (Link t1, Link t2) -> equal t1 t2
  | (_, _) -> false

let inst (t: t): t =
  let rec loop (env: 'a Env.t) (typ: t): t * 'a Env.t =
    match typ with
    | QVar name -> (
        try (lookup env name, env)
        with Not_found ->
          let tv = fresh () in
          (tv, update env name tv))
    | TVar {contents = Link typ'} -> loop env typ'
    | Arr (targ, tres) -> 
        let (targ, env) = loop env targ in
        let (tres, env) = loop env tres in
        (Arr (targ, tres), env)
    | typ' -> (typ', env) in
  fst (loop Env.empty t)

let rec occurs (tv: tv ref) (t: t): unit = 
  match t with
  | TVar ({contents = Unbound (name, level)} as tvref) -> (
      if tveq tv tvref then raise (UnificationError ("Occurs check failed"));
      let min_level = 
      match !tv with
      | Unbound (_, lev) -> min lev level 
      | _ -> level in 
      tvref := Unbound (name, min_level))
  | TVar ({contents = Link typ} as tvref) -> (
      if tveq tv tvref then raise (UnificationError ("Occurs check failed"));
      occurs tv typ)
  | Arr (t1, t2) -> occurs tv t1; occurs tv t2
  | _ -> ()

let rec unify (t1: t) (t2: t): unit =
  if equal t1 t2 then ()
  else match (t1, t2) with
  | (TVar {contents = Link t1}, t2) -> unify t1 t2
  | (t1, TVar {contents = Link t2}) -> unify t1 t2
  | (TVar ({contents = Unbound _} as tv), t2) -> occurs tv t2; tv := Link t2 
  | (t1, TVar ({contents = Unbound _} as tv)) -> occurs tv t1; tv := Link t1
  | (Arr (targ1, tres1), Arr (targ2, tres2)) -> unify targ1 targ2; unify tres1 tres2
  | (_, _) -> raise (UnificationError "Cannot unify types")

let rec typecheck (env: 'a Env.t) (expr: expr): t =
  match expr with
  | Var x -> inst (lookup env x)
  | Func (x, e) -> (
      let x_type = fresh () in 
      let e_type = typecheck (update env x x_type) e in 
      Arr (x_type, e_type))
  | Call (e1, e2) -> (
      let func_type = typecheck env e1 in 
      let arg_type = typecheck env e2 in 
      let res_type = fresh () in 
      unify func_type (Arr (arg_type, res_type));
      res_type)
  | Let (x, e1, e2) -> (
      enter_level ();
      let e1_type = typecheck env e1 in 
      exit_level ();
      typecheck (update env x e1_type) e2)
  
let rec generalize (t: t): t =
  match t with
  | TVar {contents = Unbound (name, level)} -> if level > !curr_level then QVar name else t
  | TVar {contents = Link typ} -> generalize typ
  | Arr (targ, tres) -> Arr (generalize targ, generalize tres)
  | typ -> typ 

let rec print_type (t: t): string = 
  match t with
  | TVar tvref -> (
      match !tvref with
      | Unbound (name, level) -> "TVar: Unbound " ^ name ^ ", level: " ^ string_of_int level
      | Link typ -> "TVar: Link " ^ print_type typ)
  | QVar id -> "QVar: " ^ id
  | Arr (targ, tres) -> print_type targ ^ " -> " ^ print_type tres


(* fun x -> let y = fun z -> z in y *)
let _ = print_endline (print_type (typecheck env (Func ("x", Let ("y", Func ("z", Var "z"), Var "y")))))
(* fun x -> let y = x in y *)
let _ = print_endline (print_type (typecheck env (Func ("x", Let ("y", Var "x", Var "y")))))
(* fun x -> let y = fun z -> x in y *)
let _ = print_endline (print_type (typecheck env (Func ("x", Let ("y", Func ("z", Var "x"), Var "y")))))
