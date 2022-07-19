(* partial P4-tofino syntax *)

(* Simple language:
    create, update, and print ints
    int x = 1;
    int y = 2;
    int z = 1 + 2;
    z = x + y;
    print z;
*)
exception Error of string
let error s = raise (Error s)

open Id
open Batteries

type id = [%import: Id.t]

and value = 
 | VInt of int 

and ty =
 | TInt

and op = 
 | OPlus

and exp = 
 | EVar of id
 | EVal of value
 | EOp of op * exp list

and statement = 
 | SCreate of id * ty * exp
 | SAssign of id * exp
 | SPrint of exp

and statements = statement list


(*** simple interpreter ***)
type var_ctx = (id * value) list

let empty_var_ctx = []
let eval_value v =
    match v with
    | VInt(i) -> i
;;

let rec eval_exp (var_ctx:var_ctx) exp : value = 
    match exp with 
    | EVar(id) -> (
        match (List.assoc_opt id var_ctx) with 
        | None -> error @@ "unbound identifier in expression: " ^ (Id.to_string id)
        | Some v -> v
    )
    | EVal(v) -> v
    | EOp (op, exps) -> 
        let vs = List.map (eval_exp var_ctx) exps in
        match op with
        | OPlus -> 
            VInt(List.fold
                (fun acc v -> match v with 
                    | VInt(i) -> acc + i)
                0
                vs)
;;

let eval_stmt (var_ctx:var_ctx) stmt = 
    match stmt with 
    | SCreate(id, _, exp) ->
        (id, eval_exp var_ctx exp)::var_ctx
    | SAssign(id, exp) -> (
        match List.assoc_opt id var_ctx with 
            | None -> error "assigning to undeclared variable."
            | Some _ -> 
                let new_val = eval_exp var_ctx exp in 
                (id, new_val)::(List.remove_assoc id var_ctx)
    )
    | SPrint(exp) -> eval_exp var_ctx exp |> eval_value |> string_of_int |> print_endline; var_ctx
;;

let rec eval_stmts var_ctx stmts = 
    match stmts with
    | [] -> var_ctx
    | stmt::stmts ->
        (* evaluate first statement *)
        let ctx = eval_stmt var_ctx stmt in 
        (* evaluate the rest*)
        eval_stmts ctx stmts
;;


