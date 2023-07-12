open Batteries
open Syntax
open SyntaxUtils
open Collections

type sec = High | Low
module Env = Map.Make(String)
type env = sec Env.t 

let lookup (env: env) (id: id): t =
  Env.find id env

let update (env: env) (id: id) (t: ty): env =
  Env.add id t env

let replacer =
  object (self)
    inherit [_] s_map as super

    method! visit_DHandler env id handler_sort body =
      let (params, statement) = body in 
      let params = self#visit_params env params in 
      let statement = self#visit_statement env statement in 
      DHandler(id, handler_sort, params * statement)
    
    method! visit_params env params =
      match params with
      | [] -> params
      | hd :: tl -> 
          let (id, ty) = hd in 
          let env = update env id ty in 
          self#visit_params env tl
      
    method! visit_statement env statement =
      match statement.s with
      | SNoop -> statement.s
      | SUnit of exp -> 
      | SLocal of id * ty * exp
      | SAssign of id * exp
      | SPrintf of string * exp list
      | SIf of exp * statement * statement
      | SGen of gen_type * exp
      | SRet of exp option
      | SSeq of statement * statement
      | SMatch of exp list * branch list
      | SLoop of statement * id * size
      | STableMatch of tbl_match
      | STableInstall of exp * tbl_entry list
  end
;;

let eliminate_prog ds = replacer#visit_decls Env.empty ds
