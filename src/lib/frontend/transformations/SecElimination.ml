open Batteries
open Syntax
open SyntaxUtils
open Collections
open Typer

exception AnonymizerError of string

let replacer =
  object (self)
    inherit [_] s_map as super

    method! visit_exp env exp =
      match exp.e with 
      | EDown e -> e
      | EUp e -> e
      | ECall (f, args) -> 
        let args = List.map (fun arg -> self#visit_exp env arg) args in 
        { exp with e = ECall (f, args) }
      | _ -> exp
  end
;;

let elim_sec_casts ds = replacer#visit_decls () ds
