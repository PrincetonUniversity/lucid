open Batteries
open Syntax
open SyntaxUtils
open Collections

exception AnonymizerError of string

let replacer =
  object (self)
    inherit [_] s_map as super

    method! visit_exp env exp =
      match exp.e with 
      | EDown e -> e
      | _ -> exp
  end
;;

let elim_sec_casts ds = replacer#visit_decls () ds
