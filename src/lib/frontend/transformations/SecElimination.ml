open Batteries
open Syntax
open SyntaxUtils
open Collections

exception AnonymizerError of string

let replacer =
  object (self)
    inherit [_] s_map as super

    (* method! visit_EDown env e =
      e *)

    (* method! visit_exp env exp =
      match exp.e with 
      | ECall (cid, lst) -> 
        (match cid with 
        | Id ("Anonymizer.create", _) -> 
          if (List.length lst) <> 0 then raise (AnonymizerError "Wrong number of arguments to Anonymizer module")
          else let new_exp_ty = 
            match exp.ety with 
            | Some ty -> { ty with tsec = Declassify } 
            | None -> raise (AnonymizerError "Hopefully shouldn't get here") in 
            { exp with ety = Some new_exp_ty }
        | Id (_, _) | _ -> exp)
      | _ -> exp *)
  end
;;

let set_declassifiers ds = replacer#visit_decls () ds
