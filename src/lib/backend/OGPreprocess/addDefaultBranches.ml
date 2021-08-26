(* make sure every match statement 
has an explicit default case. *)
open Syntax
module CL = Caml.List

let is_default_pats pats =
  let is_wild pat =
    match pat with
    | PWild -> true
    | _ -> false
  in
  CL.map is_wild pats |> CL.fold_left ( & ) true
;;

let new_default_branch exps : branch = CL.map (fun _ -> PWild) exps, snoop

let add_default_branches ds =
  let v =
    object
      inherit [_] s_map as super

      method! visit_statement ctx statement =
        let new_stmt = super#visit_statement ctx statement in
        match new_stmt.s with
        | SMatch (exps, branches) ->
          (match is_default_pats (CL.rev branches |> CL.hd |> fst) with
          | true -> new_stmt
          | false ->
            let branches = branches @ [new_default_branch exps] in
            { new_stmt with s = SMatch (exps, branches) })
        | _ -> new_stmt
    end
  in
  v#visit_decls () ds
;;
