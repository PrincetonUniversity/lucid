(* Remove operations that only apply in the interpreter:
   - Calls to extern functions (not builtin functions!)
   - Print statements
   - SUnits which aren't a function call (which are semantically no-ops,
     even in the interpreter)
*)

open CoreSyntax
open Batteries

let extract_extern_cids ds =
  let is_extern decl =
    match decl.d with
    | DExtern (id, _) -> Some (Cid.id id)
    | _ -> None
  in
  List.filter_map is_extern ds
;;

let eliminate_prog ds =
  let extern_cids = extract_extern_cids ds in
  let v =
    object
      inherit [_] s_map as super
      method! visit_SPrintf _ _ _ = SNoop

      method! visit_SUnit _ unit_exp =
        match unit_exp.e with
        | ECall (cid, _, _) ->
          if List.exists (Cid.equals cid) extern_cids
          then SNoop
          else super#visit_SUnit () unit_exp
        | _ -> SNoop
    end
  in
  v#visit_decls () ds
;;
