(* 
  Match statements with many branches are expensive 
  to place into shared tables during the layout phase. 
  This pass identifies match tables with many branches
  and annotates them as "solitary," so that they 
  are placed in their own tables. *)
module TCOld = TofinoCore
open TofinoCoreNew

let is_simple_branch stmt =
  let remove_noops st = InterpHelpers.unfold_stmts st |> InterpHelpers.fold_stmts in
  match ((remove_noops stmt).s) with 
  | SSeq(_)
  | SMatch(_) 
  | SIf(_) -> false
  | _ -> true
;; 

let is_solitary bs =
  let res = List.fold_left (fun acc (_, stmt) -> 
    ((is_simple_branch stmt) & acc)
    ) true bs
  in
  res
;;

let rec process (tds: TCOld.tdecl list) thresh_len : TCOld.tdecl list=   
    let v = 
        object
            inherit [_] TCOld.s_map as super
            method! visit_statement ctx stmt =
              let stmt = super#visit_statement ctx stmt in 
              match (stmt.s) with 
                | SMatch(_, bs) -> (
                if (is_solitary bs
                  & ((List.length bs) >= thresh_len))
                then (
                  {stmt with spragma = Some("solitary", [])}
                )
                else (stmt)
              )
              | _-> stmt
        end
    in
    v#visit_tdecls () tds
;;

let process_core thresh_len (core_prog:prog) = 
  let v = 
    object
        inherit [_] TofinoCoreNew.s_map as super
        method! visit_statement ctx stmt =
          let stmt = super#visit_statement ctx stmt in 
          match (stmt.s) with 
            | SMatch(_, bs) -> (
            if (is_solitary bs
              & ((List.length bs) >= thresh_len))
            then (
              {stmt with spragma = Some("solitary", [])}
            )
            else (stmt)
          )
          | _-> stmt
    end
in
v#visit_prog () core_prog
;;
