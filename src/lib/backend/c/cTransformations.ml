(* Some basic transformations applied to c code. 
   In one file because they should not be complicated. *)

open CoreSyntax 

let rec unfold_stmts (st : statement) =
  match st.s with
  | SNoop -> []
  | SSeq (s1, s2) -> unfold_stmts s1 @ unfold_stmts s2
  | _ -> [st]
;;
let rec sequence_stmts lst =
  match lst with
  | [] -> snoop
  | { s = SNoop } :: tl -> sequence_stmts tl
  | [hd] -> hd
  | hd :: tl -> sseq hd (sequence_stmts tl)
;;

let rec fold_stmts (sts : statement list) : statement = sequence_stmts sts

let rec strip_noops statement : statement = 
  (* turn into a list *)
  let statements = unfold_stmts statement in
  (* recurse on if and match branches *)
  let statements = List.map 
    (fun s -> match s.s with 
      | SIf (e, s1, s2) -> {s with s=SIf(e,(strip_noops s1),(strip_noops s2))}
      | SMatch (e, branches) -> {s with s=SMatch(e,(List.map (fun (p, s) -> (p, strip_noops s)) branches))}
      | _ -> s) statements in
  (* turn back into a sequence *)
  fold_stmts statements
;;

let strip_noops = 
  object inherit [_] s_map as super
  method! visit_body () (params, stmt) = 
    (params, strip_noops stmt)
  end
;;
