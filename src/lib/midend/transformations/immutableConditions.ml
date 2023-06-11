open Batteries
open CoreSyntax
open Collections

(* Remove the very last (non-noop) statement in a sequence.
   If the last statement contains subordinate statements (i.e. it's an
   if/match), then we can remove the last statement of each branch, since only
   one of them will ever execute. *)
let rec chop_last stmt =
  match stmt.s with
  | SSeq (s1, { s = SNoop }) | SSeq ({ s = SNoop }, s1) -> chop_last s1
  | SSeq (s1, s2) -> { stmt with s = SSeq (s1, chop_last s2) }
  | SIf (test, s1, s2) ->
    { stmt with s = SIf (test, chop_last s1, chop_last s2) }
  | SMatch (es, branches) ->
    { stmt with
      s = SMatch (es, List.map (fun (p, s) -> p, chop_last s) branches)
    }
  | _ -> { stmt with s = SNoop }
;;

(* Return a list of variables which are mutated in this statement. Includes
   variables which are defined in a subordinate scope inside the statement,
   so it requires alpha-renaming to really be useful.

   Optimization: We don't care about mutations that are the very last statement
   in a sequence, because there's nothing afterwards for them to affect. So it
   suffices to check all _but_ the last statement.
*)
let assigned_in_stmt stmt =
  let v =
    object
      inherit [_] s_iter as super
      val mutable assigned = CidSet.empty
      method assigned = assigned
      method! visit_SAssign _ id _ = assigned <- CidSet.add id assigned
    end
  in
  v#visit_statement () (chop_last stmt);
  v#assigned
;;

let vars e =
  let v =
    object
      inherit [_] s_iter as super
      val mutable vars = CidSet.empty
      method vars = vars
      method! visit_EVar _ cid = vars <- CidSet.add (cid) vars
    end
  in
  v#visit_exp () e;
  v#vars
;;

(* Ensure that the conditions of any if/match statements never change during
   execution of that statement, by creating static copies of any conditions
   that might be mutated. Assumes that variables have unique names, so no
   shadowing happens inside conditional bodies. *)
let make_conditions_immutable ds =
  let v =
    object (self)
      inherit [_] s_map as super

      method replace_exp mutated e =
        let e_vars = vars e in
        let mutated_e_vars = CidSet.inter e_vars mutated in
        if CidSet.is_empty mutated_e_vars
        then snoop, e
        else (
          (* If any variables in the exp were mutated, store the original
             value of the exp and use that instead. *)
          let new_cond = Id.fresh "new_cond" in
          let asgn = slocal new_cond (ty TBool) e in
          let new_e = exp (EVar (Cid.create_ids [new_cond])) (ty TBool) in
          asgn, new_e)

      method! visit_SIf _ test s1 s2 =
        let s1 = self#visit_statement () s1 in
        let s2 = self#visit_statement () s2 in
        let mutated = CidSet.union (assigned_in_stmt s1) (assigned_in_stmt s2) in
        let preamble, new_test = self#replace_exp mutated test in
        let sequenced = sequence_stmts [preamble; sifte new_test s1 s2] in
        sequenced.s

      (* Basically same as SIf, but multiple tests and branches. We replace
         expressions individually rather than all together, since they're each
         matched independently in potentially many different ways. *)
      method! visit_SMatch _ es branches =
        let branches = List.map (self#visit_branch ()) branches in
        let mutated =
          List.fold_left
            (fun acc (_, s) -> CidSet.union acc (assigned_in_stmt s))
            CidSet.empty
            branches
        in
        let preamble, new_es =
          List.map (self#replace_exp mutated) es |> List.split
        in
        let sequenced = sequence_stmts (preamble @ [smatch new_es branches]) in
        sequenced.s
    end
  in
  v#visit_decls () ds
;;
