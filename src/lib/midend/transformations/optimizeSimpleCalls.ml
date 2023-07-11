(*
eliminate the extra stage of overhead in code like this:

int<<32>> update_or_get_s_ret = 32;
if (dst==0) {
  update_or_get_s_ret = Array.update(arr_short_0,src,write,src,write,src);
} else {
  update_or_get_s_ret = Array.get(arr_short_0,src);
}
short_recs_0 = update_or_get_s_ret;

When partial interpretation is done, we may be able to eliminate this.

Note: This pass depends on return variables ending in "_ret". It will 
only optimize for those variables. 


*)
open CoreSyntax
open Batteries


let find_ret_vars ds = 
  let retvars = ref [] in 
  let v = 
    object
      inherit [_] s_iter as super

      method! visit_cid _ cid = 
        let last_name = Cid.names cid |> List.rev |> List.hd in 
        if (String.ends_with last_name "_ret") 
        then (
          retvars := cid::(!retvars))
        (* Cid.to_ids *)
    end
  in 
  v#visit_decls () ds;
  !retvars
;;

let count_uses ds tgt_cid = 
  (* print_endline("[count_uses] processing tgt_cid: "^(Cid.to_string tgt_cid)); *)
  let num_uses = ref 0 in 
  let last_copied_to = ref None in 

  (* we want to know the last assignment that is an assignment of tgt_cid to some variable *)
  let copy_from_tgt_cid stmt = 
    match stmt.s with 
      | SAssign(id, {e=EVar(cid);_}) -> (
          if (Cid.equals tgt_cid cid)
          then (Some(id))
          else (None)
        )
      | _ -> None
  in

  let v = 
    object
      inherit [_] s_iter as super

      method! visit_statement in_exp stmt = 
        (match (copy_from_tgt_cid stmt) with 
          | None -> (();)
          | Some id -> (last_copied_to := Some id;)
        );
        super#visit_statement in_exp stmt

(*       method! visit_SAssign _ id exp = 
        let num_uses_pre = !num_uses in 
        super#visit_exp true exp;
        let num_uses_post = !num_uses in 
        (* if tgt_cid was used, change last assigned *)
        if (num_uses_pre <> num_uses_post)
        then (last_assigned_to := Some (id);) *)

      method! visit_exp _ exp = 
        (* print_endline ("[count_uses] visit_exp:\n"^(CorePrinting.exp_to_string exp)); *)
        super#visit_exp true exp      
      (* have to make sure that the exp is _only_ tgt_cid, nothing more. *)
      method! visit_cid in_exp cid = 
(*         print_endline ("[count_uses] visiting cid: "^(Cid.to_string cid));
        print_endline ("[count_uses] tgt_cid: "^(Cid.to_string tgt_cid));        
        print_endline ("[count_uses] in_exp: "^(string_of_bool in_exp)); *)
        if ((in_exp) & (Cid.equals cid tgt_cid))
        then (num_uses := (!num_uses) + 1)
    end
  in 
  v#visit_decls false ds;
(*   print_endline("[count_uses] DONE processing tgt_cid: "^(Cid.to_string tgt_cid));
 *)
  !num_uses, !last_copied_to
;;



  (* ensure outer_id is declared before ret_id *)
let is_declared_before ds outer_id ret_id  = 
  let outer_declared_first = ref None in 
  let v = 
    object
      inherit [_] s_iter as super
      method! visit_SLocal _ id _ _ =        
        match (!outer_declared_first) with 
          (* if we previously found either declaration, 
             there's nothing to do. *)
          | Some _ -> ()
          | None -> (
          (* set outvar based on first decl found *)
            if (Id.equals outer_id id) 
            then (
              outer_declared_first := Some true;
            ) 
            else (
              if (Id.equals ret_id id) 
              then (
                outer_declared_first := Some false;
              )
            )
          )
    end
  in
  v#visit_decls () ds;
  Option.get (!outer_declared_first)
;;

(* delete the declaration of local var tgt_id *)
let delete_cid_local ds tgt_id = 
  let v = 
    object
      inherit [_] s_map as super

      method! visit_SLocal _ id ty exp = 
        if (Id.equals id tgt_id)
        then (SNoop)
        else (SLocal(id, ty, exp))
    end
  in 
  v#visit_decls () ds
;;

(* replace assignments to tgt_id with assignments to new_id *)
let replace_assignments_to ds tgt_id new_id = 
  let v = 
    object
      inherit [_] s_map as super

      method! visit_SAssign _ id exp = 
        if (Cid.equals id tgt_id)
        then (SAssign(new_id, exp))
        else (SAssign(id, exp))
    end
  in 
  v#visit_decls () ds
;;

(* delete the assignment outer := tgt *)
let delete_assignment ds outer_id ret_id = 
  let v = 
    object
      inherit [_] s_map as super
      method !visit_statement _ st = 
(*         print_endline("[delete_assignment] on:");
        print_endline (CorePrinting.statement_to_string st); *)
        let s = match st.s with 
          | SAssign(id, {e=EVar(cid)}) -> (
            (* print_endline ("match SAssign"); *)
            if ((Id.equals id (outer_id)) & (Id.equals (Cid.to_id cid) ret_id))
            then (SNoop)
            else (st.s)
          )
          | _ -> st.s
        in 
        super#visit_statement () {st with s=s}        
    end
  in 
  v#visit_decls () ds
;;


let eliminate_single_use_retvars ds = 
  (* find all the return variables generated by an earlier pass in the frontend. 
     This is dangerous. Should find a better way to do this. *)
  let retvars = find_ret_vars ds in 
  let uses_assignments =  List.map 
    (count_uses ds)
    retvars
  in 
  (* print_endline "[eliminate_single_use_retvars] retvar ids"; *)
  let retvar_strs = List.map (Cid.to_string) retvars in 
  String.concat "," retvar_strs |> print_endline;
  let retvar_ids = List.map Cid.to_id retvars in 
  let rv_data = List.combine retvar_ids uses_assignments in 

  let eliminated_retvar_ct = ref 0 in 
  let transform_for_single_var ds (retvar, (count, last_assigned_to_opt)) =
    (* print_endline("[eliminate_single_use_retvars] transforming: "^(Id.to_string retvar)); *)
    (* if there is only one use of ret, and it was assigned to var outer *)    
    match (count, last_assigned_to_opt) with 
      | (1, Some outervar) -> (
        (* print_endline "[eliminate_single_use_retvars] only one use and an assignment -- okay."; *)
        (* if outer is declared before retvar *)
        if (is_declared_before ds outervar retvar)
        then (
          (* delete declarations of retvar *)
          let ds = delete_cid_local ds retvar in 
          (* change assignments to retvar --> assignments to outervar *)
          let ds = replace_assignments_to ds retvar outervar in 
          (* delete the assignment outervar := retvar;*)
          let ds = delete_assignment ds outervar retvar in 
          eliminated_retvar_ct := (!eliminated_retvar_ct) + 1;
          (* print_endline "[eliminate_single_use_retvars] transformed."; *)
          ds
        )
        else 
          ds
      )
      | (_, _) -> 
        ds
  in 
  let new_ds = List.fold_left transform_for_single_var ds rv_data in 
(*   print_endline 
    ("[optimizeSimpleCalls.transform] eliminated retvars: "
      ^(string_of_int (!eliminated_retvar_ct))
    ); *)
  new_ds 



  (* 
    for each variable ret with "_ret" in the name, 
    if there is only one use of ret, 
      and that use is an assignment to foo
      and foo is declared before ret
      - delete the declaration of the ret variable
      - replace all assignments to the ret variable with assignments to foo

      int ret = 1;
      ret = 2; 
      int foo = 2;
      foo = ret;

      


  *)

;;

