(* convert into single assignment form *)
open Syntax
open Printf
open BatMap
open Batteries
open InterpHelpers
open MiscUtils
module CL = Caml.List

module DBG = BackendLogging
let outc = ref None
let dprint_endline = ref DBG.no_printf



let log_prog comment ds = 
  !dprint_endline ("-----"^comment^"-------");
  !dprint_endline (Printing.decls_to_string ds);
  !dprint_endline ("-----end "^comment^"-------")
;;


(* 
  Conversion to SSA form: 
  1. walk the syntax tree and maintain a context that maps each 
  original variable id to its most recent SSA variable id. 
  In each expression, replace every id with the most recent SSA id. 
  In each local declaration, add a new entry to the context. 
  In each assign statement, update the context. 
  After branches finish (when leaving an if or match node), merge 
  contexts by generating phi calls. A phi call of the form a_x = phi(a_p, a_q) 
  means that a_x is either a_p or a_q depending on which branch was taken. 
  The phi calls will be eliminated later, by assign statements inlined
  into the branches. 
*)

module SSA = struct 
  (* map original variable name to current SA names *)
  module IdTbl = struct 
    type t = Id.t
    let compare = Id.compare
    type entry = 
      | Bound of (Id.t * ty)
      | Phi of Id.t list * ty
      | Unbound
  end
  module IT = BatMap.Make(IdTbl)
  open IdTbl

  let it_fresh () = IT.empty
  ;;

  let it_get it var_id = 
    IT.find_default IdTbl.Unbound var_id it
  ;;

  let it_get_ty it var_id = 
    match IT.find var_id it with 
      | IdTbl.Unbound -> error "[singleAssignment] tried to get type of undeclared variable."
      | IdTbl.Bound(_, ty) -> ty
      | IdTbl.Phi(_, ty) -> ty
  ;;

  (* initialize variable *)
  let it_set it orig_id ty = 
    IT.add orig_id (IdTbl.Bound(orig_id, ty)) it
  ;;

  let it_update it orig_id new_id = 
    match it_get it orig_id with 
      | Unbound -> error "tried to update variable ssa id, but variable is not ever initialized"
      | Phi _ -> error "unexpected phi in IdTbl"
      | Bound(_, ty) -> IT.add orig_id (IdTbl.Bound(new_id, ty)) it 
  ;;

  (* init table with parameters from body *)
  let it_with_params params = 
    let fold_f it (id, ty) = 
      it_set it id ty   
    in 
    CL.fold_left fold_f (it_fresh ()) params 
  ;;

  (* fold an entry from a path into an id table representing multiple paths. 
  happens at a join point, i.e., the next statement after an if or match. *)
  let it_add_path_entry (orig_id:IdTbl.t) (entry:IdTbl.entry) it  = 
    let cur_branch_ids = match (it_get it orig_id) with 
      | Unbound -> [] 
      | Bound(branch_id, _) -> [branch_id]
      | Phi(branch_ids, _) -> branch_ids 
    in 
    let new_branch_ids, ty = match entry with 
      | Unbound -> error "trying to merge unbound entry into post-branch id table..."
      | Bound(branch_id, ty) -> [branch_id], ty
      | Phi(branch_ids, ty) -> branch_ids, ty
    in 
    let new_branch_ids = unique_list_of (cur_branch_ids@new_branch_ids) in 
    let new_entry = match new_branch_ids with 
      | [] -> error "no branch ids..."
      | [new_branch_id] -> Bound(new_branch_id, ty) 
      | _ -> Phi(new_branch_ids, ty)
    in 
    IT.add orig_id new_entry it 
  ;;

  (* merge id tables from multiple branches together *)
  let merge_branch_idtbls idtbls = 
    (* fold a table into merged *)
    let fold_into_merged merged_it it = 
      IT.fold it_add_path_entry it merged_it 
    in 
    CL.fold_left fold_into_merged (CL.hd idtbls) (CL.tl idtbls)
  ;;

  let phi_cid = Cid.fresh ["phi"]
  let create_phi_call orig_id alt_ids ty = 
    let phi_args = CL.map evar alt_ids in 
    let phi_call = Syntax.exp (ECall(phi_cid, phi_args)) in 
    let new_id = Id.refresh orig_id in 
    let phi_stmt = slocal new_id ty phi_call in 
    phi_stmt, new_id
  ;;

  (* eliminate all phis by adding statements that set each phi variable. *)
  let create_phi_calls it = 
    let phi_to_call id entry (new_stmts, new_it) = 
      match entry with 
        | IdTbl.Unbound -> (new_stmts, IT.add id entry new_it)
        | IdTbl.Bound(_) -> (new_stmts, IT.add id entry new_it)
        | IdTbl.Phi(alt_ids, ty) -> 
          (* add a phi call statement that sets a new variable name, bind variable to that name. *)
          let phi_args = CL.map evar alt_ids in 
          let phi_call = Syntax.exp (ECall(phi_cid, phi_args)) in 
          let new_id = Id.refresh id in 
          let phi_stmt = slocal new_id ty phi_call in 
          let new_entry = Bound(new_id, ty) in 
          new_stmts@[phi_stmt], (IT.add id new_entry new_it)
    in
    IT.fold phi_to_call it ([], it_fresh ())
  ;;


  (* update variables in exp to use most recent ids from table *)
  let update_exp it exp = 
    let v = object (_)
      inherit [_] s_map as super
      method !visit_id _ id = 
        match (it_get it id) with 
          | Unbound -> id
          | Bound (cur_id, _) -> cur_id
          | Phi(_) -> error "[update_exp] current reference to variable is phi "
      end
    in 
    v#visit_exp () exp 
  ;;

  let update_exps_in_stmt it stmt = 
    let v = object (_)
      inherit [_] s_map as super
      method !visit_id _ id = 
        match (it_get it id) with 
          | Unbound -> id
          | Bound (cur_id, _) -> cur_id
          | Phi(_) -> error "[update_exp] current reference to variable is phi "
      end
    in 
    v#visit_statement () stmt
  ;;

  (* convert a statement tree into ssa form with phi calls *)
  let rec stmt_to_ssa (it, stmt) = 
    match stmt.s with 
      (* locals initialize a new entry in id table *)
      | SLocal(id, ty, exp) -> 
        let new_exp = update_exp it exp in 
        let new_it = it_set it id ty in
        (new_it, {stmt with s=SLocal(id, ty, new_exp)})      
      (* assigns update an entry in id table *)
      | SAssign(id, exp) -> 
        let new_id = Id.refresh id in 
        (* replace all the variables with their latest ids *)
        let new_exp = update_exp it exp in 
        (* bind variable to new id. *)
        let new_it = it_update it id new_id in 
        (* get the type. *)
        let ty = it_get_ty it id in 
        (new_it, {stmt with s=SLocal(new_id, ty, new_exp)})
      (* traverse each branch separately, 
      then merge the resulting id tables and create phi calls *)
      | SIf(exp, stmt1, stmt2) -> 
        let it1, stmt1 = stmt_to_ssa (it, stmt1) in 
        let it2, stmt2 = stmt_to_ssa (it, stmt2) in       
        let merged_it = merge_branch_idtbls [it1; it2] in 
        (* create phi calls for all the variables set 
        in multiple paths and update the table. *)
        let phi_stmts, merged_it = create_phi_calls merged_it in 
        let new_sif = sifte exp stmt1 stmt2 in       
        let new_stmt = fold_stmts (new_sif::phi_stmts) in 
        merged_it, new_stmt
      | SMatch(exps, branches) -> 
        let exps = CL.map (update_exp it) exps in 
        let branches, branch_its = CL.fold_left 
          (fun (branches, branch_its) branch -> 
            let pats, stmt = branch in 
            let branch_it, branch_stmt = stmt_to_ssa (it, stmt) in 
            branches@[(pats, branch_stmt)], branch_its@[branch_it]
          )
          ([], [])
          branches 
        in 
        let merged_it = merge_branch_idtbls branch_its in 
        let phi_stmts, merged_it = create_phi_calls merged_it in 
        let new_smatch = {stmt with s=SMatch(exps, branches)} in 
        let new_stmt = fold_stmts (new_smatch::phi_stmts) in 
        merged_it, new_stmt
      (* traverse the statements sequentially *)
      | SSeq(stmt1, stmt2) -> 
        let it1, stmt1 = stmt_to_ssa (it, stmt1) in 
        let it2, stmt2 = stmt_to_ssa (it1, stmt2) in 
        (it2, {stmt with s=SSeq(stmt1, stmt2)})
      (* all other nodes are leaves in the 
      statement tree and we just need to update expressions. *) 
      | SNoop | SUnit _ | SPrintf _ | SGen _ | SRet _ -> (it, update_exps_in_stmt it stmt)
  ;; 

  (* convert the entire program into ssa form *)
  let to_ssa ds = 
    let v = object (_)  
      inherit [_] s_map as super
      (* transform each body. *)
      method !visit_body _ (params, statement) = 
        let _, statement = stmt_to_ssa (it_with_params params, statement) in 
        (params, statement)
      end
    in 
    v#visit_decls () ds
  ;;
end


module PhiElimination = struct 
  (* eliminate phi calls from an ssa form program
  by setting the variable in alternative branches *)


  (* 
  New algorithm: 
  for a straight-line sequence of statements sl: (st1, st2, ..., stn)
    1. eliminate phis: 
      for each phi call statement in sl: (phi_out = phi(phi_arg1, phi_arg2, ...))
        // there will _always_ be a branch immediately before the phi call, 
        // but there might also be an assign before the branch. 
        find the earliest statement in sl that sets a phi_arg 
          case 1: it is an assign or local 
            - declare and set phi_out immediately after
          case 2: it is the if block immediately before the phi use
            - declare phi_out = 0 right before the if block
        inline phi_out sets after phi_arg sets in _the rest of the subtree_
        delete the phi call statement
    2. recurse:
      recurse on the straight line sequences in each branch from sl
    3. return the straight line sequence
  *)

  (* is a statement a phi call *)
  let is_phi_call st = match st.s with 
    | SAssign(_, exp) | SLocal(_, _, exp) -> (
      match exp.e with
        | ECall(fcn_id, _) -> (fcn_id == SSA.phi_cid)
        | _ -> false
    )
    | _ -> false
  ;;

  (* (phi_out, phi_out_ty, [phi_args] *)
  let args_of_phi_call st = match st.s with 
    SLocal(phi_out, phi_out_ty, rhs) -> (
      match rhs.e with 
        | ECall(_, args) -> (phi_out, phi_out_ty, CL.map name_from_exp args)
        | _ -> error "[args_of_phi_call] not a phi call"
    )
    | _ -> error "[args_of_phi_call] not a phi call"
  ;;


  let stmt_sets_phi_arg st ids = 
    match st.s with 
      | SAssign(lhs, _) | SLocal(lhs, _, _) -> CL.mem (Cid.id lhs) ids
      | _ -> false
  ;;


  (* inline a phi call branch and also declare phi_out *) 
  let inline_with_decl phi_out phi_args st = 
    let phi_arg, phi_arg_ty = match st.s with 
    | SLocal(phi_arg, phi_arg_ty, _) -> phi_arg, phi_arg_ty
    | _ -> error "[inline_with_decl] argument statement is not a local declaration"
    in 
    let dec_phi_st = slocal phi_out phi_arg_ty (evar phi_arg) in 
    let new_st = sseq st dec_phi_st in 
    let phi_args = list_remove phi_args (Cid.id phi_arg) in 
    phi_args, new_st (* remaining args and new statmeent. *)
  ;;

  (* inline phi_out after every assignment of phi_args in the subtree *)
  let inline_phi_in_subtree phi_out phi_args st = 
    let v = object (_)  
      inherit [_] s_map as super
      val mutable unmatched_phi_args = phi_args 
      method unmatched_phi_args = unmatched_phi_args

      method !visit_statement ctx statement = 
        match statement.s with 
        | SLocal(id, _, _) -> (
          match (CL.mem (Cid.id id) unmatched_phi_args) with 
            (* this statement sets a phi_arg, so also set the phi_out and adjust remaining args *)
            | true -> (            
              let set_phi_st = sassign phi_out (evar id) in 
              let new_s = sseq statement set_phi_st in 
              unmatched_phi_args <- list_remove unmatched_phi_args (Cid.id id);
              new_s
            )
            | false -> statement
        )
        | _ -> super#visit_statement ctx statement 

      end
    in 
    let new_st = v#visit_statement () st in 
    let remaining_phi_args = v#unmatched_phi_args in 
    remaining_phi_args, new_st
  ;;

  let eliminate_single_phi sts phi_call = 
    let phi_out, phi_out_ty, phi_args = args_of_phi_call phi_call in 
    (* add the phi_out declaration. This will either happen the first time a phi_arg is declared, 
    or immediately before the branch statement that caused the phi to be generated. *)

    (* fold over statements, eliminating each phi arg as it appears *)
    let fold_f phi_out phi_out_ty (new_sts, is_declared, remaining_phi_args) st = 
      match remaining_phi_args with 
      (* no remaining phi args, so the only thing left to do is skip the phi_call statement. *)
      | [] -> (
        let new_sts = match st.s with 
          | SLocal(_, _, exp) -> (
            match exp.e with 
              | ECall(cid, _) -> (
                match (cid = SSA.phi_cid) with 
                  | true -> new_sts
                  | false -> new_sts@[st]
              )
              | _ -> new_sts@[st]
          )
          | _ -> new_sts@[st]
        in 
        (new_sts, is_declared, remaining_phi_args)
      )
      | _ -> (
        match is_declared with 
        (* phi_out is already declared. Just add an assign to phi_out 
        after each phi_arg that appears in a statement or subtree subtree. *) 
        | true ->
            let remaining_phi_args, new_st = inline_phi_in_subtree phi_out remaining_phi_args st in 
            (new_sts@[new_st], is_declared, remaining_phi_args)
        (* phi_out is not declared yet. Find an assignment to a phi_arg 
        or a branch with such an assignment in it. *)
        | false -> (
          match (stmt_sets_phi_arg st remaining_phi_args) with 
          (* This statement directly sets one of the arguments. Declare and set phi_out. *)
          | true -> (
            let remaining_phi_args, new_st = inline_with_decl phi_out remaining_phi_args st in 
            let is_declared = true in 
            (new_sts@[new_st], is_declared, remaining_phi_args)
          )
          (* This statement does not directly set a phi arg, but it may be a branch with a nested set *)
          | false -> (
            let final_remaining_phi_args, new_st = inline_phi_in_subtree phi_out remaining_phi_args st in 
            match ((CL.length final_remaining_phi_args) = (CL.length remaining_phi_args)) with 
            (* no args are set *)
            | true -> 
              (new_sts@[st], is_declared, remaining_phi_args)
            (* some args were set. We also need to add a stmt 
            to declare phi_out before the branch *)
            | false -> (
              (* need to get the type of the phi_arg statements *)
              let dec_phi_st = slocal phi_out phi_out_ty (eval_vint_ty 0 phi_out_ty) in 
              let new_st = sseq dec_phi_st new_st in 
              (new_sts@[new_st], true, final_remaining_phi_args)
            )
          )
        )
      )
    in 
    let new_sts, _, remaining_phi_args = CL.fold_left (fold_f phi_out phi_out_ty) ([], false, phi_args) sts in 
    match remaining_phi_args with 
      | [] -> fold_stmts new_sts |> unfold_stmts (* fold and unfold because caller expects a list not a tree *)
      | _ -> error "[eliminate_single_phi] failed to find one of the arguments to a phi function in the statement list..."
  ;; 

  (* eliminate all phis from a statement sequence and recurse on branches *)
  let rec eliminate_all_phis sts = 
    !dprint_endline "[eliminate_all_phis] called!";
    let local_phi_calls = CL.filter is_phi_call sts in 
    let sts_without_local_phis = CL.fold_left eliminate_single_phi sts local_phi_calls in 

    (* recurse on any branch that you find branches *)
    let map_f st = 
      let inner_map_f st = unfold_stmts st |> eliminate_all_phis |> fold_stmts in 
      !dprint_endline (sprintf "[map_f] \n %s" (Printing.stmt_to_string st));      
      match st.s with 
        | SIf(exp, st1, st2) -> (
          let st1 = inner_map_f st1 in 
          let st2 = inner_map_f st2 in 
          {st with s=SIf(exp, st1, st2)}
        )
        | SMatch(exps, branches) -> 
          let pats, stmts = CL.split branches in 
          let stmts = CL.map inner_map_f stmts in 
          let branches = CL.combine pats stmts in 
          {st with s=SMatch(exps, branches)}
        | SSeq(_) -> trans_info ("Sequence statement!\n"^(Printing.stmt_to_string st)); error "[eliminate_all_phis] unexpected: sequence statement in list-of-statements!"
        | _ -> st
    in 
    CL.map map_f sts_without_local_phis
  ;;

  (* public function. eliminate all phis in every handler body. *)
  let eliminate_phis ds = 
    let v = object (_)  
      inherit [_] s_map as super
      method !visit_body _ (params, statement)  = 
        let statement = 
          unfold_stmts statement |>
          eliminate_all_phis |>
          fold_stmts
        in 
        (params, statement)
      end
    in 
    v#visit_decls () ds
  ;;
end


let transform ds = 
  DBG.start_mlog __FILE__ outc dprint_endline;
  trans_info "transforming to single assignment form";  
  log_prog "before SSA" ds;
  let ds = SSA.to_ssa ds in 
  log_prog "after SSA" ds;
  trans_info "in single assignment form. removing Phi calls ";  
  let ds = PhiElimination.eliminate_phis ds in 
  log_prog "after phi_out inlining" ds;
  trans_info "phi calls eliminates ";  
  ds
;;