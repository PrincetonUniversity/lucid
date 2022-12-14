(* convert into single assignment form *)
open CoreSyntax
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
  !dprint_endline ("-----" ^ comment ^ "-------");
  !dprint_endline (Printing.decls_to_string ds);
  !dprint_endline ("-----end " ^ comment ^ "-------")
;;

(* convert handlers to ssa form, with phis. Each declaration is annotated with the phis contained within. *)
type decl_with_phis =
  { dec : decl
  ; phis : statement list
  }

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
      | Phi of Id.t list * ty * Id.t option (* SSA ids in branches, type, SSA id before branch *)
      | Unbound
  end

  module IT = BatMap.Make (IdTbl)
  open IdTbl

  let it_fresh () = IT.empty
  let it_get it var_id = IT.find_default IdTbl.Unbound var_id it

  let it_get_ty it var_id =
    match IT.find var_id it with
    | IdTbl.Unbound ->
      error "[singleAssignment] tried to get type of undeclared variable."
    | IdTbl.Bound (_, ty) -> ty
    | IdTbl.Phi (_, ty, _) -> ty
  ;;

  (* initialize variable *)
  let it_set it orig_id ty = IT.add orig_id (IdTbl.Bound (orig_id, ty)) it

  let it_update it orig_id new_id =
    match it_get it orig_id with
    | Unbound ->
      error
        "tried to update variable ssa id, but variable is not ever initialized"
    | Phi _ -> error "unexpected phi in IdTbl"
    | Bound (_, ty) -> IT.add orig_id (IdTbl.Bound (new_id, ty)) it
  ;;

  (* init table with parameters from body *)
  let it_with_params params =
    let fold_f it (id, ty) = it_set it id ty in
    CL.fold_left fold_f (it_fresh ()) params
  ;;

  (* fold an entry from a path into an id table representing multiple paths.
     happens when you finish processing an if or match statement *)
  let it_add_path_entry (orig_id : IdTbl.t) (entry : IdTbl.entry) it =
    let cur_branch_ids =
      match it_get it orig_id with
      | Unbound -> []
      | Bound (branch_id, _) -> [branch_id]
      | Phi (branch_ids, _, _) -> branch_ids
    in
    let new_branch_ids, ty =
      match entry with
      | Unbound ->
        error "trying to merge unbound entry into post-branch id table..."
      | Bound (branch_id, ty) -> [branch_id], ty
      | Phi (branch_ids, ty, _) -> branch_ids, ty
    in
    let new_branch_ids = unique_list_of (cur_branch_ids @ new_branch_ids) in
    let new_entry =
      match new_branch_ids with
      | [] -> error "no branch ids..."
      | [new_branch_id] -> Bound (new_branch_id, ty)
      | _ -> Phi (new_branch_ids, ty, None)
    in
    IT.add orig_id new_entry it
  ;;

  (* new_idtbl[var_id] = pre_branch_idtbl[var_id] *)
  let set_pre_branch_phi pre_branch_idtbl post_branch_idtbl orig_var_id =
    let pre_branch_id = it_get pre_branch_idtbl orig_var_id in
    let post_branch_id = it_get post_branch_idtbl orig_var_id in
    let new_post_branch_id =
      match pre_branch_id, post_branch_id with
      | Bound (pre_id, _), Phi (post_ids, ty, None) ->
        Phi (post_ids, ty, Some pre_id)
      | _ ->
        error
          "[set_pre_branch_phi] unexpected binding when trying to set \
           pre_branch_id in a phi binding."
    in
    IT.add orig_var_id new_post_branch_id post_branch_idtbl
  ;;

  (* merge id tables from multiple branches together *)
  let merge_branch_idtbls pre_branch_idtbl idtbls =
    (* fold a table into merged *)
    let fold_into_merged merged_it it =
      IT.fold it_add_path_entry it merged_it
    in
    let merged_idtbl =
      CL.fold_left fold_into_merged (CL.hd idtbls) (CL.tl idtbls)
    in
    (* now, for every phi in the merged idtable, use pre_branch_idtbl to add the SSA id used before the branch.
      If a phi doesn't have an entry in pre_branch, that means the same id
      is declared in two branches -- an error.
      This id is a "dominator" because by the time the phi statement executes,
      the SSA id in the pre_branch will definitely be set. *)
    let is_phi_kv _ v =
      match v with
      | Phi _ -> true
      | _ -> false
    in
    let phi_ids =
      IT.filter is_phi_kv merged_idtbl |> IT.keys |> BatList.of_enum
    in
    let merged_idtbl_with_dominator =
      CL.fold_left (set_pre_branch_phi pre_branch_idtbl) merged_idtbl phi_ids
    in
    merged_idtbl_with_dominator
  ;;

  (* let phi_cid = Cid.fresh ["phi"] *)
  let fresh_phi_cid () = Cid.fresh ["phi"]

  (*   let create_phi_call orig_id alt_ids ty =
    let phi_args = CL.map evar alt_ids in
    let phi_call = Syntax.exp (ECall(phi_cid, phi_args)) in
    let new_id = Id.refresh orig_id in
    let phi_stmt = slocal new_id ty phi_call in
    phi_stmt, new_id
  ;;
 *)
  (* eliminate all phis by adding statements that set each phi variable. *)
  (* phi call convention: the first argument is the id used BEFORE the branch .*)
  let create_phi_calls it =
    let phi_to_call id entry (new_stmts, new_it, new_phi_calls) =
      match entry with
      | IdTbl.Unbound -> new_stmts, IT.add id entry new_it, new_phi_calls
      | IdTbl.Bound _ -> new_stmts, IT.add id entry new_it, new_phi_calls
      (* This transforms a phi back to a bound variable. *)
      | IdTbl.Phi (alt_ids, ty, Some pre_phi_id) ->
        (* add a phi call statement that sets a new variable name, bind variable to that name. *)
        (* FIXME: Not totally sure if this is right *)
        let phi_ty = ty in
        let phi_args =
          CL.map (BatPervasives.flip evar phi_ty) (pre_phi_id :: alt_ids)
        in
        let phi_call = Syntax.exp (ECall (fresh_phi_cid (), phi_args)) ty in
        let new_id = Id.refresh id in
        let phi_stmt = slocal new_id ty phi_call in
        let new_entry = Bound (new_id, ty) in
        ( new_stmts @ [phi_stmt]
        , IT.add id new_entry new_it
        , phi_stmt :: new_phi_calls )
      | IdTbl.Phi (_, _, None) ->
        error "[create_phi_call] cannot create a call without a pre-phi id"
    in
    IT.fold phi_to_call it ([], it_fresh (), [])
  ;;

  (* update variables in exp to use most recent ids from table *)
  let update_exp it exp =
    let v =
      object
        inherit [_] s_map as super

        method! visit_id _ id =
          match it_get it id with
          | Unbound -> id
          | Bound (cur_id, _) -> cur_id
          | Phi _ -> error "[update_exp] current reference to variable is phi "
      end
    in
    v#visit_exp () exp
  ;;

  let update_exps_in_stmt it stmt =
    let v =
      object
        inherit [_] s_map as super

        method! visit_id _ id =
          match it_get it id with
          | Unbound -> id
          | Bound (cur_id, _) -> cur_id
          | Phi _ -> error "[update_exp] current reference to variable is phi "
      end
    in
    v#visit_statement () stmt
  ;;

  (* log the number of new variables created by this process. *)
  let new_vars = ref []

  (* convert a statement tree into ssa form with phi calls *)
  let rec stmt_to_ssa (it, stmt) =
    match stmt.s with
    (* locals initialize a new entry in id table *)
    | SLocal (id, ty, exp) ->
      let new_exp = update_exp it exp in
      let new_it = it_set it id ty in
      new_it, { stmt with s = SLocal (id, ty, new_exp) }, []
    (* assigns update an entry in id table *)
    | SAssign (id, exp) ->
      let new_id = Id.refresh id in
      new_vars := new_id :: !new_vars;
      (* replace all the variables with their latest ids *)
      let new_exp = update_exp it exp in
      (* bind variable to new id. *)
      let new_it = it_update it id new_id in
      (* get the type. *)
      let ty = it_get_ty it id in
      new_it, { stmt with s = SLocal (new_id, ty, new_exp) }, []
    (* traverse each branch separately,
      then merge the resulting id tables and create phi calls *)
    | SIf (exp, stmt1, stmt2) ->
      let exp = update_exp it exp in
      let it1, stmt1, phi_calls1 = stmt_to_ssa (it, stmt1) in
      let it2, stmt2, phi_calls2 = stmt_to_ssa (it, stmt2) in
      (* Left off here, considering giving info about the dominating variable: the merged table could use the original table too? *)
      let merged_it = merge_branch_idtbls it [it1; it2] in
      (* create phi calls for all the variables set
        in multiple paths and update the table. *)
      let phi_stmts, merged_it, new_phi_calls = create_phi_calls merged_it in
      !dprint_endline "[stmt_to_ssa] adding phi_stmts after if:";
      CL.iter (fun st -> !dprint_endline (Printing.stmt_to_string st)) phi_stmts;
      let new_sif = sifte exp stmt1 stmt2 in
      let new_stmt = fold_stmts (new_sif :: phi_stmts) in
      merged_it, new_stmt, phi_calls1 @ phi_calls2 @ new_phi_calls
    | SMatch (exps, branches) ->
      let exps = CL.map (update_exp it) exps in
      let branches, branch_its, branch_phi_calls =
        CL.fold_left
          (fun (branches, branch_its, branch_phi_calls) branch ->
            let pats, stmt = branch in
            let branch_it, branch_stmt, local_phi_calls =
              stmt_to_ssa (it, stmt)
            in
            ( branches @ [pats, branch_stmt]
            , branch_its @ [branch_it]
            , local_phi_calls @ branch_phi_calls ))
          ([], [], [])
          branches
      in
      let merged_it = merge_branch_idtbls it branch_its in
      let phi_stmts, merged_it, new_phi_calls = create_phi_calls merged_it in
      let new_smatch = { stmt with s = SMatch (exps, branches) } in
      let new_stmt = fold_stmts (new_smatch :: phi_stmts) in
      merged_it, new_stmt, branch_phi_calls @ new_phi_calls
    (* traverse the statements sequentially *)
    | SSeq (stmt1, stmt2) ->
      let it1, stmt1, phis1 = stmt_to_ssa (it, stmt1) in
      let it2, stmt2, phis2 = stmt_to_ssa (it1, stmt2) in
      it2, { stmt with s = SSeq (stmt1, stmt2) }, phis1 @ phis2
    (* all other nodes are leaves in the
      statement tree and we just need to update expressions. *)
    | SNoop | SUnit _ | SPrintf _ | SGen _ | SRet _  | STableMatch _ ->
      it, update_exps_in_stmt it stmt, []
  ;;

  let to_ssa decs =
    let handler_to_ssa dec =
      match dec.d with
      | DHandler (id, (params, statement)) ->
        let _, new_stmt, phi_calls =
          stmt_to_ssa (it_with_params params, statement)
        in
        let dec = { dec with d = DHandler (id, (params, new_stmt)) } in
        { dec; phis = phi_calls }
      | _ -> { dec; phis = [] }
    in
    CL.map handler_to_ssa decs
  ;;
end

module PhiElimination = struct
  (* PHI ELIMINATION (5/26) *)
  (* Goal: eliminate each phi statement, of the form:
      int x~new = phi(x~d, x~b1, x~b2, ...);
      phi conventions:
      - the first argument, x~d is the a dominating variable -- it will always be bound when phi is called.
      - the remaining arguments are bound depending on which control path the program takes.
      algorithm:
      1. find the statement that binds x~d, replace it with a statement that binds x~d and then,
         immediately after, binds x~new to x~d.
          - edge case: if x~d is a parameter, we put the bind x~new at the beginning of the program.
      2. for each of the other phi arguments x~bk:
          find the statement that binds x~bk, replace it with a statement that binds x~bk then SETS x~new=x~bk
      3. delete the phi statement.

      - this is a much simpler algorithm than the stuff below. its probably easiest to start fresh.
  *)
  type phi_rec =
    { phi_out : Id.t
    ; phi_ty : ty
    ; phi_dom : Id.t
    ; phi_args : Id.t list
    }

  let id_of_exp exp =
    match exp.e with
    | EVar (Id id) -> id
    | _ ->
      error
        "[NewPhiElimination@id_of_exp] expression had wrong form to convert \
         into id"
  ;;

  let rec_of_phi phi : phi_rec =
    match phi.s with
    | SLocal (out_id, ty, phi_exp) ->
      (match phi_exp.e with
      | ECall (_, phi_dom_exp :: phi_arg_exps) ->
        { phi_out = out_id
        ; phi_ty = ty
        ; phi_dom = id_of_exp phi_dom_exp
        ; phi_args = CL.map id_of_exp phi_arg_exps
        }
      | _ ->
        error
          "[NewPhiElimination@rec_of_phi] phi statement did not have a call on \
           rhs")
    | _ ->
      error
        "[NewPhiElimination@id_of_exp] phi statement not a local var \
         declaration."
  ;;

  let bind_at_dominator params phi stmt =
    (* the goal of this function is to find the right place to put bind_out *)
    let bind_out =
      slocal phi.phi_out phi.phi_ty (evar phi.phi_dom phi.phi_ty)
    in
    let _, _ = params, phi in
    let param_ids = CL.split params |> fst in
    match CL.mem phi.phi_dom param_ids with
    (* the dominator is a parameter, so bind out_var at top of body. *)
    | true -> sseq_sp bind_out stmt stmt.sspan
    (* the dominator is not a parameter. So we need to find where it is assigned and bind after that. *)
    | false ->
      let v =
        object
          inherit [_] s_map as super

          method! visit_statement ctx statement =
            match statement.s with
            | SLocal (id, _, _) ->
              (match Id.equal id phi.phi_dom with
              | true ->
                !dprint_endline
                  (sprintf
                     "[bind_at_dominator] binding %s after %s;"
                     (Id.to_string phi.phi_out)
                     (Printing.stmt_to_string statement));
                sseq_sp statement bind_out statement.sspan
              | false -> statement)
            | _ -> super#visit_statement ctx statement
        end
      in
      v#visit_statement () stmt
  ;;

  (* add the statement out = arg; right after arg is bound by a phi statement *)
  let set_arg_at_phi out stmt arg =
    let set_out ty = sassign out (evar arg ty) in
    let v =
      object
        inherit [_] s_map as super
        val mutable did_set = false
        method did_set = did_set

        method! visit_statement ctx statement =
          match statement.s with
          | SLocal (id, ty, exp) ->
            (* ty ?? = ?? *)
            (match Id.equal id arg, exp.e with
            | true, ECall (fcn_id, _) ->
              (* ty arg = ?(...) *)
              (match Id.name (Cid.to_ids fcn_id |> CL.hd) = "phi" with
              | true ->
                (* ty arg = phi(...);*)
                !dprint_endline
                  (sprintf
                     "[set_arg_at_phi] setting %s after %s;"
                     (Id.to_string out)
                     (Printing.stmt_to_string statement));
                did_set <- true;
                sseq_sp statement (set_out ty) statement.sspan
              | false -> statement)
            | _ -> statement)
          | _ -> super#visit_statement ctx statement
      end
    in
    let stmt = v#visit_statement () stmt in
    stmt, v#did_set
  ;;

  let set_arg_at_nonphi out stmt arg =
    let set_out ty = sassign out (evar arg ty) in
    let v =
      object
        inherit [_] s_map as super

        method! visit_statement ctx statement =
          match statement.s with
          | SLocal (id, ty, _) ->
            (match Id.equal id arg with
            | true ->
              !dprint_endline
                (sprintf
                   "[set_at_arg] setting %s after %s;"
                   (Id.to_string out)
                   (Printing.stmt_to_string statement));
              sseq_sp statement (set_out ty) statement.sspan
            | false -> statement)
          | _ -> super#visit_statement ctx statement
      end
    in
    v#visit_statement () stmt
  ;;

  (* add the statement out = arg; right after arg is bound.
     if out is set by a phi statement, we need to add it
     after the phi statement. *)
  let set_at_arg out stmt arg =
    let stmt, phi_set = set_arg_at_phi out stmt arg in
    match phi_set with
    | true -> stmt
    | false -> set_arg_at_nonphi out stmt arg
  ;;

  let set_at_args phi stmt =
    let unique_args =
      unique_list_of phi.phi_args |> fun xs -> list_remove xs phi.phi_dom
    in
    let new_stmt = CL.fold_left (set_at_arg phi.phi_out) stmt unique_args in
    new_stmt
  ;;

  let delete_phi_stmt phi stmt =
    (* find the statement that binds phi_out = phi(...); and delete it. *)
    let v =
      object
        inherit [_] s_map as super

        method! visit_statement ctx statement =
          match statement.s with
          | SLocal (id, _, exp) ->
            (match Id.equal id phi.phi_out, exp.e with
            | true, ECall (fcn_id, _) ->
              (match Id.name (Cid.to_id fcn_id) = "phi" with
              (* found the phi call <ty phi_out = phi(...);>*)
              | true -> snoop_sp statement.sspan
              | false -> super#visit_statement ctx statement)
            | _ -> super#visit_statement ctx statement)
          | _ -> super#visit_statement ctx statement
      end
    in
    v#visit_statement () stmt
  ;;

  (* eliminate a single phi from a handler body.*)
  let body_elim_phi phi (params, stmt) =
    !dprint_endline
      ("[body_elim_phi] eliminating phi:\n" ^ Printing.stmt_to_string phi);
    let phi_rec = rec_of_phi phi in
    (* 1. bind the out_variable of the phi when the dominator arg is bound. *)
    bind_at_dominator params phi_rec stmt
    (* 2. set out_variable whenever an argument is bound. *)
    |> set_at_args phi_rec
  ;;

  (* eliminate all phis from a handler's body. *)
  let rec body_elim_phis phis (params, stmt) =
    !dprint_endline "[body_elim_phis] eliminating phis:";
    CL.iter (fun phi -> !dprint_endline (Printing.stmt_to_string phi)) phis;
    !dprint_endline "[body_elim_phis] ---------";
    match phis with
    | [] -> stmt
    | [phi] -> body_elim_phi phi (params, stmt)
    | phi :: phis ->
      let new_stmt = body_elim_phi phi (params, stmt) in
      body_elim_phis phis (params, new_stmt)
  ;;

  let body_delete_phis phis stmt =
    CL.fold_left (fun stmt phi -> delete_phi_stmt phi stmt) stmt phis
  ;;

  (* eliminate phis from a single handler. *)
  let handler_elim_phis annotated_d =
    match annotated_d.dec.d with
    | DHandler (id, (params, stmt)) ->
      let new_stmt =
        body_elim_phis annotated_d.phis (params, stmt)
        |> body_delete_phis (CL.map rec_of_phi annotated_d.phis)
      in
      { annotated_d.dec with d = DHandler (id, (params, new_stmt)) }
    | _ ->
      error
        "[handler_elim_phis] elimination of phi statements in non-handlers is \
         not supported. "
  ;;

  (* eliminate phis from handler bodies. *)
  let elim_phis annotated_ds =
    let decl_elim_phis annotated_d =
      match annotated_d.phis with
      | [] -> annotated_d.dec
      | _ -> handler_elim_phis annotated_d
    in
    CL.map decl_elim_phis annotated_ds
  ;;
end

let transform ds =
  DBG.start_mlog __FILE__ outc dprint_endline;
  trans_info "transforming to single assignment form";
  log_prog "before SSA" ds;
  let phi_annotated_ds = SSA.to_ssa ds in
  log_prog "after SSA" (CL.map (fun a_d -> a_d.dec) phi_annotated_ds);
  trans_info "in single assignment form. removing Phi calls ";
  let ds = PhiElimination.elim_phis phi_annotated_ds in
  log_prog "after phi_out inlining" ds;
 (*  !dprint_endline
    ((CL.length !SSA.new_vars |> string_of_int)
    ^ " new variables created in transformation to SSA:\n"
    ^ (CL.map Cid.id !SSA.new_vars |> DebugPrint.cids_to_string)); *)
  trans_info "phi calls eliminates ";
  (*   exit 1; *)
  ds
;;
