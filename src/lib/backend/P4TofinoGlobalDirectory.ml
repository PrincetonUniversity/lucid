(* build a global directory from the P4Tofino form of a program. 
   assumes that GlobalConstructorTagging.annotate has been run at the 
   start of the frontend (after first type checking) *)
open SyntaxGlobalDirectory
open TaggedCid
module P4 = P4TofinoSyntax
let error = Syntax.error

(*** P4TofinoSyntax --> mat_global constructors ***)
let p4_array_to_arrmeta pipe_id id slot_sz len tcid =
  let full_arr_cid cid = Cid.concat (Cid.id pipe_id) cid in
  Arr({
    name=tcid.tcid;
    compiled_cid=full_arr_cid (Cid.id id);
    length=P4.expr_to_int len;
    cell_size = slot_sz;})
;;

let p4_table_to_tblmeta pipe_id control_id id keys actions length_opt tcid = 
  let full_tbl_cid tid = Cid.concat (Cid.concat pipe_id control_id) tid in
  let full_acn_cid aid = Cid.concat control_id aid in
  let priority_key = 
    {kid = Some(Cid.create ["$MATCH_PRIORITY"]); kty = "exact"; ksize = 32}
  in
  let ekey_to_key (ekey:P4.expr) = 
    match ekey.ex, ekey.ety with 
      |  P4.EVar(cid), P4.TKey(KTernary, sz) -> {kid=Some(cid); kty= "ternary"; ksize=sz;}
      | _ -> error "[p4_table_to_tblmeta] key expression is either not a variable, or not a key type"
  in
  let eaction_to_action (eaction:P4.expr) =
    match eaction.ex, eaction.ety, eaction.espan.global_created_in_src with
      | P4.EVar(cid), P4.TFun(_, arg_tys), Some(tcid) -> 
        {aid=tcid.tcid; acompiled_id=full_acn_cid cid; arg_sizes=List.map P4.ty_to_size arg_tys;}
      | _, _, _ -> error "[p4_table_to_tblmeta] action expression has wrong form, type, or is not annotated."
  in
  let length = match length_opt with
    | Some(length) -> length
    | None -> error "[p4_table_to_tblmeta] a user-defined table with no length?"
  in
  Tbl{
    name=tcid.tcid;
    compiled_cid=full_tbl_cid (Cid.id id);
    length=length;
    keys=(List.map ekey_to_key keys)@[priority_key];
    actions=List.map eaction_to_action actions;
  }
;;


type ctx = 
{
  ctx_dspan : Span.t;
  control_id : Cid.t option;
  pipe_id : Id.t;
}

let build_p4tofinodirectory (prog : P4.tofino_prog) = 
  let subtrees = ref [] in
  let v = object
    inherit [_] P4.s_iter as super
    method! visit_DControl ctx body =
      let ctx' = match ctx.control_id with
        | Some(obj_name) -> {ctx with control_id = Some(Cid.concat obj_name (Cid.id body.id))}
        | None -> {ctx with control_id = Some(Cid.id body.id)}
      in
      super#visit_DControl ctx' body
    
    method! visit_decl ctx decl = 
      (* put span in context for DTable *)
      super#visit_decl {ctx with ctx_dspan = decl.dspan;} decl

    method! visit_DTable ctx id keys actions _ _ size_opt = 
      match ctx.ctx_dspan.global_created_in_src with
      | Some(tcid) ->
        let tblmeta = p4_table_to_tblmeta (Cid.id ctx.pipe_id) (Option.get ctx.control_id) id keys actions size_opt tcid in
        let dir_tree = path_to_dir
          ((tcid_ancestors tcid)@[tcid])
          tblmeta
        in
        subtrees := dir_tree::(!subtrees)
      (* the compiler creates tables without source annotations *)
      | _ -> ()

    method! visit_DReg ctx id _ slot_sz _ len _ = 
      match ctx.ctx_dspan.global_created_in_src with
      | Some(tcid) -> 
        let arrmeta = p4_array_to_arrmeta ctx.pipe_id id slot_sz len tcid in
        let dir_tree = path_to_dir
          ((tcid_ancestors tcid)@[tcid])
          arrmeta
        in
        subtrees := dir_tree::(!subtrees)
      | _ -> error "[build_p4tofinodirectory] reached an un-annotated register array."      
    end  
  in

  v#visit_tofino_prog {ctx_dspan=Span.default; control_id=None; pipe_id = Id.create "pipe"} prog;
  let dir = List.fold_left (merge_roots) empty_dir (!subtrees) in
  dir
;;

let build_global_dir tofino_prog = 
  let dir = build_p4tofinodirectory tofino_prog in
  let json = dir_to_json dir in
  json
;;

