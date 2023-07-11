(* 

  After this pass, generate statements only need to set multicast ID and egress port. 


    - All event parameters are set in this pass. 
    - event count is set (really: mc_group_a)
    - After this pass, to translate a generate statement, you must: 
      1. all events: set event_generated flag
      2. port generate: set egress_port and port_evid
      3. ports generate: create multicast group and set ports_mcid
  *)

open CoreSyntax
open TofinoCore
open CoreCfg
exception Error of string
let error s = raise (Error s)

let orig param_id =
  Id.create ("pre_gen_copy_"^(fst param_id))
;;

let size_of_tint ty = 
  match ty.raw_ty with 
  | TInt(sz) -> sz
  | _ -> error "[size_of_tint] not a tint"
;;

(* generate statements that set the parameter 
   variables of evid to eargs *)
let set_event_params tds ev_cid arg_exps =
  let param_ids = List.assoc (Cid.to_id ev_cid) ((main tds).hdl_params) |> List.split |> fst in 
  let set_event_param (param_id, arg_exp) =
    (* hack to work around data dependency graph. *)
    (* scall_sp (Cid.concat scopy_fcn (Cid.id param_id)) [] param_ty Span.default *)

    (sassign (Cid.id param_id) arg_exp)
  in
  let res = List.map set_event_param (List.combine param_ids arg_exps) in
  let _ = res in 
  [] (* test!*)
;;

(* id = e + i; *)
let sassign_incr id ty e i =
  sassign id
  (op_sp
    Plus
    [
      e;
      ((vint (i) (size_of_tint ty)) |> value_to_exp)
    ]
    ty
    Span.default
  )
;;

let incr_recirc_mcid tds =
  let ct_var, ct_ty = (main tds).event_output.recirc_mcid_var in 
  sassign_incr (Cid.id ct_var) ct_ty (var_sp (Cid.id ct_var) ct_ty Span.default) 1
;;

let scopy_fcn = Cid.create ["Sys"; "copy"]

let scopy_to_orig (param_id, param_ty) = 
  sassign (Cid.id (orig param_id)) (var_sp (Cid.id param_id) param_ty Span.default)
;;

let scopy_to_origs params = 
  List.map scopy_to_orig params 
;;


let invalidate_fcn = Cid.create ["Sys"; "invalidate"]
let scall_invalidate evid evparams =
  let args = List.map
    (fun (id, ty) -> var_sp (Cid.id id) ty Span.default)
    evparams
  in
  (* pass the parameters of the header being invalidated so that 
     the invalidate statement maintains the correct data dependency. *)
  (* Note: coreDfg special-cases invalidate calls, treating the call 
     as though it _writes_ to the variables, whereas normally a call 
     only reads. *)
  (* let ev_args = [(var_sp (Cid.id evid) (ty TEvent) Span.default)] in *)
  let invalidate_fcn = Cid.concat invalidate_fcn (Cid.id evid) in 
  scall_sp invalidate_fcn args (ty TBool) Span.default
  (* snoop *)
;;


(* invalidate evid and copy all params to new vars *)
let sinvalidate_and_copy (evid, evparams) = 
  InterpHelpers.fold_stmts
    ((List.map scopy_to_orig evparams)@[(scall_invalidate evid evparams)])
;;

let prepend_invalidate mk_copies (evid, evparams) stmt =
  match mk_copies with 
  | true -> sseq (sinvalidate_and_copy (evid, evparams)) stmt
  | false -> sseq (scall_invalidate evid evparams) stmt
;;

let append_invalidate mk_copies (evid, evparams) stmt = 
  match mk_copies with 
  | true -> sseq stmt (sinvalidate_and_copy (evid, evparams)) 
  | false -> sseq stmt (scall_invalidate evid evparams)
;;

(* prepend only the copy instructions *)
let prepend_copy evparams stmt = 
  InterpHelpers.fold_stmts 
    ((scopy_to_origs evparams)@[stmt])
;;

(* are any of these variables used in the statement? *)
let used_in varids stmt = 
  let found = ref false in 
  let v = 
    object 
      inherit [_] s_iter as super
      method! visit_id _ id =
        found := (!found) || (MiscUtils.contains varids id);        
    end
  in
  v#visit_statement () stmt;
  !found
;;

(* find the right place in the statement to invalidate evid
   and make copies of all parameters. This gets called 
   on sibling branches of self generate statements. *) 
let rec traverse_to_invalidate_core mk_copies (evid, (evparams:params)) stmt = 
  let evids = evparams |> List.split |> fst in  
  match stmt.s with
  | SSeq(s1, s2) -> (
    match (used_in evids s1, used_in evids s2) with 
    (* s2 uses params, so invalidate there *)
    | _, true -> sseq s1 (traverse_to_invalidate_core mk_copies (evid, evparams) s2)
    (* s1 uses params, so invalidate there *)
    | true, false -> sseq (traverse_to_invalidate_core mk_copies (evid, evparams) s1) s2
    (* neither uses params, so invalidate before either *)
    | false, false -> prepend_invalidate mk_copies (evid, evparams) stmt
    )      
  | SIf(e, s1, s2) ->
    let s1 = traverse_to_invalidate_core mk_copies (evid, evparams) s1 in
    let s2 = traverse_to_invalidate_core mk_copies (evid, evparams) s2 in
    {stmt with s=SIf(e, s1, s2)}
  | SMatch(es, bs) -> 
    let new_bs = List.map (fun (p, s) -> (p, traverse_to_invalidate_core mk_copies (evid, evparams) s)) bs in
    {stmt with s=SMatch(es, new_bs)}
  (* leaf nodes: if a leaf node uses the param, append. If it doesnt, prepend.*)
  | _ -> (
    match (used_in evids stmt) with 
    | true -> append_invalidate mk_copies (evid, evparams) stmt
    | false -> prepend_invalidate mk_copies (evid, evparams) stmt
  )
;;

let traverse_to_copy_and_invalidate = traverse_to_invalidate_core true;;
let traverse_to_invalidate = traverse_to_invalidate_core false;;

let replace_id stmt (old_id,new_id) = 
  let v = 
    object 
      inherit [_] s_map as super
      method! visit_id _ id =
        if (Id.equal id old_id)
        then (new_id)
        else (id)
    end
  in
  v#visit_statement () stmt;
;;


(* replace evparams with orig_evparams *)
let replace_params evids stmt =  
  (* replacement map *)
  let rename_map = List.map
    (fun param_id -> (param_id, orig param_id))
    evids
  in
  List.fold_left replace_id stmt rename_map
;;


(* traverse the statement, looking for a statement generating 
   evid. When you find it: 
    1. add copy statements before it;
    2. add invalidates on all adjacent branches;
    3. "switch" into traverse_to_replace mode *)

let rec find_rec_generate params_used_after tds (evid, (evparams:params)) stmt =
  let evids = evparams |> List.split |> fst in
  match stmt.s with 
  | SSeq(s1, s2) -> (
    (* traverse 1 seeking a self generate. If you find it, switch into 
       replacement mode for 2. *)
    let params_used_after_s1 = params_used_after || (used_in evids s2) in 
    let s1_new, gen_in_s1 = find_rec_generate params_used_after_s1 tds (evid, evparams) s1 in
    let s2_new, gen_in_s2 = match (gen_in_s1) with 
      | true -> (
        match params_used_after_s1 with 
          | true -> replace_params evids s2, true
          | false -> s2, true
      )
      | false -> find_rec_generate params_used_after tds (evid, evparams) s2
    in
    sseq s1_new s2_new, gen_in_s2
  )
  | SMatch(es, bs) -> (
    let pats, stmts = List.split bs in    
    let stmts_gen_elims = List.map (find_rec_generate params_used_after tds (evid, evparams)) stmts in
    let generate_found = List.fold_left (fun found_somewhere found_in -> 
      found_somewhere || found_in)
      false
      (stmts_gen_elims |> List.split |> snd)
    in
    match generate_found with 
    | true -> (
      let new_stmts = List.map 
        (fun (stmt, gen_eliminated) -> 
          if (gen_eliminated)
          then (
            (* if a generate was eliminated, use that statement *)
            stmt
          ) 
          else (
            if (params_used_after)
            then (traverse_to_copy_and_invalidate (evid, evparams) stmt)
            else (traverse_to_invalidate (evid, evparams) stmt)
          )
        )
        stmts_gen_elims
      in
      {stmt with s=(SMatch(es,(List.combine pats new_stmts)))}, true
    )
    | false -> 
      stmt, false
  )
  | SIf(e, s1, s2) -> (
    (* try to replace generate in each branch *)
    let s1_new, gen_in_s1 = find_rec_generate params_used_after tds (evid, evparams) s1 in
    let s2_new, gen_in_s2 = find_rec_generate params_used_after tds (evid, evparams) s2 in
    (* if there was a branch with a generate, all the branches with out the generate 
       must be traversed to add invalidates *)
    (* those branches might also need to cpy param vals, if they are ever used again. *)
    match (gen_in_s1, gen_in_s2) with 
    (* s1 had the generate. s2 needs to invalidate *)
    | (true, false) -> (
      let s2_new = if (params_used_after)
        then (traverse_to_copy_and_invalidate (evid, evparams) s2)
        else (traverse_to_invalidate (evid, evparams) s2)
      in
      (* we eliminated a generate and our caller needs to know *)
      sifte e s1_new s2_new, true      
    )
    | (false, true) -> (
      let s1_new = if (params_used_after)
        then (traverse_to_copy_and_invalidate (evid, evparams) s1)
        else (traverse_to_invalidate (evid, evparams) s1)
      in
      (* we eliminated a generate and our caller needs to know *)
      sifte e s1_new s2_new, true
    )
    | (false, false) -> (
      (* neither had a generate -- we did nothing. *)
      sifte e s1 s2, false
    )
    | (true, true) -> error "only 1 generate allowed for each evid"
  )
  (* when we find the self generate, 
     add param copy statements and return true  *)
  | SGen(_, ev_exp) -> (
    match ev_exp.e with 
    | ECall(ev_cid, _) -> (
      if (Id.equals (Cid.to_id ev_cid) evid)
      then (InterpHelpers.fold_stmts ((scopy_to_origs evparams)@[stmt]), true)
      else (stmt, false)
    )
    | _ -> stmt, false
  )     
  (* do nothing for all other leaf nodes *)
  | _ -> stmt, false
;;

let paramcopy_in_stmt stmt =
  let found = ref false in 
  let v = 
    object 
      inherit [_] s_iter as super
      method! visit_id _ id =
        found := (!found) || (Base.String.is_prefix (fst id) "pre_gen_copy_")
    end
  in
  v#visit_statement () stmt;
  !found
;;

(* in every control path for the handler of event e, 
   ensure that if another event e is not generated, 
   then e's header is set to invalid. *)
let invalidate_input_event_header_when_not_reused tds rev_hdl_enum hdl_params (pats, stmt) =
  let evnum = match pats with 
    | [PNum(z)] -> Z.to_int z
    | _ -> error "[process_ev_branch] pattern of handler selector should be event num"
  in
  let evid = List.assoc evnum rev_hdl_enum in
  let evparams:params = List.assoc evid hdl_params in 
  (* let paramids = evparams |> List.split |> fst in *)

  (* determine if this handler's body generates its own event. If there 
     is a recursive generate statement, this function will also add 
     statements to invalidate the event's header in all the branches 
     that do _not_ contain a recursive generate statement.  *)
  let new_stmt, recursive_generate_found = find_rec_generate false tds (evid, evparams) stmt in 
  (* if a recursive generate was not found, then we need to 
     invalidate the event's header before the pipeline ends. *)
  let new_stmt = match recursive_generate_found with
    | true -> new_stmt
    | false -> append_invalidate false (evid, evparams) new_stmt
  in
  (* we may have added copies of event header variables, 
     which are declared outside of the statement *)
  let new_vars = match (paramcopy_in_stmt new_stmt) with 
    | true -> List.map (fun (id, ty) -> (orig id, ty)) evparams
    | false -> []
  in
  (pats, new_stmt), new_vars
;;

let count_recirc_generates tds =
  let v = 
    object 
      inherit [_] s_map as super
      method! visit_statement ctx stmt = 
        let stmt = super#visit_statement ctx stmt in 
        match stmt.s with
        | SGen(GSingle(None), _) -> 
          sseq (incr_recirc_mcid tds) stmt 
        | SGen(_) -> 
          stmt
        | _ -> stmt
    end
  in
  v#visit_tdecls () tds;    
;;


let eliminate is_ingress tds =
  TofinoCoreForms.main_with_event_match "Generates.eliminate@start" tds;
  let m = (main tds) in 
  let rev_hdl_enum = List.map (fun (x, y) -> (y, x)) m.hdl_enum in
  let stmt = m.main_body |> List.hd in
  let new_stmt, new_vars = match (stmt.s) with 
    | SMatch(es, bs) -> (
      let new_bs, new_vars = 
        List.map (invalidate_input_event_header_when_not_reused tds rev_hdl_enum m.hdl_params) bs
        |> List.split
      in
      let new_vars = List.flatten new_vars in
      {stmt with s=SMatch(es, new_bs)}, new_vars
    )
    | _ -> error "[generate elimination] body of main handler must start with match on event id"
  in
  (* update the main body and the list of shared locals. *)
  let tds = update_main tds { m with 
    main_body = [new_stmt];
    shared_locals = m.shared_locals@new_vars;
    } 
  in
  (* finally, if this is an ingress program, increment the multicast group for recirculating generates *)
  if (is_ingress) then (count_recirc_generates tds) else (tds)
  (* tds *)
;;