(* various renaming transformation *)

open CCoreSyntax
(* Make events, handlers, and patterns have the same ids.
  (i.e., event "foo" is always id "foo~x" anywhere it appears)   
  Also make events use the same parameter ids as their handlers. *)
  let unify_event_ids decls = 
  (* we can't rename handler args without renaming their uses in the bodies. 
     so we rename event args instead. *)
  (* let event_defs = List.filter_map extract_devent_opt decls in *)
  let handler_defs = List.filter_map extract_dhandle_opt decls in
  let ev_id_map = List.map (fun inp -> 
      let (hdl_cid, _, _, _) = inp in
      (Id.name (Cid.to_id hdl_cid), (Cid.to_id hdl_cid))) 
    handler_defs 
  in
  let param_id_map = List.fold_left 
    (fun acc (hdl_cid, _, params, _) -> 
      let param_id_map = List.map (fun (pid, _) -> (Id.name pid, pid)) params in
      (hdl_cid, param_id_map)::acc)
    []
    handler_defs
  in
  let rename id = 
    match List.assoc_opt (Id.name id) ev_id_map with 
    | Some(id) -> id
    | None -> id
  in
  let rename_cid cid = rename (Cid.to_id cid) |> Cid.id in

  let rename_params evid params = 
    let str_to_id = List.assoc evid param_id_map in
    List.map 
      (fun (param_id, ty) -> 
        match List.assoc_opt (Id.name param_id) str_to_id with 
        | Some(new_id) -> new_id, ty
        | None -> param_id, ty)
      params
  in

  let v = object (_) inherit [_] s_map as super 
    method! visit_decl _ decl = 
      let decl = super#visit_decl () decl in
      match decl.d with 
      | DFun(FHandler, cid, ty, params, stmt_opt) -> 
        let cid = rename_cid cid in
        {decl with d=DFun(FHandler, cid, ty, rename_params (cid) params, stmt_opt)}
      | DEvent(event_def) -> 
        let evconstrid = rename event_def.evconstrid in 
        let evparams = rename_params (Cid.id evconstrid) event_def.evparams in
        {decl with d=DEvent({event_def with evconstrid; evparams})}
      | _ -> decl
    
    method! visit_exp () e = 
      let e = super#visit_exp () e in
      match e.ety.raw_ty, e.e with
        | TEvent, EVar(cid) -> {e with e=EVar(rename_cid cid)}
        | _ -> e

    method! visit_PEvent () event_id params = 
      let params = super#visit_params () params in
      let event_id = rename_cid event_id in
      (* let params = rename_params (Cid.to_id event_id) params in  *)
      PEvent{event_id; params;}
  end in      
  v#visit_decls () decls
;;