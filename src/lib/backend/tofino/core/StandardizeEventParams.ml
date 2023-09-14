open CoreSyntax
(*  for debugging, put the name of the event into each event parameter,
    then make sure events and handlers have the same parameter ids. *)

(* replace old id with new id everywhere *)
let replace_id decl (old_id, new_id) =
  let v =
    object
      inherit [_] s_map as super
      method! visit_id _ id = if Id.equal id old_id then new_id else id
    end
  in
  v#visit_decl () decl
;;

let replace_ids ds changemap = List.fold_left replace_id ds changemap

let embed_eventid_in_paramids ds =
  let rename_event_param evid param =
    let _ = evid in
    let param_id, param_ty = param in
    let new_param_id = Id.create (fst evid ^ "_" ^ fst param_id) in
    new_param_id, param_ty
  in
  let ds =
    List.map
      (fun decl ->
        match decl.d with
        | DEvent (evid, eannot, esort, params) ->
          { decl with
            d =
              DEvent
                (evid, eannot, esort, List.map (rename_event_param evid) params)
          }
        | _ -> decl)
      ds
  in
  ds
;;

let unify_event_and_handler_params ds : decls =
  (* build a map from each event / handler name to its parameter ids *)
  let event_params =
    List.filter_map
      (fun decl ->
        match decl.d with
        | DEvent (id, _, _, params) -> Some (Id.name id, params)
        | _ -> None)
      ds
  in
  List.map
    (fun decl ->
      match decl.d with
      (* for each handler *)
      | DHandler (id, _, (params, _)) ->
        (* look up the parameters of the event with the same name as the handler *)
        let ev_params = List.assoc (Id.name id) event_params in
        (* construct a list of (handler_param_id, event_param_id) pairs *)
        let changemap =
          List.combine (List.split params |> fst) (List.split ev_params |> fst)
        in
        (* replace hdl param id with ev param id everywhere *)
        replace_ids decl changemap
      | _ -> decl)
    ds
;;

let process ds = embed_eventid_in_paramids ds |> unify_event_and_handler_params
