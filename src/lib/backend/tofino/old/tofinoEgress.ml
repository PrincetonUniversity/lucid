open CoreSyntax

(* egress pipeline processing *)
let has_egress (ds:decl list) =
  let check_decl d =
    match d.d with
    | DHandler(_, HEgress, _) -> true
    | _ -> false
  in
  List.exists check_decl ds
;;

(* find all the globals in the program *)
let find_globals ds =
  let global_ids = ref [] in
  let v =
    object
      inherit [_] s_iter as super
      method! visit_decl _ decl =
        match decl.d with
        | DGlobal(id, _, _) -> 
          print_endline (CorePrinting.decl_to_string (decl));
          global_ids := id::(!global_ids);
        | DActionConstr({aid=aid; _}) -> 
          global_ids := aid::(!global_ids);
        | _ -> ()
    end
  in
  v#visit_decls () ds;
  !global_ids
;;

(* get all the globals referenced by the handler *)
let find_globals_in_decl (globals : cid list) dhandler =
  let globals_in_dhandler = ref [] in
  let v =
    object
      inherit [_] s_iter as super
      method! visit_EVar _ (c:cid) =
        if List.mem c globals && not (List.mem c !globals_in_dhandler) then
          globals_in_dhandler := c :: !globals_in_dhandler
    end
  in
  v#visit_decl () dhandler;
  !globals_in_dhandler  
;;

(* find all the globals used in handlers of sort "handler_sort" *)
let find_globals_in_handler_sort handler_sort decls =
  let all_globals = find_globals decls |> List.map Cid.id in
  List.fold_left (fun handler_globals decl ->
    match decl.d with
    | DHandler (_, sort, _) when sort = handler_sort ->
        let handler_globals = find_globals_in_decl all_globals decl in
        List.concat [handler_globals; handler_globals]
    | _ -> handler_globals
  ) [] decls
;;

let egress_uses_disjoint_globals decls : bool =
  (* check to see whether there is overlap in the 
     globals used by handlers of sort HEgress and
     handlers of sort HData *)
  let egress_globals = find_globals_in_handler_sort HEgress decls in
  let data_globals = find_globals_in_handler_sort HData decls in
  List.for_all (fun g -> not (List.mem g data_globals)) egress_globals
;;

let split_decls decls : (decl list * decl list ) =
  (* 1. check to make sure that egress handlers use different globals 
        than all other handlers. If they do not, print an error message and then exit.
     2. split the list of declarations into 2 components: 
        1. an egress_decls list of all handlers of type HEgress 
        and all the global variable declarations they use, 
        and all shared decls.
        2. an ingress_decls list of all handlers of type HData
        and all the global variable declarations they use, 
        and all shared decls. *)
  (* no egress means everything is an ingress decl *)
  if (not (has_egress decls)) then (decls, []) else (
    if (not (egress_uses_disjoint_globals decls)) then begin
      print_endline "Error: Egress handlers use globals accessed by ingress handlers.";
      exit 1
    end;
    let egress_globals = find_globals_in_handler_sort HEgress decls in
    let ingress_globals = find_globals_in_handler_sort HData decls in
    let egr_decls, igr_decls = List.fold_left 
      (fun (egress_acc, ingress_acc) decl ->
        match decl.d with
        | DHandler(_, HEgress, _) -> (decl :: egress_acc, ingress_acc)
        | DHandler(_, HData, _) ->   (egress_acc, decl :: ingress_acc)
        | DGlobal(id, _, _) -> 
          if (List.mem (Cid.id id) egress_globals)
          then ((decl :: egress_acc, ingress_acc))
          else (
            if (List.mem (Cid.id id) ingress_globals)
            then ((egress_acc, decl :: ingress_acc))
            else ((decl :: egress_acc, decl :: ingress_acc)))
        | _ -> ((decl :: egress_acc, decl :: ingress_acc))
      ) 
      ([], []) 
      decls
    in
    List.rev igr_decls, List.rev egr_decls)
;;

(* Add a command to set the drop flag to the beginning of every egress handler. *)
let egr_drop_ctl_id = Cid.create ["drop_ctl"];;
let egr_drop_ctl_sz = 3
let rec add_default_egr_drop ds = 
  let set_drop_ctl = sassign egr_drop_ctl_id (vint_exp 1 egr_drop_ctl_sz) in
  let prepend_set_drop_ctl body =
    match body with
    | (params, stmt) -> (params, { stmt with s = SSeq(set_drop_ctl, stmt) })
  in
  let update_handler handler =
    match handler.d with
    | DHandler(id, HEgress, body) -> { handler with d = DHandler(id, HEgress, prepend_set_drop_ctl body) }
    | _ -> handler
  in
  List.map update_handler ds
;;

