(* split the data plane into ingress and egress components 
and ensure that each event has a handler in each component. 
If a handler is missing, add a default handler that just does 
  generate(this); 
  at ingress, this will generate an event of the same type to
  the recirculation port, passing through egress after the queue. 
  at egress, this will generate an event of the same type 
  going to wherever it is destined. 

  Note: this may not be the semantics that we ultimately want. 
*)

open CoreSyntax
open Collections
type split_prog = {
  globals : decl IdMap.t;
  ingress_globals : IdSet.t;
  egress_globals : IdSet.t;
  ingress : decl list;
  egress : decl list;
}

let empty_split_prog = {
  globals = IdMap.empty;
  ingress_globals = IdSet.empty;
  egress_globals = IdSet.empty;
  ingress = [];
  egress = [];
}

(* get the set of globals referenced in the statement *)
let globals_refd (global_ids : IdSet.t) stmt = 
  let globals = ref IdSet.empty in
  let v = object
    inherit [_] CoreSyntax.s_iter
    method! visit_EVar _ (c:cid) =
      if IdSet.mem (Cid.to_id c) global_ids then
        globals := IdSet.add (Cid.to_id c) !globals
    end
  in
  v#visit_statement () stmt;
  !globals
;;



let rec split_decls split_prog decls : split_prog =
  match decls with
  | [] -> 
    (* make sure that no globals overlap, and copy all the 
      ingress and egress globals over to the ingress and egress decl lists *)
    let ensure_no_shared_globals split_prog = 
      let overlap = IdSet.inter split_prog.ingress_globals split_prog.egress_globals in
      if not (IdSet.is_empty overlap) then
        error (
          Printf.sprintf 
          "Global variables are used in both ingress and egress: %s" 
          (CorePrinting.comma_sep (CorePrinting.id_to_string) (IdSet.to_list overlap)))
    in
    ensure_no_shared_globals split_prog;
    let full_ingress = IdSet.fold (fun id decls -> decls @ [IdMap.find id split_prog.globals]) split_prog.ingress_globals split_prog.ingress in
    let full_egress = IdSet.fold (fun id decls -> decls @ [IdMap.find id split_prog.globals]) split_prog.egress_globals split_prog.egress in
    { split_prog with ingress = full_ingress; egress = full_egress; }
  | d :: ds ->
    match d.d with
    (* we don't know whether a global is in ingress or egress yet *)
    | DGlobal (id, _, _) ->
      let split_prog' = { split_prog with 
        globals = IdMap.add id d split_prog.globals; } 
      in
      split_decls split_prog' ds
    (* parsers are always ingress *)
    | DParser _ -> let split_prog = { split_prog with ingress = split_prog.ingress @ [d] } in
      split_decls split_prog ds
    (* handlers go to either ingress or egress, and take all the globals 
       that they reference with them. *)
    | DHandler(_, HData, (_, body)) -> 
      (* convert the globals map into a set of all globals *)
      let all_global_ids = IdMap.fold (fun id _ ids -> IdSet.add id ids) split_prog.globals IdSet.empty in
      let globals_refd = globals_refd all_global_ids body in
      (* add the referenced globals to the ingress set *)
      let ingress_globals = IdSet.union split_prog.ingress_globals globals_refd in
      let split_prog = {split_prog with ingress_globals; ingress = split_prog.ingress @ [d]} in
      split_decls split_prog ds
    | DHandler(_, HEgress, (_, body)) -> 
      (* convert the globals map into a set of all globals *)
      let all_global_ids = IdMap.fold (fun id _ ids -> IdSet.add id ids) split_prog.globals IdSet.empty in
      let globals_refd = globals_refd all_global_ids body in
      (* add the referenced globals to the egress set *)
      let egress_globals = IdSet.union split_prog.egress_globals globals_refd in

      let split_prog = { split_prog with egress_globals; egress = split_prog.egress @ [d] } in 
      split_decls split_prog ds
    (* everything else goes to both *)
    | _ -> split_decls {split_prog with ingress = split_prog.ingress @ [d]; egress = split_prog.egress @ [d]} ds
;;



(* add handlers that route the event from ingress -> egress and vice versa. 
   This is for events that do not have both an ingress and an egress handler declared. *)
let add_continue_handlers split_prog : split_prog =
  let continue_handler hdl_sort (evid : Id.t) evparams : decl =
    let eparams = List.map (fun (id, ty) -> CoreSyntax.exp_of_id id ty) evparams in
    let body = 
      gen_sp 
        (GSingle(None))
        (call_sp (Cid.id evid) eparams (ty TEvent) Span.default)
        Span.default
    in
    handler_sp evid evparams hdl_sort body Span.default
  in
  let eventmap = IdMap.empty in
  (* get all the events *)
  let eventmap =
    List.fold_left (fun eventmap decl -> 
      match decl.d with
      | DEvent(id, _, _, _) -> (
        (* if the event isnt already in the map *)
        match IdMap.find_opt id eventmap with
        | None -> IdMap.add id decl eventmap
        | Some _ -> eventmap)
      | _ -> eventmap)
      eventmap 
      (split_prog.ingress @ split_prog.egress)
  in
  let all_events = IdMap.fold (fun _ v l -> v :: l) eventmap [] in
  (* we need a unique list of events by event id*)
  let ingress_handler_ids = List.filter_map (fun d -> match d.d with DHandler(hid, HData, _) -> Some(hid) | _ -> None) split_prog.ingress in
  let egress_handler_ids = List.filter_map (fun d -> match d.d with DHandler(hid, HEgress, _) -> Some(hid) | _ -> None) split_prog.egress in
  (* For each event that does not have a handler in ingress or egress,
      construct a new handler that generates the event with its input parameters. *) 
  let rec add_handlers split_prog (events : decls) : split_prog = 
    match events with 
    | [] -> split_prog
    | d :: ds -> 
      match d.d with
      | DEvent(eid, _, _, params) -> 
        (* add ingress handler if it doesnt exist *)
        let split_prog = if (not (List.mem eid ingress_handler_ids)) then 
          {split_prog with ingress = split_prog.ingress@[continue_handler HData eid params];}
        else split_prog 
        in
        (* add egress handler if it doesn't exist *)
        let split_prog = if (not (List.mem eid egress_handler_ids)) then 
          {split_prog with egress = split_prog.egress@[continue_handler HEgress eid params];}
        else split_prog 
        in        
        add_handlers split_prog ds
      | _ -> add_handlers split_prog ds
  in   
  add_handlers split_prog all_events
;;

(* split the program, add default / continue handlers *)
let split decls = 
  split_decls empty_split_prog decls 
  |> add_continue_handlers
;;

   