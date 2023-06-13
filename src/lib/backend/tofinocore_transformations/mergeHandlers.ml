(* 
  merge the handlers of each component.
  The input event type becomes an event union of all the handler input events.
  The output event type becomes and event union of all the handler output events.
  The handler's body is merged by adding a match statement, with a case for 
  each input event's tag in the input event union.
*)

open Batteries
open Collections
open CoreSyntax
open TofinoCoreNew

(* get the ids of all event parameters with proper scoping *)
let rec localids_of_event_params event = 
  match event with 
  | EventSingle({evid; evparams;}) -> 
    let param_ids = List.split evparams |> fst in 
    let param_ids = List.map (fun id -> Cid.create_ids [evid; id]) param_ids in
    param_ids
  | EventUnion({evid; members; tag;}) -> 
    (* get the parameters of all the members, then prefix them with 
       this event's name.  *)
    let param_ids = List.map (localids_of_event_params) members in
    let param_ids = List.flatten param_ids in
    let param_ids = List.map (fun id -> Cid.compound evid id) param_ids in
    (* now add the tag var *)
    let tag_id = Cid.create_ids [evid; (fst tag)] in
    tag_id :: param_ids
  | EventSet({evid; members; flags;}) -> 
    let param_ids = List.map (localids_of_event_params) members in
    let param_ids = List.flatten param_ids in
    let param_ids = List.map (fun id -> Cid.compound evid id) param_ids in
    (* now add all the flags *)
    let flag_ids = List.map (fun (flag, _) -> Cid.create_ids [evid; flag]) flags in
    flag_ids @ param_ids
;;

(* get all the event parameters as though you are inside the event *)
let ids_of_event_params event =
  let param_cids = localids_of_event_params event in
  List.map 
    (fun param_cid -> match param_cid with
      | Cid.Compound(_, param_id) -> param_id
      | Cid.Id(_) -> failwith "ids_of_event_params: not a compound id")
    param_cids
;;



(* scope all the undeclared evars in program as parameters of the input event, 
   then make sure that they can be resolved by type checking them *)
(* let scope_params (hdl_body : statement) (input_event : event) =
;; *)

(* merge the handlers in the tdecls from one component *)
let merge_handlers_in_component (c:component) : component =
  (* remove all the handlers from the list of decls -- we are making new ones *)
  let rec extract_handlers tdecls handlers other = 
    match tdecls with
    | [] -> (handlers, other)
    | tdecl::tdecls' -> (
      match tdecl.td with
      | TDHandler(HEvent h) -> 
        extract_handlers tdecls' (h::handlers) other
      | TDHandler(_) -> failwith "handler is not HEvent"
      (* skip events -- we only want the events for the merged handler *)
      | TDEvent(_) -> extract_handlers tdecls' handlers other
      | _ -> extract_handlers tdecls' handlers (tdecl::other)
    )
  in
  let (handlers, non_handler_decls) = extract_handlers c.comp_decls [] [] in
  (* construct the input and output events *)
  let input_evid = Id.append_string "_input" c.comp_id in
  let output_evid = Id.append_string "_output" c.comp_id in
  let hdl_evid = Id.append_string "_hdl" c.comp_id in
  let selector_tag = Id.create "tag" in
  
  let input_event = EventUnion({
    evid = input_evid;
    members = List.map 
      (fun h -> 
        if h.hdl_sort = c.comp_sort 
          then h.hdl_input 
          else error "[merge_handlers_in_tdecls] handler sort mismatch]") 
      handlers;
    tag = (selector_tag, ty (TInt 16));
    }) 
  in
  let output_event = EventUnion({
    evid = output_evid;
    members = List.map
      (fun h -> 
        if h.hdl_sort = c.comp_sort 
          then h.hdl_output
          else error "[merge_handlers_in_tdecls] handler sort mismatch]")
      handlers; 
    tag = (selector_tag, ty (TInt 16));
    })
  in
  (* construct the new handler's body -- a match statement
     that branches on the input event's selector tag. *)
  let merged_hdl_stmt = smatch
    [var (Cid.id selector_tag) (ty (TInt 16))] 
    (
      List.mapi
      (fun i h -> 
        let tag_val = Z.of_int (i + 1) in
        let stmt = match h.hdl_body with
          | SFlat(stmt) -> stmt
          | _ -> error "[merge_handlers_in_tdecls] cannot merge handlers after pipelining"        
        in
        ([PNum (tag_val)], stmt))
      handlers      
    )
  in
  (* scope local uses of event parameters *)
  let rename_map = 
    List.fold_left2
      (fun rename_map unscoped_cid scoped_cid -> 
        CidMap.add unscoped_cid scoped_cid rename_map)
      (CidMap.empty)
      (ids_of_event_params input_event)
      (localids_of_event_params input_event)
  in
  let merged_hdl_stmt = AddHandlerTypes.rename#visit_statement
    rename_map  
    merged_hdl_stmt
  in
  (* scope local uses of inner events *)
  let merged_hdl_stmt = AddHandlerTypes.scope_event_constructors
    output_event
    merged_hdl_stmt
  in

  let merged_hdl = {
      hdl_id = hdl_evid;
      hdl_sort = c.comp_sort;
      hdl_input = input_event;
      hdl_output = output_event;
      hdl_body = SFlat(merged_hdl_stmt);
      hdl_intrinsics = [];
    }  
  in
  (* the new declarations are:
     merged input event;
     merged output event; 
     merged handler *)
  let tdecls = [
    tdecl (TDEvent(input_event));
    tdecl (TDEvent(output_event));
    tdecl (TDHandler(HEvent(merged_hdl)));
  ] in
  { c with
    comp_decls = non_handler_decls @ tdecls;
  }    
;;

(* now that the handlers are merged, we need to rescope parameters and events, 
   just like we did after transforming handlers into event functions *)

let merge_handlers prog : prog = 
  List.map merge_handlers_in_component prog
;;


