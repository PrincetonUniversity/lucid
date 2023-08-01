(* 
  merge the handlers of each component.
  The input event type becomes an event union of all the handler input events.
  The output event type becomes and event union of all the handler output events.
  The handler's body is merged by adding a match statement, with a case for 
  each input event's tag in the input event union.

  This pass also renames event constructors in the ingress parser to contain 
  the merged ingress event name.
*)

open Batteries
open Collections
open CoreSyntax
open TofinoCoreNew
open AddIntrinsics

(* get the ids of all event parameters as they would appear 
    in the body of this event's handler *)
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
    (* now add the tag struct field (note: we don't add the struct's subfields
       is that okay? Do we even need to have the tag field here? ) *)
    let tag_id = Cid.create_ids [evid; (fst tag)] in
    tag_id :: param_ids
  | EventSet({evid; members; flags;}) -> 
    let param_ids = List.map (localids_of_event_params) members in
    let param_ids = List.flatten param_ids in
    let param_ids = List.map (fun id -> Cid.compound evid id) param_ids in
    (* now add all the flags *)
    let flag_struct_id, _, _ = flags in 
    let flag_id = Cid.create_ids [evid; (flag_struct_id )] in 
    (* let flag_ids = List.map (fun (flag, _) -> Cid.create_ids [evid; flag]) flags in *)
    flag_id::param_ids
    (* flag_ids @ param_ids *)
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


(* scope event constructors in a parser *)
let scope_pgen = 
  object 
    inherit [_] s_map as super
    method !visit_parser merged_hdl_event parser = 
      let parser = super#visit_parser merged_hdl_event parser in
      {parser with pret_event = Some(merged_hdl_event)}
    method !visit_PGen merged_hdl_event exp =
      let exp' = match exp.e with 
        | ECall(ev_cid, ev_params) -> 
          {exp with e = ECall (
            Cid.compound (id_of_event merged_hdl_event) ev_cid, 
            ev_params)}
        | _ -> exp
      in
      PGen(exp')
  end
;;

(* scope all the undeclared evars in program as parameters of the input event, 
   then make sure that they can be resolved by type checking them *)
(* let scope_params (hdl_body : statement) (input_event : event) =
;; *)


(* merge the events and handlers in the tdecls from one component and update 
   the parser so that generate statements use constructors of 
   the merged event. *)
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

  (* order handlers by the eventnum of their input events *)
  let num_of_handler hevent = num_of_event hevent.hdl_input in
  let sort_handlers (handlers : hevent list) = 
    List.sort (fun h1 h2 -> compare (num_of_handler h1) (num_of_handler h2)) handlers
  in
  let handlers = sort_handlers handlers in 
  (* let handlers = List.rev handlers in  *)
  (* construct the input and output events *)
  let input_evid = Id.append_string "_input" c.comp_id in
  let output_evid = Id.append_string "_output" c.comp_id in
  let hdl_evid = Id.append_string "_hdl" c.comp_id in
  (* input_event.tag.tag *)
  (* generate input and output events. Members are the 
     inputs and outputs of each event *)
  let input_members, output_members = 
    List.map (fun h -> h.hdl_input, h.hdl_output) handlers
    |> List.split
  in 
  let tag_of_evid evid = 
    Id.create ((fst evid)^"_tag"), (Id.create "tag", ty (TInt(16))  )
  in
  let input_event = EventUnion({
    evid = input_evid;
    (* the members of the merged input event are all the 
       input events of the non-merged handlers. *)
    members = input_members;
    (* tag is the FIELD NAME *)
    tag = tag_of_evid input_evid;
    member_nums = List.map num_of_event input_members;
    hdrs = []; 
    }) 
  in
  (* the merged event of the egress component gets headers 
     for serialization across the wire. *)
  let hdrs = match c.comp_sort with 
    | HEgress -> 
      let id = Id.create in
      let eth_hdr_v = vint_tups
        [1, 48; 2, 48; Builtins.lucid_ety_int, 16]
      in
      let const_lucid_eth_hdr = hdr
        (id "eth")
        (id "eth_h")
        (ty (TRecord([
          (id "dst",  (TInt(48)));
          (id "src",  (TInt(48)));
          (id "ety",  (TInt(16)));
        ])))
        (Some(eth_hdr_v))
      in
      [const_lucid_eth_hdr]
    | _ -> []
  in

  let output_event = EventUnion({
    evid = output_evid;
    members = output_members; 
    tag = tag_of_evid output_evid;
    member_nums = List.map num_of_event input_members;
    hdrs;
    })
  in
  (* construct the new handler's body -- a match statement
     that branches on the input event's selector tag.
     The first statement sets the output event's selector tag. 
     As an optimization, we might only want to do that if the 
     body of that branch has a generate. *)
  let input_tag_full_cid = Cid.create_ids [
    input_evid;
    fst (tag_of_evid input_evid);
    fst (snd (tag_of_evid input_evid))    
    ] 
  in
  let merged_hdl_stmt = smatch
    [var input_tag_full_cid (ty (TInt 16))] 
    (
      List.map
      (fun h -> 
        (* let tag_val = i + 1 in should not be using index of handler as the tag value! *)
        let hdl_tag_val = num_of_event h.hdl_input in 
        let tag_val = hdl_tag_val in 
        let handler_body = match h.hdl_body with
          | SFlat(stmt) -> stmt
          | _ -> error "[merge_handlers_in_tdecls] can't merge after pieplineing"
        in
        (* print_endline ("[mergeHandlers.merged_hdl_stmt] output event is "^(TofinoCorePrinting.event_to_string output_event)); *)
        if (is_union_of_unions output_event) 
        then (
            (* if the output is a union of unions (which happens for 
               packets from egress -> ingress), don't serialize the 
               outer tag. Could this be more explicit? *)
            (* print_endline ("[mergeHandlers] not serializing union of unions tag"); *)
            [PNum (Z.of_int tag_val)], handler_body
        )
        else 
          (* the first statement of this branch enables the tag in the 
            output header and then sets it to match the input event tag.*)
          (* in this else branch, the first 16 bits of packet indicate which handler fired *)
          let out_tag_outer, (out_tag_inner, out_tag_inner_ty) = match output_event with
            | EventUnion({tag}) -> tag
            | _ -> error "[mergeHandlers] output event is not a union"
          in
          let out_tag_cid = Cid.create_ids [output_evid; out_tag_outer] in
          let out_tag_ty_cid = Cid.create_ids [output_evid; (Id.create ((fst out_tag_outer)^"_t"))] in
          let out_tag_outer_ty  = ty (TName(out_tag_ty_cid, [], false)) in 
          let tag_enable_stmt = enable_call out_tag_cid out_tag_outer_ty in 
          (* let tag_enable_stmt = sassign tag_hdr_cid tag_hdr_val_exp in *)          
          let tag_cid = Cid.concat out_tag_cid (Cid.id out_tag_inner) in
          let tag_val_exp = vint_exp_ty tag_val out_tag_inner_ty in
          let tag_set_stmt = 
            sassign tag_cid tag_val_exp 
          in
          let tag_stmt = sseq tag_enable_stmt tag_set_stmt in  
          let stmt = sseq tag_stmt handler_body in
          ([PNum (Z.of_int tag_val)], stmt))
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

  (* finally, set the input and output parameters of the handler that carry metadata. *)
  (* NOTE: make sure these intrinsics are added to the tofinocore program 
      in AddIntrinsics.addIntrinsics!  *)
  let hdl_params, hdl_retparams = match c.comp_sort with 
  | HData -> (* ingress*)
    (List.map intrinsic_to_param [
      ingress_intrinsic_metadata_t; 
      ingress_intrinsic_metadata_from_parser_t;]),
    (List.map intrinsic_to_param [
      ingress_intrinsic_metadata_for_deparser_t; 
      ingress_intrinsic_metadata_for_tm_t;])
  | HEgress -> 
    (List.map intrinsic_to_param [
      egress_intrinsic_metadata_t; 
      egress_intrinsic_metadata_from_parser_t]),
    (List.map intrinsic_to_param [
      egress_intrinsic_metadata_for_deparser_t; 
      egress_intrinsic_metadata_for_output_port_t])
  | _ -> [], []
  in
  let merged_hdl = {
      hdl_id = hdl_evid;
      hdl_sort = c.comp_sort;
      hdl_input = input_event;
      hdl_output = output_event;
      hdl_body = SFlat(merged_hdl_stmt);
      hdl_preallocated_vars = [];
      hdl_params;
      hdl_retparams;
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
  let tdecls' = non_handler_decls @ tdecls in
  (* finally, scope event constructors in the parser. At this point, 
     there should only be 1 parser: the ingress parser.*)
  let tdecls' = scope_pgen#visit_tdecls input_event tdecls' in
  { c with
    comp_decls = tdecls';
  }    
;;

(* now that the handlers are merged, we need to rescope parameters and events, 
   just like we did after transforming handlers into event functions *)

let merge_handlers prog : prog = 
  List.map (skip_control merge_handlers_in_component) prog 

