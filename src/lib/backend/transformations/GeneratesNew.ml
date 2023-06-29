(* 
  The new generate elimination pass completely eliminates generate statements. 
  It transforms every generate statement into assign statements that 
  set fields in the output event of each component's main handler, 
  and also builtin parameters of each component's main handler.
  
  handle ingress : ingress_in -> ingress_out {
    case (ingress_in.tag == ingress_in.foo.num):
        generate(ingress_out.foo_out.bar(1, 2)); 
          --> 
        ingress_out.tag.tag = foo_out.num; (note: actually added with the branch in prior pass)
        ingress_out.foo_out.flags.bar_flag = 1;
        ingress_out.foo_out.bar.a = 1;
        ingress_out.foo_out.bar.b = 2;
        ingress.internal_params.gen_ct = ingress.internal_params.gen_ct + 1;    
        
        we also need to validate the specific headers that need to be 
        serialized. 
        and note that a union's tag is in tag.tag -- 
        the outer id is a header that can be enabled. 
        ingress
        enable_header(ingress_out.tag);
        ingress_out.tag.enable = 1;
        enable_header(ingress_out.foo_out.flags);
        enable_header(ingress_out.foo_out.bar);

  }
  if it was a generate_port or generate_ports, gen_ct would not 
  be incremented, but rather out_port or out_group would be set. 
  
  
  inline with generate elimination, we generate groups declarations 
  for constant groups and recirculation
    1. eliminate group type expressions: 
      1. create a multicast group for each constant group. 
      2. replace the group expression with the new mc group's number.
    2. construct groups for plain / self generates -- 1 group for each possible 
       value of mc_group_a. This is just one group per number of events, since 
       there can only be 1 copy of each handler generated for now. 

  TODO: flood elimination (floods are not supported currently)
  1. make a single flood group with all the defined port
  2. when we see a generate with a flood, set the multicast group to the "all" group 
     and set the exclusion id to flood's argument  *)

  
  open Batteries
  open Collections
  open CoreSyntax
  open TofinoCoreNew
  open AddIntrinsics
  open AddHandlerTypes
  
  (* get a member event *)
  let member_event event member_id =
    let rec member_event_rec events member_id =
      match events with
      | [] -> None
      | event::events -> 
        let evid = id_of_event event in
        if (Id.equal evid member_id)
          then Some(event)
          else member_event_rec events member_id        
    in 
    match event with
    | EventSingle _ -> error "[member_event] a base event has no member events"
    | EventUnion({members;})
    | EventSet({members;}) -> (
      match member_event_rec members member_id with
      | None -> error "[member_event] no member with that id"
      | Some(member_event) -> member_event)
  ;;  
  
  (* get the position of the event with id eventid in the list of events *)
  let pos_of_event events eventid = 
    let rec pos_of_event' events eventid pos = 
      match events with
      | [] -> None
      | event::events' -> 
        let evid = id_of_event event in
        if (Id.equal evid eventid)
          then Some(pos)
          else pos_of_event' events' eventid (pos+1)
    in
    match pos_of_event' events eventid 0 with
    | Some(pos) -> pos
    | None -> error "[pos_of_event] could not find event in members list"
  ;;

  (* generate a sassign to increment a variable *)
  let sincr_var evar = 
    match evar.e, evar.ety with 
    | EVar(var_cid), var_ty ->
      sassign
        var_cid
        (
          op
            Plus
            [
              evar;
              vint_exp_ty 1 var_ty
            ]
            var_ty)
    | _ -> error "[sincr] expected evar"
  ;;
  
  (* statement to assign a var to new value *)
  let sassign_var evar enew = 
    match evar.e with 
    | EVar(var_cid) ->
      sassign var_cid enew
    | _ -> error "[sassign_var] expected evar"
  ;;
    
  (* enable a field of an event  *)
  let enable_event_field ev_parents ev field = 
    let full_param_cid = (Cid.create_ids
      ((List.map id_of_event (ev_parents@[ev]))@[field]))
    in
    let full_param_ty = ty TBool in
    let scall = enable_call full_param_cid full_param_ty in
    full_param_cid, scall
  ;;


  (* get an evar referencing a field with name field_name from a 
     parameter of type intrinsic_paramty in the paramters hdl_ret_params *)
  let evar_from_paramty_field intrinsic_paramty field_name hdl_ret_params =
    (* find the handler param with type ingress_intrinsic_metadata_for_tm_t *)
    let param_ty = snd (intrinsic_to_param intrinsic_paramty) in
    let param_id = match (List.find_opt
      (fun (_, ty) -> equiv_ty ty param_ty)
      hdl_ret_params) with
    | Some(id, _) -> id 
    | None -> error "[evar_from_paramty_field] handler does not contain an output parameter of correct type"
    in
    (* get the field mcast_grp_a from it *)
    let field_id = Cid.create [field_name] in
    let cid, ty = AddIntrinsics.field_of_intrinsic
      ingress_intrinsic_metadata_for_tm_t
      (Cid.id param_id)
      (field_id)
    in
    var cid ty
  ;;
  (* helpers for specific fields *)
  let genct_evar = (evar_from_paramty_field ingress_intrinsic_metadata_for_tm_t "mcast_grp_a") ;;
  let egressport_evar = (evar_from_paramty_field ingress_intrinsic_metadata_for_tm_t "ucast_egress_port") ;;
  let group_evar = (evar_from_paramty_field ingress_intrinsic_metadata_for_tm_t "mcast_grp_b") ;;

  type ingress_ctx = {
    ctx_output_event : event;
    ctx_genct_evar : exp;
    ctx_egressport_evar : exp;
    ctx_group_evar : exp;
    ctx_groups : group list;
  }
  
  let ingress_ctx (igr:component) = 
    let main_hdl = (main_handler_of_component igr) in
    let ctx = {
      ctx_output_event = main_hdl.hdl_output;
      ctx_genct_evar = genct_evar main_hdl.hdl_retparams;
      ctx_egressport_evar = egressport_evar main_hdl.hdl_retparams;
      ctx_group_evar = group_evar main_hdl.hdl_retparams;
      ctx_groups = [];
    }
    in
    ctx
  ;;

  let egress_ctx (egr:component) = 
    (main_handler_of_component egr).hdl_output
  ;;

  (* given a constructor x.y.z, and a root event x,
     return a list of event defs [event x; event y; event z]  *)
  let event_path ctor_cid root_event : event list = 
    let rec idpath_to_eventpath event id_path = 
      match id_path with 
      | [] -> []
      | _::[] -> [event]
      | _::id'::id_path' -> 
        let event' = member_event event id' in
        event::(idpath_to_eventpath event' (id'::id_path'))            
    in
    idpath_to_eventpath root_event (Cid.to_ids ctor_cid)
  ;;

  (* create a mutlicast group for a constant group, 
     if an identical group does not already exist in context. 
      Return an expression with the group's number. *)
  let mc_group_decl ctx exp : (group list * exp) = 
    let mc_group_equiv gcopies mcg2 = 
      (* first, test if gcopies and mcg2.copies have the same length *)
      if (List.length gcopies) <> (List.length mcg2.gcopies) then false else
      (* then, test if gcopies and mcg2.copies have the same elements *)
      List.map2 (fun (a1, b1) (a2, b2) -> 
        a1 = b1 & a2 = b2)
      gcopies
      mcg2.gcopies
      |> List.exists (fun b -> b)
    in
    let find_mc gcopies = List.find_opt (mc_group_equiv gcopies) ctx.ctx_groups in
    match exp.e with 
    | EFlood _ -> error "flood groups are not supported"
    | EVal({v=VGroup(ports)}) -> (
      let gcopies = List.map (fun port -> (port, 0)) ports in 
      match (find_mc gcopies) with
      | None -> (
        let gnum = List.length (ctx.ctx_groups) in
        (* one copy to each port, all copies get rid = 0 
          because they are the "forwarded event" *)
        let gcopies = List.map (fun port -> (port, 0)) ports in 
        let exp' = vint_exp gnum 16 in
        ([{gnum; gcopies}], exp')
      )
      | Some(mcgroup) -> [], vint_exp mcgroup.gnum 16
      )
    | _ -> 
      print_endline (CorePrinting.exp_to_string exp);
      error "[mc_group_decl] not a valid group declaration"
  ;;

  (* function to transform generate statements in an ingress *)
  let ingress_transformer ctx stmt : group list * statement = 
    match stmt.s with 
    (* generate (ingress_out.foo.bar(1, 2, 3)); *)
    | SGen(gty, exp) -> (
      (* ingress_out.foo.bar, [1, 2, 3] *)
      let ctor_cid, args = match exp.e with
        | ECall(cid, args) -> cid, args
        | _ -> error "[GeneratesNew] event expression inside of a generate must be a constructor expression"
      in
      (* replace the generate statement with a sequence of assignments. *)
      (* at this point, the event id should have three components: 
          <ingress_event_id>.<handler_out_event_id>.<base_event_id> *)
      let main_event, handler_out_event, base_event = match event_path ctor_cid ctx.ctx_output_event with 
        | [m; h; b] -> m, h, b
        | _ -> error "[GeneratesNew.ingress_transformer] could not resolve event constructor in the given output event"
      in      
      (* 1. enable all the headers necessary to decode the base event. 
         the main_event's tag header is already enabled and set to handler_out_event.
         So we need to enable handler_out_event's flags header, set it, and enable 
         handler_out_event's header for base_event. *)
      let enable_hdrs_stmts = match handler_out_event with 
        | EventSet{members; flags;} -> 
          (* 1. enable the flags header of the handler event *)
          let flags_cid, enable_flags = enable_event_field [main_event] handler_out_event (Id.create"flag") in      
          (* let stmt = enable_builtin_params [main_event] handler_out_event "flags" 1 in *)
          (* 2. set the appropriate flag field for base_event in handler  *)
          let flag_id, flag_ty = List.nth flags (pos_of_event members (id_of_event base_event)) in
          let set_flag = sassign
            (Cid.concat (flags_cid) (Cid.id flag_id))
            (vint_exp_ty 1 flag_ty)
          in
          (* 3. enable the field holding the member event parameters. *)
          let _, enable_base_event = enable_event_field [main_event] handler_out_event (id_of_event base_event) in
          [enable_flags; set_flag; enable_base_event]
        | _ -> error "[GeneratesNew.ingress_transformer] at an ingress, handler output should be an eventset"
      in
      (* 2. then set the event parameters: ingress_out.foo_out.bar.a = ...; ...*)
      let enable_params_stmts = List.map2
        (fun (id, _) arg -> 
          let param_cid = Cid.create_ids ([id_of_event main_event; id_of_event handler_out_event; id_of_event base_event]@[id]) in
          sassign param_cid arg)
        (params_of_event base_event)
        args
      in
      (* 3. finally, update some of the handler's control parameters. *)
      (* 3. [case: generate default] 
              increment the handler's internal param: gen_ct *)
      (* 3. [case: generate_port] set the egress port: ingress_out.foo_out.bar.port = ...;*)
      (* 3. [case: generate_ports] set the group builtin: ingress_out.foo_out.bar.group = ...; *)
      let (groups_created:group list), update_control_param_stmt = match gty with
        | GSingle(None) -> 
          [], sincr_var ctx.ctx_genct_evar
        | GMulti(exp) -> 
          (* create a new group if it does not exist, and replace the 
             group expression with an int. update context.*)
          let (groups_created:group list), exp' = mc_group_decl ctx exp in 
          groups_created, sassign_var ctx.ctx_group_evar exp'
        | GPort(exp) -> 
            [], (sassign_var ctx.ctx_egressport_evar exp)
        | GSingle(Some(_)) -> error "[Generates.eliminate] not sure what to do with a generate of type GSingle(Some(_))"
      in 
      groups_created, List.fold_left sseq update_control_param_stmt (enable_hdrs_stmts@enable_params_stmts)
    )
    | _ -> [], stmt
  ;;
  (* function to transform generate statements in an egress *)
  let egress_transformer output_event stmt = 
    match stmt.s with 
    (* generate (ingress_out.foo.bar(1, 2, 3)); *)
    | SGen(_, exp) -> (
      (* ingress_out.foo.bar, [1, 2, 3] *)
      let ctor_cid, args = match exp.e with
        | ECall(cid, args) -> cid, args
        | _ -> error "[GeneratesNew] event expression inside of a generate must be a constructor expression"
      in    
      (* replace the generate statement with a sequence of assignments. *)
      (* at this point, the event id should have three components: 
          <ingress_event_id>.<handler_out_event_id>.<base_event_id> *)
      let main_event, handler_out_event, base_event = match event_path ctor_cid output_event with 
        | [m; h; b] -> m, h, b
        | _ -> error "[GeneratesNew.egress_transformer] could not resolve event constructor in the given output event"
      in      
      (* 1. enable all the headers necessary to decode the base event. 
         the main_event's tag header is already enabled and set to handler_out_event.
         So we need to enable handler_out_event's flags header, set it, and enable 
         handler_out_event's header for base_event. *)
      let enable_hdrs_stmts = match handler_out_event with 
        | EventUnion{tag} -> 
          (* 1. enable the tag header of the handler event *)
          let tag_cid, enable_tag = enable_event_field [main_event] handler_out_event (fst tag) in
          (* 2. set the tag to the appropriate event id 
              note that we are setting foo.bar.tag.tag = tagval -- tag is a record with a field named tag...*)
          let tagval = (vint_exp_ty (num_of_event base_event) (snd (snd tag))) in
          let set_tag = sassign (Cid.concat tag_cid (Cid.id (fst tag))) tagval in
          (* 3. enable the field holding the member event parameters. *)
          let _, enable_base_event = enable_event_field [main_event] handler_out_event (id_of_event base_event) in
          [enable_tag; set_tag; enable_base_event]
        | _ -> error "[GeneratesNew.ingress_transformer] at an ingress, handler output should be an eventset"
      in
      (* 2. then set the event parameters: ingress_out.foo_out.bar.a = ...; ...*)
      let enable_params_stmts = List.map2
        (fun (id, _) arg -> 
          let param_cid = Cid.create_ids ([id_of_event main_event; id_of_event handler_out_event; id_of_event base_event]@[id]) in
          sassign param_cid arg)
        (params_of_event base_event)
        args
      in
      let sseq_opt s1_opt s2 = match s1_opt with 
        | None -> Some(s2) 
        | Some(s1) -> Some(sseq s1 s2)
      in
      List.fold_left sseq_opt None (enable_hdrs_stmts@enable_params_stmts)
      |> Option.get
    )
    | _ -> stmt
  ;;

(* passes to run within ingress and egress components *)
  let eliminate_ingress ctx component =
    let groups_created = ref ctx.ctx_groups in 
    let v =  
    object
      inherit [_] s_map as super       
      method! visit_statement ctx stmt = 
        (* set already existing groups *)
        let ctx = {ctx with ctx_groups = !groups_created} in
        (* visit this statement *)
        let new_groups, stmt = ingress_transformer ctx stmt in
        (* add new groups to groups created *)
        groups_created := !groups_created@new_groups;
        (* visit next statement *)
        super#visit_statement ctx stmt 
      end
    in
    let component' = v#visit_component ctx component in
    !groups_created, component'
  ;;
  let eliminate_egress = 
    object
      inherit [_] s_map as super  
      method! visit_statement output_event stmt = 
        let stmt=super#visit_statement output_event stmt in 
        egress_transformer output_event stmt
      end
  ;;

let events_of_component component = 
  let v = 
    object
      inherit [_] s_iter as super
      val mutable events = []
      method events = events
      method! visit_event ctx event = 
        (* only add an event if it is a variant EventSingle *)
        match event with
        | EventSingle(_) -> (
        (* only add the event if an event with the same id is not in the list *)
        if not (List.exists (fun e -> (id_of_event e) = (id_of_event event)) events) 
        then events <- event::events;
        )
        | _ -> ();
        super#visit_event ctx event
    end
  in
  v#visit_component () component;
  v#events
;;

let construct_initial_groups recirc_port (max_num_events : int) = 
(* psuedocode 
  groups = []
  for num_copies in range(1, max_num_events+1):
    gnum = num_copies
    gcopies = [(recirc_port, j) for j in range(1, num_copies)]
    groups.append({gnum; gcopies})
  return groups
*)
  (* translate the psuedocode into ocaml *)
  let groups = ref [] in
  for num_copies = 1 to max_num_events do
    let gnum = num_copies in
    let gcopies = ref [] in
    for j = 1 to num_copies do
      gcopies := (recirc_port, j)::!gcopies
    done;
    groups := {gnum; gcopies=List.rev !gcopies}::!groups
  done;
  List.rev !groups
;;




    (* main pass *)
  let eliminate_generates recirc_port (prog : prog) : prog = 
    (* print_endline "[eliminate_generates] pass started"; *)
    let mcgroups, prog = List.fold_left
      (fun (mcgroups, prog) component -> 
        match component.comp_sort with
        | HData -> 
          let ctx = ingress_ctx component in
          let events = events_of_component component in
          let initial_groups = construct_initial_groups recirc_port (List.length events) in
          let ctx = {ctx with ctx_groups = initial_groups} in 
          let new_groups, component = eliminate_ingress (ctx) component in
          (mcgroups@new_groups), prog@[component]
        | HEgress -> 
          mcgroups, (prog@[eliminate_egress#visit_component (egress_ctx component) component])
        | _ -> 
          mcgroups, prog@[component]
      )
      ([], [])
      prog
    in
    (* add declarations for the multicast groups to the controller component -- 
       the component with comp_id = id "control" 
       use decl (TDMulticastGroup(group)) to declare a multicast group *)
    let control_component = List.find (fun c -> fst c.comp_id = "control") prog in
    let mcgroup_decls = List.map (fun g -> tdecl (TDMulticastGroup(g))) mcgroups in
    let control_component = {control_component with comp_decls = control_component.comp_decls@mcgroup_decls} in
    let prog = List.map (fun c -> if fst c.comp_id = "control" then control_component else c) prog in
    prog
  ;;

