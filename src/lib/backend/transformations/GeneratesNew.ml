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
  be incremented, but rather out_port or out_group would be set. *)
open Batteries
open Collections
open CoreSyntax
open TofinoCoreNew
open AddHandlerTypes


(* given an event constructor cid, get the cids of the event's parameters as 
   locally scoped variables *)
let params_of_single_event event =
  match event with 
  | EventSingle({evparams;}) -> evparams
  | _ -> error "[params_of_event] must provide a single evetn"
;;

let rec assigns cids args : statement = 
  List.fold_left2
    (fun stmt cid arg  -> 
      match stmt.s with
      | SNoop -> sassign cid arg
      | _ -> sseq stmt (sassign cid arg))
    snoop
    cids
    args 
;;

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

let strip_outer_id cid = 
  match cid with
  | Cid.Id(id) -> Cid.Id(id)
  | Cid.Compound(_, cid') -> cid'
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

let num_of_event events eventid = 
  (* pos_of_event + 1. TODO: use evnum *)
  1 + pos_of_event events eventid
;;

let full_event_cid (prefix : id list) event = 
  Cid.create_ids (prefix@[id_of_event event])
;;

let enable_member_event (outer_event_prefix: id list) (outer_event : event) (inner_event_id : id) =
  let outer_event_cid = full_event_cid outer_event_prefix outer_event in
  match outer_event with
  | EventSet{members; flags;} -> (
    (* get the index of the member *)
    let member_pos = pos_of_event members inner_event_id in
    (* enable the "flags" header field. *)
    let full_hdr_cid = Cid.concat outer_event_cid (Cid.create ["flags"]) in
    let ecall_enable = call (Cid.create ["enable"]) [] (ty (TInt(1))) in
    let enable_hdr = sassign (full_hdr_cid) ecall_enable in
    (* set the appropriate flag field *)
    let (flag_id, flag_ty) = List.nth flags member_pos in
    let full_flag_id = Cid.concat full_hdr_cid (Cid.id flag_id) in
    let set_flag = (sassign 
      full_flag_id
      (vint_exp_ty 1 flag_ty)) 
    in
    (* finally, enable the field for the member event *)
    let member_hdr_cid = Cid.concat outer_event_cid (Cid.id inner_event_id) in
    let enable_member_hdr = sassign member_hdr_cid ecall_enable in
    sseq (sseq enable_hdr set_flag) enable_member_hdr
  )
  | EventUnion{members; tag} -> 
    let tag_id, tag_ty = tag in
    (* enable the "tag" field *)
    let full_hdr_cid = Cid.concat outer_event_cid (Cid.create ["tag"]) in
    let ecall_enable = call (Cid.create ["enable"]) [] (ty (TInt(1))) in
    let enable_hdr = sassign (full_hdr_cid) ecall_enable in
    let full_tag_id = Cid.concat full_hdr_cid (Cid.id tag_id) in 
    let enewtagval = (vint_exp_ty (num_of_event members inner_event_id) (tag_ty)) in
    let set_tag = sassign
      full_tag_id
      enewtagval
    in
    (* finally, enable the field for the member event *)
    let member_hdr_cid = Cid.concat outer_event_cid (Cid.id inner_event_id) in
    let enable_member_hdr = sassign member_hdr_cid ecall_enable in
    sseq (sseq enable_hdr set_tag) enable_member_hdr
  | EventSingle _ -> error "[enable_member_event] single events do not have members"
;;

(* enable a member of an event, recursively. 
  so enable_member_rec [] (union outer (set foo (event inner))) foo.inner 
  will enable foo in outer and inner in foo
  note: member_cid should have the event's id stripped from its identifier. *)
let rec enable_member_rec prefix event member_cid = 
  match member_cid with
  | Cid.Id(member_id) -> 
    let res = enable_member_event prefix event member_id in
    res
  | Cid.Compound(member_id, member_cid') ->
    let enable_outer = enable_member_event prefix event member_id in
    let prefix' = prefix@[id_of_event event] in
    let event' = member_event event member_id in
    let enable_inner = enable_member_rec prefix' event' member_cid' in
    (* print_endline ("[enable_member_rec] inner statement:"); *)
    sseq enable_outer enable_inner 
;;

(* generate a sassign to increment a variable *)
let sincr (var_id,var_ty) = 
  let incr_exp = 
    op 
      Plus
      [
        var (Cid.id var_id) var_ty;
        vint_exp_ty 1 var_ty
      ]
      var_ty
  in
  sassign (Cid.id var_id) incr_exp
;;

let eliminate = 
  object
    inherit [_] s_map as super

    method! visit_component _ component =
      (* when visiting a component, save the output event and internal params to context *)
      let main_hdl = (main_handler_of_component component) in
      let output_event_and_internal_params = Some(main_hdl.hdl_output, main_hdl.hdl_internal_params) in
      super#visit_component output_event_and_internal_params component

    method! visit_statement output_event_and_internal_params_opt stmt = 
      match stmt.s with 
      | SGen(gty, exp) -> 
        (* print_endline ("---- [eliminate] generate statement: ----");  *)
        (* print_endline (CorePrinting.statement_to_string stmt); *)
        let ctor_cid, args = match exp.e with
          | ECall(cid, args) -> cid, args
          | _ -> error "[GeneratesNew.eliminate] event expression inside of a generate must be a constructor expression"
        in
        let output_event, internal_params = match output_event_and_internal_params_opt with
          | Some(output_event, internal_params) -> output_event, internal_params
          | None -> error "[eliminate_generates] no context. visit_component must not have been called."
        in
        let event = match (base_event output_event ctor_cid) with 
          | None -> error "[eliminate_generates] could not find event constructor in output event" 
          | Some(event) -> event
        in
        (* replace the generate statement with a sequence of assignments: *)
        (* example: generate(ingress_out.foo_out.bar); *)
        (* 1. enable the event *)
        let member_cid_without_outer = strip_outer_id ctor_cid in
        (* let senable = enable_member_rec [] output_event member_cid_without_outer in *)
        (* actually, the outermost member has already been enabled (e.g., ingress_out.foo_out)
           when events / handlers were merged. So we want to start 1 level deeper. *)
        let prefix = [id_of_event output_event] in
        let event_id, member_cid = match member_cid_without_outer with
          | Cid.Id(_) -> error "cid for event constructor used in generate has fewer ids than expected"
          | Cid.Compound(id, cid) -> id, cid
        in
        let inner_event = member_event output_event event_id in
        let senable = enable_member_rec prefix inner_event member_cid in
        (* 2. set the event parameters: ingress_out.foo_out.bar.a = ...; ...*)
        (* note we go back to the _outer_ event! *)
        let params = List.map
          (fun (id, ty) -> 
            Cid.concat ctor_cid (Cid.id id), ty
            )
          (params_of_single_event event)
        in
        let sparams = assigns (List.split params |> fst) args in
        (* 3. [case: generate default] 
                increment the handler's internal param: gen_ct *)
        (* 3. [case: generate_port] set the egress port: ingress_out.foo_out.bar.port = ...;*)
        (* 3. [case: generate_ports] set the group builtin: ingress_out.foo_out.bar.group = ...; *)
        let sloc_ctl = match gty with
          | GSingle(None) -> 
            sincr internal_params.gen_ct
          | GMulti(exp) -> 
            sassign (Cid.id (fst internal_params.out_group)) exp
          | GPort(exp) -> 
            (* when we generate a port event, we want to set the replica id
               to the index of the event in the output eventset... *)
            (* okay... so wait... at a generate, how do we know 
               all the possible indices? Its because an index corresponds to 
               an event type. So... find the index for this event type 
               in this set and assign that value to RID. *)
            sassign (Cid.id (fst internal_params.out_port)) exp 
          | GSingle(Some(_)) -> error "[Generates.eliminate] not sure what to do with a generate of type GSingle(Some(_))"
        in 
        sseq
          (sseq senable sparams)
          (sloc_ctl)
      | _ -> super#visit_statement output_event_and_internal_params_opt stmt
  end
;;



let eliminate_generates (prog : prog) : prog = 
  (* print_endline "[eliminate_generates] pass started"; *)
  let prog = eliminate#visit_prog None prog in
  (* print_endline "[eliminate_generates] pass finished"; *)
  prog