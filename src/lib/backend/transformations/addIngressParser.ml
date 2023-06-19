(*
   Add a call to the ingress parser for background events, 
   if it is not already present. There are two special cases: 
      1. a program with no packet events declared and no parser.
         - this program gets an ingress parser with just a 
           call to the background parser. 
      2. a program with packet events declared, but no parser. 
         - this program gets an ingress parser that matches 
           on port number to determine which event to generate. 
           The port number to event mapping is defined in an 
           external config file. If that configuration is not 
           present, and a program with packet events has no 
           parser, it is an error.
      3. a program with packet events declared and a parser. 
         - For this program, we just verify that the 
           lucid background parser is called somewhere in
           the parser. 

*)
open ParsePortSpec
open CoreSyntax
open MiscUtils


let is_pktev decl = match decl.d with 
   | DEvent(_, _, EPacket, _) -> true
   | _ -> false
;;
let is_bgev decl = match decl.d with
   | DEvent(_, _, EBackground, _) -> true
   | _ -> false
;;
let is_parser decl = match decl.d with
   | DParser(_) -> true 
   | _ -> false
;;

let name_of_event eventdecl = match eventdecl.d with
   | DEvent(id, _, _, _) -> fst id
   | _ -> error "[name of event] not an event decl"
;;


(* create a parse block to parse and generate a single packet event. *)
let packetevent_parse_block event = match event.d with
| DEvent(id,_, _, params) -> 
   let read_cmds = List.map read_id params in
   let gen_cmd = pgen 
      (call
         (Cid.id id)
         (List.map (fun (id, ty) -> var (Cid.id id) ty) params)
         (ty TEvent))
   in
   block read_cmds gen_cmd
| _ -> 
   error "[packetevent_parse_block] not an event declaration"
;;


(* call the parser for background events. *)
let call_lucid_block bg_events = 
   let (branches : parser_branch list) = List.map 
      (fun bg_ev -> match bg_ev.d with 
         | DEvent(_, Some(num), _,_) -> 
            pbranch [num] (packetevent_parse_block bg_ev)
         | DEvent(_, None, _, _) -> error "event has no number"
         | _ -> error "not an event?")
      bg_events
   in
   let tag_id = Cid.create ["tag"] in
   let tag_ty = ty (TInt(16)) in   
   let etag = var tag_id tag_ty in
   block 
      [
         read (Cid.create ["tag"]) (ty (TInt(16))) (* this tag has to be read by the merged ingress? Not really.*)
      ]
      (pmatch [etag] branches)
      
;;


(* create a full parser from a portspec file, 
   if there are packet events but no parser. *)
let portspec_to_parser portspec pkt_events bg_events = 
   (* 1. generate a parse block from each event. *)
   (* 2. generate branches from the portspec *)
   let synthesized_parser actions (step:parser_step) = decl (parser (id "main") [] (block actions step)) in
   let portspec_to_pbranches portspec events =
      (* 
         match port with 
         | recirc_dpid -> {lucid_parse();}
         | internal_ports -> {lucid_parse();}
         | port events -> (one case for each)      
         | external_ports -> {one case that each matches to default event}
      *)
      let bound_ports = ref [portspec.recirc_dpid] in 
      let bind_port dpid = 
         if (List.exists (fun d -> d = dpid) (!bound_ports))
            then (error "[portspec_to_pbranches] the given portspec tries to bind a single port to multiple events")
            else (bound_ports := dpid::(!bound_ports))
      in
      let background_branches =        
         (pbranch [portspec.recirc_dpid] (call_lucid_block bg_events))
         ::(List.map (fun port ->          
            bind_port port.dpid;
            pbranch [port.dpid] (call_lucid_block bg_events)) portspec.internal_ports)
      in
      let external_packet_branches = match events with 
         (* one event -- we can use external ports *)
         | [event] -> (
            List.map 
               (fun port -> 
                  bind_port port.dpid;
                  pbranch [port.dpid] (packetevent_parse_block event))
               portspec.external_ports)
         (* multiple events -- we _can't_ use external ports *)
         | _ -> []
      in
      (* finally, make branches for the port_event map *)
      let name_to_event = List.map (fun event -> (name_of_event event, event)) events in
      let port_packet_branches = List.map
         (fun (dpid, evname) -> 
            bind_port dpid;
            pbranch [dpid] (packetevent_parse_block (List.assoc evname name_to_event)))
         portspec.port_events
      in
      background_branches@external_packet_branches@port_packet_branches
   in
   let branches = portspec_to_pbranches portspec pkt_events in
   let eingress_port = (var (Cid.id Builtins.ingr_port_id) (Builtins.tofino_builtin_tys.ingr_port_ty |> SyntaxToCore.translate_ty)) in 
   let (step : parser_step) = pmatch [eingress_port] branches in
   synthesized_parser [] step
;;

let pcalls_to_lucid_parser parser = 
   let call_ct = ref 0 in
   let v = object
      inherit [_] s_iter as super
      method! visit_PCall () exp = 
         match exp.e with 
         | EVar(cid) -> (
            if (Id.equal (Cid.to_id cid) (Builtins.lucid_parse_id))
            then (call_ct := !call_ct + 1);)
         | ECall(cid, _) -> (
            if (Id.equal (Cid.to_id cid) (Builtins.lucid_parse_id))
               then (call_ct := !call_ct + 1);)   
         | _ -> ()
      end
   in
   v#visit_decl () parser;
   !call_ct
;;

let set_event_nums decls =
   let event_nums = List.filter_map 
     (fun decl -> match decl.d with
       | DEvent(_, nopt, _, _) -> nopt
       | _ -> None)
     decls
   in
   let rec set_event_nums' num decls = 
     if (List.exists (fun v -> v = num) event_nums)
     then set_event_nums' (num+1) decls
     else 
       match decls with
       | [] -> []
       | decl::decls -> (
         match decl.d with
         | DEvent(a, None, b, c) -> 
           {decl with d = DEvent(a, Some(num), b, c)}::(set_event_nums' (num+1) decls)
         | _ -> set_event_nums' num decls
       )
   in
   set_event_nums' 1 decls
 ;;

let add_parser (portspec:port_config) ds =
   let pkt_events = List.filter is_pktev ds in
   let bg_events = List.filter is_bgev ds in 
   let parsers = List.filter is_parser ds in
   match (pkt_events, parsers) with
   | ([], []) -> 
      (* case 1: no packet events and no parsers -- so make a parser for the bg events. *)
      (decl (parser (id "main") [] (call_lucid_block bg_events)))::ds
   | (pkt_events, []) -> 
      (* case 2: packet events declared, but no parser -- 
         so make a parser that branches on ingress port 
         according to the portspec provided. *)
      (portspec_to_parser portspec pkt_events bg_events)::ds
   | (pkt_events, [parser]) -> 
      (* case 3: packet events and a single parser -- 
         just make sure the parser eventually calls the builtin for background events. *)
      if (pcalls_to_lucid_parser parser <> 1)
         then (error "[addIngressParser] this program has an invalid ingress parser for the tofino, because it either does not call the lucid internal parser, or calls it more than once.")
         else ds 
   | (_, _) -> error "user-defined parser should be inlined by now"
;;