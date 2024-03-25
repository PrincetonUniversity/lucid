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
open Collections
open ParsePortSpec
open CoreSyntax
open MiscUtils

(* create a parse block to parse and generate a single packet event. *)
let packetevent_parse_block (pkt_var : exp) event = match event.d with
| DEvent(id,_, _, params) -> 
   let read_cmds = List.filter_map 
      (fun (id, ty) -> 
         (* don't generate a read for a payload  *)
         if (CoreSyntax.equiv_ty ty (Payloads.payload_ty |> SyntaxToCore.translate_ty)) then None
         else
         Some(PRead(Cid.id id, ty, pkt_var)))
      params
   in
   let gen_cmd = pgen 
      (call
         (Cid.id id)
         (List.map (fun (id, ty) -> var (Cid.id id) ty) params)
         (tevent))
   in
   block read_cmds gen_cmd
| _ -> 
   error "[packetevent_parse_block] not an event declaration"
;;


(* call the parser for background events. *)
let lucid_background_event_parser pkt_var bg_events = 
   let (branches : parser_branch list) = List.map 
      (fun bg_ev -> match bg_ev.d with 
         | DEvent(_, Some(num), _,_) -> 
            pbranch [num] (packetevent_parse_block pkt_var bg_ev)
         | DEvent(_, None, _, _) -> error "event has no number"
         | _ -> error "not an event?")
      bg_events
   in
   let tag_id = Cid.create ["tag"] in
   let tag_ty = ty (TInt(Sz 16)) in   
   let etag = var tag_id tag_ty in
   block 
      [ (* skip the lucid ethernet header. If we want to be safer, we can read it and check correctness. *)
         skip (ty (TInt(Sz (14*8))));
         PRead(Cid.create ["tag"], tag_ty, pkt_var) (* read the event tag *)
      ]
      (pmatch [etag] branches)
      
;;
(* a parser that starts after the ethernet header (it doesn't skip ethernet header) *)
let lucid_background_event_parser_from_eth pkt_var bg_events = 
   let (branches : parser_branch list) = List.map 
      (fun bg_ev -> match bg_ev.d with 
         | DEvent(_, Some(num), _,_) -> 
            pbranch [num] (packetevent_parse_block pkt_var bg_ev)
         | DEvent(_, None, _, _) -> error "event has no number"
         | _ -> error "not an event?")
      bg_events
   in
   let tag_id = Cid.create ["tag"] in
   let tag_ty = ty (TInt(Sz 16)) in   
   let etag = var tag_id tag_ty in
   block 
      [
         PRead((Cid.create ["tag"]),tag_ty,pkt_var)
      ]
      (pmatch [etag] branches)      
;;

(* inline all the calls in a single parser *)

type inline_ctx = (params * parser_block) CidMap.t

let subst = object (_)
   inherit [_] s_map as super
   method! visit_EVar param_map cid = 
      (* check if cid is in param_map *)
      match (CidMap.find_opt cid param_map) with
      | None -> super#visit_EVar param_map cid
      | Some(new_exp) -> new_exp
   end
;;

let inline_parser = object (_)
      inherit [_] s_map as super
      method! visit_parser_block (ctx : inline_ctx) parser_block = 
         let parser_block = super#visit_parser_block ctx parser_block in
         match parser_block.pstep with
         | (PCall(exp), _) -> (
            match exp.e with 
            | ECall(called_cid, eargs, _) -> (
               (* first replace params with args *)
               let params, called_body_block = CidMap.find called_cid ctx in
               let (subst_map: e CidMap.t) = List.fold_left2
                  (fun cidmap (param, _) earg -> CidMap.add (Cid.id param) earg.e cidmap)
                  CidMap.empty
                  params eargs
               in 
               let called_body_block' = subst#visit_parser_block subst_map called_body_block in
               (* next, inline called block's actions and step *)
               let pactions = parser_block.pactions@called_body_block'.pactions in 
               let pstep = called_body_block'.pstep in
               {pactions; pstep;}
            )
            | _ -> error "[inline_parser] PCall(!ECall)?"
         )
         | _ -> 
         parser_block
   end      
;;

let rec inline_parsers_rec (ctx:inline_ctx) (decls:decls) = 
   match decls with 
   | [] -> []
   | decl::decls -> (
      match decl.d with 
      | DParser(id, params, parser_block) -> 
         (* inline calls in the body *)
         let parser_block' = inline_parser#visit_parser_block ctx parser_block in
         (* update the context *)
         let ctx' = CidMap.add (Cid.id id) (params, parser_block') ctx in
         (* drop all parsers except main *)
         if (fst id = "main") then 
            let decl' = {decl with d=DParser(id, params, parser_block')} in
            decl'::(inline_parsers_rec ctx' decls)
         else
            inline_parsers_rec ctx' decls
      | _ -> decl::(inline_parsers_rec ctx decls)
   )
;;

type lucid_entry_block_ty = 
   | CallFromPortNum
   | CallFromEth
   | CallAlways
   | CallInvalid

let inline_parsers parser_entry_ty pkt_var bg_events decls = 
   let lucid_bg_event_block = match parser_entry_ty with
      | CallFromPortNum -> lucid_background_event_parser pkt_var bg_events
      | CallFromEth -> lucid_background_event_parser_from_eth pkt_var bg_events
      | CallAlways -> lucid_background_event_parser pkt_var bg_events
      | CallInvalid -> error "[inline_parsers] invalid parser entry type -- this should have been caught earlier"
   in
   let ctx = CidMap.add (Cid.id Builtins.lucid_parse_id) ([Id.create "pkt", pkt_arg_ty], lucid_bg_event_block) CidMap.empty in
   inline_parsers_rec ctx decls
;;


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




(* create a full parser from a portspec file, 
   if there are packet events but no parser. *)
let portspec_to_parser port_ty portspec pkt_var pkt_events bg_events = 
   (* 1. generate a parse block from each event. *)
   (* 2. generate branches from the portspec *)
   let synthesized_parser actions (step:parser_step) = decl (parser (id "main") [id"pkt", pkt_arg_ty] (block actions step)) in
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
      let background_branches = if (List.length bg_events > 0)
         then (     
         (pbranch [portspec.recirc_dpid] (lucid_background_event_parser pkt_var bg_events))
         ::(List.map (fun port ->          
            bind_port port.dpid;
            pbranch [port.dpid] (lucid_background_event_parser pkt_var bg_events)) portspec.internal_ports))
         else ([])
      in
      let external_packet_branches = match events with 
         (* one event -- we can use external ports *)
         | [event] -> (
            List.map 
               (fun port -> 
                  bind_port port.dpid;
                  pbranch [port.dpid] (packetevent_parse_block pkt_var event))
               portspec.external_ports)
         (* multiple events -- we _can't_ use external ports *)
         | _ -> []
      in
      (* finally, make branches for the port_event map *)
      let name_to_event = List.map (fun event -> (name_of_event event, event)) events in
      let port_packet_branches = List.map
         (fun (dpid, evname) -> 
            bind_port dpid;
            pbranch [dpid] (packetevent_parse_block pkt_var (List.assoc evname name_to_event)))
         portspec.port_events
      in
      background_branches@external_packet_branches@port_packet_branches
   in
   let branches = portspec_to_pbranches portspec pkt_events in
   let eingress_port = (var (Cid.id Builtins.ingr_port_id) port_ty) in 
   let (step : parser_step) = pmatch [eingress_port] branches in
   synthesized_parser [] step
;;



(* this is a block that has no actions and a step that just calls the 
   lucid parser *)
let direct_call_parser_block parser_block =
   let {pactions=acns_spans; pstep=(step, _);} = parser_block in
   let acns, _ = List.split acns_spans in
   match acns, step with 
   | [], PCall({e=ECall(cid, _, _); _}) when (Id.equal Builtins.lucid_parse_id (Cid.to_id cid)) -> true
   | _ -> false 
;;
(* a direct call parser branch is one that matches a single variable and 
   does nothing but call the lucid parser. *)
let direct_call_parser_branch (parser_branch:parser_branch) = match parser_branch with
   | (_::[], parser_block) when direct_call_parser_block parser_block -> true
   | _ -> false
;;

let never_calls_lucid_parser branch =
   let call_found = ref false in
   let v = object (_)
      inherit [_] s_iter as super
      method! visit_PCall _ exp = 
         match exp.e with 
            | ECall(cid, _, _)
               when (Id.equal Builtins.lucid_parse_id (Cid.to_id cid)) -> 
                  call_found := true
            | _ -> ()
   end
   in
   v#visit_parser_branch () branch;
   not (!call_found)
;;
let cid_is_ingress_port cid = 
   Id.equal Builtins.ingr_port_id (Cid.to_id cid)
;;

let bytes_read_by_action action =
   match action with 
   | PRead(_, ty, _) -> size_of_rawty ty.raw_ty
   | PSkip(ty) -> size_of_rawty ty.raw_ty
   | _ -> 0   
;;
let bits_read_by_actions actions = 
   List.fold_left (fun acc action -> acc + (bytes_read_by_action action)) 0 actions
;;

let size_of_exp exp = size_of_rawty exp.ety.raw_ty ;;

let last_ele ls = 
   List.rev ls |> List.hd
;;


(* foo.bar.baz *)
let rec is_last_subfield_of_evar var_cid exp = 
   match exp.e with 
      (* the var itself satisfies *)
   | EVar(cid) ->  Cid.equal var_cid cid
   | EProj(rec_exp, field_id) -> (
      (* rec_exp has to be a last field or evar itself, and then 
         field_id has to point to the last field of rec_exp *)
      if (is_last_subfield_of_evar var_cid rec_exp) then (
         let field_ids = match rec_exp.ety.raw_ty with 
            | TRecord(id_tys) -> List.split id_tys |> fst
            | _ -> error "project on a non-record-type"
         in 
         (Id.equal field_id (List.nth field_ids ((List.length field_ids) -1)))
      ) 
      else (
         false
      )
   )
   | _ -> false
   ;;

let exp_refs_last_read actions exp =
    (* actions in the block, expression being matched on *)
   let read_cids = List.filter_map (fun action -> 
      match action with 
      | PRead(cid, _, _) -> Some(cid)
      | _ -> None) actions
   in
   match read_cids with
   | [] -> false 
   | _ -> 
      let last_read_cid = last_ele read_cids in
      is_last_subfield_of_evar last_read_cid exp
      (* match exp.e with 
      | EVar(cid') -> Cid.equal last_read_cid cid'
      | EProj(inner_exp, field_id) -> 

         failwith "todo"
      | _ -> false) *)
;;


let is_lucid_etherty_pat pat = match pat with 
   | PNum(z) -> Z.equal z (Z.of_int Builtins.lucid_ety_int)
   | _ -> false

let at_least_one lst f =
   let matches = List.filter f lst in
   List.length matches >0
;;

(* check to see if the entry block of the user-defined 
   parser has a valid form.  *)
let check_valid_entry_parse_block parse_block : lucid_entry_block_ty =
   let acns_spans, (step, _) = parse_block.pactions, parse_block.pstep in
   let acns, _ = List.split acns_spans in 
   match acns, step with
   (* parser main() { do_lucid_parsing(); } *)
   | [], PCall({e=ECall(cid, _, _); _}) when cid_is_ingress_port cid -> 
      CallAlways
   (* parser main() { match ingress_port with <branches>, where branches 
      either directly call the lucid parser or never call the lucid parser } *)
   | [], PMatch([{e=EVar(cid); _}], branches) 
      when (cid_is_ingress_port cid)
      && (at_least_one branches direct_call_parser_branch)
      && (List.for_all
            (fun branch -> 
               direct_call_parser_branch branch || never_calls_lucid_parser branch)
            branches) -> 
               CallFromPortNum
   (* parser main() {read 14 bytes (eth hdr); match v : int16 with | LUCID_ETHERTY -> do_lucid_parsing(); | <never call lucid parse branches>} *)
   | actions, PMatch([exp], ([fst_pat], fst_block)::rest_branches) -> (
      let bits_read_check = bits_read_by_actions actions = 14 * 8 in 
      let size_check = size_of_exp exp = 16 in
      let exp_ref_check = exp_refs_last_read actions exp in
      let lucid_etherty_pat_check = is_lucid_etherty_pat fst_pat in
      let direct_call_check = direct_call_parser_block fst_block in
      let rest_branches_check = List.for_all never_calls_lucid_parser rest_branches in
      if not bits_read_check then Printf.printf "main parser reads wrong number of bytes before branch to lucid internal.\n";
      if not size_check then Printf.printf "main parser does not match on a 16-bit field (the size of ethertype)\n";
      if not exp_ref_check then Printf.printf "main parser does not match on the last variable read. \n";
      if not lucid_etherty_pat_check then Printf.printf "parser does not contain a first branch that matches LUCID_ETHERTY\n";
      if not direct_call_check then Printf.printf "parser's first branch does not call lucid parser directly.\n";
      if not rest_branches_check then Printf.printf "branches besides the parser's first branch call lucid's parser.\n";
      if (bits_read_check && size_check && exp_ref_check && lucid_etherty_pat_check && direct_call_check && rest_branches_check)
         then CallFromEth
         else CallInvalid)
   | _ -> CallInvalid

;;

let pkt_param_exp params = 
   match params with 
   | (id, ty)::_ when (CoreSyntax.equiv_ty ty pkt_arg_ty) -> 
      var (Cid.id id) ty
   | _ -> 
      error "main parser has wrong signature (expected a single argument of type bitstring)"
;;

let rec main_parser_opt ds = 
   match ds with 
   | [] -> None
   | decl::ds -> (
      match decl.d with 
      | DParser(id, params, parser_block) when (fst id = "main") -> 
         Some(params, parser_block)
      | _ -> main_parser_opt ds
   )
;;

let add_simple_parser recirc_port_opt ds = 
   (* add a simple parser given an optional recirculation port. 
      If the target is one that uses recirculation ports for 
      self events, then this event MUST be passed a port id *)
   (* used in new C IR *)
   (* 
      case 1: no packet events, no user parser, _ ->
         // parse all packets as events with compiler-given formats:
            lucid_background_event_parser 
      case 2: 1 packet event, no user parser, no recirc port ->  
         // parse all packets as the single packet event
         // (this assumes self-events are handled some way besides recirc)
         packetevent_parse_block
      case 3: 1 packet event, no user parser, recirc port -> 
         // parse all packets from recirc port as lucid-formatted events
         // parse all other packets as the packet event
         if (port == recirc_port) then lucid_background_event_parser
            else  packetevent_parse_block
   *)
   let pkt_events = List.filter is_pktev ds in
   let bg_events = List.filter is_bgev ds in 
   let main_user_parser_opt = main_parser_opt ds in
   let pkt_var = var (Cid.create ["pkt"]) pkt_arg_ty in
   match pkt_events, main_user_parser_opt, recirc_port_opt with 
      | [], None, _ -> 
         let parser_body = lucid_background_event_parser pkt_var bg_events in
         let decl = (decl (parser (id "main") [id"pkt", pkt_arg_ty] parser_body)) in
         decl::ds
      | [pkt_ev_decl], None, None -> 
         let parser_body = packetevent_parse_block pkt_var pkt_ev_decl in
         let decl = (decl (parser (id "main") [id"pkt", pkt_arg_ty] parser_body)) in
         decl::ds
      | [pkt_ev_decl], None, Some(recirc_dpid, port_ty) -> 
         let branches = [
            (pbranch [recirc_dpid] (lucid_background_event_parser pkt_var bg_events));
            (pbranch_wild 1 (packetevent_parse_block pkt_var pkt_ev_decl))
            ] 
         in
         let eingress_port = (var (Cid.id Builtins.ingr_port_id) port_ty) in 
         let (step : parser_step) = pmatch [eingress_port] branches in
         let parser_body = block [] step in
         let decl = (decl (parser (id "main") [id"pkt", pkt_arg_ty] parser_body)) in
         decl::ds
      | _ , Some(main_params, main_block), _ -> 
      (* if there's a main parser, check the user parser to make sure it's well-formed, 
         then inline parsers, including the background block which 
         replaces the call_lucid_parser builtin *)
         let parser_entry_ty = check_valid_entry_parse_block main_block in
         let pkt_var = pkt_param_exp main_params in (* we need to know the name of the packet argument to main *)
         inline_parsers parser_entry_ty pkt_var bg_events ds
      | _, _, _ -> 
         error "invalid combination of packet events, user parser, and recirc port"
;;

let add_parser port_ty (portspec:port_config) ds =
   (* separate packet and background events *)
   let pkt_events = List.filter is_pktev ds in
   let bg_events = List.filter is_bgev ds in 
   (* construct the background event parsing block *)
   (* note: we need to construct a slightly different parser depending on whether the user calls the lucid parser before or after eth *)
   match main_parser_opt ds with 
   | Some(main_params, main_block) -> 
      (* if there's a main parser, check the user parser to make sure it's well-formed, 
         then inline parsers, including the background block which 
         replaces the call_lucid_parser builtin *)
      let parser_entry_ty = check_valid_entry_parse_block main_block in
      let pkt_var = pkt_param_exp main_params in (* we need to know the name of the packet argument to main *)
      inline_parsers parser_entry_ty pkt_var bg_events ds
   (* if there's no main parser, attempt to generate one *)
   | None -> (
      let pkt_var = var (Cid.create ["pkt"]) pkt_arg_ty in
      match pkt_events with 
      | [] -> 
         (* case 1: no packet events and no parsers -- so make a parser for the bg events. *)
         let bg_block = lucid_background_event_parser pkt_var bg_events in

         (decl (parser (id "main") [id"pkt", pkt_arg_ty] bg_block))::ds
      | _ -> 
      (* case 2: packet events declared, but no parser -- 
         so make a parser that parses packet or background events 
         according to the portspec. *)
         (portspec_to_parser port_ty portspec pkt_var pkt_events bg_events)::ds)
;;