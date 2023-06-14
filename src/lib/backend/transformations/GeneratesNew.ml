(* 

  After this pass, generate statements only need to set multicast ID and egress port. 


    - All event parameters are set in this pass. 
    - event count is set (really: mc_group_a)
    - After this pass, to translate a generate statement, you must: 
      1. all events: set event_generated flag
      2. port generate: set egress_port and port_evid
      3. ports generate: create multicast group and set ports_mcid
  *)

(* 
   the new generate elimination pass is different. It does not need to 
invalidate any headers, because there are separate input and output events. 
Generate elimination is now a local transformation -- you can translate a 
generate statement by itself into a sequence of assign operations to set:
1) output event arguments
2) output event tag
3) output port
4) output multicast group

ah.. but a tricky part: we need to know which input event branch we are in 
  to properly set the output event tag.
no. you can figure that out from the constructor used in the generate event.
*)


open Batteries
open Collections
open CoreSyntax
open TofinoCoreNew
open AddHandlerTypes

(* Disable warnings 21, 27, and 26 *)
[@@@ocaml.warning "-21-27-26"]


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
  | EventSet{members; flags; active_member_ct;} -> (
    (* get the index of the member *)
    let member_pos = pos_of_event members inner_event_id in
    (* set the appropriate flag variable *)
    let (flag_id, flag_ty) = List.nth flags member_pos in
    let active_member_ct_cid = Cid.concat 
      outer_event_cid 
      (Cid.id (fst active_member_ct)) 
    in
    let active_member_ct_ty = snd active_member_ct in
    let full_flag_id = Cid.concat outer_event_cid (Cid.id flag_id) in
    let eincr_ct = 
      op
      Plus
      [(var active_member_ct_cid active_member_ct_ty);
        (vint_exp_ty 1 active_member_ct_ty)]
      (active_member_ct_ty)  
    in
    sseq 
      (sassign 
        full_flag_id
        (vint_exp_ty 1 flag_ty))
      (sassign active_member_ct_cid eincr_ct)
  )
  | EventUnion{members; tag} -> 
    let tag_id, tag_ty = tag in
    let full_tag_id = Cid.concat outer_event_cid (Cid.id tag_id) in
    let enewtagval = (vint_exp_ty (num_of_event members inner_event_id) (tag_ty)) in
    sassign
      full_tag_id
      enewtagval

  | EventSingle _ -> error "[enable_member_event] single events do not have members"
;;

let rec enable_member_rec event_prefix event member_event_cid = 
  (* 1. enable the first component of member_event_cid in event, 
     2. move the events id to event_prefix
     3. 
     
  *)
  match member_event_cid with
  | Cid.Id(event_id) -> (

  )  
  (*  *)

;;

let eliminate = 
  object
    inherit [_] s_map as super

    method! visit_component _ component =
      let output_event_opt = Some ((main_handler_of_component component).hdl_output) in 
      super#visit_component output_event_opt component

    method! visit_statement output_event_opt stmt = 
      match stmt.s with 
      | SGen(gty, exp) -> 
        print_endline ("---- [eliminate] generate statement: ----"); 
        print_endline (CorePrinting.statement_to_string stmt);
        let ctor_cid, args = match exp.e with
          | ECall(cid, args) -> cid, args
          | _ -> error "[GeneratesNew.eliminate] event expression inside of a generate must be a constructor expression"
        in
        let output_event = match output_event_opt with
          | Some(output_event) -> output_event
          | None -> error "[eliminate_generates] no output event was set. This method may have been called incorrectly."
        in
        let event = match (base_event output_event ctor_cid) with 
          | None -> error "[eliminate_generates] could not find event constructor in output event" 
          | Some(event) -> event
        in
        (* params with full cids *)
        let params = List.map
          (fun (id, ty) -> 
            Cid.concat ctor_cid (Cid.id id), ty
            )
          (params_of_single_event event)
        in
        (* replace the generate statement with a sequence of assignments: *)
        (* example: generate(ingress_out.foo_out.bar); *)
        (* 1. enable the event 
                enable_member [] ingress_out foo_out;
                enable_member [ingress_out; foo_out] bar
        
        *)
        (* 1. set the output event's tag: ingress_out.foo_out.tag = get_num ingress_out.foo_out.tag *)
        (* 2. set the event parameters: ingress_out.foo_out.bar.a = ...; ...*)
        (* 3. [case: generate_port] set the egress port: ingress_out.foo_out.bar.port = ...;*)
        (* 3. [case: generate default and out event is set] increment the output event's active members counter: ingress_out.foo_out.active_ct += 1; *)
        (* 3. [case: generate_ports] set the group builtin: ingress_out.foo_out.bar.group = ...; *)
        stmt
      | _ -> super#visit_statement output_event_opt stmt
  end
;;



let eliminate_generates (prog : prog) : prog = 
  print_endline "[eliminate_generates] pass started";
  let prog = eliminate#visit_prog None prog in
  print_endline "[eliminate_generates] pass finished";
  exit 1;

  prog