(* add types to handlers in TofinoCore, changing all handlers from HParams to HEvent *)

open CoreSyntax
open TofinoCoreNew
open BackendLogging


(* context for this transformation pass: a list of events *)
module Ctx = Collections.IdMap
type ctx = {
  is_egress : bool;
  events : event Ctx.t;
}
let empty_ctx = { is_egress = false; events = Ctx.empty;}

(* find the generates in a statement *)
let rec find_generates (stmt : statement) : (gen_type * exp) list =
  match stmt.s with
  | SNoop | SUnit _ | SLocal _ | SAssign _ | SPrintf _ | SRet _ ->
      []
  | STableMatch _ | STableInstall _-> []
  | SIf (_, then_stmt, else_stmt) ->
      find_generates then_stmt @ find_generates else_stmt
  | SGen (gen_type, exp) -> [(gen_type, exp)]
  | SSeq (stmt1, stmt2) ->
      find_generates stmt1 @ find_generates stmt2
  | SMatch (_, branch_list) ->
      List.concat (List.map (fun (_, stmt) -> find_generates stmt) branch_list)
;;
  
(* derive the output event for this handler based on the generate statements *)
let derive_output_event (ctx:ctx) (hdl_id : id) (hdl_body:statement) : event =
  let generates = find_generates hdl_body in
  (* At this point, each generate expression should be an ecall, where the id 
     is the id of the event that it generates. We get that list of event ids. *)
  let event_ids = List.map (fun (_, exp) -> match exp.e with
    | ECall (cid, _) -> Cid.to_id cid
    | _ -> error "[addHandlerTypes.derive_output_type] generate expression should be an ecall") 
    generates 
  in
  (* now we look up the event ids in the context to get events *)
  let events = List.map (fun id -> match Ctx.find_opt id ctx.events with
    | Some e -> e
    | None -> error "[addHandlerTypes.derive_output_type] could not find event with same ID as generate expression") 
    event_ids
  in
  (* now we create an event set or union for the output. Ingress 
     is a set, because it may generate multiple events that are encoded 
     as one event. Whereas egress produces a union, because it only 
     generates one event. *)
  let eventset = if (ctx.is_egress)
    then EventUnion({
      evid = Id.append_string "_egress_output" hdl_id;
      members = List.mapi (fun i e -> (i, e)) events;})
    else EventSet({
      evid = Id.append_string "_ingress_output" hdl_id;
      members = List.mapi (fun i e -> (i+1, e)) events;})
  in
  eventset
;;

(* set the handler input and output events *)
let type_handler (ctx:ctx) hdl : handler * tdecl =  
  let _ = ctx in 
  match hdl with 
  | HParams ({hdl_id; hdl_sort; hdl_params; hdl_body}) ->
    let _ = hdl_params in 
    let input_event = match Ctx.find_opt hdl_id ctx.events with 
      | Some e -> e
      | None -> error "[addHandlerTypes.type_handler] could not find event with same ID as user-defined handler"  
    in
    let output_event = derive_output_event ctx hdl_id hdl_body in
    HEvent({hdl_id; 
      hdl_sort; 
      hdl_body=[hdl_body]; 
      hdl_input=input_event; 
      hdl_output=output_event; 
      hdl_inparams=[]; 
      hdl_outparams=[];})
    , {td=TDEvent(output_event); tdspan = Span.default; tdpragma = None;}
  | _ -> error "[addHandlerTypes.type_handler] there shouldn't be any HEvent handlers at this point"

let rec type_handlers_in_tdecls ctx tdecls : tdecl list =
  match tdecls with
  | [] -> []
  | td :: tdecls' ->
    match td.td with
    (* type the handlers, possibly adding new decls for event types *)
    | TDHandler (hdl) -> 
      let hdl', hdl_out_event = type_handler ctx hdl in
      let td' = { td with td = TDHandler (hdl') } in
      hdl_out_event :: td' :: type_handlers_in_tdecls ctx tdecls'
    (* add events to the context *)
    | TDEvent(e) ->     
      let ctx' = {ctx with events=(Ctx.add (id_of_event e) e ctx.events);} in
      td :: type_handlers_in_tdecls ctx' tdecls
    (* leave all the other decls alone *)
    | _ -> td :: type_handlers_in_tdecls ctx tdecls'
;;

let type_handlers prog : prog =  
  List.map (fun component -> 
    match (Id.name component.comp_id) with
    | "ingress" -> 
      let ctx = { empty_ctx with is_egress = false } in
      { component with comp_decls = type_handlers_in_tdecls ctx component.comp_decls }
    | "egress" ->
      let ctx = { empty_ctx with is_egress = true } in
      { component with comp_decls = type_handlers_in_tdecls ctx component.comp_decls }
    | _ -> component)
    prog
;;