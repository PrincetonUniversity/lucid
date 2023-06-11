(* Transforms handlers from having parameters to having 
input and output events. (HParams to HEvent) *)

(*
  Also quantifies the names of:
    - parameter variables used in the body (prepend the input event's id)
    - event names in event assignments and generates (prepend the output event's id)
*)

open CoreSyntax
open TofinoCoreNew
open BackendLogging
open Collections


(* context for this transformation pass: a list of events *)
module Ctx = Collections.IdMap
type ctx = {
  is_egress : bool;
  events : event Ctx.t;
}
let empty_ctx = { is_egress = false; events = Ctx.empty;}

(***find all the generate paths ***)
let find_generate_sequences stmt =
  let stmt_filter stmt = match stmt.s with
    | SGen (_, _) -> Some(stmt)
    | _ -> None
  in
  find_statement_paths [] stmt_filter stmt
;;


(* takes the body of a handler and its output eventset
   and computes the possible bitvectors for the output eventset *)
let generated_eventid_subsets (hdl_body:statement) =
  let generate_sequences = find_generate_sequences hdl_body in
  (* convert the generate sequences into sets of event ids *)
  let event_id_sequences = List.map
    (fun generate_sequence ->
      List.map id_of_generate generate_sequence)
    generate_sequences 
  in
  (* remove duplicate event id sequences *)
  let compare_id_seqs id_seq1 id_seq2 =
    let compare_ids id1 id2 = Id.compare id1 id2 in
    List.compare compare_ids id_seq1 id_seq2
  in

  let event_id_sequences = List.sort_uniq compare_id_seqs event_id_sequences in
  event_id_sequences
;;



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
  (* now make the list of event ids unique *)
  let event_ids = List.sort_uniq Id.compare event_ids in
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
      members = events;})
    else EventSet({
      evid = Id.append_string "_ingress_output" hdl_id;
      members = events;
      subsets = generated_eventid_subsets hdl_body;})
  in
  eventset
;;


(* make sure the event constructor is defined in the given event. 
   for example, given: 
    - event_union bar { event foo(int a, int b); event baz(int c) }
    - bar.baz
    - we want to make sure that bar defines baz, 
      and thus bar.baz is a valid constructor id.   
   *)
let rec ensure_event_defines_econs event econs_cid : bool = 
  match econs_cid with
  | Cid.Id(id) -> (
    match event with 
    | EventSingle({evid;}) ->
      if (Id.equal evid id)
        then true
        else false
    | EventUnion _ | EventSet _ -> false
  )
  | Cid.Compound(id, cid) -> (
    match event with
    | EventSingle _ -> false
    | EventUnion({evid; members;})
    | EventSet({evid; members;}) -> (
      (* first, make sure the id is the event id *)
      if (Id.equal evid id)
        then (
          (* second, recurse on the remaining cid on all the members *)
          let member_results = List.map (fun e -> ensure_event_defines_econs e cid) members in
          (* third, exactly one member should have returned true *)
          let num_true = List.fold_left (fun acc b -> if b then acc + 1 else acc) 0 member_results in
          match num_true with
          | 1 -> true
          | 0 -> false
          | _ -> error "[addHandlerTypes.ensure_event_defines_econs] event id uniqueness violated: multiple sub-events with the same name."
          )
        else false
    )
  )
;;
(* derive the type of a parameter named cid in the event. 
   The event may be a compound event, in which case the 
   cid has a compound component. *)
let rec get_event_param_ty event cid : ty option = 
  match cid with 
  | Cid.Id(id) -> (
    match event with 
    | EventSingle({evparams;}) -> (
      match List.assoc_opt id evparams with 
      | None -> None
      | Some(ty) -> Some(ty)
    )
    (* non-compound ids cannot be parameters of union or set events. *)
    | EventUnion _ | EventSet _ -> error "[get_event_param_ty] non-compound ids cannot be parameters of union or set events."
  )
  | Cid.Compound(id, cid) -> (
    match event with
    | EventSingle _ -> error "[get_event_param_ty] compound ids cannot be parameters of single events."    
    | EventUnion({evid; members;})
    | EventSet({evid; members;}) -> (
      (* first, make sure the id is the event id *)
      if (not (Id.equal evid id))
        then None
        else (          
          (* second, try recursing on every member event. Only one should resolve. *)
          let ty_opts = List.filter_map (fun e -> get_event_param_ty e cid) members in
          match ty_opts with
          | [] -> None
          | [ty] -> Some(ty)
          | _ -> error "[get_event_param_ty] compound id should resolve to exactly one parameter type."
        )
    )
  )
;;

(* rename variables given a map from old cids -> new cids *)
let rename =
  object
    inherit [_] s_map as super

    method! visit_EVar (env : Cid.t CidMap.t) x =
      match CidMap.find_opt (x) env with
      | Some e -> EVar e
      | None -> EVar x
  end
;;

(* generic visitor to transform an expression. Arguments: 
   1) transformer function 
   2) statement or whatever node to start traversal on *)
let transform_exp =
  object
  inherit [_] s_map as super
    method! visit_exp (env : exp -> exp) exp =
      (* transform subexpressions, then the outer expression *)
      env (super#visit_exp env exp)
    end
;;

(* scope the parameters of the handler by renaming them from foo to input_event.foo *)
let scope_params (hdl_params: params) (hdl_body: statement) (input_event : event) =
  (* check if the parameters are all members of the input event by getting their types. *)
  let derived_param_tys = List.map (fun (id, _) -> get_event_param_ty input_event (Cid.id id)) hdl_params in
  (* if any of the param types are none, its an error *)
  let _ = List.iter (fun ty_opt -> match ty_opt with
    | None -> error "[addHandlerTypes.scope_params] could not find parameter type in input event."
    | Some _ -> ()) derived_param_tys
  in
  (* make sure the inferred parameter types are equal to the listed parameter types *)
  let listed_tys = List.split hdl_params |> snd in
  let inferred_tys = List.filter_map (fun ty_opt -> ty_opt) derived_param_tys in
  let _ = if (not (equiv_list equiv_ty listed_tys inferred_tys))
    then error 
      "[addHandlerTypes.scope_params] parameter types inferred from input event do not match listed parameter types."
  in
  (* now we can scope the parameters, by just prepending the event name *)
  let scoped_params = List.map (fun (id, ty) -> 
    (Cid.create_ids [id_of_event input_event; id], ty)) hdl_params 
  in
  (* now build a map from old parameter cids to new parameter cids *)
  let hdl_param_cids = List.map (fun p -> fst p |> Cid.id) hdl_params in
  let (cid_tuples : (cid * cid) list) = List.combine hdl_param_cids (List.map fst scoped_params) in
  let rename_map = List.fold_left
    (fun acc (old_cid, new_cid) -> CidMap.add old_cid new_cid acc)
    CidMap.empty
    cid_tuples
  in
  let hdl_body' = rename#visit_statement rename_map hdl_body in
  hdl_body'
;;

let scope_event_constructors (output_event : event) (hdl_body : statement) =
  (* scope event constructor expressions wherever they appear in the statement, 
     so that the event id in the expression is prefixed with the event that 
     contains it in the output event. For example: 
      generate (foo(1, 2));
      --> 
      generate (output_event.foo(1, 2)); *)

      (* transform event constructor expressions
         (type: event; variant: ECall(evcid, evargs)); *)
      let econs_transformer exp = 
        match exp.e, exp.ety.raw_ty with
        | (ECall(evcid, evargs), TEvent) -> (
          (* this is an event constructor. The new name is 
             the old name, with the output event id prefixed. *)
          let evcid' = Cid.compound (id_of_event output_event) evcid in
          (* check to make sure the new name is valid *)
          if (ensure_event_defines_econs output_event evcid')
            then {exp with e=ECall(evcid', evargs)}
            else error "[addHandlerTypes.scope_event_constructors] event constructor not defined in output event."
        )
        (* non-event-constructor expressions: do nothing. *)
        | _ -> exp 
      in
      transform_exp#visit_statement econs_transformer hdl_body
;;


(* set the handler input and output events, 
   update all the parameter variable ids used in the body (scope input), 
   update all the event ids used in generates (scope output) *)
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
    let hdl_body' = scope_params hdl_params hdl_body input_event 
      |> scope_event_constructors output_event
  in
    HEvent({hdl_id; 
      hdl_sort; 
      hdl_body=[hdl_body']; 
      hdl_input=input_event;
      hdl_output=output_event; 
      hdl_inparams=[]; 
      hdl_outparams=[];})
    , {td=TDEvent(output_event); tdspan = Span.default; tdpragma = None;}
  | _ -> error "[addHandlerTypes.type_handler] there shouldn't be any HEvent handlers at this point"

let rec type_handlers_in_tdecls ctx tdecls : tdecl list =
  match tdecls with
  | [] -> []
  | td :: tdecls ->
    match td.td with
    (* type the handlers, possibly adding new decls for event types *)
    | TDHandler (hdl) -> 
      let hdl', hdl_out_event = type_handler ctx hdl in
      let td' = { td with td = TDHandler (hdl') } in
      hdl_out_event :: td' :: type_handlers_in_tdecls ctx tdecls
    (* add events to the context *)
    | TDEvent(e) ->     
      let ctx' = {ctx with events=(Ctx.add (id_of_event e) e ctx.events);} in
      td :: type_handlers_in_tdecls ctx' tdecls
    (* leave all the other decls alone *)
    | _ -> td :: type_handlers_in_tdecls ctx tdecls
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