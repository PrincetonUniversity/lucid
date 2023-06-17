(* 
Updated TofinoCore IR (6/2023)
  - event types
  - handler types for representation of 
    ingress and egress controls as handlers that 
    generate "union events" or "set events"
  - parsers
  - simple representation of architecture
*)
open Collections
open CoreSyntax
open BackendLogging
open MiscUtils


(* most of the tofinocore syntax tree is 
   directly from coreSyntax *)
type id = [%import: (Id.t[@opaque])]
and cid = [%import: (Cid.t[@opqaue])]
and tagval = [%import: (TaggedCid.tagval[@opqaue])]
and tcid = [%import: (TaggedCid.t[@opqaue])]
and sp = [%import: Span.t]
and z = [%import: (Z.t[@opaque])]
and zint = [%import: (Integer.t[@with Z.t := (Z.t [@opaque])])]
and location = int
and size = int
and sizes = size list
and raw_ty = [%import: CoreSyntax.raw_ty]
and tbl_ty = [%import: CoreSyntax.tbl_ty]
and acn_ty = [%import: CoreSyntax.acn_ty]
and func_ty = [%import: CoreSyntax.func_ty]
and ty = [%import: CoreSyntax.ty]
and tys = [%import: CoreSyntax.tys]
and op = [%import: CoreSyntax.op]
and pat = [%import: CoreSyntax.pat]
and v = [%import: CoreSyntax.v]
and event_val = [%import: CoreSyntax.event_val]
and value = [%import: CoreSyntax.value]
and pragma = [%import: CoreSyntax.pragma]
and e = [%import: CoreSyntax.e]
and exp = [%import: CoreSyntax.exp]
and branch = [%import: CoreSyntax.branch]
and gen_type = [%import: CoreSyntax.gen_type]
and s = [%import: CoreSyntax.s]
and tbl_def = [%import: CoreSyntax.tbl_def]
and tbl_match_out_param = [%import: CoreSyntax.tbl_match_out_param]
and tbl_match = [%import: CoreSyntax.tbl_match]
and tbl_entry = [%import: CoreSyntax.tbl_entry]
and statement = [%import: CoreSyntax.statement]
and params = [%import: CoreSyntax.params]
and body = [%import: CoreSyntax.body]
and event_sort = [%import: CoreSyntax.event_sort]
and handler_sort = [%import: CoreSyntax.handler_sort]
and conditional_return = [%import: CoreSyntax.conditional_return]
and complex_body = [%import: CoreSyntax.complex_body]
and memop_body = [%import: CoreSyntax.memop_body]
and memop = [%import: CoreSyntax.memop]
and action_body = [%import: CoreSyntax.action_body]
and action = [%import: CoreSyntax.action]
and parser_action = [%import: CoreSyntax.parser_action]
and parser_branch = [%import: CoreSyntax.parser_branch]
and parser_step = [%import: CoreSyntax.parser_step]
and parser_block = [%import: CoreSyntax.parser_block]

(*NEW 6/2023 -- event types / definitions *)

and event =
  | EventSingle of {
    evid:id; 
    evnum : int option; 
    evsort : event_sort; 
    evparams : params;
    }
  (* an event union is a union of events, with each event having a tag. *)
  | EventUnion  of {
    evid:id;
    members: event list;
    (* tag is an internal field of type TEvent that holds the active event's id *)
    tag : (id * ty);
    member_nums : int list; (*store the tag values of the members *)
  }
  (* an event set is a set of optional events *)
  | EventSet of {
    evid:id;
    members: event list;
    (* active members: an internal field listing the subset of members that are active *)
    flags : (id * ty) list;
    subsets: (id list) list; (*optional metadata for optimization: 
      the subsets of members that the eventset may hold.  *)
  }


and hbody = 
  | SFlat of statement
  | SPipeline of statement list


(* the string is direction specifier *)
and intrinsic_params = (id * ty * string option) list

and hinternal_params = {
  out_port : (id * ty);
  gen_ct : (id * ty);
  out_group : (id * ty);
}

(* definition of a handler using input and output events *)
and hevent = {    
  hdl_id : id;
  hdl_sort : handler_sort;
  hdl_body : hbody;  
  hdl_input : event; 
  hdl_output : event;
  hdl_internal_params : hinternal_params;
  (* hdl_intrinsics : intrinsic_params;  *)
    (* hdl_intrinsics are polluting the IR and have no semantic meaning.
        We should just have fixed fields for relevant output: 
        - output port
        - number of self generates
        - output group 
        Then let the lower p4-ish layer worry about mapping 
        those fields to the appropriate intrinsics.
        *)
  hdl_preallocated_vars : params; 
  (*variables that the handler can assume are allocated, but not set*)
}

and handler = 
  (* a handler with parameters -- basically just copied from input. *) 
  | HParams of {
    hdl_id : id;
    hdl_sort : handler_sort;
    hdl_params : params;
    hdl_body : statement;
  }
  (* a handler that operates on events instead of parameters -- 
     all handlers are tranformed into this form then merged together *)
  | HEvent of hevent

(* the events are the events that the parser may generate *)
(* and parser = 
  | PMain of parser_block * event list
  | PCalled of parser_block * event list *)

and td =
  | TDGlobal of id * ty * exp
  | TDMemop of memop
  | TDExtern of id * ty
  | TDAction of action
  | TDParser of id * params * parser_block
  (* new / changed decls *)
  | TDEvent of event
  | TDHandler of handler
  | TDVar of id * ty (* a variable used by multiple functions and handlers *)
  | TDOpenFunction of id * params * statement (* not an open function anymore *)
  | TDUserTy of {
      tyid : id;
      tyfields : (id * ty) list (* a user-defined record type *);
      tyextern : bool; (* when printing p4, do we assume that this type is defined somewhere else? *)
  }
and tdecl =
  { td : td
  ; tdspan : sp
  ; tdpragma : pragma option
  }

and tdecls = tdecl list

(* on the tofino, the program is distributed across
  multiple components (e.g., ingress, egress, other)
  Each component has an id, a list of successors that 
  it can send events to, and a list of declarations.  
  (There can also be some other metadata, e.g., io types) *)
and component = {
  comp_id   : id;
  comp_succ : id list; 
  comp_sort : handler_sort;
  comp_decls : tdecls; 
  }

and prog = component list

[@@deriving
  visitors
    { name = "s_iter"
    ; variety = "iter"
    ; polymorphic = false
    ; data = true
    ; concrete = true
    ; nude = false
    }
  , visitors
      { name = "s_map"
      ; variety = "map"
      ; polymorphic = false
      ; data = true
      ; concrete = true
      ; nude = false
      }]
 


(** core -> tofinocore translation: splitting into ingress and egress **)
type ctx = {
  globals : decl IdMap.t;
  ingress_globals : IdSet.t;
  egress_globals : IdSet.t;
  ingress : decl list;
  egress : decl list;
}

let empty_ctx = {
  globals = IdMap.empty;
  ingress_globals = IdSet.empty;
  egress_globals = IdSet.empty;
  ingress = [];
  egress = [];
}

let no_span = Span.default;;
let tdecl td = {td; tdspan = no_span; tdpragma = None}


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

let rec split_decls ctx decls : ctx =
  match decls with
  | [] -> 
    (* make sure that no globals overlap *)
    let ensure_no_globals_overlap ctx = 
      let overlap = IdSet.inter ctx.ingress_globals ctx.egress_globals in
      if not (IdSet.is_empty overlap) then
        error (
          Printf.sprintf 
          "Global variables are used in both ingress and egress: %s" 
          (CorePrinting.comma_sep (CorePrinting.id_to_string) (IdSet.to_list overlap)))
      else ctx
    in
    ensure_no_globals_overlap ctx
  | d :: ds ->
    match d.d with
    (* we don't know whether a global is in ingress or egress yet *)
    | DGlobal (id, _, _) ->
      let ctx' = { ctx with 
        globals = IdMap.add id d ctx.globals; } 
      in
      split_decls ctx' ds
    (* parsers are always ingress *)
    | DParser _ -> let ctx = { ctx with ingress = ctx.ingress @ [d] } in
      split_decls ctx ds
    (* handlers go to either ingress or egress, and take all the globals 
       that they reference with them. *)
    | DHandler(_, HData, (_, body)) -> 
      (* convert the globals map into a set of all globals *)
      let all_global_ids = IdMap.fold (fun id _ ids -> IdSet.add id ids) ctx.globals IdSet.empty in
      let globals_refd = globals_refd all_global_ids body in
      (* add the referenced globals to the ingress set *)
      let ingress_globals = IdSet.union ctx.ingress_globals globals_refd in
      print_endline ("adding DHandler to ingress");
      let ctx = {ctx with ingress_globals; ingress = ctx.ingress @ [d]} in
      split_decls ctx ds
    | DHandler(_, HEgress, (_, body)) -> 
      (* convert the globals map into a set of all globals *)
      let all_global_ids = IdMap.fold (fun id _ ids -> IdSet.add id ids) ctx.globals IdSet.empty in
      let globals_refd = globals_refd all_global_ids body in
      (* add the referenced globals to the egress set *)
      let egress_globals = IdSet.union ctx.egress_globals globals_refd in
      let ctx = { ctx with egress_globals; egress = ctx.egress @ [d] } in 
      split_decls ctx ds
    (* everything else goes to both *)
    | _ -> split_decls {ctx with ingress = ctx.ingress @ [d]; egress = ctx.egress @ [d]} ds
;;

(* add handlers that route the event from ingress -> egress and vice versa. 
   This is for events that do not have both an ingress and an egress handler declared. *)
let add_continue_handlers ctx : ctx =
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
      (ctx.ingress @ ctx.egress)
  in
  let all_events = IdMap.fold (fun _ v l -> v :: l) eventmap [] in
  (* we need a unique list of events by event id*)
  let ingress_handler_ids = List.filter_map (fun d -> match d.d with DHandler(hid, HData, _) -> Some(hid) | _ -> None) ctx.ingress in
  let egress_handler_ids = List.filter_map (fun d -> match d.d with DHandler(hid, HEgress, _) -> Some(hid) | _ -> None) ctx.egress in
  (* For each event that does not have a handler in ingress or egress,
      construct a new handler that generates the event with its input parameters. *) 
  let rec add_handlers ctx (events : decls) : ctx = 
    match events with 
    | [] -> ctx
    | d :: ds -> 
      match d.d with
      | DEvent(eid, _, _, params) -> 
        (* add ingress handler if it doesnt exist *)
        let ctx = if (not (List.mem eid ingress_handler_ids)) then 
          {ctx with ingress = ctx.ingress@[continue_handler HData eid params];}
        else ctx 
        in
        (* add egress handler if it doesn't exist *)
        let ctx = if (not (List.mem eid egress_handler_ids)) then 
          {ctx with egress = ctx.egress@[continue_handler HEgress eid params];}
        else ctx 
        in        
        add_handlers ctx ds
      | _ -> add_handlers ctx ds
  in   
  add_handlers ctx all_events
;;

(* 
TODO: put this somewhere, in a pass after we add the 
intrinsics for each component. 
Implicit semantics: add a command to set the drop flag 
   to the beginning of every egress handler. *)
let egr_drop_ctl_id = Cid.create ["drop_ctl"];;
let egr_drop_ctl_sz = 3
let rec add_default_egr_drop ds = 
  let set_drop_ctl = sassign egr_drop_ctl_id (vint_exp 1 egr_drop_ctl_sz) in
  let prepend_set_drop_ctl body =
    match body with
    | (params, stmt) -> (params, { stmt with s = SSeq(set_drop_ctl, stmt) })
  in
  let update_handler handler =
    match handler.d with
    | DHandler(id, HEgress, body) -> { handler with d = DHandler(id, HEgress, prepend_set_drop_ctl body) }
    | _ -> handler
  in
  List.map update_handler ds
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

(** core -> tofinocore translation: building components **)
(* translate decl and add to a component *)
let decl_to_tdecl (decl:decl) = 
  match decl.d with
  | DGlobal (id, ty, exp) ->
    { td = TDGlobal (id, ty, exp)
    ; tdspan = decl.dspan
    ; tdpragma = decl.dpragma
    }
  | DMemop m -> { td = TDMemop m; tdspan = decl.dspan; tdpragma = decl.dpragma }
  | DExtern (i, t) ->
    { td = TDExtern (i, t); tdspan = decl.dspan; tdpragma = decl.dpragma }
  | DAction a ->
    { td = TDAction a; tdspan = decl.dspan; tdpragma = decl.dpragma }
   | DEvent (evid, evnum, evsort, evparams) ->
    let event = EventSingle{
      evid; 
      evnum; 
      evsort; 
      evparams; 
      } in
    { td = TDEvent event; tdspan = decl.dspan; tdpragma = decl.dpragma }
  | DHandler (hdl_id, hdl_sort, (hdl_params, hdl_body)) ->
    let handler = HParams {hdl_id; hdl_sort; hdl_params; hdl_body} in
    { td = TDHandler (handler); tdspan = decl.dspan; tdpragma = decl.dpragma }
  | DParser _ -> error "Parsers are not yet supported by the tofino backend!"  
  ;;

(* raw translation pass -- just split program into ingress and egress components *)
let rec decls_to_tdecls tdecls ds : tdecls = 
  match ds with
  | [] -> tdecls
  | d :: ds -> 
    let tdecl = decl_to_tdecl d in
    decls_to_tdecls (tdecls@[tdecl]) ds
  ;;

(* translate the program into a tofinocore program *)
let core_to_tofinocore decls : prog = 
  (* first, fix event numbers for all events *)
  let decls = set_event_nums decls in 
  (* split the decls and add handlers to direct 
     events that are handled, but at a different component
     (e.g., an event arrives at ingress but there's only 
     an egress handler) *)
  let ctx = split_decls empty_ctx decls
    |> add_continue_handlers 
  in
  let ingress_decls = ctx.ingress in
  let egress_decls = ctx.egress in
  (* two components: ingress and egress *)
  let ingress = {
    comp_id = id "ingress"; 
    comp_succ = [id "egress"]; 
    comp_decls = decls_to_tdecls [] ingress_decls;
    comp_sort = HData;
    } in
  let egress = {
    comp_id = id "egress"; 
    comp_succ = []; 
    comp_sort = HEgress;
    comp_decls = decls_to_tdecls [] egress_decls;
    } in
  [ingress; egress]
;;

(* destructors -- get back to the decl lists form that 
   the current pipeline expects. *)
let find_component_by_id prog id = 
  List.find (fun c -> c.comp_id = id) prog
;;

let handlers tdecls = 
  List.filter_map 
    (fun td -> 
      match td.td with 
      | TDHandler _ -> Some(td) 
      | _ -> None) 
  tdecls
  ;;

let handlers_of_component comp = 
  handlers comp.comp_decls
;;

let prog_to_ingress_egress_decls prog = 
  (find_component_by_id prog (id "ingress")).comp_decls
  , (find_component_by_id prog (id "egress")).comp_decls
;;
let id_of_event event = 
  match event with
  | EventSingle {evid;} -> evid
  | EventUnion {evid;} -> evid
  | EventSet {evid;} -> evid
;;
let params_of_event event =
  match event with 
  | EventSingle {evparams;} -> evparams
  | _ -> error "[params_of_event] only base events (EventSingle) have parameters"
;;
let members_of_event event = 
  match event with 
  | EventSingle _ -> error "[members_of_event] single event has no event menbers"
  | EventUnion{members;}
  | EventSet{members;} -> members
;;
let num_of_event event =
  match event with
    | EventSingle {evnum=Some(n);} -> n
    | _ -> error "[num_of_event] not a numbered event!"
;;




let etag event = 
  match event with 
  | EventUnion{tag;} -> tag
  | _ -> error "[etag] not a union event, so no tag"
;;

let etagged_members event = 
  match event with 
  | EventUnion{members; member_nums;} -> 
    List.combine member_nums members
  | _ -> error "[etagged_members] not a union, so no tagged members"
;;

let eventset_flag_id_of_member event = 
  Id.prepend_string "flag_" (id_of_event event) 
;;

let id_of_generate statement = 
  match statement.s with
  | SGen(_, exp) -> (
    match exp.e with
    | ECall(cid, _) -> Cid.to_id cid
    | _ -> error "[id_of_generate] event variables are not yet supported in generates."
  )
  | _ -> error "[id_of_generate] expected generate statement."
;;

let memops tds = 
  List.filter_map
  (fun dec ->
    match dec.td with 
    | TDMemop m -> Some(m.mid, m)
    | _ -> None)
  tds
;;

(* returns assoc list: (arrayid : (slot width, num slots)) list *)
let array_dimensions tds =
  List.filter_map
    (fun dec ->
      match dec.td with
      | TDGlobal
          ( id
          , { raw_ty = TName (ty_cid, sizes, true); _ }
          , { e = ECall (_, num_slots :: _) } ) ->
        (match Cid.names ty_cid |> List.hd with
         | "Array" ->
           let num_slots = InterpHelpers.int_from_exp num_slots in
           Some (id, (List.hd sizes, num_slots))
         | "PairArray" ->
           let num_slots = InterpHelpers.int_from_exp num_slots in
           Some (id, (2 * List.hd sizes, num_slots))
         | _ -> None)
      | _ -> None)
    tds
;;


let main_handler_of_decls decls : hevent = 
  let handlers = handlers decls in
  (* if there is a main handler, there should only be 1 *)
  match handlers with 
  | [] -> error "[main_handler_of_decls] no main handler found."
  | [{td=TDHandler(HEvent(handler))}] -> handler
  | _ -> error "[main_handler_of_decls] decls not in single-handler form."

let main_handler_of_component component : hevent = 
  let handlers = handlers_of_component component in
  (* if there is a main handler, there should only be 1 *)
  match handlers with 
  | [] -> error "[main_handler_of_component] no main handler found."
  | [{td=TDHandler(HEvent(handler))}] -> handler
  | _ -> error "[main_handler_of_component] component not in single-handler form."
;;

let main_body_of_decls decls : hbody = 
  (main_handler_of_decls decls).hdl_body
;;


let replace_main_handler_of_decls tdecls new_handler = 
  List.map (fun tdecl -> 
    match tdecl.td with
    | TDHandler (HEvent _) -> 
      {tdecl with td = TDHandler((HEvent(new_handler)))}      
    | _ -> tdecl)
    tdecls
;;

let replace_main_handler_of_component component (new_handler:hevent) = 
  (* make sure there's a single main handler *)
  let _ = main_handler_of_component component in
  (* then replace *)
  {component with 
    comp_decls = List.map 
    (fun tdecl -> 
      match tdecl.td with
      | TDHandler (HEvent _) -> 
        {tdecl with td = TDHandler((HEvent(new_handler)))}      
      | _ -> tdecl)
    component.comp_decls}
;;

let main_of_decls tds =
  match (main_handler_of_decls tds).hdl_body with
  | SFlat s -> s
  | SPipeline _ -> error "[main_of_decls] main handler is not flat."
;;

let add_shared_local component tmp_id tmp_ty = 
  let tmp_e = var_sp (Cid.id tmp_id) tmp_ty Span.default in
  let main_handler = main_handler_of_component component in
  tmp_e, replace_main_handler_of_component
    component
    ({main_handler with 
      hdl_preallocated_vars = 
        main_handler.hdl_preallocated_vars @ [(tmp_id, tmp_ty)]})
;;
let add_shared_local tds tmp_id tmp_ty =
  let tmp_e = var_sp (Cid.id tmp_id) tmp_ty Span.default in
  let main_handler = main_handler_of_decls tds in
  tmp_e, replace_main_handler_of_decls
    tds
    ({main_handler with 
      hdl_preallocated_vars = 
        main_handler.hdl_preallocated_vars @ [(tmp_id, tmp_ty)]})


(* helper: find all the paths in the program containing statements that match 
           the filter map function. *)
let append_and_new seqs e =
  let rec append_to_all seqs e =
    match seqs with
    | [] -> []
    | seq :: seqs -> (seq @ [e]) :: append_to_all seqs e
  in
  match seqs with
  (* if there's nothing, make a new seq *)
  | [] -> [[e]]
  (* if there's something, append to first and recurse *)
  | seq :: seqs -> (seq @ [e]) :: append_to_all seqs e
;;
(* find all paths of statements that match the filter_map  *)
let rec find_statement_paths paths_so_far stmt_filter stmt =
  match stmt.s with
  | SSeq (s1, s2) ->
    let paths_including_s1 = find_statement_paths paths_so_far stmt_filter s1 in
    (* [[eva()]] *)
    (* [[eva()]] *)
    let paths_including_s2 =
      find_statement_paths paths_including_s1 stmt_filter s2
    in
    (* printres "seq" res; *)
    paths_including_s2
  | SIf (_, s1, s2) ->
    (* we get all paths for s1 + all paths for s2 *)
    let res =
      find_statement_paths paths_so_far stmt_filter s1
      @ find_statement_paths paths_so_far stmt_filter s2
    in
    res
  | SMatch (_, ps) ->
    let res =
      List.fold_left
        (fun seqs (_, bstmt) ->
          seqs @ find_statement_paths paths_so_far stmt_filter bstmt)
        []
        ps
    in
    res
  | _ ->
    (match stmt_filter stmt with
     | Some r -> append_and_new paths_so_far r
     | None -> paths_so_far)
;;          


(* find all paths of statements that match stmt_filter and 
   transform matching statements according to stmt_transformer *) 
let rec transform_statement_paths paths_so_far stmt_filter stmt_transformer stmt =
  match stmt.s with
  | SSeq (s1, s2) ->
    let s1', paths_including_s1 = transform_statement_paths paths_so_far stmt_filter stmt_transformer s1 in
    (* [[eva()]] *)
    (* [[eva()]] *)
    let s2', paths_including_s2 =
      transform_statement_paths paths_including_s1 stmt_filter stmt_transformer s2
    in
    (* printres "seq" res; *)
    (* transform the substatements and return paths *)
    {stmt with s=SSeq(s1', s2')}, paths_including_s2
  | SIf (e, s1, s2) ->
    (* we get all paths for s1 + all paths for s2 *)
    let s1', s1paths = transform_statement_paths paths_so_far stmt_filter stmt_transformer s1 in
    let s2', s2paths = transform_statement_paths paths_so_far stmt_filter stmt_transformer s2 in
    {stmt with s=SIf(e, s1', s2')}, s1paths @ s2paths
  | SMatch (es, ps) ->
    let ps', ps_paths =
      List.fold_left
        (fun (ps', ps_paths) (p, bstmt) ->
          let bstmt', bstmt_paths = transform_statement_paths paths_so_far stmt_filter stmt_transformer bstmt in
          (ps' @ [(p, bstmt')], ps_paths @ bstmt_paths))
        ([], [])
        ps
    in
    {stmt with s=SMatch(es, ps')}, ps_paths
  | _ ->
    (match stmt_filter stmt with
     | Some r -> stmt_transformer stmt, append_and_new paths_so_far r
     | None -> stmt, paths_so_far)
;;

(* find the component in the program with the given id *)
let find_component_by_id prog id = 
  match (List.find_opt (fun c -> c.comp_id = id) prog) with
  | Some c -> c
  | None -> error ("[find_component_by_id] could not find program component with id " ^ (Id.to_string id))
;;

let transform_component_by_id (f:component -> component) prog id : prog =
  List.map (fun c -> if c.comp_id = id then f c else c) prog
;;