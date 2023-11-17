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
open AddIntrinsics
open TofinoSplit

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
and bit = [%import: CoreSyntax.bit]
and bits = [%import: CoreSyntax.bits]

(*NEW 6/2023 -- event types / definitions *)

and header = {
  header_id : id;
  header_tyid : id;
  header_ty : ty;
  header_const : value option;
}

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

    (* outer id is a struct id, inner id is struct's field. *)
    tag : id * (id * ty);

    hdrs : header list; 
      (* headers to put in front of the packet when serialized *)
    member_nums : int list; (*the numbers of all the members of this event, in other words, the values the tag may hold *)
  }
  (* an event set is a set of optional events *)
  | EventSet of {
    evid:id;
    members: event list;
    flags : id * (id * ty) list * (id * ty) option;
      (* struct id, flag field ids and tys, optional padding id and ty *)
    (* flags : (id * ty) list; a 2-bit flag for each member *)
    (* this is a list of the generate sequences that occur in the program.
        each inner list is a subset of members that are generated together, 
        in the same control flow. Each member in the inner list is a tuple, 
        where the first element is the id of the generated event and the secon 
        element is the type of generate statement that produced it. *)
    generated_events : (id * gen_type) list list;
  }
  | EventWithMetaParams of {
    event : event;
    params : params;
  }

and hbody = 
  | SFlat of statement
  | SPipeline of statement list

(* definition of a handler using input and output events *)
and hevent = {    
  hdl_id : id;
  hdl_sort : handler_sort;
  hdl_body : hbody;  
  hdl_deparse_params : params; (* parameters of the handler's deparser *)
  hdl_deparse : statement; (* the deparser, if there is one *)
  hdl_input : id; 
  hdl_output : id; 
    (*input and output are event ids, for merged events that should be 
     constructed at the same time as the handler. *)
  
  hdl_preallocated_vars : params;(*variables that are pre allocated, but not set*)
  
  (* params and retparams are builtins of the underlying hardware *)
  hdl_params : params; (*parser output that is not wrapped in 
     an event. timestamp, ingress port, queue depth, etc *)
  hdl_retparams : params; (* multicast group, egress port, etc *)
  
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

and parser = {
  pid : id;
  pparams : params;
  pblock : parser_block;
  pret_event : id option;
  phdlret_event : id option; 
    (* P4's semantics force the parser to know about 
       the return value of the function that the parser feeds into... *)
  pret_params : params;
}

and group = {
  gnum : int; (* group number *)
  gcopies : (int * int) list; (* port, replica id *)
}

and td =
  | TDGlobal of id * ty * exp
  | TDMemop of memop
  | TDExtern of id * ty
  | TDAction of action
  | TDParser of parser
  (* new / changed decls *)
  | TDEvent of event
  | TDHandler of handler
  | TDOpenFunction of id * params * statement (*accessible variables should be tracked*)
  | TDMulticastGroup of group (* used by the control component *)
and tdecl =
  { td : td
  ; tdspan : sp
  ; tdpragma : pragma list
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
 


let no_span = Span.default;;
let tdecl td = {td; tdspan = no_span; tdpragma = []}
let opt_to_list opt = match opt with | None -> [] | Some x -> [x];;
(* translate decl and add to a component *)
let decl_to_tdecl (decl:decl) = 
  match decl.d with
  | DGlobal (id, ty, exp) ->
    { td = TDGlobal (id, ty, exp)
    ; tdspan = decl.dspan
    ; tdpragma = opt_to_list decl.dpragma
    }
  | DMemop m -> { td = TDMemop m; tdspan = decl.dspan; tdpragma = opt_to_list decl.dpragma }
  | DExtern (i, t) ->
    { td = TDExtern (i, t); tdspan = decl.dspan; tdpragma = opt_to_list decl.dpragma }
  | DAction a ->
    { td = TDAction a; tdspan = decl.dspan; tdpragma = opt_to_list decl.dpragma }
   | DEvent (evid, evnum, evsort, evparams) ->
    let event = EventSingle{
      evid; 
      evnum; 
      evsort; 
      evparams; 
      } in
    { td = TDEvent event; tdspan = decl.dspan; tdpragma = opt_to_list decl.dpragma }
  | DHandler (hdl_id, hdl_sort, (hdl_params, hdl_body)) ->
    (* handlers are initially of variant "HParams", and translated to 
       "HEvent" in the "addhandlertypes" pass. *)
    let handler = HParams {hdl_id; hdl_sort; hdl_params; hdl_body} in
    { td = TDHandler (handler); tdspan = decl.dspan; tdpragma = opt_to_list decl.dpragma }
  (* the main parse block is the entry point of the ingress parser.
     we add the return parameters (intrinsic metadata) and an action 
     at the beginning of the parser to read that parameter *)
  | DParser(pid, pparams, pblock) when ((fst pid) = "main") -> 
    let pkt_id, _ = match pparams with 
      (* this should be checked by wellformed in frontend and maintained throughout *)
      | [(id, ty)] -> (id, ty)
      | _ -> error "internal error -- main parser did not have correct arguments."
    in
    
    let actions, spans = List.split pblock.pactions in
    let intr_id, intr_ty = intrinsic_to_param ingress_intrinsic_metadata_t in 
    let read_intr_acn = PRead((Cid.id intr_id), intr_ty, var (Cid.id pkt_id) pkt_arg_ty) in
    let skip_resubmit_intr_acn = skip (tint 64) in 
    let pblock = {pblock with
      pactions=
      [read_intr_acn, Span.default; skip_resubmit_intr_acn, Span.default]
        @(List.combine actions spans);}
    in
    {
    td = TDParser({pid; pparams; pblock;
      pret_event=None; (*the return event of the ingress parser 
                         is set when events are merged*)
      phdlret_event=None;
      pret_params=[intr_id, intr_ty];});
    tdspan=decl.dspan;
    tdpragma=opt_to_list decl.dpragma;}
  (* other parsers are also ingress parsers that will be inlined later *)
  | DParser(pid, pparams, pblock) -> {
    td = TDParser({pid; pparams; pblock; 
        pret_event=None;
        phdlret_event=None;
        pret_params = [];});
    tdspan = decl.dspan;
    tdpragma = opt_to_list decl.dpragma;}
;;

let rec decls_to_tdecls tdecls ds : tdecls = 
  match ds with
  | [] -> tdecls
  | d :: ds -> 
    let tdecl = decl_to_tdecl d in
    decls_to_tdecls (tdecls@[tdecl]) ds
  ;;

(* translate the program into a tofinocore program *)
let core_to_tofinocore split_prog : prog = 
  (* the input is a split_prog from splitDataplane *)
  let ingress_decls = split_prog.ingress in
  let egress_decls = split_prog.egress in
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
  (* 6/29/23 -- the control component just holds multicast group declarations for now. *)
  let control = {
    comp_id = id "control";
    comp_succ = [id "ingress"];
    comp_decls = [];
    comp_sort = HControl;
  } in
  [ingress; egress; control]
;;

(* helpers (TODO: find / delete unused) *)
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
let rec id_of_event event = 
  match event with
  | EventSingle {evid;} -> evid
  | EventUnion {evid;} -> evid
  | EventSet {evid;} -> evid
  | EventWithMetaParams {event; params=_;} -> id_of_event event
;;
let params_of_event event =
  match event with 
  | EventSingle {evparams;} -> evparams
  | _ -> error "[params_of_event] only base events (EventSingle) have parameters"
;;
let rec members_of_event event = 
  match event with 
  | EventSingle _ -> error "[members_of_event] single event has no event menbers"
  | EventUnion{members;}
  | EventSet{members;} -> members
  | EventWithMetaParams{event; params=_;} -> members_of_event event
;;
let num_of_event event =
  match event with
    | EventSingle {evnum=Some(n);} -> n
    | _ -> error "[num_of_event] not a numbered event!"
;;

let sort_of_event event = 
  match event with 
  | EventSingle {evsort} -> evsort
  | _ -> error "cant get the sort of a compound event"
;;
let rec etag event = 
  match event with 
  | EventUnion{tag;} -> tag
  | EventWithMetaParams{event; params=_;} -> etag event
  | _ -> error "[etag] not a union event, so no tag"
;;

let rec etagged_members event = 
  match event with 
  | EventUnion{members; member_nums;} -> 
    List.combine member_nums members
  | EventWithMetaParams{event; params=_;} -> etagged_members event
  | _ -> error "[etagged_members] not a union, so no tagged members"
;;

let eventset_flag_id_of_member event = 
  Id.prepend_string "flag_" (id_of_event event) 
;;

let id_of_generate statement = 
  match statement.s with
  | SGen(_, exp) -> (
    match exp.e with
    | ECall(cid, _, _) -> Cid.to_id cid
    | _ -> error "[id_of_generate] event variables are not yet supported in generates."
  )
  | _ -> error "[id_of_generate] expected generate statement."
;;

let sort_of_generate statement = 
  match statement.s with
  | SGen(gty, _) -> ( gty )
  | _ -> error "[sort_of_generate] expected generate statement."
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
          , { e = ECall (_, num_slots :: _, _) } ) ->
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


let main_parser_of_tds tds = 
  let opt_things = List.filter_map (fun tdecl -> 
    match tdecl.td with 
    | TDParser(p) -> (
      if (fst p.pid = "main")
        then (Some(p))
        else None)
    | _ -> None)
  tds
      in
  List.hd opt_things
;;
let main_parser_of_component component : parser = 
  let opt_things = List.filter_map (fun tdecl -> 
    match tdecl.td with 
    | TDParser(p) -> (
      if (fst p.pid = "main")
        then (Some(p))
        else None)
    | _ -> None)
  component.comp_decls
      in
  List.hd opt_things
;;

let replace_main_parser_of_component parser component  = 
  let new_decls = List.map (fun tdecl -> 
    match tdecl.td with 
    | TDParser(p) -> (
      if (fst p.pid = "main")
        then {tdecl with td = TDParser(parser)}
        else tdecl)
    | _ -> tdecl)
  component.comp_decls
      in
  {component with comp_decls = new_decls}
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

let rec add_pragma_to_main_handler pragma tdecls  = 
  match tdecls with
  | [] -> []
  | tdecl::tdecls -> (
    match tdecl.td with
    | TDHandler(_) -> 
      {tdecl with tdpragma = pragma::tdecl.tdpragma}::tdecls
    | _ -> tdecl::(add_pragma_to_main_handler pragma tdecls)
  )
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

let add_preallocated_locals pvars params = 
  pvars@params
;;

let add_shared_local tds tmp_id tmp_ty =
  let tmp_e = var_sp (Cid.id tmp_id) tmp_ty Span.default in
  let main_handler = main_handler_of_decls tds in
  let new_preallocated_vars =  
    main_handler.hdl_preallocated_vars@ 
    [(tmp_id, tmp_ty)]
  in
  tmp_e, replace_main_handler_of_decls tds
    {main_handler with
      hdl_preallocated_vars=new_preallocated_vars}


  (* tmp_e, replace_main_handler_of_decls
    tds
    ({main_handler with 
      hdl_preallocated_vars = 
        main_handler.hdl_preallocated_vars @ [(tmp_id, tmp_ty)]}) *)


        

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

(* add a call to enable a header field. 
   This call has no semantic meaning in this ir, 
   but is translated to an isValid call in p4, which 
   inlines event serialization. *)
let enable_call var_cid var_ty = 
  SUnit(
    call (Cid.create ["Sys"; "enable"])
    [var (var_cid) var_ty] (ty TBool))
  |> statement
;;


(* if comp is sort HControl, return comp, else return fcn comp *)
let skip_control fcn comp = 
  match comp.comp_sort with 
  | HControl -> comp
  | _ -> fcn comp
;;


let rec is_union_of_unions event = 
  match event with 
  | EventUnion({members;}) -> 
    List.for_all 
      (fun event -> match event with 
      | EventUnion(_) -> true
      | _ -> false)
      members
  | EventWithMetaParams({event; params=_;}) -> is_union_of_unions event
  | _ -> false
;;


let hdr header_id header_tyid header_ty header_const = 
  {header_id; header_tyid; header_ty; header_const}
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

(* list all the fields of the event, fully prefixed *)
let rec tyfields_of_event event : cid_params = 
  match event with 
  | EventSingle({evid; evparams;}) -> 
    List.map (fun (id, ty) -> (Cid.create_ids [evid; id], ty)) evparams
  | EventUnion({evid; members; tag;}) -> 
    let user_params = tyfields_of_members evid members in
    let tag_outer, (tag_inner, tag_ty) = tag in 
    (Cid.create_ids [evid;tag_outer; tag_inner], tag_ty)::user_params
  | EventSet({evid; members; flags;}) -> (
    let user_params = tyfields_of_members evid members in
    (* now add all the flags *)
    let flag_struct_id, flag_fields, pad_field = flags in 
    let flag_fields = List.map 
      (fun (flag_id, flag_ty) -> 
        Cid.create_ids [evid; flag_struct_id; flag_id], flag_ty)
      flag_fields
    in
    match pad_field with
    | None -> user_params@flag_fields
    | Some(pad_id, pad_ty) -> 
      user_params
      @flag_fields
      @[Cid.create_ids [evid; flag_struct_id; pad_id], pad_ty]
  )
  | EventWithMetaParams({event; params;}) ->
    let inner_param_ids = tyfields_of_event event in
    let meta_param_ids = List.map 
      (fun (id, ty) -> Cid.create_ids [id_of_event event; id], ty) 
      params 
    in
    inner_param_ids @ meta_param_ids
and tyfields_of_members evid members = 
  List.fold_left
    (fun params member -> 
      let member_params = tyfields_of_event member in
      params@
      (List.map 
        (fun (cid, ty) -> Cid.compound evid cid, ty)
        member_params))
    []
    members

let fields_of_event event = tyfields_of_event event |> List.split |> fst ;;


(* event update helpers. So that we don't have to use refs. *)
(* look through the declarations in comp and find the event with id evid *)
let get_event_tds tds evid = 
  let events = List.filter_map (fun tdecl -> 
    match tdecl.td with 
    | TDEvent(e) -> Some(e)
    | _ -> None)
  tds in
  List.find (fun e -> (id_of_event e) = evid) events
;;

let get_event comp evid = get_event_tds comp.comp_decls evid

(* replace the event in the component *)
let set_event_tds tds event = 
  let evid = id_of_event event in
  List.map (fun tdecl -> 
    match tdecl.td with 
    | TDEvent(e) -> 
      if (id_of_event e) = evid then {tdecl with td = TDEvent(event)}
      else tdecl
    | _ -> tdecl)
  tds

let set_event comp event = 
  {comp with comp_decls=set_event_tds comp.comp_decls event}

let main_input_event tds = 
  let main_handler = main_handler_of_decls tds in
  get_event_tds tds main_handler.hdl_input

let main_output_event tds = 
  let main_handler = main_handler_of_decls tds in
  get_event_tds tds main_handler.hdl_output