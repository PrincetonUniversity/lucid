(* 
Updated TofinoCore IR (6/2023)
  - event types
  - handler types for representation of 
    ingress and egress controls as handlers that 
    generate "union events" or "set events"
  - parsers
  - simple representation of architecture
*)
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
  | EventSingle of {evid:id; evnum : int option; evsort : event_sort; evparams : params}
  (* an event union is a union of events, with each event having a tag. *)
  | EventUnion  of {
    evid:id;
    members: (int * event) list;
  }
  (* an event set is a set of events, with each event having an index *)
  (* one of each *)
  | EventSet of {
    evid:id;
    members: (int * event) list;
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
  | HEvent of {
    hdl_id : id;
    hdl_sort : handler_sort;
    hdl_body : statement list;  
    hdl_input : event;
    hdl_output : event;
    (* a handler might also need to io params for externs or something? *)
    hdl_inparams : params;
    hdl_outparams : params;
  }

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
  comp_decls : tdecls; }

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
    let event = EventSingle{evid; evnum; evsort; evparams} in
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
let core_to_tofinocore ingress_ds egress_ds : prog = 
  (* two components: ingress and egress *)
  let ingress = {
    comp_id = id "ingress"; 
    comp_succ = [id "egress"]; 
    comp_decls = decls_to_tdecls [] ingress_ds
    } in
  let egress = {
    comp_id = id "egress"; 
    comp_succ = []; 
    comp_decls = decls_to_tdecls [] egress_ds} in
  [ingress; egress]
;;

(* destructors -- get back to the decl lists form that 
   the current pipeline expects. *)
let find_component_by_id prog id = 
  List.find (fun c -> c.comp_id = id) prog
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