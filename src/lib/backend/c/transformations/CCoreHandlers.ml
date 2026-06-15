(* 
  Construct the event handling function. 
    - input: an event
    - output: struct with next event, out event, and out port
    - inputs and outputs are by reference
    - has one branch for each handler 
    - generate is implemented by filling appropriate fields in output event
      - implications: 
        - no control flow is allowed to call "generate" and "generate_port" 
          more than once per control flow. 
        - if an event is recursive, the entire recursive loop may only 
          call generate_port once.        
*)
open CCoreSyntax
open CCorePPrint
open CCoreTransformers

let id = Id.create

let handler_cid = Cid.create ["handle_event"] ;;

let port_size = CConfig.c_cfg.port_id_size

(* out-param form (used by the below-the-boundary `lower` pass) *)
let in_ev_param = cid"ev_in", tref tevent
let next_ev_param = cid"ev_next", tref tevent
let out_ev_param = cid"ev_out", tref tevent
let out_port_param () = cid"out_port", tref@@tint port_size (* port for out event, 0 means no out event *)

(* value-semantic form (the waist): ev_in is taken by value, and the three
   outputs are locals returned as a tuple. *)
let v_ev_in = evar (cid"ev_in") tevent
let v_ev_next = evar (cid"ev_next") tevent
let v_ev_out = evar (cid"ev_out") tevent
let v_out_port = evar (cid"out_port") (tint port_size)
(* placeholder "no event" for unset outputs; never reaches C (the `lower` pass
   drops these inits -- the driver pre-initializes the out-params). A VSymbol so
   it types as tevent without referencing a real event constructor. *)
let no_event = { v = VSymbol(Cid.create ["_no_event"], tevent); vty = tevent; vspan = Span.default }

(* transform a generate statement into an assignment to the appropriate output
   local (value-semantic; the `lower` pass turns these locals into out-params). *)
let transform_generate statement =
  match statement.s with
  | SUnit(exp) when is_egen_self exp ->
      sassign_exp v_ev_next (arg exp)
  | SUnit(exp) when is_egen_port exp ->
    let port_exp, event_exp = unbox_egen_port exp in
    let port_exp = if (size_of_ty port_exp.ety < size_of_ty (tint port_size))
      then ecast (tint port_size) port_exp
      else port_exp
    in
    sseq (sassign_exp v_ev_out event_exp) (sassign_exp v_out_port port_exp)
  | SUnit(exp) when is_egen_switch exp ->
    let switch_exp, event_exp = unbox_egen_switch exp in
    let switch_exp = ecast (tint port_size) switch_exp in
    sseq (sassign_exp v_ev_out event_exp) (sassign_exp v_out_port switch_exp)
  | SUnit(exp) when is_egen_group exp ->
    let port_exp, event_exp = unbox_egen_port exp in
    sseq (sassign_exp v_ev_out event_exp) (sassign_exp v_out_port port_exp)
  | _ ->
    statement
;;
type handler_rec = {
  hcid : cid;
  hparams : params; 
  hbody : statement;
}

(* make the main handler -- value-semantic form: takes the event by value,
   returns (ev_next, ev_out, out_port) by value. The below-the-boundary `lower`
   pass converts this to the out-param calling convention. *)
let handler_ret_ty = ttuple [tevent; tevent; tint port_size]
let mk_main_handler handlers =
  (* ingress_port size is derived from mutable that is set in translation to CCore *)
  let ingress_port_param = (Cid.id Builtins.ingr_port_id), tint port_size in
  let in_ev_val_param = cid"ev_in", tevent in
  let branches = List.map
    (fun handler ->
      (* one branch for each handler *)
      let pats = [pevent handler.hcid handler.hparams] in
      (pats, subst_statement#visit_statement transform_generate handler.hbody))
    handlers
  in
  (* add a default no-op branch *)
  let branches = branches@[([PWild tevent], snoop)] in
  let merged_body = stmts [
    slocal (cid"ev_next") tevent (eval no_event);
    slocal (cid"ev_out") tevent (eval no_event);
    slocal (cid"out_port") (tint port_size) (eval (vint 0 port_size));
    smatch [v_ev_in] branches;
    sret (etuple [v_ev_next; v_ev_out; v_out_port]);
  ]
  in
  dfun handler_cid handler_ret_ty [ingress_port_param; in_ev_val_param] merged_body
;;

let transform_handler last_handler_cid (handlers, decls) decl : (handler_rec list * decls) = 
  match extract_dhandle_opt decl with 
  | None -> (handlers, decls@[decl]) (* not a handler, no change *)
  | Some(handler_cid, _, params, statement) ->
    (* a handler. update handlers list *)
    let handlers = handlers@[{hcid=handler_cid; hparams=params; hbody=statement}] in 
    if (Cid.equal handler_cid last_handler_cid) then (
      let handler_fun = mk_main_handler handlers in
      handlers, decls@[handler_fun]
    )
    else (* not the last handler, don't keep this handler decl *)
      handlers, decls
;;


let process decls = 
  (* get id of last handler -- that declaration will become the 
     merged handler *)
  let last_handler_cid = List.filter_map extract_dhandle_opt decls 
    |> List.map (fun (cid, _, _, _) -> cid)
    |> List.rev |> List.hd
  in 
  (* merge the handlers into 1 call/return by value event function *)
  let decls = List.fold_left (transform_handler last_handler_cid) ([], []) decls |> snd in

  (* finally, remove the declarations for builtin generate functions, since they're no longer needed *)
  let decls = List.filter 
    (fun decl -> 
      match decl.d with 
      | DFun(_, cid, _, _, BExtern) -> 
        (* if (Cid.to_id cid |> fst) is in ["generate"; "generate_port"; "generate_switch"; "generate_group"] *)
        if (List.mem (Cid.to_id cid |> fst) ["generate_self"; "generate_port"; "generate_switch"; "generate_group"]) then false else true
      | _ -> true)
    decls
  in
  decls
;;

(* ===== phase 2: lower the value-return handle_event to out-params ===== *)
(* the four event/port vars become out-params, accessed by dereference *)
(* exact-cid match (not by name) so we never touch a user variable that merely
   prints as e.g. "out_port" -- those are distinct (uniquified) cids. *)
let is_out_var c =
  List.exists (Cid.equal c) [cid"ev_in"; cid"ev_next"; cid"ev_out"; cid"out_port"]
let deref_out_vars = object
  inherit [_] s_map as super
  method! visit_exp () e =
    let e = super#visit_exp () e in
    match e.e with
    | EVar c when is_out_var c -> ederef (evar c (tref e.ety))
    | _ -> e
end

let lower decls =
  let out_params =
    [ (Cid.id Builtins.ingr_port_id), tint port_size;
      in_ev_param; next_ev_param; out_ev_param; out_port_param () ]
  in
  List.map
    (fun decl ->
      match decl.d with
      | DFun(_, cid, _, _, BStatement body) when Cid.equal cid handler_cid ->
        (* drop the output-local inits and the tuple return; the rest stays, with
           the output vars dereferenced (they are now out-params, pre-initialized
           by the driver). *)
        let body =
          to_stmt_block body
          |> List.filter (fun s -> match s.s with
            | SAssign(OLocal(c, _), _) when is_out_var c -> false
            | SRet _ -> false
            | _ -> true)
          |> stmts
        in
        let body = deref_out_vars#visit_statement () body in
        dfun handler_cid tunit out_params body
      | _ -> decl)
    decls
;;