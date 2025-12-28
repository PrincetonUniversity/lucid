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

let in_ev_param = cid"ev_in", tref tevent
let next_ev_param = cid"ev_next", tref tevent
let out_ev_param = cid"ev_out", tref tevent
let out_port_param () = cid"out_port", tref@@tint (CConfig.c_cfg.port_id_size) (* port for out event, 0 means no out event *)
let ev_in = param_evar in_ev_param
let ev_next = param_evar next_ev_param 
let ev_out = param_evar out_ev_param
let out_port () = param_evar (out_port_param ())

(* transform a generate statement in the body of the event handler function *)
let transform_generate statement = 
  let out_port = out_port () in
  (* instead of generating the event, set the appropriate event variable *)
  match statement.s with 
  | SUnit(exp) when is_egen_self exp -> 
      (sassign_exp (ederef ev_next) (arg exp))
  | SUnit(exp) when is_egen_port exp -> 
    let port_exp, event_exp = unbox_egen_port exp in
    let port_exp_size = size_of_ty port_exp.ety in
    let out_port_size = size_of_ty (extract_tref out_port.ety) in
    let port_exp = if (port_exp_size < out_port_size)
      then ecast (extract_tref out_port.ety) port_exp 
      else port_exp
    in
    sseq 
      (sassign_exp (ederef ev_out) event_exp)
      (sassign_exp (ederef out_port) port_exp)
  | SUnit(exp) when is_egen_switch exp ->
    let switch_exp, event_exp = unbox_egen_switch exp in
    (* at this point, switch is just another name for generate_port with a different type *)
    let switch_exp = ecast (extract_tref out_port.ety) switch_exp in
    sseq 
      (sassign_exp (ederef ev_out) event_exp)
      (sassign_exp (ederef out_port) switch_exp)
  | SUnit(exp) when is_egen_group exp ->
    (* treat group the same as port *)
    let port_exp, event_exp = unbox_egen_port exp in
    sseq 
      (sassign_exp (ederef ev_out) event_exp)
      (sassign_exp (ederef out_port) port_exp)
  | _ -> 
    statement
;;
type handler_rec = {
  hcid : cid;
  hparams : params; 
  hbody : statement;
}

(* make the main handler *)
let mk_main_handler handlers = 
  (* ingress_port size is derived from mutable that is set in translation to CCore *)
  let ingress_port_param = (Cid.id Builtins.ingr_port_id), tint CConfig.c_cfg.port_id_size in
  let out_port_param = out_port_param () in
  let branches = List.map 
    (fun handler -> 
      (* one branch for each handler *)
      let pats = [pevent handler.hcid handler.hparams] in
      (pats, subst_statement#visit_statement transform_generate handler.hbody))
    handlers
  in  
  (* add a default no-op branch *)
  (* we're matching on a pointer to an event *)
  let branches = branches@[([PWild (extract_tref ev_in.ety)], snoop)] in
  let merged_body = stmts [
    smatch [ederef ev_in] branches]
  in
  dfun handler_cid (tunit) [ingress_port_param; in_ev_param; next_ev_param; out_ev_param; out_port_param] merged_body
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