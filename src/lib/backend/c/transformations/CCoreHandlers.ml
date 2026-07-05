(*
  Construct the event handling function.
    - input: an event (by value above the boundary, by ref below)
    - output: a fixed-capacity array of out_events + the number produced
    - has one branch for each handler
    - generate is implemented by appending an out_event to the output array:
        out_events[n] = { ev; out_loc; port }; n++;
      where out_loc is 1 (recirc) for generate_self and 2 (port) for
      generate_port / generate_switch / generate_group. The driver queues
      recirc events and deparses+sends port events.
    - there is NO per-control-flow restriction on how many times generate may be
      called: each call simply appends another out_event (up to the static
      capacity; we deliberately emit no runtime overflow guard -- a future static
      pass can bound the max generates per program).
*)
open CCoreSyntax
open CCorePPrint
open CCoreTransformers

let id = Id.create

let handler_cid = Cid.create ["handle_event"] ;;

let port_size = CConfig.c_cfg.port_id_size

(* ----- the out_event record: the unit the handler produces ----- *)
(* out_loc tells the driver where each produced event goes. *)
let loc_none   = 0  (* unused slot                                   *)
let loc_recirc = 1  (* feed back into the dispatch queue (generate_self) *)
let loc_port   = 2  (* deparse + send out `port` (generate_port/switch/group) *)

let out_event_cid = Cid.create ["out_event"]
let out_event_def =
  trecord [ cid"ev", tevent; cid"out_loc", tint 8; cid"port", tint port_size ]
let tout_event = tabstract_cid out_event_cid out_event_def
let out_event_dty = dty out_event_cid out_event_def

(* the handler writes into a fixed-capacity array of out_events (an in-place
   mutable aggregate, allowed at the value-semantics boundary like any array) and
   returns the count. No overflow guard is generated (see header). *)
let out_events_cap = 64
let tout_events = tlist tout_event (IConst out_events_cap)

(* the count return type *)
let count_size = 16

(* shared cids / vars for the handler body *)
let ingr_cid = Cid.id Builtins.ingr_port_id
let ev_in_cid = cid"ev_in"
let out_events_cid = cid"out_events"
let n_cid = cid"n"
let v_ev_in = evar ev_in_cid tevent
let v_out_events = evar out_events_cid tout_events
let v_n = evar n_cid (tint count_size)

(* append one out_event: out_events[n] = { ev; out_loc=loc; port }; n = n + 1; *)
let mk_push loc ev_exp port_exp =
  let rec_exp =
    { (erecord [ cid"ev", ev_exp;
                 cid"out_loc", eval (vint loc 8);
                 cid"port", port_exp ])
      with ety = tout_event }
  in
  sseq
    ((v_out_events, v_n) /<- rec_exp)
    (sassign_exp v_n (v_n /+ eval (vint 1 count_size)))
;;

(* transform a generate statement into an out_event append. *)
let transform_generate statement =
  match statement.s with
  | SUnit(exp) when is_egen_self exp ->
      (* recirculation: port is irrelevant (the driver re-queues by event) *)
      mk_push loc_recirc (arg exp) (eval (vint 0 port_size))
  | SUnit(exp) when is_egen_port exp ->
    let port_exp, event_exp = unbox_egen_port exp in
    let port_exp = if (size_of_ty port_exp.ety < size_of_ty (tint port_size))
      then ecast (tint port_size) port_exp
      else port_exp
    in
    mk_push loc_port event_exp port_exp
  | SUnit(exp) when is_egen_switch exp ->
    let switch_exp, event_exp = unbox_egen_switch exp in
    let switch_exp = ecast (tint port_size) switch_exp in
    mk_push loc_port event_exp switch_exp
  (* generate_ports (multicast, egen_group) is rejected up front by
     CCoreWellformedC.feature_gate -- the single-port out_event model can't fan out to a
     group -- so it never reaches here. (It used to be silently mis-compiled as a
     single-port generate with the group id as the port.) *)
  | _ ->
    statement
;;
type handler_rec = {
  hcid : cid;
  hparams : params;
  hbody : statement;
}

(* Sys.time() -> a read of the current event's meta.timestamp. The driver writes that
   stamp when it dequeues the event for handling (so it covers both arriving packets and
   recirculated events). The frontend builtin lowers to a 0-arg call of System.sys_time_cid;
   we rewrite each such call to `<ev>.meta.timestamp`. *)
let replace_sys_time ev_exp = object
  inherit [_] s_map as super
  method! visit_exp () e =
    let e = super#visit_exp () e in
    match e.e with
    | ECall{f = {e = EVar cid; _}; args = []; _} when Cid.equal cid System.sys_time_cid ->
      event_timestamp ev_exp
    | _ -> e
end

(* ingress_port -> a read of the current event's meta.in_port (set by the driver at
   RX, inherited by recirculated events). Mirrors replace_sys_time, so the handler
   needs no ingress_port parameter -- it reads its ingress from the event. *)
let replace_ingress_port ev_exp = object
  inherit [_] s_map as super
  method! visit_exp () e =
    let e = super#visit_exp () e in
    match e.e with
    | EVar cid when Cid.equal cid ingr_cid -> event_in_port ev_exp
    | _ -> e
end

(* make the main handler -- value-semantic form: takes the event by value and a
   fixed-capacity out_events array, fills the array and returns the count. The
   below-the-boundary `lower` pass converts ev_in to a by-ref param. *)
let handler_ret_ty = tint count_size
let mk_main_handler handlers =
  let in_ev_val_param = ev_in_cid, tevent in
  let out_events_param = out_events_cid, tout_events in
  let branches = List.map
    (fun handler ->
      (* one branch for each handler *)
      let pats = [pvariant handler.hcid handler.hparams] in
      (pats, subst_statement#visit_statement transform_generate handler.hbody))
    handlers
  in
  (* add a default no-op branch *)
  let branches = branches@[([PWild tevent_variant], snoop)] in
  let merged_body = stmts [
    slocal n_cid (tint count_size) (eval (vint 0 count_size));
    (* match on the incoming event's variant (envelope's .data) *)
    smatch [event_data v_ev_in] branches;
    sret v_n;
  ]
  in
  (* Sys.time() -> v_ev_in.meta.timestamp; ingress_port -> v_ev_in.meta.in_port
     (both set by the driver: timestamp at dequeue, in_port at RX). So the handler
     reads them from the event and takes no ingress_port parameter. *)
  let merged_body = (replace_sys_time v_ev_in)#visit_statement () merged_body in
  let merged_body = (replace_ingress_port v_ev_in)#visit_statement () merged_body in
  dfun handler_cid handler_ret_ty
    [in_ev_val_param; out_events_param] merged_body
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
  (* merge the handlers into 1 array-filling event function *)
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
  (* declare the out_event type after the `events` type it embeds (and ahead of
     the handler that uses it). *)
  let is_events_dty d = match d.d with
    | DTy(cid, _) -> Cid.equal cid events_cid
    | _ -> false
  in
  if List.exists is_events_dty decls then
    List.concat_map
      (fun d -> if is_events_dty d then [d; out_event_dty] else [d])
      decls
  else
    out_event_dty :: decls  (* no events decl found (shouldn't happen): front *)
;;

(* ===== phase 2: lower the value-semantic handle_event to the driver ABI ===== *)
(* ev_in becomes a by-ref param (events ptr); the out_events array param decays to a
   pointer for free (lower_vecs); the count return stays. The only rewrite needed
   is dereferencing uses of ev_in. exact-cid match (not by name) so we never touch
   a user variable that merely prints as "ev_in". *)
let deref_in_var = object
  inherit [_] s_map as super
  method! visit_exp () e =
    let e = super#visit_exp () e in
    match e.e with
    | EVar c when Cid.equal c ev_in_cid -> ederef (evar c (tref e.ety))
    | _ -> e
end

let lower decls =
  List.map
    (fun decl ->
      match decl.d with
      | DFun(fun_kind, cid, ret_ty, params, BStatement body) when Cid.equal cid handler_cid ->
        (* ev_in: by value -> by reference, derefing its uses *)
        let params = List.map
          (fun (c, ty) -> if Cid.equal c ev_in_cid then (c, tref tevent) else (c, ty))
          params
        in
        let body = deref_in_var#visit_statement () body in
        dfun_kind fun_kind handler_cid ret_ty params body
      | _ -> decl)
    decls
;;