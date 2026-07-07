(*
  Construct the event handling function.
    - input: an event (by value above the boundary, by ref below)
    - output: a fixed-capacity array of out_events + the number produced
    - has one branch for each handler
    - generate is implemented by appending an out_event to the output array:
        out_events[n] = { ev; port }; n++;
      where `port` is the egress port for generate_port / generate_switch, and the
      sentinel PORT_RECIRC (0xFFFFFFFF) for generate_self. The driver re-queues
      recirc events (port == PORT_RECIRC) and deparses+sends the rest to `port`.
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
(* `port` is the egress port, or PORT_RECIRC for a recirculated (generate_self)
   event -- the driver routes on this single field (no separate out_loc). *)
let port_recirc = (1 lsl port_size) - 1  (* all-ones sentinel: 0xFFFFFFFF at port_size=32 *)

let out_event_cid = Cid.create ["out_event_t"]
let out_event_def =
  trecord [ cid"ev", tevent; cid"port", tint port_size ]
let tout_event = tabstract_cid out_event_cid out_event_def
let out_event_dty = dty out_event_cid out_event_def

(* the handler's output: a fixed-capacity array of out_events plus a count
   (no overflow guard -- see header) *)
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

(* append one out_event: out_events[n] = { ev; port }; n = n + 1; *)
let mk_push ev_exp port_exp =
  let rec_exp =
    { (erecord [ cid"ev", ev_exp;
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
      (* recirculation: the sentinel port tells the driver to re-queue by event *)
      mk_push (arg exp) (eval (vint port_recirc port_size))
  | SUnit(exp) when is_egen_port exp ->
    let port_exp, event_exp = unbox_egen_port exp in
    let port_exp = if (size_of_ty port_exp.ety < size_of_ty (tint port_size))
      then ecast (tint port_size) port_exp
      else port_exp
    in
    mk_push event_exp port_exp
  | SUnit(exp) when is_egen_switch exp ->
    let switch_exp, event_exp = unbox_egen_switch exp in
    let switch_exp = ecast (tint port_size) switch_exp in
    mk_push event_exp switch_exp
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

(* rewrite the ambient builtins to reads of the current event's meta, which the
   driver stamps: Sys.time() -> <ev>.meta.timestamp (written at dequeue) and
   ingress_port -> <ev>.meta.in_port (written at RX, inherited by recirculated
   events). So the handler takes no extra parameters for either. *)
let replace_meta_builtins ev_exp = object
  inherit [_] s_map as super
  method! visit_exp () e =
    let e = super#visit_exp () e in
    match e.e with
    | ECall{f = {e = EVar cid; _}; args = []; _} when Cid.equal cid System.sys_time_cid ->
      event_timestamp ev_exp
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
  let merged_body = (replace_meta_builtins v_ev_in)#visit_statement () merged_body in
  dfun handler_cid handler_ret_ty
    [in_ev_val_param; out_events_param] merged_body
;;

let process decls =
  (* merge all handlers into one array-filling event function. Everything the
     merged handler references precedes the handlers, so it (and the out_event
     type it uses) can simply go at the end. *)
  let handlers = List.filter_map extract_dhandle_opt decls
    |> List.map (fun (cid, _, params, body) -> {hcid=cid; hparams=params; hbody=body})
  in
  let is_generate_extern decl = match decl.d with
    | DFun(_, cid, _, _, BExtern) ->
      List.mem (Cid.to_id cid |> fst)
        ["generate_self"; "generate_port"; "generate_switch"; "generate_group"]
    | _ -> false
  in
  (* drop the handler decls and the now-unreferenced generate externs *)
  let decls = List.filter
    (fun d -> not (is_dhandler d) && not (is_generate_extern d))
    decls
  in
  decls @ [out_event_dty; mk_main_handler handlers]
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