(*
  Merge event handlers into a single function. 
    - input: an event
    - output: a fixed-capacity array of out_events + the number produced
    - has one branch for each handler
    - generate is implemented by appending an out_event to the output array:
        out_events[n] = { ev; port }; n++;
      where `port` is the egress port for generate_port / generate_switch, 
      and a special PORT_RECIRC (0xFFFFFFFF) for generate_self, which 
      indicates recirculation. 
    - out event buffer overflow is not currently checked. (TODO)
*)
open CCoreSyntax
open CCorePPrint
open CCoreTransformers

let id = Id.create

let port_size = CConfig.c_cfg.port_id_size
let port_recirc = (1 lsl port_size) - 1  (* all-ones sentinel: 0xFFFFFFFF at port_size=32 *)
let out_event_cid = Cid.create ["out_event_t"]
let out_event_def =
  trecord [ cid"ev", tevent; cid"port", tint port_size ]
let tout_event = tabstract_cid out_event_cid out_event_def
let out_event_dty = dty out_event_cid out_event_def

let out_events_cap = 64
let tout_events = tlist tout_event out_events_cap
let count_size = 16

(* shared cids / vars for the handler body *)
let ev_in_cid = cid"ev_in"
let out_events_cid = cid"out_events"
let n_cid = cid"n"
let v_ev_in = evar ev_in_cid tevent
let v_out_events = evar out_events_cid tout_events
let v_n = evar n_cid (tint count_size)

let handler_cid = Cid.create ["handle_event"] ;;
let handler_ret_ty = tint count_size

(* append one out_event: out_events[n] = { ev; port }; n = n + 1; *)
let append_out_event ev_exp port_exp =
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
      append_out_event (arg exp) (eval (vint port_recirc port_size))
  | SUnit(exp) when is_egen_port exp ->
    let port_exp, event_exp = unbox_egen_port exp in
    let port_exp = if (size_of_ty port_exp.ety < size_of_ty (tint port_size))
      then ecast (tint port_size) port_exp
      else port_exp
    in
    append_out_event event_exp port_exp
  | SUnit(exp) when is_egen_switch exp ->
    let switch_exp, event_exp = unbox_egen_switch exp in
    let switch_exp = ecast (tint port_size) switch_exp in
    append_out_event event_exp switch_exp
  | SUnit(exp) when is_egen_group exp -> 
    failwith "compiler bug: generate_ports not supported"
  | _ -> statement
;;
type handler_rec = {
  hcid : cid;
  hparams : params;
  hbody : statement;
}

(* rewrite builtins that are implemented as reads from event metadata 
   (Sys.time() and ingress_port) *)
let replace_meta_builtins ev_exp = object
  inherit [_] s_map as super
  method! visit_exp () e =
    let e = super#visit_exp () e in
    match e.e with
    | ECall{f = {e = EVar cid; _}; args = []; _} when Cid.equal cid System.sys_time_cid ->
      event_timestamp ev_exp
    | EVar cid when Cid.equal cid (Cid.Id Builtins.ingr_port_id) -> event_in_port ev_exp
    | _ -> e
end

let mk_main_handler handlers =
  let in_ev_val_param = ev_in_cid, tevent in
  let out_events_param = out_events_cid, tout_events in
  let branches = List.map
    (fun handler ->
      let pats = [pvariant handler.hcid handler.hparams] in
      (pats, subst_statement#visit_statement transform_generate handler.hbody))
    handlers
  in
  (* add a default no-op branch *)
  let branches = branches@[([PWild tevent_variant], snoop)] in
  let merged_body = stmts [
    slocal n_cid (tint count_size) (eval (vint 0 count_size));
    smatch [event_data v_ev_in] branches;
    sret v_n;
  ]
  in
  let merged_body = (replace_meta_builtins v_ev_in)#visit_statement () merged_body in
  dfun handler_cid handler_ret_ty
    [in_ev_val_param; out_events_param] merged_body
;;

let process decls =
  let handlers = List.filter_map extract_dhandle_opt decls
    |> List.map (fun (cid, _, params, body) -> {hcid=cid; hparams=params; hbody=body})
  in
  let decls = List.filter (fun d -> not (is_dhandler d)) decls in
  decls @ [out_event_dty; mk_main_handler handlers]
;;

(* ev_in becomes a by-ref param (events ptr) *)
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
        let params = List.map
          (fun (c, ty) -> if Cid.equal c ev_in_cid then (c, tref tevent) else (c, ty))
          params
        in
        let body = deref_in_var#visit_statement () body in
        dfun_kind fun_kind handler_cid ret_ty params body
      | _ -> decl)
    decls
;;