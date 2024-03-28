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

(* input argument for all handlers *)
let in_event_param_id = id"ev"
let in_event_param = (in_event_param_id, tevent)
let in_event = param_evar in_event_param

(* return type and variable for all handlers *)
let t_handler_ret = 
  tabstract "handler_ret_t"
  (trecord 
    [id"next_ev"; id"out_ev"; id"out_port"]
    [tevent; tevent; tint 32])
let rv_id = id"rv";;
let rv = evar (Cid.id rv_id) t_handler_ret

let merged_handler_cid = Cid.create ["event_handler"] ;;


(* transform a generate statement in the body of the event handler function *)
let transform_generate statement = 
  (* instead of generating the event, set the appropriate event variable *)
  match statement.s with 
  | SUnit(exp) when is_egen_self exp -> 
    sassign_exp (rv/.id"next_ev") (arg exp)
  | SUnit(exp) when is_egen_port exp -> 
    let port_exp, event_exp = unbox_egen_port exp in
    sseq 
      (sassign_exp (rv/.id"out_ev") event_exp)
      (sassign_exp (rv/.id"out_port") port_exp)
  | _ -> 
    statement
;;

let slocal_rv = slocal 
    (Cid.id rv_id) 
    t_handler_ret
    (default_value t_handler_ret |> eval)
;;

type handler_rec = {
  hcid : cid;
  hparams : params; 
  hbody : statement;
}

let transform_handler last_handler_cid (handlers, decls) decl : (handler_rec list * decls) = 
  match extract_dhandle_opt decl with 
  | None -> (handlers, decls@[decl]) (* not a handler, no change *)
  | Some(handler_cid, _, params, statement) ->
    (* a handler. update handlers list *)
    let handlers = handlers@[{hcid=handler_cid; hparams=params; hbody=statement}] in 
    if (Cid.equal handler_cid last_handler_cid) then (
      (* if this is the last handler, replace with merged handler *)
      let branches = List.map 
        (fun handler -> 
          (* one branch for each handler *)
          let pats = [pevent handler.hcid handler.hparams] in
          (pats, subst_statement#visit_statement transform_generate handler.hbody))
        handlers
      in
      (* add a default no-op branch *)
      let branches = branches@[([PWild (in_event.ety)], snoop)] in
      let merged_body = stmts [
        slocal_rv;
        smatch [in_event] branches;
        sret rv ]
      in
      let merged_decl = 
        dfun merged_handler_cid t_handler_ret [in_event_param] merged_body
      in
      handlers, decls@[decl_tabstract t_handler_ret; merged_decl]
    )
    else (* not the last handler, don't keep this handler decl *)
      handlers, decls
;;

(* DEPRECIATED *)
(* let add_default_event_if_not_exists decls = 
  (* check if it exists *)
  let default_found = List.exists is_default_event_decl decls in
  if default_found then decls else 
    default_event_decl::decls
;; *)

(* turn an evar that refs one of the params 
  into a dref expression *)
let globalize_evars param_ids exp = 
  let var_id, _ = extract_evar_id exp in
  if (List.exists (Id.equal var_id) param_ids) then 
    to_ref exp
  else 
    exp
;;

(* transform a function to call by reference instead of call by value. 
    assumptions: 
      first statement declares a return var
        - the declaration initializes it to a value that doesn't matter
      last statement returns the return var
*)
let val_to_ref_args target_functions_cids decl = 
  match extract_dfun_opt decl with
  | Some(fun_cid, ret_ty, params, statement) -> (
    (* only apply to the target functions *)
    if (not (List.mem fun_cid target_functions_cids)) then decl else 
    match (to_stmt_block statement) with 
    (* drop first statement (assumed to be ret var declaration)
       drop last statement (assumed to be return statement) *)
    | [slocal_rv; inner_statement; _] -> 
      let ret_var_id = match slocal_rv.s with 
        | SAssign(OLocal(cid, _), _) -> cid
        | _ -> failwith "cannot transform function into call-by-reference: first statement does not declare the return variable"
      in
      let param_ids = List.map (fun (id, _) -> id) params in
      let statement = subst_evar#visit_statement (globalize_evars (rv_id::param_ids)) inner_statement in
      let new_params = List.map 
        (fun (id, ty) -> 
          (id, tref ty)
        ) 
        (params@[(Cid.to_id ret_var_id), ret_ty])
      in
      dfun fun_cid (tunit) new_params statement
    | _ -> decl
  )
  | _ -> decl
;;

let process_decls decls = 
  (* get id of last handler -- that declaration will become the 
     merged handler *)
  let last_handler_cid = List.filter_map extract_dhandle_opt decls 
    |> List.map (fun (cid, _, _, _) -> cid)
    |> List.rev |> List.hd
  in 
  (* merge the handlers into 1 call/return by value event function *)
  let decls = List.fold_left (transform_handler last_handler_cid) ([], []) decls in

  let decls = snd decls
    |> subst_decl#visit_decls (val_to_ref_args [merged_handler_cid]) (* convert the function to call by reference *)
  in
  (* finally, remove the declarations for builtin generate functions, since they're no longer needed *)
  let decls = List.filter 
    (fun decl -> 
      match decl.d with 
      | DFun(_, cid, _, _, None) -> 
        (* if (Cid.to_id cid |> fst) is in ["generate"; "generate_port"; "generate_switch"; "generate_group"] *)
        if (List.mem (Cid.to_id cid |> fst) ["generate_self"; "generate_port"; "generate_switch"; "generate_group"]) then false else true
      | _ -> true)
    decls
  in
  decls
;;