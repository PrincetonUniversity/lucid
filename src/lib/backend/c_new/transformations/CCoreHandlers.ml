(* 
  Transform each handler foo(<params>) into a function from an event to a record with a 
  next event, an output event, and an output port.
    - The function to handle event $ev is called handle$ev
    - The function takes an event as an argument
    - The function returns a record of type handler_ret_t, which 
      contains the next event, the output event, and the output port.
    - The first statment of the function declares a variable rv of type handler_ret_t
    - The function then matches on the input event, and runs the handler if the input event
      matches the event the function is handling.
    - The function returns rv
    The transformation assumes that each handler only ever returns at most 1 next event 
    and 1 out event (i.e., 1 generate call and 1 generate_port call). 
    
  Then, in a second pass, we transform the event functions into functions 
  that operate by reference: the input event and output struct are pointers.    
    
*)
open CCoreSyntax
open CCorePPrint
open CCoreTransformers

let id = Id.create

(* input argument for all events *)
let in_event_param_id = id"ev"
let in_event_param = (in_event_param_id, tevent)
let in_event = param_evar in_event_param

(* return type and variable for all events *)
let t_handler_ret = 
  tabstract "handler_ret_t"
  (trecord 
    [id"next_ev"; id"out_ev"; id"out_port"]
    [tevent; tevent; tint 32])
let rv_id = id"rv";;
let rv = evar (Cid.id rv_id) t_handler_ret



let transform_generate statement = 
  (* instead of generating the event, set the appropriate event variable *)
  match statement.s with 
  | SUnit(exp) when is_egen_self exp -> 
    print_endline ("generate self?");
    sassign_exp (rv/->"next_ev") (arg exp)
  | SUnit(exp) when is_egen_port exp -> 
    print_endline ("generate port?");
    let port_exp, event_exp = unbox_egen_port exp in
    sseq 
      (sassign_exp (rv/->"out_ev") event_exp)
      (sassign_exp (rv/->"out_port") port_exp)
  | _ -> 
    statement
;;

let slocal_rv = slocal 
    (Cid.id rv_id) 
    t_handler_ret
    (default_value t_handler_ret |> eval)
;;
let transform_handler decl = 
  match extract_dhandle_opt decl with 
  | None -> decl
  | Some(ev_cid, ty, params, statement) ->
    let hdl_cid = Cid.str_cons_plain "handle" ev_cid in
    print_endline ("transforming handler: "^(CCorePPrint.cid_to_string ev_cid));
    print_endline ("return type: "^(ty_to_string ty));
    let statement' = 
      stmts [
        slocal_rv; (* rv == default_of_ty(ty(rv)); *)
        (smatch (* match on input event parameter, run handler or noop *)
          [in_event]
          [
            ([pevent (Cid.id in_event_param_id) params], 
              subst_statement#visit_statement transform_generate statement);
            ([pevent (Cid.id default_event_id) []], snoop) 
          ]);
        sret rv (* return the return value *)
      ]
    in
    dfun hdl_cid t_handler_ret [in_event_param] statement'
;;

let add_default_event_if_not_exists decls = 
  (* check if it exists *)
  let default_found = List.exists is_default_event_decl decls in
  if default_found then decls else 
    default_event_decl::decls
;;

(* turn an evar that refs one of the params 
  into a dref expression *)
let globalize_evars param_ids exp = 
  let var_id, _ = extract_evar_id exp in
  if (List.exists (Id.equal var_id) param_ids) then 
    to_global exp
  else 
    exp
;;
let globalize_ret_var retvar_id exp = 
  let var_id, _ = extract_evar_id exp in
  if (Id.equal var_id retvar_id) then
    to_global exp
  else 
    exp 
;;

(* transform a function to call by reference instead of call by value. 
    assumptions: 
      first statement declares a return var
        - the declaration initializes it to a value that doesn't matter
      last statement returns the return var
*)
let value_args_to_ref_args decl = 
  match extract_dfun_opt decl with
  | Some(fun_cid, ret_ty, params, statement) -> (
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
          (id, tglobal ty)
        ) 
        (params@[(Cid.to_id ret_var_id), ret_ty])
      in
      dfun fun_cid (tunit ()) new_params statement
    | _ -> decl
  )
  | _ -> decl
;;


let process_decls decls = 
  subst_decl#visit_decls transform_handler decls 
  |> add_default_event_if_not_exists (* we need the default event now *)
  |> subst_decl#visit_decls value_args_to_ref_args
;;