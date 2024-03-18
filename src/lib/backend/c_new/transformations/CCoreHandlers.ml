(* 
  1. move generates to the end of the handler, 
     making each event generate 1 event to local and 1 event to a port
  transform handlers into event functions.
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
  (* | SUnit(exp) -> 
    print_endline ("unit exp: "^(CCorePPrint.exp_to_string exp));
    statement *)
  | _ -> 
    (* print_endline ("NOT A UNIT: ");
    print_endline (CCorePPrint.statement_to_string statement); *)
    statement
;;

let slocal_rv = slocal 
    (Cid.id rv_id) 
    t_handler_ret
    (default_value t_handler_ret |> eval)
;;
(* all the event functions return a default pattern that returns the return variable. 
let default_pat = 
 *)

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
    dfun hdl_cid tevent [in_event_param] statement'
;;

let rec add_default_event_if_not_exists default_found decls = 
  match decls with 
  | [] -> if (default_found) then [] else [default_event_decl]
  | decl::decls -> 
      decl::(add_default_event_if_not_exists 
        (default_found || is_default_event_decl decl)
        decls)
;;

let process_decls decls = 
  subst_decl#visit_decls transform_handler decls 
  |> add_default_event_if_not_exists false (* we need the default event now *)
;;