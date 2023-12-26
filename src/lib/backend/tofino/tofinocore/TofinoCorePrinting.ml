(* printing for tofinocore (new) *)
open TofinoCore
open CorePrinting


let line_sep f itms = 
  List.map f itms |> String.concat "\n"
;;

let indent_body s = 
  String.split_on_char '\n' s
  |> List.map (fun s -> "  " ^ s)
  |> String.concat "\n"

(* print event types *)
let rec event_to_string (event:TofinoCore.event) =
  match event with
  | EventSingle({evid; evnum; evsort; evparams}) -> (
    Printf.sprintf
      "%s %s%s(%s);"
      (event_sort_to_string evsort)
      (id_to_string evid)
      (Batteries.Option.map_default (fun n -> "@" ^ string_of_int n) "" evnum)
      (params_to_string evparams)
  )
  | EventUnion({evid; members}) -> (
    Printf.sprintf
      "eventunion %s {\n%s\n};"
      (id_to_string evid)
      (line_sep event_to_string members |> indent_body)
      (* (Batteries.String.join " " (Batteries.List.map event_to_string members)) *)
  )
  | EventSet({evid; members}) -> (
    Printf.sprintf
      "eventset %s {\n%s\n};"
      (id_to_string evid)
      (line_sep event_to_string members |> indent_body)
      (* (Batteries.String.join " " (Batteries.List.map event_to_string members)) *)
  )
  | EventWithMetaParams({event=event; params=params;}) -> (
    Printf.sprintf
      "%s(%s);"
      (event_to_string event)
      (params_to_string params)
  )
;;

let intrinsic_param_to_string (id, ty, dirstr_opt) = 
  Printf.sprintf "%s %s %s" (Batteries.Option.default "" dirstr_opt) (ty_to_string ty) (id_to_string id) 
;;

let hevent_to_string h = 
  let hdl_sort_str = match h.hdl_sort with
    | HControl -> "control"
    | HData -> ""
    | HEgress -> "@egress"
  in
  let params = ( h.hdl_input, {raw_ty=TEvent; tspan=Span.default})::h.hdl_params in 
  let out_params = ( h.hdl_output, {raw_ty=TEvent; tspan=Span.default})::h.hdl_retparams in 
  let body_str = match h.hdl_body with 
    | SFlat stmt -> stmt_to_string stmt |> indent_body
    | SPipeline stmts -> 
      let stage_strs = List.map stmt_to_string stmts in
      let stage_strs = List.mapi
        (fun stage_num stage_str  -> 
          ("// Stage "^(string_of_int stage_num)^(" \n")^stage_str))
        stage_strs
      in
      String.concat "\n" stage_strs |> indent_body
  in
  Printf.sprintf
    "%shandle %s(%s)\ndeparse_locals(%s)\nundefined_locals(%s)\nreturns (%s)\nbody = {\n%s\n}\ndeparse={\n%s\n}"
    hdl_sort_str
    (id_to_string h.hdl_id)
    (params_to_string params)
    (params_to_string h.hdl_deparse_params)
    (params_to_string h.hdl_preallocated_vars)
    (params_to_string out_params)
    body_str
    (stmt_to_string h.hdl_deparse |> indent_body)
;;
    (* print handler types *)
let handler_to_string (handler:TofinoCore.handler) = 
  match handler with HParams({hdl_id; hdl_sort; hdl_params; hdl_body}) -> (
    Printf.sprintf
      "%shandle %s(%s) {\n%s\n}"
      (match hdl_sort with
       | HControl -> "control "
       | HData -> ""
       | HEgress -> "@egress ")
      (id_to_string hdl_id)
      (params_to_string hdl_params)
      (stmt_to_string hdl_body |> indent_body)
  )
  | HEvent(h) -> hevent_to_string h
;;

let parser_to_string p = 
  match p.pret_event with 
  | Some(pret_event) -> 
    Printf.sprintf
      "parser %s(%s) returns(event %s, %s)\n{\n%s\n}\n"
      (id_to_string p.pid)
      (params_to_string p.pparams)
      ( (pret_event) |> id_to_string)
      (params_to_string p.pret_params)
      (parser_block_to_string p.pblock |> indent_body)
  | None -> 
    Printf.sprintf
      "parser %s(%s)\n{\n%s\n}\n"
      (id_to_string p.pid)
      (params_to_string p.pparams)
      (parser_block_to_string p.pblock |> indent_body)
;;

(* group type: 
and group = {
  gnum : int; (* group number *)
  gcopies : (int * int) list; (* port, replica id *)
}  
*)
let group_to_string (group : group) = 
  Printf.sprintf
    "group %d {\n%s\n}"
    group.gnum
    (List.map (fun (port, replica) -> Printf.sprintf "  %d %d;" port replica) group.gcopies |> String.concat "\n")
let td_to_string (td:td)= 
  match td with
  | TDGlobal (id, ty, exp) ->
    Printf.sprintf
      "global %s %s = %s;\n"
      (ty_to_string ty)
      (id_to_string id)
      (exp_to_string exp)
  | TDHandler (handler) ->
    handler_to_string handler
  | TDEvent (event) ->
    event_to_string event
  | TDMemop { mid; mparams; mbody } ->
    Printf.sprintf
      "memop %s(%s)\n {%s}"
      (id_to_string mid)
      (params_to_string mparams)
      (memop_to_string mbody |> indent_body)
  | TDExtern (id, ty) ->
    Printf.sprintf "extern %s %s;" (id_to_string id) (ty_to_string ty)
  | TDActionConstr acn ->
    (* id, ret_tys, const_params, (dyn_params, acn_body)) ->  *)
    Printf.sprintf
      "action (%s) %s(%s)(%s) {\n\taction_return (%s)\n}\n"
      (comma_sep ty_to_string acn.artys)
      (id_to_string acn.aid)
      (params_to_string acn.aconst_params)
      (params_to_string acn.aparams)
      (comma_sep exp_to_string acn.abody |> indent_body)
  | TDParser (p) -> parser_to_string p
  | TDOpenFunction(id, params, statement) -> 
    Printf.sprintf
      "method %s(%s) {\n%s\n}\n"
      (id_to_string id)
      (params_to_string params)
      (stmt_to_string statement |> indent_body)  
  | TDMulticastGroup (group) -> group_to_string group  
  | TDFun(id, ty, body) -> Printf.sprintf
    "fun %s %s(%s){%s}"
      (ty_to_string ty)
      (id_to_string id)
      (params_to_string (fst body))
      (stmt_to_string (snd body))
  | TDUserTy(id, ty) -> 
    Printf.sprintf "type %s = %s;" (id_to_string id) (ty_to_string ty)

;;

let tdecl_to_string td = td_to_string td.td

let tdecls_to_string tds = String.concat "\n" (List.map tdecl_to_string tds)

let component_to_string component = 
  Printf.sprintf
    "component %s(successors = [%s]) {\n%s\n}\n"
    (id_to_string component.comp_id)
    (String.concat " , " (List.map id_to_string component.comp_succ))
    (tdecls_to_string component.comp_decls |> indent_body)
;;

let prog_to_string components = 
  String.concat "\n" (List.map component_to_string components)
;;



(* sanity checking: can we print the P4 for an event? *)


let param_to_p4field (id, ty) = 
  Printf.sprintf "%s: %s;"
    (Id.to_string id)
    (ty_to_string ty)
;;


let params_to_p4fields params = line_sep param_to_p4field params


(* the event's variable name is the events name *)
let rec  event_to_p4varname event : string =
  match event with
    | EventSingle({evid;}) -> (
      (Id.to_string evid)
    )
    | EventUnion({evid;}) ->(
      (Id.to_string evid)
    )
    | EventSet({evid;}) -> (
      (Id.to_string evid)
    )
    | EventWithMetaParams({event;}) -> (
      event_to_p4varname event
    )
;;  
let rec event_to_p4tyname event : string = 
  match event with
    | EventSingle({evid;}) -> (
      (Id.to_string evid) ^ "_t"
    )
    | EventUnion({evid;}) ->(
      (Id.to_string evid) ^ "_t"
    )
    | EventSet({evid;}) -> (
      (Id.to_string evid) ^ "_t"
    )
    | EventWithMetaParams({event;}) -> (
      event_to_p4tyname event
    )
;;

let event_to_p4fieldstr event : string =
  Printf.sprintf "%s : %s;"
    (event_to_p4varname event)
    (event_to_p4tyname event)
;;

let rec event_to_p4flag event : string = 
  match event with
  | EventSingle{evid;} -> 
    Id.prepend_string "flag_" (evid) |> Id.to_string
  | EventUnion{evid;} -> 
    Id.prepend_string "flag_" (evid) |> Id.to_string
  | EventSet{evid;} -> 
    Id.prepend_string "flag_" (evid) |> Id.to_string
  | EventWithMetaParams{event;} -> 
    event_to_p4flag event
  ;;

let event_to_p4flagfield event : string =
  event_to_p4flag event ^ " : " ^ "int<1>;"
;;

let rec event_to_struct_body event : string = 
  match event with
  | EventSingle({evparams}) -> 
    Printf.sprintf "%s" (params_to_p4fields evparams)
  | EventUnion({members}) -> 
    Printf.sprintf "%s\n%s"
      ("tag : int<8>;")
      (line_sep event_to_p4fieldstr members)
  | EventSet({members}) -> 
    Printf.sprintf "%s\n%s"
      (line_sep event_to_p4flagfield members)
      (line_sep event_to_p4fieldstr members)
  | EventWithMetaParams({event; params;}) -> 
    event_to_struct_body event^"\n"^(params_to_p4fields params)

let rec event_to_p4tystr event : string = 
  match event with
  (* an event declaration is a type declaration *)
  | EventSingle({evid;}) -> (
    (* a single event -- just a struct or header *)
    Printf.sprintf 
      "struct %s_t {\n%s\n}"
      (Id.to_string evid)
      (event_to_struct_body event)
  )
  (* an event union... we have to print out the members, then 
     print a union of those members... and thhe tag? *)
  | EventUnion({evid; members}) -> (
    (line_sep event_to_p4tystr members) ^ "\n"
    ^ 
    Printf.sprintf 
      "struct %s_t {\n%s\n}"
      (Id.to_string evid)
      (event_to_struct_body event)
  )
  | EventSet({evid; members}) -> (    
    (line_sep event_to_p4tystr members) ^ "\n"
    ^ 
    Printf.sprintf 
      "struct %s_t {\n%s\n}"
      (Id.to_string evid)
      (event_to_struct_body event)
  )
  | EventWithMetaParams({event; _;}) -> (
    Printf.sprintf 
      "struct %s_t {\n%s\n}"
      (Id.to_string (id_of_event event))
      (event_to_struct_body event)
  )
;;