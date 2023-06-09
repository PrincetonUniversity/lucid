(* printing for tofinocore (new) *)
open TofinoCoreNew
open CorePrinting

(* print event types *)
let rec event_to_string (event:TofinoCoreNew.event) =
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
      "%s { %s };"
      (id_to_string evid)
      (Batteries.String.join " | " (Batteries.List.map event_to_string members))
  )
  | EventSet({evid; members}) -> (
    Printf.sprintf
      "%s { %s };"
      (id_to_string evid)
      (Batteries.String.join ", " (Batteries.List.map event_to_string members))
  )
  | _ -> failwith "not done!"


(* print handler types *)
let handler_to_string (handler:TofinoCoreNew.handler) = 
  match handler with HParams({hdl_id; hdl_sort; hdl_params; hdl_body}) -> (
    Printf.sprintf
      "%shandle %s(%s) {\n%s\n}"
      (match hdl_sort with
       | HControl -> "control "
       | HData -> ""
       | HEgress -> "@egress ")
      (id_to_string hdl_id)
      (params_to_string hdl_params)
      (stmt_to_string hdl_body)
  )
  | HEvent({hdl_id; hdl_sort; hdl_body; hdl_input; hdl_output}) -> (
    Printf.sprintf
      "%shandle %s : %s -> %s \n {\n%s\n}"
      (match hdl_sort with
       | HControl -> "control "
       | HData -> ""
       | HEgress -> "@egress ")
      (id_to_string hdl_id)
      (event_to_string hdl_input)
      (event_to_string hdl_output)
      (match hdl_body with 
      | [] -> ""
      | [stmt] -> stmt_to_string stmt
      | stmts -> String.concat "\n" (List.map stmt_to_string stmts))
  )
;;

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
      (memop_to_string mbody)
  | TDExtern (id, ty) ->
    Printf.sprintf "extern %s %s;" (id_to_string id) (ty_to_string ty)
  | TDAction acn ->
    (* id, ret_tys, const_params, (dyn_params, acn_body)) ->  *)
    Printf.sprintf
      "action (%s) %s(%s)(%s) {\n\taction_return (%s)\n}\n"
      (comma_sep ty_to_string acn.artys)
      (id_to_string acn.aid)
      (params_to_string acn.aconst_params)
      (params_to_string acn.aparams)
      (comma_sep exp_to_string acn.abody)
  | TDParser (id, params, parser) ->
    Printf.sprintf
      "parser %s(%s) {\n%s\n}\n"
      (id_to_string id)
      (params_to_string params)
      (parser_block_to_string parser)
  | TDVar (id, ty) ->
    Printf.sprintf "sharedlocal %s %s;" (ty_to_string ty) (id_to_string id)
  | TDOpenFunction(id, params, statement) -> 
    Printf.sprintf
      "method %s(%s) {\n%s\n}\n"
      (id_to_string id)
      (params_to_string params)
      (stmt_to_string statement)    
;;

let tdecl_to_string td = td_to_string td.td

let tdecls_to_string tds = String.concat "\n" (List.map tdecl_to_string tds)

let component_to_string component = 
  Printf.sprintf
    "component %s(successors = [%s]) {\n%s\n}\n"
    (id_to_string component.comp_id)
    (String.concat " , " (List.map id_to_string component.comp_succ))
    (tdecls_to_string component.comp_decls)
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

let line_sep f itms = 
  List.map f itms |> String.concat "\n"
;;

let params_to_p4fields params = line_sep param_to_p4field params


(* the event's variable name is the events name *)
let   event_to_p4varname event : string =
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
;;  
let event_to_p4tyname event : string = 
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
;;

let event_to_p4fieldstr event : string =
  Printf.sprintf "%s : %s;"
    (event_to_p4varname event)
    (event_to_p4tyname event)
;;

let event_to_p4flag event : string = 
  match event with
  | EventSingle{evid;} -> 
    Id.prepend_string "flag_" (evid) |> Id.to_string
  | EventUnion{evid;} -> 
    Id.prepend_string "flag_" (evid) |> Id.to_string
  | EventSet{evid;} -> 
    Id.prepend_string "flag_" (evid) |> Id.to_string
  ;;
let event_to_p4flagfield event : string =
  event_to_p4flag event ^ " : " ^ "int<1>;"
;;

let rec event_to_p4tystr event : string = 
  match event with
  (* an event declaration is a type declaration *)
  | EventSingle({evid; evnum; evsort; evparams}) -> (
    (* a single event -- just a struct or header *)
    Printf.sprintf 
      "struct %s_t {\n%s\n}"
      (Id.to_string evid)
      (params_to_p4fields evparams)
  )
  (* an event union... we have to print out the members, then 
     print a union of those members... and thhe tag? *)
  | EventUnion({evid; members}) -> (
    (line_sep event_to_p4tystr members) ^ "\n"
    ^ (Printf.sprintf
        "struct %s_t {\n%s\n%s\n}"
        (Id.to_string evid)
        ("tag : int<8>;") (*the tag and its type -- global*)
        (line_sep event_to_p4fieldstr members) (*the member fields *)    
    )
  )
  | EventSet({evid; members}) -> (    
    (line_sep event_to_p4tystr members) ^ "\n"
    ^ (Printf.sprintf
        "struct %s_t {\n%s\n%s\n}"
        (Id.to_string evid)
        (line_sep event_to_p4flagfield members)
        (line_sep event_to_p4fieldstr members) (*the member fields *)    
    )
  )

(**)
