(* syntax extensions for the interpreter, mainly related 
   to the input stream, which consists of 
   events located at specific ports in the network 
   and interpreter control commands *)
open CoreSyntax
open Yojson.Basic
open Str

(* interpreter state events *)
type control_e = 
  | ArraySet of string * value * (value list)
  | ArraySetRange of string * value * value * (value list)
  | ArrayGet of string * value
  | ArrayGetRange of string * value * value
  | TableInstall of string * tbl_entry
  | Print of string
  | Noop

type control_event = {
  ctl_cmd : control_e;
  ctl_edelay : int;
}

type interp_event =
  | IEvent of event
  | IControl of control_event

(* event queued for execution at a switch *)
type switch_event = {
  sevent : interp_event;
  stime : int;
  sport : int;
}

type loc = {
  switch : int;
  port : int;
}
(* input to the interpreter from the user *)
type located_event = {
  ievent : interp_event;
  ilocs : loc list;
}

(* constructors *)
let loc (switch, port) = {switch; port}
let ievent ev = IEvent(ev)
let ilocated_event (ev, locs) =
  {ievent = IEvent(ev); ilocs = List.map loc locs}
;;
let ilocated_control (ctl_ev, locs) = 
  {ievent = IControl(ctl_ev); ilocs = List.map loc locs}
;;

let assoc_to_vals keys lst =
  List.map
    (fun aname -> List.assoc aname lst)
    keys
;;    


module JP = struct
  let to_size v =
    match v with 
      | `Int v -> v
      | `String v -> int_of_string v        
      | _ -> error "[json_to_intval] got a value that is not an int or string"
  ;;

  (* json parsing module *)
  let to_vint v = 
    match v with 
      (* 32-bit int *)
      | `Int v -> vint v 32
      | `String vstr -> 
        (* x<<size>> *)
        let v, sz = if (String.contains vstr '<')
          then (
            Scanf.sscanf vstr "%d<<%d>>" (fun x y -> x,y))
          else (
            Scanf.sscanf vstr "%d" (fun x -> x, 32)
          )
        in
        vint v sz
      | _ -> error "[to_vint] got a value that is not an int or string"
  ;;
  let to_eval_int v = v |> to_vint |> value_to_exp ;;



  (* parse string to value. Value may have optional size. 
     Value may be a wildcard for table pattern (_). *)
  let string_to_val_components str =
    let re_val_size = Str.regexp "\\(.*\\)<<\\(.*\\)>>" in
    let re_val = Str.regexp "\\(.*\\)" in
    try 
      let _ = Str.search_forward re_val_size str 0 in
      (Str.matched_group 1 str, Str.matched_group 2 str)
    with _ -> (
      try 
        let _ = Str.search_forward re_val str 0 in
        Str.matched_group 1 str, "32"
      with _ -> (
        error "invalid form for pattern value string"
      )
    )
  ;;
end

let parse_array_set lst = 
  let arglist = match (List.assoc "args" lst) with
    | `List lst -> lst
    | `Assoc lst -> assoc_to_vals ["array"; "index"; "value"] lst
    | _ -> error "err"
  in
  match arglist with
    | [`String a; idx; `List vs] -> 
      ArraySet(a, JP.to_vint idx, List.map JP.to_vint vs)
    | _ -> error "[parse_array_set] args must be an assoc or list"
;;

let parse_array_setrange lst = 
  let arglist = match (List.assoc "args" lst) with
    | `List lst -> lst
    | `Assoc lst -> assoc_to_vals ["array"; "start"; "end"; "value"] lst
    | _ -> error "err"
  in
  match arglist with
    | [`String a; s; e; `List vs] -> 
      ArraySetRange(a, JP.to_vint s, JP.to_vint e, List.map JP.to_vint vs)
    | _ -> error "[parse_array_set] args must be an assoc or list"

let parse_array_get lst =
  let arglist = match (List.assoc "args" lst) with
    | `List lst -> lst
    | `Assoc lst -> assoc_to_vals ["array"; "index"] lst
    | _ -> error "[parse_array_get] args must be an assoc or list"
  in
  match arglist with
    | [`String a; v] -> ArrayGet(a, JP.to_vint v)
    | _ -> error "[malformed array.get args]"
;;
let parse_array_getrange lst =
  let arglist = match (List.assoc "args" lst) with
    | `List lst -> lst
    | `Assoc lst -> assoc_to_vals ["array"; "start"; "end"] lst
    | _ -> error "[parse_array_getrange] args must be an assoc or list"
  in
  match arglist with
    | [`String a; s; e] -> ArrayGetRange(a, JP.to_vint s, JP.to_vint e)
    | _ -> error "[malformed array.get args]"
;;

let parse_pat patjson =
  match patjson with
  | `String s -> (
    match Str.split (Str.regexp "&&&") s with
    (* no mask, just base *)
    | [b] -> (
      let v, sz = JP.string_to_val_components b in
      let v, sz = String.trim v, String.trim sz in 
      let sz = int_of_string sz in
      match v with
        | "_" -> vwild sz
        | vint -> vpat (int_to_bitpat (int_of_string vint) sz))
    (* base and mask. Note: use size from base. *)
    | [b; m] -> (
      let v, sz = JP.string_to_val_components b in
      let m, _  = JP.string_to_val_components m in
      let v, sz, m = String.trim v, String.trim sz, String.trim m in 
      vpat (int_mask_to_bitpat (int_of_string v) (int_of_string m) (int_of_string sz))      
    )
    (* something else -- fail. *)
    | _ -> error "malformed pattern string";
  )
  (* int -- assume 32-bit *)
  | `Int i -> vpat (int_to_bitpat i 32)
  | _ -> error "not a pattern string"
;;

let parse_entry entrylst =
  let key = match (List.assoc "key" entrylst) with
    | `List patlst -> List.map parse_pat patlst |> List.map value_to_exp
    | _ -> error "[parse_entry] key is in wrong format"
  in
  let action = match (List.assoc "action" entrylst) with
    | `String acn_name -> 
      Id.create acn_name
    | _ -> error "[parse_entry] action name is wrong format"
  in
  let priority = match (List.assoc_opt "priority" entrylst) with
    | Some(v) -> JP.to_size v
    | _ -> 0
  in
  let action_args = match (List.assoc "args" entrylst) with
    | `List argslst -> 
      List.map JP.to_eval_int argslst
    | _ -> error "[parse_entry] action args are in wrong format"
  in
  {
    eprio= priority;
    ematch=key;
    eaction=action;
    eargs = action_args;
  }
;;

let parse_str_field lst field = match (List.assoc field lst) with
  | `String s -> s
  | _ -> error "[parse_str_field] not a string value"
;;

(*
  Table.install format: 
  {"type":"command", "name": "Table.install", 
    "args":{"table":"mytbl", "pattern":["4<<32>> &&& 3<<32>>", "_<<32>>"], "action":"hit_acn", "args":[3], "priority":1}    
    }*)
let parse_table_install lst =
  let tblname, entry = match (List.assoc "args" lst) with
    | `Assoc entrylst -> 
      parse_str_field entrylst "table", parse_entry entrylst
    | _ -> error "[parse_table_install] args is wrong type"
  in
  TableInstall(tblname, entry)
;;

let parse_control_e lst =
  match (List.assoc "name" lst) with
    | (`String "Array.set") -> parse_array_set lst      
    | (`String "Array.setrange") -> parse_array_setrange lst      
    | (`String "Array.get") -> parse_array_get lst
    | (`String "Array.getrange") -> parse_array_getrange lst
    | (`String "Table.install") -> parse_table_install lst
    | _ -> error "unknown control command"
    (* | None -> error "control command has no name field" *)
;;
(* printing *)
let control_event_to_string control_e =
  match control_e with
  | ArraySet(arrname, idx, newvals) -> 
    Printf.sprintf 
      "Array.set(%s, %s, %s);" 
      arrname 
      (CorePrinting.value_to_string idx) 
      (Printf.sprintf "[%s]" (List.map CorePrinting.value_to_string newvals |> String.concat ","))
  | ArraySetRange(arrname, s, e, newvals) -> 
    Printf.sprintf 
      "Array.setrange(%s, %s, %s, %s);" 
      arrname 
      (CorePrinting.value_to_string s)
      (CorePrinting.value_to_string e) 
      (Printf.sprintf "[%s]" (List.map CorePrinting.value_to_string newvals |> String.concat ","))
  | ArrayGet(arrname, idx) -> 
    Printf.sprintf "Array.get(%s, %s);" arrname (CorePrinting.value_to_string idx)
  | _ -> "<control event without printer>"
;;

(* destructors *)
let loc_to_tup loc = (loc.switch, loc.port)
let locs_to_tups = List.map loc_to_tup

let interp_event_delay (iev : interp_event) = match iev with
  | IEvent(ev) -> ev.edelay
  | IControl(ev) -> ev.ctl_edelay
;;

let located_event_delay (lev:located_event) = 
  interp_event_delay lev.ievent
;;
let interp_event_to_string ievent =
  match ievent with
  | IEvent(e) -> CorePrinting.event_to_string e
  | IControl(_) -> "(Control event (printer not implemented))"
;;