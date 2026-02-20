(* json parsing and deparsing for interpreter io from user *)

open Yojson.Basic
type json = Yojson.Basic.t

open CoreSyntax
open InterpSyntax
open InterpControl
open Pipeline

(* ---- interpreter input types -- (probably should go somewhere else!) ----  *)
type interp_input =
  | IEvent of {iev : event_val; ilocs : loc list; itime : int}
  | IControl of {ictl : control_val; ilocs : loc list; itime : int}
;;
let ievent iev ilocs itime = IEvent{iev; ilocs; itime}
let icontrol ictl ilocs itime = IControl{ictl; ilocs; itime}
let interp_input_to_time interp_input = 
  match interp_input with
  | IEvent({itime}) -> itime
  | IControl({itime}) -> itime
;;

let input_locs interp_input = 
  match interp_input with
  | IEvent({ilocs}) -> ilocs
  | IControl({ilocs}) -> ilocs
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
      | `String vstr -> (
        match vstr with
        | "true" -> vbool true
        | "false" -> vbool false
        | _ -> (
        (* x<size> *)
        let v, sz = if (String.contains vstr '<')
          then (
            let v_str = String.split_on_char '<' vstr |> List.hd in
            let pre_sz = String.split_on_char '>' vstr |> List.hd in
            let sz_str = String.split_on_char '<' pre_sz |> List.rev |> List.hd in
            int_of_string v_str, int_of_string sz_str

            (* Scanf.sscanf vstr "%d%[<]%d%[>]" (fun x _ y _ -> x,y) *)
          )
          else (
            int_of_string vstr,
              if (String.length vstr >= 2 && String.sub vstr 0 2 = "0x") 
                then(  
                  let hex_str = String.split_on_char 'x' vstr |> List.rev |> List.hd in
                  (String.length hex_str) * 4
                )
                else 32
            (* Scanf.sscanf vstr "%d" (fun x -> x, 32) *)
          )
        in
        vint v sz
        )
      )
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

(* control command parsing *)
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

(* just get the mask component *)
let rec v_to_mask (v : CoreSyntax.v) = 
  match v with 
    | VTuple(vs) -> 
      CoreSyntax.VTuple(List.map v_to_mask vs)
    | VRecord(id_vs) -> 
      VRecord(
        List.combine
          ((List.split id_vs) |> fst)
          (List.split id_vs |> snd |> List.map v_to_mask))
    | VBool _ -> CoreSyntax.VBool true
    | VInt(z) -> 
      let mask = Integer.max_int (Integer.size z) in
      CoreSyntax.VInt(mask)
    | VBits b -> 
      (* let v = BitString.bits_to_int b  in  *)
      (* let v = Integer.create ~value:v ~size:(List.length b) in *)
      let m  = Integer.max_int (List.length b) in
      CoreSyntax.VInt(m)
    | VGlobal _ -> Console.error "a global cannot appear as a key in a table"
    | VEvent _ -> Console.error "an event cannot appear as a key in a table"
    | VPat _  -> Console.error "pat values are depreciated"
    | VGroup _ -> Console.error "group values cannot appear as a key in a table"
  ;;


let parse_entry entrylst =
  let key = match (List.assoc "key" entrylst) with
    | `List intlist -> List.map JP.to_eval_int intlist
    (* | `List patlst -> List.map parse_pat patlst |> List.map value_to_exp *)
    | _ -> error "[parse_entry] key is in wrong format"
  in

  let mask = match (List.assoc_opt "mask" entrylst) with
    | Some(`List intlist) -> List.map JP.to_eval_int intlist
    | Some(_) -> error "[parse_entry] mask is in wrong format"
    | None -> (
      let exp_to_mask exp = match exp.e with 
        | EVal({v; vty; vspan}) -> {exp with e=EVal({v=v_to_mask v; vty; vspan})}
        | _ -> error "[parse_entry] key expression is not a value"
      in
      List.map exp_to_mask key
    )
  in

  let action = match (List.assoc "action" entrylst) with
    | `String acn_name -> 
      Id.create acn_name
    | _ -> error "[parse_entry] action name is wrong format"
  in
  let priority = match (List.assoc_opt "priority" entrylst) with
    | Some(v) -> JP.to_size v
    | _ -> 50
  in
  let action_args = match (List.assoc "args" entrylst) with
    | `List argslst -> 
      List.map JP.to_eval_int argslst
    | _ -> error "[parse_entry] action args are in wrong format"
  in
  {
    InterpControl.iprio= priority;
    imatch=key;
    imask = mask;
    iaction=action;
    iargs = action_args;
  }
;;

let parse_str_field lst field = match (List.assoc field lst) with
  | `String s -> s
  | _ -> error "[parse_str_field] not a string value"
;;

(*
  Table.install format: 
  { "type":"command", 
    "name": "Table.install", 
    "args":{
      "table":"mytbl", 
      "key":["4<32>", "0<32>"],
      "mask":["3<32>", "0<32>"],
      "action":"hit_acn", 
      "args":[3], 
      "priority":1
    }
  }
  Note: mask is optional, the default is an exact mask    *)
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

(*** event parsing (plus helpers ripped out of InterpSpec)***)

let rename env err_str id_str =
  match Collections.CidMap.find_opt (Cid.from_string id_str) env with
  | Some id -> id
  | None -> error @@ Printf.sprintf "Unknown %s %s" err_str id_str
;;
    
let parse_port str =
  match String.split_on_char ':' str with
  | [id; port] ->
    (try int_of_string id, int_of_string port with
     | _ -> error "Incorrect format for link entry!")
  | _ -> error "Incorrect format for link entry!"
;;

let parse_int err_str (j) =
  match j with
  | `Int n -> n
  | _ -> error @@ "Non-integer value for " ^ err_str
;;

let rec parse_value payloads_t_id err_str ty j =
  match j, ty.raw_ty with
  | `Int n, TInt (Sz size) -> vint n size
  (* payloads should be hex strings, but we also read ints as 32-bit uints *)
  | `Int n, TName (cid, _) when Cid.equal cid payloads_t_id -> (
    BitString.int_to_bits 32 n |> CoreSyntax.vbits
  )
  | `String s, TName (cid, _) when Cid.equal cid payloads_t_id -> (
    BitString.hexstr_to_bits s |> CoreSyntax.vbits
  )
  | `String s, TInt (Sz size) -> 
    let res = JP.to_vint (`String s) in
    let z = match res with 
      | {v=VInt(z)} -> Integer.to_int z
      | _ -> error "invalid string for arg value"
    in
    vint z size
  | `Bool b, TBool -> vbool b
  | `List lst, TGroup ->
    vgroup (List.map (fun n -> parse_int "group value definition" n) lst)
  | _ ->
    error
    @@ err_str
    ^ " specification had wrong or unexpected argument "
    ^ Yojson.Basic.to_string j
;;



let parse_locations default_port num_switches lst : loc list =
  let locations =
    match List.assoc_opt "locations" lst with
    | Some (`List lst) ->
      List.map
        (function
         | `String str ->
           let sw, port = parse_port str in
           if sw < 0 || sw >= num_switches
           then
             error
             @@ "Cannot specify event at nonexistent switch "
             ^ string_of_int sw;
           if port < 0 || port >= 255
           then
             error
             @@ "Cannot specify event at nonexistent port "
             ^ string_of_int port;
           {switch=Some(sw);port}
         | _ -> error "Event specification had non-string location")
        lst
    | None -> [{switch=Some(0); port=default_port}]
    | _ -> error "Event specification has non-list locations field"
  in
  locations
;;


(* control locations are just a switch number, no port *)
let parse_control_locations num_switches lst = 
  let locations =
    match List.assoc_opt "locations" lst with
    | Some (`List lst) ->
      List.map
        (function
        | `Int sw ->
          if sw < 0 || sw >= num_switches
          then
            error
            @@ "Cannot specify control command at nonexistent switch "
            ^ string_of_int sw;
          {switch=Some(sw); port=0}
        | _ -> error "Control command specification had non-int location")
        lst
    | None -> [{switch=Some(0); port=0}]
    | _ -> error "control command specification has non-list locations field"
  in
  locations
;;


let parse_timestamp default_next_ts lst =
  match List.assoc_opt "timestamp" lst with
  | Some (`Int n) -> n
  | None -> default_next_ts
  | _ -> error "Event specification had non-integer delay field"
;;

let get_eid var_map lst =
  match List.assoc_opt "name" lst with
  | Some (`String id) -> rename var_map "event" id
  | None -> error "Event specification missing name field"
  | _ -> error "Event specification had non-string type for name field"
;;

(* get event data  *)
let get_data (payloads_t_id, var_map) event_json events = 
  let eid = get_eid var_map event_json in
  let data =
    match List.assoc_opt "args" event_json with
    | Some (`List lst) -> 
      (* let _, tys = InterpSyntax.get_event_tys eid pp.events in *)
      let _, _, tys = Collections.CidMap.find eid events in
      (try List.map2 (parse_value payloads_t_id "Event") tys lst with
      | Invalid_argument _ ->
        error
        @@ Printf.sprintf
             "Event specification for %s had wrong number of arguments"
             (Cid.to_string eid))
    | None -> error "Event specification missing args field"
    | _ -> error "Event specification had non-list type for args field"
  in
  data
;;

let get_num events eid = 
  let _, num, _ = Collections.CidMap.find eid events in
  num
;;

let packet_event bits_val edelay = 
  {eid=Cid.create ["packet"]; evnum=None; data=[bits_val]; edelay; eserialized=true}
;;

let parse_interp_input 
  payloads_t_id
  lucid_eventnum_ty
  ev_renames
  events
  num_switches
  default_next_ts (* the current timestamp of the interpreter *)
  default_port
  event : interp_input = 
  match event with
  | `Assoc event_json ->
    (match List.assoc_opt "type" event_json with
     | Some (`String "event")
     | None -> 
      (* located user event event *)
      let eid = get_eid ev_renames event_json in
      let data = get_data (payloads_t_id, ev_renames) event_json events in
      let timestamp = parse_timestamp default_next_ts event_json in
      let locations = parse_locations default_port num_switches event_json in
      let num = get_num events eid in
      let evnum = match num with 
      | None -> None
      | Some(n) -> Some(CoreSyntax.vint n (size_of_tint lucid_eventnum_ty))
       in
      ievent { eid; data; edelay=0; evnum; eserialized=false} locations timestamp
  
     | Some (`String "command") -> 
      (* located control/command event *)
      let timestamp = parse_timestamp default_next_ts event_json in
      let locations = parse_control_locations num_switches event_json in
      let control_event =parse_control_e event_json in
      icontrol control_event locations timestamp
     | Some (`String "packet") -> 
       let timestamp = parse_timestamp default_next_ts event_json in
       let locations = parse_locations default_port num_switches event_json in
       let pkt_bytes = match List.assoc_opt "bytes" event_json with
        | Some (`String s) -> s
        | _ -> error "packet event must have a bytes field"
       in
       let pkt_event = packet_event 
          (hexstr_to_vbits pkt_bytes)
          0 (*default delay*)
       in
       ievent pkt_event locations timestamp
     | Some _ ->
       error "unknown interpreter input type (expected event or command)"
    )
  | _ -> error "Non-assoc type for interpreter input"
;;

(* string functions *)
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

(* json exporter functions *)
(* let packet_event_to_json event = 
  ["type", `String "packet";   
  "bytes", `String (List.hd event.data |> CorePrinting.value_to_string)]
;; *)

let event_to_json {eid; data; eserialized} =   
  let raw_json_val (v : value) =
    match v.v with
    | VInt i -> `Int (Integer.to_int i)
    | VBool b -> `Bool b
    | VBits bs -> `String (BitString.bits_to_hexstr bs)
    | _ -> error "[json event arg printing] not an int, bool, or bitstring / payload"
  in  
  if (not eserialized) then (
    let name = `String (CorePrinting.cid_to_string eid) in
    let args = `List (List.map raw_json_val data) in
    [("name", name); ("args", args)])
  else (
    let ty = `String "packet" in    
    let bs = `String (List.hd data |> CorePrinting.value_to_string) in
    [("type", ty); ("bytes", bs)]
  )
;;

let event_exit_to_json swid port_opt event time =
  let open Yojson.Basic in
  let base_event = event_to_json event in  
  let port =
    match port_opt with
    | None -> -1
    | Some p -> p
  in
  let locs = `List [`String (Printf.sprintf "%i:%i" swid port)] in
  let timestamp = `Int time in
  (* let args = CorePrinting.value_to_string data in  *)
  let evjson = `Assoc(base_event@["locations", locs; "timestamp", timestamp]) in
  Yojson.Basic.to_string evjson
;;


let interp_report_json msgty msg swid_opt =
`Assoc ([msgty, `String msg]
  @ match swid_opt with
    | Some swid -> ["switch", `Int swid]
    | _ -> [])
|> Yojson.Basic.pretty_to_string
;;


