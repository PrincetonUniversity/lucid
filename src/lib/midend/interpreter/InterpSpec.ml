(* Parsing the interpreter specification file. *)
open Batteries
open CoreSyntax
open InterpSyntax
open InterpJson
open InterpCore
open Yojson.Basic
open Preprocess
module Env = InterpState.Env
module IntMap = InterpState.IntMap

module IC = InterpControl

type json = Yojson.Basic.t

type t =
  { num_switches : int
  ; links : InterpState.State.topology
  ; externs : value Env.t list
  ; events : interp_input list
  ; config : InterpConfig.interp_config_from_test
  ; extern_funs : (InterpState.State.network_state InterpSyntax.ival) Env.t
  ; ctl_pipe_name : string option
  }

(* and empty configuration. For function interpretation. *)
let empty_spec = {
  num_switches = 1;
  links = IntMap.empty;
  externs = [];
  events = [];
  config = { 
      num_switches = 1
    ; max_time = 0
    ; default_input_gap = 0
    ; generate_delay = 0
    ; propagate_delay = 0
    ; random_delay_range = 1
    ; random_propagate_range = 1
    ; random_seed = 0
    ; drop_chance = 0 };
  extern_funs = Env.empty;
  ctl_pipe_name = None;
}

let parse_int err_str (j : json) =
  match j with
  | `Int n -> n
  | _ -> error @@ "Non-integer value for " ^ err_str
;;

let parse_int_entry lst str default =
  match List.assoc_opt str lst with
  | Some j -> parse_int str j
  | None -> default
;;

let rec parse_value err_str ty j =
  match j, ty.raw_ty with
  | `Int n, TInt Sz size -> vint n size
  | `Int n, TName (cid, _) when Cid.equal cid Payloads.t_id -> vint n 32
  | `Bool b, TBool -> vbool b
  | `List lst, TGroup ->
    vgroup (List.map (fun n -> parse_int "group value definition" n) lst)
  | _ ->
    error
    @@ err_str
    ^ " specification had wrong or unexpected argument "
    ^ to_string j
;;

let parse_port str =
  match String.split_on_char ':' str with
  | [id; port] ->
    (try int_of_string id, int_of_string port with
     | _ -> error "Incorrect format for link entry!")
  | _ -> error "Incorrect format for link entry!"
;;

let default_port = 0

(* parse a line provided to the interactive mode interpreter,
   which may either be a single event (an assoc) or a list of
   multiple events (List of assocs) *)
let parse_interp_event_list
  (pp : Preprocess.t)
  (renaming : Renaming.env)
  (num_switches : int)
  (gap : int)
  (input_json : json)
  =
  let parse_f =
    (parse_interp_input
      Payloads.t_id 
      (Builtins.lucid_eventnum_ty |> SyntaxToCore.translate_ty)
      renaming.var_map pp.events num_switches gap default_port)
  in
  match input_json with
  | `List lst -> List.map parse_f lst
  | _ -> [parse_f input_json]
;;

let parse_interp_inputs
  (pp : Preprocess.t)
  (renaming : Renaming.env)
  (num_switches : int)
  (inter_event_gap : int)
  (events : json list)
  =
  let parse_f =
    (parse_interp_input
      Payloads.t_id (Builtins.lucid_eventnum_ty |> SyntaxToCore.translate_ty) renaming.var_map pp.events num_switches)
  in
  let wrapper
    ((located_events_rev : interp_input list), (default_next_event_time : int))
    (event_json : json)
    =
    let located_event =
      parse_f default_next_event_time default_port event_json
    in
    let next_ts = (interp_input_to_time located_event) + inter_event_gap in
    located_event :: located_events_rev, next_ts
  in
  let located_events_rev, _ = List.fold_left wrapper ([], 0) events in
  List.rev located_events_rev
;;

let builtins renaming n =
  List.init n (fun i ->
    Env.singleton (rename renaming "self" "self") (vint i 32))
;;

let parse_externs
  (pp : Preprocess.t)
  (renaming : Renaming.env)
  (num_switches : int)
  (externs : (string * json) list)
  =
  List.fold_left
    (fun acc (id, values) ->
      let id = rename renaming.var_map "extern" id in
      let ty = Env.find id pp.externs in
      let vs =
        match values with
        | `List lst -> List.map (parse_value "Extern" ty) lst
        | _ -> error "Non-list type for extern value specification"
      in
      if List.length vs <> num_switches
      then
        error
        @@ "Number of values for extern "
        ^ Cid.to_string id
        ^ " does not match number of switches";
      List.map2 (fun env v -> Env.add id v env) acc vs)
    (builtins renaming.var_map num_switches)
    externs
;;

let parse_links num_switches links recirc_ports =
  let add_link id port dst acc =
    try
      IntMap.modify
        id
        (fun map ->
          match IntMap.find_opt port map with
          | None -> IntMap.add port dst map
          | Some dst' when dst = dst' -> map
          | _ ->
            error
            @@ Printf.sprintf
                 "Switch:port pair %d:%d assigned to two different \
                  destinations!"
                 id
                 port)
        acc
    with
    | Not_found -> error @@ "Invalid switch id " ^ string_of_int id
  in
  let add_links acc (src, dst) =
    let src_id, src_port = parse_port src in
    let dst_id, dst_port =
      match dst with
      | `String dst -> parse_port dst
      | _ -> error "Non-string format for link entry!"
    in
    acc
    |> add_link src_id src_port (dst_id, dst_port)
    |> add_link dst_id dst_port (src_id, src_port)
  in
  List.fold_left add_links (InterpState.State.empty_topology num_switches recirc_ports) links
;;

(* Make a full mesh with arbitrary port numbers.
   Specifically, we map 1:2 to 2:1, and 3:4 to 4:3, etc. *)
let make_full_mesh num_switches recirc_ports =
  let switch_ids = List.init num_switches (fun n -> n) in
  List.fold_left2
    (fun acc id recirc_port ->
      let port_map =
        List.fold_left
          (fun acc port ->
            if id = port then acc else IntMap.add port (port, id) acc)
          (IntMap.add recirc_port (id, recirc_port) IntMap.empty)
          switch_ids
      in
      IntMap.add id port_map acc)
    IntMap.empty
    switch_ids
    recirc_ports
;;

(*   and code = network_state -> int (* switch *) -> ival list -> value
*)
let create_foreign_functions renaming efuns python_file =
  let open InterpState.State in
  let oc_to_py v =
    match v with
    | InterpSyntax.V { v = VBool b } -> Py.Bool.of_bool b
    | InterpSyntax.V { v = VInt n } -> Py.Int.of_int (Integer.to_int n)
    | _ -> error "Can only call external functions with int/bool arguments"
  in
  let obj =
    Py.Run.simple_file (Channel (Legacy.open_in python_file)) python_file;
    Py.Module.main ()
  in
  if Collections.IdSet.is_empty efuns
  then
    Console.warning
      "A Python file was provided, but no extern functions were declared.";
  Collections.IdSet.fold
    (fun id acc ->
      let id = rename renaming.Renaming.var_map "extern" (Id.name id) in
      let f_id = Id.name (Cid.to_id id) in
      match Py.Object.get_attr_string obj f_id with
      | None ->
        error @@ "External function " ^ f_id ^ " not found in python file"
      | Some o when not (Py.Callable.check o) ->
        error
        @@ "External function "
        ^ f_id
        ^ " is not a function in the python file!"
      | Some o ->
        let f =
          InterpSyntax.anonf
            (fun _ _ args ->
              let pyretvar =
                Py.Callable.to_function
                  o
                  (Array.of_list @@ List.map oc_to_py args)
              in
              let _ = pyretvar in
              (* this is the return from python? *)
              (* Dummy return value *)
              InterpSyntax.V(value (VBool false)))
        in
        Env.add id f acc)
    efuns
    Env.empty
;;

let parse 
(* (nst : InterpState.State.network_state) *)
(pp : Preprocess.t) (renaming : Renaming.env) (filename : string) : t =
  let json = from_file filename in
  match json with
  | `Assoc lst ->
    let parse_int_entry = parse_int_entry lst in
    let num_switches = parse_int_entry "switches" 1 |> max 1 in
    let default_input_gap = parse_int_entry "default input gap" 1 |> max 1 in
    let generate_delay = parse_int_entry "generate delay" 600 |> max 0 in
    let drop_chance = parse_int_entry "drop chance" 0 |> max 0 in
    let propagate_delay = parse_int_entry "propagate delay" 0 |> max 0 in
    let random_delay_range = parse_int_entry "random delay range" 1 |> max 1 in
    let random_propagate_range =
      parse_int_entry "random propagate range" 1 |> max 1
    in
    let random_seed =      
      (* "random seed" or just "seed" *)
      parse_int_entry "random seed"       
        (parse_int_entry "seed" (int_of_float @@ Unix.time ()))
    in
    let _ =
      (* Initialize Python env *)
      match List.assoc_opt "python path" lst with
      | Some (`String library_name) -> Py.initialize ~library_name ()
      | Some _ -> error "Python path entry must be a string!"
      | None -> Py.initialize ()
    in
    let externs, recirc_port_ints =
      let externs =
        match List.assoc_opt "externs" lst with
        | Some (`Assoc lst) -> lst
        | None -> []
        | Some _ -> error "Non-assoc type for extern definitions"
      in
      let recirc_ports_def =
        (* This is an extern under the hood, but users don't see it that way *)
        match List.assoc_opt "recirculation_ports" lst with
        | Some (`List lst) -> "recirculation_port", `List lst
        | None ->
          ( "recirculation_port"
          , `List (List.init num_switches (fun _ -> `Int 196)) )
        | Some _ -> error "Non-list type for recirculation port definitions"
      in
      (* each switch may have a different recirc port, for some reason *)
      let recirc_ports_ints = match (snd recirc_ports_def) with
        | `List lst -> List.map (fun j -> parse_int "recirculation port" j) lst
        | _ -> error "Non-list type for recirculation port definitions"
      in
      parse_externs pp renaming num_switches (recirc_ports_def :: externs), recirc_ports_ints
    in
    if ((List.length recirc_port_ints) <> num_switches)
    then
      error
      @@ "Number of recirculation ports does not match number of switches!";
    let links =
      if num_switches = 1
      then InterpState.State.empty_topology 1 recirc_port_ints
      else (
        match List.assoc_opt "links" lst with
        | Some (`Assoc links) -> parse_links num_switches links recirc_port_ints
        | Some (`String "full mesh") -> make_full_mesh num_switches recirc_port_ints
        | None -> error "No links field in network with multiple switches"
        | _ -> error "Unexpected format for links field")
    in
    let max_time =
      match List.assoc_opt "max time" lst with
      | Some (`Int n) -> n
      | _ -> (
        (* for interactive mode, max_time is the start timestamp for incoming events *)
        (* TODO: this isn't quite right. A max_time of 0 in interactive mode 
           prevents any recirculated events from running until the simulation starts. 
           That might be what we want, but it also might not be. Hmm... *)
        if (InterpConfig.cfg.interactive)
          then 0
          else 10000
      )
    in
    let events =
      match List.assoc_opt "events" lst with
      | Some (`List lst) ->
        parse_interp_inputs pp renaming num_switches default_input_gap lst
      | _ -> [] (* default of no init events for interactive mode *)
    in
    let extern_funs =
      match List.assoc_opt "python file" lst with
      | Some (`String file) ->
        create_foreign_functions renaming pp.extern_funs file
      | None ->
        if Collections.IdSet.is_empty pp.extern_funs
        then Env.empty
        else
          error
            "Extern functions were declared but no python file was provided!"
      | _ -> error "Non-string value for python_file"
    in
    let ctl_pipe_name =
      match List.assoc_opt "control pipe" lst with
      | Some (`String filename) -> Some filename
      | _ -> None
    in
    let config : InterpConfig.interp_config_from_test =
      { num_switches
      ; max_time
      ; default_input_gap
      ; generate_delay
      ; propagate_delay
      ; random_delay_range
      ; random_propagate_range
      ; random_seed
      ; drop_chance
      }
    in
    { num_switches; links; externs; events; config; extern_funs; ctl_pipe_name }
  | _ -> error "Unexpected interpreter specification format"
;;
