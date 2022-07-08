(* 
  streaming input and output for interactive interpreter
    - read from stdin
    - write to stdout
*)

open CoreSyntax
open InterpSpec
open Yojson.Basic



(* parse a single event json 
   this should probably be merged with 
   parse_event inside of parse_events, 
   if possible *)
let parse_streamed_event 
  (pp:Preprocess.t) 
  (renaming:Renaming.env) 
  (spec:InterpSpec.t)
  (current_time:int)
  (event : json) =
  match event with
  | `Assoc lst ->
    (* Find the event name, accounting for the renaming pass, and get its
       sort and argument types *)
    let eid =
      match List.assoc_opt "name" lst with
      | Some (`String id) -> rename renaming.var_map "event" id
      | None -> error "Event specification missing name field"
      | _ -> error "Event specification had non-string type for name field"
    in
    let sort, tys = Env.find eid pp.events in
    if sort = EExit then error "Cannot specify exit event";
    (* Parse the arguments into values, and make sure they have the right types.
       At the moment only integer and boolean arguments are supported *)
    let data =
      match List.assoc_opt "args" lst with
      | Some (`List lst) ->
        (try List.map2 (parse_value "Event") tys lst with
        | Invalid_argument _ ->
          error
          @@ Printf.sprintf
               "Event specification for %s had wrong number of arguments"
               (Cid.to_string eid))
      | None -> error "Event specification missing args field"
      | _ -> error "Event specification had non-list type for args field"
    in
    (* Parse the timestamp and location fields, if they exist *)
    let ets = 
      match List.assoc_opt "timestamp" lst with
      (* time must be greater than current time *)
      | Some (`Int n) -> max n current_time
      | None -> current_time
      | _ -> error "Event specification had non-integer delay field"
    in
    let locations =
      match List.assoc_opt "locations" lst with
      | Some (`List lst) ->
        List.map
          (function
            | `String str ->
              let sw, port = parse_port str in
              if sw < 0 || sw >= spec.num_switches
              then
                error
                @@ "Cannot specify event at nonexistent switch "
                ^ string_of_int sw;
              if port < 0 || port >= 255
              then
                error
                @@ "Cannot specify event at nonexistent port "
                ^ string_of_int port;
              sw, port
            | _ -> error "Event specification had non-string location")
          lst
      | None -> [0, default_port]
      | _ -> error "Event specification has non-list locations field"
    in
    { eid; data; edelay=ets }, locations
  | _ -> error "Non-assoc type for event definition"
;;


