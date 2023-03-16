(* Interpret control events (defined in InterpSyntax.ml) *)

open CoreSyntax
open InterpSyntax
open InterpState
open CoreSyntaxGlobalDirectory

let extract_int v = match v.v with
  | VInt(z) -> Integer.to_int z
  | _ -> error "[extract_int] not an int"
;;
let extract_zint v = match v.v with
  | VInt(z) -> z
  | _ -> error "[extract_zint] not an int"

let handle_control (nst : State.network_state) swidx ctl_ev = 
  let p = nst.switches.(swidx).pipeline in
  match ctl_ev.ctl_cmd with
  | ArraySet(arrname, idx, newval) -> 
    let id = name_to_compiled_cid nst.global_names arrname |> Cid.to_id in  
    Pipeline.control_set id (extract_int idx) (List.map extract_zint newval) p
  | ArraySetRange(arrname, s, e, newval) -> 
    let id = name_to_compiled_cid nst.global_names arrname |> Cid.to_id in
    Pipeline.control_setrange id (extract_int s) (extract_int e) (List.map extract_zint newval) p
  | ArrayGet(arrname, idx) -> 
    let id = name_to_compiled_cid nst.global_names arrname |> Cid.to_id in
    let result = Pipeline.control_get id (extract_int idx) p in
    let result_json = 
      `Assoc[
        ("array", `String arrname);
        ("index", `Int (extract_int idx));
        ("value", `List (List.map (fun z -> `Int (Integer.to_int z)) result))
      ]
    in
    print_endline (Yojson.Basic.pretty_to_string result_json);
  | ArrayGetRange(arrname, s, e) -> 
    let id = name_to_compiled_cid nst.global_names arrname |> Cid.to_id in
    let cells = Pipeline.control_getrange id (extract_int s) (extract_int e) p in
    let vals = List.map 
      (fun cell -> `List (List.map (fun zword -> `Int (Integer.to_int zword)) cell))
      cells
    in
    let result_json = 
      `Assoc[
        ("array", `String arrname);
        ("start", `Int (extract_int s));
        ("end", `Int (extract_int e));
        ("values", `List vals)
      ]
    in
    print_endline (Yojson.Basic.pretty_to_string result_json);
  | TableInstall(tblname, entry) -> 
    let id = name_to_compiled_cid nst.global_names tblname |> Cid.to_id in
    (* resolve correct action name *)
    let eaction_str = fst entry.eaction in
    let eaction' = name_to_compiled_cid nst.global_names eaction_str |> Cid.to_id in
    let entry = {entry with 
      eaction = eaction';
    } in
    Pipeline.control_install_table_entry id entry p
  | _ -> 
    error "[execute_control] unknown control command"
;;