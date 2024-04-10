(* Interpret control events (defined in InterpSyntax.ml) *)

open CoreSyntax
(* open InterpSyntax *)
open CoreSyntaxGlobalDirectory
open Pipeline

(* left off here -- make this tbl_entry an unevaluated expression, 
   which handle_control should transform into an action function *)

type entry_install_cmd = {
  iprio : int;
  imatch : exp list;  
  imask  : exp list;
  iaction : id;
  iargs : exp list;
}
(* a control value *)
type control_val = 
  | ArraySet of string * value * (value list)
  | ArraySetRange of string * value * value * (value list)
  | ArrayGet of string * value
  | ArrayGetRange of string * value * value
  | TableInstall of string * entry_install_cmd
  | Print of string
  | Noop


let handle_control do_tbl_install p global_names ctl_val =
  let extract_int v = match v.v with
    | VInt z -> Integer.to_int z
    | _ -> error "[extract_int] not an int"
  in  
  let extract_zint v = match v.v with
    | VInt z -> z
    | _ -> error "[extract_zint] not an int"
  in
  
  match ctl_val with
  | ArraySet (arrname, idx, newval) ->
    let id = name_to_compiled_cid global_names arrname |> Cid.to_id in
    Pipeline.control_set id (extract_int idx) (List.map extract_zint newval) p
  | ArraySetRange (arrname, s, e, newval) ->
    let id = name_to_compiled_cid global_names arrname |> Cid.to_id in
    Pipeline.control_setrange
      id
      (extract_int s)
      (extract_int e)
      (List.map extract_zint newval)
      p
  | ArrayGet (arrname, idx) ->
    let id = name_to_compiled_cid global_names arrname |> Cid.to_id in
    let result = Pipeline.control_get id (extract_int idx) p in
    let result_json =
      `Assoc
        [ "array", `String arrname
        ; "index", `Int (extract_int idx)
        ; "value", `List (List.map (fun z -> `Int (Integer.to_int z)) result) ]
    in
    print_endline (Yojson.Basic.pretty_to_string result_json)
  | ArrayGetRange (arrname, s, e) ->
    let id = name_to_compiled_cid global_names arrname |> Cid.to_id in
    let cells =
      Pipeline.control_getrange id (extract_int s) (extract_int e) p
    in
    let vals =
      List.map
        (fun cell ->
          `List (List.map (fun zword -> `Int (Integer.to_int zword)) cell))
        cells
    in
    let result_json =
      `Assoc
        [ "array", `String arrname
        ; "start", `Int (extract_int s)
        ; "end", `Int (extract_int e)
        ; "values", `List vals ]
    in
    print_endline (Yojson.Basic.pretty_to_string result_json)
  | TableInstall (tblname, entry) ->
    let tbl_cid = name_to_compiled_cid global_names tblname in
    (* resolve correct action constructor name *)
    let eaction' =
      name_to_compiled_cid global_names (fst entry.iaction) |> Cid.to_id
    in
    let entry = {entry with iaction = eaction' } in
    (* use the callback, which should call Tables.install_fun *)
    let _ = do_tbl_install tbl_cid entry in
    ()
  | _ -> error "[execute_control] unknown control command"
;;
