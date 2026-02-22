open Batteries
open Dpt

let cfg = InterpConfig.cfg

let find_spec_file dpt_file =
  if not (String.ends_with dpt_file ".dpt")
  then None
  else (
    let json_name = String.rchop ~n:4 dpt_file ^ ".json" in
    if Sys.file_exists json_name
    then (
      if (not cfg.interactive) && not cfg.json
      then Console.report @@ "Auto-detected specification file " ^ json_name;
      Some json_name)
    else None)
;;
let nst_to_string
  ?(show_vars = false)
  ?(show_pipeline = true)
  ?(show_queue = true)
  ?(show_exits = true)
  (nst : InterpState.network_state)
  =
  let base_str = Array.fold_lefti
    (fun acc idx st ->
      Printf.sprintf "%s\nSwitch %d : %s" acc idx
      @@ InterpSwitch.to_string ~show_vars ~show_pipeline ~show_queue ~show_exits st)
    ""
    nst.switches
  in
  if InterpConfig.cfg.json || InterpConfig.cfg.interactive
    then InterpJson.interp_report_json "final_state" base_str None
    else base_str
;;

let main () =
  let target_filename = InterpConfig.parse_interp () in
  let ds = Input.parse Config.base_cfg.dpt_file in
  let renaming, ds =
    FrontendPipeline.process_prog Builtins.interp_builtin_tys ds
  in
  let spec_file =
    if cfg.spec_file = ""
    then find_spec_file target_filename
    else Some cfg.spec_file
  in
  match spec_file with
  | None ->
    Console.report "No specification file provided, so skipping simulation"
  | Some spec_file ->
    let ds =
      MidendPipeline.process_prog ds
    in
    let nst, pp, spec =
      Interp.initialize renaming spec_file ds
    in
    if cfg.interactive
    then ignore (Interp.run pp renaming spec nst)
    else (
      if not cfg.json then Console.report "Simulating...";
      let nst = Interp.simulate nst in
      if cfg.show_interp_state then (
        if (not cfg.interactive) && not cfg.json
          then Console.report "Final State:";
      print_endline @@ nst_to_string nst))
;;

let _ = main ()
