open Batteries
open Dpt

let cfg = Cmdline.cfg

let find_spec_file dpt_file =
  if not (String.ends_with dpt_file ".dpt")
  then None
  else (
    let json_name = String.rchop ~n:4 dpt_file ^ ".json" in
    if Sys.file_exists json_name
    then (
      if (cfg.verbose)
      then 
      Console.report @@ "Auto-detected specification file " ^ json_name;
      Some json_name)
    else None)
;;


let main () =
  let target_filename = Cmdline.parse () in
  Cmdline.set_dpt_file target_filename;
  if (cfg.verbose)
  then Console.report "Parsing ...";
  let ds = Input.parse target_filename in
  let renaming, ds = FrontendPipeline.process_prog ds in
  let spec_file =
    if cfg.spec_file = ""
    then find_spec_file target_filename
    else Some cfg.spec_file
  in
  (match spec_file with
  | None ->
    Console.report "No specification file provided, so skipping simulation"
  | Some spec_file ->
    let ds = MidendPipeline.process_prog ~for_interp:true ds in
    let nst, pp, spec = Interp.initialize renaming spec_file ds in
    if (cfg.interactive) 
    (* interactive mode -- no printing *)
    then (
      let _ = Interp.run pp renaming spec nst in 
      ())
    else (
      Console.report "Simulating...";
      let nst = Interp.simulate nst in 
      Console.report "Final State:";
      if cfg.show_interp_state
      then print_endline @@ InterpState.State.nst_to_string nst;
      Console.report "Done"))
;;

let _ = main ()
