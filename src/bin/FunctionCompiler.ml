(* compile a lucid function (plus globals) to c *)
open Batteries
open Dpt

let cfg = Cmdline.cfg


let main () =
  Cmdline.cfg.debug <- true; 

  if ((Array.length Sys.argv) != 3)
    then failwith "Usage: interpfcn <lucid program> <output filename>";
  let target_filename = Sys.argv.(1) in
  let out_filename = Sys.argv.(2) in
  (* now parse the file and run the frontend and midend *)
  Cmdline.set_dpt_file target_filename;
  let ds = Input.parse target_filename in
  let _, ds =
    (* Profile.time_profile "frontend" @@ fun () -> *)
    FrontendPipeline.process_prog ~opts:{match_event_handlers=false; elim_records=false;} Builtins.interp_builtin_tys ds
  in
  print_endline ("compiling");
  let prog_str = CPipeline.compile ds in
  output_string (open_out out_filename) prog_str
;;

let _ = main ();;