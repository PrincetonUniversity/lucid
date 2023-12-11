open Batteries
open Dpt

let cfg = Cmdline.cfg


let total_silence () =
  (* This function should suppress all stdout from 
     the interpreter / compiler. *)
  cfg.verbose <- false;
  cfg.debug <- false;
  cfg.show_effects <- false;
  cfg.verbose_types <- false;
  cfg.show_tvar_links <- false;
  cfg.show_queries <- false;
  cfg.show_constraints <- false;
  cfg.use_type_names <- false;
  cfg.show_all_effects <- false;
  cfg.partial_interp <- false;
  cfg.show_interp_state <- false;
  cfg.show_interp_events <- false;
  cfg.show_printf <- false;
;;


(* run the main function in <filename> on arguments passed in on commandline *)
let main () =
  if ((Array.length Sys.argv) < 2)
    then failwith "Usage: interpfcn <filename> <args>";
  let target_filename = Sys.argv.(1) in
  let args = Array.sub Sys.argv 2 ((Array.length Sys.argv) - 2) in
  (* args should all be integers -- try to convert and fail if not possible *)
  let args = Array.map int_of_string args in
  total_silence ();
  (* now parse the file and run the frontend and midend *)
  Cmdline.set_dpt_file target_filename;
  let ds = Input.parse target_filename in
  let _, ds =
    (* Profile.time_profile "frontend" @@ fun () -> *)
    FrontendPipeline.process_prog Builtins.interp_builtin_tys ds
  in
  let ds =
    (* Profile.time_profile "midend" @@ fun () -> *)
    MidendPipeline.process_prog ds
  in
  (* finally, interpret a call to the interpreter...  *)
  (* left off HERE *)
  let init = FunInterp.init_function ds in
  let results = FunInterp.run_function init (Array.to_list args) in
  let res_str = String.concat " " (List.map string_of_int results) in
  print_endline res_str;
;;


(* 
  start a read evaluate print loop that read arguments 
  from stdin (one line at a time, arguments are space-delimited ints), 
  calls the main function on those arguments, and prints the result to stdout   *)
let usage = "interpfcn -- read arguments from stdin, \
call the function annotated with @main on them, \
and print the result to stdout\
Usage: interpfcn <filename>";;

let stdio_loop () =
  if ((Array.length Sys.argv) != 2)
    then (print_endline usage; exit 1);
  if ((Sys.argv.(1) = "-h") || (Sys.argv.(1) = "--help"))
    then (print_endline usage; exit 0);
  let target_filename = Sys.argv.(1) in
  Cmdline.set_dpt_file target_filename;
  total_silence ();
  let ds = Input.parse target_filename in
  let _, ds =
    (* Profile.time_profile "frontend" @@ fun () -> *)
    FrontendPipeline.process_prog 
      ~opts:{match_event_handlers=false;}
    Builtins.interp_builtin_tys ds
  in
  let ds =
    (* Profile.time_profile "midend" @@ fun () -> *)
    MidendPipeline.process_prog ds
  in
  let ctx = FunInterp.init_function ds in
  try
    while true do
      let line = input_line stdin in
      let args = Str.split (Str.regexp " ") line in
      let args = List.map int_of_string args in
      let results = FunInterp.run_function ctx args in
      let res_str = String.concat " " (List.map string_of_int results) in
      print_endline res_str;
    done
  with End_of_file -> ()
;;


(* let _ = main () *)
let _ = stdio_loop ()
