(* parse command line arguments for docker. Return: 
  spec: spec file
  main: main source file
  include: other source file
  include: other source file
  ...
   *)
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
      if not cfg.interactive
      then Console.report @@ "Auto-detected specification file " ^ json_name;
      Some json_name)
    else None)
;;

let main () =
  let command = Sys.argv.(1) in 
  Arg.current := 1;
  match command with 
  | "main" -> 
    let target_filename = Cmdline.parse () in
    print_endline (target_filename)
  | "includes" -> 
    let target_filename = Cmdline.parse () in
    let source_files = Input.get_includes target_filename in
    String.concat ":" source_files |> print_endline    
  | _ -> ()
;;

let _ = main ()
