open Batteries
open Dpt

let find_spec_file dpt_file =
  if not (String.ends_with dpt_file ".dpt")
  then None
  else (
    let json_name = String.rchop ~n:4 dpt_file ^ ".json" in
    if Sys.file_exists json_name
    then (
      Console.report @@ "Auto-detected specification file " ^ json_name;
      Some json_name)
    else None)
;;

let cfg = Cmdline.cfg

let main () =
  let print_if_verbose ds =
    if cfg.verbose
    then (
      print_endline "decls: ";
      let str = Printing.decls_to_string ds in
      Console.report str)
  in
  let target_filename = Cmdline.parse () in
  Console.report "Parsing ...";
  let ds = Input.parse target_filename in
  print_if_verbose ds;
  print_endline "---------typing---------";
  let ds = Typer.infer_prog ds in
  print_endline "---------Making returns explicit-------------";
  let ds = ExplicitReturns.adjust_returns ds in
  print_if_verbose ds;
  print_endline "-----------renaming-----------";
  let renaming, ds = Renaming.rename ds in
  print_if_verbose ds;
  print_endline "-------Eliminating modules---------";
  let ds = ModuleElimination.eliminate_prog ds in
  print_if_verbose ds;
  print_endline "-----------inlining-----------";
  let ds = FunctionInlining.inline_prog ds in
  print_if_verbose ds;
  print_endline "---------------typing again-------------";
  let ds = Typer.infer_prog ds in
  print_endline "------------Checking entry handlers---------------";
  Linerate.check ds;
  print_endline "---------Eliminating events with global arguments----------";
  let ds = GlobalArgElimination.eliminate_prog ds in
  print_if_verbose ds;
  print_endline "-------Eliminating user global types-------";
  let ds = UserTypeElimination.eliminate_prog ds in
  print_if_verbose ds;
  print_endline "---------------typing again-------------";
  (* Just to be safe *)
  let ds = Typer.infer_prog ds in
  print_if_verbose ds;
  let spec_file =
    if cfg.spec_file = ""
    then find_spec_file target_filename
    else Some cfg.spec_file
  in
  (match spec_file with
  | None ->
    Console.report "No specification file provided, so skipping simulation"
  | Some spec_file ->
    Console.report "Simulating...";
    let nst = Interp.initialize renaming spec_file ds in
    let nst = Interp.simulate nst in
    Console.report "Final State:";
    print_endline @@ InterpState.State.nst_to_string nst);
  Console.report "Done"
;;

let _ = main ()
