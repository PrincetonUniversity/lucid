let print_if_verbose ds =
  if Cmdline.cfg.verbose
  then (
    let str = Printing.decls_to_string ds in
    Console.report str)
;;

let process_prog ds =
  print_if_verbose ds;
  let ds = ReplaceUserTys.replace_prog ds in
  print_if_verbose ds;
  Wellformed.pre_typing_checks ds;
  let time1 = Sys.time () in
  let ds = Typer.infer_prog ds in
  print_endline @@ "Typing time: " ^ string_of_float (Sys.time () -. time1);
  print_if_verbose ds;
  let ds = ExplicitReturns.adjust_returns ds in
  print_if_verbose ds;
  let renaming, ds = Renaming.rename ds in
  print_if_verbose ds;
  let ds = ModuleElimination.eliminate_prog ds in
  print_if_verbose ds;
  let ds = ReplaceUserTys.replace_prog ds in
  print_if_verbose ds;
  let ds = SizeInlining.replace_prog ds in
  print_if_verbose ds;
  let ds = GlobalArgElimination.eliminate_prog ds in
  print_if_verbose ds;
  let ds = FunctionInlining.inline_prog ds in
  print_if_verbose ds;
  let ds = Typer.infer_prog ds in
  let ds = RecordElimination.eliminate_prog ds in
  print_if_verbose ds;
  let ds = VectorElimination.eliminate_prog ds in
  print_if_verbose ds;
  let ds = TupleElimination.eliminate_prog ds in
  print_if_verbose ds;
  (* Just to be safe *)
  let ds = Typer.infer_prog ds in
  print_if_verbose ds;
  renaming, ds
;;
