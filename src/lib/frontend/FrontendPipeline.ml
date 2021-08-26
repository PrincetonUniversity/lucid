let print_if_verbose ds =
  if Cmdline.cfg.verbose
  then (
    print_endline "decls: ";
    let str = Printing.decls_to_string ds in
    Console.report str)
;;

let process_prog ds =
  print_if_verbose ds;
  print_endline "-------Eliminating type aliases 1---------";
  let ds = ReplaceUserTys.replace_prog ds in
  print_if_verbose ds;
  print_endline "-------Checking well-formedness---------";
  Wellformed.pre_typing_checks ds;
  print_endline "---------typing---------";
  let ds = Typer.infer_prog ds in
  print_if_verbose ds;
  print_endline "---------Making returns explicit-------------";
  let ds = ExplicitReturns.adjust_returns ds in
  print_if_verbose ds;
  print_endline "-----------renaming-----------";
  let renaming, ds = Renaming.rename ds in
  print_if_verbose ds;
  print_endline "-------Eliminating modules---------";
  let ds = ModuleElimination.eliminate_prog ds in
  print_if_verbose ds;
  print_endline "-------Eliminating type aliases 2---------";
  let ds = ReplaceUserTys.replace_prog ds in
  print_if_verbose ds;
  print_endline "-------Inlining size declarations---------";
  let ds = SizeInlining.replace_prog ds in
  print_if_verbose ds;
  print_endline "---------Eliminating events with global arguments----------";
  let ds = GlobalArgElimination.eliminate_prog ds in
  print_if_verbose ds;
  print_endline "-----------inlining functions-----------";
  let ds = FunctionInlining.inline_prog ds in
  print_if_verbose ds;
  print_endline "---------------typing again-------------";
  let ds = Typer.infer_prog ds in
  print_endline "------------Checking entry handlers---------------";
  Linerate.check ds;
  (* Record elimination removes useful debugging information, so we want it as
     close to the end of the pipeline as possible. *)
  print_endline "-------Eliminating records-------";
  let ds = RecordElimination.eliminate_prog ds in
  print_if_verbose ds;
  print_endline "-------Eliminating vectors-------";
  let ds = VectorElimination.eliminate_prog ds in
  print_if_verbose ds;
  print_endline "-------Eliminating tuples-------";
  let ds = TupleElimination.eliminate_prog ds in
  print_if_verbose ds;
  print_endline "---------------typing again-------------";
  (* Just to be safe *)
  let ds = Typer.infer_prog ds in
  print_if_verbose ds;
  renaming, ds
;;
