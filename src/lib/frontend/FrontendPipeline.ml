let print_if_debug ds =
  if Cmdline.cfg.debug
  then (
    print_endline "decls: ";
    let str = Printing.decls_to_string ds in
    Console.report str)
;;

let print_if_verbose str = if Cmdline.cfg.verbose then Console.report str

let process_prog builtin_tys ds =
  print_if_debug ds;
  print_if_verbose "-------Checking well-formedness---------";
  Wellformed.pre_typing_checks ds;
  print_if_debug ds;
  print_if_verbose "---------typing1---------";
  let ds = Typer.infer_prog builtin_tys ds in
  let ds = GlobalConstructorTagging.annotate ds in
  (* let ds = SourceTracking.init_tracking ds in  *)
  print_if_verbose "---------Concretizing symbolics-------------";
  let ds = SymbolicElimination.eliminate_prog ds in
  print_if_debug ds;
  print_if_verbose "---------Aliasing Modules-------------";
  let ds = ModuleAliasing.alias_prog ds in
  print_if_debug ds;
  print_if_verbose "---------Making returns explicit-------------";
  let ds = ExplicitReturns.adjust_returns ds in
  print_if_debug ds;
  print_if_verbose "-----------renaming-----------";
  let renaming, ds = Renaming.rename ds in
  print_if_debug ds;
  (* TODO: Might be nice to have an additional renaming pass earlier, so we
     can run the slot analysis immediately after typing *)
  print_if_verbose "-------Performing parser slot analysis---------";
  let slot_assignments = SlotAnalysis.analyze_prog ds in
  print_if_verbose "-------Eliminating modules---------";
  let ds = ModuleElimination.eliminate_prog ds in
  print_if_debug ds;
  print_if_verbose "-------Inlining size declarations---------";
  let ds = SizeInlining.replace_prog ds in
  print_if_debug ds;
  print_if_verbose "---------typing2---------";
  let ds = Typer.infer_prog builtin_tys ds in
  print_if_debug ds;
  print_if_verbose "-------Eliminating type aliases 2---------";
  let ds = ReplaceUserTys.replace_prog ds in
  print_if_debug ds;
  print_if_verbose "-----------inlining functions-----------";
  let ds = FunctionInlining.inline_prog ds in
  print_if_debug ds;
  print_if_verbose "-----------inlining tables-----------";
  let ds = TableInlining.eliminate_prog ds in
  print_if_debug ds;
  print_if_verbose "---------Eliminating events with global arguments----------";
  let ds = GlobalArgElimination.eliminate_prog ds in
  print_if_debug ds;
  print_if_verbose "---------------typing3-------------";
  let ds = Typer.infer_prog builtin_tys ds in
  print_if_debug ds;
  (* print_if_verbose "------------Checking entry handlers---------------";
  Linerate.check ds; *)
  print_if_verbose "-------Making user types concrete-------";
  let ds = ConcreteUserTypes.replace_prog ds in 
  print_if_debug ds;
  print_if_verbose "---------------typing4-------------";
  let ds = Typer.infer_prog builtin_tys ds in
  print_if_verbose "-------Eliminating vectors-------";
  let ds = VectorElimination.eliminate_prog ds in
  print_if_debug ds;
  print_if_verbose "---------------typing5-------------";
  let ds = Typer.infer_prog builtin_tys ds in
  print_if_debug ds;
  (* We might have duplicate variable names in EStmts that got copied during
     vector elimination *)
  print_if_verbose "-----------re-renaming-----------";
  let renaming', ds = Renaming.rename ds in
  print_if_debug ds;
  print_if_verbose "-------Eliminating EStmts-------";
  let ds = EStmtElimination.eliminate_prog ds in
  print_if_debug ds;
  print_if_verbose "---------------typing6-------------";
  let ds = Typer.infer_prog builtin_tys ds in
  (* Record elimination removes useful debugging information, so we want it as
     close to the end of the pipeline as possible. *)
  print_if_verbose "-------Eliminating records-------";
  let ds = RecordElimination.eliminate_prog ds in
  print_if_debug ds;
  print_if_verbose "---------------typing7-------------";
  let ds = Typer.infer_prog builtin_tys ds in
  print_if_verbose "-------Eliminating tuples-------";
  let ds = TupleElimination.eliminate_prog ds in
  print_if_debug ds;
  print_if_verbose "---------------typing8-------------";
  let ds = Typer.infer_prog builtin_tys ds in
  print_if_verbose "-------Inlining Constants-------";
  let ds = ConstInlining.inline_prog ds in
  print_if_debug ds;
  (* Not sure if this is still necessary *)
  print_if_verbose "-----------re-re-renaming-----------";
  let renaming'', ds = Renaming.rename ds in
  let renaming = Renaming.compose_envs [renaming; renaming'; renaming''] in
  print_if_debug ds;
  print_if_verbose "---------------typing again-------------";
  (* Just to be safe *)
  let ds = Typer.infer_prog builtin_tys ds in
  print_if_debug ds;
  (* TODO: Return these, maybe apply renaming to them or something *)
  ignore slot_assignments;
  renaming, ds
;;
