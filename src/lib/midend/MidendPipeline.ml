let print_if_debug ds =
  if Cmdline.cfg.debug
  then (
    print_endline "decls: ";
    let str = CorePrinting.decls_to_string ds in
    Console.report str)
;;

let print_if_verbose str = if Cmdline.cfg.verbose then Console.report str
let report str = Console.show_message str ANSITerminal.Green "compiler"
let cfg = Cmdline.cfg
let enable_compound_expressions = true
let do_ssa = false

let optimize_simple_calls = ref true

let set_no_call_optimize () = 
  optimize_simple_calls := false
;;


let process_prog ?(for_interp = false) ds =
  print_if_verbose "-------Translating to core syntax---------";
  let ds = SyntaxToCore.translate_prog ds in
  print_if_debug ds;
  (* The rest of these transformations aren't necessary for the interpreter *)
  match for_interp with
  | true ->
    (* partial interpretation is causing execution tests to fail in P4. *)
    print_if_verbose "-------Partial interpreting---------";
    let ds = PartialInterpretation.interp_prog ds in
    print_if_debug ds;
    ds
  | false ->
    LogIr.log_lucid "midend_start.dpt" ds;
    let ds = if (!optimize_simple_calls)
      then (
        print_if_verbose "-------Optimizing simple calls--------";
        OptimizeSimpleCalls.eliminate_single_use_retvars ds
      )
      else (ds)
    in 
    print_if_verbose "-------Eliminating range relational ops--------";
    let ds = EliminateEqRangeOps.transform ds in
    (* temporary patches for incomplete features. *)
    let ds = PoplPatches.eliminate_noncall_units ds in
    let ds = PoplPatches.delete_prints ds in
    print_if_verbose "-------Adding default branches--------";
    let ds = AddDefaultBranches.add_default_branches ds in
    print_if_debug ds;
    print_if_verbose
      (if do_ssa
      then "-------SSA Transform--------"
      else "-------Partial SSA Transform--------");
    let ds =
      if do_ssa
      then SingleAssignment.transform ds
      else PartialSingleAssignment.const_branch_vars ds
    in
    print_if_debug ds;
    let ds =
      match enable_compound_expressions with
      | true ->
        print_if_verbose "-------Removing compound expressions--------";
        let ds = PrecomputeArgs.precompute_args ds in
        (* get rid of boolean expressions *)
        let ds = EliminateBools.do_passes ds in
        (* convert integer operations into atomic exps *)
        let ds = NormalizeInts.do_passes ds in
        ds
      | false ->
        print_if_verbose "-------Only eliminating bools--------";
        (* get rid of boolean expressions, but don't do the other
         transformations that normalize expression format. *)
        let ds = EliminateBools.do_passes ds in
        (* let ds = EliminateBools.elimination_only ds in  *)
        ds
    in
    print_if_debug ds;
    (* give all the spans in a program unique IDs. This should be a middle pass, before translate. *)
    let ds = UniqueSpans.make_unique_spans ds in
    (* make sure that all variables in the program have unique names. 
        for non-unique ids, bring the variable's number into the name *)
    let ds = UniqueIds.make_var_names_unique ds in 
    (* log the program at the end of the mid end *)
    LogIr.log_lucid "midend_end.dpt" ds;
    ds
;;
