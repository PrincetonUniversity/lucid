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
let set_no_call_optimize () = optimize_simple_calls := false

let process_prog ds =
  print_if_verbose "-------Translating to core syntax---------";
  let ds = SyntaxToCore.translate_prog ds in
  print_if_debug ds;
  let ds =
    if cfg.partial_interp
    then (
      print_if_verbose "-------Partial interpreting---------";
      let ds = PartialInterpretation.interp_prog ds in
      print_if_debug ds;
      ds)
    else ds
  in
  ds
;;
