(* initial C backend, for compiling function programs *)
open BackendLogging

let print_if_debug str = if Cmdline.cfg.debug then Console.report str ;;

let report_if_verbose str = 
  if (Cmdline.cfg.verbose)
    then Console.show_message str ANSITerminal.Green "C backend"
;;

let print_pause = false ;;
let printprog_if_debug ds =
  if (Cmdline.cfg.debug)
    then 
      print_endline (CorePrinting.decls_to_string ds);
      if (print_pause)
        then (
          print_endline "[debug] press enter key to continue";
          ignore (read_line ())
        )
;;

let compile ds =  
  let ds = SyntaxToCore.translate_prog ds in
  report_if_verbose "-------Translated to Midend IR---------";
  printprog_if_debug ds;
  (* partial interpretation... maybe.. this reduces readability. *)
  let ds = PartialInterpretation.interp_prog ds in
  report_if_verbose "-------Partially Interpreted---------";
  printprog_if_debug ds;
  (* some simple transformations *)
  let ds = CTransformations.transform ds in 
  report_if_verbose "-------Translating to C---------";
  let c_str = CTranslate.translate ds in
  c_str
;;
