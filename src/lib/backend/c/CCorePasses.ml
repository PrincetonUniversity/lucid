(* compile a frontend program into a C program *)

let ccore_print phase_str decls = 
  print_endline ("---- "^phase_str^" ----");
  print_endline@@CCorePPrint.decls_to_string decls;
  print_endline ("------------------------")
;;

let compile ds = 
  (*** 1. translate to core syntax *)
  let ds = SyntaxToCore.translate_prog ~preserve_user_decls:true ds in

  (*** 2. a few passes in core *)
  let ds = MiscCorePasses.this_eliminator#visit_decls () ds in
  let ds = PartialInterpretation.interp_prog ds in
  let ds = CoreRegularizeMemops.process ds in
  let ds = InlineEventVars.set_event_nums ds in
  let ds = AddIngressParser.add_simple_parser None ds in 
  let ds = MiscCorePasses.noop_deleter#visit_decls () ds in

  (*** 3. translate to CCore and some cleanup *)
  let cds = CoreToCCore.translate_prog ds in
  let cds = CCoreTNameToTAbstr.process cds in
  let cds = CCoreRenaming.unify_event_ids cds in
  ccore_print "initial CCore" cds;

  (*** 4. Code generation to implement builtins ***)
  let cds = CCoreParse.process cds in
  let cds = CCoreTables.process_decls cds in
  let cds = CCoreArrays.process_decls cds in
  (* TODO: implement misc helpers (hash, printf) *)
  ccore_print "after code generation" cds;

  (* type check *)
  CheckFFuns.check cds; (* foriegn function checking *)
  let cds = CCoreTyper.check_decls cds in
  ccore_print "after type checking" cds;

  (*** 5. eliminate events and handlers *)
  let cds = CCoreHandlers.process_decls cds in
  ccore_print "after handler elimination" cds;
  let cds = CCoreEvents.process cds in
  ccore_print "after event elimination" cds;
  let cds = CCoreTyper.check_decls cds in
  (*** 6. small transformations for c-compatible form *)
  let cds = CCoreCForm.normalize_matches cds in
  let cds = CCoreCForm.normalize_struct_inits cds in

  (* final type check *)
  CheckFFuns.check cds;
  let cds = CCoreTyper.check_decls cds in

  (*** 7. add toplevel driver functions *)
  (* let cds = CCoreDrivers.StdinDriver.process cds in *)
  (* CheckFFuns.check cds; *)
   
  (*** 8. print as C *)
  let s = CCorePPrint.decls_to_string cds in
  print_endline ("---- final CCore ----");
  print_endline s;
  print_endline ("----------------------");
  s
;;
