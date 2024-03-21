(* translate a frontend program into a C program *)

let fcore_passes fds = fds 
;;


let compile ds = 
  (* 1. translate to core syntax *)
  let ds = SyntaxToCore.translate_prog ~preserve_user_decls:true ds in
  (* 2. a few core passes *)
  let ds = PartialInterpretation.interp_prog ds in
  let ds = CoreRegularizeMemops.process ds in
  let ds = AddIngressParser.add_simple_parser None (InlineEventVars.set_event_nums ds) in 
    (*try to add parser for platform with no recirc port *)
  let ds = DeleteNoops.deleter#visit_decls () ds in
  (* 3. translate to FCore *)
  let fds = CoreToCCore.translate_prog ds in
  let fds = CCoreRenaming.unify_event_ids fds in
  let s = CCorePPrint.decls_to_string fds in
  print_endline ("---- intial CCore ----");
  print_endline s;
  print_endline ("----------------------");
  exit 1;
  (* 1: implement all the "builtins" of lucid *)
  (* data structures *)
  let fds = CCoreTables.process_decls fds in
  let fds = CCoreArrays.process_decls fds in
  (* misc helpers (hash, printf)  TODO *)
  (* parsing helpers + payload arguments  TODO*)

  print_endline ("---- after data structure implementation ----");
  print_endline (CCorePPrint.decls_to_string fds);
  print_endline ("----------------------");

  (* 2: eliminate events and handlers *)
  (* merge handlers to create event handler function
     and implement generate as setting output fields *)
  let fds = CCoreHandlers.process_decls fds in
  print_endline ("---- after handler to function transformation ----");
  print_endline (CCorePPrint.decls_to_string fds);
  print_endline ("----------------------");
  (* eliminate events by converting into 
     tagged unions of records *)
  let fds = CCoreEvents.process fds in
  print_endline ("---- after event elimination ----");
  print_endline (CCorePPrint.decls_to_string fds);
  print_endline ("----------------------");

  (* 3. put into c-normal form: TODO
      1. record expressions, record values, union expression, and union values 
         can only appear in local or global variable declarations. 
      2. no match statements (probably convert into nested ifs?) 
        
    - this must be done after event elimination, which generates 
      records and union types all over the place. 
  *)

  (* 4. type check -- this should pass any time after step 1 *)
  CheckFFuns.check fds;
  let fds = CCoreTyper.check_decls fds in
 
  (* 5. This must come after step 3, but it might as well 
        go at the end. *) 

  (* 6. print as C *)

  let s = CCorePPrint.decls_to_string fds in
  print_endline ("---- final CCore ----");
  print_endline s;
  print_endline ("----------------------");

  (* capture variables in closures *)
  (* let fds = ClosureConversion.capture_vars fds in *)
  (* add closure types *)
  (* let fds = ClosureConversion.add_closure_types fds in *)
  (* add closure conversion functions *)  
  (* print_endline ("--- after closure conversion ---"); *)
  (* let s = CCorePPrint.decls_to_string fds in *)
  (* let s = CCorePrinting.show_decls fds in *)
  s
(* 
  print_endline ("translation to FCore complete");
  FCorePrinting.decls_to_string fds  *)
  (* core_str *)
;;
