(* translate a frontend program into a C program *)

let fcore_passes fds = fds 
;;




let compile ds = 
  (* 1. translate to core syntax *)
  let ds = SyntaxToCore.translate_prog ~preserve_user_decls:true ds in
  (* 2. a few core passes *)
  let ds = PartialInterpretation.interp_prog ds in
  let ds = CoreRegularizeMemops.process ds in
  let ds = DeleteNoops.deleter#visit_decls () ds in
  (* 3. translate to FCore *)
  let fds = CoreToCCore.translate_prog ds in
  let s = CCorePPrint.decls_to_string fds in
  print_endline ("---- intial CCore ----");
  print_endline s;
  print_endline ("----------------------");
  (* implement tables *)
  let fds = CCoreTables.process_decls fds in
  (* implement arrays *)
  let fds = CCoreArrays.process_decls fds in
  print_endline ("---- after data structure implementation ----");
  print_endline (CCorePPrint.decls_to_string fds);
  print_endline ("----------------------");


  (* type check -- this will only pass after all the 
     builtin type / functions from Core are eliminated *)
  CheckFFuns.check fds;
  let fds = CCoreTyper.check_decls fds in
 
  (* implement arrays *)


  (* eliminate complex primitives and add misc builtins
      (match statements, hash, printf) *)

  (* add parser builtins *)

  (* add other misc builtins *)

  (* add event loop / event generation / continuation scaffolding *)
    (* (target specific) *)
  (* check for compatability with c *)  

  (* print as C *)

  let s = CCorePPrint.decls_to_string fds in
  print_endline ("---- after table implementation ----");
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
