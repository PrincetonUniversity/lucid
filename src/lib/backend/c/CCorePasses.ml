(* compile a frontend program into a C program *)

let ccore_print phase_str decls = 

  if Cmdline.cfg.debug then 
  (
    print_endline ("---- "^phase_str^" ----");
    print_endline@@CCorePPrint.decls_to_string decls;
    print_endline ("------------------------")
  )
;;

let test_core_translation ds = 
  (* translate into ccore syntax, run type checker, 
     and then translate back into core *)
  let cds = CoreToCCore.translate_prog ds in
  (* let cds = CCoreTyper.check_decls cds in *)
  let ds' = CCoreToCore.translate_prog cds in
  ds'

let compile ds = 
  (*** 1. translate to core syntax *)
  let ds = SyntaxToCore.translate_prog ~preserve_user_decls:true ds in
  (*** 2. a few passes in core *)
  print_endline ("---- core passes----");
  let ds = MiscCorePasses.this_eliminator#visit_decls () ds in
  let ds = PartialInterpretation.interp_prog ds in
  let ds = CoreRegularizeMemops.process ds in
  let ds = InlineEventVars.set_event_nums ds in
  let ds = EliminateEventCombinators.process ds in
  let ds = AddIngressParser.add_simple_parser None ds in 
  let ds = MiscCorePasses.noop_deleter#visit_decls () ds in
  let ds = MiscCorePasses.pack_hash_args#visit_decls () ds in

  (*** 3. translate to CCore and some cleanup *)
  print_endline ("---- Translating to CCore ----");
  let cds = CoreToCCore.translate_prog ds in
  ccore_print "initial CCore" cds;
  let cds = CCoreTNameToTAbstr.process cds in
  let cds = CCoreRenaming.unify_event_ids cds in
  let cds = CCoreTyper.check_decls cds in

  (*** 4. Code generation to implement builtins ***)
  print_endline ("---- Implementing builtins ----");
  let cds = CCoreParse.process cds in
  let cds = CCoreTables.process_decls cds in
  let cds = CCoreArrays.process_decls cds in
  let cds = CCoreSystem.process cds in
  (* TODO: implement misc helpers (hash, printf) *)
  ccore_print "after code generation" cds;

  (* type check *)
  print_endline ("---- Type checking ----");
  CheckFFuns.check cds; (* foriegn function checking *)
  let cds = CCoreTyper.check_decls cds in

  (*** 5. eliminate events and handlers *)
  print_endline ("---- Eliminating events and handlers ----");
  let cds = CCoreHandlers.process_decls cds in
  let cds = CCoreEvents.process cds in
  let cds = CCoreTyper.check_decls cds in
  ccore_print "after event and handler generation" cds;

  (*** 6. small transformations for c-compatible form *)
  print_endline ("---- Normalizing code forms for c ----");
  let cds = CCoreCForm.normalize_matches cds in
  let cds = CCoreCForm.normalize_struct_inits cds in
  let cds = CCoreCForm.delete_empty_tuples cds in
  let cds = CCoreCForm.declare_tuples cds in

  (*** 7. toplevel function generation ***)
  (* these functions are outside of the user program, 
     and can just be tacked onto the end. *)
  (* deparser generation *)
  let cds = CCoreDeparse.process cds in
  (* packet handler generation (platform specific) *)
  let cds = CCorePacketHandler.process cds in

  (* toplevel driver function (not always necessary, platform specific) *)
  (* TODO 
      final steps: 
        - Tagged unions instead of enums + unions
        - c pretty printer
        - libpcap driver 
        - driver function in ccore, not just a string of c
        - feature completeness: 
          - pairarrays
          - payloads
          - counters
      then, the actual fun:
        - making it compatible with ebpf (parameterized?)
        - representing multiple threads
          - locks
          - multiple processes (probably don't want this)
          - steering
          - atomics
        - optimizations? 
  *)
  (* let cds = CCoreDrivers.StdinDriver.process cds in *)

  (* final type check *)
  (* CheckFFuns.check cds; *)
  ccore_print "RIGHT BEFORE FINAL TYPE CHECKING" cds;

  let cds = CCoreTyper.check_decls cds in
  
  (* add some includes (likely platform specific, may go in a particular pass) *)
  let cds = [
      CCoreSyntax.dinclude "<stdio.h>";
      CCoreSyntax.dinclude "<stdlib.h>";
      CCoreSyntax.dinclude "<stdint.h>"]
    @cds
  in

  (*** 8. print as C *)
  let s = CCorePPrint.decls_to_string cds in
  print_endline ("---- C compilation done ----");
  s
;;
