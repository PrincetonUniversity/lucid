(* compile a frontend program into a C program *)

  (* TODO 
      final steps: 
*         1. eliminate tuple assign and local (and check?)
*         2. eliminate bit and tern types (probably in match elim) (and check?)
*         3. add logic to set ingress_port
          4. add port binding config
          5. printf
          6. payloads          
      - libpcap driver 
        - improvements: 
          - Tagged unions instead of enums + unions
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


let ccore_print phase_str decls = 

  if Cmdline.cfg.debug then 
  (
    print_endline ("---- "^phase_str^" ----");
    print_endline@@CCorePPrint.decls_to_string decls;
    print_endline ("------------------------")
  )
;;

let test_core_translation ds = 
  (* translate into ccore syntax and back *)
  let cds = CoreToCCore.translate ds in
  let ds' = CCoreToCore.translate cds in
  ds'

let compile ds = 
  (*** 1. translate to core syntax *)
  let ds = SyntaxToCore.translate_prog ~preserve_user_decls:true ds in
  (*** 2. a few passes in core *)
  print_endline ("---- core passes----");
  let ds = MiscCorePasses.implicit_payloads ds in
  let ds = MiscCorePasses.set_event_nums ds in
  let ds = CoreRegularizeMemops.process ds in
  let ds = PartialInterpretation.interp_prog ds in
  let ds = AddIngressParser.add_simple_parser None ds in 
  let ds = MiscCorePasses.this_eliminator#visit_decls () ds in
  let ds = MiscCorePasses.delete_event_combinators ds in
  let ds = MiscCorePasses.noop_deleter#visit_decls () ds in
  let ds = MiscCorePasses.pack_hash_args#visit_decls () ds in

  (*** 3. translate to CCore and some cleanup *)
  print_endline ("---- Translating to CCore ----");
  let cds = CoreToCCore.translate ds in
  ccore_print "initial CCore" cds;
  let cds = CCoreTNameToTAbstr.process cds in
  let cds = CCoreRenaming.unify_event_ids cds in
  let cds = CCoreTyper.check cds in

  (*** 4. Code generation to implement builtins ***)
  print_endline ("---- Implementing builtins ----");
  let cds = CCoreParse.process cds in
  let cds = CCoreTables.process cds in
  let cds = CCoreArrays.process cds in
  let cds = CCoreSystem.process cds in
  (* TODO: implement misc helpers (hash, printf) *)
  ccore_print "after code generation" cds;
  let cds = CCoreTyper.check cds in

  (*** 5. eliminate events and handlers *)
  print_endline ("---- Eliminating events and handlers ----");
  let cds = CCoreHandlers.process cds in
  let cds = CCoreEvents.process cds in
  let cds = CCoreTyper.check cds in
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

  (* final type check *)
  (* let cds = CCoreTyper.check cds in *)
  CCoreWellformedC.all_checks cds;

  let prog, cflags = CCoreDriverInterface.package (module CCoreDriverPcap) cds in

  (*** 8. print as C *)
  let s = CCoreCPrint.decls_to_string prog in
  print_endline ("---- C compilation done ----");
  s, cflags
;;
