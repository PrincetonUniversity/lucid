(* compile a frontend program into a C program *)

(* unimplemented:    
    - pairarrays
    - payloads
    - counters
    - packet duplication / multicast (not sure if we want)
*)

open CCoreExceptions

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
  let ds = AddEthStartMain.process ds in
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
  (* exit 1; *)
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
  ccore_print "after event and handler generation type checking" cds;
  (*** 6. small transformations for c-compatible form *)
  print_endline ("---- Normalizing code forms for c ----");
  let cds = CCoreCForm.normalize_matches cds in
  let cds = CCoreCForm.normalize_struct_inits cds in
  let cds = CCoreCForm.delete_empty_tuples cds in
  let cds = CCoreCForm.declare_tuples cds in

  (*** 7. generate deparser (probably should come earlier) ***)
  let cds = CCoreDeparse.process cds in
  ccore_print "after deparse" cds;

  (* final type check *)
  (* let cds = CCoreTyper.check cds in *)
  CCoreWellformedC.all_checks cds;

  (*** 8. add target-specific driver interface *)
  let progbundle = match CCoreConfig.cfg.driver with 
    | "lpcap" -> CCoreDriverPcap.package_prog cds
    | "dpdk" -> CCoreDriverDpdk.package_prog cds
    | d -> err (Printf.sprintf "unknown driver %s. valid options are: [lpcap (pcap driver); dpdk (dpdk driver)]" d)
  in
  (*** 9. print as C *)
  let progbundle = List.map (fun (fn, contents) -> match contents with 
    | `Decls(decls) -> fn,CCoreCPrint.decls_to_string decls
    | `String(s) -> fn,s
    | _ -> err "unexpected contents")
    progbundle
  in
  print_endline ("---- C compilation done ----");
  progbundle
;;
