(* compile a frontend program into a C program *)

(* unimplemented:    
    - pairarrays
    - payloads
    - counters
    - packet duplication / multicast (not sure if we want)
*)

open CCoreExceptions

let ccore_print phase_str decls = 

  if Config.base_cfg.debug then 
  (
    print_endline ("---- "^phase_str^" ----");
    print_endline@@CCorePPrint.decls_to_string decls;
    print_endline ("------------------------")
  )
;;

let ccore_print_always phase_str decls = 
    print_endline ("---- "^phase_str^" ----");
    print_endline@@CCorePPrint.decls_to_string decls;
    print_endline ("------------------------")
;;

let compile ds =
  (*** 1. translate to core syntax *)
  print_endline ("---- translating to core ----");
  let ds = SyntaxToCore.translate_prog ~preserve_user_decls:true ds in
 
  print_endline ("---- core passes----");
  let ds = AddEthStartMain.process ds in
  let payload_event_names, ds = MiscCorePasses.implicit_payloads ds in
  let ds = MiscCorePasses.set_event_nums ds in
  let ds = CoreRegularizeMemops.process ds in
  let ds = PartialInterpretation.interp_prog ds in
  let ds = AddIngressParser.add_simple_parser None ds in 
  let ds = MiscCorePasses.this_eliminator#visit_decls () ds in
  let ds = MiscCorePasses.delete_event_combinators ds in
  let ds = MiscCorePasses.noop_deleter#visit_decls () ds in
  let ds = MiscCorePasses.pack_hash_args#visit_decls () ds in

  print_endline ("---- translating to CCore ----");
  print_endline@@CorePrinting.decls_to_string ds;
  let cds = CoreToCCore.translate ~payload_event_names ds in

  print_endline ("---- checking types and feature compatibility ----");
  let cds = CCoreTyper.check cds in
  CCoreWellformed.check_ccore_compat cds;

  print_endline ("---- Lowering parsers and handlers ----");
  let cds = CCoreParse.process cds in
  let cds = CCoreHandlers.process cds in
  let cds = CCoreTyper.check cds in
  print_endline ("---- Implementing tables and arrays ----");
  let cds = CCoreTables.process cds in
  let cds = CCoreArrays.process cds in
  let cds = CCoreTyper.check cds in
  ccore_print_always "---- After table gen ----" cds;

  (* Make sure there's no references or pointers by this stage *)
  CCoreWellformed.check_no_ptrs cds;

  (* simple builtins as foreign functions *)
  let cds = CCoreSystem.process cds in

  (*** 6. lowering to C-compatible form *)
  print_endline ("---- Normalizing code forms for c ----");
  (* handlers to functions *)
  let cds = CCoreHandlers.lower cds in
  (* variants to tagged unions *)
  let cds = CCoreVariants.lower cds in
  (* parser, expects variants to be lowered *)
  let cds = CCoreParse.lower cds in
  (* masks on non-standard int width operations *)
  let cds = CCoreMaskWidths.process cds in
  (* a bunch of small transformations *)
  let cds = CCoreCForm.lower_vecs cds in
  let cds = CCoreCForm.normalize_matches cds in
  let cds = CCoreCForm.eliminate_tuple_assigns cds in
  let cds = CCoreCForm.name_types cds in
  let cds = CCoreCForm.normalize_struct_inits cds in
  let cds = CCoreCForm.delete_empty_tuples cds in
  let cds = CCoreCForm.declare_tuples cds in

  (* final type check *)
  (* let cds = CCoreTyper.check cds in *)
  CCoreWellformed.check_ccore_compat cds;

  (*** 8. add target-specific driver interface *)
  let progbundle = match CConfig.c_cfg.driver with
    | "lpcap" -> CCoreDriverPcap.package_prog cds
    | "dpdk" -> CCoreDriverDpdk.package_prog cds
    | "rawsock" -> CCoreDriverRawSocket.package_prog cds
    | d -> err (Printf.sprintf "unknown driver %s. valid options are: [lpcap (pcap driver); dpdk (dpdk driver); rawsock (raw-socket switch on real interfaces)]" d)
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
