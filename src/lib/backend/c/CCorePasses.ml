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

let test_core_translation ds = 
  (* translate into ccore syntax and back *)
  let cds = CoreToCCore.translate ds in
  let ds' = CCoreToCore.translate cds in
  ds'

let compile ds = 
  (*** 1. translate to core syntax *)
  print_endline ("---- source program ----");
  print_endline@@Printing.decls_to_string ds;
  let ds = SyntaxToCore.translate_prog ~preserve_user_decls:true ds in
  (*** 2. a few passes in core *)
  print_endline ("---- core passes----");
  let ds = AddEthStartMain.process ds in
  (* events with an explicit Payload.t arg keep the input tail when deparsed (recorded in
     meta.has_payload); a no-payload event serializes only its header fields. Capture their
     names here, before implicit_payloads strips the Payload.t arg. *)
  let payload_event_names =
    let is_payload (ty : CoreSyntax.ty) = match ty.raw_ty with
      | CoreSyntax.TName(cid, _) -> Cid.equal cid Payloads.t_id
      | _ -> false
    in
    List.filter_map (fun (d : CoreSyntax.decl) -> match d.d with
      | CoreSyntax.DEvent(id, _, _, params) when List.exists (fun (_, ty) -> is_payload ty) params -> Some (fst id)
      | _ -> None)
      ds
  in
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
  print_endline@@CorePrinting.decls_to_string ds;
  let cds = CoreToCCore.translate ~payload_event_names ds in
  ccore_print_always "CoreToCCore.translate" cds;
  (* exit 1; *)
  CCoreSyntax.refresh_tydefs cds;
  let cds = CCoreTyper.check cds in
  ccore_print_always "CCoreTyper.check" cds;
  (* early gate: reject features the C backend doesn't support yet (sub-byte event
     fields, concatenation, saturating arithmetic) with clear errors, before any
     lowering. *)
  CCoreWellformedC.feature_gate cds;

  print_endline ("---- Lowering parsers and handlers ----");
  let cds = CCoreParse.process cds in
  let cds = CCoreHandlers.process cds in
  let cds = CCoreTyper.check cds in

  print_endline ("---- Implementing tables and arrays ----");
  let cds = CCoreTables.process cds in
  let cds = CCoreArrays.process cds in
  (* TODO: implement misc helpers (hash, printf) *)
  let cds = CCoreTyper.check cds in
  ccore_print_always "after code generation type checking" cds;

  
  (* DIAGNOSTIC (read-only): report pointer leaks across the planned
     value-semantics boundary, before any C-specific pass runs. *)
  CCoreValueSemantics.report "value-semantics boundary (handlers eliminated; events still abstract)" cds;

  (*** 6. small transformations for c-compatible form *)
  print_endline ("---- Normalizing code forms for c ----");
  (* "pointerize" steps below the boundary: events (TVariant/ECall CVariant/PVariant) ->
     tagged-union punning; value-semantic vectors (TVec/Idx) and the parser bytes
     ADT (TPacket + Peek/Skip/BytesOk) -> their pointer forms *)
  ccore_print_always "BEFORE CCoreHandlers.lower" cds;
  let cds = CCoreHandlers.lower cds in
  ccore_print_always "CCoreHandlers.lower" cds;
  let cds = CCoreVariants.lower cds in
  ccore_print_always "CCoreVariants.lower" cds;
  (* keep non-standard-width ints (int<n>, n not 8/16/32/64) in range: they print
     as their container, so mask every width-violating producer. Runs while
     Read/Peek are still ops (before CCoreParse.lower) so packet reads are masked
     at the entry too. *)
  let cds = CCoreMaskWidths.process cds in
  ccore_print_always "CCoreMaskWidths.process" cds;
  (* the system runtime (time/flood/hash) is foreign C; generate it below the
     boundary so the waist stays free of its byte-pointer helper *)
  let cds = CCoreSystem.process cds in
  let cds = CCoreCForm.lower_vecs cds in
  (* lower the packet codec to the packet_t pointer form: the deparser's Write ops
     + (event,bytes) calling convention, and the parsers' Peek/Skip + helpers. Runs
     after CCoreVariants.lower, since both need the event match already lowered. *)
  let cds = CCoreParse.lower cds in
  ccore_print_always "CCoreParse.lower" cds;
  let cds = CCoreCForm.normalize_matches cds in
  (* lower multi-value tuple unpacks (e.g. Table.lookup returning >1 field) to
     single assignments before C printing -- C has no multi-assign *)
  let cds = CCoreCForm.eliminate_tuple_assigns cds in
  (* name inlined structural types (C needs shared typedefs; the value-semantic
     half is fine with anonymous structs) *)
  let cds = CCoreCForm.name_types cds in
  let cds = CCoreCForm.normalize_struct_inits cds in
  let cds = CCoreCForm.delete_empty_tuples cds in
  let cds = CCoreCForm.declare_tuples cds in

  ccore_print "after deparse" cds;

  (* final type check *)
  (* let cds = CCoreTyper.check cds in *)
  CCoreWellformedC.all_checks cds;

  

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
