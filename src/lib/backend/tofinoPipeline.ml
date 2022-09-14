(* Tofino backend pipeline. *)
open TofinoCore

exception Error of string
let error s = raise (Error s)


let verbose = ref true

let cprint_endline s =
  if (!verbose)
  then (print_endline s)
;;

let cprint_prog label tds =
  cprint_endline label;
  tdecls_to_string tds |> cprint_endline;
  cprint_endline label
;;

let process_prog ds portspec build_dir = 
    (* 0. make sure handlers always have the same params as their events *)
    let ds = UnifyHandlerParams.rename_event_params ds in 
    let ds = UnifyHandlerParams.unify_event_and_handler_params ds in 
    let ds = AlignEventParams.process ds in
    let ds = EliminateExitEvents.process ds in 
    (* 1. inline event variables. *)
    let ds = InlineEventVars.inline ds in 
    (* 2. Translate the program into the tofinoCore ir, with a merged main handler *)
    let tds = tdecls_of_decls ds in 
    TofinoCore.dump_prog (!BackendLogging.irLogDir ^ "/initial.tofinocore.dpt") tds;
    cprint_prog "----------- initial tofinoCore program------- " tds;
    (* 3. tag wide match statements as solitary *)
    let tds = SolitaryMatches.process tds 20 in
    (* 4. remove if statements *)
    let tds = IfToMatch.process tds in 
    cprint_prog "----------- after IfToMatch ------- " tds;
    (* 5. regularize memop and Array update call formats *)
    let tds = RegularizeMemops.process tds in 
    cprint_prog "----------- after RegularizeMemops ------- " tds;
    (* 6. ensure that the memops to each register only reference two input variables *)
    (* TofinoCore.dump_prog (!BackendLogging.irLogDir ^ "before_reg_alloc.tofinocore.dpt") tds; *)
    let tds = ShareMemopInputs.process tds in 
    cprint_prog "----------- after ShareMemopInputs ------- " tds;
    (* 7. eliminate all generate statements and add invalidate calls *)
    let tds = Generates.eliminate tds in 
    cprint_prog "----------- after Generates.eliminate ------- " tds;
    TofinoCore.dump_prog (!BackendLogging.irLogDir ^ "/initial.before_layout.dpt") tds;

    (**** instruction layout ****)
    (* 8. compute control flow graph for main handler *)
    let cfg = CoreCfg.cfg_of_main tds in 
    (* 9. compute control dependency graph *)
    let cdg = CoreCdg.to_control_dependency_graph cfg in        
    (* 10. compute data flow / dependency graph *)
    let dfg = CoreDfg.process cdg in 
    CoreDfg.print_dfg ((!BackendLogging.graphLogDir)^"/dfg.dot") dfg;
    (* 11. lay out the dfg on a pipeline of match stmt seqs *)
    print_endline "-------- layout ----------";
    let tds = CoreLayout.process tds dfg in
    CoreLayout.profile tds build_dir;
    cprint_prog "----------- after layout ------- " tds;

    TofinoCore.dump_prog (!BackendLogging.irLogDir ^ "/laid_out.tofinocore.dpt") tds;
    (* 12. put each branch into a labeled statement *)
    let tds = ActionForm.process tds in 
    (* 12.1 deduplicate certain expensive operation within the labeled statements *)
    let tds = Dedup.process tds in 
    cprint_prog "----------- after action extraction ------- " tds;
    (*** 13. translate to P4-like IR ***)
    let tofino_prog = CoreToP4Tofino.translate tds portspec in 
    (*** 14. Print programs ***)
    (* now, print out the p4, c, and python components *)
    let p4_str = P4TofinoPrinting.p4_of_prog tofino_prog in 
    let py_str = ControlPrinter.pyctl_of_prog tofino_prog in
    let cpp_str = ControlPrinter.cppctl_of_prog tofino_prog in

(*     print_endline "----- p4 program -----";
    print_endline p4_str;

    print_endline "----- python controller -----";
    print_endline py_str; *)
    p4_str, cpp_str, py_str

;;

(* compile the single handler of ds into a control block. *)
let process_handler_block ds = 
    (* let config = load_config () in  *)
    let ds = UnifyHandlerParams.unify_event_and_handler_params ds in 
    let ds = InlineEventVars.inline ds in 
    let tds = tdecls_of_decls ds in 
    let tds = SolitaryMatches.process tds 20 in
    let tds = IfToMatch.process tds in 
    let tds = RegularizeMemops.process tds in 
    let tds = ShareMemopInputs.process tds in 
    (* let tds = Generates.eliminate tds in  *)
    (**** instruction layout ****)
    let cfg = CoreCfg.cfg_of_main tds in 
    let cdg = CoreCdg.to_control_dependency_graph cfg in        
    let dfg = CoreDfg.process cdg in 
    print_endline "-------- layout ----------";
    let tds = CoreLayout.process tds dfg in
    let tds = ActionForm.process tds in 
    let p4decls = CoreToP4Tofino.translate_to_control_block tds in 
    P4TofinoPrinting.string_of_decls p4decls |> P4TofinoPrinting.doc_to_string
;;
