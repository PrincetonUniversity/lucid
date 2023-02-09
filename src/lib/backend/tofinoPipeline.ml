(* Tofino backend pipeline. *)
open TofinoCore

let fail_report str = 
  Console.show_message str ANSITerminal.Red "Tofino Checker"
;;

exception Error of string
let error s = raise (Error s)

let verbose = ref false
let do_log = ref true
let do_const_branch_vars = ref true

let cprint_endline s =
  if (!verbose)
  then (print_endline s)
;;

let cprint_prog label tds =
  cprint_endline label;
  tdecls_to_string tds |> cprint_endline;
  cprint_endline label
;;

let mk_ir_log_dirs () = 
    Core.Unix.mkdir_p !BackendLogging.irLogDir;
    Core.Unix.mkdir_p !BackendLogging.graphLogDir
;;

let ir_dump_path phasename = 
  !BackendLogging.irLogDir ^ "/" ^ phasename ^ ".dpt"
;;
let dbg_dump_core_prog phasename ds =
  if (!do_log)
  then (
    let outf = (open_out (ir_dump_path phasename)) in 
    Printf.fprintf outf "// after phase: %s" phasename;
    Printf.fprintf outf "%s" (CorePrinting.decls_to_string ds);
    flush outf)
;;  

(* run the tofino branch of MidendPipeline.ml *)
(* these passes are basically about converting 
   expressions into simpler forms, so that every 
   expression in the program can either be 
   1) evaluated by a single (s)ALU (for expressions over ints)
   2) mapped directly to tcam rules (for expressions over bools) *)
let tofino_midend_pipeline ds =
  let print_if_verbose = MidendPipeline.print_if_verbose in
  print_if_verbose "-------Eliminating extern calls--------";
  let ds = EliminateExterns.eliminate_externs ds in 
  print_if_verbose "-------Eliminating value cast ops--------";
  let ds = EliminateValueCasts.eliminate_value_casts ds in 
  print_if_verbose "-------Eliminating range relational ops--------";
  let ds = EliminateEqRangeOps.transform ds in
  let ds = PoplPatches.eliminate_noncall_units ds in
  let ds = PoplPatches.delete_prints ds in
  print_if_verbose "-------Adding default branches--------";
  let ds = AddDefaultBranches.add_default_branches ds in
  MidendPipeline.print_if_debug ds;
  print_if_verbose "-------Breaking down compound expressions--------";
  let ds = if (!do_const_branch_vars)
    then ( PartialSingleAssignment.const_branch_vars ds)
    else (ds)
  in
(*   dbg_dump_core_prog "BeforeConstBranchVars" ds;
  dbg_dump_core_prog "AfterConstBranchVars" ds; *)
  (* MidendPipeline.print_if_debug ds; *)
  print_if_verbose "-------Breaking down compound expressions--------";
  (* let ds = EliminateFloods.eliminate_floods ds in  *)
  let ds = PrecomputeArgs.precompute_args ds in
  (* get rid of boolean expressions *)
  let ds = EliminateBools.do_passes ds in
  (* convert integer operations into atomic exps *)
  let ds = NormalizeInts.do_passes ds in
  MidendPipeline.print_if_debug ds;
  (* give all the spans in a program unique IDs *)
  let ds = UniqueSpans.make_unique_spans ds in
  (* make sure that all variables in the program have unique names. 
      for non-unique ids, bring the variable's number into the name *)
  let ds = UniqueIds.make_var_names_unique ds in 
  ds
;;

(* a bit of cleanup. Events are not handled well here. *)
let core_passes ds = 
    mk_ir_log_dirs ();
    dbg_dump_core_prog "midend" ds;
    let ds = EliminateEventCombinators.process ds in
    (* 1. make sure handlers always have the same params as their events *)
    let ds = UnifyHandlerParams.rename_event_params ds in 
    let ds = UnifyHandlerParams.unify_event_and_handler_params ds in 
    (* let ds = AlignEventParams.process ds in *)
    (* 2. convert exit events to regular events *)
    let ds = EliminateExitEvents.process ds in 
    (* 3. inline event variables. NOTE: this pass is broken for 
          event variables that are changed conditionally in 
          subsequent control flow. *)
    let ds = InlineEventVars.inline ds in 
    ds
;;
(* normalize code and eliminate compile-time abstractions that are easier 
   to deal with in tofinocore syntax *)
let tofinocore_normalization full_compile tds = 
    cprint_prog "----------- initial tofinoCore program------- " tds;
    (* 1. tag match statements with many cases as solitary, 
          i.e., they compile to their own table. *)
    let tds = SolitaryMatches.process tds 20 in
    (* 2. convert if statements to match statements. *)
    let tds = IfToMatch.process tds in 
    cprint_prog "----------- after IfToMatch ------- " tds;
    (* 3. regularize memop and Array update call formats *)
    let tds = RegularizeMemops.process tds in 
    cprint_prog "----------- after RegularizeMemops ------- " tds;
    (* 4. ensure that the memops to each register only reference two input variables *)
    (* TofinoCore.dump_prog (!BackendLogging.irLogDir ^ "before_reg_alloc.tofinocore.dpt") tds; *)
    let tds = ShareMemopInputs.process tds in 
    cprint_prog "----------- after ShareMemopInputs ------- " tds;
    (* 5. eliminate all generate statements and add invalidate calls *)
    let tds = if (full_compile) then (Generates.eliminate tds) else tds in 
    cprint_prog "----------- after Generates.eliminate ------- " tds;
    TofinoCore.dump_prog (!BackendLogging.irLogDir ^ "/initial.before_layout.dpt") tds;
    (* 6. transform code so that there is only 1 match 
          statement per user-defined table. *)
    (* WARNING: SingleTableMatch must come after generates.eliminate, because it 
                changes the form of the program. *)
    let tds = SingleTableMatch.process tds in
    cprint_prog "----------- after SingleTableMatch ------- " tds;
    (* 7. create per-table copies of actions. *)
    let tds = InlineTableActions.process tds in
    cprint_prog "----------- after InlineTableActions ------- " tds;
    tds
;;

(* transform the tofinocore program into a 
   straightline of match statements *)
let layout tds build_dir_opt =
    (* 1. compute control flow graph for main handler *)
    let cfg = CoreCfg.cfg_of_main tds in 
    CoreCfg.print_cfg ((!BackendLogging.graphLogDir)^"/cfg.dot") cfg;
    (* 2. compute control dependency graph *)
    let cdg = CoreCdg.to_control_dependency_graph cfg in        
    CoreCfg.print_cfg ((!BackendLogging.graphLogDir)^"/cdg.dot") cdg;

    (* 3. compute data flow / dependency graph *)
    let dfg = CoreDfg.process cdg in 
    CoreDfg.print_dfg ((!BackendLogging.graphLogDir)^"/dfg.dot") dfg;
    (* 4. lay out the dfg on a pipeline of match stmt seqs *)
    print_endline "-------- layout ----------";
    let tds = CoreLayout.process tds dfg in
    (match build_dir_opt with 
        | Some build_dir -> CoreLayout.profile tds build_dir;
        | _ -> ()
    );
    cprint_prog "----------- after layout ------- " tds;
    TofinoCore.dump_prog (!BackendLogging.irLogDir ^ "/laid_out.tofinocore.dpt") tds;
    (* 5. put branches of match statements into actions. If a branch calls a 
           table_match, which cannot be put into an action, then 
           rewrite the match statement as an if expression. *)
    let tds = ActionForm.process tds in 
    (* 6. deduplicate actions that contain certain expensive operations. *)
    let tds = Dedup.process tds in 
    TofinoCore.dump_prog (!BackendLogging.irLogDir ^ "/laid_out.actionform.tofinocore.dpt") tds;
    tds 
;;

let compile ds portspec build_dir = 
    if (!do_log) then (
        CoreCdg.start_logging ();
        CoreDfg.start_logging ();
    );

    let ds = tofino_midend_pipeline ds in 
    (* static analysis to see if this program is compile-able *)
    InputChecks.all_checks ds;
    cprint_endline "starting transformations";
    (* some transformations in the core syntax *)
    let ds = core_passes ds in 
    (* translate into tofinocore -- basically just coresyntax 
       with labeled statements, shared variables,and a main handler *)
    let tds = tdecls_of_decls ds in 
    (* TofinoCore.dump_prog (!BackendLogging.irLogDir ^ "/initial.tofinocore.dpt") tds; *)
    (* some transformation passes in tofinocore syntax *)
    let tds = tofinocore_normalization true tds in 
    (* transform program into a layout of match statements *)
    let tds = layout tds (Some build_dir) in 
    (* translate to final low-level P4-ish IR *)
    let tofino_prog = CoreToP4Tofino.translate tds portspec in 
    (* print data and control plane components *)
    let p4_str = P4TofinoPrinting.p4_of_prog tofino_prog in 
    let py_str = ControlPrinter.pyctl_of_prog tofino_prog in
    let cpp_str = ControlPrinter.cppctl_of_prog tofino_prog in

    p4_str, cpp_str, py_str
;;

(* compile a program with a single handler and only 1 generate 
   statement into a p4 control block, to be used as a module 
   in other P$ programs. *)
let compile_handler_block ds =
    InputChecks.all_checks ds;
    let ds = core_passes ds in
    let tds = tdecls_of_decls ds in 
    let tds = tofinocore_normalization false tds in 
    let tds = layout tds None in 
    let p4decls = CoreToP4Tofino.translate_to_control_block tds in 
    P4TofinoPrinting.string_of_decls p4decls |> P4TofinoPrinting.doc_to_string
;;
