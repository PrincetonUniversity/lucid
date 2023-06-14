(* Tofino backend pipeline. *)
(* This is a new pipeline, based on the new TofinoCore (6/2023), 
   which adds event types (union and set), parsing, and a simple 
   architectural model to encode the way a program is split across 
   multiple components in the switch. *)

open TofinoCore
open BackendLogging
(* Disable warnings 21, 27, and 26 *)
[@@@ocaml.warning "-21-27-26"]
let do_log = ref true
let do_const_branch_vars = ref true


let cprint_prog label tds =
  cprint_endline label;
  tdecls_to_string tds |> cprint_endline;
  cprint_endline label
;;

let dbg_dump_core_prog phasename ds =
  if !do_log
  then (
    let outf = open_out (ir_dump_path phasename) in
    Printf.fprintf outf "// after phase: %s" phasename;
    Printf.fprintf outf "%s" (CorePrinting.decls_to_string ds);
    flush outf)
;;

let print_if_debug ds =
  if Cmdline.cfg.debug
  then (
    print_endline "decls: ";
    let str = CorePrinting.decls_to_string ds in
    Console.report str)
;;

(* perform midend passes that must be done before splitting
   the program. After this point, the ids of globals and actions
   should not change.  *)
(* input form: any CoreSyntax program
   output form: a CoreSyntax program with
    + no Event.delay calls
    + events and handlers have same parameter ids
    + no exit events
    + no event variables
    + each action belongs to 1 table *)
let common_midend_passes ds =
  mk_ir_log_dirs ();
  dbg_dump_core_prog "midend" ds;
  (* delete Event.delay calls and warn the user *)
  (* form: + no Event.delay calls *)
  let ds = EliminateEventCombinators.process ds in
  (*  for debugging, put the name of the event into each event parameter,
      then make sure events and handlers have the same parameter ids. *)
  (* form: + events and handlers have same parameter ids *)
  let ds = StandardizeEventParams.process ds in
  (* let ds = AlignEventParams.process ds in *)
  (* inline event variables. NOTE: this pass is broken for
        event variables that are changed conditionally in
        subsequent control flow. To fix, we should integrate
        the solution from Andrew's fork. *)
  (* form: + no event variables *)
  let ds = InlineEventVars.inline ds in
  (* make sure that each table uses unique actions *)
  (* form: + each action belongs to 1 table *)
  (* Why do this? Does something break without it? *)
  UniqueTableActions.process ds
;;

(* these passes are basically about converting
   expressions into simpler forms, so that every
   expression in the program can either be
   1) evaluated by a single (s)ALU (for expressions over ints)
   2) mapped directly to tcam rules (for expressions over bools) *)

(* input form: a CoreSyntax program with
    + no Event.delay calls
    + events and handlers have same parameter ids
    + no exit events
    + no event variables
    + each action belongs to 1 table
   output form: a CoreSyntax program with all of the above, and:
    + no casts of literals
    + unit statements may only be function calls
    + no print statements
    + every match statement has a default branch
    + No variable referenced in an if or match statement's condition mutated before the end of its branches
    + every argument in a function call is either a variable or value
    + in handlers, boolean operations only occur in if condition expressions
    + in handlers, if condition expressions are in disjunctive normal form, with atoms of the form (<var> == | != <value>)
    + there are no boolean variables (1-bit ints instead)
    + integer operations are all binary operations *)
let atomic_op_form ds =
  let print_if_verbose = MidendPipeline.print_if_verbose in
  print_if_verbose "-------Eliminating interpreter-only operations--------";
  (* form: + no calls to extern functions *)
  (* form: + unit statements may only be function calls *)
  (* form: + no print statements *)
  let ds = EliminateInterpOps.eliminate_prog ds in
  print_if_debug ds;
  print_if_verbose "-------Eliminating value cast ops--------";
  (* form: + no casts of literals *)
  (* convert casts of values into values, e.g., (int<<2>>)1 --> 1w2 *)
  (* FIXME: Shouldn't be necessary if we get partial interpretation working, but
            will still be needed if we allow users to disable partial interpretation *)
  let ds = EliminateValueCasts.eliminate_value_casts ds in
  print_if_debug ds;
  print_if_verbose "-------Adding default branches--------";
  (* form: + every match statement has a default branch *)
  let ds = AddDefaultBranches.add_default_branches ds in
  MidendPipeline.print_if_debug ds;
  print_if_debug ds;
  print_if_verbose
    "-------Making variables in if / match conditions constants--------";
  (* form: + any variable referenced in an if or match statement's
           condition is not mutated in its branches, except possibly
           as the very last statement. *)
  let ds = ImmutableConditions.make_conditions_immutable ds in
  print_if_debug ds;
  (*   dbg_dump_core_prog "BeforeConstBranchVars" ds;
  dbg_dump_core_prog "AfterConstBranchVars" ds; *)
  (* MidendPipeline.print_if_debug ds; *)
  print_if_verbose "-------Breaking down compound expressions--------";
  (* let ds = EliminateFloods.eliminate_floods ds in  *)
  (* form: + every argument in a function call is either a variable or value *)
  let ds = PrecomputeArgs.precompute_args ds in
  (* form:
    + in handlers, boolean operations only occur in if condition expressions
    + in handlers, if condition expressions are in disjunctive normal form,
      with atoms of the form (<var> == <value>) or (<var> != <value>)
    + there are no boolean variables (1-bit ints instead) *)
  let ds = EliminateBools.do_passes ds in
  (* form: + integer operations are all binary operations *)
  let ds = NormalizeInts.do_passes ds in
  MidendPipeline.print_if_debug ds;
  ds
;;

(* normalize code and eliminate compile-time abstractions that are easier
   to deal with in tofinocore syntax *)
let tofinocore_normalization is_ingress eliminate_generates tds =
  cprint_prog "----------- initial tofinoCore program------- " tds;
  (* 1. tag match statements with many cases as solitary,
          which means they get placed into their own table.
          This could just as well be in coreSyntax.  *)
  let tds = SolitaryMatches.process tds 20 in
  (* 2. convert if statements to match statements -- this could just as well be in coreSyntax *)
  let tds = IfToMatch.process tds in
  cprint_prog "----------- after IfToMatch ------- " tds;
  (* 3. regularize memop and Array update call formats -- could be in CoreSyntax *)
  let tds = RegularizeMemops.process tds in
  cprint_prog "----------- after RegularizeMemops ------- " tds;
  (* 4. ensure that the memops to each register only reference two input variables.
          We do this in TofinoCore because CoreSyntax doesn't have "locals" that can be globally scoped. *)
  (* TofinoCore.dump_prog (!BackendLogging.irLogDir ^ "before_reg_alloc.tofinocore.dpt") tds; *)
  let tds = ShareMemopInputs.process tds in
  cprint_prog "----------- after ShareMemopInputs ------- " tds;
  (* 5. partially eliminate generate statements. Unclear if this could be done in CoreSyntax. *)
  let tds = Generates.eliminate is_ingress tds in
  cprint_prog "----------- after Generates.eliminate ------- " tds;
  TofinoCore.dump_prog
    (!BackendLogging.irLogDir ^ "/initial.before_layout.dpt")
    tds;
  (* 6. transform code so that there is only 1 match
          statement in the entire program for each table.
          This has to be in TofinoCore because it is an operation on a merged control flow. *)
  let tds = SingleTableMatch.process tds in
  (* WARNING: SingleTableMatch must come after generates.eliminate, because it
                changes the form of the program. *)
  cprint_prog "----------- after SingleTableMatch ------- " tds;
  (* 7. convert actions into functions --
          this has to be in TofinoCore because there are no functions in coreSyntax =(  *)
  let tds = ActionsToFunctions.process tds in
  cprint_prog "----------- after ActionsToFunctions ------- " tds;
  tds
;;


(* Translate the program into the new TofinoCore. This: 
   - splits the program (ingress, egress, parser(s), control program (eventually))
   - puts the data plane components (ingress and egress) into atomic operation form *)
let tofinocore_prep ds = 
  (* 1. split into control and data plane components. *)
  let control_component_support = false in
  let _, data_ds = if (control_component_support)
    then TofinoControl.split_program 196 9 ds
    else ([], ds) 
  in
  (* 2. put all data plane code into atomic op form. *)
  let data_ds = atomic_op_form data_ds in
  let ds = data_ds in
  (* at this point:
      1. all the non-control flow statements in the program are
         either table_matches or atomic operations that can
         fit into a single ALU or SALU.
      2. all the control flow statements are either:
          1) match statements
          2) if statements where the expression is in a
             DNF form that is easy to convert into a match statement. *)

  (* Statements and variable names don't change much beyond this point,
     so we give everything a unique identifier. *)
    let ds = UniqueSpans.make_unique_spans ds in
    let ds = UniqueIds.make_var_names_unique ds in
    print_if_debug ds;
    ds
;;

(* transform into tofinocore, but don't layout yet *)
let to_tofinocore ds =
  let core_prog = TofinoCoreNew.core_to_tofinocore ds  in
  print_endline ("--- initial tofinocore program ---");
  print_endline (TofinoCorePrinting.prog_to_string core_prog);

  let core_prog = AddHandlerTypes.type_handlers core_prog in
  print_endline ("--- transformed handlers into event-function form ---");
  print_endline (TofinoCorePrinting.prog_to_string core_prog);

  let core_prog = MergeHandlers.merge_handlers core_prog in
  print_endline ("--- merged component handlers ---");
  print_endline (TofinoCorePrinting.prog_to_string core_prog);

  core_prog
;;
(* do a few more transformations in tofinocore. 
   most of these could be done earlier, except: 
    - ShareMemopInputs needs shared locals
    - SingleTableMatch needs a merged / main event handler *)
let tofinocore_normalization_new core_prog =
  (* the final transformations before layout *)
  (* SolitaryMatch; IfToMatch; RegularizeMemops; ShareMemopInputs;
     Generates.eliminate; SingleTableMatch; ActionsToFunctions *)
  let core_prog = SolitaryMatches.process_core 20 core_prog in
  let core_prog = IfToMatch.process_core core_prog in 
  let core_prog = RegularizeMemopsNew.process_core core_prog in
  let core_prog = ShareMemopInputsNew.process_core core_prog in
  let core_prog = GeneratesNew.eliminate_generates core_prog in
  let core_prog = SingleTableMatch.process_core core_prog in
  let core_prog = ActionsToFunctionsNew.process_core core_prog in
  core_prog
;;


(* layout the components of a tofinocore prog *)
let layout_new old_layout (prog : TofinoCoreNew.prog) build_dir_opt = 
  (* we want to do the layout pipeline for each component *)
  let layout_component (comp:TofinoCoreNew.component) = 
    let cn = comp.comp_id |> Id.name in
    let logging_prefix = !BackendLogging.graphLogDir ^ "/" ^ cn in 
    (* 1. compute control flow graph for main handler *)
    let cfg = CoreCfg.cfg_of_component_main comp.comp_decls in
    CoreCfg.print_cfg ( logging_prefix ^ "_cfg.dot") cfg;
    (* 2. compute control dependency graph *)
    let cdg = CoreCdg.to_control_dependency_graph cfg in
    CoreCfg.print_cfg ( logging_prefix ^ "_cdg.dot") cfg;
    (* 3. compute data flow / dependency graph *)
    let dfg = CoreDfg.process cdg in
    CoreDfg.print_dfg (logging_prefix ^ "/dfg.dot") dfg;
    (* 4. lay out the dfg on a pipeline of match stmt seqs *)
    print_endline "-------- layout ----------";
    let hdl_body = if old_layout
      then CoreLayoutOld.process_new comp.comp_decls dfg 
      else CoreLayout.process_new comp.comp_decls dfg 
    in
    let main_handler = TofinoCoreNew.main_handler_of_decls comp.comp_decls in
    let main_handler' = {main_handler with hdl_body = TofinoCoreNew.SPipeline(hdl_body);} in
    let comp = TofinoCoreNew.replace_main_handler_of_component comp main_handler' in 
    (* TODO: missing some IR dumps here *)
    (* finally, run the actionform and dedup transformation passes *)
    let comp = ActionFormNew.process_comp comp in
    (* 6. deduplicate actions that contain certain expensive operations. *)
    let comp = DedupNew.process_comp comp in
    comp 
  in
  List.map layout_component prog
;;

(* transform the tofinocore program into a
   straightline of match statements *)
let layout old_layout tds build_dir_opt =
  (* 1. compute control flow graph for main handler *)
  let cfg = CoreCfg.cfg_of_main tds in
  CoreCfg.print_cfg (!BackendLogging.graphLogDir ^ "/cfg.dot") cfg;
  (* 2. compute control dependency graph *)
  let cdg = CoreCdg.to_control_dependency_graph cfg in
  CoreCfg.print_cfg (!BackendLogging.graphLogDir ^ "/cdg.dot") cdg;
  (* 3. compute data flow / dependency graph *)
  let dfg = CoreDfg.process cdg in
  CoreDfg.print_dfg (!BackendLogging.graphLogDir ^ "/dfg.dot") dfg;
  (* 4. lay out the dfg on a pipeline of match stmt seqs *)
  print_endline "-------- layout ----------";
  (* let tds = CoreLayout.process tds dfg in *)
  let tds =
    if old_layout
    then CoreLayoutOld.process tds dfg
    else CoreLayout.process tds dfg
  in
  (* let tds = CoreLayoutNew.process_new tds dfg in *)
  (match build_dir_opt with
   | Some build_dir -> CoreLayout.profile tds build_dir
   | _ -> ());
  cprint_prog "----------- after layout ------- " tds;
  TofinoCore.dump_prog
    (!BackendLogging.irLogDir ^ "/laid_out.tofinocore.dpt")
    tds;
  (* 5. Take the statements in each match statement branch and put them into
          functions that represent actions. If a branch does a table_match,
          which cannot be executed in an action, we transform the outer match
        statement into an if statement that calls the table_match. *)
  let tds = ActionForm.process tds in
  (* 6. deduplicate actions that contain certain expensive operations. *)
  let tds = Dedup.process tds in
  TofinoCore.dump_prog
    (!BackendLogging.irLogDir ^ "/laid_out.actionform.tofinocore.dpt")
    tds;
  tds
;;

(* layout the program then translate into p4 IR. *)
let compile_dataplane old_layout ingress_tds egress_tds portspec build_dir =
  let ingress_tds = layout old_layout ingress_tds (Some build_dir) in
  let egress_tds = if List.length egress_tds <> 0
    then layout old_layout egress_tds (Some build_dir)
    else [] 
  in
  let tofino_prog = CoreToP4Tofino.translate portspec ingress_tds egress_tds in
  tofino_prog
;;

(* main compilation function *)
let compile old_layout ds portspec build_dir ctl_fn_opt =
  if !do_log
  then (
    CoreCdg.start_logging ();
    CoreDfg.start_logging ());
  let ds = common_midend_passes ds in
  (* static analysis to see if this program is compile-able *)
  InputChecks.all_checks ds;
  (* after this point, there should not be any changes
       to the globals, actions, event, or handlers defined
       in the program.
     The program should be in a form where:
      - each action is used by at most 1 table
      - event variables have been eliminated *)
  (* new IR change: event variables should still remain. *)
  (* new IR change: unwind / refactor all the complicated generate elimination *)

  (* do final preparation for tofinocore: split into ingress / egress, 
     atomize expressions. *)
  let ds = tofinocore_prep ds in

  (* translate into TofinoCore IR *)
  let core_prog = to_tofinocore ds in
  (* some more transformations *)
  let core_prog = tofinocore_normalization_new core_prog in
  (* now do layout, then put code into actionform and dedup actions *)
  let core_prog = layout_new old_layout core_prog in

  (* finally, translate into the P4 ir *)
  (* <<left off here>> 
    TODO: 
      1. update translation.
      2. rewrite parser in tofinoCore and translate that instead of generating new. *)
  let tofino_prog = CoreToP4TofinoNew.translate_core portspec core_prog in
  let _ = tofino_prog in 

  error "not done"

(*   (* generate the python event library *)
  let py_eventlib =
    if Cmdline.cfg.serverlib then PyEventLib.coresyntax_to_pyeventlib ds else ""
  in
  (* build the globals name directory json *)
  let globals_directory =
    P4TofinoGlobalDirectory.build_global_dir tofino_prog
    |> Yojson.Basic.pretty_to_string
  in
  (* print everything to strings *)
  let p4 = P4TofinoPrinting.p4_of_prog tofino_prog in
  let py_ctl = ControlPrinter.pyctl_of_prog tofino_prog ctl_fn_opt in
  let cpp_ctl = ControlPrinter.cppctl_of_prog tofino_prog in
  p4, cpp_ctl, py_ctl, py_eventlib, globals_directory *)
;;

(* DEPRECIATED compile a program with a single handler and only 1 generate
   statement into a p4 control block, to be used as a module
   in other P4 programs. *)
let compile_handler_block ds =
  InputChecks.all_checks ds;
  let ds = common_midend_passes ds in
  let tds = tdecls_of_decls ds in
  let tds = tofinocore_normalization true false tds in
  let tds = layout false tds None in
  let p4decls = CoreToP4Tofino.translate_to_control_block tds in
  P4TofinoPrinting.string_of_decls p4decls |> P4TofinoPrinting.doc_to_string
;;