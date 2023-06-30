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

let dump_prog fn comment prog = 
  let outf = open_out fn in
  Printf.fprintf outf "// %s" comment;
  Printf.fprintf outf "%s" (TofinoCorePrinting.prog_to_string prog);
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
  let ds = UniqueTableActions.process ds in
  ds
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

(* Translate the program into the new TofinoCore. This: 
   - splits the program (ingress, egress, parser(s), control program (eventually))
   - puts the data plane components (ingress and egress) into atomic operation form *)
let tofinocore_prep ds = 
  (* 1. split into control and data plane components. *)
  (* TODO: figure out how to only apply atomic_op_form passes to 
           handlers in the data plane (HData or HEgress). 
           Probably the easiest way is to just extend the 
            atomic_op_form passes, but there are a lot of passes...
          or maybe we could split the 
            declarations here and then re-merge them...
          or maybe we could delay atomic_op_form until 
            after the split? But the split happens in tofinocore, 
            which is a bit later...*)
  (* let control_component_support = false in
  let _, data_ds = if (control_component_support)
    then TofinoControl.split_program 196 9 ds
    else ([], ds) 
  in *)
  let data_ds = ds in
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

  let core_prog = AddHandlerTypes.type_handlers core_prog in
  print_endline ("--- transformed handlers into event-function form ---");

  let core_prog = MergeHandlers.merge_handlers core_prog in
  print_endline ("--- merged component handlers ---");

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

  let core_prog = SingleTableMatch.process_core core_prog in
  let core_prog = ActionsToFunctionsNew.process_core core_prog in
  core_prog
;;


(* layout the ingress and egress components of a tofinocore prog *)
let layout_new old_layout (prog : TofinoCoreNew.prog) build_dir_opt = 
  (* we want to do the layout pipeline for each component *)
  let layout_component (comp:TofinoCoreNew.component) = 
    (* skip component with sort HControl -- it does not need layout *)
    if (comp.comp_sort = TofinoCoreNew.HControl) then comp
    else
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
    CoreDfg.print_dfg (logging_prefix ^ "_dfg.dot") dfg;
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

(* main compilation function *)
let compile old_layout ds portspec build_dir ctl_fn_opt =


  if !do_log
  then (
    CoreCdg.start_logging ();
    CoreDfg.start_logging ());
  let ds = common_midend_passes ds in


  (* add extern record type definitions -- 
     these records represent tofino intrinsic 
     metadata structures defined in the tofino's
     p4 architectural specification files. 
     An "extern record type" means a record type 
     that the compiler knows the name and definition of, 
     but does not serialize to P4 because it is 
     included elsewhere. *)
  let ds = AddIntrinsics.add_intrinsics ds in
  (* assign each event a number, if it is not already set *)
  let ds = AddIngressParser.set_event_nums ds in 
  (* generate the ingress parser or add background event parsing *)
  let ds = AddIngressParser.add_parser portspec ds in
  (* static analysis to see if this program is compile-able *)
  InputChecks.all_checks ds;

  (* after this point, there should not be any changes
       to the globals, actions, event, or handlers defined
       in the program.
     The program should be in a form where:
      - each action is used by at most 1 table *)

  (* do final preparation for tofinocore: atomize expressions. *)
  (* NOTE: this currently removes any control-tagged 
     handlers, which are not supported. *)
  let ds = tofinocore_prep ds in

  (* translate into TofinoCore IR *)
  let core_prog = to_tofinocore ds in

  (* add the egress parser. This could go anywhere in tofinocore
     passes. *)
  let core_prog = AddEgressParser.add_parser core_prog in  

  let core_prog = GeneratesNew.eliminate_generates portspec.recirc_dpid core_prog in
  dump_prog 
    "parsers_added_generates_eliminated.dpt" 
    "after parsers have been added and generates have been eliminated in handlers"
    core_prog;
  (* optimize parsers to read headers directly into event parameters.
    This should reduce phv constraints. *)
  let core_prog = ParseOptimizer.parser_passes core_prog in 

  (* some more transformations *)
  let core_prog = tofinocore_normalization_new core_prog in
  print_endline ("----- before layout -----");
  TofinoCorePrinting.prog_to_string core_prog |> print_endline;


  (* now do layout, then put code into actionform and dedup actions *)
  let core_prog = layout_new old_layout core_prog None in

  (* print_endline ("----- final tofinocore program -----");
  TofinoCorePrinting.prog_to_string core_prog |> print_endline; *)

  (* finally, translate into the P4 ir *)
 (* let tofino_prog = CoreToP4TofinoNew.translate_core portspec core_prog in *)
  (* let _ = tofino_prog in  *)
  print_endline ("----- translating to p4-tofino ir -----");
  let tofino_prog = TofinoCoreToP4.translate_prog core_prog in
  (* error "not done" *)

   (* generate the python event library *)
  let py_eventlib =
    if Cmdline.cfg.serverlib then PyEventLib.coresyntax_to_pyeventlib ds else ""
  in
  print_endline ("python eventlib processing complete...");
  (* build the globals name directory json *)
  let globals_directory =
    P4TofinoGlobalDirectory.build_global_dir tofino_prog
    |> Yojson.Basic.pretty_to_string
  in
  print_endline ("global directory processing complete...");
  print_endline ("globals directory:\n" ^ globals_directory);
  (* print everything to strings *)
  let p4 = P4TofinoPrinting.p4_of_prog tofino_prog in
  let py_ctl = ControlPrinter.pyctl_of_prog tofino_prog ctl_fn_opt in
  let cpp_ctl = ControlPrinter.cppctl_of_prog tofino_prog in
  p4, cpp_ctl, py_ctl, py_eventlib, globals_directory
;;

(* DEPRECIATED compile a program with a single handler and only 1 generate
   statement into a p4 control block, to be used as a module
   in other P4 programs. *)
