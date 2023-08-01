(* Tofino backend pipeline. *)
(* This is a new pipeline, based on the new TofinoCore (6/2023), 
   which adds event types (union and set), parsing, and a simple 
   architectural model to encode the way a program is split across 
   multiple components in the switch. *)

open TofinoCore
open BackendLogging

let dump_ir_prog comment fn prog = 
  dump (IoUtils.ir_dump_path fn) comment (CorePrinting.decls_to_string prog)
;;
let dump_prog comment fn prog = 
  dump (IoUtils.ir_dump_path fn) comment (TofinoCorePrinting.prog_to_string prog)
;;


let print_if_debug str = if Cmdline.cfg.debug then Console.report str

let report_if_verbose str = 
  if (Cmdline.cfg.verbose)
    then Console.show_message str ANSITerminal.Green "Tofino backend"
;;

let print_pause = false ;;
let printprog_if_debug ds =
  if (Cmdline.cfg.debug)
    then 
      print_endline (CorePrinting.decls_to_string ds);
      if (print_pause)
        then (
          print_endline "[debug] press enter key to continue";
          ignore (read_line ())
        )
;;

let printtofcoreprog_if_debug prog = print_if_debug (TofinoCorePrinting.prog_to_string prog);;

(* transform into a form where each statement is atomic *)
let atomic_op_form ds =
  report_if_verbose "-------Eliminating interpreter-only operations--------";
  let ds = EliminateInterpOps.eliminate_prog ds in
  printprog_if_debug ds;
  report_if_verbose "-------Eliminating value cast ops--------";
  let ds = EliminateValueCasts.eliminate_value_casts ds in
  printprog_if_debug ds;
  report_if_verbose "-------Adding default branches--------";
  let ds = AddDefaultBranches.add_default_branches ds in
  MidendPipeline.print_if_debug ds;
  printprog_if_debug ds;
  report_if_verbose
    "-------Making variables in if / match conditions constants--------";
  let ds = ImmutableConditions.make_conditions_immutable ds in
  printprog_if_debug ds;
  report_if_verbose "-------Breaking down compound expressions--------";
  let ds = PrecomputeArgs.precompute_args ds in
  let ds = EliminateBools.do_passes ds in
  let ds = NormalizeInts.do_passes ds in
  MidendPipeline.print_if_debug ds;
  ds
;;

(* transform into tofinocore, but don't layout yet *)
let to_tofinocore ds =
  report_if_verbose "-------splitting program into ingress / egress components--------";
  let split_prog = SplitDataplane.split ds in 
  report_if_verbose "-------Translating to Tofino dialect of Midend IR---------";
  let core_prog = TofinoCoreNew.core_to_tofinocore split_prog  in
  report_if_verbose "-------Translating handlers into event-typed functions--------";
  let core_prog = AddHandlerTypes.type_handlers core_prog in
  report_if_verbose "-------Merging handlers and constructing union events--------";
  let core_prog = MergeHandlers.merge_handlers core_prog in
  core_prog
;;

(* layout the ingress and egress components of a tofinocore prog *)
let layout old_layout (prog : TofinoCoreNew.prog) = 
  let layout_info = ref [] in
  (* we want to do the layout pipeline for each component *)
  let layout_component (comp:TofinoCoreNew.component) = 
    (* skip component with sort HControl -- it does not need layout *)
    if (comp.comp_sort = TofinoCoreNew.HControl) then comp
    else
    let cn = comp.comp_id |> Id.name in
    let logging_prefix = !IoUtils.graphLogDir ^ "/" ^ cn in 
    report_if_verbose (Printf.sprintf "-------Layout for %s: computing control flow graph-------" cn);
    let cfg = CoreCfg.cfg_of_component_main comp.comp_decls in
    CoreCfg.print_cfg ( logging_prefix ^ "_cfg.dot") cfg;
    report_if_verbose (Printf.sprintf "-------Layout for %s: computing control dependency graph-------" cn);
    let cdg = CoreCdg.to_control_dependency_graph cfg in
    CoreCfg.print_cfg ( logging_prefix ^ "_cdg.dot") cfg;
    report_if_verbose (Printf.sprintf "-------Layout for %s: computing data dependency graph-------" cn);
    let dfg = CoreDfg.process cdg in
    CoreDfg.print_dfg (logging_prefix ^ "_dfg.dot") dfg;
    report_if_verbose (Printf.sprintf "-------Layout for %s: scheduling data dependency graph to pipeline-------" cn);
    let pipeline_stmts = if old_layout
      then CoreLayoutOld.process_new comp.comp_decls dfg 
      else CoreLayout.process_new comp.comp_decls dfg 
    in
    let num_stages = List.length(pipeline_stmts) in
    layout_info := (!layout_info)@[(cn, (string_of_int num_stages))];
    let main_handler = TofinoCoreNew.main_handler_of_decls comp.comp_decls in
    let main_handler' = {main_handler with hdl_body = TofinoCoreNew.SPipeline(pipeline_stmts);} in
    let comp = TofinoCoreNew.replace_main_handler_of_component comp main_handler' in 
    dump_prog "tofinocore after layout (final)" (Printf.sprintf "tofinocore_%s_layout_pre_actionform" cn) [comp];

    report_if_verbose (Printf.sprintf "-------Layout for %s: wrapping table branches in functions-------" cn);
    let comp = ActionFormNew.process_comp comp in
    report_if_verbose (Printf.sprintf "-------Layout for %s: deduplicating table branch functions-------" cn);
    let comp = DedupNew.process_comp comp in
    comp 
  in
  let res = List.map layout_component prog in
  (* print stage info *)
  let stage_info_str = 
    List.map (fun (cn, nstgs) -> Printf.sprintf "%s : %s" cn nstgs) (!layout_info)
    |> String.concat "\n" 
  in
  BackendLogging.fprintf ((!IoUtils.outDir)^"/layout_info.txt") stage_info_str;
  res
;;

(* main compilation function *)
let compile ds portspec =  
  let old_layout = Cmdline.cfg.old_layout in 
  let ctl_fn_opt = Cmdline.cfg.ctl_fn in 
  let partial_interp = Cmdline.cfg.partial_interp in
  if (Cmdline.cfg.debug)
  then (
    CoreCdg.start_logging ();
    CoreDfg.start_logging ();
    PropagateEvars.start_logging());
  report_if_verbose "-------Translating to Midend IR---------";
  let core_ds = SyntaxToCore.translate_prog ds in
  printprog_if_debug core_ds;
  let ds = core_ds in 

  dump_ir_prog "midend before partial interp (initial prog)" "midend_pre_partial_interp.dpt" ds;
  let ds = if partial_interp
    then (
      report_if_verbose "-------Partial interpreting---------";
      let res = PartialInterpretation.interp_prog ds in 
      printprog_if_debug res;
      res
      )
    else ds 
  in
  dump_ir_prog "midend after partial interp" "midend_post_partial_interp.dpt" ds;
  let ds = EliminateEventCombinators.process ds in
  report_if_verbose "-------Unifying event and handler parameter ids---------";
  let ds = StandardizeEventParams.process ds in
  report_if_verbose "-------Inlining event variables---------";
  let ds = InlineEventVars.inline ds in
  report_if_verbose "-------Inlining actions into tables---------";
  let ds = UniqueTableActions.process ds in
  report_if_verbose "-------Adding declarations for P4Tofino intrinsics---------";
  let ds = AddIntrinsics.add_intrinsics ds in
  report_if_verbose "-------Numbering events---------";
  let ds = AddIngressParser.set_event_nums ds in 
  (* generate the ingress parser or add background event parsing *)
  report_if_verbose "-------Adding background event parser---------";
  let ds = AddIngressParser.add_parser portspec ds in
  (* static analysis to see if this program is compile-able *)
  report_if_verbose "-------Checking tofino compatibilty---------";
  InputChecks.all_checks ds;

  (* after this point, there should not be any changes
       to the globals, actions, event, or handlers defined
       in the program.
     The program should be in a form where:
      - each action is used by at most 1 table *)

  let ds = atomic_op_form ds in
  (* Statements and variable names don't change much beyond this point,
     so we give everything a unique identifier. *)
  report_if_verbose "-------Assigning spans with unique IDs---------";
  let ds = UniqueSpans.make_unique_spans ds in
  let ds = UniqueIds.make_var_names_unique ds in
  printprog_if_debug ds;

  (* translate into TofinoCore IR *)
  let core_prog = to_tofinocore ds in
  dump_prog "tofinocore initial program" "tofinocore_initial" core_prog;

  (* add the egress parser. This could go anywhere in tofinocore
     passes. *)
  report_if_verbose "-------Adding egress parser-------";
  let core_prog = AddEgressParser.add_parser core_prog in
  report_if_verbose "-------Eliminating generates-------";
  let core_prog = GeneratesNew.eliminate_generates portspec core_prog in
  report_if_verbose "-------Optimizing parsers (speculative peeking)-------";
  let core_prog = ParseOptimizer.parser_passes core_prog in 

  report_if_verbose "-------Tagging large match statements as solitary-------";
  let core_prog = SolitaryMatches.process_core 20 core_prog in
  printtofcoreprog_if_debug core_prog;
  report_if_verbose "-------Eliminating local initializations wherever possible -------";
  let core_prog = RemoveLocalInits.process core_prog in 
  dump_prog "tofinocore local inits removed" "tofinocore_nolocalinit" core_prog;

(*   report_if_verbose "-------Transforming certain declarations to no-init declarations-------";
  let core_prog = RemoveInitOnlyStmts.process core_prog in 
 *)
  printtofcoreprog_if_debug core_prog;
  report_if_verbose "-------Eliminating if statements-------";
  let core_prog = IfToMatch.process_core core_prog in 
  report_if_verbose "-------Converting all memops to complex form-------";
  let core_prog = RegularizeMemopsNew.process_core core_prog in
  report_if_verbose "-------Allocating memop input variables-------";
  let core_prog = ShareMemopInputsNew.process_core core_prog in

  dump_prog "IfToMatch; RegularizeMemopsNew; ShareMemopInputsNew" "tofinocore_regularized_memops" core_prog;
  report_if_verbose "-------Transforming table matches into single-call form-------";
  let core_prog = SingleTableMatch.process_core core_prog in
  report_if_verbose "-------Transforming actions into functions-------";
  let core_prog = ActionsToFunctionsNew.process_core core_prog in
  dump_prog "SingleTableMatch; ActionsToFunctions" "tofinocore_single_table_match" core_prog;

  let core_prog = PropagateEvars.process core_prog in
  (* exit 1; *)
  dump_prog "PropagateEvars; (tofinocore right before layout)" "tofinocore_pre_layout" core_prog;
  let core_prog = layout old_layout core_prog in
  dump_prog "tofinocore after layout (final)" "tofinocore_post_layout" core_prog;
  report_if_verbose "-------Translating to final P4-tofino-lite IR-------";
  let tofino_prog = TofinoCoreToP4.translate_prog core_prog in
  (* generate the python event library *)
  let py_eventlib =
    if 
      report_if_verbose "-------generating python event parsing library-------";
      (* TODO: generate this from the parser?   *)
      Cmdline.cfg.serverlib then PyEventLib.coresyntax_to_pyeventlib ds else ""
  in
  (* build the globals name directory json *)
  report_if_verbose "-------generating Lucid name => P4 name directory-------";
  let globals_directory = 
    P4TofinoGlobalDirectory.build_global_dir tofino_prog
    |> Yojson.Basic.pretty_to_string
  in
  (* print everything to strings *)
  report_if_verbose "-------printing P4 program to string-------";
  let p4 = P4TofinoPrinting.p4_of_prog tofino_prog in
  report_if_verbose "-------printing Python control plane to string-------";
  let py_ctl = ControlPrinter.pyctl_of_prog tofino_prog ctl_fn_opt in
  (* let cpp_ctl = ControlPrinter.cppctl_of_prog tofino_prog in *)
  let cpp_ctl = "" in 
  p4, cpp_ctl, py_ctl, py_eventlib, globals_directory
;;
