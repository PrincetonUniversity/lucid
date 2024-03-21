(* Tofino backend pipeline. *)
(* This is a new pipeline, based on the new TofinoCore (6/2023), 
   which adds event types (union and set), parsing, and a simple 
   architectural model to encode the way a program is split across 
   multiple components in the switch. *)

open TofinoCore
open BackendLogging

let start_backend_logging () = 
  if (Cmdline.cfg.debug)
    then (
      TofinoCdg.start_logging ();
      TofinoDfg.start_logging ();
      PropagateEvars.start_logging();
      DeparserChecksums.start_logging();
      ShareMemopInputsSat.start_logging();
      TofinoResources.start_logging ();
      );
;;  
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


(* transform into a form where each statement is a simple operation that the 
   layout algorithm knows how to place )and do some other processing) *)
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
  dump_ir_prog "midend after precompute args" "midend_precompute" ds;
  let ds = EliminateBools.do_passes ds in
  dump_ir_prog "midend after EliminateBools" "midend_elim_bools" ds;
  let ds = NormalizeInts.do_passes ds in
  dump_ir_prog "midend after NormalizeInts" "midend_norm_ints" ds;
  MidendPipeline.print_if_debug ds;
  ds
;;

let core_passes ds portspec = 
  (* all the passes over CoreSyntax *)
  print_endline ("--------eliminating event match--------");
  let ds = EliminateEventMatch.process_prog ds in
  let ds = EliminateTBuiltin.process_prog ds in
  dump_ir_prog "midend before partial interp (initial prog)" "midend_pre_partial_interp.dpt" ds;
  let ds = if Cmdline.cfg.partial_interp
    then (
      report_if_verbose "-------Partial interpreting---------";
      let res = PartialInterpretation.interp_prog ds in 
      printprog_if_debug res;
      res
      )
    else ds 
  in
  dump_ir_prog "midend after partial interp" "midend_post_partial_interp.dpt" ds;
  (* these passes are misc transformations that can happen in any order. 
     In a refactored backend, EliminateEventCombinators, StandardizeEventParams, and InlineEventVars
     should be removable. *)
  let ds = EliminatePayloads.process ds in 
  let ds = EliminateEventCombinators.process ds in
  report_if_verbose "-------Unifying event and handler parameter ids---------";
  let ds = StandardizeEventParams.process ds in
  report_if_verbose "-------Numbering events---------";
  let ds = InlineEventVars.set_event_nums ds in 
  report_if_verbose "-------Inlining event variables---------";
  let ds = InlineEventVars.inline ds in
  report_if_verbose "-------Creating unique per-table actions---------";
  let ds = UniqueTableActions.process ds in

  report_if_verbose "-------Adding declarations for P4Tofino intrinsics---------";
  let ds = AddIntrinsics.add_intrinsics ds in
  (* generate the ingress parser or add background event parsing *)
  report_if_verbose "-------Adding background event parser---------";
  let port_ty = (Builtins.tofino_builtin_tys.ingr_port_ty |> SyntaxToCore.translate_ty) in 

  let ds = AddIngressParser.add_parser port_ty portspec ds in
  (* static analysis to see if this program is compile-able *)
  report_if_verbose "-------Checking tofino compatibilty---------";
  InputChecks.all_checks ds;

  (* after this point, there should not be any changes
       to the globals, actions, event, or handlers defined
       in the program.
     The program should be in a form where:
      - each action is used by at most 1 table *)
  (* atomic op form breaks down statements so that each statement can 
     map to a single ALU operation or a single match table invocation. *)
  dump_ir_prog "midend in atomic op form" "midend_pre_atomic_op.dpt" ds;
  let ds = atomic_op_form ds in
  dump_ir_prog "midend in atomic op form" "midend_atomic_op.dpt" ds;
  (* the final coreSyntax transformation is hoisting local declarations 
     out of conditional branches, moving them as early in the code as 
     possible. This breaks false dependencies that arise in nested branches *)
  let ds = Hoisting.process ds in 
  dump_ir_prog "midend after hoisting" "midend_hoisting.dpt" ds;
  (* Statements and variable names don't change much beyond this point,
     so we give everything a unique identifier. *)
  report_if_verbose "-------Assigning spans with unique IDs---------";
  let ds = UniqueSpans.make_unique_spans ds in
  let ds = UniqueIds.make_var_names_unique ds in
  printprog_if_debug ds;
  ds
;;

(* transform into canonical tofinocore form, which takes several smaller passes. *)
let to_tofinocore ds =
  report_if_verbose "-------splitting program into ingress / egress components--------";
  let split_prog = TofinoSplit.split ds in 
  report_if_verbose "-------Translating to Tofino dialect of Midend IR---------";
  let core_prog = TofinoCore.core_to_tofinocore split_prog  in
  report_if_verbose "-------Translating handlers into event-typed functions--------";
  let core_prog = AddHandlerTypes.type_handlers core_prog in
  report_if_verbose "-------Merging handlers and constructing union events--------";
  let core_prog = MergeHandlers.merge_handlers core_prog in
  core_prog
;;


let tofinocore_passes core_prog portspec = 
  (* all the passes over the tofinocore IR, which is an extension of the 
     core IR with nodes for merged events and handlers (and a few other things) *)

  report_if_verbose "-------Adding egress parser-------";
  let core_prog = AddEgressParser.add_parser core_prog in
  report_if_verbose "-------Eliminating generates-------";
  dump_prog "tofinocore pre generate elimination" "tofinocore_pre_genelim" core_prog;
  let core_prog = EliminateGenerates.eliminate_generates portspec core_prog in
  dump_prog "tofinocore post generate elimination" "tofinocore_post_genelim" core_prog;

  report_if_verbose "-------Eliminating event generations in parser (and hoisting)-------";
  let core_prog = ParserHoisting.parser_passes core_prog in 

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
  (* ideally, iftomatch would be a transformation on the plain CoreSyntax, rather 
     than the tofino specific one. The only reason it is not is non-refactored code. *)
  let core_prog = IfToMatch.process_core core_prog in 
  report_if_verbose "-------Converting all memops to complex form-------";
  let core_prog = RegularizeMemops.process_core core_prog in
  report_if_verbose "-------Allocating memop input variables-------";
  dump_prog "before ShareMemopInputsSat" "tofinocore_pre_memop_overlay" core_prog;
  let core_prog = ShareMemopInputsSat.process_core core_prog in
  dump_prog "IfToMatch; RegularizeMemops; ShareMemopInputsSat" "tofinocore_regularized_memops" core_prog;
  report_if_verbose "-------Transforming table matches into single-call form-------";
  let core_prog = SingleTableMatch.process_core core_prog in
  dump_prog "SingleTableMatch; ActionsToFunctions" "tofinocore_single_table_match" core_prog;
  report_if_verbose "-------Transforming actions into functions-------";
  let core_prog = ActionsToFunctions.process_core core_prog in
  dump_prog "SingleTableMatch; ActionsToFunctions" "tofinocore_action_functions" core_prog;
  (* propagateEvars is another hoisting pass that benefits control flows with 
     match tables that are declared and used once. *)
  let core_prog = PropagateEvars.process core_prog in
  dump_prog "PropagateEvars; (tofinocore right before layout)" "tofinocore_evars" core_prog;
  (* this moves checksums over event outputs to the deparser. It has to come 
     after propagateEvars because it requires that the checksum statement be inlined. *)
  report_if_verbose "-------Moving output checksum operations to deparser-------";
  let core_prog = DeparserChecksums.process core_prog in
  dump_prog "tofinocore post deparser generation" "tofinocore_post_deparser" core_prog;
  core_prog
;;


(* layout the ingress and egress components of a tofinocore prog *)
let layout (prog : TofinoCore.prog) = 
  let layout_info = ref [] in
  (* we want to do the layout pipeline for each component *)
  let layout_component (comp:TofinoCore.component) = 
    (* skip component with sort HControl -- it does not need layout *)
    if (comp.comp_sort = TofinoCore.HControl) then comp
    else
    let cn = comp.comp_id |> Id.name in
    let logging_prefix = !IoUtils.graphLogDir ^ "/" ^ cn in 
    report_if_verbose (Printf.sprintf "-------Layout for %s: computing control flow graph-------" cn);
    let cfg = TofinoCfg.cfg_of_component_main comp.comp_decls in
    TofinoCfg.print_cfg ( logging_prefix ^ "_cfg.dot") cfg;
    report_if_verbose (Printf.sprintf "-------Layout for %s: computing control dependency graph-------" cn);
    let cdg = TofinoCdg.to_control_dependency_graph cfg in
    TofinoCfg.print_cfg ( logging_prefix ^ "_cdg.dot") cfg;
    report_if_verbose (Printf.sprintf "-------Layout for %s: computing data dependency graph-------" cn);
    let dfg = TofinoDfg.process cdg in
    TofinoDfg.print_dfg (logging_prefix ^ "_dfg.dot") dfg;
    report_if_verbose (Printf.sprintf "-------Layout for %s: scheduling data dependency graph to pipeline-------" cn);
    let pipeline_stmts = TofinoLayout.process_new comp.comp_decls dfg in
    let num_stages = List.length(pipeline_stmts) in
    layout_info := (!layout_info)@[(cn, (string_of_int num_stages))];
    let main_handler = TofinoCore.main_handler_of_decls comp.comp_decls in
    let main_handler' = {main_handler with hdl_body = TofinoCore.SPipeline(pipeline_stmts);} in
    let comp = TofinoCore.replace_main_handler_of_component comp main_handler' in 
    dump_prog "tofinocore after layout (final)" (Printf.sprintf "tofinocore_%s_layout_pre_actionform" cn) [comp];
    report_if_verbose (Printf.sprintf "-------Layout for %s: wrapping table branches in functions-------" cn);
    let comp = ActionForm.process_comp comp in
    report_if_verbose (Printf.sprintf "-------Layout for %s: deduplicating table branch functions-------" cn);
    let comp = TofinoDedup.process_comp comp in
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
  start_backend_logging ();
  report_if_verbose "-------Translating to Midend IR---------";
  let ds = SyntaxToCore.translate_prog ds in
  printprog_if_debug ds;

  let ds = core_passes ds portspec in

  (* translate into TofinoCore IR *)
  let core_prog = to_tofinocore ds in
  dump_prog "tofinocore initial program" "tofinocore_initial" core_prog;
  (* do all the tofinocore passes *)
  let core_prog = tofinocore_passes core_prog portspec in

  (* layout the program. Uses its own internal IRs, but dumps out 
     something in tofinocore *)
  let core_prog = layout core_prog in
  dump_prog "tofinocore after layout (final)" "tofinocore_final" core_prog;

  (* translate into final P4-tofino-lite IR *)
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
  let py_ctl = ControlPrinter.pyctl_of_prog tofino_prog Cmdline.cfg.ctl_fn in
  (* let cpp_ctl = ControlPrinter.cppctl_of_prog tofino_prog in *)
  let cpp_ctl = "" in 
  p4, cpp_ctl, py_ctl, py_eventlib, globals_directory
;;
