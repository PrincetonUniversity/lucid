(* use the backend to estimate resource utilization on the tofino *)
(* NOTE: this is only for the ingress pipeline!  *)
open TofinoCore
open CoreSyntax

let info str = 
  Console.show_message str ANSITerminal.Green "Profiler"
;;

exception Error of string
let error s = raise (Error s)


let partial_backend_pipeline ds portspec = 
  let partial_interp = Config.base_cfg.partial_interp in

  let core_ds = SyntaxToCore.translate_prog ds in
  let ds = core_ds in 
  let ds = if partial_interp
    then (
      let res = PartialInterpretation.interp_prog ds in 
      res
      )
    else ds 
  in
  let ds = EliminateEventCombinators.process ds in
  let ds = StandardizeEventParams.process ds in
  let ds = InlineEventVars.set_event_nums ds in 
  let ds = InlineEventVars.inline ds in
  let ds = UniqueTableActions.process ds in
  let ds = AddIntrinsics.add_intrinsics ds in
  let port_ty = (Builtins.tofino_builtin_tys.ingr_port_ty |> SyntaxToCore.translate_ty) in 
  let ds = AddIngressParser.add_parser port_ty portspec ds in
  InputChecks.all_checks ds;
  let ds = TofinoPipeline.atomic_op_form ds in
  let ds = Hoisting.process ds in 
  let ds = UniqueSpans.make_unique_spans ds in
  let ds = UniqueIds.make_var_names_unique ds in
  let core_prog = TofinoPipeline.to_tofinocore ds in
  let core_prog = AddEgressParser.add_parser core_prog in
  let core_prog = EliminateGenerates.eliminate_generates portspec core_prog in
  let core_prog = ParserHoisting.parser_passes core_prog in 
  let core_prog = SolitaryMatches.process_core 20 core_prog in
  let core_prog = RemoveLocalInits.process core_prog in 
  let core_prog = IfToMatch.process_core core_prog in 
  let core_prog = RegularizeMemops.process_core core_prog in
  let core_prog = ShareMemopInputsSat.process_core core_prog in
  let core_prog = SingleTableMatch.process_core core_prog in
  let core_prog = ActionsToFunctions.process_core core_prog in
  let core_prog = PropagateEvars.process core_prog in
  let core_prog = DeparserChecksums.process core_prog in
  (* now get the dfg. This is the first part of the "layout" function in tofinoPipeline. *)
  let comp = List.find (fun c -> c.comp_sort = CoreSyntax.HData) core_prog in
  let cfg = TofinoCfg.cfg_of_component_main comp.comp_decls in
  let cdg = TofinoCdg.to_control_dependency_graph cfg in
  let dfg = TofinoDfg.process cdg in
  comp, cdg, dfg
;;

let export_dfg ds portspec dfg_json_fn =
  let comp, _, dfg = partial_backend_pipeline ds portspec in
  ExternalLayout.export comp.comp_decls dfg dfg_json_fn;
;;
  
let profile ds portspec output_dir profile_cmd = 
  let _, _, _ = ds, portspec,output_dir in 
  info@@"profiling program. profiling command: "^(profile_cmd);
  (* first, run all the transformation and optimization passes *)
  let comp, cdg, dfg = partial_backend_pipeline ds portspec in
  (* now profile the resulting program *)
  match profile_cmd with 
    | "layout" -> ExternalLayout.export comp.comp_decls dfg output_dir;
    | "all" -> DependencyProfiling.profile (cdg, dfg) output_dir;
    | "dependencies" -> DependencyProfiling.profile (cdg, dfg) output_dir;
    | _ -> error@@"unknown profiling command: "^(profile_cmd);
