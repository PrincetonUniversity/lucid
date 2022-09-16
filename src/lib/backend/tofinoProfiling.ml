(* use the backend to estimate resource utilization on the tofino *)
open TofinoCore

let info str = 
  Console.show_message str ANSITerminal.Green "Profiler"
;;

exception Error of string
let error s = raise (Error s)

let core_passes ds = ds 
  |> EliminateEventCombinators.process
  |> UnifyHandlerParams.rename_event_params 
  |> UnifyHandlerParams.unify_event_and_handler_params 
  |> EliminateExitEvents.process 
  |> InlineEventVars.inline 
;;

let tofinocore_passes ds = ds
  |> tdecls_of_decls
  |> (fun ds -> SolitaryMatches.process ds 20)
  |> IfToMatch.process
  |> RegularizeMemops.process
  |> ShareMemopInputs.process
  |> Generates.eliminate
;;

let profile ds portspec output_dir profile_cmd = 
  let _, _, _ = ds, portspec,output_dir in 
  info@@"profiling program. profiling command: "^(profile_cmd);

  (* first, run the passes that get the program into a canonical form *)
  let ds = ds |> core_passes |> tofinocore_passes in
  (* now profile the resulting program *)
  match profile_cmd with 
    | "" -> DependencyProfiling.profile ds output_dir;
    | "all" -> DependencyProfiling.profile ds output_dir;
    | "dependencies" -> DependencyProfiling.profile ds output_dir;
    | _ -> error@@"unknown profiling command: "^(profile_cmd);
