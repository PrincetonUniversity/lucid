(* use the backend to estimate resource utilization on the tofino *)
open TofinoCore

let info str = 
  Console.show_message str ANSITerminal.Green "Profiler"
;;

exception Error of string
let error s = raise (Error s)

let export_dfg ds dfg_json_fn = 
  (* first, run all the transformation and optimization passes *)
  let ds = ds 
    |> TofinoPipeline.core_passes 
    |> tdecls_of_decls
    |> (TofinoPipeline.tofinocore_normalization true) 
  in
  ExternalLayout.export ds dfg_json_fn;
;;
let profile ds portspec output_dir profile_cmd = 
  let _, _, _ = ds, portspec,output_dir in 
  info@@"profiling program. profiling command: "^(profile_cmd);

  (* first, run all the transformation and optimization passes *)
  let ds = ds 
    |> TofinoPipeline.core_passes 
    |> tdecls_of_decls
    |> (TofinoPipeline.tofinocore_normalization true) 
  in
  (* now profile the resulting program *)
  match profile_cmd with 
    | "layout" -> ExternalLayout.export ds output_dir;
    | "all" -> DependencyProfiling.profile ds output_dir;
    | "dependencies" -> DependencyProfiling.profile ds output_dir;
    | _ -> error@@"unknown profiling command: "^(profile_cmd);
