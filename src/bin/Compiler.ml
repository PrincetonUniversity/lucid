(* Main file of compiler. *)
(* open Batteries *)
open Dpt
open Cmdline
open BackendLogging
open Printf
open IoUtils
open Yojson.Basic

(* minimal input
   1. dpt program
   2. build directory *)

(* output (in build dir):
   1. P4 program -- lucid.p4
   2. switchd program -- lucid.cpp
   3. python control -- lucid.py
   4. makefile
   5. helpers *)

let do_logging = ref true ;;
let silent_mode () = 
  do_logging := false;
  InterpHelpers.silent := true;
  NormalizeInts.silent := true;
  PackageTofinoApp.silent := true;
  TofinoPipeline.verbose := false;
  TofinoPipeline.do_log := false;
  ()
;;
let debug_mode () = 
  do_logging := true;
  InterpHelpers.silent := false;
  NormalizeInts.silent := false;
  PackageTofinoApp.silent := false;
  TofinoPipeline.verbose := true;
  TofinoPipeline.do_log := true;
  ()
;;


let unmutable_report str = 
  Console.show_message str ANSITerminal.Green "compiler"
;;

let report str = 
  if (!do_logging)
  then (
  Console.show_message str ANSITerminal.Green "compiler";
  )
;;

type args_t =
  { dptfn : string
  ; builddir : string
  ; portspec : string option
  ; interp_spec_file : string
  ; aargs : string list
  ; profile_cmd : string option
  ; ctl_fn : string option
  ; old_layout : bool
  }

let mk_args cfg =
  {
    dptfn = (cfg.dpt_file);
    builddir = (cfg.builddir);
    portspec = (cfg.portspec);
    interp_spec_file = (cfg.spec_file);
    profile_cmd = (cfg.profile_cmd);
    ctl_fn = (cfg.ctl_fn);
    old_layout = (cfg.old_layout);
    aargs = [];
  }
;;

let profile_for_tofino target_filename portspec build_dir profile_cmd = 
  let ds = Input.parse target_filename in
  let ds = FunctionInliningSpecialCase.inline_prog_specialcase ds in
  let _, ds = FrontendPipeline.process_prog Builtins.tofino_builtin_tys ds in
  let core_ds = MidendPipeline.process_prog ds in
  let portspec = ParsePortSpec.parse portspec in 
  TofinoProfiling.profile core_ds portspec build_dir profile_cmd
;;

let compile_to_tofino (args:args_t) =
  let portspec = ParsePortSpec.parse args.portspec in 
  unmutable_report@@"Starting P4-Tofino compilation. Using switch port configuration: ";
  print_endline (ParsePortSpec.string_of_portconfig portspec);
  let ds = Input.parse args.dptfn in
  (* before the standard frontend, do temporary optimization 
     passes that will eventually be removed once the 
     mid/back-end is better optimized. *)
  let ds = FunctionInliningSpecialCase.inline_prog_specialcase ds in
  (* frontend type checks and eliminates most abstractions (modules, functions) *)
  let _, ds = FrontendPipeline.process_prog Builtins.tofino_builtin_tys ds in
  (* translate to IR *)
  let core_ds = SyntaxToCore.translate_prog ds in
  (* tofino backend *)
  let p4_str, c_str, py_str, py_eventlib, globals = TofinoPipeline.compile args.old_layout core_ds portspec args.builddir args.ctl_fn in 
  (* package the program with some helper makefiles *)
  unmutable_report@@"Compilation to P4 finished. Writing to build directory:"^(args.builddir);
  PackageTofinoApp.generate p4_str c_str py_str py_eventlib globals args.builddir 

let main () = 
  unmutable_report "Compilation to P4 started...";
  let _ = Cmdline.parse_tofino () in
  (if (not cfg.verbose)
  then (silent_mode ()));
  (if (cfg.debug)
    then (debug_mode ()));
  let args = mk_args cfg in

  match args.profile_cmd with 
  | None -> (
    (* setup build directory directory. *)
    IoUtils.setup_build_dir args.builddir;
    (* todo: also copy the included files *)
    let _ = cpy_src_to_build args.dptfn args.builddir in
    (* compile lucid code to P4 / python / C *)
    compile_to_tofino args
  )
  | Some(profile_cmd) -> (
    IoUtils.setup_profile_dir args.builddir;
    profile_for_tofino args.dptfn args.portspec args.builddir profile_cmd
  )
;;


(* for profiling. limit is in bytes. *)
(* let run_with_memory_limit limit f =
  let limit_memory () =
    let mem = Gc.(quick_stat ()).heap_words in
    if mem > limit / (Sys.word_size / 8) then raise Out_of_memory
  in
  let alarm = Gc.create_alarm limit_memory in
  Fun.protect f ~finally:(fun () -> Gc.delete_alarm alarm ; Gc.compact ())


let b_1gb   = 1000000000;;
let b_100mb = 100000000;;

let _ = run_with_memory_limit b_100mb main
 *)

let _ = main ()




