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

let report str =
  Console.show_message str ANSITerminal.Green "compiler"
;;

let profile_for_tofino target_filename portspec build_dir profile_cmd =
  let ds = Input.parse target_filename in
  let _, ds = FrontendPipeline.process_prog Builtins.tofino_builtin_tys ds in
  let portspec = ParsePortSpec.parse portspec in
  TofinoProfiling.profile ds portspec build_dir profile_cmd
;;

let compile_to_tofino dptfn =
  let portspec = ParsePortSpec.parse Cmdline.cfg.portspec in
  report
  @@ "Starting P4-Tofino compilation. Using switch port configuration: ";
  print_endline (ParsePortSpec.string_of_portconfig portspec);
  let ds = Input.parse dptfn in
  let _, ds = FrontendPipeline.process_prog Builtins.tofino_builtin_tys ds in
  (* tofino midend / backend *)
  let p4_str, c_str, py_str, py_eventlib, globals =
    TofinoPipeline.compile
      ds
      portspec
  in
  (* package the program with some helper makefiles *)
  report
  @@ "Compilation to P4 finished. Writing to build directory:"
  ^ Cmdline.cfg.builddir;
  PackageTofinoApp.generate
    p4_str
    c_str
    py_str
    py_eventlib
    globals
    Cmdline.cfg.builddir
;;

let main () =
  report "Compilation to P4 started...";
  let dpt_fn = Cmdline.parse_tofino () in
  match (Cmdline.cfg.profile_cmd) with
  | None ->
    (* setup build directory directory. *)
    IoUtils.setup_build_dir Cmdline.cfg.builddir;
    (* compile lucid code to P4 / python / C *)
    (* todo: also copy the included files *)
    let _ = cpy_src_to_build dpt_fn Cmdline.cfg.builddir in
    compile_to_tofino dpt_fn
  | Some profile_cmd ->
    IoUtils.setup_profile_dir Cmdline.cfg.builddir;
    profile_for_tofino dpt_fn Cmdline.cfg.portspec Cmdline.cfg.builddir profile_cmd
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
