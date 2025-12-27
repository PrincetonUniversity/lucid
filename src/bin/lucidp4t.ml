(* Lucid compiler to P4 Tofino *)
(* open Batteries *)
open Dpt
open Config
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

(* load port specs from port config file, or use command-line args *)
let get_portspec () = 
  match Config.cfg.portspec with
    | Some(portspec_fn) -> 
        TofinoPorts.parse portspec_fn
    | None -> 
        TofinoPorts.create Config.cfg.ports Config.cfg.recirc_port
;;

let profile_for_tofino target_filename portspec build_dir profile_cmd =
  let ds = Input.parse target_filename in
  let _, ds = FrontendPipeline.process_prog Builtins.tofino_builtin_tys ds in
  TofinoProfiling.profile ds portspec build_dir profile_cmd
;;

let compile_to_tofino dptfn =
  let portspec = get_portspec () in 
  report
  @@ "Starting P4-Tofino compilation. Using switch port configuration: ";
  print_endline (TofinoPorts.string_of_portconfig portspec);
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
  ^ Config.cfg.builddir;
  PackageTofinoApp.generate
    p4_str
    c_str
    py_str
    py_eventlib
    globals
    Config.cfg.builddir
;;

let main () =
  report "Compilation to P4 started...";
  let dpt_fn = Config.parse_tofino () in
  match (Config.cfg.profile_cmd) with
  | None ->
    (* setup build directory directory. *)
    IoUtils.setup_build_dir Config.cfg.builddir;
    (* compile lucid code to P4 / python / C *)
    let _ = cpy_src_to_build dpt_fn Config.cfg.builddir in
    compile_to_tofino dpt_fn
  | Some profile_cmd ->
    IoUtils.setup_profile_dir Config.cfg.builddir;
    profile_for_tofino dpt_fn (get_portspec ()) Config.cfg.builddir profile_cmd
;;


let _ = main ()
