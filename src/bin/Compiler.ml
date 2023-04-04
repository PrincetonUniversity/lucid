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

(* hack in a disable logging option for optimizer iterations.
   turns out this doesn't matter for performance.*)
let do_logging = ref true ;;
let disable_logging () = 
  do_logging := false;
  Cmdline.cfg.verbose <- false;
  InterpHelpers.silent := true;
  NormalizeInts.silent := true;
  PackageTofinoApp.silent := true;
  ()
;;
let enable_debug () = 
  do_logging := true;
  TofinoPipeline.verbose := true;
  TofinoPipeline.do_log := true;
  (* Cmdline.cfg.debug <- true; *)
  Cmdline.cfg.verbose <- true;
  Cmdline.cfg.verbose_types <- true;
(*   InterpHelpers.silent := true;
  NormalizeInts.silent := true;
  PackageTofinoApp.silent := true; *)
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

(* args parsing *)
module ArgParse = struct
  let usage = "Usage: ./dptc myProg.dpt myBuildDir"

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

  let args_default =
    { dptfn = ""
    ; builddir = "lucid_tofino_build"
    ; portspec = None
    ; interp_spec_file = ""
    ; aargs = []
    ; profile_cmd = None
    ; ctl_fn = None
    ; old_layout = false
    }
  ;;

  let parse_args () =
    let args_ref = ref args_default in
    (* set named args *)
    let set_spec s = args_ref := { !args_ref with interp_spec_file = s } in
    (* FIXME: Crazy hack, we should unify the two methods of commandline parsing *)
    let set_symb s = Cmdline.cfg.symb_file <- s in
    let set_ports s = args_ref := { !args_ref with portspec = Some(s) } in
    let set_profile_cmd s = args_ref := {!args_ref with profile_cmd = Some(s)} in
    let set_build_dir s = args_ref :=  {!args_ref with builddir=s;} in 
    let set_control ctl_fn = args_ref := {!args_ref with ctl_fn=Some(ctl_fn);} in
    let set_old_layout () = args_ref := {!args_ref with old_layout = true;} in
    let speclist =
      [ ( "--spec"
        , Arg.String set_spec
        , "Path to the interpreter specification file" )
      ; "-o", Arg.String set_build_dir, "Output build directory"
      ; "--ports", Arg.String set_ports, "Path to the ports specification file"
      ; "--symb", Arg.String set_symb, "Path to the symbolic specification file"
      ; "--nocallopt", Arg.Unit MidendPipeline.set_no_call_optimize, "Disable call optimization" 
      ; "--silent", Arg.Unit disable_logging, "Disable all logging"
      ; "-p", Arg.String set_profile_cmd, "Profile program instead of compiling."
      ; "-d", Arg.Unit enable_debug, "Enable debug print / log"
      ; "--control", Arg.String set_control, "Python control program"
      ; "--oldlayout", Arg.Unit set_old_layout, "Use old layout algorithm"
      ]
    in
    let parse_aarg (arg : string) =
      args_ref := { !args_ref with aargs = !args_ref.aargs @ [arg] };
      (* Fixme: second part of crazy hack *)
      Cmdline.cfg.dpt_file <- arg
    in
    Arg.parse speclist parse_aarg usage;
    (* interpret positional args as named *)
    let args =
      match !args_ref.aargs with
      | [dptfn;] ->
        { !args_ref with dptfn; aargs = [] }
      | [dptfn; builddir] ->
        { !args_ref with dptfn; builddir; aargs = [] }
      | _ -> error usage
    in
    args
  ;;
end

(* right now, the interpreter is setting the extern
   variables in its global state. They aren't getting
   replaced with constants or anything. *)
let parse_externs_from_interp_spec spec_file =
  let json = from_file spec_file in
  match json with
  | `Assoc lst ->
    (* return a map from extern name strings to extern values *)
    (match List.assoc_opt "externs" lst with
    | Some (`Assoc lst) -> lst
    | None -> []
    | Some _ -> error "Non-assoc type for extern definitions")
  | _ -> error "Unexpected interpreter specification format"
;;

let profile_for_tofino target_filename portspec build_dir profile_cmd = 
  let ds = Input.parse target_filename in
  let ds = FunctionInliningSpecialCase.inline_prog_specialcase ds in
  let _, ds = FrontendPipeline.process_prog Builtins.tofino_builtin_tys ds in
  let core_ds = MidendPipeline.process_prog ds in
  let portspec = ParsePortSpec.parse portspec in 
  TofinoProfiling.profile core_ds portspec build_dir profile_cmd
;;
let compile_to_tofino (args:ArgParse.args_t) =
  let ds = Input.parse args.dptfn in
  (* before the standard frontend, do temporary optimization 
     passes that will eventually be removed once the 
     mid/back-end is better optimized. *)
  let ds = FunctionInliningSpecialCase.inline_prog_specialcase ds in
  (* frontend type checks and eliminates most abstractions (modules, functions) *)
  let _, ds = FrontendPipeline.process_prog Builtins.tofino_builtin_tys ds in
  (* middle passes do regularization over simplified syntax *)
  let core_ds = MidendPipeline.process_prog ds in
  (* backend does tofino-specific transformations, layout, 
  then translates into p4tofino syntax and produces program strings *)
  let portspec = ParsePortSpec.parse args.portspec in 
  unmutable_report@@"Starting P4-Tofino compilation. Using switch port configuration: ";
  print_endline (ParsePortSpec.string_of_portconfig portspec);
  let p4_str, c_str, py_str, py_eventlib, globals = TofinoPipeline.compile args.old_layout core_ds portspec args.builddir args.ctl_fn in 
  (* finally, generate the build directory with the programs + some helpers and a makefile *)
  unmutable_report@@"Compilation to P4 finished. Writing to build directory:"^(args.builddir);
  PackageTofinoApp.generate p4_str c_str py_str py_eventlib globals args.builddir 

let main () = 
  unmutable_report "Compilation to P4 started...";
  let args = ArgParse.parse_args () in
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




