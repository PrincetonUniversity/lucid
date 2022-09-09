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
    }

  let args_default =
    { dptfn = ""
    ; builddir = ""
    ; portspec = None
    ; interp_spec_file = ""
    ; aargs = []
    }
  ;;

  let parse_args () =
    let args_ref = ref args_default in
    (* set named args *)
    let set_spec s = args_ref := { !args_ref with interp_spec_file = s } in
    (* FIXME: Crazy hack, we should unify the two methods of commandline parsing *)
    let set_symb s = Cmdline.cfg.symb_file <- s in
    let set_ports s = args_ref := { !args_ref with portspec = Some(s) } in
    let speclist =
      [ ( "--spec"
        , Arg.String set_spec
        , "Path to the interpreter specification file" )
      ; "--ports", Arg.String set_ports, "Path to the ports specification file"
      ; "--symb", Arg.String set_symb, "Path to the symbolic specification file"
      ; "--nocallopt", Arg.Unit MidendPipeline.set_no_call_optimize, "Disable call optimization" 
      ; "--silent", Arg.Unit disable_logging, "Disable all logging"
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
      | [dptfn; builddir] ->
        { !args_ref with dptfn; builddir; aargs = [] }
      | _ -> error usage
    in
    args
  ;;
end

exception Error of string

let error s = raise (Error s)

(* start per-pass logs. *)
let start_backend_logs () = 
  if (!do_logging) 
  then ()
  else ()
;;
(* shouldn't need to do this anymore. *)
let clear_output_dir () =
  (* clear output directory of old source. *)
  let cmd = "rm " ^ !outDir ^ "/*.dpt" in
  (* print_endline ("[clear_output_dir] command: " ^ cmd); *)
  let _ = Sys.command cmd in
  ()
;;



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


let compile_to_tofino target_filename portspec build_dir =
  start_backend_logs ();
  (* parse *)
  let ds = Input.parse target_filename in
  (* before the "official" frontend, do temporary optimization 
     passes that will eventually be removed once the 
     mid/back-end is better optimized. *)
  let ds = FunctionInliningSpecialCase.inline_prog_specialcase ds in
  (* frontend type checks and eliminates most abstractions (modules, functions) *)
  let _, ds = FrontendPipeline.process_prog ds in
  (* middle passes do regularization over simplified syntax *)
  let core_ds = MidendPipeline.process_prog ds in
  (* backend does tofino-specific transformations, layout, 
  then translates into p4tofino syntax and produces program strings *)
  let portspec = ParsePortSpec.parse portspec in 
  let p4_str, c_str, py_str = TofinoPipeline.process_prog core_ds portspec build_dir in 
  (* finally, generate the build directory *)
  unmutable_report@@"Compilation to P4 finished. Writing to build directory:"^(build_dir);
  PackageTofinoApp.generate p4_str c_str py_str build_dir

let main () = 
  unmutable_report "Compilation to P4 started...";
  let args = ArgParse.parse_args () in
  (* setup build directory directory. *)
  IoUtils.setup_build_dir args.builddir;
  (* todo: also copy the included files *)
  let _ = cpy_src_to_build args.dptfn args.builddir in
  (* compile lucid code to P4 / python / C *)
  compile_to_tofino args.dptfn args.portspec args.builddir
;;

let _ = main ()


