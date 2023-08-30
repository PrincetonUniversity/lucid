(* Compile a lucid program to a dataflow graph 
   of conditional statements ready to be 
   laid out on the tofino. *)
(* open Batteries *)
open Dpt
open Cmdline
open BackendLogging
open Printf
open IoUtils
open Yojson.Basic


(* hack in a disable logging option for optimizer iterations.
   turns out this doesn't matter for performance.*)
let do_logging = ref true ;;
let disable_logging () = 
  do_logging := false;
  Cmdline.cfg.verbose <- false;
  Cmdline.cfg.debug <- false;
  (* InterpHelpers.silent := true; *)
  (* NormalizeInts.silent := true;
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
  let usage = "Usage: ./dfgCompiler myProg.dpt myProgDfg.json"

  type args_t =
    { dptfn : string
    ; json_fn : string
    }

  let args_default =
    { dptfn = ""
    ; json_fn = ""
    }
  ;;

  let parse_args () =
    let args_ref = ref args_default in
    (* FIXME: Crazy hack, we should unify the two methods of commandline parsing *)
    let set_symb s = Cmdline.cfg.symb_file <- s in
    let set_json_fn s = args_ref :=  {!args_ref with json_fn=s;} in 
    let speclist =
      [ "-o", Arg.String set_json_fn, "Output json file"
      ; "--symb", Arg.String set_symb, "Path to the symbolic specification file"
      ]
    in
    let parse_aarg (arg : string) =
      Cmdline.cfg.dpt_file <- arg;
      args_ref := { !args_ref with dptfn = arg };
    in
    Arg.parse speclist parse_aarg usage;
    (* interpret positional args as named *)
    !args_ref
  ;;
end

let profile_for_tofino target_filename portspec build_dir profile_cmd = 
  let ds = Input.parse target_filename in
  (* let ds = FunctionInliningSpecialCase.inline_prog_specialcase ds in *)
  let _, ds = FrontendPipeline.process_prog Builtins.tofino_builtin_tys ds in
  let core_ds = MidendPipeline.process_prog ds in
  let portspec = ParsePortSpec.parse portspec in 
  TofinoProfiling.profile core_ds portspec build_dir profile_cmd
;;
let compile_to_dfg target_filename json_fn =
  (* parse *)
  let ds = Input.parse target_filename in
  (* let ds = FunctionInliningSpecialCase.inline_prog_specialcase ds in *)
  let _, ds = FrontendPipeline.process_prog Builtins.tofino_builtin_tys ds in
  let core_ds = MidendPipeline.process_prog ds in
  TofinoProfiling.export_dfg core_ds json_fn;
;;
let main () = 
  disable_logging ();
  unmutable_report "Compiling to dataflow graph for layout...";
  let args = ArgParse.parse_args () in
  compile_to_dfg args.dptfn args.json_fn
;;

let _ = main ()


