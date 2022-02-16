(* Main file of compiler. *)
(* open Batteries *)
open Dpt
open Consts
open BackendLogging
open LinkP4
open Printf
open IoUtils
open Yojson.Basic

[@@@ocaml.warning "-21"]

let report str = Console.show_message str ANSITerminal.Green "compiler"

(* minimal input
   1. dpt program
   2. P4 harness
   3. entry event trigger configuration file
   3. build directory *)

(* output (in build dir):
   1. P4 program (dpt + harness) -- lucid.p4
   2. switchd program -- lucid.cpp
   3. build script
   4. run sim script
   5. run hw script
   6. makefile *)

(* args parsing *)
module ArgParse = struct
  let usage = "Usage: ./dptc myProg.dpt myHarness.p4 myHarness.json myBuildDir"

  type args_t =
    { dptfn : string
    ; p4fn : string
    ; configfn : string option
    ; builddir : string
    ; interp_spec_file : string
    ; aargs : string list
    }

  let args_default =
    { dptfn = ""
    ; p4fn = ""
    ; configfn = None
    ; builddir = ""
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
    let speclist =
      [ ( "--spec"
        , Arg.String set_spec
        , "Path to the interpreter specification file" )
      ; "--symb", Arg.String set_symb, "Path to the symbolic specification file"
      ; "--nomc", Arg.Unit LLConfig.set_nomc, "Disable multicast" 
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
      | [dptfn; p4fn; configfn; builddir] ->
        { !args_ref with dptfn; p4fn; configfn=(Some configfn); builddir; aargs = [] }
      | [dptfn; p4fn; builddir] -> (* manual linking for custom entry / exit P4 integration *)
        { !args_ref with dptfn; p4fn; configfn=None; builddir; aargs = [] }
      | _ -> error usage
    in
    args
  ;;
end

exception Error of string

let error s = raise (Error s)

(* start per-pass logs. *)
let start_backend_logs () = 
  LLTranslate.start_logging ();
  LLOp.start_logging ();
  LLContext.start_logging ();

  OGSyntax.start_logging ();
  BranchElimination.start_logging ();
  MergeUtils.start_logging ();
  DataFlow.start_logging ();
  Liveliness.start_logging ();
  RegisterAllocation.start_logging ();
  PipeSyntax.start_logging ();

  P4tPrint.start_logging ();
  ()
;;
(* shouldn't need to do this anymore. *)
let clear_output_dir () =
  (* clear output directory of old source. *)
  let cmd = "rm " ^ !outDir ^ "/*.dpt" in
  print_endline ("[clear_output_dir] command: " ^ cmd);
  let _ = Sys.command cmd in
  ()
;;

let dump_decls logname decls =
  print_endline ("----" ^ logname ^ "----");
  CL.iter (fun dec -> print_endline ("decl: " ^ LLSyntax.show_decl dec)) decls;
  print_endline ("----" ^ logname ^ "----")
;;

let dump_decls_from_dagprog logname dagprog =
  let dmap, _, _ = dagprog in
  dump_decls logname (CL.split dmap |> snd)
;;

(**** compiler passes ****)
(* do a few final pre-ir setup and temporary passes *)
let final_ir_setup ds =
  (* make sure all the event parameters have unique ids *)
  let ds = InterpHelpers.refresh_event_param_ids ds in
  (* In the body of each handler, replace parameter variables with struct instance fields. *)
  (* let ds = CL.map TranslateHandlers.rename_handler_params ds in  *)
  (* generate the control flow graph version of the program *)
  (* convert handler statement trees into operation-statement graphs *)
  let opgraph_recs = CL.filter_map OGSyntax.opgraph_from_handler ds in
  ds, opgraph_recs
;;

(* translate to lucid ir *)
let to_ir ds =
  let ds, opgraph_recs = final_ir_setup ds in
  LogIr.log_lucid "final_lucid.dpt" ds;
  LogIr.log_lucid_osg "lucid_opstmt.dot" opgraph_recs;
  Console.report "converting to tofino instructions";
  let dag_instructions = LLTranslate.from_dpt ds opgraph_recs in
  let df_prog = DFSyntax.to_dfProg dag_instructions in
  LogIr.log_lir "initial_ir" df_prog;
  df_prog
;;

(* do optimizations on tofino ir *)
let backend_passes df_prog =
  Console.report "SALU register allocation";
  let df_prog = RegisterAllocation.merge_and_temp df_prog in
  Console.report "Control flow elimination";
  let df_prog = BranchElimination.do_passes df_prog in
  Console.report "Control flow -> Data flow";
  let dataflow_df_prog = DataFlow.do_passes df_prog in
  LogIr.log_lir "partial_df_nobranch" df_prog;
  LogIr.log_lir "df_prog" dataflow_df_prog;
  Console.report "Data flow -> Pipeline";
  let pipe, straightline_prog = PipeSyntax.do_passes dataflow_df_prog in
  LogIr.log_lir_pipe "pipe_ir" pipe;
  straightline_prog
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

(* new (3/20/21) compilation pipeline *)
let compile_to_tofino target_filename p4_harness_fn config_fn_opt interp_spec_fn =
  start_backend_logs ();
  (* parse *)
  let ds = Input.parse target_filename in
  let _ = interp_spec_fn in
  (* before the "official" frontend, do temporary optimization 
     passes that will eventually be removed once the 
     mid/back-end is better optimized. *)
  let ds = FunctionInliningSpecialCase.inline_prog_specialcase ds in
  (* frontend eliminates most abstractions (modules, functions) *)
  let _, ds = FrontendPipeline.process_prog ds in
  (* middle passes do a bit of regularization of the syntax tree *)
  let ds = MidendPipeline.process_prog ds in
  (* convert to IR for backend *)
  let dag_instructions = to_ir ds in
  (* backend passes do optimization and layout. *)
  let straightline_dpa_prog = backend_passes dag_instructions in
  (* printing: to blocks of P4 *)
  let p4_obj_dict = P4tPrint.from_straightline straightline_dpa_prog in
  (* the linker is really just a simple macro engine. Pass it an associative
     list: (pragma string, code string to replace macro with) *)
  (* generate the entry event trigger table *)
  (* generate entry event triggers (if any) from config file *)
  let trigger_macro_defs = match config_fn_opt with 
    | Some config_fn -> JsonBlocks.generate config_fn
    | None -> [] 
  in
  (* linking: put p4 blocks together into a single file. *)
  let p4_str =
    LinkP4.link_p4 (p4_obj_dict @ trigger_macro_defs) p4_harness_fn
  in
  (* printing: other manager code *)
  let c_str = P4tMgrPrint.c_mgr_of straightline_dpa_prog in
  let py_str = P4tMgrPrint.py_mgr_of straightline_dpa_prog in
  p4_str, c_str, py_str
;;

let main () =
  let args = ArgParse.parse_args () in
  (* setup output directory. *)
  IoUtils.setup_build_dir args.builddir;
  (* todo: also copy the included files *)
  let _ = cpy_src_to_build args.dptfn args.builddir in
  let _ = cpy_src_to_build args.p4fn args.builddir in
  (* compile lucid code to P4 and C blocks *)
  let p4_str, c_str, py_str =
    compile_to_tofino args.dptfn args.p4fn args.configfn args.interp_spec_file
  in
  report "Compilation to P4 finished.";
  PackageTofinoApp.generate p4_str c_str py_str args.builddir
;;

let _ = main ()
