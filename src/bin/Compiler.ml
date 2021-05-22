(* Main file of compiler. *)
(* open Batteries *)
open Dpt
open Consts
open BackendLogging
open LinkP4
open Printf
open IoUtils
[@@@ocaml.warning "-21"] 


let report str = Console.show_message str ANSITerminal.Green "compiler"
;;

(* minimal input
  1. dpt program
  2. P4 harness
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

  let usage = "Usage:\n\
  \twithout p4tapp: ./dptc myProg.dpt myHarness.p4 myBuildDir\n\
  \twith p4tapp: ./dptc myProg.dpt myHarness.p4 myBuildDir [-p4tapp template.p4tapp]"
  type args_t = {dptfn : string; p4fn : string; builddir : string; use_p4tapp : bool; p4tappfn : string; aargs : string list;}
  let args_default = {dptfn = ""; p4fn = ""; builddir = ""; use_p4tapp = false; p4tappfn = ""; aargs = [];} ;;

  let parse_args () = 
    let args_ref = ref args_default in 
    (* set named args *)
    let speclist = [
      ("-p4tapp", Arg.String (fun s -> args_ref := {!args_ref with p4tappfn=s; use_p4tapp = true};), " <p4tapp template directory>");
    ]
    in 
    let parse_aarg (arg:string) = 
        args_ref := {!args_ref with aargs = ((!args_ref).aargs)@[arg]};
    in 
    Arg.parse speclist parse_aarg usage;
    (* interpret positional args as named *)
    let args = match !(args_ref).aargs with 
      | [dptfn; p4fn; builddir] -> {!(args_ref) with dptfn=dptfn; p4fn=p4fn; builddir=builddir; aargs=[]}
      | _ -> error usage
    in 
    args 
  ;;
end 

exception Error of string
let error s = raise (Error s)
(* todo: start all the per-module logging *)
let start_logs () = ()
;;

(* shouldn't need to do this anymore. *)
let clear_output_dir () = 
  (* clear output directory of old source. *)
  let cmd = "rm "^(!outDir)^"/*.dpt" in 
  print_endline ("[clear_output_dir] command: "^cmd);
  let _ = Sys.command cmd in 
  ()
;;

let dump_decls logname decls = 
    print_endline ("----"^logname^"----");
    CL.iter (fun dec -> print_endline ("decl: "^(InstrSyntax.show_decl dec))) decls;
    print_endline ("----"^logname^"----")
  ;;

let dump_decls_from_dagprog logname dagprog = 
  let (dmap, _, _) = dagprog in 
  dump_decls logname (CL.split dmap |> snd)
;;

let log_current_prog fn ds = 
  let full_fn = (!BackendLogging.irLogDir)^"/"^fn in 
  fprintf (open_out full_fn) "%s" (Printing.decls_to_string ds)
;;

(**** compiler passes ****)
let frontend_passes ds = 
  (* type check *)
  let ds = Typer.infer_prog ds in
  (* rename vars *)
  let _, ds = Renaming.rename ds in
  let ds = Typer.infer_prog ds in
  (* inline fcns *)
  let ds = FunctionInlining.inline_prog ds in 
  (* do renaming again, so that the inlined function bodies don't 
  re-use the same variables. *)
  let _, ds = Renaming.rename ds in 
  let ds = Typer.infer_prog ds in
  (* simplify expressions *)
  (* replaced with new cannonizeIf passes *)
  (* let ds = ExpressionReducer.simplify_expressions ds in  *)
  let ds = Typer.infer_prog ds in
  ds
;;

let enable_compound_expressions = false
;;

(* normalize code before translating to ir. *)
let middle_passes ds = 
  Cmdline.cfg.verbose_types <- true;
  let ds = EliminateConsts.eliminate_consts ds in 
  (* compound expression elimination is still a bit buggy. 
  Use it optionally for now. *)
  match enable_compound_expressions with 
    | true -> 
      (* let ds = SingleAssignment.transform ds in  *)
      (* normalize arguments *)
      let ds = PrecomputeArgs.precompute_args ds in 
      (* get rid of boolean expressions *)
      let ds = EliminateBools.do_passes ds in 
      (* convert integer operations into atomic exps *)
      let ds = NormalizeInts.do_passes ds in 
      ds
    | false -> 
      (* get rid of boolean expressions, but don't do the other 
      transformations that normalize expression format. *)
      let ds = EliminateBools.do_passes ds in 
      (* let ds = EliminateBools.elimination_only ds in  *)
      ds
;;

(* translate to tofino ir *)
let to_ir ds = 
  log_current_prog "before_ir.dpt" ds;
  Console.report "converting to tofino instructions";  
  OpGraph.start_log ();
  let dag_instructions = TofinoFromDpt.from_dpt ds in 
  (*   print_endline "done converting to tofino instructions";
  print_endline "--------------";
  print_endline (InstrSyntax.show_instrProg dag_instructions);
  print_endline "--------------";   *)
  Console.report "generating instruction graph";  
  let dag_prog = DagSyntax.translate_instrProg dag_instructions in 
  dag_prog
  (* dump_decls_from_dagprog "AFTER [DagSyntax.translate_instrProg]" dag_prog; *)
;;  

(* do optimizations on tofino ir *)
let backend_passes dag_prog = 
  Console.report "SALU register allocation";
  let dag_prog = RegisterAllocation.doPasses dag_prog in  

  (* dump_decls_from_dagprog "AFTER [RegisterAllocation.doPasses]" dag_prog; *)
 
  Console.report "Control flow elimination";
  let dataflow_dag_prog = BranchElimination.do_passes dag_prog in 

  (* dump_decls_from_dagprog "AFTER [BranchElimination.doPasses]" dataflow_dag_prog; *)

  Console.report "Control DAG -> Data DAG";  
  let dataflow_dag_prog = DataFlow.do_passes dataflow_dag_prog in 

  (* dump_decls_from_dagprog "AFTER [DataFlow.doPasses]" dataflow_dag_prog; *)

  Console.report "Instruction selection / pipeline planning";
  let layout = PipelinePlanning.do_passes dataflow_dag_prog in     
  Console.report "Pipeline generation and straightline program generation";
  let straightline_dpa_prog = PipelineGeneration.do_passes dataflow_dag_prog layout in 
  straightline_dpa_prog
;;

(* new (3/20/21) compilation pipeline *)
let compile_to_tofino ds p4_harness_fn = 
  start_logs ();
  (* compilation *)
  let ds = frontend_passes ds in 
  let ds = middle_passes ds in 
  let dag_instructions = to_ir ds in 
  let straightline_dpa_prog = backend_passes dag_instructions in 

  (* P4 linking *)
  let p4_obj_dict = P4tPrint.from_straightline straightline_dpa_prog in 
  let p4_str = LinkP4.link_p4 p4_obj_dict p4_harness_fn in 

  (* manager code *)
  let c_str = P4tMgrPrint.c_mgr_of straightline_dpa_prog in 
  let py_str = P4tMgrPrint.py_mgr_of straightline_dpa_prog in 
  p4_str, c_str, py_str
;;

let main () =
  let (args) = ArgParse.parse_args () in 
  (* setup output directory. *)
  IoUtils.setup_build_dir args.builddir; 
  let args = {args with dptfn = cpy_src_to_build args.dptfn args.builddir} in 
  let args = {args with p4fn = cpy_src_to_build args.p4fn args.builddir} in 
  let ds = Input.parse args.dptfn in

  (* compile lucid code to P4 and C blocks *)
  let p4_str, c_str, py_str = compile_to_tofino ds args.p4fn in 
  report "Compilation to P4 finished.";
  match args.use_p4tapp with 
    (* generate a P4tapp project directory. note -- this currently does not emit manager code! *)
    | true ->  GenP4tappProj.generate p4_str args.p4fn args.dptfn args.builddir args.p4tappfn 
    (* generate a tofino app *)
    | false -> PackageTofinoApp.generate p4_str c_str py_str args.builddir
;;

let _ = main ()
