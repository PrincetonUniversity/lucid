open Dpt
open Consts
open BackendLogging
open LinkP4
open Printf
open IoUtils
open Yojson.Basic

exception Error of string
let error s = raise (Error s)
let report str = Console.show_message str ANSITerminal.Green "function compiler"

(* Compiles each lucid handler to a P4 control block 
   with its own copies of all globals. 
   Handlers must never generate events. 

  (* input: dpt program name. *)
*)

(* args parsing *)
module ArgParse = struct
  let usage = "Usage: ./dptf myProg.dpt"

  type args_t =
    { dptfn : string
    ; aargs : string list
    }

  let args_default =
    { dptfn = ""
    ; aargs = []
    }
  ;;

  let parse_args () =
    let args_ref = ref args_default in
    (* parse everything as an anon argument. *)
    let parse_aarg (arg : string) =
      args_ref := { !args_ref with aargs = !args_ref.aargs @ [arg] };
    in
    Arg.parse [] parse_aarg usage;
    (* interpret anon args as named. *)
    let args =
      match !args_ref.aargs with
      | [dptfn] -> {!args_ref with dptfn;}
      | _ -> error usage
    in
    args
  ;;
end


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



(* do a rough check to delete unused 
   global declarations. This is just for 
 readability. The P4 compiler will delete 
 unused code too. *)
let delete_unused_globals ds = 
  let id_eq_cid id cid = 
    Cid.equals (Cid.Id id) cid 
  in 
  let in_list id cids = 
    CL.exists (id_eq_cid id) cids
  in 
  (* find all declared globals. *)  
  let declared_globals = CL.filter_map
    (fun (decl:Syntax.decl) -> 
      match decl.d with
       | DGlobal(id, _, _) -> Some (Cid.Id id)
       | _ -> None
    )
    ds
  in 
  (* see which ones are used in the program. *)
  let used_globals = ref [] in 
  let used_global_finder =
    object
      inherit [_] Syntax.s_iter as super
      method! visit_cid _ cid = 
        let b = CL.exists (Cid.equals cid) declared_globals in 
        if (b)
        then (used_globals := cid::(!used_globals);)
    end
  in
  used_global_finder#visit_decls () ds;
  (* remove the globals that are not used. *)
  CL.filter_map 
    (fun (decl:Syntax.decl) -> 
      match decl.d with 
        | DGlobal(id, _, _) -> (
          if (in_list id !used_globals)
          then (Some decl)
          else (None)
        )
        | _ -> Some decl
    )
    ds
;;

(* slice a program up into multiple programs 
   that each have one handler/event and their own copies 
   of all accessed global state. *)
let to_single_handler_progs (ds:Syntax.decls) = 
  let handler_split ((handlers, events), others) (decl:Syntax.decl) = 
    match decl.d with 
      | Syntax.DHandler _ -> ((handlers@[decl], events), others)
      | Syntax.DEvent _ -> ((handlers, events@[decl]), others)
      | _ -> ((handlers, events), others@[decl])
  in 
  let (handlers, events), others = CL.fold_left 
    handler_split 
    (([], []), []) 
    ds 
  in 
  CL.map 
    (fun (handler, event) -> others@[event;handler])
    (CL.combine handlers events)
  |> CL.map delete_unused_globals
;;


let compile_single_handler_to_tofino ds = 
  (* before the "official" frontend, do temporary optimization 
     passes that will eventually be removed once the 
     mid/back-end is better optimized. *)
  let ds = FunctionInliningSpecialCase.inline_prog_specialcase ds in
  (* frontend eliminates high level abstractions (modules, functions) *)
  let _, ds = FrontendPipeline.process_prog ds in
  (* middle passes do a bit of regularization of the syntax tree *)
  let ds = MidendPipeline.process_prog ds in
  (* make sure all the event parameters have unique ids *)
  let ds = InterpHelpers.refresh_event_param_ids ds in
  (* convert handler statement trees into operation-statement graphs *)
  let opgraph_recs = CL.filter_map OGSyntax.opgraph_from_handler ds in
  let opgraph = CL.hd opgraph_recs in 
  LogIr.log_lucid "final_lucid.dpt" ds;
  LogIr.log_lucid_osg "lucid_opstmt.dot" opgraph_recs;
  Console.report "translating to tofino instructions";
  (* translation to backend instructions *)
  let ll_prog = LLTranslate.from_handler ds opgraph in
  (* compute data flow *)
  let df_prog = DFSyntax.to_dfProg ll_prog in
  LogIr.log_lir "initial_ir" df_prog;
  (* backend optimization, layout, and straightlining *)
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
  (* print to a P4 control object *)
  let p4_ctlblock_str = P4tPrint.to_p4ctl_str straightline_prog in 
  print_endline ("-------- P4 control block --------");
  print_endline p4_ctlblock_str;
  print_endline ("----------------------------------");
  p4_ctlblock_str
;;

(* compile each handler in the program to its own P4 block 
with local copies of all state. *)
let compile_handlers_to_p4_controls target_filename = 
  start_backend_logs ();
  (* parse *)
  let ds = Input.parse target_filename in
  print_endline (Printing.decls_to_string ds);
  let handler_progs = to_single_handler_progs ds in 
  let p4_ctl_strs = CL.map 
    compile_single_handler_to_tofino 
    handler_progs 
  in 
  p4_ctl_strs
;;

let main () =
  let p4t_includes = "#include <core.p4>\n#include <tna.p4>" in   
  let args = ArgParse.parse_args () in
  setup_build_dir "LucidCompilerLogs";
  (* compile the lucid function to a P4 block. 
     Note: since we're just compiling a function, there's 
     no control plane stuff to do. *)
  let p4_ctl_strs = compile_handlers_to_p4_controls args.dptfn in   
  report "Compilation to P4 finished.";
  (* write the generated P4 blocks to the file. *)
  IoUtils.writef 
    (args.dptfn^".p4") 
    (String.concat "\n" (p4t_includes::p4_ctl_strs))
;;

let _ = main ()
