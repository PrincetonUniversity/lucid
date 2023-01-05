open Dpt
open BackendLogging
open Printf
open IoUtils
open Yojson.Basic
module CL = List

exception Error of string
let error s = raise (Error s)
let report str = Console.show_message str ANSITerminal.Green "function compiler"

(* Compiles each lucid handler to a P4 control block 
   with its own copies of all globals. 
   Handlers must never generate events. 

  (* input: dpt program name, output: p4 file *)
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

let compile_to_tofino target_filename =
  (* parse *)
  let ds = Input.parse target_filename in
  (* before the "official" frontend, do temporary optimization 
     passes that will eventually be removed once the 
     mid/back-end is better optimized. *)
  let ds = FunctionInliningSpecialCase.inline_prog_specialcase ds in
  (* frontend type checks and eliminates most abstractions (modules, functions) *)
  let _, ds = FrontendPipeline.process_prog Builtins.tofino_builtin_tys ds in
  (* middle passes do regularization over simplified syntax *)
  let core_ds = MidendPipeline.process_prog ds in
  (* backend does tofino-specific transformations, layout, 
  then translates into p4tofino syntax and produces program strings *)
  let p4_str = TofinoPipeline.compile_handler_block core_ds in 
  IoUtils.writef 
    (target_filename^".temp.p4") 
    (p4_str)

let new_main () = 
  let args = ArgParse.parse_args () in
  setup_build_dir "LucidCompilerLogs";
  compile_to_tofino args.dptfn
;;

let _ = new_main ()
