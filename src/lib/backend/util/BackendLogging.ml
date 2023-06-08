(* Logging functions *)
open Batteries
open Format
module CF = Caml.Format

exception Error of string

let error s = raise (Error s)

(* output directories *)
let moduleLogDir = ref "./lucidCompilerOut/modules"
let irLogDir = ref "./lucidCompilerOut/ir"
let graphLogDir = ref "./lucidCompilerOut/graphs"
let dopen f out = out := Some (CF.formatter_of_out_channel f)
let null_out = CF.formatter_of_out_channel (Caml.open_out "/dev/null")
let no_printf (_ : string) = ()

let printf outf fmt =
  match !outf with
  | None -> CF.kfprintf (fun out -> CF.fprintf out "") null_out fmt
  | Some outf -> CF.kfprintf (fun out -> CF.fprintf out "%!") outf fmt
;;

let dclose f out =
  (match !out with
  | None -> ()
  | Some out -> pp_print_flush out ());
  Caml.close_out f
;;

let start_mlog module_name outc logfcn_ref =
  Core.Unix.mkdir_p !moduleLogDir;
  let module_name = Filename.basename module_name in
  let out_fn = !moduleLogDir ^ "/" ^ module_name ^ ".txt" in
  (* print_endline (sprintf "logging for %s: %s" module_name out_fn); *)
  let outf = Caml.open_out out_fn in
  dopen outf outc;
  printf outc "%s" ("---" ^ module_name ^ " log---\n");
  Caml.flush outf;
  logfcn_ref := printf outc "%s\n"
;;

(* open the output file reference, get a debug printline function. *)
let start_module_log module_name outc =
  Core.Unix.mkdir_p !moduleLogDir;
  let module_name = Filename.basename module_name in
  let out_fn = !moduleLogDir ^ module_name ^ ".txt" in
  (* print_endline (sprintf "logging for %s: %s" module_name out_fn); *)
  let outf = Caml.open_out out_fn in
  dopen outf outc;
  (* printf outc "%s" ("---"^module_name^" log---\n"); *)
  printf outc "%s\n"
;;

let dbg_print_example () =
  (* example usage *)
  let my_outf = Caml.open_out "DEBUG_PRINT_EXAMPLE.txt" in
  let my_out = ref None in
  dopen my_outf my_out;
  printf my_out "visitList.append(\"%s\")\n" "example_table";
  printf my_out "visitList.append(\"%s\") %i\n" "example_table" 2;
  dclose my_outf my_out;
  ()
;;

(* dbg_print_example () ;; *)


(* misc helpers used in tofinoPipeline *)

let fail_report str = Console.show_message str ANSITerminal.Red "Tofino Checker"
let cprint_endline s = if Cmdline.cfg.debug then print_endline s



let mk_ir_log_dirs () =
  Core.Unix.mkdir_p !irLogDir;
  Core.Unix.mkdir_p !graphLogDir
;;


let ir_dump_path phasename = !irLogDir ^ "/" ^ phasename ^ ".dpt"
