(* generates simple event parsing and 
   deparsing functions for servers.  *)
open Batteries
open Dpt
open Syntax
open CEventLib

let cfg = Config.cfg


let main () =
  let target_filename = Cmdline.parse_serverlib () in
  Cmdline.set_dpt_file target_filename;
  let ds = Input.parse target_filename in
  let c_str = generate_c ds in
  print_endline ("generating c event library to: "^(Config.cfg.output));
  IoUtils.writef (Config.cfg.output) c_str
;;

let _ = main ()
