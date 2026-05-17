(* generates simple event parsing and 
   deparsing functions for servers.  *)
open Batteries
open Dpt
open Syntax
open CEventLib

let main () =
  let target_filename = TofinoConfig.parse_serverlib () in
  let ds = Input.parse target_filename in
  let c_str = generate_c ds in
  print_endline ("generating c event library to: "^(TofinoConfig.serverlib_cfg.output));
  IoUtils.writef (TofinoConfig.serverlib_cfg.output) c_str
;;

let _ = main ()
