(* compile a lucid function (plus globals) to c *)
open Batteries
open Dpt

let cfg = Cmdline.cfg


let main () =
  if ((Array.length Sys.argv) != 2)
    then failwith "Usage: interpfcn <lucid program> <output filename>";
  let target_filename = Sys.argv.(1) in
  (* now parse the file and run the frontend and midend *)
  Cmdline.set_dpt_file target_filename;
  let ds = Input.parse target_filename in
  let _, ds =
    (* Profile.time_profile "frontend" @@ fun () -> *)
    FrontendPipeline.process_prog Builtins.interp_builtin_tys ds
  in
  let ds =
    (* Profile.time_profile "midend" @@ fun () -> *)
    MidendPipeline.process_prog ds
  in
  (* now we're ready to call the experimental backend *)
  (* finally, interpret a call to the interpreter...  *)
  print_endline ("compiling");
;;

let _ = main ();;