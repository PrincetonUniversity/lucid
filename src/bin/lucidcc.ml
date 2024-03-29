(* compile a lucid function (plus globals) to c *)
open Dpt

let cfg = Cmdline.cfg

(* parse function with added c-compiler args *)
let parse () =
  let speclist = Cmdline.parse_common () in
  let set_output s = cfg.output <- s in
  let speclist = speclist @ ["-o", Arg.String set_output, "Output filename."] in
  let target_filename = ref "" in
  let usage_msg = "lucidcc (c compiler). Options available:" in
  Arg.parse speclist (fun s -> target_filename := s) usage_msg;
  !target_filename
;;

let main () = 
  let target_filename = parse () in
  Cmdline.set_dpt_file target_filename;
  let out_filename = Cmdline.cfg.output in 

  let ds = Input.parse target_filename in
  (* run frontend pipeline with options to:
      - allow multiple handlers for an event
      - preserve user-defined record types to the midend ir *)
  let _, ds =
    FrontendPipeline.process_prog ~opts:{
      match_event_handlers=false; 
      elim_records=false;
      } Builtins.interp_builtin_tys ds
  in
  print_endline (" --- compiling to c --- ");
  let prog_str = CCorePasses.compile ds in
  output_string (open_out out_filename) prog_str
;;


let _ = main ();;