(* compile a lucid function (plus globals) to c *)
open Dpt

let cfg = Cmdline.cfg

let init_dpdk_config _ =
  CCoreConfig.cfg.driver <- "dpdk";
  CCoreConfig.cfg.port_id_size <- 16;
  CCoreConfig.cfg.switch_id_size <- 16;
;;

let init_lpcap_config _ =
  CCoreConfig.cfg.driver <- "lpcap";
  CCoreConfig.cfg.port_id_size <- 32;
  CCoreConfig.cfg.switch_id_size <- 32;
;;

(* parse function with added c-compiler args *)
let parse () =
  let speclist = Cmdline.parse_common () in
  let speclist = speclist @ [
    "-o", Arg.String (fun s -> Cmdline.cfg.output <- s), "Output filename.";
    "--dpdk", Arg.Unit (init_dpdk_config), "Compile against dpdk library";    
    "--lpcap", Arg.Unit (init_lpcap_config), "Compile against lpcap library";    
  ] 
  in
  let target_filename = ref "" in
  let usage_msg = "lucidcc (c compiler). Options available:" in
  Arg.parse speclist (fun s -> target_filename := s) usage_msg;  
  Cmdline.set_dpt_file !target_filename; (* for symbolic pass *)
  !target_filename
;;

let main () = 
  let target_filename = parse () in
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
  let prog_str, cflags = CCorePasses.compile ds in
  let base_filename = Filename.chop_extension out_filename in
  let build_cmd = Printf.sprintf "gcc -o %s %s %s" base_filename out_filename cflags in
  let prog_str =  "// "^ build_cmd  ^ "\n" ^ prog_str in
  output_string (open_out out_filename) prog_str
;;


let _ = main ();;