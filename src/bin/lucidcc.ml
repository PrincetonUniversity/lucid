(* compile a lucid function (plus globals) to c *)
open Dpt

let cfg = Config.cfg

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
let build_dir = ref None
(* parse function with added c-compiler args *)
let parse () =
  let speclist = Config.parse_common () in
  let speclist = speclist @ [
    "-o", Arg.String (fun s -> Config.cfg.output <- s), "Output filename.";
    "--dpdk", Arg.Unit (init_dpdk_config), "Compile against dpdk library";    
    "--lpcap", Arg.Unit (init_lpcap_config), "Compile against lpcap library";  
    "--build", Arg.String (fun s -> build_dir := Some(s)), "Output directory for build files. Overrides output filename.";
  ] 
  in
  let target_filename = ref "" in
  let usage_msg = "lucidcc (c compiler). Options available:" in
  Arg.parse speclist (fun s -> target_filename := s) usage_msg;  
  Config.set_dpt_file !target_filename; (* for symbolic pass *)
  !target_filename
;;

let main () = 
  let target_filename = parse () in
  let out_filename = Config.cfg.output in 

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
  let generated_files = CCorePasses.compile ds in
  match (!build_dir) with 
    | Some(dir) -> 
      (* make sure the directory exists, put all the files there *)
      print_endline ("Writing files to " ^ dir);
      IoUtils.ensure_dir dir;
      List.iter (fun (filename, prog_str) -> 
        let out_filename = Filename.concat dir filename in
        print_endline ("Writing " ^ out_filename);
        output_string (open_out out_filename) prog_str
      ) generated_files;
      (* put the source files there too. Kind of hacky. *)
      IoUtils.ensure_dir (Filename.concat dir "src");
      let source_fnames = Input.get_all_source_filenames target_filename in
      List.iter (fun source_fn -> let _ = IoUtils.cpy_src_to_build source_fn dir in ()) source_fnames;


    | None -> 
      (* just write the program to out_filename *)
      let prog_str = List.assoc "lucidprog.c" generated_files in
      print_endline ("Writing " ^ out_filename);
      output_string (open_out out_filename) prog_str;
;;


let _ = main ();;