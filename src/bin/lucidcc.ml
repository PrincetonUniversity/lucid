(* compile a lucid function (plus globals) to c *)
open Dpt

let main () = 
  let target_filename = Config.parse_c () in
  let out_filename = Config.c_cfg.output in 

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
  match (Config.c_cfg.build_dir) with 
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