(* C backend config *)
open Config

(* C backend *)
type c_config = {
  mutable output : string;
  mutable port_id_size : int;
  mutable switch_id_size : int; (* for testing compatibility *)
  mutable recirc_port : int;
  mutable self_id_num: int;
  mutable driver : string;
  mutable build_dir : string option;
}
;;
let c_cfg = {
  output = "";
  port_id_size = 32;
  switch_id_size = 32;
  recirc_port = 0;
  self_id_num = 0;
  driver = "lpcap";
  build_dir = None
} 
;;

let c_speclist = 
  let init_dpdk_config _ =
    c_cfg.driver <- "dpdk";
    c_cfg.port_id_size <- 16;
    c_cfg.switch_id_size <- 16;
  in
  let init_lpcap_config _ =
    c_cfg.driver <- "lpcap";
    c_cfg.port_id_size <- 32;
    c_cfg.switch_id_size <- 32;
  in  
  [
    "-o", Arg.String (fun s -> c_cfg.output <- s), "Output filename.";
    "--dpdk", Arg.Unit (init_dpdk_config), "Compile against dpdk library";    
    "--lpcap", Arg.Unit (init_lpcap_config), "Compile against lpcap library";  
    "--build", Arg.String (fun s -> c_cfg.build_dir <- Some(s)), "Output directory for build files. Overrides output filename.";
  ]
;;

let parse_c () = parse
  c_speclist
  "lucidcc (c compiler). Options available:"
;;
