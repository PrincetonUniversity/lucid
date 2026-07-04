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
    (* port/switch id widths are 32, matching lpcap/rawsock and the rest of the
       pipeline (the midend port arg, CCoreHandlers.port_size's module-load
       default, and the generated out_event.port field are all 32). Using 16 here
       -- the lone outlier -- made the first CCoreTyper.check die with "int types
       with different lengths (16 vs 32)" on any generate_port program. The port is
       internal dispatch metadata (never serialized to the wire), and the dpdk
       driver narrows out_event.port to uint16_t for the actual rte_eth_tx_burst
       call, so 32 here is correct and costs nothing. *)
    c_cfg.port_id_size <- 32;
    c_cfg.switch_id_size <- 32;
  in
  let init_lpcap_config _ =
    c_cfg.driver <- "lpcap";
    c_cfg.port_id_size <- 32;
    c_cfg.switch_id_size <- 32;
  in
  let init_rawsock_config _ =
    c_cfg.driver <- "rawsock";
    c_cfg.port_id_size <- 32;
    c_cfg.switch_id_size <- 32;
  in
  [
    "-o", Arg.String (fun s -> c_cfg.output <- s), "Output filename.";
    "--dpdk", Arg.Unit (init_dpdk_config), "Compile against dpdk library";
    "--lpcap", Arg.Unit (init_lpcap_config), "Compile against lpcap library";
    "--rawsock", Arg.Unit (init_rawsock_config), "Compile a raw-socket switch (runs on real interfaces, like lucidSwitch)";
    "--build", Arg.String (fun s -> c_cfg.build_dir <- Some(s)), "Output directory for build files. Overrides output filename.";
  ]
;;

let parse_c () = parse
  c_speclist
  "lucidcc (c compiler). Options available:"
;;
