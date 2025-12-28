(* tofino backend config *)
open Config

type tofino_config = 
  { mutable builddir : string (* build directory where p4 + other code goes *)
  ; mutable portspec : string option (* path to port specification file *)
  ; mutable ports :(int * int) list (* list of port ids and speeds *)
  ; mutable recirc_port : int (* port id for recirculation *)
  ; mutable profile_cmd :
      string option (* something with profiling -- probably depreciated *)
  ; mutable ctl_fn : string option (* path to optional python control program *)
  ; mutable serverlib : bool
      (* If false, disable the python event library generation *)
  ; mutable optimal_memop_input_alloc : bool
    (* find an allocation of memop inputs to sALU input register that requires no extra copy operations.
      In some cases, this may create larger PHV clusters. *)    
  }
;;

let tofino_cfg : tofino_config = 
  { builddir = "lucid_tofino_build"
  ; portspec = None
  ; ports = [(128, 10); (129, 10); (130, 10); (131, 10)] 
  ; recirc_port = 196
  ; profile_cmd = None
  ; ctl_fn = None
  ; serverlib = false
  ; optimal_memop_input_alloc = true
  } 
;;

let tofino_speclist = 
  let set_builddir (s : string) =  tofino_cfg.builddir <- s in
  let set_portspec (s : string) =  tofino_cfg.portspec <- Some s in
  let set_profile_cmd (s : string) =  tofino_cfg.profile_cmd <- Some s in
  let set_ctl_fn (s : string) =  tofino_cfg.ctl_fn <- Some s in
  let set_serverlib () = tofino_cfg.serverlib <- true in
  [ "-o", Arg.String set_builddir, "Output build directory."
      ; ( "--ports"
        , Arg.String set_portspec
        , "Path to the ports specification file" )
      ; ( "--port"
        , Arg.String
            (fun s ->
              let (id, speed) =
                match String.split_on_char '@' s with
                | [id; speed] -> (int_of_string id, int_of_string speed)
                | _ -> failwith "Invalid port specification"
              in
               tofino_cfg.ports <- (id, speed) ::  tofino_cfg.ports)
        , "--port <dpid>@<speed> Specify a port to be brought up automatically in the generated control plane. Can be used multiple times." )
      ; ( "--recirc_port"
        , Arg.Int (fun i ->  tofino_cfg.recirc_port <- i)
        , "Port id for recirculation" )
      ; ( "-p"
        , Arg.String set_profile_cmd
        , "Profile program instead of compiling." )
      ; "--control", Arg.String set_ctl_fn, "Python control program."
      ; ( "--old_memop_alloc"
      , Arg.Unit (fun () ->  tofino_cfg.optimal_memop_input_alloc <- false)
      , "If true, use the old memop argument to sALU register allocator, which may \
        add extra copy operations, but is more extensively tested and might produce allocations \
        with smaller phv cluster sizes.")    
      ; ( "--serverlib"
      , Arg.Unit set_serverlib
      , "If true, generate the python event library generator" ) 
  ]
;;


let parse_tofino () = parse 
  tofino_speclist
  "Lucid Tofino compiler. Options:"
;;


(* c event binding generator *)
type serverlib_config = {
  mutable output : string
}
;;
let serverlib_cfg : serverlib_config = {
  output = ""
}
;;

let parse_serverlib () = 
  let speclist = ["-o", Arg.String (fun s -> serverlib_cfg.output <- s), "Output filename."] in
  parse
    speclist
    "Event interface generator. Options: "
;;