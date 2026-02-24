(* Interpreter switch config (lucidSwitch.ml) *)
open Config

type interface = {
  switch : int;
  port   : int;
  interface : string
}

type config = 
  {
    mutable show_printf        : bool (* report printf statements *)
  ; mutable interface : interface list (** port id to interface bindings **)
  }
;;
let cfg : config = 
  {
    show_printf = true
  ; interface = []
  }
;;

let speclist = 
  let hide_printf () = cfg.show_printf <- false in
  let set_interface (intf : string) =
    match String.split_on_char ':' intf with
    | [port_str; interface_name] ->
      let switch = 0 in
      let port = int_of_string port_str in
      cfg.interface <- cfg.interface@[{ switch; port; interface = interface_name }]
    | _ -> failwith "Invalid interface format. Expected: <portnum>:<interfacename>"
  in
  [ 
        ( "--no-printf"
        , Arg.Unit hide_printf
        , "If set, don't print printf statements while interpreting. ")
      ; ( "--interface"
        , Arg.String set_interface
        , "port:interface Bind the internal port number to the interface name" )
   ]  
;;

let parse_args () = 
  InterpConfig.cfg.spec_file <- "";
  InterpConfig.cfg.show_interp_state <- false;
  InterpConfig.cfg.show_interp_events <- false;
  InterpConfig.cfg.show_printf <- cfg.show_printf;
  InterpConfig.cfg.interactive <- true;
  InterpConfig.cfg.json <- true;
  Config.base_cfg.verbose <- false;
  InterpConfig.cfg.show_interp_state <- false;
  InterpConfig.cfg.show_interp_events <- false;  
  parse speclist "Lucid software switch. Options:"
;;
