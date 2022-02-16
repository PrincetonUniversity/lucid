(* Backend configuration *)
type ll_opts_t = 
  { use_multicast : bool }

let ll_opts_default = 
  { use_multicast = true; }

let ll_opts = ref ll_opts_default
;;

let set_nomc () = 
  Console.show_message 
    "Backend config" 
    ANSITerminal.Green 
    "Compiling without multicast.";
  ll_opts := {use_multicast = false}
;;
