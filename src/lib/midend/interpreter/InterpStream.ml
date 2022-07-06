(* 
  streaming input and output for interactive interpreter
    - read from stdin
    - write to stdout
*)


(* read an event from stdin *)
let get_event () = 
  print_endline "Event> ";
  flush stdout;
  let event_line = input_line stdin in 
  event_line
  ;;