(* Toplevel drivers for the program *) 
open CCoreSyntax
open CCorePPrint
let sprintf = Printf.sprintf

let cid s = Cid.create [s]
let id = Id.create

(* Note: 
  stdin is hard because of strings. 
  We should start with a simple driver that reads events from a binary file. *)

module BinaryFileDriver = struct 
  (* left off here process binary events from a file *)
  (* main: 
      1. open input file and output file. 
      2. while input file is not empty: 
          a. read an event from the input file. 
          b. parse the event. 
          c. in an inner loop: 
              1. handle the event
              2. if the handler generates a local event, repeat the inner loop
          d. if there is an output event, write it to the output file.   
  *)
              

end
