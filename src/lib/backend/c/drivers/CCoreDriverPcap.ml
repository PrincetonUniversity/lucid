open CCoreSyntax
open CCoreExceptions
open CCoreUtils
open CCoreDriverInterface


(* Simple Libpcap toplevel. 
    Just uses the default helpers, imports, pkt_handler and main_fun, 
    and has a simple main function that opens some pcaps from stdin *)   

let helpers = default_helpers
let imports = default_imports
let pkt_handler = default_pkt_handler
let main = 
  dforiegn 
{|int main(int argc, char const *argv[])
  {
    /* code */
    return 0;
  }|}

let cflags = "-lpcap"

(* *)

