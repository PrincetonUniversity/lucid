(* convert tables to inlined tables 

  We inline tables because without tuples or records in the backend 
  ir, non-inlined tables cannot return / modify more than 1 variable. 
*)



let inline_prog ds =
  print_endline "inlining tables... (TODO)";
  exit 1;
;;