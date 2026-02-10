(* Port configuration 

*)

type portid = int

type port = 
  | SimLink of {num:portid;} (* simulated link *)
  | Interface of {num:portid; interface:string} (* posix net interface *)
  | Recirc of {num:portid;} (* recirc port *)
  | Stdio of {num:portid;} (* standard io port *)
