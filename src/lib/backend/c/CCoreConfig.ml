type ccore_config = {
  mutable port_id_size : int;
  mutable switch_id_size : int; (* not really used, just ofr tst compatability *)
  mutable recirc_port : int;
  mutable self_id_num: int;
  mutable driver : string;
}
let default_config () = {
  port_id_size = 32;
  switch_id_size = 32;
  recirc_port = 0;
  self_id_num = 0;
  driver = "lpcap" 
}

let cfg = default_config ()