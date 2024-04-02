type ccore_config = {
  port_id_size : int;
  switch_id_size : int; (* not really used, just ofr tst compatability *)
  recirc_port : int;
  self_id_num: int;
}
let default_config = {
  port_id_size = 8;
  switch_id_size = 32;
  recirc_port = 0;
  self_id_num = 0;
}

let cfg = ref default_config