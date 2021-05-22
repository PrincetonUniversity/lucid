(* Generates configuration blocks to run 
at startup on the controller. *) 
open TofinoConstants
open InstrSyntax


(* A single multicast group 
   used to generate new event packets. *)
let event_pkt_generator_mc_config = 
  new_config_block 
    (Cid.create ["single_event_pkt_generator"])
    (McConfig [{
  	mc_id = lucid_mc_group;
  	mc_instrs = [{
  		port = lucid_recirc_port;
  		pkt_copy_id = 1;
  	}]
  }])
;;

let generate ds = 
  let _ = ds in 
  [event_pkt_generator_mc_config]
;;

