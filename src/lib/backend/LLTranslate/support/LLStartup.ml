(* Generates configuration blocks to run 
at startup on the controller. *)
open LLConstants
open LLSyntax
open MiscUtils

(* A single multicast group 
   used to generate new event packets. 

  group name  num copies  replica ids
  1066        1           1
  1066+1      2           1; 2
  1066+2      3           1; 2; 3

 *)
let event_pkt_generator_mc_config =
  let mk_mc_group num_extra_copies =
    let mk_mc_copy_instr copy_id =
      { port = lucid_recirc_port; pkt_copy_id = copy_id }
    in
    let copy_instrs =
      CL.map mk_mc_copy_instr (range 1 (num_extra_copies + 2))
    in
    { mc_id = lucid_mc_group + num_extra_copies
    ; (* base group represents 1 copy. *)
      mc_instrs = copy_instrs
    }
  in
  let mc_groups = CL.map mk_mc_group (range 0 max_generated_events) in
  new_config_block (Cid.create ["event_pkt_generator"]) (McConfig mc_groups)
;;

(* (Cid.create ["single_event_pkt_generator"]) *)
(*     (McConfig
       [ { mc_id = lucid_mc_group
         ; mc_instrs = [{ port = lucid_recirc_port; pkt_copy_id = 1 }]
         } ]) *)

let generate ds =
  let _ = ds in
  [event_pkt_generator_mc_config]
;;
