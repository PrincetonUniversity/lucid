(**** 
  Constants used specifically by the Tofino backend.  
  This should eventually replace "Consts.ml" in the backend. 
  a lot of this stuff should be configuration parameters, 
  or derived automatically from the template P4 program. 
****)
open LLSyntax
module GS = LLSyntax.Generators
module CL = Caml.List

let event_structdef_suffix = "_t"
let out_structname_prefix = "new_"
let md_instance_prefix = "md"
let md_instance = Id.create "md"
(* Id of the struct. refactor to use this instead of the strings. *)

let md_instance_cid = Cid.create_ids [md_instance]
let hdr_instance = Id.create "hdr"
let hdr_instance_cid = Cid.create_ids [hdr_instance]
let hdr_instance_prefix = "hdr"
let pkt_instance_prefix = "pkt"

(* dpt internal ingress metadata *)
let dpt_meta_str = "dptMeta"

let timestamp_str = "timestamp"
let timestamp_width = 32
let handle_selector_str = "eventType"
let handle_selector_width = 8
let exit_event_str = "exitEventType"
let exit_event_width = 8
let next_event_str = "nextEventType"
let next_event_width = 8
let events_count_str = "eventsCount"
let events_count_width = 16
let event_port_str = "outPort"
let event_port_width = 9
let event_group_str = "outGroup"
let event_group_width = 16
let dpt_meta_struct_instance = Cid.create [md_instance_prefix; dpt_meta_str]


let timestamp_field =
  Cid.create_ids
    (Cid.to_ids dpt_meta_struct_instance @ [Id.create timestamp_str])
;;
let current_event_field =
  Cid.create_ids
    (Cid.to_ids dpt_meta_struct_instance @ [Id.create handle_selector_str])
;;
let handle_selector_name = current_event_field ;;
let exit_event_field =
  Cid.create_ids
    (Cid.to_ids dpt_meta_struct_instance @ [Id.create exit_event_str])
;;
let next_event_field =
  Cid.create_ids
    (Cid.to_ids dpt_meta_struct_instance @ [Id.create next_event_str])
;;
let event_count_field =
  Cid.create [md_instance_prefix; dpt_meta_str; events_count_str]
;;
let event_port_field =
  Cid.create [md_instance_prefix; dpt_meta_str; event_port_str]
;;
let event_group_field =
  Cid.create [md_instance_prefix; dpt_meta_str; event_group_str]
;;



(* lucid's internal metadata struct for ingress processing *)
let lucid_internal_struct = 
  let struct_cid = Cid.create ["dptMeta_t"] in
  let struct_instance_cid = dpt_meta_struct_instance in 
  let struct_fields =
    CL.map
      (fun (f, w) -> (Cid.create [f], w))
      [ timestamp_str, timestamp_width
      ; handle_selector_str, handle_selector_width
      ; exit_event_str, exit_event_width
      ; next_event_str, next_event_width
      ; events_count_str, events_count_width 
      ; event_port_str, event_port_width
      ; event_group_str, event_group_width
      ; ]
  in 
  let dptMeta_struct = LLSyntax.new_meta_structdef struct_cid struct_fields in
  let dptMeta_instance =
    LLSyntax.new_struct struct_cid () struct_instance_cid
  in
  [dptMeta_struct; dptMeta_instance]
;;  




(* hidden event fields *)
let event_id_field = Cid.create ["eventType"]
let event_id_width = 8
let event_loc_field = Cid.create ["eventLoc"]
let event_loc_width = 32
let event_delay_field = Cid.create ["eventDelay"]
let event_delay_width = 32
(* hidden fields that every event carries. *)
let hidden_event_fields = [
  (event_id_field, event_id_width); 
  (event_loc_field, event_loc_width); 
  (event_delay_field, event_delay_width)
]


(* ids of code building functions *)
let generate_self_cid = Cid.create ["generate_self"]
let generate_port_cid = Cid.create ["generate_port"]
let generate_ports_cid = Cid.create ["generate_ports"]

let lucid_parser_name = Cid.create ["DptIngressParser"]
let final_table_name = Cid.create ["dptContinueHandler"]
let final_invalidate_acn_name = Cid.create ["invalidateInputHeaders"]
let t_warn str = Console.show_message str ANSITerminal.Yellow "DPT-to-Tofino"
let t_info str = Console.show_message str ANSITerminal.Green "DPT-to-Tofino"

(* used by scheduler *)
let lucid_etype = 0x1111
let ip_etype = 0x0800
let lucid_mc_group = 1066
let lucid_recirc_port = 196

(*** 11/21 -- new runtime-generated struct ids for multi-event generation ***)
let event_out_flags_struct = Cid.create ["ev_out_flags_t"]

(* assumes that we link into the struct 
   with id md_instance, which is declared externally. *)
let event_out_flags_instance =
  Cid.create_ids [hdr_instance; Id.create "ev_out_flags"]
;;

(**** 5/22 -- builtin datatypes and variables ****)

(* The lucid footer that goes after all the event headers. *)
let footer_t = Cid.create ["lucid_footer_t"]
let footer = Cid.create ["lucid_footer"]
let footer_fields = [Cid.create ["end"], 8]
let footer_struct = GS.hdr_struct footer_t (CL.split footer_fields)
let footer_instance = GS.struct_inst footer_struct footer
let footer_instance_cid = Cid.concat (Cid.create_ids [hdr_instance]) footer


(*** some more misc config that really should be in a config file ***)
let max_generated_events = 4
let active_ports = [128; 129; 130; 131]
let packet_unicast_field = Cid.create ["ig_tm_md"; "ucast_egress_port"]
(* used for control event generation *)
let multicast_group_a = Cid.create ["ig_tm_md"; "mcast_grp_a"]
(* used for (ports) packet event generation *)
let packet_multicast_field = Cid.create ["ig_tm_md"; "mcast_grp_b"]
