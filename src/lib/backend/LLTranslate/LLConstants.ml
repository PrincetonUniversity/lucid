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
let md_instance = Id.create "md" (* Id of the struct. refactor to use this instead of the strings. *)
let md_instance_cid = Cid.create_ids [md_instance]

let hdr_instance = Id.create "hdr"
let hdr_instance_cid = Cid.create_ids [hdr_instance]
let hdr_instance_prefix = "hdr"
let pkt_instance_prefix = "pkt"

(* dpt private metadata *)
let dpt_meta_str = "dptMeta"
let handle_selector_str = "eventType"
let exit_event_str = "exitEventType"
let next_event_str = "nextEventType"
let timestamp_str = "timestamp"
let events_count_str = "eventsCount"
let dpt_meta_struct_instance = Cid.create [md_instance_prefix; dpt_meta_str]

let timestamp_field =
  Cid.create_ids
    (Cid.to_ids dpt_meta_struct_instance @ [Id.create timestamp_str])
;;

let exit_event_field =
  Cid.create_ids
    (Cid.to_ids dpt_meta_struct_instance @ [Id.create exit_event_str])
;;
let exit_event_width = 8
;;

let current_event_field = 
  Cid.create_ids
    (Cid.to_ids dpt_meta_struct_instance @ [Id.create handle_selector_str])
;;

let next_event_field =
  Cid.create_ids
    (Cid.to_ids dpt_meta_struct_instance @ [Id.create next_event_str])
;;


let handle_selector_name =
  Cid.create [md_instance_prefix; dpt_meta_str; handle_selector_str]
;;

let event_counts_field = 
  Cid.create [md_instance_prefix; dpt_meta_str; events_count_str]
;;
let event_counts_width = 16
;;
(* event metadata fields *)
let event_id_field = Cid.create ["eventType"]
let event_id_width = 8
let event_mc_field = Cid.create ["eventMc"]
let event_mc_width = 8
let event_loc_field = Cid.create ["eventLoc"]
let event_loc_width = 32
let event_delay_field = Cid.create ["eventDelay"]
let event_delay_width = 32
let lucid_parser_name = Cid.create ["DptIngressParser"]
let event_generate_cid = Cid.create ["generate"]
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
let event_out_flags_instance = Cid.create_ids [hdr_instance;  Id.create "ev_out_flags"]
;;
(* The lucid footer that goes after all the event headers. *)
let footer_t = Cid.create ["lucid_footer_t"]
let footer = Cid.create ["lucid_footer"]
let footer_fields = [(Cid.create ["end"], 8)]
let footer_struct = GS.hdr_struct footer_t (CL.split footer_fields)
let footer_instance = GS.struct_inst footer_struct footer 
let footer_instance_cid = Cid.concat (Cid.create_ids [hdr_instance]) footer

let max_generated_events = 4


