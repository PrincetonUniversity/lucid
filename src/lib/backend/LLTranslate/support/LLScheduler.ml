(* Generate Lucid scheduler objects for the Tofino.
   Part of the source to IR layer -- generates nativeblocks
   (native P4 blocks wrapped in IR syntax tree nodes) *)

(* TODO: 11/21 -- don't assume ethertype is IP, hold it in the footer. *)

open MiscUtils
open CoreSyntax
open LLSyntax
open InterpHelpers
open LLContext
open LLOp
open LLConstants
open Format
open P4tPrint
open P4ExternSyntax
module P4S = P4ExternSyntax

exception Error of string

(* utils *)
let rec nth_pos pos x lst =
  match lst with
  | [] -> error "[find_first] could not find element in list."
  | hd :: lst -> if hd = x then pos else nth_pos (pos + 1) x lst
;;

let first_pos = nth_pos 0

let last_pos x lst =
  let from_end = CL.rev lst |> first_pos x in
  CL.length lst - 1 - from_end
;;

let struct_name evid =
  P4tPrint.str_of_varid (TofinoStructs.in_struct_from_ev evid)
;;

(* header instance name *)
let instance_name evid =
  (* P4tPrint.str_of_varid (TofinoStructs.full_in_struct_from_ev evid EBackground) *)
  ignore evid;
  failwith "TODO:HEADERS"
;;

let undeclared_instance_name = P4tPrint.str_of_public_varid

(** commands  (just strings of P4) **)
let etype_field = "hdr.ethernet.ether_type"

let cmd_lucid_etype = sprintf "%s = %#06x;" etype_field lucid_etype
let cmd_ip_etype = sprintf "%s = %#06x;" etype_field ip_etype
let cmd_enable_hdr event_id = sprintf "%s.setValid();" (instance_name event_id)
let cmd_enable_hdrs ev_ids = CL.map cmd_enable_hdr ev_ids
let cmd_disable_hdr hdr_id = sprintf "%s.setInvalid();" (instance_name hdr_id)

let cmd_disable_cid hdr_cid =
  sprintf "%s.setInvalid();" (undeclared_instance_name hdr_cid)
;;

let cmds_disable_cids hdr_cids = CL.map cmd_disable_cid hdr_cids
let cmds_disable_hdrs ev_ids = CL.map cmd_disable_hdr ev_ids

let cmd_disable_other_hdrs ev_ids ev_id =
  list_remove ev_ids ev_id |> cmds_disable_hdrs
;;

let cmd_exit = "exit;"
let cmd_drop = "ig_dprsr_md.drop_ctl = 0x1;"

let cmd_copy_to_recirc =
  match !LLConfig.ll_opts.use_multicast with
  | true ->
    sprintf
      "ig_tm_md.mcast_grp_a = %i + md.dptMeta.eventsCount;"
      (lucid_mc_group - 1)
  | false ->
    (* forward background events instead of using multicast. *)
    sprintf "ig_tm_md.ucast_egress_port = 196;"
;;

(* the ingress exit table applies after the handler tables. *)
module IngressExit = struct
  (***
    Ingress exit table.

    As a Lucid event handler executes, it annotates a packet
    with metadata containing the names and arguments of
    other events that need to be generated.
    After the event handler finishes, the packet is effectively
    a "multi-event" packet -- it stores data belonging to
    multiple events that need to be executed somewhere at
    some time in the future.
    The ingress exit table sets up the multi-event packet
    so that:
      1) all packets with wire events continue in the egress pipeline.
      2) packets with non-wire events are cloned to egress the appropriate number of times.
      3) (temporary) packets with only non-wire events have ether_type = Lucid
      4) (temporary) packets with wire events have ether_type = IP

    Notes: (3) should be done by the egress table;
           (4) should get ether_type from a hidden event parameter that stores
               the ether_type of the original packet.

    Below is a quick summary of the actions that this table uses,
    and the cases that it matches on.
    new table:
        actions:
          entry_hdl_bg_continue() {
            mc_group = 1065 + eventCount;
            ethertype = LUCID;
          }
          entry_hdl_bg_no_continue() {
            mc_group = 1065 + eventCount;
            ethertype = LUCID;
            exit;
          }

          bg_hdl_recurse_continue() {
            mc_group = 1065 + eventCount;
            ethertype = LUCID;
          }
          bg_hdl_recurse_continue() {
            mc_group = 1065 + eventCount;
            ethertype = LUCID;
            exit;
          }
          for each bg event foo:
            bg_hdl_foo_no_recurse_continue() {
              hdr.foo.setInvalid();
              mc_group = 1065 + eventCount;
              ethertype = LUCID;
            }
            bg_hdl_foo_no_recurse_no_continue() {
              hdr.foo.setInvalid();
              mc_group = 1065 + eventCount;
              ethertype = LUCID;
              exit;
            }
          no_bg_continue() {
              // invalidate all event headers.
              ethertype = IP;
          }
          no_events() {
              // exit
          }
        keys:
          eventType (hdl id), <event_generated flags>, eventCount, exitEventType

        cases:
        (bg hdl, bg recursive event, no continue event) --> bg_hdl_recurse_continue();
              [eventType = foo; event_generated_foo = 1; event_count = *; exitEventType = 0]
              --> bg_hdl_recurse_no_continue();
        (bg hdl, bg recursive event, continue event):
              [eventType = foo; event_generated_foo = 1; event_count = *; exitEventType = *]
              --> bg_hdl_recurse_continue();
        (bg hdl, bg non-recursive event, no continue event)
              [eventType = foo; event_generated_foo = 0; event_count = *; exitEventType = 0]
              -->
              bg_hdl_foo_no_recurse_no_continue();
        (bg hdl, bg non-recursive event, continue event)
              [eventType = foo; event_generated_foo = 0; event_count = *; exitEventType = *]
              -->
              bg_hdl_foo_no_recurse_continue();
        (bg hdl, bg non-recursive event, no continue event) --> bg_hdl_foo_no_recurse_no_continue(); // PER-EVENT FOO.
        (entry hdl, bg event, continue event) --> entry_hdl_bg_continue();
        (entry hdl, bg event, no continue event) --> entry_hdl_bg_no_continue();
        (bg hdl, no bg event, continue event) --> no_bg_continue();
        (bg hdl, no bg event, no continue event) --> no_bg_no_continue();
        (entry hdl, no bg event, continue event) --> no_bg_continue();
        (entry hdl, no bg event, no continue event) --> no_bg_no_continue();
  ***)

  let generate_lucid_exit_table current_event_cid event_count_cid exit_event_cid
    =
    let bg_evrecs =
      CL.filter
        (fun evrec ->
          match evrec.event_sort with
          (* | Syntax.EBackground -> true *)
          (* TODO:HEADERS *)
          | _ -> false)
        (ctx_get_event_recs ())
    in
    let bg_evcids = CL.map (fun f -> f.event_id) bg_evrecs in
    let lucid_sys_hdr_cids = [footer_instance_cid; event_out_flags_instance] in
    (* create actions. *)
    let entry_hdl_bg_continue =
      { aname = "entry_hdl_bg_continue"
      ; aparams = []
      ; acmds = [cmd_copy_to_recirc; cmd_lucid_etype]
      }
    in
    let entry_hdl_bg_no_continue =
      { aname = "entry_hdl_bg_no_continue"
      ; aparams = []
      ; acmds = [cmd_copy_to_recirc; cmd_lucid_etype; cmd_exit]
      }
    in
    let bg_hdl_recurse_continue =
      { aname = "entry_hdl_recurse_continue"
      ; aparams = []
      ; acmds = [cmd_copy_to_recirc; cmd_lucid_etype]
      }
    in
    (* same as coming in from an entry handle *)
    let bg_hdl_recurse_no_continue =
      { aname = "bg_hdl_recurse_no_continue"
      ; aparams = []
      ; acmds = [cmd_copy_to_recirc; cmd_lucid_etype; cmd_exit]
      }
    in
    let no_bg_continue =
      { aname = "no_bg_continue"
      ; aparams = []
      ; acmds =
          cmd_ip_etype
          :: (cmds_disable_hdrs bg_evcids @ cmds_disable_cids lucid_sys_hdr_cids)
      }
    in
    (* the only time we drop a packet is when there is both no continue event and also no
       background event.
        NO. When there is no continue event, but a background event, we want to
        disable _unicast_. Setting port to 0 doesn't work in the asic model because
        that's an actual port!
     *)
    let no_events =
      { aname = "no_events"; aparams = []; acmds = [cmd_drop; cmd_exit] }
    in
    (* per-background event generators *)
    let bg_hdl_no_recurse_no_continue event_id =
      (* the handler for erec generated a non recursive background event and NO continue. *)
      { aname = "bg_hdl_" ^ Id.name event_id ^ "_no_recurse_no_continue"
      ; aparams = []
      ; acmds =
          [ cmd_disable_hdr event_id
          ; cmd_copy_to_recirc
          ; cmd_lucid_etype
          ; cmd_exit ]
      }
    in
    let bg_hdl_no_recurse_continue event_id =
      (* the handler for erec generated a non recursive background event and a continue. *)
      { aname = "bg_hdl_" ^ Id.name event_id ^ "_no_recurse_continue"
      ; aparams = []
      ; acmds = [cmd_disable_hdr event_id; cmd_copy_to_recirc; cmd_lucid_etype]
      }
    in
    (* create rules. note: the rules are ordered! *)
    let eventType_str = str_of_public_varid current_event_cid in
    let eventCount_str = str_of_public_varid event_count_cid in
    let exitEvent_str = str_of_public_varid exit_event_cid in
    let no_events_rule =
      { guard =
          [ { field = eventType_str; value = VAny }
          ; { field = eventCount_str; value = VInt 0 }
          ; { field = exitEvent_str; value = VInt 0 } ]
      ; action = no_events
      ; action_args = []
      }
    in
    let no_bg_continue_rule =
      { guard =
          [ { field = eventType_str; value = VAny }
          ; { field = eventCount_str; value = VInt 0 }
          ; { field = exitEvent_str; value = VAny } ]
      ; action = no_bg_continue
      ; action_args = []
      }
    in
    (* create rules for each background event. *)
    let create_bg_event_rules erec =
      let recurse_no_continue_rule =
        { guard =
            [ { field = eventType_str; value = VInt erec.event_iid }
            ; { field = str_of_public_varid erec.event_generated_flag
              ; value = VInt 1
              }
            ; { field = eventCount_str; value = VAny }
            ; { field = exitEvent_str; value = VInt 0 } ]
        ; action = bg_hdl_recurse_no_continue
        ; action_args = []
        }
      in
      let recurse_continue_rule =
        { guard =
            [ { field = eventType_str; value = VInt erec.event_iid }
            ; { field = str_of_public_varid erec.event_generated_flag
              ; value = VInt 1
              }
            ; { field = eventCount_str; value = VAny }
            ; { field = exitEvent_str; value = VAny } ]
        ; action = bg_hdl_recurse_continue
        ; action_args = []
        }
      in
      let no_recurse_no_continue_rule =
        { guard =
            [ { field = eventType_str; value = VInt erec.event_iid }
            ; { field = str_of_public_varid erec.event_generated_flag
              ; value = VInt 0
              }
            ; { field = eventCount_str; value = VAny }
            ; { field = exitEvent_str; value = VInt 0 } ]
        ; action = bg_hdl_no_recurse_no_continue erec.event_id
        ; action_args = []
        }
      in
      let no_recurse_continue_rule =
        { guard =
            [ { field = eventType_str; value = VInt erec.event_iid }
            ; { field = str_of_public_varid erec.event_generated_flag
              ; value = VInt 0
              }
            ; { field = eventCount_str; value = VAny }
            ; { field = exitEvent_str; value = VAny } ]
        ; action = bg_hdl_no_recurse_continue erec.event_id
        ; action_args = []
        }
      in
      [ recurse_no_continue_rule
      ; recurse_continue_rule
      ; no_recurse_no_continue_rule
      ; no_recurse_continue_rule ]
    in
    let bg_event_rules =
      CL.filter_map
        (fun erec ->
          match erec.event_sort with
          (* | EBackground -> Some (create_bg_event_rules erec) *)
          (*TODO:HEADERS*)
          | _ ->
            ignore create_bg_event_rules;
            None)
        (ctx_get_event_recs ())
      |> CL.flatten
    in
    (* create the entry event rules. *)
    let entry_hdl_bg_no_continue_rule =
      { guard =
          [ { field = eventType_str; value = VAny }
          ; { field = eventCount_str; value = VAny }
          ; { field = exitEvent_str; value = VInt 0 } ]
      ; action = entry_hdl_bg_no_continue
      ; action_args = []
      }
    in
    let entry_hdl_bg_continue_rule =
      { guard =
          [ { field = eventType_str; value = VAny }
          ; { field = eventCount_str; value = VAny }
          ; { field = exitEvent_str; value = VAny } ]
      ; action = entry_hdl_bg_continue
      ; action_args = []
      }
    in
    let rules =
      [no_events_rule; no_bg_continue_rule]
      @ bg_event_rules
      @ [entry_hdl_bg_no_continue_rule; entry_hdl_bg_continue_rule]
    in
    let tbl_str = "lucid_return_table" in
    let tbl_cid = Cid.create [tbl_str] in
    (* put everything together: assemble the table! *)
    let table = assemble_table tbl_str rules in
    (* convert the table into a native
       object that goes at the end of ingress. *)
    nativeblock_of_tbl LIgrEnd tbl_cid table
  ;;
end

module Egress = struct
  (*   Egress deserialization table generation.

    The egress table converts a packet that holds many events
    into a packet that holds one event. This is necessary because
    every packet that leaves ingress must be the same. So, to
    generate multiple events, we have to generate multiple identical
    packets that hold the data for _all_ generated events, and then
    transform the multi-event packet into a single event packet here,
    in egress.
    The egress table matches on rid (replica id) and a bitvector
    that has a flag for each non-wire event that the multi-event packet
    contains. The egress table's action list has one action for each non-wire event,
    and 1 action for wire events.
    The rules for rid = x filter out all but the x'th
    _generated_ event. Note that, this is not the same as the
    x'th event, as not all events may have been generated.
    The pattern of a rule for rid = x has exactly x elements
    in the bit vector set to 1. The rule's action is action_list[last_ones_idx],
    where last_ones_idx is the index of the last 1 in the bit vector.

    There are approximately ((N^3)/6 - N/6)) rules in this table, where
    N is the number of non-wire events. It is not exponential because
    the table uses TCAM wildcards so that we don't need a rule for
    every possible combination of event generations.
 *)

  let mcid_field = Cid.create ["eg_intr_md"; "egress_rid"]

  let rec zeros len =
    match len with
    | 0 -> []
    | 1 -> [0]
    | _ -> 0 :: zeros (len - 1)
  ;;

  let rec bv_ones len =
    match len with
    | 0 -> []
    | 1 -> [1]
    | _ -> 1 :: bv_ones (len - 1)
  ;;

  let replace l pos a = List.mapi (fun i x -> if i = pos then a else x) l
  let single_one len pos = replace (zeros len) pos 1
  let str_of_bitvec bv = CL.map string_of_int bv |> String.concat ";"

  let print_bitvecs bvs =
    print_endline "--- bitvec set ---";
    CL.iter (fun bv -> print_endline (str_of_bitvec bv)) bvs;
    print_endline "--- bitvec set ---"
  ;;

  (* generate all the bitvectors of length len that have ones elements set to 1 *)
  let rec gen_bv len ones =
    match len = ones with
    | true -> [bv_ones len]
    | false ->
      (match ones with
      | 0 -> [zeros len]
      | _ ->
        let suffixes_with_one = gen_bv (len - 1) (ones - 1) in
        let suffixes_with_zero = gen_bv (len - 1) ones in
        let prepend v suffix = v :: suffix in
        let result_with_one = CL.map (prepend 1) suffixes_with_one in
        let result_with_zero = CL.map (prepend 0) suffixes_with_zero in
        result_with_one @ result_with_zero)
  ;;

  (* generate all the bitvectors of length len that have _up to_ ones elements set to 1 *)
  let rec gen_bv_up_to len ones =
    match ones with
    | 0 | 1 -> gen_bv len ones
    | _ -> gen_bv len ones @ gen_bv_up_to len (ones - 1)
  ;;

  (* convert a bitvector to a value vector from P4S syntax *)
  let rec bv_valv bv =
    match bv with
    | [] -> []
    | [b] -> [P4S.VInt b]
    | b :: bv -> VInt b :: bv_valv bv
  ;;

  let bvs_valvs = CL.map bv_valv

  (* wildcard prefix and suffix zeros *)
  let rec wildcard_prefix_zeros valv =
    match valv with
    | P4S.VInt 0 :: valv -> VAny :: wildcard_prefix_zeros valv
    | valv -> valv
  ;;

  (* anything besides a 0 breaks the conversion. *)

  let wildcard_suffix_zeros valv =
    valv |> CL.rev |> wildcard_prefix_zeros |> CL.rev
  ;;

  let wildcard_suffix_zeros_list = CL.map wildcard_suffix_zeros

  (* convert a value vector and a field into a guard *)
  let combine_fields_vals fields vals =
    CL.combine vals fields |> CL.map (fun (v, f) -> { field = f; value = v })
  ;;

  type egr_acns = (action * action) * (id * action) list

  let egress_demultiplex_actions erecs footer_cid : egr_acns =
    (* generate the commands inside of the actions. *)
    let bg_erecs =
      erecs
      |> CL.filter_map (fun erec ->
             match erec.event_sort with
             (* | Syntax.EBackground -> Some erec *)
             (*TODO:HEADERS*)
             | _ -> None)
    in
    let event_to_flag =
      bg_erecs |> CL.map (fun erec -> erec.event_id, erec.event_generated_flag)
    in
    let cmds_reset_flags =
      event_to_flag
      |> (* flags are header fields. *)
      CL.map (fun (_, flg) -> cmd_set (str_of_public_varid flg) 0)
    in
    let cmds_reset = cmds_reset_flags in
    (* str_of_public_varid *)
    (* instance_name *)
    let cmds_invalidate_all_but_one one =
      bg_erecs
      |> CL.map (fun erec ->
             match erec.event_id = one with
             | true -> cmd_valid (instance_name one)
             | false -> cmd_invalid (instance_name erec.event_id))
    in
    (* invalidate every lucid header*)
    let cmds_invalidate_all bg_erecs =
      cmd_invalid (undeclared_instance_name footer_cid)
      :: cmd_invalid (undeclared_instance_name event_out_flags_instance)
      :: (bg_erecs
         |> CL.map (fun erec -> cmd_invalid (instance_name erec.event_id)))
    in
    (* serialize the wire event, disable all others. *)
    (* not a lucid packet, do nothing.*)
    let acn_nonlucid_wire =
      { aname = "acn_nonlucid_wire"; aparams = []; acmds = [] }
    in
    (* the wire copy of a lucid multi-event packet. *)
    let acn_lucid_wire =
      { aname = "acn_lucid_wire"
      ; aparams = []
      ; acmds = cmd_ip_etype :: cmds_invalidate_all bg_erecs
      }
    in
    (* serialize one action event, disable all others. *)
    let acn_event ev_id =
      ( ev_id
      , { aname = "acn_bgev_" ^ Id.name ev_id
        ; aparams = []
        ; acmds = cmds_reset @ cmds_invalidate_all_but_one ev_id
        } )
    in
    let acn_event_map = event_to_flag |> CL.split |> fst |> CL.map acn_event in
    (acn_nonlucid_wire, acn_lucid_wire), acn_event_map
  ;;

  (* generate all the rules for a given multicast id *)
  let mcid_rules ev_flags ev_actions mcid =
    (* generate the bitvectors relevant to this mcid. *)
    (* each bitvector has up to mcid ones set.*)
    let bitvecs = gen_bv (CL.length ev_flags) mcid in
    (* craft a single rule for mcid from a bitvec. *)
    let craft_rule actions mcid bitvec =
      (* HACK: ignore mcid *)
      let ev_flag_values = bitvec |> bv_valv |> wildcard_suffix_zeros in
      let flag_guard = combine_fields_vals ev_flags ev_flag_values in
      let guard =
        match !LLConfig.ll_opts.use_multicast with
        | true ->
          { field = etype_field; value = VInt lucid_etype }
          :: { field = str_of_public_varid mcid_field; value = VInt mcid }
          :: flag_guard
        | false ->
          { field = etype_field; value = VInt lucid_etype }
          :: { field = str_of_public_varid mcid_field; value = VInt 0 }
          :: flag_guard
      in
      let res =
        { guard
        ; (* the last position in the bitvector that holds a 1. *)
          action = CL.nth actions (last_pos 1 bitvec)
        ; action_args = []
        }
      in
      res
    in
    CL.map (craft_rule ev_actions mcid) bitvecs
  ;;

  (* generate the rules for the egress table. *)
  (* Use the bitvec generators to create a list
     of rules for each replica ID value from 0 : len(bg_events)
     The action of a rule is the index of the last bitvector element
     set to 1... *)
  let egress_demultiplex_table footer_cid =
    let erecs = ctx_get_event_recs () in
    (* get the flags *)
    let ev_flag_cids =
      ctx_get_event_recs ()
      |> CL.filter (fun erec ->
             match erec.event_sort with
             (* | Syntax.EBackground -> true *)
             (* TODO:HEADERS *)
             | _ -> false)
      |> CL.map (fun erec -> str_of_public_varid erec.event_generated_flag)
    in
    (* get the actions *)
    let (acn_nonlucid_wire, acn_lucid_wire), acn_event_map =
      egress_demultiplex_actions erecs footer_cid
    in
    let bg_event_acns = CL.map snd acn_event_map in
    (* one action for each bg event *)
    let bg_ev_ids = CL.split acn_event_map |> fst in
    (* create the rules for every mcid value that indicates a background event. *)
    let mcids = range 1 (1 + CL.length bg_ev_ids) in
    let mcid_rule_lists =
      CL.map (mcid_rules ev_flag_cids bg_event_acns) mcids
    in
    (* create a rule for mcid = 0, which is the wire event. *)
    (* ethernet packet, non-lucid multicast replica id --> do nothing *)
    let non_lucid_pkt_rule =
      { guard =
          [ { field = etype_field; value = VInt ip_etype }
          ; { field = str_of_public_varid mcid_field; value = VInt 0 } ]
      ; action = acn_nonlucid_wire
      ; action_args = []
      }
    in
    let lucid_wire_pkt_rule =
      { guard =
          [ { field = etype_field; value = VInt lucid_etype }
          ; { field = str_of_public_varid mcid_field; value = VInt 0 } ]
      ; action = acn_lucid_wire
      ; action_args = []
      }
    in
    let all_rules =
      match !LLConfig.ll_opts.use_multicast with
      (* its unclear why we change the order when multicast is disabled *)
      | true ->
        non_lucid_pkt_rule :: lucid_wire_pkt_rule :: CL.flatten mcid_rule_lists
      | false ->
        (non_lucid_pkt_rule :: CL.flatten mcid_rule_lists)
        @ [lucid_wire_pkt_rule]
    in
    (* create the table from rules. *)
    let tbl_str = "egr_serialize_clone" in
    let tbl_cid = Cid.create [tbl_str] in
    let tbl = assemble_table tbl_str all_rules in
    nativeblock_of_tbl LEgr tbl_cid tbl
  ;;
end

let generate _ =
  let ingress_exit_tbl =
    IngressExit.generate_lucid_exit_table
      current_event_field
      event_count_field
      exit_event_field
  in
  let egress_demultiplex_tbl =
    Egress.egress_demultiplex_table footer_instance_cid
  in
  [ingress_exit_tbl; egress_demultiplex_tbl]
;;
