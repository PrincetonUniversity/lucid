(* Generate Lucid scheduler objects for the Tofino. 
   Part of the source to IR layer -- generates nativeblocks 
   (native P4 blocks wrapped in IR syntax tree nodes) *)
open MiscUtils
open Syntax
open LLSyntax
open InterpHelpers
open LLContext
open LLOp
open LLConstants
open Format
open P4tPrint

(* get the struct id of each event *)
(* utils *)
let struct_name evid =
  P4tPrint.str_of_varid (TofinoStructs.in_struct_from_ev evid)
;;

let instance_name evid =
  P4tPrint.str_of_varid (TofinoStructs.full_in_struct_from_ev evid EBackground)
;;

(* Read the event's id from context as hex. *)
let hex_iid evid =
  let ev_iid = sprintf "%#x" (ctx_find_eventrec (Cid.id evid)).event_iid in
  print_endline ("[hex_iid]: " ^ Id.to_string evid ^ " -- " ^ ev_iid);
  ev_iid
;;

let ids_of_bg_events ds =
  let map_f dec =
    match dec.d with
    | DEvent (evid, EBackground, _, _) -> Some evid
    | _ -> None
  in
  CL.filter_map map_f ds
;;

(* simple partial syntax tree for hand-written P4 blocks. *)
type cmd = string

type acn =
  { aname : string
  ; acmds : cmd list
  ; arule : string
  }

type tbl =
  { tid : Cid.t
  ; tacns : acn list
  ; tkeys : string list
  }

(* table has an id for integration into the syntax tree *)

let cmd_lucid_etype = sprintf "hdr.ethernet.ether_type = %#06x;" lucid_etype
let cmd_ip_etype = sprintf "hdr.ethernet.ether_type = %#06x;" ip_etype

let cmd_disable_hdr event_id =
  sprintf "%s.setInvalid();" (instance_name event_id)
;;

let cmds_disable_hdrs ev_ids = CL.map cmd_disable_hdr ev_ids

let acn_str acn =
  sprintf "action %s() {\n\t%s\n\t}" acn.aname (String.concat "\n\t" acn.acmds)
;;

(* the ingress exit table applies after the handler tables. *)
module IngressExit = struct
  (*** commands constructors ***)
  let cmd_exit = "exit;"
  let cmd_drop = "ig_dprsr_md.drop_ctl = 0x1;"
  let cmd_copy_to_recirc = sprintf "ig_tm_md.mcast_grp_a = %i;" lucid_mc_group

  (* let cmd_lucid_recirc = sprintf "ig_tm_md.ucast_egress_port = %i;" lucid_recirc_port;; *)

  let cmd_enable_hdr event_id =
    sprintf "%s.setValid();" (instance_name event_id)
  ;;

  let cmd_enable_hdrs ev_ids = CL.map cmd_enable_hdr ev_ids

  let cmd_disable_other_hdrs ev_ids ev_id =
    list_remove ev_ids ev_id |> cmds_disable_hdrs
  ;;

  (*** action constructors ***)

  (* RET: not generated, BG: not generated *)
  (* stop (no event was generated) *)
  let acn_stop bg_ev_ids =
    let _ = bg_ev_ids in
    { aname = "acn_stop"
    ; acmds = [cmd_drop; cmd_exit]
    ; arule = "(0x0, 0x0) : acn_stop();"
    }
  ;;

  (* RET: generated, BG: not generated *)
  (* return (ONLY an exit event was generated) *)
  (* we set the ethertype to IP because the packet 
     might be an event packet at this point *)
  let acn_return bg_ev_ids =
    { aname = "acn_return"
    ; acmds = cmd_ip_etype :: cmds_disable_hdrs bg_ev_ids
    ; arule = "(_, 0x0) : acn_return();"
    }
  ;;

  (* RET: not generated, BG: generated *)
  (* note that in this case, we don't drop the packet -- it must 
     get to the multicast engine. But we _do_ exit the ingress 
     pipeline. *)
  (* There may be a bug in my logic here -- we want to make sure 
     that the original packet actually gets dropped in egress. *)
  let acn_bg bg_ev_ids ev_id =
    let acn_name = "acn_bg_" ^ struct_name ev_id in
    { aname = acn_name
    ; acmds =
        (cmd_copy_to_recirc
        :: cmd_lucid_etype
        :: cmd_disable_other_hdrs bg_ev_ids ev_id)
        @ [cmd_exit]
        (* exit must be last! *)
    ; arule = "(0x0, " ^ hex_iid ev_id ^ ") :" ^ acn_name ^ "();"
    }
  ;;

  (* RET: generated, BG: generated *)
  let acn_bg_ret bg_ev_ids ev_id =
    let acn_name = "acn_bg_ret" ^ struct_name ev_id in
    { aname = acn_name
    ; acmds =
        cmd_copy_to_recirc
        :: cmd_lucid_etype
        :: cmd_disable_other_hdrs bg_ev_ids ev_id
    ; arule = "(_, " ^ hex_iid ev_id ^ ") :" ^ acn_name ^ "();"
    }
  ;;

  (*** table constructor ***)

  (* all the actions for the emit table *)
  let acns_emit bg_ev_ids =
    (* note that order matters! *)
    (acn_stop bg_ev_ids
    :: acn_return bg_ev_ids
    :: CL.map (acn_bg bg_ev_ids) bg_ev_ids)
    @ CL.map (acn_bg_ret bg_ev_ids) bg_ev_ids
  ;;

  let tbl_emit bg_ev_ids =
    { tid = Cid.create ["lucid_return_table"]
    ; tacns = acns_emit bg_ev_ids
    ; tkeys =
        [ str_of_varid exit_event_field ^ " : ternary;"
        ; str_of_varid next_event_field ^ " : ternary;" ]
    }
  ;;

  (*** printers ***)
  let tbl_acns_str tbl = CL.map acn_str tbl.tacns |> String.concat "\n"

  let tbl_body_str (tbl : tbl) =
    let keys = String.concat " " tbl.tkeys in
    let acns =
      CL.map (fun acn -> acn.aname ^ ";") tbl.tacns |> String.concat " "
    in
    let entries =
      CL.map (fun acn -> acn.arule) tbl.tacns |> String.concat " "
    in
    sprintf
      "key = {%s}\n\tactions = {%s}\n\tconst entries = {%s}"
      keys
      acns
      entries
  ;;

  (*** table to native block in IR ***)
  let nativeblock_of_tbl tbl =
    (* construct the printer function *)
    let print_fcn tbl tbl_id =
      let tbl_name = str_of_private_oid tbl_id in
      let acn_str = tbl_acns_str tbl in
      let tbl_body = tbl_body_str tbl in
      [%string "$acn_str\ntable $tbl_name {\n\t$tbl_body\n}"]
    in
    new_native_block tbl.tid LIgrEnd (print_fcn tbl)
  ;;

  (*** construct the emit table native block ***)
  let make_emit_nativeblock ds =
    let bg_ev_ids = ids_of_bg_events ds in
    let emit_tbl = tbl_emit bg_ev_ids in
    nativeblock_of_tbl emit_tbl
  ;;
end

module Egress = struct
  (* the block in egress that does transformations to cloned packets. *)
  let make_acn_egr_drop_event bg_ev_ids =
    { aname = "egr_disable_events"
    ; acmds = cmd_ip_etype :: cmds_disable_hdrs bg_ev_ids
    ; arule = "(0x0, 0x0) : acn_stop();"
    }
  ;;

  let make_egress_nativeblock ds =
    let bg_ev_ids = ids_of_bg_events ds in
    let acn_egr_drop_event = make_acn_egr_drop_event bg_ev_ids in
    let egr_drop_name = acn_egr_drop_event.aname in
    let egr_drop_decl = acn_str acn_egr_drop_event in
    let tbl_name = "egr_serialize_clone" in
    let print_fcn tbl_id =
      let lucid_etype_str = string_of_int lucid_etype in
      let tbl_name = str_of_private_oid tbl_id in
      [%string
        {__nativeblock___|
        $egr_drop_decl
        action egr_noop() { }
        table $tbl_name {
          key = {hdr.ethernet.ether_type : ternary; eg_intr_md.egress_rid : ternary;}
          actions = {$egr_drop_name; egr_noop;}
          const entries = {
            ($lucid_etype_str, 0) : $egr_drop_name();
            ($lucid_etype_str, _) : egr_noop();
            (_, _)      : egr_noop();  
          }
        }
        |__nativeblock___}]
    in
    (* can't use the constructors for the ingress emit block because they require 
    a 1:1 mapping from action names to rules. Might want to make that more general. *)
    new_native_block (Cid.create [tbl_name]) LEgr print_fcn
  ;;
end

let generate ds =
  [IngressExit.make_emit_nativeblock ds; Egress.make_egress_nativeblock ds]
;;
