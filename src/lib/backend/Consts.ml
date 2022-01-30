(* Configuration constants used throughout the backend *)

let progId = Cid.create ["DptEngine"]
let mergedDpName = Cid.Id ("apply", 0)
let branchTbl = Cid.Id ("selectEventType", 0)

(* lucid-generate arguments *)
let branchArg = Cid.Id ("eventType", 0), 8
let nextEventArg = Cid.Id ("nextEventType", 0), 8
let exitEventArg = Cid.Id ("exitEventType", 0), 8
let tsArg = Cid.Id ("timestamp", 0), 32

let globalArgs, globalArgWidths =
  Caml.List.split [branchArg; nextEventArg; exitEventArg; tsArg]
;;

let dpa_global_raw_name = "dptMeta"
let dpa_global_name = "md." ^ dpa_global_raw_name

(* default width for parts of the P4 printer that don't use polymorphic widths yet. *)
let defWidth = 32
let boolWidth = 8
(* adjust boolWidth to 1 if possible with sALUs *)

(* DPT-generated code block names *)
let dpt_igr_objs = "DPT_OBJECTS"
let dpt_igr_call = "DPT_DISPATCH"
let dpt_igr_raw_call = "DPT_HANDLERS"
let dpt_hdr_decls = "DPT_HEADERS"
let dpt_hdr_inst = "DPT_HEADER_INSTANCES"
let dpt_meta_inst = "DPT_METADATA_INSTANCES"
let dpt_parser = "DPT_PARSER"
let dpt_egr_call = "DPT_EGRESS"
let dpt_egr_objs = "DPT_EGRESS_OBJECTS"

(* output P4 file name. *)
let p4out_prefix = "DPT_"

(* output directories. was in ioUtils.ml *)
let outDir = ref "./LucidCompileLogs"
let srcDir = ref (!outDir ^ "/src")
let logDir = ref (!outDir ^ "/logs")
let scriptsDir = ref (!outDir ^ "/scripts")

(* Tofino switch configuration *)
(* depreciated -- 7/21 *)
(* let multicast_grp_id = 1066 *)
(* let dpt_etype_name = "DPT_ETYPE" *)
(* let dpt_etype_id = Cid.create [dpt_etype_name] *)
(* let dpt_etype = 0x6666 *)
(* let dpt_recirc_port_name = "DPT_RECIRC_PORT" *)
(* let dpt_recirc_port_id = Cid.create [dpt_recirc_port_name] *)
(* let dpt_recirc_port = 196 *)
(* #define DPT_RECIRC_PORT 1
#define DPT_ETYPE 0x6666 *)
