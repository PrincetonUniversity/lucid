(* Configuration constants used throughout the backend *)



let progId = Cid.create ["DptEngine"]
let branchTbl = Cid.Id ("selectEventType", 0)
let branchArg_name = "eventType"
let branchArg_id = (branchArg_name, 0)
let branchArg = Cid.Id branchArg_id

let branchArgWidth = 8
let nextEventArg = Cid.Id ("nextEventType", 0)
let exitEventFldName = "exitEventType"
let exitEventArg = Cid.Id (exitEventFldName, 0)
let mergedDpName = Cid.Id ("apply", 0)
(* let continueName = "doContinue" *)
(* let continueArg = Cid.Id (continueName, 0) *)
let tsArg = Cid.Id ("timestamp", 0)

(* struct that contains the ts arg. *)
(* When support for struct args is added, 
this will be a different struct *)
let tsStruct = Cid.Id ("dpaIn", 0)
let tsArgQualified = Cid.create (Cid.names tsStruct @ Cid.names tsArg)
let globalArgs = [branchArg; nextEventArg; exitEventArg; tsArg]
let globalArgWidths = [branchArgWidth; 8; 8; 32]

(* global names used in DpaToP4 and P4Print *)
let dpaHdrType = "dpt_event_h" (* output struct type. *)

let dpaBridgeHdrType = "dpt_bridge_h"
let dpaHdrName = "dpaArgs" (* input struct name *)

let dpaOutName = "dpaArgs" (* output struct name *)


(* new (7/30/20) dpt headers that separate recirculated from non recirculated fields.*)
let dpa_global_t = "dpt_meta_h"
let dpa_global_raw_name = "dptMeta"
let dpa_global_name = "md."^dpa_global_raw_name
let dpa_local_t = "dpt_args_h"
let dpa_local_name = "md.dptArgs"

(* let packetInThreadName = "packetin"
	let packetinHandleId = Id.create packetInThreadName
	let continueEventId = Cid.Id ("continue", 0)
	let continueHandleId = Id.create "continue"
	let builtinEventIds = [continueEventId]
 *)




let p4_md_name = "md"
let p4_hdr_name = "hdr"

(* default width for parts of the P4 printer that don't use polymorphic widths yet. *)
let defWidth = 32


let boolWidth = 8 (* adjust to 1 eventually. Need ot make sure it doesn't mess up salu logic. *)

(* event delay and location field names *)
let delayField = Id.create "eventDelay"
let locField = Id.create "eventLoc"
let eventMetaWidth = 16


(* DPT-generated code block names *)
let dpt_igr_objs = "DPT_OBJECTS"
let dpt_igr_call = "DPT_DISPATCH"
let dpt_hdr_decls = "DPT_HEADERS"
let dpt_hdr_inst = "DPT_HEADER_INSTANCES"
let dpt_meta_inst = "DPT_METADATA_INSTANCES"
let dpt_parser    = "DPT_PARSER"
let dpt_egr_call = "DPT_EGRESS"
let dpt_egr_objs = "DPT_EGRESS_OBJECTS"


(* output P4 file name. *)
let p4out_prefix = "DPT_"


(* Tofino switch configuration *)

let multicast_grp_id = 1066
let dpt_etype_name = "DPT_ETYPE"
let dpt_etype_id = Cid.create [dpt_etype_name]
let dpt_etype = 0x6666
let dpt_recirc_port_name = "DPT_RECIRC_PORT"
let dpt_recirc_port_id = Cid.create [dpt_recirc_port_name]
let dpt_recirc_port = 196
;;
(* #define DPT_RECIRC_PORT 1
#define DPT_ETYPE 0x6666
 *)

