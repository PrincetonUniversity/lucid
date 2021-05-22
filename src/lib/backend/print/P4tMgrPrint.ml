(* print controller configuration program / script. *)
open Printf
open InstrSyntax
open InstrSeqSyntax

let print_mids mids = 
  CL.iter (fun m -> print_endline ("mid: "^(Cid.to_string m))) mids
;;

(* the c manager configures via low-level driver APIs*)
module Cmgr = struct 
let mgr_empty = {__csrc__|
#include "libs/mgr.h"
int main(int argc, char **argv) {
  start_switchd(argc, argv);
  return join_switchd();
}|__csrc__} ;;
end

(* the python manager configures via grpc/thrift*)
module Pymgr = struct 
let mgr_pre = {__pysrc__|
import sys, os, time
sys.path.append(os.path.dirname(os.path.realpath(__file__))+"/libs")
from mgr import *
m = Manager()
|__pysrc__} ;;

let mgr_post = {__pysrc__|
m.disconnect()
|__pysrc__} ;;

let mgr_empty = mgr_pre^mgr_post ;;

(* generate python to set up the multicast groups *)
let str_of_mc_node_tup mc_instr = sprintf "(%i, %i)" mc_instr.port mc_instr.pkt_copy_id 
let str_of_mc_nodes mc_instrs = 
  sprintf 
    "[%s]" 
    (String.concat ", " 
      (CL.map str_of_mc_node_tup mc_instrs)
    )
;;
let str_of_mc_group mc_group = 
  sprintf 
    "mgr.add_multinode_mc_group(%i, %s)"
  mc_group.mc_id
  (str_of_mc_nodes mc_group.mc_instrs)
;;

let str_of_mc_config mc_groups = 
  (String.concat
    "\n"
    (CL.map str_of_mc_group mc_groups)
  )
  ^"\n"
;;
let str_of_config dec = match dec with 
  | ConfigBlock (_, (McConfig mc_groups)) -> Some (str_of_mc_config mc_groups)
  | _ -> None
;;
(* to get python commands, make a string from every relevant config block. *)
let to_cmds decs = CL.filter_map str_of_config decs ;;

end

(* print the bf_switchd manager 
This configures pfc queues and pktgen *)
let c_mgr_of (tsprog:tblSeqProg) = 
  let _ = tsprog in 
  (* nothing to do here yet. *)
  Cmgr.mgr_empty
;;

(* print the python grpc / thrift manager 
This configures multicast *)
let py_mgr_of (tsprog:tblSeqProg) = 
  let py_mgr_str = Pymgr.mgr_pre
    ^(String.concat "\n" (Pymgr.to_cmds tsprog.tspdecls))
    ^Pymgr.mgr_post
  in 
  py_mgr_str
;;