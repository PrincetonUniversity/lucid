(* eliminate branch nodes from a DAG representation of a DPA program *)
open InstrSyntax
open MiscUtils
open DagSyntax
module MU = MergeUtils
module CL = Caml.List
open Printf
open DebugPrint

exception Error of string
let error s = raise (Error s)

(* logging *)
module DBG = BackendLogging
let outc = ref None 
let dprint_endline = ref DBG.no_printf
;;

let log_rules rules = CL.iter (fun r -> DBG.printf outc "%s" (dbgstr_of_rule r)) rules ;;
(* let log_prog cid_decls = DBG.printf outc "----\n%s\n----\n" (str_of_prog cid_decls) ;; *)

(* push the constraints on table tbl_id to every one of its successor table. *)
let push_constraints_to_successors cid_decls tbl_id = 
	(* if the successor is a join table, we have to 
		propagate conditions with a parallel merge 
		instead of a sequential merge.  *)
	let succ_tids = succs_of_tid cid_decls tbl_id in 
	DBG.printf outc "pushing constraints from %s --> %s\n" (P4tPrint.str_of_private_oid tbl_id) (str_of_cids succ_tids);
	let push_constraint_to_successor cid_decls succ_tid = 
		DBG.printf outc "[push_constraints_to_successor] PROPAGATING CONSTRAINTS %s --> %s\n" (P4tPrint.str_of_private_oid tbl_id) (P4tPrint.str_of_private_oid succ_tid);
		DBG.printf outc "[push_constraints_to_successor] PREDECESSOR (%s)\n" (P4tPrint.str_of_private_oid tbl_id);
		(* DBG.printf outc "%s\n" (p4str_from_tid cid_decls tbl_id); *)
		DBG.printf outc "[push_constraints_to_successor] SUCCESSOR (%s)\n" (P4tPrint.str_of_private_oid succ_tid);
		(* DBG.printf outc "%s\n" (p4str_from_tid cid_decls succ_tid); *)

		let cid_decls = match (is_join_tbl cid_decls succ_tid) with 
			| false ->  DBG.printf outc "[push_constraints_to_successor] propagating with ALL_MUST_MATCH\n"; MU.propagate_condition_generic MU.AllMustMatch cid_decls tbl_id succ_tid 
			| true  ->  DBG.printf outc "[push_constraints_to_successor] propagating with ONE_MUST_MATCH\n"; MU.propagate_condition_generic MU.OneMustMatch cid_decls tbl_id succ_tid
		in 
		DBG.printf outc "[push_constraints_to_successor] UPDATED SUCCESSOR (%s)\n" (P4tPrint.str_of_private_oid succ_tid);
		(* DBG.printf outc "%s\n" (p4str_from_tid cid_decls succ_tid); *)
		cid_decls
	in
	CL.fold_left push_constraint_to_successor cid_decls succ_tids
;;


(* visit tbl_id, considering it as a successor node
	(get the conditions from all of tbl_id's predecessors)
 *)
let visit_tbl_as_succ tbl_id cid_decls = 
	match (pred_tids_of_tid cid_decls tbl_id) with 
		(* no predecessors, do nothing *)
		| [] -> (
			printf "table %s has no predecessors\n" (P4tPrint.str_of_private_oid tbl_id);
			cid_decls
		)
		(* one predecessor, propagate sequential conditions *)
		| [pred_id] -> MU.propagate_condition_generic MU.AllMustMatch cid_decls pred_id tbl_id
		(* more than 1 predecessor, merge preds and propagate new table sequentially *)
		| pred_ids -> MU.merge_pred_conditions cid_decls pred_ids tbl_id
;;

(* visit node_id in unmodified graph g. transform cid_decls and the graph *)
let visit_node_condition_pushdown node_id cid_decls = 
	(* DBG.dreopen deb_outf outc; *)
	DBG.printf outc "[visit_node_condition_pushdown] %s\n" (P4tPrint.str_of_private_oid node_id);
	(* DBG.printf outc "%s" "[visit_node_condition_pushdown] table before visit\n"; *)
 	(* BUG: sometimes MU functions delete actions from table. visit really should only be applied to tables. *)
 	let cid_decls = 
 		match Cid.exists cid_decls node_id with 
 			| true -> (
		 	match (is_tbl cid_decls node_id) with 
				| false -> cid_decls (* not a table? nothing changes. *)
				| true -> visit_tbl_as_succ node_id (cid_decls)
			)
			| false -> cid_decls
	in 
(* 	DBG.printf outc "%s" "[visit_node_condition_pushdown] table after visit\n";
	DBG.printf outc "%s" (p4str_from_tid cid_decls node_id);
	DBG.printf outc "%s" "\n-------\n";
 *)	cid_decls
;;


(* replace tid with its successor tables in acn_id. acn_id is a predecessor action of tid.*)
let replace_tid_in_pred tbl_id cid_decls pred_aid = 
	let succ_tids_of_tbl = succs_of_tid cid_decls tbl_id in 
	let (aid, oids, succ_tids_of_pred) = match (Cid.lookup cid_decls pred_aid) with 
		| Action(aid, oids, succ_tids) -> (aid, oids, succ_tids) 
		| _ -> error "replace_tid_in_pred: pred_aid not an action"
	in 		
	let fold_replace_tbl_id tids tid = 
		match (Cid.equals tid tbl_id) with 
		| false -> tids@[tid]
		| true -> tids@succ_tids_of_tbl
	in 
	let new_succ_tids_of_pred = CL.fold_left fold_replace_tbl_id [] succ_tids_of_pred in 
	let new_acn = Action(aid, oids, new_succ_tids_of_pred) in 
	Cid.replace cid_decls pred_aid new_acn 
;;

(* remove a table whose actions are all empty *)
let remove_noop_tbl cid_decls tbl_id = 
	(* 	
		in the objects: 
			tbl.pred.successor_tbls += tbl.actions.successor_tbls
			tbl.pred.successor_tbls -= tbl
	*)
	DBG.printf outc "[remove_noop_tbl] tbl_id = %s\n" (P4tPrint.str_of_private_oid tbl_id);
	let pred_aids = pred_aids_of_tid cid_decls tbl_id in 

	let cid_decls = CL.fold_left (replace_tid_in_pred tbl_id) cid_decls pred_aids in 
	(* remove all of tbl_id's actions from cid_decls *)
	let cid_decls = CL.fold_left Cid.remove cid_decls (aids_of_tid cid_decls tbl_id) in 
	(* remove tbl_id from cid_decls *)
	let cid_decls = Cid.remove cid_decls tbl_id in 
	cid_decls
;;

let delete_if_noop tbl_id cid_decls  = 
	let _ = tbl_id in 
	match (CL.length (oids_of_tid cid_decls tbl_id)) with 
		| 0 -> remove_noop_tbl cid_decls tbl_id (* table has no actions, so delete *)
		| _ -> cid_decls (* table does something, can't delete *)
;;

let visit_node_noop_delete node_id cid_decls = 
	DBG.printf outc "[visit_node_noop_delete] node_id = %s\n" (P4tPrint.str_of_private_oid node_id);
	let cid_decls = match (Cid.exists cid_decls node_id) with 
		| true -> (
			match (is_tbl cid_decls node_id) with 
				| false -> cid_decls (* not a table? nothing changes. *)
				| true -> delete_if_noop node_id cid_decls (* a table and its actions might get deleted. *)
		)
		| false -> cid_decls (* object doesn't exist? nothing changes.*)
	in 
	cid_decls
;;


(* add a match on a flag to a rule *)
let with_flag flag_id rule = 
	match rule with 
		| Match(cid, pat, aid) -> 
			let new_pat = (flag_id, Exact (Integer.of_int 1))::pat in 
			Match(cid, new_pat, aid)
		| OffPath (pat) -> 
			let new_pat = (flag_id, Exact (Integer.of_int 1))::pat in 
			OffPath(new_pat)
;;



let gen_set_flag_alu flag_id = 
	let ivec_id = Cid.fresh [("set_flag"^P4tPrint.str_of_varid flag_id)] in
	let ivec = InstrVec(ivec_id, [IAssign(flag_id, Oper(Const(Integer.of_int 1)))]) in 
	ivec_id, ivec
;;

let acn_with_activate_next tid_flags cid_decls (_, dec) = 
	match dec with 
		| Action(aid, oids, next_tids) -> (
			let flag_ids = CL.map (Cid.lookup tid_flags) next_tids in 
			let new_ivec_declsmap = CL.map gen_set_flag_alu flag_ids in 
			let new_ivec_ids, _ = CL.split new_ivec_declsmap in 
			let updated_acn = Action(aid, oids@new_ivec_ids, next_tids) in 
			let new_cid_decls = Cid.replace cid_decls aid updated_acn in 
			new_ivec_declsmap@(new_cid_decls)
		)
		| _ -> cid_decls (* no-op *)


(* replace branch nodes with tables that test all constraints on their control flow *)
let eliminate_branch_nodes cid_decls g = 
	(*  We must traverse the dag topologically -- 
		before you can operate on a node, 
		you must operate on all the node's predecessors. 
	*)		
	DBG.printf outc "[eliminate_branch_nodes] adding full constraints to all tables \n";
	let cid_decls  = Topo.fold visit_node_condition_pushdown g cid_decls in 

	DBG.printf outc "[eliminate_branch_nodes] removing noop tables \n"; 
	let cid_decls  = Topo.fold visit_node_noop_delete g cid_decls in 
	(* let new_cid_decls, new_g = Topo.fold visit_node g (cid_decls, g) in  *)
	(* rebuild the graph based on new_cid_decls *)
	let new_g = DagSyntax.graph_of_declsMap cid_decls in
	cid_decls, new_g
;;




let do_passes dag_prog = 
	DBG.start_mlog __FILE__ outc dprint_endline;

	let (cid_decls, root_tid, g) = dag_prog in 
	DBG.printf outc "----original program----\n";
	(* log_prog cid_decls; *)
	let new_cid_decls, g = eliminate_branch_nodes cid_decls g in 

	DBG.printf outc "----new program----\n";
	(* log_prog new_cid_decls; *)

	let new_prog = (new_cid_decls, root_tid, g) in 
	(* log_tbl_g_and_ir new_prog "nobranch_table_call"; *)
	(* log_tbl_dot_and_prog new_prog "nobranch_table_call"; *)
  	new_prog
;;
