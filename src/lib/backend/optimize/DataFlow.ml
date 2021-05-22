open InstrSyntax
open MiscUtils
open DagSyntax
module MU = MergeUtils
module CL = Caml.List
open MiscUtils
open Printf
open DebugPrint


exception Error of string
let error s = raise (Error s)

(* logging *)
module DBG = BackendLogging
let outc = ref None 
let dprint_endline = ref DBG.no_printf

(* find the list of most recent predecessors that write to a certain variable, starting from 1 table *)
let rec most_recent_set_var_from_tbl cid_decls var_id tbl_id : oid list = 
	match (tbl_writes_var cid_decls tbl_id var_id) with 
		(* if the tbl writes var_id, that is the pred *)
		| true -> [tbl_id] 
		(* tbl does not write var_id, so we must check its predecessors. Note that there can be multiple predecessors. *)
		| false -> CL.map (most_recent_set_var_from_tbl cid_decls var_id) (pred_tids_of_tid cid_decls tbl_id) |> CL.flatten
;;
(* find most recent list of predecessors that write to var_id, starting from a list of predecessors *)
let most_recent_set_var_from_preds cid_decls pred_tids var_id = 
	CL.map (most_recent_set_var_from_tbl cid_decls var_id) pred_tids |> CL.flatten |> unique_list_of
;;


(* find the list of most recent predecessors that write to a certain variable, starting from 1 table *)
let rec most_recent_use_var_from_tbl cid_decls var_id tbl_id : oid list = 
	match (tbl_reads_var cid_decls tbl_id var_id) with 
		(* if the tbl reads var_id, that is the pred *)
		| true -> [tbl_id] 
		(* tbl does not read var_id, so we must check its predecessors. Note that there can be multiple predecessors. *)
		| false -> CL.map (most_recent_use_var_from_tbl cid_decls var_id) (pred_tids_of_tid cid_decls tbl_id) |> CL.flatten
;;
(* find most recent list of predecessors that read var_id, starting from a list of predecessors *)
let most_recent_use_var_from_preds cid_decls pred_tids var_id = 
	CL.map (most_recent_use_var_from_tbl cid_decls var_id) pred_tids |> CL.flatten |> unique_list_of
;;

(* get the edges (s -> tbl_id), where s is a table that writes a variable that tbl_id reads *)
let get_data_dep_edges cid_decls tbl_id all_data_dep_edges = 
	(* for each variable read in the table, find the most recently called predecessor table 
	that writes to it*)
	(* for each variable written by the table, find the most recently called predecessor table 
	that reads it. *)
	match (is_tbl cid_decls tbl_id) with 
	| false -> all_data_dep_edges (* skip non-tables *)
	| true -> (
		let pred_tids = pred_tids_of_tid cid_decls tbl_id in 
		(* get the list of variables that tbl_id reads *)
		let vars_used = read_vars_of_tbl cid_decls tbl_id in 
		DBG.printf outc "[get_data_dep_edges] tbl %s uses variables: [%s]\n" (P4tPrint.str_of_private_oid tbl_id) (str_of_cids vars_used);
		let use_var_dependee_tids = 
			CL.map 
				(most_recent_set_var_from_preds cid_decls pred_tids) 
				vars_used 
			|> CL.flatten 
			|> unique_list_of 
		in 
		let use_var_edges = (CL.map (fun pred_t -> (pred_t, tbl_id)) use_var_dependee_tids) in 
		DBG.printf outc "[get_data_dep_edges] due to the vars it uses, tbl %s must execute after tbls: [%s]\n" (P4tPrint.str_of_private_oid tbl_id) (str_of_cids use_var_dependee_tids);

		let vars_set = write_vars_of_tbl cid_decls tbl_id in 
		DBG.printf outc "[get_data_dep_edges] tbl %s sets variables: [%s]\n" (P4tPrint.str_of_private_oid tbl_id) (str_of_cids vars_set);
		let set_var_dependee_tids = 
			CL.map 
				(most_recent_use_var_from_preds cid_decls pred_tids) 
				vars_set 
			|> CL.flatten 
			|> unique_list_of 
		in 
		let set_var_edges = (CL.map (fun pred_t -> (pred_t, tbl_id)) set_var_dependee_tids) in 
		DBG.printf outc "[get_data_dep_edges] due to vars it sets, tbl %s must execute after tbls: [%s]\n" (P4tPrint.str_of_private_oid tbl_id) (str_of_cids set_var_dependee_tids);

		all_data_dep_edges@use_var_edges@set_var_edges
	)
;; 

(* generate a data dependent dag of the tables in g *)
let data_dep_dag_of cid_decls g = 
	(* copy of g with reversed edges *)
	let rev_g = reverse_dag_of g in 
	let new_edges = Topo.fold (get_data_dep_edges cid_decls) rev_g [] in 
	let oids, _ = CL.split cid_decls in 
	(* create data_dag, adding tables as vertices *)
	let data_dag = oids 
		|> CL.filter (is_tbl cid_decls) 
		|> CL.fold_left (fun data_dag tbl_id -> G.add_vertex data_dag tbl_id) G.empty 
	in 
	(* add data dependency edges *)
	let data_dag = CL.fold_left (fun data_dag (s, d) -> G.add_edge data_dag s d) data_dag new_edges in 
	data_dag
;;




(* convert a DAG with edges that represent call order into a DAG with edges that represent data dependencies. *)
let do_passes dag_prog = 
	DBG.start_mlog __FILE__ outc dprint_endline;
	let (cid_decls, root_tid, g) = dag_prog in 
	let data_dag = data_dep_dag_of cid_decls g in 
	let new_prog =	(cid_decls, root_tid, data_dag) in 
(* 	log_tbl_g_and_ir new_prog "table_dataflow";
 *)
	new_prog	
;;

