(* Pipeline Layout *)
open Consts
open InstrSyntax
open MiscUtils
open DagSyntax
module MU = MergeUtils
module CL = Caml.List
open Printf
open Format
open DebugPrint
open MiscUtils

exception Error of string
let error s = raise (Error s)

(* logging *)
module DBG = BackendLogging
let outc = ref None 
let dprint_endline = ref DBG.no_printf
;;

type layout_constraints = 
	{
	(* per-stage constraints *)
	max_tbls_per_stage : int;
	max_regs_per_stage : int;
	max_hashers_per_stage : int;
	(* per-table constraints *)
	max_regs_per_tbl : int;
	max_hashers_per_tbl : int;
	max_tbls_per_tbl : int;
	}
;;
(* tofino1 limitations *)
let def_pipe_limits = 
	{
		max_tbls_per_stage = 16;
		max_regs_per_stage = 4;
		max_hashers_per_stage = 6; 
		max_regs_per_tbl = 1;
		max_hashers_per_tbl = 1;
		(* 
			A table can hash 32-bits. The limit is 1 because we currently assume hash functions are 32 bits. 
			But we can handle multiple hash functions, as long as total output is 32-bits:
			"In table table Ingress.dptIngressManager.dptEng.merged_tbl_1, the number of bits required to go through the immediate pathway 64 (e.g. data coming from Hash, RNG, Meter Color) is greater than the available bits 32, and can not be allocated"		
		 *)
		max_tbls_per_tbl = 100; (* how many tables can we merge into 1? This is a self-imposed limit. Should actually limit number of columns in table *)
	}
;;

(* a table_group is a set of tables that can be merged *)
type table_group = oid list
(* a stage_layout is a list of table_groups *)
type stage_layout = table_group list
(* a layout is a list of stage_layouts *)
type layout = stage_layout list

(**** layout printing ****)

let str_of_group tbl_grp = sprintf "[%s]" (str_of_cids tbl_grp) 

let str_of_stage_layout stg = sprintf "[%s]" (String.concat ", " (CL.map str_of_group stg))

let str_of_pipe pipe = String.concat "\n" (CL.map str_of_stage_layout pipe) 

let dump_layout pipe = DBG.printf outc "layout:\n%s\n" (str_of_pipe pipe)

(* helpers *)
let root_id = Cid.create ["ROOT"]

let root_vertices_of g = 
	let is_root_vertex v =
		match (CL.length (G.pred g v)) with
			| 0 -> true
			| _ -> false
	in 
	let fold_f v roots = 
		match (is_root_vertex v) with 
		| true -> roots@[v]
		| false -> roots
	in 
	let roots = G.fold_vertex fold_f g [] in 
	roots
;;

(* create a graph that has both tables and registers, and an explicit root node *)
let to_tbl_reg_graph cid_decls g = 
	let add_edge src_id g dst_id = G.add_edge g src_id dst_id in 
	(* for each table, get all registers. Add edges from tbl to register *)	
	let add_tbl_regs (tbl_id:oid) (g:G.t) : G.t = 
(* 		G.add_edge g tbl_id tbl_id *)		
		let reg_ids = rids_of_tid cid_decls tbl_id in 
		CL.fold_left (add_edge tbl_id) g reg_ids
	in 
	let new_g = G.fold_vertex add_tbl_regs g g in 
	(* add a root node, with edges to all root tables *)
	let new_g = CL.fold_left (add_edge root_id) new_g (root_vertices_of new_g) in 
	new_g
;;	

(* length of the longest path to rid in tr_g *)
let max_len_to tr_g tgt_id = 
	let (edge_path, len) = ND.shortest_path tr_g root_id tgt_id in 
	let _ = len in 
	let edge_len = CL.length edge_path in 
	(* use edge len -- len is a negative because of negative weights*)
	edge_len
;;

(* length of longest path from root to one of the registers that tid uses *)
let longest_pathlen_to_tid cid_decls tr_g tid = 
	let rids = rids_of_tid cid_decls tid in 
	let rid_pathlens = CL.map (max_len_to tr_g) rids in 
	CL.fold_left max 0 rid_pathlens
;;

(* sort tables based on the maximum length of the path from root to a register that the table uses *)
let reg_call_sort cid_decls tr_g tids = 
	let cmp tid_a tid_b = 
		compare (longest_pathlen_to_tid cid_decls tr_g tid_a) (longest_pathlen_to_tid cid_decls tr_g tid_b)
	in 
	CL.sort cmp tids
;;

(* are all the predecessors of tbl_id placed? *)
let is_tbl_placeable tr_g unalloc_tids tbl_id = 
	let pred_tids = G.pred tr_g tbl_id in 
	(* if one pred tid is in unallocated, we're done *)
	let pred_in_unalloc = CL.map (fun pred_id -> CL.mem pred_id unalloc_tids) pred_tids in 
	let cannot_be_placed_yet = CL.fold_left (or) (false) pred_in_unalloc in 
	cannot_be_placed_yet
;;

let remove a_list a = CL.filter (fun aa -> aa<>a) a_list ;;


(* create a new group with this table in it *)
let new_group_with_tbl tbl_id = 
	[tbl_id]
;;

(* create a new stage_layout with this table in it *)
let new_stage_layout_with_tbl tbl_id = 
	let new_stage_layout = [new_group_with_tbl tbl_id] in 
	new_stage_layout
;;

let rids_of_group cid_decls tbl_group = 
	CL.fold_left (fun tids tbl_id -> tids@(rids_of_tid cid_decls tbl_id)) [] tbl_group |> unique_list_of
;;

let rids_of_groups cid_decls tbl_groups = 
	CL.map (rids_of_group cid_decls) tbl_groups |> CL.flatten |> unique_list_of
;;

let hids_of_group cid_decls tbl_group = 
	tbl_group |> CL.map (hids_of_tid cid_decls) |> CL.flatten |> unique_list_of
;;

let tids_of_stage_layout stage_layout = CL.flatten stage_layout

let tids_of_stage_layouts stage_layouts = CL.map tids_of_stage_layout stage_layouts |> CL.flatten 

let rids_of_stage_layout cid_decls stage_layout = 
	(tids_of_stage_layout stage_layout) 
	|> CL.fold_left (fun rids tbl_id -> rids@(rids_of_tid cid_decls tbl_id)) [] 
	|> unique_list_of
;;
let rids_of_stage_layouts cid_decls stage_layouts = 
	CL.map (rids_of_stage_layout cid_decls) stage_layouts |> CL.flatten |> unique_list_of
;;

let hids_of_stage_layout cid_decls stage_layout = 
	(tids_of_stage_layout stage_layout) 
	|> CL.fold_left (fun hids tbl_id -> hids@(hids_of_tid cid_decls tbl_id)) [] 
	|> unique_list_of
;;


let regs_accessed_by_groups cid_decls tbl_groups tbl_id = 
	let other_groups_rids = rids_of_groups cid_decls tbl_groups in 
	CL.map (fun reg_id -> CL.mem reg_id other_groups_rids) (rids_of_tid cid_decls tbl_id) 
	|> CL.fold_left (or) false
;;

(* all the hash objects used by a group *)
(* let hids_of_group cid_decls tbl_group =  *)


(******** resource limit checking ********)

let group_has_reg_capacity cid_decls group tbl_id = 
	let all_rids = unique_list_of ((rids_of_group cid_decls group)@(rids_of_tid cid_decls tbl_id)) in 
	CL.length (all_rids) <= def_pipe_limits.max_regs_per_tbl
;;

let group_has_hasher_capacity cid_decls group tbl_id = 
	let all_hids = unique_list_of ((hids_of_group cid_decls group)@(hids_of_tid cid_decls tbl_id)) in 
	CL.length (all_hids) <= def_pipe_limits.max_hashers_per_tbl
;;

let group_has_tbl_capacity _ group tbl_id = 
	let all_tids = unique_list_of (tbl_id::group) in 
	CL.length (all_tids) <= def_pipe_limits.max_tbls_per_tbl
;;


(* can tbl_id be merged into this group? *)
let placeable_in_table_group cid_decls stage_layout tbl_id group = 
	let parallel_groups = remove stage_layout group in 
	let tbl_rids = rids_of_tid cid_decls tbl_id in 
	let other_group_rids = rids_of_groups cid_decls parallel_groups in 
	(* none of the tbls regs can be accessed by another group *)
	let tbl_rids_in_use = CL.map (fun reg_id -> CL.mem reg_id other_group_rids) tbl_rids in 
	let cond_reg_is_free = not (CL.fold_left (or) false tbl_rids_in_use) in 
	let cond_reg_slots = (group_has_reg_capacity cid_decls group tbl_id) in 
	let cond_hash_slots = (group_has_hasher_capacity cid_decls group tbl_id) in 
	let cond_tbl_slots = group_has_tbl_capacity cid_decls group tbl_id in 
	DBG.printf outc "[fits_in_table_group] table: %s group: [%s] cond_reg_is_free: %s cond_reg_slots: %s cond_hash_slots: %s " (P4tPrint.str_of_private_oid tbl_id) (str_of_group group) (string_of_bool cond_reg_is_free) (string_of_bool cond_reg_slots) (string_of_bool cond_hash_slots);
	cond_reg_is_free (* no tbl regs are used by other groups *)
	&& cond_reg_slots (* the group has room for the tbl's regs *)
	&& cond_hash_slots (* the group has room for the tbl's hashers *)
	&& cond_tbl_slots (* the group has room for more tables, in general *)
;;

(* number of regs with table added <= def_pipe_limits.max_regs_per_stage *)
let stage_has_reg_slots cid_decls stage_layout tbl_id = 
	let all_rids = unique_list_of ((rids_of_tid cid_decls tbl_id)@(rids_of_stage_layout cid_decls stage_layout)) in 
	CL.length all_rids <= def_pipe_limits.max_regs_per_stage
;;

let stage_has_hasher_slots cid_decls stage_layout tbl_id = 
	let all_hids = unique_list_of (
		(hids_of_tid cid_decls tbl_id)
		@(hids_of_stage_layout cid_decls stage_layout)
	)
	in 
	CL.length all_hids <= def_pipe_limits.max_hashers_per_stage
;;




(* returns true if any register used by tbl_id is accessed in a prior stage_layout *)
let reg_previously_accessed cid_decls prev_stage_layouts tbl_id = 
	let all_stage_layout_rids = rids_of_stage_layouts cid_decls prev_stage_layouts in 
	let tbl_rids = rids_of_tid cid_decls tbl_id in 

	DBG.printf outc "[reg_previously_accessed] tbl: %s \n" (P4tPrint.str_of_private_oid tbl_id);
	DBG.printf outc "[reg_previously_accessed] tbl_rids: [%s]\n" (str_of_cids tbl_rids);
	DBG.printf outc "[reg_previously_accessed] all_stage_layout_rids: [%s]\n" (str_of_cids all_stage_layout_rids);

	(CL.fold_left 
			(fun previous_reg_accessed rid -> 
				previous_reg_accessed or (CL.mem rid all_stage_layout_rids)
			) 
			false 
			tbl_rids
		)
;;

(* are all the predecessors of tbl_id placed in the layout? *)
let dependees_are_placed cid_decls tr_g layout tbl_id = 
	let rootless_tr_g = (G.remove_vertex tr_g root_id) in 
	let tbl_rids = rids_of_tid cid_decls tbl_id in 
	(* get dependees of other tables that access the register *)
	let other_rid_tbls = CL.map (fun rid -> G.pred rootless_tr_g rid) tbl_rids |> CL.flatten in 
	let other_rid_tbl_preds = CL.map (fun tid -> G.pred rootless_tr_g tid) other_rid_tbls |> CL.flatten in 
	let pred_ids = G.pred rootless_tr_g tbl_id in 

	let all_pred_ids = pred_ids@other_rid_tbl_preds in 
	let placed_tids = tids_of_stage_layouts layout in 
	DBG.printf outc "[dependees_are_placed] tbl_id: %s\n" (P4tPrint.str_of_private_oid tbl_id);
	DBG.printf outc "[dependees_are_placed] pred_ids: %s\n" (str_of_cids pred_ids);
	DBG.printf outc "[dependees_are_placed] tbl_rids: %s\n" (str_of_cids tbl_rids);
	DBG.printf outc "[dependees_are_placed] other_rid_tbls: %s\n" (str_of_cids other_rid_tbls);
	DBG.printf outc "[dependees_are_placed] other_rid_tbl_preds: %s\n" (str_of_cids other_rid_tbl_preds);
	DBG.printf outc "[dependees_are_placed] placed_tids: %s\n" (str_of_cids placed_tids);
	let are_preds_placed = CL.map (fun p_id -> CL.mem p_id placed_tids) all_pred_ids in 
	CL.fold_left (&&) true are_preds_placed
;;

(* does the stage_layout have room for the table? *)
let stage_has_tbl_capacity cid_decls stage_layout tbl_id = 
	match (CL.length stage_layout < def_pipe_limits.max_tbls_per_stage) with 
		| true -> true (* if we can fit another table group, we can fit this table. *)
		| false -> (
			(* if we can't fit another table group, we need to check if there's at least 
			1 table that the group can be merged into. *)
			let compatible_groups = CL.filter (placeable_in_table_group cid_decls stage_layout tbl_id) stage_layout in 
			match (CL.length compatible_groups) with
				| 0 -> false
				| _ -> true
		) 
;;

(* can the table be placed into a stage? *)
let placeable_in_stage cid_decls tr_g stage_layout previous_stage_layouts tbl_id = 
	(* 
		conditions: 
			1. no registers used by tbl_id are accessed in other_stage_layouts
			2. number of regs with table added <= def_pipe_limits.max_regs_per_stage
			3. number of hashers with table added <= def_pipe_limits.max_hashers_per_stage
			4. either: 
				1. the table can be placed into some group in the stage_layout 
				or
				2. there is room for an additional group in the stage_layout
	*)

	let prev_reg_use = (reg_previously_accessed cid_decls previous_stage_layouts tbl_id) in 
	let reg_cap = (stage_has_reg_slots cid_decls stage_layout tbl_id) in 
	let hasher_cap = (stage_has_hasher_slots cid_decls stage_layout tbl_id) in 
	let dep_placed = (dependees_are_placed cid_decls tr_g previous_stage_layouts tbl_id) in 
	let tbl_cap = (stage_has_tbl_capacity cid_decls stage_layout tbl_id) in 
 	let sob = string_of_bool in 
	DBG.printf outc "[fits_in_stg] tbl_id: %s\n\tprev_reg_use: %s\n\treg_cap: %s\n\tdep_placed: %s\n\ttbl_cap: %s\n" (P4tPrint.str_of_private_oid tbl_id) (sob prev_reg_use) (sob reg_cap) (sob dep_placed) (sob tbl_cap);

	(not prev_reg_use)
	&& reg_cap
	&& hasher_cap
	&& dep_placed
	&& tbl_cap
	(* && (check_stage_layout_max_hashers cid_decls stage_layout tbl_id) *)
;;


let place_in_group group tbl_id = group@[tbl_id]

(* try to place a table tid into a table group in the stage_layout *)
let place_tid_in_stage_layout cid_decls stage_layout tbl_id = 
	(* get the first table group that can fit the table *)
	DBG.printf outc "[place_tid_in_stage_layout] placing table %s\n" (P4tPrint.str_of_private_oid tbl_id);
	let update_group (is_placed, new_stage_layout) group = 
		match is_placed with
			| true -> true, new_stage_layout@[group]
			| false -> (
				match (placeable_in_table_group cid_decls stage_layout tbl_id group) with 
				| true -> true, new_stage_layout@[place_in_group group tbl_id]
				| false -> false, new_stage_layout@[group]
			)
	in 
	let placed_in_current_groups, new_stage_layout = CL.fold_left update_group (false, []) stage_layout in 

	(* if no table group can fit the table, place it in a new table group *)
	match placed_in_current_groups with
		| false -> stage_layout@[new_group_with_tbl tbl_id]
		| true -> new_stage_layout
;;

(* 
	place the table in the last stage of the layout if possible and return updated layout. 
	if not possible, return none
 *)
let place_tid (cid_decls:declsMap) tr_g curr_pipe tbl_id : layout option = 	
	DBG.printf outc "[place_tid] tid: %s\n" (P4tPrint.str_of_private_oid tbl_id);	
	let pred_ids = G.pred (G.remove_vertex tr_g root_id) tbl_id in 
	DBG.printf outc "[place_tid] dependees: [%s]\n" (str_of_cids pred_ids);	

	match CL.rev (curr_pipe) with 
	| [] -> (
		(* can the table be placed in the first stage? *)
		match (placeable_in_stage cid_decls tr_g [] [] tbl_id) with 
			| true -> Some [new_stage_layout_with_tbl tbl_id]
			| false -> None
	)
	| last_stage_layout::stage_layouts_rev -> (
		(* if possible, place the table in the last stage_layout. If not, place it in a new stage_layout. *)
		(* DBG.printf deb_outc "[place_tid] placing table: %s\n" (P4tPrint.str_of_private_oid tbl_id); *)
		match (placeable_in_stage cid_decls tr_g last_stage_layout stage_layouts_rev tbl_id) with 
			(* the table can be placed in this stage. *)
			| true -> (
					DBG.printf outc "[place_tid] tbl fits in stage\n";
					let new_last_stage_layout = place_tid_in_stage_layout cid_decls last_stage_layout tbl_id in 
					let new_layout = CL.rev (new_last_stage_layout::stage_layouts_rev) in 
					DBG.printf outc "layout after placing table %s:\n" (P4tPrint.str_of_private_oid tbl_id);
					dump_layout new_layout;
					DBG.printf outc "------\n";
					Some new_layout	
				)			
			(* the table cannot be placed in this stage. *)
			| false ->  (
				DBG.printf outc "[place_tid] tbl does NOT fit in stage\n";
				None
(* 				let new_layout = CL.rev ((new_stage_layout_with_tbl tbl_id)::last_stage_layout::stage_layouts_rev) in 

				DBG.printf outc "layout after placing table %s:\n" (P4tPrint.str_of_private_oid tbl_id);
				dump_layout new_layout;
				DBG.printf outc "------\n";
				new_layout
 *)			)
	
	)
;;

(* place the next tid from the unallocated tids, prefer tids with registers used sooner *)
(* assumption: the first unallocated tid (sorted by register dependencies) is always placeable,
either in the last stage_layout or a new stage_layout  *)
let rec place_next_tid cid_decls tr_g this_round_tids next_round_tids curr_pipe = 
	let this_round_tids = reg_call_sort cid_decls tr_g this_round_tids in 
	match this_round_tids with 
		| [] -> (this_round_tids, next_round_tids, curr_pipe) (* done with this round. *)
		| next_tid::this_round_tids -> (
			(* try to place next_tid in the last stage of curr_pipe *)
			let updated_pipe_opt = place_tid cid_decls tr_g curr_pipe next_tid in 
			match updated_pipe_opt with
				(* success! move to next table.  *)
				| Some updated_pipe -> 
					place_next_tid cid_decls tr_g this_round_tids next_round_tids updated_pipe
				(* failure. place tid in list for next round and move to next table. *)
				| None -> (
					let updated_next_round_tids = (next_round_tids@[next_tid]) in 
					place_next_tid cid_decls tr_g this_round_tids updated_next_round_tids curr_pipe
				)
		)
;;
(* place every table in tids. Repeat passes until complete. *)
let rec place_pass cid_decls tr_g tids (curr_pipe:layout) : layout = 
	match (CL.length tids) with 
		| 0 -> curr_pipe
		| _ -> (
			let _, next_round_tids, updated_pipe = place_next_tid cid_decls tr_g tids [] curr_pipe in 
			let pipe_with_new_stage = updated_pipe@[[]] in 
			place_pass cid_decls tr_g next_round_tids pipe_with_new_stage
		)
;;

let to_layout (cid_decls:declsMap) (g:G.t) : layout = 
	let tr_g = to_tbl_reg_graph cid_decls g in 
	let oids, _ = CL.split cid_decls in 
	let tids = CL.filter (is_tbl cid_decls) oids in 

	DBG.printf outc "unallocated tids before placement: %i\n" (CL.length tids);	
	let layout = place_pass cid_decls tr_g tids [] in 
	let layout = match CL.rev layout with 
		| [] -> []
		| _::layout -> CL.rev layout
	in 
	layout
;;

let log_layout layout fn = 
	Caml.Printf.fprintf (open_out ((!IoUtils.logDir)^fn)) "%s" (str_of_pipe layout)
;;

let do_passes (dag_prog : dagProg) = 
	DBG.start_mlog __FILE__ outc dprint_endline;
	let (cid_decls, _, g) = dag_prog in 
(* 	let _, _, _, _, _, _ = (pid, builtin_params, user_params, cid_decls, root_tid, g) in 
 *)	DBG.printf outc "----DagInstructionSelection----\n";
	let layout = to_layout cid_decls g in 
	let num_stages = CL.length layout in 
	let num_tbls = CL.map (CL.length) layout |> CL.fold_left (+) 0 in 
	DBG.printf outc "------LAYOUT (%i stages and %i tables) ------\n" num_stages num_tbls;
	dump_layout layout;
	DBG.printf outc ("------END LAYOUT------\n");
	log_layout layout "/table_dataflow_layout.txt";
	layout
;;

