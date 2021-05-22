(* Generate a pipeline from a layout *)
open Consts
open InstrSyntax
open MiscUtils
open DagSyntax
open InstrSeqSyntax
module MU = MergeUtils
module CL = Caml.List
open Printf
open Format
open PipelinePlanning
open MiscUtils

exception Error of string
let error s = raise (Error s)

(* logging *)
module DBG = BackendLogging
let outc = ref None 
let dprint_endline = ref DBG.no_printf
;;

let info_outc = ref None 
let info_print_endline = ref DBG.no_printf
;;

(* a pipeline is a list of local variables (metadata) and stages of tables.
	each table in the pipeline implements an entire table_group from layout. *)
(* type local_var = decl
type stage = decl list 
type pipeline = local_var list * stage list
 *)
type data_defs = decl list
type compute_stage = decl list
type pipeline = data_defs * compute_stage list 

let vars_of_pipeline p = fst p
let objs_of_pipeline p = snd p |> CL.flatten

let verify_no_dup_decls straightline_prog = 
	let decls = straightline_prog.tspdecls in 
	match (find_first_dup decls) with 
		| Some (d) -> error ("duplicate declarations at the end of pipeline generation! ("^(P4tPrint.str_of_private_oid (id_of_decl d)))
		| _ -> ()
;;

(**** pipeline printing ****)

let str_of_pipeline_vars pipeline = 
	let (local_vars, _) = pipeline in 
	let _ = local_vars in 
	pp_open_vbox str_formatter 0;
	(* fprintf str_formatter "//--------VARIABLES----------@,"; *)
	(* DagToP4.print_decls_from_list str_formatter local_vars; *)
	pp_close_box str_formatter ();
	let str = flush_str_formatter () in 
	str 
;;

let str_of_stage (stage_layout_num:int) stage = 	
	fprintf str_formatter "//--------STAGE %i ----------@," stage_layout_num;
	let _ = stage in 
	(* DagToP4.print_decls_from_list str_formatter stage; *)
	stage_layout_num + 1
;;

let str_of_pipeline_stages pipeline = 
	let (_, compute) = pipeline in 
	pp_open_vbox str_formatter 0;
	let _ = CL.fold_left str_of_stage 0 compute in 
	pp_close_box str_formatter ();
	let str = flush_str_formatter () in 
	str
;;

let tbls_of_decls decls = (Caml.List.filter (fun d -> match d with Table _ -> true | _ -> false) decls)
let nontbls_of_decls decls = (Caml.List.filter (fun d -> match d with Table _ -> false | _ -> true) decls)

let apply_str_of_stage stage = 
	let tids = tbls_of_decls stage |> CL.map tid_of_tbl in 
	CL.iter (fun tid -> fprintf str_formatter "apply(%s);@," (P4tPrint.str_of_private_oid tid)) tids;
;;
(* print the apply statement for a pipeline. This should just be a sequence of table calls *)
let apply_str_of_pipeline pipeline = 
	let _, stages = pipeline in 
	(* print out the table names in order *)
	pp_open_vbox str_formatter 4;
	fprintf str_formatter ("apply {@,");
	CL.iter apply_str_of_stage stages;
	pp_close_box str_formatter ();
	fprintf str_formatter ("}@,");
	flush_str_formatter ()
;;

let body_str_of_pipeline pipeline = 
	let pipeline_body = String.concat "\n" [
		(str_of_pipeline_vars pipeline);
		(str_of_pipeline_stages pipeline);
		(apply_str_of_pipeline pipeline)
	]
	in 
	sprintf "{\n%s\n}" pipeline_body
;;

(**** pipeline generation from layouts ****)

let merged_tbl_ctr = ref 0

let next () = 
  merged_tbl_ctr := !merged_tbl_ctr + 1;
  !merged_tbl_ctr
;;

let fresh_tbl cid_decls = 
	let merged_id = next () in 
	let tbl_id = Cid.create_ids [Id.to_id ("merged_tbl", merged_id)] in 
	let acn_id = Cid.create_ids [Id.to_id ("merged_acn", merged_id)] in 
	let acn = Action(acn_id, [], []) in
	(* a rule to do nothing for any packet.*)
	let def_rule = Match (Cid.fresh ["r"], [], acn_id) in 
	let tbl = Table(tbl_id, [def_rule], None) in 
	tbl_id, cid_decls@[ (acn_id, acn);(tbl_id, tbl)]
;;


let dedup_sivec decs = 
	(* make map of instruction vectors. 
	   for each instruction vector, only add it if its not already in the list. 
		if it is in the list, replace all references to it. *)
	let mapf_f dec = match dec with 
		| SInstrVec (oid, rid, wid, siv, out, idx) -> (
			Some (oid, (rid, wid, siv, out, idx))
		)
		| _ -> None 
	in 
	let saluid_body = CL.filter_map mapf_f decs in 
	let _, bodies = CL.split saluid_body in 
	let bodies = unique_list_of bodies in 
	let newids = CL.map (fun _ -> Cid.fresh ["unique_salu"]) bodies in 
	let body_newid = CL.combine bodies newids in 
	(* update every salu_id reference *)
	let replace_f dec = match dec with 
		| Action (aid, oids, tids) -> (
			(* oid : body : new_id *)
			let inner_map_f oid = 
				print_endline (P4tPrint.str_of_varid oid);
				let body_opt = CL.assoc_opt oid saluid_body in 
				match body_opt with 
					| Some body -> CL.assoc body body_newid
					| None -> oid 
			in 
			let new_oids = CL.map inner_map_f oids |> unique_list_of in 
			Some (Action (aid, new_oids, tids))
		)
		| SInstrVec _ -> None 
		| _ -> Some dec
	in 
	let new_non_salu_decls = CL.filter_map replace_f decs in 
	let gen_salu_f ((rid, wid, siv, out, idx), oid) = 
		SInstrVec(oid, rid, wid, siv, out, idx)
	in 
	let new_salu_decls = CL.map gen_salu_f body_newid in 
	new_salu_decls@new_non_salu_decls
;;

(* convert a table group into a merged table declaration *)
let merged_tbl_of_tbl_group cid_decls tbl_grp : decl list = 
	DBG.printf outc "[merge_into_master]%s" "-----Original tables-----\n";
	(* CL.iter (fun tid -> DBG.printf outc "[merge_into_master]------\n%s\n" (p4str_from_tid cid_decls tid)) tbl_grp; *)
	let new_tbl_id, cid_decls = fresh_tbl cid_decls in 
	let merge_into_master cid_decls tbl_id = 
		DBG.printf outc "[merge_into_master] merging %s into %s\n" (P4tPrint.str_of_private_oid tbl_id) (P4tPrint.str_of_private_oid new_tbl_id);
		DBG.printf outc "[merge_into_master]-----MASTER-----\n";
		(* DBG.printf outc "%s\n" (p4str_from_tid cid_decls new_tbl_id); *)
		DBG.printf outc "[merge_into_master]-----TABLE TO BE MERGED-----\n";
		(* DBG.printf outc "%s\n" (p4str_from_tid cid_decls tbl_id); *)
		let cid_decls = MU.parallel_merge cid_decls new_tbl_id tbl_id in 
		DBG.printf outc "[merge_into_master]-----NEW MASTER-----\n";
		(* DBG.printf outc "%s\n" (p4str_from_tid cid_decls new_tbl_id); *)
		DBG.printf outc "[merge_into_master]-----DUP STATUS-----\n";		
		cid_decls 
	in 
	let cid_decls = CL.fold_left merge_into_master cid_decls tbl_grp in 

	(* return the merged table, prefixed by all of the objects that it calls and actions that it uses *)
	let ret_decls = (decls_of_tid cid_decls new_tbl_id)@[Cid.lookup cid_decls new_tbl_id] in 
	let ret_decls = dedup_sivec ret_decls in 
	(* DBG.printf outc "[merged_tbl_of_tbl_group] tbl_grp: [%s]\n" (str_of_cids tbl_grp); *)
	ret_decls
;;

(* set the stage annotations of all tables in a stage *)
let annotate_decls_in_stage stage_num stage = 
	DBG.printf outc "[annotate_decls_in_stage] stage %i has %i declarations\n" stage_num (CL.length stage);
	let map_f dec = 
		match dec with 
			| Table(tid, rules, _) -> DBG.printf outc "[annotate_decls_in_stage] table: %s stage: %i\n" (P4tPrint.str_of_private_oid tid) (stage_num); Table(tid, rules, Some stage_num)
			| _ -> dec
	in 
	stage_num + 1, CL.map map_f stage
;;

(* set the stage annotations of all tables in the pipe *)
let annotate_decls_in_pipe stages = 	
	let fold_f (stage_num, new_stages) stage = 
		let nxt_stage_num, new_stage = annotate_decls_in_stage stage_num stage in 
		nxt_stage_num, new_stages@[new_stage]
	in 
	let _, annotated_stages = CL.fold_left fold_f (0, []) stages in 
	annotated_stages
;;

(* convert a stage_layout into a stage *)
let stage_of_stage_layout cid_decls (stage_layout:stage_layout) : compute_stage = 
	let tbl_decls = stage_layout 
		|> CL.map (fun tbl_group -> merged_tbl_of_tbl_group cid_decls tbl_group) 
		|> CL.flatten 
	in 
	(* 5/18/21 fix: register decls go at the beginning, not near the stage objects *)
(* 	let reg_decls = rids_of_stage_layout cid_decls stage_layout 
		|> CL.map (fun rid -> Cid.lookup cid_decls rid) in 
	reg_decls@tbl_decls *)
	tbl_decls
;;

(* convert a layout into a pipeline *)
let pipeline_of_layout cid_decls (layout:layout) : pipeline = 
	let stages = layout 
		(* merge the tables *)
		|> CL.map (fun stage_layout -> stage_of_stage_layout cid_decls stage_layout) 
		(* annotate the tables *)
		|> annotate_decls_in_pipe
	in 
	DBG.printf outc "[pipeline_of_layout] number of stages: %i\n" (CL.length stages);	
	(* gather all the declarations that are at fixed locations. *)
	let map_f (_, dec) = match (is_fixedloc_decl dec) with 
		| true -> Some dec
		| false -> None
	in
	let var_decls = CL.filter_map map_f cid_decls in 
	(var_decls, stages)
;;

(* generate a straight line of calls to dynamically placed tables *) 
let dpa_statements_of_pipe (pipe:pipeline) : tblStmt = 
	let _, stages = pipe in 

	let placed_tbls = CL.flatten stages |> tbls_of_decls in 
	(* 5/18/21 -- give native blocks their own processors. *)
	(* let end_native_blocks =  filter_end_native_blocks fixedloc_decls in  *)
	(* the sequence of table ids to call. *)
	let tids = CL.map tid_of_tbl (placed_tbls) in 

	let rec dpa_statement_of_tids (tids:oid list) : tblStmt = 
		match tids with
			| [] -> Noop
			| tid::[] -> CallTable tid
			| tid::tids -> Seq (CallTable(tid),dpa_statement_of_tids tids)
	in 
	dpa_statement_of_tids tids
;;

let tblseq_of_pipe (pipe : pipeline) : tblSeq = 
	let dpa_statements = dpa_statements_of_pipe pipe in 
	{
		tsname = mergedDpName;
		tsstmt = dpa_statements;
	}
;;

let tblseqprog_of_pipe (pipe : pipeline) : tblSeqProg = 
	{
		tspname = Consts.progId;
		tspglobals = Consts.globalArgs;
		tspglobal_widths = Consts.globalArgWidths;
		tspdecls = (vars_of_pipeline pipe)@(objs_of_pipeline pipe);
		tsptblseq = tblseq_of_pipe pipe;
	}
;;

let remove_noops_from_stage noop_aid (stg_decls:compute_stage) : compute_stage = 
	let decls = stg_decls in 
	let filter_f dec = 
		match dec with 
			| Action(_, obj_ids, next_ids) -> (
				match (CL.length obj_ids) with 
				| 0 -> (
					match (CL.length next_ids) with
						| 0 -> true (* action does nothing *)
						| _ -> error "[remove_noop_actions] unexpected: action has no objects but a successor table." 
				)
				| _ -> false
			)
			| _ -> false
	in 
	let empty_acns = CL.filter filter_f decls in 

	let fold_f_inner (empty_aid:Cid.t) (new_decls: decl list) (dec: decl) : decl list = 
		match dec with 
			(* *)
			| Action(aid, _, _) -> (
				match (Cid.equals empty_aid aid) with 
					(* add this action *)
					| false -> new_decls@[dec]
					(*don't add this action (use the global noop) *)
					| true -> new_decls
			)
			| Table(tid, rules, stg_opt) -> (
				let map_f rule = match rule with 
					| Match(rid, pat, aid) -> (
						match (Cid.equals aid empty_aid) with 
							| true -> Match(rid, pat, noop_aid)
							| false -> rule
					)
					| _ -> rule
				in 
				let rules = CL.map map_f rules in 
				(* add table with updated rules. *)
				new_decls@[Table(tid, rules, stg_opt)]
			)
			| _ -> new_decls@[dec]
	in 
	(* update decls for a single empty_aid *)
	let fold_f_outer (decls: decl list) (empty_aid : Cid.t) : decl list = 
		CL.fold_left (fold_f_inner empty_aid) [] decls
	in 
	let empty_aids = CL.map aid_of_acn empty_acns in 
	let updated_decls = CL.fold_left fold_f_outer decls empty_aids in

	updated_decls
;;


(* clean up the pipeline -- remove all the actions that do nothing with a noop *)
let remove_noop_actions (pipe:pipeline) : pipeline = 
	(* find all the actions that do nothing. *)
	let (vars, stages) = pipe in 
	let noop_aid = Cid.fresh ["noop"] in 
	let noop_acn = Action(noop_aid, [], []) in 			
	let stages = CL.map (remove_noops_from_stage noop_aid) stages in 
	match stages with 
		| [] -> pipe
		| fst_stg::remaining_stgs -> (
			(* place the noop action at the front of the first stage's compute decls *)
			vars, (noop_acn::fst_stg)::remaining_stgs
		)
;;

let print_tbl_stats stage_idx cid_decls tid = 
	let oids = oids_of_tid_opt cid_decls tid in 
	let tbl = Cid.lookup cid_decls tid in 
	let rules, fst_pat = match tbl with 
		| Table(_, rules, _) -> (
			rules, pat_of_rule (CL.hd rules)
		) 
		| _ -> [], []
	in 
	(* want number of rules *)
	let num_rules = CL.length rules in 
	let num_cols = CL.length fst_pat in 
	let num_prim_ops =  List.length oids in 
	let num_actions = List.length (unique_list_of (aids_of_tid cid_decls tid)) in 

	(* print table name, 
		total number of objects (i.e., primative statements), 
		number of columns in rule
		number of rules
	*)	
	let sline = 
		(P4tPrint.str_of_private_oid tid)^","^(string_of_int stage_idx)^","^
		(String.concat "," (CL.map string_of_int [num_actions; num_prim_ops; num_rules; num_cols])) in 
	!info_print_endline sline

let print_stage_stats stage_idx (s : compute_stage) = 
	(* a stage is a list of declarations. *)
	let local_cid_decls = dict_of_decls s in 
	(* get all the tables. *)
	let tids = tids_of_declmap local_cid_decls in 


	(* for each table, get all the salus that it uses. *)
	CL.iter (print_tbl_stats stage_idx local_cid_decls) tids;
	()
;;

let print_prog_stats (p:pipeline) = 
	let (_, stages) = p in 
	print_endline ("----- pipeline stats -----");
	!info_print_endline ("table_id,stage_num,num_actions,num_ops,num_rules,num_cols");
	CL.iteri (print_stage_stats) stages
;;


let do_passes (dag_prog:dagProg) layout = 
	DBG.start_mlog __FILE__ outc dprint_endline;
	DBG.start_mlog "pipe_stats.csv" info_outc info_print_endline;

	let (cid_decls, _, _) = dag_prog in 
    let out_prog_decls = snd (CL.split cid_decls) in 
    !dprint_endline ("---- struct defs at start of do_passes ---- ");
    !dprint_endline (P4tPrint.print_p4_struct_defs out_prog_decls);
    !dprint_endline ("---- struct defs at start of do_passes ---- ");

    !dprint_endline ("---- ingress before conversion to straightline  ---- ");
    !dprint_endline (P4tPrint.str_of_igr_objs out_prog_decls);
    !dprint_endline ("---- ingress before conversion to straightline  ---- ");



	DBG.printf outc "[PipelineGeneration.do_passes] got layout for %i stages\n" (CL.length layout);
	let pipe = pipeline_of_layout cid_decls layout
		|> remove_noop_actions 
	in 
	let _, pipe_stgs = pipe in 
	DBG.printf outc "[PipelineGeneration.do_passes] pipeline has %i stages\n" (CL.length pipe_stgs);
	DBG.printf outc ("----rebuilding dpa program----\n");


	let straightline_prog = tblseqprog_of_pipe pipe in 

    !dprint_endline ("---- ingress after conversion to straightline  ---- ");
    !dprint_endline (P4tPrint.str_of_igr_objs straightline_prog.tspdecls);
    !dprint_endline ("---- ingress after conversion to straightline  ---- ");

	(* let straightline_prog = dpa_prog_of_pipeline pipe in  *)
	verify_no_dup_decls straightline_prog;
(* 	log_p4_program (straightline_prog) "logs/ir/final_straightline_prog.p4"; *)
	(* information printing pass *)
	print_prog_stats pipe;


	straightline_prog
;;
