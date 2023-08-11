open Collections
open CoreSyntax
open TofinoCore
open MatchAlgebra
open CoreCfg
open CoreDfg
open CoreResources
(****  greedy layout algorithm based on 
       a single topological traversal 
       of a statement group data dependency dag. ****)

let debug_print_endline str = 
  if (Cmdline.cfg.debug)
  then print_endline str
;;

(* a statement group is a group of statements that must be placed in the same stage
   (because the access the same array) *)
type statement_group = {
  sguid : int;
  (* vertices to place *)
  vertex_stmts : vertex_stmt list;
  (* array that the statements uses. *)
  arr : Cid.t option;
}
let cur_uid = ref 0 ;;

let atomic_statement_group s =
  cur_uid := (!cur_uid) + 1;
  {
    sguid = (!cur_uid);
    vertex_stmts = [s];
    arr = None;
  }
let array_statement_group s arr =
  cur_uid := (!cur_uid) + 1;
  {
  sguid = (!cur_uid);
    vertex_stmts = [s];
    arr = Some(arr);
  }
;;

let vertex_stmts_of_stmt_groups stmt_groups =
  let vertex_stmts = List.map (fun sg -> sg.vertex_stmts) stmt_groups in
  List.concat vertex_stmts
;;

module SgDfgNode = struct
  type t = statement_group
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = ( = )
end

module SgDfg = Graph.Persistent.Digraph.Concrete(SgDfgNode)
module SgDfgTopo = Graph.Topological.Make(SgDfg)

(*** mapping arrays <--> caller statements ***)
type array_users_map = (vertex_stmt list) CidMap.t
;;

let vertex_to_arrays v = arrays_of_stmt v.stmt 
let array_to_vertexes (arrmap:array_users_map) arrid = CidMap.find arrid arrmap
;;

let build_array_map dfg : array_users_map =
  let update_array_map vertex m = 
    let update_for_array m arr_cid = 
      (* there's already an entry for arr_cid *)
      match CidMap.mem arr_cid m with 
      | true -> (
        let new_users = (CidMap.find arr_cid m)@[vertex] in
        CidMap.add 
          arr_cid 
          new_users
          (CidMap.remove arr_cid m)
      )
      | false -> (
        CidMap.add arr_cid [vertex] m
      )      
    in 
    CL.fold_left update_for_array m (vertex_to_arrays vertex)
  in 
  Dfg.fold_vertex update_array_map dfg CidMap.empty
;;

(* convert a data dependency graph of statements 
   into a data dependency graph of statement groups *)
let dfg_to_sgdfg dfg =
  let array_to_vertices_map = build_array_map dfg in
  let _ = array_to_vertices_map in

  (* (array_id * statement_group) list *)
  let (atomic_statement_groups : statement_group list ref) = ref [] in
  let (array_statement_groups : (Cid.t * statement_group) list ref) = ref [] in
  (* id of a statement vertex to id of its group *)
  let vertex_id_to_group_id = ref [] in

  Dfg.iter_vertex
    (fun v -> 
      match (vertex_to_arrays v) with
      (* no arrays, make atomic group *)
      | [] -> 
        let new_group = atomic_statement_group v in 
        vertex_id_to_group_id := (v.vuid, new_group.sguid)::(!vertex_id_to_group_id);
        atomic_statement_groups := (!atomic_statement_groups)@[new_group];
      (* uses array, merge into existing array group or make new *)
      | [arr_cid] -> (
        match (List.assoc_opt arr_cid (!array_statement_groups)) with
        | Some(cur_group) -> 
          let new_group = {cur_group with vertex_stmts = cur_group.vertex_stmts@[v]} in
          vertex_id_to_group_id := (v.vuid, new_group.sguid)::(!vertex_id_to_group_id);
          array_statement_groups := 
            (List.remove_assoc arr_cid (!array_statement_groups))@[arr_cid, new_group];
        | None -> 
          let new_group = array_statement_group v arr_cid in
          vertex_id_to_group_id := (v.vuid, new_group.sguid)::(!vertex_id_to_group_id);
          array_statement_groups := (!array_statement_groups)@[arr_cid, new_group];)
      | _ -> error "[dfg_to_sgdfg] a statement in the dependency graph accesses more than one array.")
    dfg;
  (* got all the statement groups, i.e., nodes. Now fill in the edges? *)
  let statement_groups = (!atomic_statement_groups)@(!array_statement_groups |> List.split |> snd) in
  (* debug_print_endline ("[dfg_to_sgdfg] number of vertex_stmts in statement_groups before constructing dfg: "^(vertex_stmts_of_stmt_groups statement_groups |> List.length |> string_of_int)); *)

  (* debug_print_endline ("number of statement groups: "^(List.length (statement_groups) |> string_of_int)); *)
  (* sguid -> sg *)
  let sg_assoc = List.map (fun sg -> (sg.sguid, sg)) statement_groups in
  let add_edges_from_ts_to_s g s ts =
    let add_edge_from_t_to_s g t = SgDfg.add_edge g t s in
    List.fold_left add_edge_from_t_to_s g ts
  in
  (* you are adding edges, but what about the nodes that don't have any edges? That happens too... *)
  let sgdfg_with_nodes =
    List.fold_left (fun acc sg -> SgDfg.add_vertex acc sg) SgDfg.empty statement_groups 
  in
  let g = List.fold_left
    (* add edges from each statement group to 
       all of its unique predecessor statement groups *)
    (fun g stmt_group ->
      let preds = preds_of_vertices dfg stmt_group.vertex_stmts in
      (* debug_print_endline ("number of preds:"^(List.length preds |> string_of_int)); *)
      let pred_stmt_groups = List.map 
          (fun stmt_v -> List.assoc (stmt_v.vuid) (!vertex_id_to_group_id))
          preds
        |> (List.sort_uniq Stdlib.compare) 
        |> List.map (fun sguid -> List.assoc sguid sg_assoc)
      in
      add_edges_from_ts_to_s g stmt_group pred_stmt_groups)
    sgdfg_with_nodes
    statement_groups
  in
  (* let nodes = SgDfg.fold_vertex (fun v acc -> v :: acc) g [] in *)
(*   debug_print_endline (
    "[dfg_to_sgdfg] number of statement group nodes in dfg: "^
    (nodes |> List.length |> string_of_int ));
  debug_print_endline ("[dfg_to_sgdfg] number of vertex_stmts in dfg: "^(vertex_stmts_of_stmt_groups nodes |> List.length |> string_of_int)); *)
  g
;;


type layout_args = {
  num_nodes_to_place : int;
  dfg : SgDfg.t;
  arr_dimensions : (id * (int * int)) list;
}
;;

type table = {
  tkeys  : exp list;
  branches : branch list; (* the "rules" of the table *)
  solitary : bool; 
  stmts : vertex_stmt list;}

type stage = {
  tables : table list;
}

type pipeline = {
  stages : stage list;
}

(* constructots *)
let empty_table = {tkeys =[]; branches = []; solitary = false; stmts = [];}
let empty_stage = {tables = [];}
let empty_pipeline = {stages = [];}

let table_is_empty table =
  match table.stmts with
  | [] -> true
  | _ -> false
;;

(* deconstructors *)
let stmt_of_table t = smatch t.tkeys t.branches
  (* smatch_of_pattern_branches t.rules; *)
;;
let stmt_of_stage s =  
  InterpHelpers.fold_stmts (List.map stmt_of_table s.tables) 
;;
let tbls_in_pipe p =
  List.map (fun stage -> stage.tables) p.stages |> List.flatten
;;

let src_stmts_in_table t = t.stmts
  (* List.map (fun sg -> sg.vertex_stmts) t.stmts
    |> List.flatten *)
;;
let src_stmts_in_stage s =
  List.map src_stmts_in_table s.tables |> List.flatten
;;
let src_stmts_in_pipe p =
  List.map src_stmts_in_stage p.stages |> List.flatten
;;

let stmts_of_pipe p = 
  CL.map stmt_of_stage p.stages
;;

(* information printers *)
let summarystr_of_table t = 
  "(branches: "^(CL.length t.branches |> string_of_int)
  ^", IR statements: "^(CL.length (src_stmts_in_table t) |> string_of_int)
  ^", statements: "^(CL.length t.stmts |> string_of_int)^")"
;;

let summarystr_of_stage s = 
  Printf.sprintf "%i tables: [%s]"
    (List.length s.tables)
    (List.map (summarystr_of_table) s.tables |> String.concat ",")
;;

let summarystr_of_pipeline p = 
  (Printf.sprintf "--- %i IR statements in %i physical tables across %i stages ---\n"
    (List.length (src_stmts_in_pipe p))
    (List.length (tbls_in_pipe p))
    (List.length p.stages))^
  ((List.mapi (fun i s -> 
    ("stage "^(string_of_int (i))^" -- "^(summarystr_of_stage s))
  )
  p.stages)
  |> String.concat "\n")
;;

(* code printers *)
let string_of_rules rs = (smatch_of_pattern_branches rs
  |> CorePrinting.statement_to_string)

let string_of_table t =
  "// #branches/rules in table: "^(CL.length t.branches |> string_of_int)^"\n"^
  "// #statements in table: "^(CL.length t.stmts |> string_of_int)^"\n"^
  (CorePrinting.statement_to_string (stmt_of_table t))
;;

let string_of_stage s =
  (CL.mapi 
      (fun i tbl -> "// ------ TABLE "^(string_of_int i)^"------\n"^(string_of_table tbl))
      s.tables |> String.concat "\n"
    )
;;

let string_of_pipe p =
  (CL.mapi 
      (fun i s -> "// ------ STAGE "^(string_of_int i)^"------\n"^(string_of_stage s))
      p.stages |> String.concat "\n"
    )
;;

(* accessors *)
let statements_of_table table = 
  CL.map (fun (_, bstmt) -> bstmt) table.branches |> MatchAlgebra.unique_stmt_list

let statements_of_table table = table.stmts
let statements_of_stage s = List.map statements_of_table s.tables |> List.flatten
;;
let statements_of_stages ss = List.map statements_of_stage ss |> List.flatten
;;

let vertices_of_table (t:table) = t.stmts
let vertices_of_stage s = CL.map vertices_of_table s.tables |> CL.flatten
let vertices_of_stages ss = CL.map vertices_of_stage ss |> CL.flatten
let vertices_of_pipe  p = vertices_of_stages p.stages
;;

(*** resource usage constraints ***)
type constraints_table = {
  max_statements : int;
  max_matchbits : int;  
  max_arrays : int;
  max_hashers : int;
  max_array_addrs : int; (* how many expressions can be used as array addresses? *)
}
type constraints_stage = {
  max_tables : int;
  max_arrays : int;
  max_hashers : int;
  max_array_blocks : int;
}
(* default config for tofino -- change for other architectures *)
let table_constraints = {
  max_statements = 25;
  max_matchbits = 512;
  max_arrays = 1;
  max_hashers = 1;
  max_array_addrs = 1;
}

let stage_constraints = {
  max_tables = 16;
  max_arrays = 4;
  max_hashers = 6;
  max_array_blocks = 48;
}
(* calculate resources used by program components *)
let arrays_of_vertex v = arrays_of_stmt v.stmt

let arrays_of_table (t:table) = arrays_of_stmt (stmt_of_table t)
let hashers_of_table table =
  hashers_of_stmt (stmt_of_table table) |> unique_stmt_list
;;
let keywidth_of_table table = 
  let width_of_exp exp =
    match exp.ety.raw_ty with
    | TBool -> 1
    | TInt(sz) -> sz
    | _ -> error "[keywidth_of_table] reached table key that is not bool or int..."
  in
  CL.fold_left (+) 0 (CL.map width_of_exp table.tkeys)
;;

let array_addrs_of_table (t:table) = 
  stmt_of_table t |> CoreResources.array_addrs_of_stmt
;;
(* does the table fit in the target? *)
let table_fits table = 
  let c_stmts = (statements_of_table table |> CL.length) <= table_constraints.max_statements in
  let c_keywidth = (keywidth_of_table table) <= table_constraints.max_matchbits in
  let c_hashers = (hashers_of_table table |> CL.length) <= table_constraints.max_hashers in
  let c_arrays = (arrays_of_table table |> CL.length) <= table_constraints.max_arrays in
  let c_addrs = (array_addrs_of_table table |> CL.length) <= table_constraints.max_array_addrs in
  if (
    c_stmts && c_keywidth && c_hashers && c_arrays && c_addrs
(*     ((statements_of_table table |> CL.length) <= table_constraints.max_statements)
    && 
    ((keywidth_of_table table) <= table_constraints.max_matchbits)
    && 
    ((hashers_of_table table |> CL.length) <= table_constraints.max_hashers)
    && 
    ((arrays_of_table table |> CL.length) <= table_constraints.max_arrays) *)
  )
  then (true)
  else (
    debug_print_endline "[table_fits] FAIL!";
  debug_print_endline@@
    "[table_fits] c_stmts: "^(string_of_bool c_stmts)
    ^" c_keywidth: "^(string_of_bool c_keywidth)
    ^" c_hashers: "^(string_of_bool c_hashers)
    ^" c_arrays: "^(string_of_bool c_arrays)
    ^" c_addrs: "^(string_of_bool c_addrs)
    ;
  if (not c_arrays)
  then (
    debug_print_endline (Printf.sprintf "arrays: [%s]"
      (arrays_of_table table |> CorePrinting.comma_sep CorePrinting.cid_to_string)
    )
  );
  false)
;;

(* does the stage fit in the target? *)
let arrays_of_stage s = CL.map arrays_of_table s.tables |> CL.flatten |> (MatchAlgebra.unique_list_of_eq Cid.equal)

let hashers_of_stage stage =
  CL.map hashers_of_table stage.tables |> CL.flatten |> unique_stmt_list
;;

let string_of_statement_group stmt_group = 
  let string_of_vertex_statement (vs : vertex_stmt) =
    CorePrinting.statement_to_string vs.stmt
  in 
  String.concat "\n" (List.map string_of_vertex_statement stmt_group.vertex_stmts)
;;

(*****  the new layout algorithm, based on statement groups *****)
(* place_in_table operates on a stmt, not a stmt_group, 
   because stmt_groups have to be in the same stage -- not 
   necessarily the same table. *)
let place_in_table (vertex_stmt: vertex_stmt) table = 
  let can_proceed = match(vertex_stmt.solitary, table_is_empty table, table.solitary) with 
    | true, true, false -> true (* solitary into an empty table *)
    | true, _, _ -> false       (* solitary into anything else *)
    | _, _, true -> false       (* anything into a solitary *)
    | false, _, false -> true   (* non solitary into non-solitary *)
  in 
  if can_proceed then (
    (* construct the new table. *)
    let new_tbl_smatch = merge_matches (stmt_of_table table) (vertex_stmt.stmt) in
    match new_tbl_smatch.s with
    | SMatch(es, bs) ->
     let tbl = {
        tkeys=es;
        branches=bs;
        solitary = table.solitary || vertex_stmt.solitary;
        stmts=table.stmts@[vertex_stmt];
        }
      in
      Some(tbl)
    | _ -> error "[merge_into_table] merge matches didn't return a match statement. What?]" 
  )
  else (None)  
;;
let old_place_in_table stmt_group table =
  let cond_stmts = stmt_group.vertex_stmts in
  (* first, figure out if the merge is possible depending on whether the 
     conditional statement or table is a user-defined table *)
  let contains_solitary = List.fold_left 
    (fun acc (s:vertex_stmt) -> acc || s.solitary) 
    false 
    cond_stmts 
  in
  let can_proceed = match (contains_solitary, table_is_empty table, table.solitary) with 
    | true, true, false -> true (* solitary into an empty table *)
    | true, _, _ -> false       (* solitary into anything else *)
    | _, _, true -> false       (* anything into a solitary *)
    | false, _, false -> true   (* non solitary into non-solitary *)
  in 
  if (can_proceed) then (
    (* let tstart = Unix.gettimeofday () in *)

    (* fold each conditional statement into the table *)
    let new_tbl_smatch = List.fold_left 
      (fun m1 m2 -> merge_matches m1 m2 (* |> delete_redundant_branches *))
      (stmt_of_table table)
      (List.map (fun s -> s.stmt) cond_stmts)
    in
    (* let tend = Unix.gettimeofday () in *)
    (* debug_print_endline("[place_in_table] innermost table merge took "^(string_of_float (tend -. tstart))); *)
    match new_tbl_smatch.s with
    | SMatch(es, bs) ->
     let tbl = {
        tkeys=es;
        branches=bs;
        solitary = table.solitary || contains_solitary;
        stmts=table.stmts@stmt_group.vertex_stmts;
        }
      in
    (*       print_endline ("-----------[place_in_table] report------------");
      print_endline ("input table:\n"^(string_of_table table));
      print_endline ("input stmt group:\n"^(string_of_statement_group stmt_group));
      print_endline ("output table:\n"^(string_of_table tbl));
      print_endline ("-----------[place_in_table] report------------"); *)
      Some(tbl)
    | _ -> error "[merge_into_table] merge matches didn't return a match statement. What?]" 
  )
  else (None)  
;;

(* check that the table created by merging stmts into table 
   will satisfy as many of the table constraints as possible. *)
let table_placement_precheck table (stmt:vertex_stmt) = 
  (* note: not sure what the right metric for statements should be, 
     (unique statements or not? 
     based on source statements or what is in the table branches?) *)
(*   let all_stmts = 
     (statements_of_table table)
    @(List.map (fun vs -> vs.stmt) stmt_group.vertex_bundle)
    |> MatchAlgebra.unique_stmt_list  
  in  *)
  (* estimating the keywidth is a bit more complex, can add if needed. *)
  (* let c_keywidth = (keywidth_of_table table) + ... in *)
  let hash_stmts = 
    (hashers_of_table table)@(hashers_of_stmt stmt.stmt)
    |> MatchAlgebra.unique_stmt_list
  in
  let arrays = (arrays_of_table table)@(arrays_of_stmt stmt.stmt) |> unique_list_of_eq Cid.equal in
  (not table.solitary) 
  && (List.length table.branches <= 100) (* put an upper bound on number of rules in table, for now.*)
  (* && ((List.length all_stmts) <= table_constraints.max_statements) *)
  && ((List.length hash_stmts) <= table_constraints.max_hashers)
  && ((List.length arrays) <= table_constraints.max_arrays)
;;

(* try to place the statement group into the table. *)
let try_place_in_table (prior_tables, (stmt_opt:vertex_stmt option)) (table:table) =
(*   let tstart = Unix.gettimeofday () in *)
  let not_placed_result = (prior_tables@[table], stmt_opt) in 
  let result = match stmt_opt with 
  | None -> not_placed_result
  | Some(stmt) -> (
      (* does the table have room for this statement group? *)
      let precheck_success = table_placement_precheck table stmt in
      if (not (precheck_success))
      then (not_placed_result)
      else (        
        let new_table_opt = place_in_table stmt table in
        (* table placement may still fail, because 
           placement precheck doesn't check everything. *)
        match new_table_opt with 
        | None -> not_placed_result
        | Some(new_table) -> (
          (* check to make sure resulting table is not too big *)
          if (not (table_fits new_table))
          then (not_placed_result)
          (* if not too big, return the new table and empty placement request *)
          else ((prior_tables@[new_table], None))
        )
      )
  )
  in 
(*   let tend = Unix.gettimeofday () in
  if (success) then (
    debug_print_endline ("[try_place_in_table] took "^(tend -. tstart |> string_of_float)^(" and SUCCEEDED."))); *)
  result
;;

let stage_fits_verbose (prog_info:layout_args) stage =
  let c_tbls = (CL.length stage.tables <= stage_constraints.max_tables) in
  let c_arrays = ((arrays_of_stage stage |> CL.length) <= stage_constraints.max_arrays) in
  let c_hashers = ((hashers_of_stage stage |> CL.length) <= stage_constraints.max_hashers) in 
  let n_blocks = sblocks_of_stmt prog_info.arr_dimensions (stmt_of_stage stage) in
  let c_blocks = (n_blocks <= stage_constraints.max_array_blocks) in
  (c_tbls, c_arrays, c_hashers, c_blocks)
;;  


let stage_fits (prog_info:layout_args) stage =
  let a, b, c, d = stage_fits_verbose prog_info stage in
  a && b && c && d
;;  


let placement_done pargs_opt = 
  match pargs_opt with 
  | None -> true
  | Some _ -> false
;;

(* sort tables by number of branches, 
   so that we attempt to merge into tables with 
   fewer branches first.  *)
let sort_tables_by_nbranches tbls =
  List.sort (fun t1 t2 -> (List.length t1.branches) - (List.length t2.branches)) tbls
;;

let unplaced_dependencies prog_info stages stmt_group = 
  let deps = SgDfg.pred prog_info.dfg stmt_group |> List.fold_left (fun ss stmt_group -> ss@stmt_group.vertex_stmts) [] in
  let placed = statements_of_stages stages in
  List.filter (fun dep -> not (List.mem dep placed)) deps
;;
(* have all the dependencies of the 
   statement group been placed in stages? *)
let dependencies_ready prog_info stages stmt_group =
  (* deps are all the statements in the predecessor statement groups. *)
  let deps = SgDfg.pred prog_info.dfg stmt_group |> List.fold_left (fun ss stmt_group -> ss@stmt_group.vertex_stmts) [] in
  let placed = statements_of_stages stages in
  List.for_all (fun dep -> List.mem dep placed) deps
;;


(* place a single statement into the tables in a stage.
   Always succeed, by adding a new table if necessary.*)
let rec place_in_tables stmt tables =   
  match tables with  
  (* if we reach the end of the tables without 
      placing, add a new table *)
  | [] -> (
    match (place_in_table stmt empty_table) with
    | Some(table') -> [table']
    | None -> error "[coreLayout.place_one_in_current] could not place statement into an empty table... should be impossible."
  )
  | table::tables -> (
    if (table_placement_precheck table stmt)
    then (
      match place_in_table stmt table with
      | Some(table') -> (
        (* make sure the generated table isn't too big *)
        if (table_fits table')
          then 
            (table'::tables)
          else 
            (table::(place_in_tables stmt tables))
      )
      | None -> table::(place_in_tables stmt tables))
    (* precheck failed, no room in table *)
    else (
      table::(place_in_tables stmt tables))
  )
;;
    
let try_place_in_stage prog_info (prior_stages, stmt_group_opt) stage = 
  let not_placed_result reason = 
    match stmt_group_opt with
    | Some(stmt_group) -> (
      (match stmt_group.arr with
      | Some(arr) -> (
        let stage_num = List.length prior_stages in      
        print_endline 
          ("[try_place_in_stage] failed to place array "
          ^(CorePrinting.cid_to_string arr)
          ^" in stage "^string_of_int stage_num^"."
          ^" Reason: "^reason^".");
        (* if (reason = "dependencies")          
          then (
            print_endline "---Unplaced dependencies---";
            let deps = unplaced_dependencies prog_info prior_stages stmt_group in
            List.iter (fun dep -> print_endline (CorePrinting.statement_to_string dep.stmt)) deps;
            print_endline "-----------------------------";
          ) *)
      )
      | _ -> ());
      (prior_stages@[stage], stmt_group_opt) 
    )
    | _ -> 
      (prior_stages@[stage], stmt_group_opt) 
  in 
  match stmt_group_opt with 
  | None -> not_placed_result "already placed" (* already placed in a prior stage *)
  | Some(stmt_group) -> (
    (* make sure that all the dependencies are placed for all the stmts in this group *)
    if (dependencies_ready prog_info prior_stages stmt_group)
    then (
      (* place each statement from this group into a table from this stage. *)
      let tables' = List.fold_left
        (fun tables' stmt -> 
          place_in_tables stmt tables')
        stage.tables
        stmt_group.vertex_stmts
      in
      let updated_stage = {tables = tables';} in
      if (not (stage_fits prog_info updated_stage)) 
        then (
          let (c_tbls, c_arrays, c_hashers, c_blocks) = stage_fits_verbose prog_info updated_stage in
          let dbg_msg = 
            Printf.sprintf 
              "stage %i would be too big: c_tbls=%b, c_arrays=%b, c_hashers=%b, c_blocks=%b"
              (List.length prior_stages)
              c_tbls c_arrays c_hashers c_blocks
          in
          not_placed_result dbg_msg)
        else (prior_stages@[updated_stage], None)
    )
    else ( not_placed_result "dependencies" )
  )
;;
(* <<TODO:refactor>> replace try_place_in_stage with a recursive place_in_stages, 
                     similar to place_in_tables *)

(* place a statement group into the pipe *)
let place_in_pipe prog_info stmt_group pipe : pipeline =
  (* debug_print_endline ("[place_in_pipe] trying to place statement group: "^(stmt_group.sguid |> string_of_int)); *)
  if (Cmdline.cfg.debug)
  then (
    debug_print_endline 
      (Printf.sprintf 
      "placing IR statement %i / %i into current pipeline:" 
      ((List.length (src_stmts_in_pipe pipe) + 1))
      (prog_info.num_nodes_to_place));
    debug_print_endline (summarystr_of_pipeline pipe);
  )
  else (
    print_string "."; flush stdout;
  );
  (* try to place in existing stage *)
  let updated_stages, stmt_group_opt = CL.fold_left 
    (try_place_in_stage prog_info)
    ([], Some(stmt_group))
    pipe.stages
  in 
  if (placement_done stmt_group_opt) 
    then ({stages = updated_stages;})
    else (
      (* could not place in current stages: place in a new stage at the end *)
      let updated_stages, _ = try_place_in_stage prog_info
        (updated_stages, stmt_group_opt) 
        empty_stage
      in 
      (* at this point, we can assume that the placement had to succeed *)
      ({stages = updated_stages;}))
;;

(* lay out the program tds given the data dependency graph dfg. 
   This optimized pass converts the dfg into a dfg of statement 
   groups, then does a single topological 
   traversal of the statement group dfg and inserts one at a time. 
   Since we traverse statement groups, we are guaranteed that 
   all the dependencies of node v have been added before v. 
 *)

let ensure_all_statements_in_groupdfg dfg sgdfg =
  let num_dfg_nodes = Dfg.fold_vertex (fun v acc -> v :: acc) dfg [] |> List.length in
  let num_sg_dfg_nodes = SgDfg.fold_vertex (fun v acc -> v :: acc) sgdfg [] |> vertex_stmts_of_stmt_groups |> List.length in
  if (num_dfg_nodes <> num_sg_dfg_nodes)
  then (error "[coreLayout] some statements are missing from the statement group dependency graph.")
;;

let process tds dfg = 
  let num_dfg_nodes = Dfg.fold_vertex (fun v acc -> v :: acc) dfg [] |> List.length in
  debug_print_endline ("number of nodes in dfg: "^(num_dfg_nodes |> string_of_int));
  let layout_args = {
    num_nodes_to_place = num_dfg_nodes;
    dfg = dfg_to_sgdfg dfg;
    arr_dimensions = array_dimensions tds;}
  in 
  ensure_all_statements_in_groupdfg dfg layout_args.dfg;
  (* exit 1; *)
  print_endline ("placing "^(string_of_int num_dfg_nodes)^(" atomic statement groups into pipeline"));
  let pipe = SgDfgTopo.fold (place_in_pipe layout_args) layout_args.dfg empty_pipeline in
  print_endline ("[coreLayout] final pipeline");
  print_endline (summarystr_of_pipeline pipe);

  update_main tds {(main tds) with main_body=stmts_of_pipe pipe;}
;;


(* print the number of stages to a file in the build directory *)
let profile tds build_dir = 
  let num_stages = string_of_int ((main tds).main_body |> CL.length) in 
  let stages_fn = "num_stages.txt" in
  IoUtils.writef (build_dir ^ "/" ^ stages_fn) num_stages
;;

open TofinoCoreNew


let process_new (tds : tdecls) dfg =
  let num_dfg_nodes = Dfg.fold_vertex (fun v acc -> v :: acc) dfg [] |> List.length in
  debug_print_endline ("number of nodes in dfg: "^(num_dfg_nodes |> string_of_int));
  let layout_args = {
    num_nodes_to_place = num_dfg_nodes;
    dfg = dfg_to_sgdfg dfg;
    arr_dimensions = array_dimensions tds;}
  in 
  ensure_all_statements_in_groupdfg dfg layout_args.dfg;

  print_endline ("[coreLayout] placing "^(string_of_int num_dfg_nodes)^(" atomic statement groups into pipeline"));
  let pipe = SgDfgTopo.fold (place_in_pipe layout_args) layout_args.dfg empty_pipeline in
  print_endline("");
  print_endline ("[coreLayout] final pipeline");
  print_endline (summarystr_of_pipeline pipe);
  let pipe_stmts = stmts_of_pipe pipe in
  
  pipe_stmts 
;;

