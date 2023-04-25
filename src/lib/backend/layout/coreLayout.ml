open Collections
open CoreSyntax
open TofinoCore
open MatchAlgebra
open CoreCfg
open CoreDfg
open CoreResources
(****  updated layout algorithm based on 
       a single topological traversal 
       of a statement group dag ****)

(* a statement group is a group of statements that should be scheduled together. *)
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
  print_endline ("[dfg_to_sgdfg] number of vertex_stmts in statement_groups before constructing dfg: "^(vertex_stmts_of_stmt_groups statement_groups |> List.length |> string_of_int));

  (* print_endline ("number of statement groups: "^(List.length (statement_groups) |> string_of_int)); *)
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
      (* print_endline ("number of preds:"^(List.length preds |> string_of_int)); *)
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
  let nodes = SgDfg.fold_vertex (fun v acc -> v :: acc) g [] in
  print_endline (
    "[dfg_to_sgdfg] number of statement group nodes in dfg: "^
    (nodes |> List.length |> string_of_int ));
  print_endline ("[dfg_to_sgdfg] number of vertex_stmts in dfg: "^(vertex_stmts_of_stmt_groups nodes |> List.length |> string_of_int));
  g
;;


(**** from CoreLayout.ml ****)
type layout_args = {
  num_nodes_to_place : int;
  dfg : SgDfg.t;
  arr_dimensions : (id * (int * int)) list;
}
;;

type table = {
  tkeys  : exp list;
  branches : branch list; (* the "rules" of the table *)
  stmt_groups : statement_group list;
  solitary : bool; 
}

type stage = {
  tables : table list;
}

type pipeline = {
  stages : stage list;
}

(* constructots *)
let empty_table = {tkeys =[]; branches = []; solitary = false; stmt_groups = [];}
let empty_stage = {tables = [];}
let empty_pipeline = {stages = [];}

let table_is_empty table =
  match table.stmt_groups with
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

let src_stmts_in_table t =
  List.map (fun sg -> sg.vertex_stmts) t.stmt_groups
    |> List.flatten
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
  ^", statement groups: "^(CL.length t.stmt_groups |> string_of_int)^")"
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
  "// #statement groups in table: "^(CL.length t.stmt_groups |> string_of_int)^"\n"^
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

let statement_groups_of_table table = table.stmt_groups
let statement_groups_of_stage s = List.map statement_groups_of_table s.tables |> List.flatten
;;
let statement_groups_of_stages ss = List.map statement_groups_of_stage ss |> List.flatten
;;

let vertices_of_table (t:table) = CL.map (fun sg -> sg.vertex_stmts) t.stmt_groups |> CL.flatten
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
let arrays_of_stage s = CL.map arrays_of_table s.tables |> CL.flatten |> (MatchAlgebra.unique_list_of_eq Cid.equal)

let hashers_of_table table =
  hashers_of_stmt (stmt_of_table table) |> unique_stmt_list
;;

let hashers_of_stage stage =
  CL.map hashers_of_table stage.tables |> CL.flatten |> unique_stmt_list
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

(* does the table fit in the target? *)
let table_fits table = 
  let c_stmts = (statements_of_table table |> CL.length) <= table_constraints.max_statements in
  let c_keywidth = (keywidth_of_table table) <= table_constraints.max_matchbits in
  let c_hashers = (hashers_of_table table |> CL.length) <= table_constraints.max_hashers in
  let c_arrays = (arrays_of_table table |> CL.length) <= table_constraints.max_arrays in

  if (
    c_stmts && c_keywidth && c_hashers && c_arrays
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
    print_endline "[table_fits] FAIL!";
  print_endline@@
    "[table_fits] c_stmts: "^(string_of_bool c_stmts)
    ^" c_keywidth: "^(string_of_bool c_keywidth)
    ^" c_hashers: "^(string_of_bool c_hashers)
    ^" c_arrays: "^(string_of_bool c_arrays)
    ;
  if (not c_arrays)
  then (
    print_endline (Printf.sprintf "arrays: [%s]"
      (arrays_of_table table |> CorePrinting.comma_sep CorePrinting.cid_to_string)
    )
  );
  false)
;;
(* does the stage fit in the target? *)
let stage_fits prog_info stage =
  let c_tbls = (CL.length stage.tables <= stage_constraints.max_tables) in
  let c_arrays = ((arrays_of_stage stage |> CL.length) <= stage_constraints.max_arrays) in
  let c_hashers = ((hashers_of_stage stage |> CL.length) <= stage_constraints.max_hashers) in 
  let n_blocks = sblocks_of_stmt prog_info.arr_dimensions (stmt_of_stage stage) in
  let c_blocks = (n_blocks <= stage_constraints.max_array_blocks) in
  (* print_endline@@"[stage_fits] c1: "^(string_of_bool c1)^" c2: "^(string_of_bool c2)^" c3: "^(string_of_bool c3); *)
  if ( c_tbls && c_arrays && c_hashers && c_blocks)
  then (
    (* print_endline "[stage_fits] TRUE";  *)
    true)
  else (
    (* print_endline "[stage_fits] FALSE";  *)
    false)
;;  


(*****  the new layout algorithm, based on statement groups *****)
let place_in_table stmt_group table =
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
      merge_matches 
      (stmt_of_table table)
      (List.map (fun s -> s.stmt) cond_stmts)
    in
    (* let tend = Unix.gettimeofday () in *)
    (* print_endline("[place_in_table] innermost table merge took "^(string_of_float (tend -. tstart))); *)
    match new_tbl_smatch.s with
    | SMatch(es, bs) ->
      Some({
        tkeys=es;
        branches=bs;
        solitary = table.solitary || contains_solitary;
        stmt_groups=table.stmt_groups@[stmt_group];
        })
    | _ -> error "[merge_into_table] merge matches didn't return a match statement. What?]" 
  )
  else (None)  
;;

(* check that the table created by merging stmts into table 
   will satisfy as many of the table constraints as possible. *)
let table_placement_precheck table (stmt_group:statement_group) = 
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
    (hashers_of_table table)
    @((List.map 
        (fun (v:vertex_stmt) -> v.stmt |> hashers_of_stmt)
          stmt_group.vertex_stmts)
      |> List.flatten)
    |> MatchAlgebra.unique_stmt_list
  in
  let arrays = (arrays_of_table table)@(stmt_group.arr |> Option.to_list) |> unique_list_of_eq Cid.equal in

  (not table.solitary) 
  && (List.length table.branches <= 100) (* put an upper bound on number of rules in table, for now.*)
  (* && ((List.length all_stmts) <= table_constraints.max_statements) *)
  && ((List.length hash_stmts) <= table_constraints.max_hashers)
  && ((List.length arrays) <= table_constraints.max_arrays)
;;

(* try to place the statement group into the table. *)
let try_place_in_table (prior_tables, stmt_group_opt) (table:table) =
(*   let tstart = Unix.gettimeofday () in *)
  let not_placed_result = (prior_tables@[table], stmt_group_opt) in 
  let result = match stmt_group_opt with 
  | None -> not_placed_result
  | Some(stmt_group) -> (
      (* does the table have room for this statement group? *)
      let precheck_success = table_placement_precheck table stmt_group in
      if (not (precheck_success))
      then (not_placed_result)
      else (        
        let new_table_opt = place_in_table stmt_group table in
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
    print_endline ("[try_place_in_table] took "^(tend -. tstart |> string_of_float)^(" and SUCCEEDED."))); *)
  result
;;



let table_fits table = 
  let c_stmts = (statements_of_table table |> CL.length) <= table_constraints.max_statements in
  let c_keywidth = (keywidth_of_table table) <= table_constraints.max_matchbits in
  let c_hashers = (hashers_of_table table |> CL.length) <= table_constraints.max_hashers in
  let c_arrays = (arrays_of_table table |> CL.length) <= table_constraints.max_arrays in

  if (
    c_stmts && c_keywidth && c_hashers && c_arrays
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
    print_endline "[table_fits] FAIL!";
  print_endline@@
    "[table_fits] c_stmts: "^(string_of_bool c_stmts)
    ^" c_keywidth: "^(string_of_bool c_keywidth)
    ^" c_hashers: "^(string_of_bool c_hashers)
    ^" c_arrays: "^(string_of_bool c_arrays)
    ;
  if (not c_arrays)
  then (
    print_endline (Printf.sprintf "arrays: [%s]"
      (arrays_of_table table |> CorePrinting.comma_sep CorePrinting.cid_to_string)
    )
  );
  false)
;;

let stage_fits (prog_info:layout_args) stage =
  let c_tbls = (CL.length stage.tables <= stage_constraints.max_tables) in
  let c_arrays = ((arrays_of_stage stage |> CL.length) <= stage_constraints.max_arrays) in
  let c_hashers = ((hashers_of_stage stage |> CL.length) <= stage_constraints.max_hashers) in 
  let n_blocks = sblocks_of_stmt prog_info.arr_dimensions (stmt_of_stage stage) in
  let c_blocks = (n_blocks <= stage_constraints.max_array_blocks) in
  (* print_endline@@"[stage_fits] c1: "^(string_of_bool c1)^" c2: "^(string_of_bool c2)^" c3: "^(string_of_bool c3); *)
  if ( c_tbls && c_arrays && c_hashers && c_blocks)
  then (
    (* print_endline "[stage_fits] TRUE";  *)
    true)
  else (
    (* print_endline "[stage_fits] FALSE";  *)
    false)
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

(* have all the dependencies of the 
   statement group been placed in stages? *)
let dependencies_ready prog_info stages stmt_group =
  let deps = SgDfg.pred prog_info.dfg stmt_group in
  let placed = statement_groups_of_stages stages in
  List.for_all (fun dep -> List.mem dep placed) deps
;;

let try_place_in_stage prog_info (prior_stages, stmt_group_opt) stage = 
  let not_placed_result = (prior_stages@[stage], stmt_group_opt) in 
  match stmt_group_opt with 
  | None -> not_placed_result (* already placed in a prior stage *)
  | Some(stmt_group) -> (
    (* make sure that all the dependencies are placed *)
    if (dependencies_ready prog_info prior_stages stmt_group)
    then (       
(*       print_endline ("[try_place_in_stage] attempting placement in stage...");
      let tstart = Unix.gettimeofday () in *)
      let updated_tables, pargs_opt = CL.fold_left 
        try_place_in_table 
        ([], Some(stmt_group))
        (stage.tables) (* place into tables in whatever order the tables are created *)
        (* (stage.tables |> sort_tables_by_nbranches)  *)
        (* try to place in tables with fewer branches first *)
      in
      (* if placement in all current tables fails, add a new table to the stage. *)
      let updated_tables = 
        if (placement_done pargs_opt)
        then (updated_tables)
        else (
          print_endline ("[try_place_in_stage] attempting to create new table in stage.");
          updated_tables@
          (
            match (place_in_table stmt_group empty_table) with 
          | None -> error "[try_place_in_stage] placement in this stage failed: no room in current tables, and no room for a new table.";
          | Some (tbl) -> [tbl]
          )
        )
      in
      let updated_stage = {tables = updated_tables;} in
      (* if the placement makes the stage full, placement fails.*)
      if (not (stage_fits prog_info updated_stage)) 
      then (
(*         let tend = Unix.gettimeofday() in
        let t = (tend -. tstart) in
        print_endline ("[try_place_in_stage] placement failed and took "^(string_of_float t)); *)
        not_placed_result)
      else (
(*         let tend = Unix.gettimeofday() in
        let t = (tend -. tstart) in
        print_endline ("[try_place_in_stage] placement succeeded and took "^(string_of_float t)); *)
        prior_stages@[updated_stage], None)
    )
  else ( not_placed_result )
  )
;;

(* 
  TODO: the new layout algorithm causes test applications to fail. 
        they also lay out to fewer stages... what's going on? 
        something with dependencies, maybe? *)

(* place a statement group into the pipe *)
let place_in_pipe prog_info stmt_group pipe : pipeline =
  (* print_endline ("[place_in_pipe] trying to place statement group: "^(stmt_group.sguid |> string_of_int)); *)
  print_endline 
    (Printf.sprintf 
    "placing IR statement %i / %i into current pipeline:" 
    ((List.length (src_stmts_in_pipe pipe) + 1))
    (prog_info.num_nodes_to_place));
  print_endline (summarystr_of_pipeline pipe);

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

let ensure_all_statements_placed dfg sgdfg =
  let num_dfg_nodes = Dfg.fold_vertex (fun v acc -> v :: acc) dfg [] |> List.length in
  let num_sg_dfg_nodes = SgDfg.fold_vertex (fun v acc -> v :: acc) sgdfg [] |> vertex_stmts_of_stmt_groups |> List.length in
  if (num_dfg_nodes <> num_sg_dfg_nodes)
  then (error "[coreLayout] some statements are missing from the statement group dependency graph.")
;;

let process tds dfg = 
  let num_dfg_nodes = Dfg.fold_vertex (fun v acc -> v :: acc) dfg [] |> List.length in
  print_endline ("number of nodes in dfg: "^(num_dfg_nodes |> string_of_int));
  let layout_args = {
    num_nodes_to_place = num_dfg_nodes;
    dfg = dfg_to_sgdfg dfg;
    arr_dimensions = array_dimensions tds;}
  in 
  ensure_all_statements_placed dfg layout_args.dfg;
  (* exit 1; *)
  let pipe = SgDfgTopo.fold (place_in_pipe layout_args) layout_args.dfg empty_pipeline in
  print_endline ("[coreLayoutNew] final pipeline");
  print_endline (summarystr_of_pipeline pipe);

  update_main tds {(main tds) with main_body=stmts_of_pipe pipe;}
;;


(* print the number of stages to a file in the build directory *)
let profile tds build_dir = 
  let num_stages = string_of_int ((main tds).main_body |> CL.length) in 
  let stages_fn = "num_stages.txt" in
  IoUtils.writef (build_dir ^ "/" ^ stages_fn) num_stages
;;
