(* CoreLayout -- lay out the data flow graph in a pipeline of tables *)
(* This is a depreciated, older version of the layout algorithm 
   that is just here until we have high confidence in the 
   refactored implementation. *)
(* this operates on a data flow graph of match statements, 
   some of which are solitary and must be mapped to their own table... *)

open Collections
open CoreSyntax
open TofinoCore
open MatchAlgebra
open CoreCfg
open CoreDfg
open CoreResources

(**** A simple model of the hardware ****)

(* a table is just a match statement annotated with 
   a list of source statements (for debugging)
   and a "solitary" flag, which means no more can be added to it. *)
type table = {
  tkeys  : exp list;
  branches : branch list; (* the "rules" of the table *)
  sources : CoreCfg.vertex_stmt list;
  solitary : bool; 
}

type stage = {
  tables : table list;
}

type pipeline = {
  stages : stage list;
}


(* program information used by the placement loop *)
type prog_info = {
  dfg : CoreDfg.Dfg.t;
  arr_users : vertex_stmt list CidMap.t;
  arr_dimensions : (id * (int * int)) list;
}
;;


(* initializers *)
let empty_table = {tkeys =[]; branches = []; solitary = false; sources = [];}
let empty_stage = {tables = [];}
let empty_pipeline = {stages = [];}

let table_is_empty table =
  match table.sources with
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
let stmts_of_pipe p = 
  CL.map stmt_of_stage p.stages
;;

(* printers *)
let summarystr_of_table t = 
  "(branches: "^(CL.length t.branches |> string_of_int)^" IR statements: "^(CL.length t.sources |> string_of_int)^")"
;;

let summarystr_of_stage s = 
  Printf.sprintf "%i tables: [%s]"
    (List.length s.tables)
    (List.map (summarystr_of_table) s.tables |> String.concat ",")
;;

let tbls_in_pipe p =
  List.map (fun stage -> stage.tables) p.stages |> List.flatten
;;

let src_stmts_in_stage s =
  List.map (fun tbl -> tbl.sources) s.tables |> List.flatten
;;
let src_stmts_in_pipe p =
  List.map src_stmts_in_stage p.stages |> List.flatten
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

(* 
let summarystr_of_stage s = ("number of tables: "^((CL.length s.tables) |> string_of_int))


let summarystr_of_pipeline p = 
  (List.mapi (fun i s -> 
    ("stage "^(string_of_int (i))^" tables: "^(summarystr_of_stage s))
  )
  p.stages)
  |> String.concat "\n"
 *)
let string_of_rules rs = (smatch_of_pattern_branches rs
  |> CorePrinting.statement_to_string)

let string_of_table t =
  "// #branches in table: "^(CL.length t.branches |> string_of_int)^"\n"^
  "// #source statements in table: "^(CL.length t.sources |> string_of_int)^"\n"^
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

let vertices_of_table (t:table) = t.sources
let vertices_of_stage s = CL.map vertices_of_table s.tables |> CL.flatten
let vertices_of_stages ss = CL.map vertices_of_stage ss |> CL.flatten
let vertices_of_pipe  p = vertices_of_stages p.stages
;;

(*** resource constraints ***)
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

let table_fits table = 
  if (
    ((statements_of_table table |> CL.length) <= table_constraints.max_statements)
    && 
    ((keywidth_of_table table) <= table_constraints.max_matchbits)
    && 
    ((hashers_of_table table |> CL.length) <= table_constraints.max_hashers)
    && 
    ((arrays_of_table table |> CL.length) <= table_constraints.max_arrays)
  )
  then (true)
  else (false)
;;

let stage_fits prog_info stage =
  let c_tbls = (CL.length stage.tables <= stage_constraints.max_tables) in
  let c_arrays = ((arrays_of_stage stage |> CL.length) <= stage_constraints.max_arrays) in
  let c_hashers = ((hashers_of_stage stage |> CL.length) <= stage_constraints.max_hashers) in 
  let n_blocks = sblocks_of_stmt prog_info.arr_dimensions (stmt_of_stage stage) in
  let c_blocks = (n_blocks <= stage_constraints.max_array_blocks) in
  (* left off here. Check additional constraint: all the arrays 
     that the stage uses fit into the stage's memory *)
  (* print_endline@@"[stage_fits] c1: "^(string_of_bool c1)^" c2: "^(string_of_bool c2)^" c3: "^(string_of_bool c3); *)
  if ( c_tbls && c_arrays && c_hashers && c_blocks)
  then (
    (* print_endline "[stage_fits] TRUE";  *)
    true)
  else (
    (* print_endline "[stage_fits] FALSE";  *)
    false)
;;  

(*** statement scheduling / placement ***)

(*** core placement method: 
    merge a list of match statements that must be placed in the same stage 
    into a table. ***)
let place_in_table cond_stmts table =
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
    (* fold each conditional statement into the table *)
    let new_tbl_smatch = List.fold_left 
      merge_matches 
      (stmt_of_table table)
      (List.map (fun s -> s.stmt) cond_stmts)
    in
    match new_tbl_smatch.s with
    | SMatch(es, bs) ->
      Some({
        tkeys=es;
        branches=bs;
        solitary = table.solitary || contains_solitary;
        sources=cond_stmts@table.sources;
        })
    | _ -> error "[merge_into_table] merge matches didn't return a match statement. What?]" 
  )
  else (None)  


;;

(* are all of these vertices placed in 
   one of the stages? *)
let vertices_placed vertices stages =
(*   print_endline@@"[vertices_placed] checking if "
    ^(CL.length vertices |> string_of_int)
    ^" vertices are placed in "
    ^(CL.length stages |> string_of_int)
    ^" prior stages"; *)
  let placed_vs = vertices_of_stages stages in
  let vertex_placed_acc prev vertex =
    prev && (CL.mem vertex placed_vs)
  in 
  let res = CL.fold_left vertex_placed_acc true vertices in 
  (* print_endline@@"[vertices_placed] result: "^(string_of_bool res); *)
  res
;;



(*** map from arrays to caller statements ***)
type array_users_map = (vertex_stmt list) CidMap.t
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
    CL.fold_left update_for_array m (arrays_of_vertex vertex)
  in 
  Dfg.fold_vertex update_array_map dfg CidMap.empty
;;

(* the main helpers for array stuff 
   node -> array and array -> node *)
let vertices_of_array arrmap arrid = 
  CidMap.find arrid arrmap
;;

(* A bundle is a list of vertices that must be 
  scheduled at the same time because they all 
  access the same array. *)
let vertex_bundle arrmap vertex = 
  match (arrays_of_vertex vertex) with 
  | [] -> 
    (* can schedule cs by itself *)
    [vertex] 
  | [arr] -> 
    (* must schedule everyone that uses arr*)
    (vertices_of_array arrmap arr) 
  | _ -> error "[vertex_bundle] not supported -- more than 1 array per vertex"
;;


(*** placement request arguments ***)
type placement_args = 
{
  (* vertices to place *)
  vertex_bundle : vertex_stmt list;
  (* nodes that must be placed first *)
  dependencies : vertex_stmt list;
  (* array that the vertices access *)
  arrays : Cid.t list;
}

let placement_done pargs_opt = 
  match pargs_opt with 
  | None -> true
  | Some _ -> false
;;

(**** placement loops ****)

(* try to place all the vertices in the table *)
let try_place_in_table (prior_tables, pargs_opt) (table:table) =
  let not_placed_result = (prior_tables@[table], pargs_opt) in 
  match pargs_opt with 
  | None -> not_placed_result
  | Some(pargs) -> (
      (* check to make sure we are allowed to merge into this table *)
      if (table.solitary)
      then (not_placed_result)
      else (        
        (* table merge may fail, if the table is a user table *)
        let new_table_opt = place_in_table pargs.vertex_bundle table in
        match new_table_opt with 
        | None -> not_placed_result
        | Some(new_table) -> (
          (* check to make sure resulting table is not too big *)
          if (not (table_fits new_table))
          then (not_placed_result)
          (* if not too big, return the new table and empty placement request *)
          else (prior_tables@[new_table], None)
        )
      )
  )
;;

(* If the vertex is ready to be placed, 
   based on the dataflow constraints, 
   get placement args *)
let bundle_placement_args prog_info pipe vertex : placement_args option =
  let bundle = vertex_bundle prog_info.arr_users vertex in 
  let bundle_preds = preds_of_vertices prog_info.dfg bundle in
  let placed_vertices = vertices_of_pipe pipe in
  (* print_endline@@"[bundle_placement_args] placed vertices: "^(ids_of_vs placed_vertices); *)
  let rdy = 
    let update_rdy rdy pred = 
      let pred_rdy = (CL.mem pred placed_vertices) in 
(*       (if (not pred_rdy)
      then (
        print_endline@@"vertex "^(id_of_v vertex)^" not rdy for placement because of unplaced dependency vertex "^(id_of_v pred);
      ));
 *)      rdy && pred_rdy 
    in 
    CL.fold_left update_rdy true bundle_preds
  in
  match rdy with 
    | true -> Some ({
      vertex_bundle = bundle;
      dependencies = bundle_preds;
    arrays = arrays_of_vertex vertex;})
    | false -> None 
;;

let try_place_in_stage prog_info (prior_stages, pargs_opt) stage = 
  let not_placed_result = (prior_stages@[stage], pargs_opt) in 
  match pargs_opt with 
  | None -> not_placed_result (* already placed in a prior stage *)
  | Some(pargs) -> (
    let deps_satisfied = vertices_placed pargs.dependencies prior_stages in
    if (not deps_satisfied)
    then (
      (* print_endline "[try_place_in_stage] fail: data dependencies not satisfied."; *)
      not_placed_result
    )
    else (
      let updated_tables, pargs_opt = CL.fold_left 
        try_place_in_table 
        ([], Some(pargs))
        stage.tables
      in
      (* if placement in all current tables fails, add a new table to the stage. *)
      let updated_tables = 
        if (placement_done pargs_opt)
        then (updated_tables)
        else (
          updated_tables@
          (
            match (place_in_table pargs.vertex_bundle empty_table) with 
          | None -> error "[try_place_in_stage] failed to merge into an emtpy table...";
          | Some (tbl) -> [tbl]
          )
        )
      in     
      let updated_stage = {tables = updated_tables;} in
      (* if the placement makes the stage full, placement fails.*)
      if (not (stage_fits prog_info updated_stage)) 
      then (not_placed_result)
      else (prior_stages@[updated_stage], None)
    )
  )
;;

let place_in_pipe prog_info pargs pipe : (pipeline * vertex_stmt list) option =
  let placed_vertices = pargs.vertex_bundle in 
  let updated_stages, pargs_result_opt = CL.fold_left 
    (try_place_in_stage prog_info)
    ([], Some(pargs))
    pipe.stages
  in 
  if (placement_done pargs_result_opt) 
    then (Some ({stages = updated_stages;}, placed_vertices))
    else (
      (* could not place in current stages: try new*)
      (* print_endline ("[place_in_pipe] placement in current stages failed, attempting to place in new stage"); *)
      let updated_stages, pargs_result_opt = try_place_in_stage prog_info
        (updated_stages, pargs_result_opt) 
        empty_stage
      in 
      (* print_endline ("[place_in_pipe] attempt to place in new stage"); *)
      if (placement_done pargs_result_opt) 
        then (Some ({stages = updated_stages;}, placed_vertices))
        (* placement _still_ failed. This should not happen.*)
        else (
          (* print_endline "[place_in_pipe] placement in new stage failed"; *)
          None
        )
  )
;;

(* try to place the node somewhere in the pipeline. *)
let try_place_vertex prog_info pipe vertex =
(*   print_endline ("attempting to place:\n"^(str_of_cond_stmt vertex));
  print_endline ("current pipe: ");
  print_endline (string_of_pipe pipe); *)
  (* make sure the vertex is ready to be placed according to data dependencies, 
     if it is, place the vertex in the pipe.  *)
  match (bundle_placement_args prog_info pipe vertex) with 
    | None -> 
      (* print_endline ("vertex cannot be placed: dependencies not yet placed."); *)
      None
    | Some(pargs) -> 
      (* print_endline ("placing a vertex:\n"^(str_of_cond_stmt vertex)); *)
      place_in_pipe prog_info pargs pipe
;;

(*** main placement function ***)
let rec schedule prog_info pipeline scheduled_nodes unscheduled_nodes n_failures : pipeline option = 
  (* if we have had n successive failures, and there are n unscheduled nodes, 
     that means we have tried to schedule every unscheduled node and none is 
   ready, i.e., there is no schedule. *) 

  let n_unscheduled = CL.length unscheduled_nodes in 
  (if (((n_unscheduled mod 10) = 0) && (n_failures = 0))
  then (print_endline@@"statements left to lay out: "^(string_of_int n_unscheduled)^" # pipeline stages: "^(CL.length pipeline.stages |> string_of_int)));
  (* print_endline ("number of unscheduled vertices: "^(CL.length unscheduled_nodes |> string_of_int)); *)
  (* print_endline ("number of scheduled vertices: "^(CL.length scheduled_nodes |> string_of_int)); *)
  if ((n_unscheduled>0) &&  (n_unscheduled = n_failures))
  then (None)
  else (
    match unscheduled_nodes with 
    (* nothing to schedule, done *)
    | [] -> Some (pipeline)
    | node::unscheduled_nodes -> (
      match try_place_vertex prog_info pipeline node with 
      | Some (updated_pipe, placed_vertices) -> 
        (* print_endline@@"[schedule] placed "^(CL.length placed_vertices |> string_of_int)^" vertices"; *)
        (* some vertices were placed. Move them to scheduled list and go on. *)
        let new_scheduled_nodes = placed_vertices@scheduled_nodes in 
        let new_unscheduled_nodes = MiscUtils.list_sub unscheduled_nodes placed_vertices in 
        (* node was scheduled, go on with the rest *)
        schedule prog_info updated_pipe new_scheduled_nodes new_unscheduled_nodes 0
      | None -> 
        (* print_endline@@"[schedule] node could not be scheduled!"; *)
      (* node could _not_ be scheduled. Move it to the back 
         of the unscheduled node list and go on. *)
        schedule prog_info pipeline scheduled_nodes (unscheduled_nodes@[node]) (n_failures + 1)   
    )
  )
;;

let prog_info tds dfg = {
  dfg = dfg;
  arr_users = build_array_map dfg;
  arr_dimensions = array_dimensions tds;
}
;;
(* find a layout based on dfg, update main body in tds, replacing 
   it with the laid-out version. *)
let process tds dfg = 
  (* extract information from program that is important for layout *)
  let prog_info = prog_info tds dfg in
  (* 2. get list of statements to schedule. *)
  let unscheduled_nodes = Dfg.fold_vertex 
    (fun v vs -> 
      match v.stmt.s with 
      | SNoop -> vs
      | _ -> vs@[v]
    )
    dfg
    []
  in 
  (* 3. schedule everything *)
  let result_pipe_opt = schedule prog_info (empty_pipeline) [] unscheduled_nodes 0 in 
  match result_pipe_opt with 
  | Some(pipe) -> 
    print_endline "---- layout summary ----";
    print_endline (summarystr_of_pipeline pipe);
(*     print_endline ("resulting pipeline: ");
    print_endline (string_of_pipe pipe); *)
    update_main tds {(main tds) with main_body=stmts_of_pipe pipe;}
  | None -> error "[coreLayout] pipeline could not be laid out."
;;

(* print the number of stages to a file in the build directory *)
let profile tds build_dir = 
  let num_stages = string_of_int ((main tds).main_body |> CL.length) in 
  let stages_fn = "num_stages.txt" in
  IoUtils.writef (build_dir ^ "/" ^ stages_fn) num_stages
;;

let compare_layouts tds_old tds_new = 
  let stmt_old = (main tds_old).main_body in
  let stmt_new = (main tds_new).main_body in
  let len_old = (CL.length stmt_old) in
  let len_new = (CL.length stmt_new) in 
  if (len_old = len_new) then (
    print_endline ("old and new layouts both have "^(string_of_int len_old)^" statements");
  ) else (
    print_endline ("LAYOUTS DIFFER. OLD STAGES: " ^ (string_of_int len_old) ^ " NEW STAGES: "^(string_of_int len_new));
    error "[compare_layouts] layout changed in new algo!"
  )
;;
