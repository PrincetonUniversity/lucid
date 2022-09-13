(* CoreLayout -- lay out the data flow graph in a pipeline of tables *)

(* this operates on a data flow graph of match statements, 
   some of which are solitary and must be mapped to their own table... *)

open Collections
open CoreSyntax
open TofinoCore
open MatchAlgebra
open CoreCfg
open CoreDfg

(*** helpers that belong in a prior module (coredfg?) ***)


let string_of_fcncid cid = 
    Caml.String.concat "." @@ Cid.names cid
;;


let arrays_of_exp exp = 
  match exp.e with 
  | ECall(fid, args) -> (
    match (string_of_fcncid fid) with 
      | "Array.get"| "Array.getm" | "Array.set"
      | "Array.setm" | "Array.update" 
      | "Array.update_complex" | "PairArray.update" -> 
      [List.hd args |> InterpHelpers.name_from_exp]
      | _ -> []
  )
  | _ -> []
;;

let rec arrays_of_stmt stmt : Cid.t list = 
  match stmt.s with
  | SLocal(_, _, exp)
  | SAssign(_, exp)
  | SUnit(exp) -> arrays_of_exp exp
  | SIf(exp, s1, s2) -> 
      (arrays_of_exp exp)
      @(arrays_of_stmt s1)
      @(arrays_of_stmt s2)
  | SMatch(es, bs) ->
    (CL.map arrays_of_exp es |> CL.flatten)
    @(CL.map 
        (fun (_, stmt) -> arrays_of_stmt stmt)
        bs
      |> CL.flatten
    )
  | SSeq(s1, s2) -> 
    (arrays_of_stmt s1)@(arrays_of_stmt s2)
  | _ -> []
;;

let arrays_of_cond_stmt v = 
  arrays_of_stmt v.stmt
;;


(**** branch representation simple match-statement merging ****)

type pattern_branch = {
  pattern : (exp * pat) list;
  stmt    : statement;
}

type pattern_branches = pattern_branch list ;;

let cid_from_exp (ex : exp) : Cid.t =
  match ex.e with
  | EVar n -> n
  | _ -> error "could not evaluate expression to a name"
;;


let pattern_branches_of_match m: pattern_branches = 
  match m.s with 
  | SMatch(es, bs) -> (
(*     print_endline ("es:"^( (List.map CorePrinting.exp_to_string es) |> String.concat "," ));
    print_endline ("pats:"^(CorePrinting.comma_sep CorePrinting.pat_to_string (List.hd bs |> fst))); *)
    let pattern_branch_of_branch (pats, stmt) =
      { pattern = List.combine es pats;
        stmt = stmt; }
    in 
    List.map pattern_branch_of_branch bs
  )
  | _ -> error "[pattern_branches_of_match] not a match statement."
;;

let keys_of_pattern_branches pbs =
  let foo = CL.map (fun pb -> pb.pattern |> CL.split |> fst) pbs
  |> CL.flatten 
  in
  MatchAlgebra.unique_exp_list foo
;;

let cases_of_pattern_branches pbs =  
  CL.map 
    (fun pb -> 
      (CL.split pb.pattern |> snd, pb.stmt)
    )
    pbs
;;

(* match statement of a pattern branches list. *)
let smatch_of_pattern_branches pbs =
  smatch 
    (keys_of_pattern_branches pbs)
    (cases_of_pattern_branches pbs)
;;


let can_match_after = MatchAlgebra.Z3Helpers.is_pattern_matchable
;;

(*** 

BUG: when there's a no-op rule, there's an unmatchable pattern and it somehow messes everything up...



***)

let string_of_patts_something rs = 
(smatch_of_pattern_branches rs
  |> CorePrinting.statement_to_string)
;;
(* compute the cross product branch 
   of (b1, b2) and add it into bs 
    The cross product branch is simply: 
      b1 && b2 --> (s1; s2)
      b1 --> s1
      b2 --> s2
    Each of these branches is only added if it is 
    matchable after all rules added before it.*) 
(* find the rules that are the intersection of b1 and b2, if any. *)
let cross_product bs (b1, b2) = 
  (* b1 and b2 may not have the same fields.*)
(*   print_endline ("[cross_product] bs:");
  string_of_patts_something bs |> print_endline;
  print_endline ("[cross_product] b1:");
  string_of_patts_something [b1] |> print_endline;
  print_endline ("[cross_product] b2:");
  string_of_patts_something [b2] |> print_endline; *)

  let patterns_of_bs bs = List.map (fun b -> b.pattern) bs in 
  let intersect_pattern = MatchAlgebra.and_patterns b1.pattern b2.pattern in 
  (* add the intersection branch after previous intersect branches, if its 
     possible for the intersection branch to match after them. *)
  let bs = match intersect_pattern with 
    | Some (pattern) -> (

      match (can_match_after pattern (patterns_of_bs bs)) with 
      | true -> 
        let stmt = sseq_sp b1.stmt b2.stmt Span.default in
        bs@[{pattern; stmt;}]
      | false -> bs
    )
    | None -> 
      (* the intersection between the two statements cannot 
         match anything.  *)
      (* print_endline ("[cross_product] no intersection pattern."); *)
      bs
  in 
  let res = bs in 
  (* add b1 and b2, if they can match after new bs. *)
(*   let update_bs bs b =
    match (can_match_after b.pattern (patterns_of_bs bs)) with 
    | true -> bs@[b]
    | false -> bs
  in 
  let res = List.fold_left (update_bs) bs [b1; b2] in
 *)
  (* print_endline("[cross_product] result:\n"^(string_of_patts_something res)); *)
  res
;;

let align_keys pb1 pb2 =
  let keys = keys_of_pattern_branches (pb1@pb2) in 
  CL.map 
    (fun pb -> {pb with pattern=extend_pat keys pb.pattern;})
    pb1
  ,CL.map 
    (fun pb -> {pb with pattern=extend_pat keys pb.pattern;})
    pb2

;;


(* delete the pattern branches that cannot be matched in a list *)
let delete_unreachable bs =
  let patterns_of_bs bs = List.map (fun b -> b.pattern) bs in 
  let res = List.fold_left
    (fun bs b -> 
      if (can_match_after b.pattern (patterns_of_bs bs))
      then (bs@[b])
      else (bs)
    )
    []
    bs
  in 
  res

;;

let combine_pattern_branches bs1 bs2 = 
  (* first, make sure bs1 and bs2 are over the same fields. *)
(*   print_endline ("[combine_pattern_branches] bs1_PRE:");
  print_endline (string_of_patts_something bs1);
  print_endline ("[combine_pattern_branches] bs2_PRE:");
  print_endline (string_of_patts_something bs2); *)

  let bs1, bs2 = align_keys bs1 bs2 in 
(*   print_endline ("[combine_pattern_branches] bs1:");
  print_endline (string_of_patts_something bs1);
  print_endline ("[combine_pattern_branches] bs2:");
  print_endline (string_of_patts_something bs2); *)
  match (bs1, bs2) with 
    | [], [] -> []
    | _, [] -> bs1
    | [], _ -> bs2
    | _, _ -> 
      let m2_integrate bs b2 =     
        let m1_integrate bs b1 = 
          cross_product bs (b1, b2)
        in 
        List.fold_left m1_integrate bs bs1
      in 
      let intersect_rules = List.fold_left m2_integrate [] bs2 in 
      (* the original rules must come after _all_ the intersect rules, 
         else they may block some from matching. Also, some rules 
         may not be reachable anymore. *)
      delete_unreachable (intersect_rules@bs1@bs2)
;;

(* combine two match statements into a pattern branch *)
let combine_matches m1 m2 = 
  let bs1 = pattern_branches_of_match m1 in
  let bs2 = pattern_branches_of_match m2 in 
  combine_pattern_branches bs1 bs2
;;

(* update pattern branch list bs1, folding in the match statement m2 *)
let fold_match_into_pattern_branches bs1 m2 =
  let bs2 = pattern_branches_of_match m2 in 
  combine_pattern_branches bs1 bs2
;;


(**** A simple model of the hardware ****)

type table = {
  keys  : exp list;
  rules : pattern_branches;
  arrays : Cid.t list;
  sources : CoreCfg.vertex_stmt list;
  solitary : bool; 
}

type stage = {
  tables : table list;
}

type pipeline = {
  stages : stage list;
}

let empty_table = {keys =[]; rules=[]; arrays = []; solitary = false; sources = [];}
let empty_stage = {tables = [];}
let empty_pipeline = {stages = [];}

let summarystr_of_stage s = ("number of tables: "^((CL.length s.tables) |> string_of_int))

let summarystr_of_pipeline p = 
  (List.mapi (fun i s -> 
    ("stage "^(string_of_int (i))^" tables: "^(summarystr_of_stage s))
  )
  p.stages)
  |> String.concat "\n"


let string_of_rules rs = (smatch_of_pattern_branches rs
  |> CorePrinting.statement_to_string)

let string_of_table t =
  "//rules in table: "^(CL.length t.rules |> string_of_int)^"\n"^
  "//source statements in table: "^(CL.length t.sources |> string_of_int)^"\n"^
  (string_of_rules t.rules)
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

let vertices_of_table (t:table) = t.sources
let vertices_of_stage s = CL.map vertices_of_table s.tables |> CL.flatten
let vertices_of_stages ss = CL.map vertices_of_stage ss |> CL.flatten
let vertices_of_pipe  p = vertices_of_stages p.stages

let arrays_of_table (t:table) = t.arrays
let arrays_of_stage s = CL.map arrays_of_table s.tables |> CL.flatten |> (MatchAlgebra.unique_list_of_eq Cid.equal)

(* a table is a match statement *)
let stmt_of_table t =
  smatch_of_pattern_branches t.rules;
;;

(* a stage is a sequence statement of tables *)
let stmt_of_stage s =  
  InterpHelpers.fold_stmts (List.map stmt_of_table s.tables) 
;;

(* a pipeline is a list of stage statements *)
let stmts_of_pipe p = 
  CL.map stmt_of_stage p.stages
;;


let rec hashers_of_exp exp = 
  match exp.e with 
  | EOp(_, es) 
  | ECall(_, es) -> 
    CL.map hashers_of_exp es |> CL.flatten
  | EHash(_, es) -> 
    exp::(CL.map hashers_of_exp es |> CL.flatten)
  | EFlood(e) -> hashers_of_exp e
  | _ -> []


let rec hashers_of_stmt stmt = 
  match stmt.s with
  | SAssign(_, exp)
  | SLocal(_, _, exp)
  | SUnit(exp)
  | SGen(_, exp) 
  | SRet(Some exp)
  ->
   (
    match hashers_of_exp exp with 
    | [] -> []
    | _ -> [stmt]
  )
  | SPrintf(_, exps) -> (
    match CL.map hashers_of_exp exps |> CL.flatten with 
    | [] -> []
    | _ -> [stmt]
  )
  | SIf(exp, s1, s2) -> (
    match hashers_of_exp exp with 
    | [] -> (hashers_of_stmt s1)@(hashers_of_stmt s2)
    | _ -> stmt::(hashers_of_stmt s1)@(hashers_of_stmt s2)
  )
  | SRet(None)
  | SNoop -> []
  | SSeq(s1, s2) -> (hashers_of_stmt s1)@(hashers_of_stmt s2)
  | SMatch(es, bs) -> (
    let es_has_hasher = match CL.map hashers_of_exp es with
      | [] -> false
      | _ -> true
    in 
    let hashers_of_branch (_, stmt) = hashers_of_stmt stmt in
    (match es_has_hasher with | true -> [stmt] | false -> [])@(CL.map hashers_of_branch bs |> CL.flatten)
  )

let hashers_of_table table =
  let hashers_of_pattern_branch (pb:pattern_branch) = hashers_of_stmt pb.stmt in 
  CL.map hashers_of_pattern_branch table.rules |> CL.flatten |> unique_stmt_list
;;

let hashers_of_stage stage =
  CL.map hashers_of_table stage.tables |> CL.flatten |> unique_stmt_list
;;

(* let hashers_of_table (t:table)  *)



let preds_of_vertices dfg vs =
  CL.map (Dfg.pred dfg) vs |> CL.flatten |> MiscUtils.unique_list_of
;;

let statements_of_table table = 
  CL.map (fun (pb:pattern_branch) -> pb.stmt) table.rules |> MatchAlgebra.unique_stmt_list

let keywidth_of_table table = 
  let width_of_exp exp =
    match exp.ety.raw_ty with
    | TBool -> 1
    | TInt(sz) -> sz
    | _ -> error "[keywidth_of_table] reached table key that is not bool or int..."
  in
  CL.fold_left (+) 0 (CL.map width_of_exp table.keys)
;;


let table_is_empty table =
  match table.sources with
  | [] -> true
  | _ -> false
;;

(*** core placement method: 
    merge a match table in the DFG into a table ***)
let merge_bundle_into_table cond_stmts table =
  (* merge a bundle of conditional statements that 
     touch the same array into a table 
     It would be nicer to fold the conditional statements into the table 
     one at a time, but we can't do that because we need to be 
     able to merge user match statements that are in the same 
     bundle (i.e., touch the same register array).
   *)
  (* start by merging all the conditional statements in the bundle together *)
  let new_pbs, contains_usermatch = 
    let folder (pbs, contains_usermatch) (vertex_stmt:vertex_stmt) = 
      let pbs = fold_match_into_pattern_branches pbs vertex_stmt.stmt in 
(*       print_endline ("[merge_bundle_into_table.folder] folding in the statement:");
      print_endline (CorePrinting.statement_to_string vertex_stmt.stmt);
      print_endline ("[merge_bundle_into_table.folder] fold_match_into_pattern_branches result ");
      print_endline (string_of_rules pbs);
      print_endline ("[merge_bundle_into_table.folder] ---------------------- "); *)
      let contains_usermatch = contains_usermatch || vertex_stmt.solitary in 
      (* replaced -- old merge logic and usermatch stuff, before it was a dfg of tables *)
      (* let new_pbs, new_is_usermatch = pattern_branches_of_cond_stmt vertex_stmt in *)
      (* let pbs = combine_pattern_branches pbs new_pbs in *)
      (* let contains_usermatch = contains_usermatch || new_is_usermatch in  *)
      (pbs, contains_usermatch)
    in 
    CL.fold_left folder ([], false) cond_stmts
  in 
  let can_proceed = match (contains_usermatch, table_is_empty table, table.solitary) with 
    | true, true, false -> true (* usermatch into an empty table *)
    | true, _, _ -> false       (* usermatch into anything else *)
    | _, _, true -> false       (* anything into a user table *)
    | false, _, false -> true   (* non usermatch into any non user table *)
  in 

  if (can_proceed)
  then (
    (* the main op: combine the rules *)
(*     print_endline ("-------");
    print_endline ("[merge_bundle_into_table] table.rules:\n"^(string_of_rules table.rules));
    print_endline ("-------");
    print_endline ("[merge_bundle_into_table] new_pbs:\n"^(string_of_rules new_pbs));
    print_endline ("-------"); *)
    let rules = combine_pattern_branches (table.rules) new_pbs in 
(*     print_endline ("-------");
    print_endline ("[merge_bundle_into_table] new table rules:\n"^(string_of_rules rules));
    print_endline ("-------"); *)
    let keys = (keys_of_pattern_branches rules) in
    let arrays = (arrays_of_cond_stmt (CL.hd cond_stmts))@(table.arrays) in
    let sources = cond_stmts@table.sources in
    let solitary = table.solitary || contains_usermatch in
    let new_table = {rules; keys; arrays;sources;solitary} in 
    (* print_endline@@"[merge_bundle_into_table] created table:\n"^(string_of_table new_table); *)
    Some(new_table)
  )
  else (None)
;;

(**** simple resource constraints and checks ****)


(* some simple constraints on tables and stages *)
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
}

let table_fits (table:table) = 
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

let stage_fits stage =
  let c1 = (CL.length stage.tables <= stage_constraints.max_tables) in
  let c2 = ((arrays_of_stage stage |> CL.length) <= stage_constraints.max_arrays) in
  let c3 = ((hashers_of_stage stage |> CL.length) <= stage_constraints.max_hashers) in 
  (* print_endline@@"[stage_fits] c1: "^(string_of_bool c1)^" c2: "^(string_of_bool c2)^" c3: "^(string_of_bool c3); *)
  if ( c1 && c2 && c3)
  then (
    (* print_endline "[stage_fits] TRUE";  *)
    true)
  else (
    (* print_endline "[stage_fits] FALSE";  *)
    false)
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
    CL.fold_left update_for_array m (arrays_of_cond_stmt vertex)
  in 
  Dfg.fold_vertex update_array_map dfg CidMap.empty
;;

(* the main helpers for array stuff 
   node -> array and array -> node *)
let arrays_of_vertex = arrays_of_cond_stmt
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

(* If the vertex is ready to be placed, 
   based on the dataflow constraints, 
   get placement args *)
let bundle_placement_args arrmap dfg pipe vertex : placement_args option =
  let bundle = vertex_bundle arrmap vertex in 
  let bundle_preds = preds_of_vertices dfg bundle in
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
    arrays = arrays_of_cond_stmt vertex;})
    | false -> None 
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
        let new_table_opt = merge_bundle_into_table pargs.vertex_bundle table in
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

let try_place_in_stage (prior_stages, pargs_opt) stage = 
  let not_placed_result = (prior_stages@[stage], pargs_opt) in 
  match pargs_opt with 
  | None -> not_placed_result (* placed in a prior stage *)
  | Some(pargs) -> (
    let dfg_satisfied = vertices_placed pargs.dependencies prior_stages in
    if (not dfg_satisfied)
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
      (* add a new table to the stage if no room in current tables. *)
      let updated_tables = 
        if (placement_done pargs_opt)
        then (updated_tables)
        else (
          updated_tables@
          (
            match (merge_bundle_into_table pargs.vertex_bundle empty_table) with 
          | None -> error "[try_place_in_stage] failed to merge into an emtpy table...";
          | Some (tbl) -> [tbl]
          )
        )
      in     
      let updated_stage = {tables = updated_tables;} in
      (* if the placement makes the stage full, placement fails.*)
      if (not (stage_fits updated_stage)) 
      then (not_placed_result)
      else (prior_stages@[updated_stage], None)
    )
  )
;;

let place_in_pipe pargs pipe : (pipeline * vertex_stmt list) option =
  let placed_vertices = pargs.vertex_bundle in 
  let updated_stages, pargs_result_opt = CL.fold_left 
    try_place_in_stage
    ([], Some(pargs))
    pipe.stages
  in 
  if (placement_done pargs_result_opt) 
    then (Some ({stages = updated_stages;}, placed_vertices))
    else (
      (* could not place in current stages: try new*)
      (* print_endline ("[place_in_pipe] placement in current stages failed, attempting to place in new stage"); *)
      let updated_stages, pargs_result_opt = try_place_in_stage 
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
let try_place_vertex arrmap dfg pipe vertex =
(*   print_endline ("attempting to place:\n"^(str_of_cond_stmt vertex));
  print_endline ("current pipe: ");
  print_endline (string_of_pipe pipe); *)
  (* make sure the vertex is ready to be placed according to data dependencies, 
     if it is, place the vertex in the pipe.  *)
  match (bundle_placement_args arrmap dfg pipe vertex) with 
    | None -> 
      (* print_endline ("vertex cannot be placed: dependencies not yet placed."); *)
      None
    | Some(pargs) -> 
      (* print_endline ("placing a vertex:\n"^(str_of_cond_stmt vertex)); *)
      place_in_pipe pargs pipe
;;

(*** main placement function ***)
let rec schedule (arrmap:array_users_map) dfg pipeline scheduled_nodes unscheduled_nodes n_failures : pipeline option = 
  (* if we have had n successive failures, and there are n unscheduled nodes, 
     that means we have tried to schedule every unscheduled node and none is 
   ready, i.e., there is no schedule. *) 

  let n_unscheduled = CL.length unscheduled_nodes in 
  (if (((n_unscheduled mod 10) = 0) && (n_failures = 0))
  then (print_endline@@"statements left to lay out: "^(string_of_int n_unscheduled)^" # pipeline stages (so far): "^(CL.length pipeline.stages |> string_of_int)));
  (* print_endline ("number of unscheduled vertices: "^(CL.length unscheduled_nodes |> string_of_int)); *)
  (* print_endline ("number of scheduled vertices: "^(CL.length scheduled_nodes |> string_of_int)); *)
  if ((n_unscheduled>0) &&  (n_unscheduled = n_failures))
  then (None)
  else (
    match unscheduled_nodes with 
    (* nothing to schedule, done *)
    | [] -> Some (pipeline)
    | node::unscheduled_nodes -> (
      match try_place_vertex arrmap dfg pipeline node with 
      | Some (updated_pipe, placed_vertices) -> 
        (* print_endline@@"[schedule] placed "^(CL.length placed_vertices |> string_of_int)^" vertices"; *)
        (* some vertices were placed. Move them to scheduled list and go on. *)
        let new_scheduled_nodes = placed_vertices@scheduled_nodes in 
        let new_unscheduled_nodes = MiscUtils.list_sub unscheduled_nodes placed_vertices in 
        (* node was scheduled, go on with the rest *)
        schedule arrmap dfg updated_pipe new_scheduled_nodes new_unscheduled_nodes 0
      | None -> 
        (* print_endline@@"[schedule] node could not be scheduled!"; *)
      (* node could _not_ be scheduled. Move it to the back 
         of the unscheduled node list and go on. *)
        schedule arrmap dfg pipeline scheduled_nodes (unscheduled_nodes@[node]) (n_failures + 1)   
    )
  )
;;


(* find a layout based on dfg, update main body in tds, replacing 
   it with the laid-out version. *)
let process tds dfg = 
  (* 1 build array map. *)
  let arrmap = build_array_map dfg in
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
  let result_pipe_opt = schedule arrmap dfg (empty_pipeline) [] unscheduled_nodes 0 in 
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



