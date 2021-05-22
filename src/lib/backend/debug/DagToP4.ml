(* 
For debugging 
- This module prints P4 code from a dag program.
 *)
open Format
open InstrSyntax
open MiscUtils
open DagSyntax
open Consts
open P4IdPrint
open P4InstrPrint
open P4DeclPrint
exception Error of string

(* logging *)
module DBG = BackendLogging
let outc = ref None 
let dprint_endline = ref DBG.no_printf


(* The dagControl is a DAG program syntax that is 
easy to print as a P4 control object. *) 
type dagControl = dagStatement list 

and dagStatement = 
  | TableCall of tid (* just call a table *)
  | TableBranch of tid * dagBranch list (* call a table and branch depending on the action taken *)

and dagBranch = (aid * dagStatement list)

and aid = cid

and tid = cid

and cid = [%import: Cid.t]

and id =  [%import: Id.t]
[@@deriving
  visitors
    { name = "dagSyntaxIter"
    ; variety = "iter"
    ; polymorphic = true
    ; data = true
    ; concrete = true
    }
  , visitors
      { name = "dagSyntaxMap"
      ; variety = "map"
      ; polymorphic = true
      ; data = true
      ; concrete = true
      }
  , show]



(* Todo (10/6/20): update header and parser printing so that they don't need 
  to have the various "block signatures" passed in. *)
let tab () =
  pp_open_vbox str_formatter 4;
  ()
;;

let untab () =
  pp_close_box str_formatter ();
  ()
;;

let lineSep fmt () = fprintf fmt "@,"

let map_and_concat mf vals = (Caml.List.map mf vals) |> (Caml.String.concat ", ")


(* build a list of successors from a node that gets called. *)
let rec build_call_statement decl_map node_id next_tids = 
  match next_tids with 
    | [] -> [TableCall node_id] (* no next tid. This is the end. *) 
    (* one next tid. This is a call, followed by at least one more call. *)
    (* if the next call is a join, we don't want to visit it... *)
    | [next_tid] -> (
      (* printf "next_tid: %s\n" (mid_to_str_suffix next_tid);  *)
      let pred_aids = pred_aids_of_tid decl_map next_tid in 
      match (Caml.List.length pred_aids) with 
      | 0 -> error "error: found no predecessors for a table, but I am the predecessor..."
      | 1 -> [TableCall node_id]@(build_statement_tree_from_node decl_map next_tid)      
      | _ -> [TableCall node_id]
    )
    | _ -> error "multiple next tables, not a call."

(* statement tree building *)
and build_statement_tree_from_node decl_map node_id : dagStatement list = 

  (* there are only statements for tables and native blocks. *)
	match (Cid.lookup decl_map node_id) with 
	| Table _ -> (
    let next_tids = succs_of_tid decl_map node_id in 
    printf "build_statement_tree_from_node -- table: %s next_tids: %s\n" (mid_to_str_suffix node_id) (map_and_concat mid_to_str_suffix next_tids); 
    
    match Caml.List.length (next_tids) with 
      (* 0 or 1 next table -- this is a call. *)
      | 0 | 1 -> build_call_statement decl_map node_id next_tids
      (* multiple next tables -- this is a branch. *)
      | _ -> (
        (* for each action id, we need to generate a list of statements based on the next node. *)
        (* then, we need to figure out what the common successor is and recurse from there. *)
        (* get the ids of the (possibly) branching actions and their successor tables.*)
        let aids = aids_of_tid decl_map node_id in
        let successor_tids = Caml.List.map (succ_of_aid decl_map) aids in 
        let acn_succtbl = Caml.List.combine aids successor_tids in 

        (* find the table where all the branches join. *)        
        let join_tid_opt = get_first_common_descendent decl_map aids in 

        (* if the successor of any action branch is the join table, remove it -- that will be processed separately *)
        let is_branch_empty (_, tbl_id) = 
          match join_tid_opt with 
            | None -> true
            | Some join_tbl_id ->
              if (tbl_id = join_tbl_id)
              then false
              else true
        in 
        let acn_succtbl = Caml.List.filter is_branch_empty acn_succtbl in 
        let (aids, successor_tids) = Caml.List.split acn_succtbl in
        DBG.printf outc "build_statement_tree_from_node: table id: %s\n" (mid_to_str_suffix node_id);
        DBG.printf outc "build_statement_tree_from_node: action ids: %s\n" (map_and_concat mid_to_str_suffix aids);
        DBG.printf outc "build_statement_tree_from_node: successor tids: %s\n" (map_and_concat mid_to_str_suffix successor_tids);
        let branch_statements = Caml.List.map (build_statement_tree_from_node decl_map) successor_tids in         
        let dag_branches = Caml.List.combine aids branch_statements in 
        let branch_stmt = TableBranch(node_id, dag_branches) in 
        (* recurse from the join table. *)
        match join_tid_opt with 
          | None -> [branch_stmt] (* the branches never rejoin, so we're done. *)
          | Some join_tid -> [branch_stmt]@(build_statement_tree_from_node decl_map join_tid)
      )
  )
(*   | SchedBlock _ -> (
    DBG.printf outc "processing native block %s\n" (mid_to_str_suffix node_id);
    let successor_tids = successors_of_native_block decl_map node_id in 
    build_call_statement decl_map node_id successor_tids
  ) *)
	| node -> 
    print_endline ("Don't know how to print this in the apply");
    print_endline ("---------------");
    print_endline (InstrSyntax.show_decl node);
    print_endline ("---------------");
    error "A syntax tree must start from a table."
;;

let build_statement_tree dag = 
	let (decl_map, root_tbl_id) = dmap_rtid_of_dprog dag in 
  build_statement_tree_from_node decl_map root_tbl_id
;;
(* printers *)


let print_decl fmt dec = 
  match dec with 
  | Table (tbl_id, rules, stage_opt) -> (
    let match_vars = match_vars_of_rules rules in 
    pp_open_vbox fmt 0;
    print_tbl_decl fmt tbl_id match_vars rules stage_opt [];
    pp_close_box fmt ()
  )
  | _ -> ()
;;

let print_decls_from_list fmt decls = 
  let print_filtered_decls decls = pp_print_list ~pp_sep:lineSep (DpaToP4Control.print_decl []) fmt decls in 
  pp_open_vbox fmt 0;
  print_filtered_decls (Caml.List.filter (fun d -> match d with GlobalMeta _ -> true | _ -> false) decls); 
  print_filtered_decls (Caml.List.filter (fun d -> match d with RegVec _ -> true | _ -> false) decls); 
  print_filtered_decls (Caml.List.filter (fun d -> match d with SInstrVec _ -> true | _ -> false) decls); 
  print_filtered_decls (Caml.List.filter (fun d -> match d with Hasher _ -> true | _ -> false) decls); 
  print_filtered_decls (Caml.List.filter (fun d -> match d with InstrVec _ -> true | _ -> false) decls); 
  print_filtered_decls (Caml.List.filter (fun d -> match d with Action _ -> true | _ -> false) decls); 
  print_filtered_decls (Caml.List.filter (fun d -> match d with Table _ -> true | _ -> false) decls); 
  print_filtered_decls (Caml.List.filter (fun d -> match d with SchedBlock _ -> true | _ -> false) decls);   
  pp_close_box fmt ()
;;  

let print_decls_from_map fmt decl_map = 
  let _, decls = Caml.List.split decl_map in   
  print_decls_from_list fmt decls;
;;

let print_decls fmt dag = 
  let decl_map = dmap_of_dprog dag in
  print_decls_from_map fmt decl_map
;;

let rec print_dagStatements fmt stmts = 
  (* Caml.List.iter print_dagStatement stmts *)
  pp_print_list ~pp_sep:lineSep print_dagStatement fmt stmts

and print_dagStatement fmt (stmt:dagStatement) = 
  match stmt with
  | TableCall table_id -> (
    fprintf fmt "%s.apply();" (mid_to_str_suffix table_id)
  )
  | TableBranch (table_id,branches) -> (
    tab ();
    fprintf
      fmt
      "switch (%s.apply().action_run) {@,"
      (mid_to_str_suffix table_id);
    print_branches fmt branches;
    untab ();
    fprintf fmt "@,}"
  )

and print_branches fmt (branches: dagBranch list) = 
  match branches with
  | [] -> ()
  | (acn_id, stmts) :: branches -> 
    tab ();
    fprintf fmt "%s : {@," (mid_to_str_suffix acn_id);
    print_dagStatements fmt stmts;
    untab ();
    fprintf fmt "@,}@,";
    print_branches fmt branches
;;

let print_apply_body fmt dag = 
  let syntax_tree = build_statement_tree dag in
  DBG.printf outc "---- syntax tree ----\n%s\n-----\n" (show_dagControl syntax_tree);
  print_dagStatements fmt syntax_tree;
;;

let print_apply dag = 
  fprintf str_formatter "@,";
  pp_open_vbox str_formatter 4;
  fprintf str_formatter "apply {@,";
  print_apply_body str_formatter dag;
  pp_close_box str_formatter ();
  fprintf str_formatter "@,}"
;;

let print_control_params prog_id = 
  fprintf
    str_formatter
    "control %s(inout %s %s, inout %s %s){@,"
    (mid_to_str prog_id)
    dpa_global_t
    dpa_global_name
    dpa_local_t
    dpa_local_name;
;;

let print_control (dag:dagProg) ctl_fn = 
  DBG.start_mlog __FILE__ outc dprint_endline;
  (* let (prog_id, builtin_params,user_params,decl_map,root_tbl_id) = dag in  *)

  let prog_id = Consts.progId in 
  pp_open_vbox str_formatter 4;  
  print_control_params prog_id;
  print_decls str_formatter dag;
  print_apply dag;
  print_endline ("here...");
  pp_close_box str_formatter ();
  fprintf str_formatter "@,}";  
  let ctl_text = flush_str_formatter () in 
  !dprint_endline ("printing control body to: "^(ctl_fn));
  Caml.Printf.fprintf (open_out ctl_fn) "%s" ctl_text
;;



(**** from mergeutils ****)
let str_of_prog decls_map = 
  pp_open_vbox str_formatter 0;
  print_decls_from_map str_formatter decls_map;
  pp_close_box str_formatter ();
  let str = flush_str_formatter () in 
  str
;;

let print_prog decls_map = 
  printf "-----example program-----\n";
  let str = str_of_prog decls_map in 
  printf "%s\n" str;
  printf "-----end example program-----\n"
;;  

let str_of_pattern_entry (var_id, cond) = 
  match cond with 
  | Exact (c) -> sprintf "%s = %i" (mid_to_str var_id) (Integer.to_int c)
  | Any -> sprintf "%s = *" (mid_to_str var_id) 
;;


let lineSep fmt () = fprintf fmt "@,"

let str_of_tbls_and_acns decls = 
  pp_open_vbox str_formatter 0;
  let print_filtered_decls decls = pp_print_list ~pp_sep:lineSep (DpaToP4Control.print_decl []) str_formatter decls in 
(*   fprintf str_formatter "//----actions----@,"; *)
  print_filtered_decls (Caml.List.filter (fun d -> match d with |Action _ -> true | _ -> false) decls); 
(*   fprintf str_formatter "//----tables----@,"; *)
  print_filtered_decls (Caml.List.filter (fun d -> match d with |Table _  -> true | _ -> false) decls); 
  pp_close_box str_formatter ();
  let out_str = flush_str_formatter () in 
  out_str
;;

let p4str_from_tid cid_decls tid = 
  let decls = (Cid.lookup cid_decls tid)::(decls_of_tid cid_decls tid) in 
  let str = str_of_tbls_and_acns decls in
  str
;;


(**** logging ir and graphs to files ****)

module DotConfig = struct
   include G (* use the graph module from above *)
   let graph_attributes _ = []
   let edge_attributes (_, _, _) = []
   let default_edge_attributes _ = []
   let get_subgraph _ = None
   let vertex_attributes _ = [`Shape `Box]
   let vertex_name v = (mid_to_str_suffix v)
   let default_vertex_attributes _ = []
end 

module Dot = Graph.Graphviz.Dot(DotConfig)

let dump_dag g fn = 
  (* printf "dumping dag to: %s\n" fn; *)
  Dot.output_graph (open_out_bin fn) g
;;

(* log tables and actions in current program. *)
let log_tbls_and_acns (dag_prog:dagProg) fn = 
  let cid_decls = dmap_of_dprog dag_prog in 
  let _, decls = CL.split cid_decls in 
  let out_str = (str_of_tbls_and_acns decls) in 
  Caml.Printf.fprintf (open_out fn) "%s" out_str
;;

(* log the p4 form of the current program *)
(* let log_p4_program (dpa_prog:dpaProg) fn = 
    DpaToP4Control.print_p4_control dpa_prog (open_out fn);
;;  
 *)
(* log a dag of tables to a dot file. *)
let dump_tbl_g dag_prog fn = 
  let (cid_decls, _, g) = dag_prog in 
  let tbl_g = table_g_of cid_decls g in 
  dump_dag tbl_g fn 
;;

(* log a dag of tables and the tables and actions of the current prog.*)
let log_tbl_g_and_ir (dag_prog) fn_base = 
  let fn_dot = !BackendLogging.graphLogDir^"/"^fn_base^".dot" in 
  let fn_prog = !BackendLogging.irLogDir^"/"^fn_base^".p4" in 
  dump_tbl_g dag_prog fn_dot;
  log_tbls_and_acns dag_prog fn_prog
;;
