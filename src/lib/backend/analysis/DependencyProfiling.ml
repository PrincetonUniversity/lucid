(* use the backend to estimate resource utilization on the tofino *)
open Yojson.Basic.Util
open TofinoCore
open CoreCfg
open CoreDfg
open CorePrinting

let info str = 
  Console.show_message str ANSITerminal.Green "Dependency profiler"
;;

exception Error of string
let error s = raise (Error s)

let id_of_vertex v = v.stmt.sspan.spid |> string_of_int

let string_of_vertex v =
  match v.stmt.s with 
  | SMatch(_, branches) -> (
    let op_branches = List.filter_map 
      (fun (_, stmt) -> (
        match (InterpHelpers.unfold_stmts stmt) with 
        | [] -> None
        | [stmt] -> Some(stmt)
        | _ -> error "[string_of_vertex] unexpected: a match branch with multiple statements inside of a dependency graph"
        )
      )
      branches
    in
    match op_branches with 
      | [single_branch_stmt] -> 
        (string_of_int single_branch_stmt.sspan.spid)
        ^":"^(statement_to_string single_branch_stmt)
      | _ -> 
        (string_of_int v.stmt.sspan.spid)
        ^":"^(statement_to_string v.stmt)
  )
  | _ -> statement_to_string v.stmt

;;

module CfgDotConfig = struct
  include Cfg
  let graph_attributes _ = []
  let edge_attributes (_, _, _) = []
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes _ = [`Shape `Box]
  let vertex_name v = "\"" ^(id_of_vertex v)^ "\""
  let default_vertex_attributes _ = []
end

module CfgDot = Graph.Graphviz.Dot (CfgDotConfig)

module DfgDotConfig = struct
  include Dfg
  let graph_attributes _ = []
  let edge_attributes (_, _, _) = []
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes _ = [`Shape `Box]
  let vertex_name v = "\"" ^(id_of_vertex v)^ "\""
  let default_vertex_attributes _ = []
end

module DfgDot = Graph.Graphviz.Dot (DfgDotConfig)

let print_cfg fn g =
    CfgDot.output_graph (Caml.open_out_bin fn) g
;;

let print_dfg fn g =
    DfgDot.output_graph (Caml.open_out_bin fn) g
;;

(* create a json that maps each node id to a statement *)
let dump_symbol_table_json f_fold_vertex fn g =
  let collect_symbols v symbol_map = 
    (v.stmt.sspan.spid, (statement_to_string v.stmt))::symbol_map 
  in
  let symbol_map = f_fold_vertex collect_symbols g [] in
  (* now translate this to a json... *)
  let json_assoc = List.map 
    (fun (stmt_id, stmt_str) -> (string_of_int stmt_id, `String stmt_str))
    symbol_map
  in
(*   let json_assoc = [
        ("id", `String "398eb027");
        ("name", `String "John Doe");
      ]
  in
 *)
  let json_output = `Assoc json_assoc in

  let oc = open_out fn in
  Yojson.Basic.pretty_to_channel oc json_output;
  (* Yojson.Basic.pretty_to_channel  *)

;;

(* dump the data dependency graph *)
let dump_dep_graphs tds output_dir = 
  let cdg_fn = output_dir^"/control_dependency_graph.dot" in
  let cdg_symbols_fn = output_dir^"/control_dependency_graph.json" in
  let ddg_fn = output_dir^"/data_dependency_graph.dot" in
  let ddg_symbols_fn = output_dir^"/data_dependency_graph.json" in
  let cfg = CoreCfg.cfg_of_main tds in 
  let cdg = CoreCdg.to_control_dependency_graph cfg in        
  let dfg = CoreDfg.process cdg in 
  info@@"Writing control dependency graph to: "^(cdg_fn)^" with symbols defs in: "^(cdg_symbols_fn);
  info@@"Writing data dependency graph to: "^(ddg_fn)^" with symbols defs in: "^(ddg_symbols_fn);
  print_cfg (cdg_fn) cdg;
  dump_symbol_table_json Cfg.fold_vertex cdg_symbols_fn cdg;
  print_dfg (ddg_fn) dfg;
  dump_symbol_table_json Dfg.fold_vertex ddg_symbols_fn dfg;
  ()
 ;;

let profile ds output_dir = 
  let _, _ = ds, output_dir in
  dump_dep_graphs ds output_dir
;;

