(* Helpers to log the various IR representations
of Lucid. *)
module IS = LLSyntax
module CL = Caml.List

(**** Print Lucid program ****)
let log_lucid fn ds =
  let full_fn = !BackendLogging.irLogDir ^ "/" ^ fn in
  let outf = (open_out full_fn) in 
  Printf.fprintf outf "%s" (CorePrinting.decls_to_string ds);
  flush outf
;;

(* log program before and after a source pass *)
let log_lucid_pre module_name ds = log_lucid (module_name ^ "_pre.dpt") ds
let log_lucid_post module_name ds = log_lucid (module_name ^ "_post.dpt") ds

(**** Print Lucid opstatement program ****)
module StDotConfig = struct
  include OGSyntax.StGraph

  let graph_attributes _ = []
  let edge_attributes (_, _, _) = []
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes _ = [`Shape `Box]
  let vertex_name v = "\"" ^ OGSyntax.print_op_stmt v ^ "\""
  let default_vertex_attributes _ = []
end

module StDot = Graph.Graphviz.Dot (StDotConfig)

let log_lucid_osg fn (osg : OGSyntax.prog_opgraph) =
  let full_fn = !BackendLogging.irLogDir ^ "/" ^ fn in
  let combined_opgraph = OGSyntax.combine_opgraph osg in
  StDot.output_graph (Caml.open_out_bin full_fn) combined_opgraph
;;

(**** Print Lir program ****)
module DotConfig = struct
  include DFSyntax.G (* use the graph module from above *)

  let graph_attributes _ = []
  let edge_attributes (_, _, _) = []
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes _ = [`Shape `Box]
  let vertex_name v = P4tPrint.str_of_private_oid v
  let default_vertex_attributes _ = []
end

module Dot = Graph.Graphviz.Dot (DotConfig)

(* log the current program in lucid ir (graph + node definitions) *)
let log_lir_objs fn cid_decls =
  let full_fn = !BackendLogging.irLogDir ^ "/" ^ fn ^ ".lir" in
  Printf.fprintf (open_out full_fn) "%s" (DebugPrint.str_of_cid_decls cid_decls)
;;

let log_lir_dag fn dag =
  let full_fn = !BackendLogging.irLogDir ^ "/" ^ fn ^ ".dot" in
  Dot.output_graph (open_out_bin full_fn) dag
;;

let log_lir fn (dagProg:DFSyntax.dagProg) =
  log_lir_objs fn dagProg.dp_instr_dict;
  log_lir_dag fn dagProg.dp_g;
;;

(**** Print pipeline ****)
let log_lir_pipe fn pipe =
  let full_fn = !BackendLogging.irLogDir ^ "/" ^ fn ^ ".pipe.lir" in
  Printf.fprintf (open_out full_fn) "%s" (PipeSyntax.dbgstr_of_pipe pipe);
  let summary_fn = !BackendLogging.irLogDir ^ "/" ^ fn ^ ".pipe.summary.lir" in
  Printf.fprintf (open_out summary_fn) "%s" (PipeSyntax.summary_of_pipe pipe)
;;
