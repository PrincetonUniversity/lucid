(* build an operation graph from dpt statements *)
(* an op statement is a statement that does computation or moves data. 
   basically anything besides a noop or sequence. *)
open Syntax
open Batteries
open InterpHelpers
module CL = Caml.List

(* logging *)
module DBG = BackendLogging
let outc = ref None 
let dprint_endline = ref DBG.no_printf
;;


let print_op_stmt stmt = 
  match stmt.s with 
  | SSeq _ -> error "[print_op_stmt] sequence is not an op statement!"
  | SMatch _ -> error "[print_op_stmt] match is not an op statement!"
  | SIf(e, _, _) -> "[SIf] ("^(Printing.exp_to_string e)^")"
  | SNoop -> "[Op] "^"\""^(Printing.stmt_to_string stmt)^"---"^(Span.to_string stmt.sspan)^"\""
  | _ -> "[Op] "^(Printing.stmt_to_string stmt)
;;

let rec print_stmt_tree_recurse (st:statement) d = 
  match st.s with 
    | SSeq(a, b) ->
      !dprint_endline (PrintUtils.indent_block ~nspaces:d "[SSeq]");
      print_stmt_tree_recurse a (d+2);
      print_stmt_tree_recurse b (d+2);
    | SIf(_, a, b) ->
      !dprint_endline (PrintUtils.indent_block ~nspaces:d (print_op_stmt st));
      print_stmt_tree_recurse a (d+2);
      print_stmt_tree_recurse b (d+2);
    | _ ->
      !dprint_endline (PrintUtils.indent_block ~nspaces:d (print_op_stmt st));      
;;


let rec print_stmt_edges (edges:(statement * statement) list) : unit = 
  match edges with 
    | (a, b)::edges -> (      
      let print_str = match (a.s, b.s) with 
        | (SSeq (_), _) | (_, SSeq (_)) -> "UNEXPECTED -- A SEQUENCE IN AN EDGE!"
        | _ -> Printf.sprintf "edge: %s --> %s\n" (print_op_stmt a) (print_op_stmt b)
      in 
      !dprint_endline print_str;
      print_stmt_edges edges
    )
    | _ -> ()
;;


(* a graph of statements *)
module StNode = struct
   type t = statement
   let compare = Pervasives.compare
   let hash = Hashtbl.hash
   let equal = (=)
end
module StEdge = struct
   type t = string
   let compare = Pervasives.compare
   let equal = (=)
   let default = ""
end
module StGraph = Graph.Persistent.Digraph.ConcreteLabeled(StNode)(StEdge)

module StDotConfig = struct
   include StGraph 
   let graph_attributes _ = []
   let edge_attributes (_, _, _) = []
   let default_edge_attributes _ = []
   let get_subgraph _ = None
   let vertex_attributes _ = []
   let vertex_name v = "\""^(Printing.stmt_to_string v)^"---"^(Span.to_string v.sspan)^"\""
   let default_vertex_attributes _ = []
end 

module StDot = Graph.Graphviz.Dot(StDotConfig)

let dump_opstmt_graph (g:StGraph.t) fn = 
  let fn = !BackendLogging.graphLogDir ^ "/"^fn in 
  DBG.printf outc "dumping undirected graph to: %s\n" fn;
  StDot.output_graph (Caml.open_out_bin fn) g
;;

(* the opstatement graph for a single handler *)
type handler_opgraph_rec = {
  h_name   : id;
  h_root   : statement;
  h_opgraph : StGraph.t;
}

(* the first and last statement in a sequence *)
let fst_op_stmt (st:statement) = 
  match st.s with 
    | SSeq _ ->  Some (CL.hd (unfold_stmts st))
    | SNoop -> None
    | _ -> Some st
;;
let lst_op_stmt (st:statement) = 
  match st.s with 
    | SSeq _ ->  Some (CL.hd (CL.rev (unfold_stmts st)))
    | SNoop -> None
    | _ -> Some st
;;
(* find the pair of statements that link two branches of a sequence tree. *)
(* connect two sibling branches: 
  e.g., generate the edge b --> c in the tree below: 
  SSeq
    SSeq
      a
      b
    SSeq
      c
      d
*)
let immediate_op_pair parent child = 
  match (lst_op_stmt parent, fst_op_stmt child) with 
    | (Some parent, Some child) -> [(parent, child)]
    | _ -> [] (* this happens if parent or child is a noop *)

(* let fst_op_of_branch st = 
  match fst_op_stmt st with 
    | Some s -> s
    | None -> snoop
;;
 *)
(* need to also support op statements here.  *)
let fst_op_of_branch st = 
  match st.s with 
    | SSeq _ -> (CL.hd (unfold_stmts st))
    | _ -> st
;;

(** these are better versions of the above functions for connecting sibling branches **)
let rec op_stmt_root (st:statement) = 
  (CL.hd (unfold_stmts st))
;;
(* get the leaves of a statement tree. *)
let rec op_stmt_leaves (st:statement) = 
  match st.s with 
    | SSeq(a, b) -> (op_stmt_leaves a)@(op_stmt_leaves b)
    | SIf(_, a, b) -> (op_stmt_leaves a)@(op_stmt_leaves b)
    | SMatch(_, bs) -> (
      let map_f (_, st) = op_stmt_leaves st in 
      (CL.map map_f bs |> CL.flatten)
    )
(*     | SNoop -> [] (* skip noops *) *)
    | _ -> [st]
;;
(* return edges linking two sibling branches *)
let connect_siblings left right = 
  let left_leaves = op_stmt_leaves left in 
  let right_root = op_stmt_root right in 
  CL.map (fun st -> (st, right_root)) left_leaves
;;

(* remove all noops except those that occur at the beginning of an empty branch *)
let rec remove_interior_noops (st:statement) =
  match st.s with 
    | SIf(e, a, b) -> 
      sifte_sp e (remove_interior_noops a) (remove_interior_noops b) st.sspan
    | SMatch(e, branches) ->
      let map_f (b_e, b_s) = 
        (b_e, remove_interior_noops b_s)
      in 
      {st with s=SMatch(e, CL.map map_f branches)}
    | SSeq(a, b) -> (
      let a = remove_interior_noops a in 
      let b = remove_interior_noops b in       
      match (a, b) with 
        | ({s=SNoop; _}, {s=SNoop; _}) -> snoop_sp st.sspan
        | (st_sub, {s=SNoop; _})
        | ({s=SNoop; _}, st_sub) -> st_sub
        | _ -> sseq_sp a b st.sspan
    )
    | _ -> st
;;

(* Make noops unique, with spans, for now. *)
let cur_span = ref 0 ;;
let fresh_span () = 
  cur_span := (!cur_span) + 1;
  {Span.fname = (string_of_int !cur_span); Span.start= -1; Span.finish = -1}
;;
let unique_noop_spans st = 
  let v = object 
    inherit [_] s_map as super 
      method !visit_statement ctx st = 
        let new_st = super#visit_statement ctx st in 
        match new_st.s with 
        | SNoop -> snoop_sp (fresh_span ())
        | _ -> new_st
    end
  in 
  v#visit_statement () st 
;;

let print_fst_in_seq (st:statement) = 
  let fst_op_stmt = CL.hd (unfold_stmts st) in 
  Printing.stmt_to_string fst_op_stmt
;;



(* missing: logic to connect the last node of each branch 
  to the next node after the SIf. examples: 
  -----
  SSeq
    SIf
      a
      b
    c
  -----
  SSeq
    SSeq
      _  
      SIf
        a
        b
    c

example 1:
  - you'll get the edge from SIf --> c in the outer SSeq via immediate_op_pair
  - in immediate_op_pair, if the last left statement is an SIf, then...
  - 

*)
let rec to_op_edges (st:statement) : (statement * statement) list = 
  let resulting_edges = match st.s with 
  | SIf(_, a, b) -> 
    (* connect if to first statement in each branch. recurse on branches. *)
    (* let self_to_a = (immediate_op_pair st a) in 
    let self_to_b = (immediate_op_pair st b) in  *)
    let edges_of_a = to_op_edges a in 
    let edges_of_b = to_op_edges b in 
    [(st, fst_op_of_branch a); (st, fst_op_of_branch b)]
    @edges_of_a
    @edges_of_b
  | SMatch(_, branches) -> 
    let map_start_f (_, branch_st) = 
      immediate_op_pair st branch_st
    in 
    let map_continue_f (_, branch_st) = 
      to_op_edges branch_st
    in 
    (CL.map map_start_f branches)
    @(CL.map map_continue_f branches)
    |> CL.flatten
  | SSeq(a, b) ->
    (* recurse on a, connect a to b, recurse on b.*)
    let edges_of_a = to_op_edges a in 
    let a_to_b_edge = connect_siblings a b in 
    let edges_of_b = to_op_edges b in 
    edges_of_a@a_to_b_edge@edges_of_b
  | _ -> !dprint_endline ("HERE: "^(print_op_stmt st));

  [] (* none of the other statements produce edges *)
  in 

  !dprint_endline ("----------[to_op_edges]-----------");
  !dprint_endline ("---- statement ----");
  !dprint_endline (Printing.stmt_to_string st);
  !dprint_endline ("---- statement tree ----");
  print_stmt_tree_recurse st 0;
  !dprint_endline ("----resulting edges -----");
  print_stmt_edges resulting_edges;

  resulting_edges

;;

let rec to_op_vertices (st:statement) : (statement list) = 
  let resulting_vertices = match st.s with 
    | SIf(_, a, b) -> 
      (st)::(to_op_vertices a)@(to_op_vertices b)
    | SMatch(_, branches) -> 
      let map_f (_, branch_st) = 
        to_op_vertices branch_st
      in 
      (st)::(CL.map map_f branches |> CL.flatten)
    | SSeq(a, b) -> (to_op_vertices a)@(to_op_vertices b)
    | _ -> [st] (* base case: don't skip *)
  in 
  !dprint_endline ("---------[to_op_vertices]----------");
  CL.iter (fun v -> (!dprint_endline (print_op_stmt v))) resulting_vertices;
  resulting_vertices
;;

let start_log () = 
  DBG.start_mlog __FILE__ outc dprint_endline;
;;

let opgraph_from_handler  (dec:decl) = 

  match dec.d with 
    | DHandler(hdl_id, (_, hdl_stmt)) -> (
      (* the actual work *)
      let hdl_stmt = remove_interior_noops hdl_stmt in 
      let hdl_stmt = unique_noop_spans hdl_stmt in 
      !dprint_endline ("***** [extracting edges] *****");
      let edges = to_op_edges hdl_stmt in 
      !dprint_endline ("***** [extracting vertices] *****");
      let vertices = to_op_vertices hdl_stmt in 
      (* add vertices and edges *)
      let g = CL.fold_left StGraph.add_vertex (StGraph.empty) vertices in 
      let fold_edge_f g (s, d) = StGraph.add_edge g s d in 
      let g = CL.fold_left fold_edge_f g edges in 
      (* packaging *)
      let root, g = match (fst_op_stmt hdl_stmt) with 
        | None -> 
          !dprint_endline ("handler "^(Id.to_string hdl_id)^" has no statements in it. This is untested!");
          snoop, StGraph.empty
        | Some root -> root, g
      in 
      let hdl_name = (Id.name hdl_id) in 
      let opstmt_dbg_graph_fn = (hdl_name^"_opstmtgraph.dot") in 
      !dprint_endline ("dumping opstatement graph to: "^opstmt_dbg_graph_fn);
      dump_opstmt_graph g opstmt_dbg_graph_fn;
      Some ({h_name = hdl_id; h_root = root; h_opgraph = g;}) 
    )
    | _ -> None    
;;