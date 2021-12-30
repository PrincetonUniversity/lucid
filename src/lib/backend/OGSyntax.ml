(* An intermediate representation of handlers in a lucid program,
   as a graph of statements instead of a tree. Used in
 translation from Syntax --> LLSyntax.*)
(* nodes in the graph are "op statements" -- statements that
do computation or move data, basically anything besides a noop or sequence. *)
open CoreSyntax
open Batteries
open InterpHelpers
open Printf
module CL = Caml.List

(* logging *)
module DBG = BackendLogging

let outc = ref None
let dprint_endline = ref DBG.no_printf

let print_op_match exps (branches : branch list) =
  let pats_list = CL.map fst branches in
  let fold_f out_str (pats : pat list) =
    let to_str (exp, pat) =
      Printf.sprintf
        "%s : %s;"
        (Printing.exp_to_string exp)
        (Printing.pat_to_string pat)
    in
    out_str
    ^ "\n----\n"
    ^ (CL.combine exps pats |> CL.map to_str |> String.concat "\n")
  in
  CL.fold_left fold_f "[SMatch]" pats_list
;;

let print_op_stmt stmt =
  let codestr = match stmt.s with
    | SSeq _ -> error "[print_op_stmt] sequence is not an op statement!"
    | SMatch (exps, branches) -> print_op_match exps branches
    | SIf (e, _, _) -> "[SIf] (" ^ Printing.exp_to_string e ^ ")"
    | SNoop ->
      "[Op]" ^ Printing.stmt_to_string stmt ^ "---" ^ Span.to_string stmt.sspan
    | _ -> "[Op] " ^ Printing.stmt_to_string stmt
  in 
  let idstr = string_of_int stmt.sspan.spid in 
  codestr^" //[id="^idstr^"]"
;;

let rec print_stmt_tree_recurse (st : statement) d =
  match st.s with
  | SSeq (a, b) ->
    !dprint_endline (PrintUtils.indent_block ~nspaces:d "[SSeq]");
    print_stmt_tree_recurse a (d + 2);
    print_stmt_tree_recurse b (d + 2)
  | SIf (_, a, b) ->
    !dprint_endline (PrintUtils.indent_block ~nspaces:d (print_op_stmt st));
    print_stmt_tree_recurse a (d + 2);
    print_stmt_tree_recurse b (d + 2)
  | _ -> !dprint_endline (PrintUtils.indent_block ~nspaces:d (print_op_stmt st))
;;

let rec print_stmt_edges (edges : (statement * statement) list) : unit =
  match edges with
  | (a, b) :: edges ->
    let print_str =
      match a.s, b.s with
      | SSeq _, _ | _, SSeq _ -> "UNEXPECTED -- A SEQUENCE IN AN EDGE!"
      | _ ->
        Printf.sprintf "edge: %s --> %s\n" (print_op_stmt a) (print_op_stmt b)
    in
    !dprint_endline print_str;
    print_stmt_edges edges
  | _ -> ()
;;

(* a graph of statements *)
module StNode = struct
  type t = statement

  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = ( = )
end

module StEdge = struct
  type t = string

  let compare = Pervasives.compare
  let equal = ( = )
  let default = ""
end

module StGraph = Graph.Persistent.Digraph.ConcreteLabeled (StNode) (StEdge)

(* the opstatement graph for a single handler *)
type handler_opgraph_rec =
  { h_name : id
  ; h_root : statement
  ; h_opgraph : StGraph.t
  }

(* the opstatement graph for all the handlers in a program *)
type prog_opgraph = handler_opgraph_rec list

(* merge all the ophandlers into one graph
-- just meant for debugging / logging. *)
let combine_graphs g1 g2 =
  let add_edge s d g = StGraph.add_edge g s d in
  StGraph.fold_edges add_edge g2 g1
;;

let combine_opgraph prog_opgraph =
  let fold_in_opgrec merged_opgraph ogrec =
    combine_graphs merged_opgraph ogrec.h_opgraph
  in
  CL.fold_left fold_in_opgrec StGraph.empty prog_opgraph
;;

(* the first and last statement in a sequence *)
let fst_op_stmt (st : statement) =
  match st.s with
  | SSeq _ -> Some (CL.hd (unfold_stmts st))
  | SNoop -> None
  | _ -> Some st
;;

let lst_op_stmt (st : statement) =
  match st.s with
  | SSeq _ -> Some (CL.hd (CL.rev (unfold_stmts st)))
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
  match lst_op_stmt parent, fst_op_stmt child with
  | Some parent, Some child -> [parent, child]
  | _ -> []
;;

(* this happens if parent or child is a noop *)

(* let fst_op_of_branch st =
  match fst_op_stmt st with
    | Some s -> s
    | None -> snoop
;;
 *)
(* need to also support op statements here.  *)
let fst_op_of_branch st =
  match st.s with
  | SSeq _ -> CL.hd (unfold_stmts st)
  | _ -> st
;;

(** these are better versions of the above functions for connecting sibling branches **)
let rec op_stmt_root (st : statement) = CL.hd (unfold_stmts st)

(* get the leaves of a statement tree. *)
let rec op_stmt_leaves (st : statement) =
  match st.s with
  | SSeq (a, b) -> op_stmt_leaves a @ op_stmt_leaves b
  | SIf (_, a, b) -> op_stmt_leaves a @ op_stmt_leaves b
  | SMatch (_, bs) ->
    let map_f (_, st) = op_stmt_leaves st in
    CL.map map_f bs |> CL.flatten
  (*     | SNoop -> [] (* skip noops *) *)
  | _ -> [st]
;;



(* return edges linking two sibling branches *)
(* 
left and right are two statements that appear in sequence. 
The goal is to find the last statement to execute in every 
control flow of left, the first statement to execute in 
right, and create an edge from each last statement of left 
to the first statement of right. 
LEFT OFF HERE. We don't actually do this because left_leaves 
is finding the leaves of the syntax tree, not the last 
statements to execute. 
*)
let connect_siblings_old left right =
  let left_leaves = op_stmt_leaves left in
  let right_root = op_stmt_root right in
  CL.map (fun st -> st, right_root) left_leaves
;;


(* find the non control statements 
  that execute last. *)
let rec last_to_execute (stmt : statement) : statement list = 
  match stmt.s with 
    | SSeq (_, b) -> last_to_execute b
    | SIf (_, a, b) -> (last_to_execute a)@(last_to_execute b)
    | SMatch (_, bs) -> 
      CL.map last_to_execute (CL.split bs |> snd) |> CL.flatten
    | _ -> [stmt]
;;
(* find the statement that executes first. 
   note that this may be a control statement, 
   just not a sequence. *)
let rec first_to_execute stmt = 
  match stmt.s with
    | SSeq (a, _) -> first_to_execute a
    | _ -> stmt

let connect_siblings left right =
  let left_leaves = last_to_execute left in
  let right_root = first_to_execute right in
  CL.map (fun st -> st, right_root) left_leaves
;;


(* remove all noops except those that occur in an empty branch *)
let rec remove_interior_noops (st : statement) =
  match st.s with
  | SIf (e, a, b) ->
    sifte_sp e (remove_interior_noops a) (remove_interior_noops b) st.sspan
  | SMatch (e, branches) ->
    let map_f (b_e, b_s) = b_e, remove_interior_noops b_s in
    { st with s = SMatch (e, CL.map map_f branches) }
  | SSeq (a, b) ->
    let a = remove_interior_noops a in
    let b = remove_interior_noops b in
    (match a, b with
    | { s = SNoop; _ }, { s = SNoop; _ } -> snoop_sp st.sspan
    | st_sub, { s = SNoop; _ } | { s = SNoop; _ }, st_sub -> st_sub
    | _ -> sseq_sp a b st.sspan)
  | _ -> st
;;

let print_fst_in_seq (st : statement) =
  let fst_op_stmt = CL.hd (unfold_stmts st) in
  Printing.stmt_to_string fst_op_stmt
;;

let rec to_op_edges (st : statement) : (statement * statement) list =
  print_endline ("[to_op_edges] processing statement -----\n"
    ^(CorePrinting.statement_to_string st)
    ^"\n-------"
  );
  let resulting_edges, base_edges_dbg, dbg_str =
    match st.s with
    | SIf (_, a, b) ->
      (* connect if to first statement in each branch. recurse on branches. *)
      (* let self_to_a = (immediate_op_pair st a) in
    let self_to_b = (immediate_op_pair st b) in  *)
      let edges_of_a = to_op_edges a in
      let edges_of_b = to_op_edges b in
      let base_edges = [st, fst_op_of_branch a; st, fst_op_of_branch b] in 
      base_edges @ edges_of_a @ edges_of_b, base_edges, ""
    | SMatch (_, branches) ->
      (* connect each branch to the first op statement in the branch *)
      let map_start_f (_, branch_st) = [st, fst_op_of_branch branch_st] in
      (* recurse on each branch. *)
      let map_continue_f (_, branch_st) = to_op_edges branch_st in
      let base_edges = CL.map map_start_f branches |> CL.flatten in 
      (base_edges @ (CL.map map_continue_f branches |> CL.flatten)), base_edges, ""
    | SSeq (a, b) ->
      (* recurse on a, connect a to b, recurse on b.*)
      print_endline ("its a sequence.");
      print_endline ("first statement: ");
      print_endline ("****");
      CorePrinting.statement_to_string a |> print_endline;
      print_endline ("****");
      let edges_of_a = to_op_edges a in
      let a_to_b_edge = connect_siblings a b in    
      let a_leaves = op_stmt_leaves a in 
      let b_root = op_stmt_root b in   
      let dbg_str =  
        ("\na statement:\n"^CorePrinting.statement_to_string a)
        ^("\nb statement:\n"^CorePrinting.statement_to_string b)
        ^("\nSSeq: # a_to_b_edges: "^(CL.length a_to_b_edge |> string_of_int))
        ^("\nleaves of a:\n"
          ^(
            (CL.map 
            (fun st -> CorePrinting.statement_to_string st) a_leaves) 
            |> String.concat "\n "
          )
        )
        ^("\nroot of b:\n"
          ^(
            (CL.map 
            (fun st -> CorePrinting.statement_to_string st) [b_root]) 
            |> String.concat "\n "
          )
        )
      in 
      let edges_of_b = to_op_edges b in
      let base_edges = a_to_b_edge in 
      edges_of_a @ a_to_b_edge @ edges_of_b, base_edges, dbg_str
    | _ ->
      !dprint_endline ("HERE: " ^ print_op_stmt st);
      [], [], ""
    (* none of the other statements produce edges *)
  in
  !dprint_endline "----------[to_op_edges]-----------";
  !dprint_endline "---- statement ----";
  !dprint_endline (Printing.stmt_to_string st);
  !dprint_endline "---- statement tree ----";
  print_stmt_tree_recurse st 0;
  !dprint_endline "---- debug string ----";
  !dprint_endline dbg_str;
  !dprint_endline "----new BASE EDGES -----";
  print_stmt_edges base_edges_dbg;
  !dprint_endline "-------";
  !dprint_endline "----resulting edges -----";
  print_stmt_edges resulting_edges;
  !dprint_endline "-------";
  resulting_edges
;;

let rec to_op_vertices (st : statement) : statement list =
  let resulting_vertices =
    match st.s with
    | SIf (_, a, b) -> (st :: to_op_vertices a) @ to_op_vertices b
    | SMatch (_, branches) ->
      let map_f (_, branch_st) = to_op_vertices branch_st in
      st :: (CL.map map_f branches |> CL.flatten)
    | SSeq (a, b) -> to_op_vertices a @ to_op_vertices b
    | _ -> [st]
    (* base case: don't skip *)
  in
  !dprint_endline "---------[to_op_vertices]----------";
  CL.iter (fun v -> !dprint_endline (print_op_stmt v)) resulting_vertices;
  resulting_vertices
;;

let start_log () = DBG.start_mlog __FILE__ outc dprint_endline

(* make sure that every statement has a unique span *)
let check_unique_stmt_spans dec =
  let v =
    object
      inherit [_] s_map as super
      val mutable spans = []
      method spans = spans

      method! visit_statement ctx st =
        spans <- spans @ [st.sspan];
        super#visit_statement ctx st
    end
  in
  let dec = v#visit_decl () dec in
  let unique_spans = MiscUtils.unique_list_of v#spans in
  let n_spans = CL.length v#spans in
  let n_u_spans = CL.length unique_spans in
  (* print_endline (Printing.decl_to_string dec); *)
  (* print_endline (sprintf "# spans: %i # unique spans: %i" n_spans n_u_spans); *)
  match n_spans = n_u_spans with
  | true -> dec
  | false -> error "there are duplicate spans."
;;

let opgraph_from_handler (dec : decl) =
  let dec = check_unique_stmt_spans dec in
  match dec.d with
  | DHandler (hdl_id, (_, hdl_stmt)) ->
    let hdl_stmt = remove_interior_noops hdl_stmt in
    !dprint_endline "***** [extracting edges] *****";
    let edges = to_op_edges hdl_stmt in
    !dprint_endline "***** [extracting vertices] *****";
    let vertices = to_op_vertices hdl_stmt in
    (* add vertices and edges *)
    let g = CL.fold_left StGraph.add_vertex StGraph.empty vertices in
    let fold_edge_f g (s, d) = StGraph.add_edge g s d in
    let g = CL.fold_left fold_edge_f g edges in
    (* packaging *)
    let root, g =
      match fst_op_stmt hdl_stmt with
      | None ->
        !dprint_endline
          ("handler "
          ^ Id.to_string hdl_id
          ^ " has no statements in it. This is untested!");
        snoop, StGraph.empty
      | Some root -> root, g
    in
    Some { h_name = hdl_id; h_root = root; h_opgraph = g }
  | _ -> None
;;
