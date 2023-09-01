(* 
  This pass finds expressions of the form 
  out_var := intermediate_var; 

  where out_var is an output event parameter and intermediate_var is 
  some intermediate variable that is only ever used to set out_var.

  It optimized the program by replacing intermediate_var with out_var 
  in all lhs ids (i.e., the left hand side of an assign), 
  deleting the declaration of intermediate_var, and deleting 
  the statement that sets out_var := intermediate_var.

  The pass does the optimization recursively, so if you have: 
  // source:
  a = b;
  c = b;
  d = c;
  out = d;
  // result
  out = b;
  (the same as inlining, in this case)

  // an example where this pass is different from inlining:
  //source:
  b := a;
  if (...) {
    c := b;
  } else {
    c := 1;
  }
  out := c;

  // result:
  b := a;
  if (...) {
    out := b;
  } else {
    out := 1;
  }
*)

(* bug: this pass is dropping Array ops? *)

open TofinoCoreNew
module Ht = Caml.Hashtbl
open Batteries

(* logging *)
module DBG = BackendLogging
let outc = ref None
let dprint_endline = ref DBG.no_printf
let start_logging () = DBG.start_mlog (!IoUtils.moduleLogDir) __FILE__ outc dprint_endline

let str_comp to_str a b =
  String.compare (to_str a) (to_str b)
;;

(* the graph structure *)
module Node = struct
  type t = 
    | Input of cid
    | Output of cid
    | Intermediate of cid
  let to_string tv = 
    match tv with
      | Input cid -> ("IN:"^CorePrinting.cid_to_string cid)
      | Output cid -> ("OUT:"^CorePrinting.cid_to_string cid)
      | Intermediate cid -> CorePrinting.cid_to_string cid
  let to_cid tv = 
    match tv with
      | Input cid
      | Output cid
      | Intermediate cid -> cid
  let compare = (str_comp to_string)
  let hash = Hashtbl.hash
  let equal = (fun n1 n2 -> Cid.equal (to_cid n1) (to_cid n2))
end

module Edge = struct
  type t = CoreSyntax.exp
  let compare = (str_comp CorePrinting.exp_to_string)
  let hash = Hashtbl.hash
  let equal = CoreSyntax.equiv_exp
  let default = CoreSyntax.value_to_exp (CoreSyntax.vbool false)
  let to_string exp = match exp.e with
    | EVar(_) -> "EVAR"
    | _ -> ""
end
module G = Graph.Persistent.Digraph.ConcreteLabeled (Node) (Edge)


module GDotCfg = struct
  include G
  let graph_attributes _ = []
  let edge_attributes (_, (exp:(Edge.t)), _) = 
    [`Label (Edge.to_string exp)]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes _ = [`Shape `Box]
(*     match v with
    | Intermediate(_) -> [`Shape `Box]
    | _ -> [`Shape `Circle] *)
  let vertex_name v = "\"" ^Node.to_string v^ "\""
  let default_vertex_attributes _ = []
end

module CDot = Graph.Graphviz.Dot (GDotCfg)
module GTopo = Graph.Topological.Make (G)

let reverse_edges (g: G.t) : G.t =
  let g' = G.empty in
  let g' = G.fold_vertex (fun v g' -> G.add_vertex g' v) g g' in
  let g' = G.fold_edges_e (fun (v1, e, v2) g' -> G.add_edge_e g' (v2, e, v1)) g g' in
  g'
;;

let g_to_string g =
  let buffer = Buffer.create 0 in
  let formatter = Format.formatter_of_buffer buffer in
  CDot.fprint_graph formatter g;
  Format.pp_print_flush formatter ();
  Buffer.contents buffer
;;

let write_string_to_file filename str =
  Base.output_string (Base.open_out filename) str
;;

let g_to_dotfile g fn =
  write_string_to_file ((!IoUtils.graphLogDir)^"/"^fn) (g_to_string g)
;;


(**  statement to graph translation **)
let exp_to_cids exp =
  let rhs_cids = ref [] in
  let v = object (_)
    inherit [_] s_iter as super
    method! visit_EVar () cid = 
      rhs_cids := cid::(!rhs_cids)
    end
  in
  v#visit_exp () exp;
  !rhs_cids
;;

(* convert the cid to a tagged var, where the 
   tag is based on whether the cid is in the 
   input or output event. *)
let cid_to_taggedvar input_event output_event cid = 
  let input_event_id = id_of_event input_event in
  let output_event_id = id_of_event output_event in
  let ids = Cid.to_ids cid in
  let contains xs x = List.exists (fun x' -> Id.equal x' x) xs in
  if (contains ids input_event_id) 
  then (Node.Input(cid))
  else (
    if (contains ids output_event_id) 
    then (Output(cid))
    else (Intermediate(cid))) 
;;

let rec stmt_to_g input_event output_event g stmt =
  let tagcid = cid_to_taggedvar input_event output_event in
  match stmt.s with
  | SAssign(cid, exp) -> 
    List.fold_left 
      (fun g rhs_cid -> 
        G.add_edge_e g (tagcid rhs_cid, exp, tagcid cid))
      g
      (exp_to_cids exp)  
  | SLocal(id, _, exp) -> 
    let cid = Cid.id id in
    List.fold_left 
      (fun g rhs_cid -> 
        G.add_edge_e g (tagcid rhs_cid, exp, tagcid cid))
      g
      (exp_to_cids exp)        
  | SSeq(s1, s2)
  | SIf(_, s1, s2) -> 
    let g = stmt_to_g input_event output_event g s1 in
    stmt_to_g input_event output_event g s2
  | SMatch(_, branches) -> 
    List.fold_left
      (fun g (_, stmt) ->
        stmt_to_g input_event output_event g stmt) 
      g
      branches
  | _ -> g (* no changes *)
;;

let var_dfg input_event output_event stmt =
  stmt_to_g input_event output_event G.empty stmt
;;

(* compute a list of optimizations to perform on a program, using the dfg.
    each optimization contains: 
      1) an output variable o; 
      2) an intermediate i
    to perform the optimization, transform:
      SAssign(i, ...) -> SAssign(o, ...)
      SLocal(i, ...) -> SAssign(o, ...)
      i.e., lhs[i/o]

  
  A valid optimization has to have a path from rhs -> edge node where 
  each edge is an evar and each node has only 1 successor.
 *)

(* is it safe to replace the intermediate node 
   with the output node, everywhere the intermediate 
   occurs? *)
let rec valid_path cur fin g =
  (* cur must be an intermediate. *)
  match cur with 
  | Node.Intermediate(_) -> (
    let out_es = G.succ_e g cur in
    (* one out edge *)
    match out_es with
    | [(_, exp, next)] -> (
      (* data flows with an EVar *)
      match exp.e with
      | EVar(_) -> (
        (* next node is fin, then we're done *)
        if (Node.equal next fin) then true
        else 
        (* otherwise, recurse *)
        valid_path next fin g
      )
      | _ -> false
    )
    | _ -> false
  )
  | _ -> false
;;

let rec collect_valid_optimizations src_var out g rev_g =
  (* if there's a valid path, then we can add this node 
     and recurse on its children in rev_g *)
(*   !dprint_endline ("checking if there is a valid optimization from");
  !dprint_endline (Node.to_string src_var);
  !dprint_endline (Node.to_string out); *)
  if (valid_path src_var out g)
  then (
    let next_vars = G.succ rev_g src_var in
(*     !dprint_endline ("next up: ");
    List.iter (fun src_var -> !dprint_endline (Node.to_string src_var)) next_vars; *)
    let opts = List.map
      (fun next_var -> collect_valid_optimizations next_var out g rev_g)
      next_vars
      |> List.flatten
    in
    (src_var, out)::opts
  )
  else ([])
;;

let compute_optimizations dfg =
  let rdfg = reverse_edges dfg in
  let outs = G.fold_vertex
    (fun v outs -> 
      match v with
      | Output(_) -> v::outs
      | _ -> outs)
    dfg
    []
  in
  let opt_list = List.fold
   (fun opt_list out -> 
      (* the vars that out gets set to *)
      let src_vars = G.succ rdfg out in
      let new_opts = List.map 
        (fun src_var -> collect_valid_optimizations src_var out dfg rdfg)
        src_vars
      |> List.flatten in
      opt_list@new_opts)
   []
   outs
  in
  List.map
    (fun (inter_node, out_node) -> 
      match (inter_node, out_node) with
      | Node.Intermediate(icid), Node.Output(ocid) -> (icid, ocid)
      | _, _ -> failwith "[compute_optimizations] computed an illegal optimization"
    )
  opt_list
;;

let do_optimization stmt (inter_var, output_var) = 
  (* do one optimization in the body of a handler
     1. replace all assignments to inter_var with assignments to output var. 
     2. replace declaration of inter_var with an assignment to output var.
     3. delete all statements with rhs expressions that contain inter_var -- 
        by definition, these should all be EVar expressions.*)
  let v = object (_)
    inherit [_] s_map as super  

    method! visit_SAssign () cid exp = 
      (* 3. *)
      match exp.e with
      | EVar(rcid) when (Cid.equal rcid inter_var) -> SNoop
      | _ ->
        (* 1. *)
        if (Cid.equal cid inter_var) 
        then (SAssign(output_var, exp))
        else (SAssign(cid, exp))
    method! visit_SLocal () id ty exp =
      (* 3. *)
      match exp.e with
      | EVar(rcid) when (Cid.equal rcid inter_var) -> SNoop
      | _ ->
        (* 2. *)
        if (Cid.equal (Cid.id id) inter_var) 
        then (
          !dprint_endline ("replacing SLocal with noop");
          !dprint_endline (CorePrinting.s_to_string (SLocal(id, ty, exp)));  
          SAssign(output_var, exp))
          (* SNoop) *)
        else (SLocal(id, ty, exp))
    end
  in
  v#visit_statement () stmt
;;

let do_optimizations = (List.fold_left do_optimization)
;;

let process_comp comp =
    !dprint_endline ("on component: "^(CorePrinting.id_to_string comp.comp_id));
    match comp.comp_sort with 
    | HControl -> comp
    | _ -> 
    begin
      let main = main_handler_of_component comp in
      let main_stmt = match main.hdl_body with
        | SFlat(stmt) -> stmt
        | _ -> failwith "error -- program must be in flat form (pre layout)"
      in
      (* 1. get the dfg *)
      let g = var_dfg main.hdl_input main.hdl_output main_stmt in
      (* some debugging *)
      g_to_dotfile g ((comp.comp_id |> CorePrinting.id_to_string)^"_var_dataflow.dot");

      (* 2. get the list of valid optimizations. Each optimization is a tuple (inter_var, out_var) 
            and means it is safe to replace inter_var with out_var wherever it appears. *)
      let optimizations = compute_optimizations g in
      !dprint_endline ("computed optimizations:");
      List.iter (fun (inter,output) -> 
        (CorePrinting.cid_to_string inter)^", "
        ^(CorePrinting.cid_to_string output) |> !dprint_endline)
      optimizations;

      (* 3. perform the replacements *)
      let main_stmt' = do_optimizations main_stmt optimizations in
      (* 4. see how the optimized dfg looks *)
      let g = var_dfg main.hdl_input main.hdl_output main_stmt' in
      g_to_dotfile g ((comp.comp_id |> CorePrinting.id_to_string)^"_var_dataflow_OPTIMIZED.dot");

      (* 5. done, reinsert updated stuff *)
      let main' = {main with hdl_body=SFlat(main_stmt')} in 
      replace_main_handler_of_component comp main'
    end
;;

let process core_prog =
  List.map process_comp core_prog
;;