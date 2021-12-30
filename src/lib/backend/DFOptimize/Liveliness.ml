(* liveliness and variable interference analysis over a DAG program *)
open Graph
open Format
module CL = Caml.List
open LLSyntax
open MiscUtils
open DFSyntax

(* open P4IdPrint *)
open DebugPrint
open MiscUtils


(* logging *)
module DBG = BackendLogging
let outc = ref None
let dprint_endline = ref DBG.no_printf
let start_logging () = DBG.start_mlog __FILE__ outc dprint_endline

type varMap = (oid * mid list) list

type liveMaps =
  { lin : varMap
  ; lout : varMap
  ; def : varMap
  ; use : varMap
  }

let empty_varmap tg =
  G.fold_vertex (fun tid live_map -> (tid, []) :: live_map) tg []
;;

let def_or_use_varmap cid_decls tg getvars =
  G.fold_vertex (fun tid use -> (tid, getvars cid_decls tid) :: use) tg []
;;

let init_livemap cid_decls tg : liveMaps =
  let empty_livemap =
    { lin = empty_varmap tg
    ; lout = empty_varmap tg
    ; def = def_or_use_varmap cid_decls tg write_vars_of_tbl
    ; use = def_or_use_varmap cid_decls tg read_vars_of_tbl
    }
  in
  empty_livemap
;;

(* update liveliness data for one node. *)
let live_fold_tid g tid (lm, is_converged) =
  let tmp_lin = Cid.lookup lm.lin tid in
  let tmp_lout = Cid.lookup lm.lout tid in
  let cur_def = Cid.lookup lm.def tid in
  (* calculate new_lin *)
  let filter_f lout_mid = not (CL.mem lout_mid cur_def) in
  let out_minus_def = CL.filter filter_f tmp_lout in
  let new_lin = unique_list_of (Cid.lookup lm.use tid @ out_minus_def) in
  (* calculate new_lout *)
  let fold_f succ_tid new_lout = Cid.lookup lm.lin succ_tid @ new_lout in
  let new_lout = unique_list_of (G.fold_succ fold_f g tid []) in
  (* update lin and lout in the assoc list *)
  let new_lm =
    { lin = Cid.replace lm.lin tid new_lin
    ; lout = Cid.replace lm.lout tid new_lout
    ; def = lm.def
    ; use = lm.use
    }
  in
  (* check if this node has converged in this iteration. *)
  let is_lin_eq = test_set_eq tmp_lin new_lin in
  let is_lout_eq = test_set_eq tmp_lout new_lout in
  new_lm, is_converged && is_lin_eq && is_lout_eq
;;

let rec live_analysis cid_decls g lm =
  (* do an iteration for all vertices *)
  let new_lm, is_converged = G.fold_vertex (live_fold_tid g) g (lm, true) in
  match is_converged with
  | true -> new_lm (* done *)
  | false -> live_analysis cid_decls g new_lm
;;

(* need another iteration *)

let str_of_livemap_entry livemap tid =
  let live_ins = Cid.lookup livemap.lin tid in
  let live_outs = Cid.lookup livemap.lout tid in
  let defs = Cid.lookup livemap.def tid in
  let uses = Cid.lookup livemap.use tid in
  let out_str =
    sprintf "tid: %s\n" (P4tPrint.str_of_private_oid tid)
    ^ sprintf "defs: %s\n" (str_of_cids defs)
    ^ sprintf "uses: %s\n" (str_of_cids uses)
    ^ sprintf "live in: %s\n" (str_of_cids live_ins)
    ^ sprintf "live out: %s\n" (str_of_cids live_outs)
  in
  out_str
;;

let dump_livemap_entries tids livemap =
  let iter_f tid =
    DBG.printf outc "------\n%s-----\n" (str_of_livemap_entry livemap tid)
  in
  CL.iter iter_f tids
;;

let analyze_liveliness cid_decls =
  let tg = table_dag_of cid_decls in
  let livemap = init_livemap cid_decls tg in
  let livemap = live_analysis cid_decls tg livemap in
  let tids = tids_of_declmap cid_decls in
  !dprint_endline "liveliness analysis complete.";
  dump_livemap_entries tids livemap;
  livemap
;;

(* ********** interference graph ********** *)

(* undirected graph and dot printer *)
module UG = Graph.Persistent.Graph.ConcreteLabeled (Node) (Edge)

module DotConfig = struct
  include UG

  let graph_attributes _ = []
  let edge_attributes (_, _, _) = []
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes _ = [`Shape `Box]
  let vertex_name v = P4tPrint.str_of_private_oid v
  let default_vertex_attributes _ = []
end

module UGDot = Graph.Graphviz.Dot (DotConfig)

let dump_undirected (g : UG.t) fn =
  DBG.printf outc "dumping undirected graph to: %s\n" fn;
  UGDot.output_graph (open_out_bin fn) g
;;

(* does the table just set a = b? *)
let copy_var_opt_of cid_decls tid =
  let oids = oids_of_tid cid_decls tid in
  let objs = CL.map (Cid.lookup cid_decls) oids in
  (* find all the instructions in the table of the form lvar = rvar; *)
  let map_f obj =
    match obj with
    | InstrVec (_, [IAssign (lvar, Oper (Meta rvar))]) -> Some (lvar, rvar)
    | _ -> None
  in
  let op_vars = CL.map map_f objs in
  (* if all the instructions in the table reduce to setting exactly 1 lvar = rvar, 
	then the table is a copy table. Else its not.*)
  let op_vars = unique_list_of op_vars in
  match op_vars with
  | [copy_var_pair] -> copy_var_pair
  | _ -> None
;;

let interference_g_of_livemap cid_decls livemap : UG.t =
  (* 
		create a graph over variables, with edge (a, b) representing 
		that a and b are alive at the same time in the program. 
	*)
  let mids = mids_of_declmap cid_decls in
  let g = CL.fold_left (fun g tid -> UG.add_vertex g tid) UG.empty mids in
  (* add edges for all pairs of variables active at lout[tid]*)
  let add_conflicts_after_tbl g tid =
    (* check if tid is a copy table. If so, get the lhs and rhs of the copy vars so 
		that we don't add this false dependency. *)
    let copy_pair_opt = copy_var_opt_of cid_decls tid in
    let concurrently_live_vars = Cid.lookup livemap.lout tid in
    (* fold over all pairs of variables in lout[tid]*)
    let deps = get_all_pairs concurrently_live_vars concurrently_live_vars in
    let add_conflict_pair g conc_live_var_pair =
      let a, b = conc_live_var_pair in
      match a = b with
      | true -> g (* dont add edges from a node to a node. *)
      | false ->
        (match copy_pair_opt with
        | Some (a_cpy, b_cpy) ->
          (match a = a_cpy && b = b_cpy with
          | true -> g
          | false -> UG.add_edge g a b)
        | _ -> UG.add_edge g a b)
    in
    CL.fold_left add_conflict_pair g deps
  in
  let g = CL.fold_left add_conflicts_after_tbl g (tids_of_declmap cid_decls) in
  g
;;

(* ********** module functions ********** *)
let interf_g_of_dmap external_vars cid_decls =
  let interf_g =
    analyze_liveliness cid_decls |> interference_g_of_livemap cid_decls
  in
  (* add conflict edges between all pairs of parameter variables *)
  let external_var_pairs =
    get_unique_unordered_pairs external_vars external_vars
  in
  let fold_f g (s, t) =
    match s = t with
    | false -> UG.add_edge g s t
    | true -> g
    (* don't add self edges *)
  in
  let interf_g = CL.fold_left fold_f interf_g external_var_pairs in
  dump_undirected
    interf_g
    (!BackendLogging.graphLogDir ^ "/interference_graph.dot");
  interf_g
;;
