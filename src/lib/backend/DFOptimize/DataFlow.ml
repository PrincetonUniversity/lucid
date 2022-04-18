open LLSyntax
open MiscUtils
open DFSyntax
module CL = Caml.List

(* convert a DAG with edges that represent call order
   into a DAG with edges that represent data dependencies. 
 we consider:
  (read, write) and
  (write, read) and 
  (write, write)
  dependencies. That is, 
 a table s must be placed before t if: 
  s reads or writes variables that t writes or 
  s writes variables that t reads
 we will be able to eliminate the (read, write) dependencies
 once we add SSA. *)

exception Error of string

let error s = raise (Error s)

(* logging *)
module DBG = BackendLogging
let outc = ref None
let dprint_endline = ref DBG.no_printf
let start_logging () = DBG.start_mlog __FILE__ outc dprint_endline

module CidMap = struct 
  type t = Cid.t
  let compare = Cid.compare
end

module UseMap = BatMap.Make(CidMap)

(* we need a use map (reads) and a def map (writes) *) 

let tid_vars cid_decls tid = 
  let lvars, rvars = lr_mids_of_tid cid_decls tid in 
  lvars@rvars
;;

(* record all the reads or writes *)
let record_uses_generic usefinder cid_decls usemap tid = 
  (* add tid to the list of tables that use var *)
  let record_use tid usemap var = 
    let users = UseMap.find_default [] var usemap in 
    let users = tid::users |> unique_list_of in 
    UseMap.add var users usemap
  in 
  CL.fold_left (record_use tid) usemap (usefinder cid_decls tid)
  (* for each variable v add v to CidMap[t] *)
;;
let record_reads = (record_uses_generic read_vars_of_tbl)
let record_writes = (record_uses_generic write_vars_of_tbl)

let build_readmap cid_decls = 
  CL.fold_left 
    (record_reads cid_decls) 
    UseMap.empty 
    (tids_of_declmap cid_decls)
;;
let build_writemap cid_decls = 
  CL.fold_left 
    (record_writes cid_decls) 
    UseMap.empty 
    (tids_of_declmap cid_decls)
;;

(* find the tables that must execute before tid in control graph g *)
let get_dependencies cid_decls readmap writemap g tid = 
  !dprint_endline ("[get_dependencies] start processing node: "^(P4tPrint.str_of_private_oids [tid]));
  let checker = PCheck.create g in (* path checker *)
  (* get write, read dependencies (tid reads) *)
  let read_by_tid = read_vars_of_tbl cid_decls tid in 
  !dprint_endline ("variables that the table reads: "^(P4tPrint.str_of_varids read_by_tid));
  let writer_tids = 
    CL.map
      (fun varid -> UseMap.find_default [] varid writemap)
      read_by_tid
    |> CL.flatten
    |> unique_list_of
    |> CL.filter (fun tidb -> tidb <> tid)
  in 
  !dprint_endline ("other tables that write these variables: "^(P4tPrint.str_of_private_oids writer_tids));
  (* which writers are preds of tid? *)
  let pred_writers = CL.filter 
      (fun tidb ->
        PCheck.check_path checker tidb tid
      )
      writer_tids
  in 
  !dprint_endline ("predecessors that write these variables: "^(P4tPrint.str_of_private_oids writer_tids));
  (* now get read, write dependencies (tid writes) *)
  let written_by_tid = write_vars_of_tbl cid_decls tid in 
  (* !dprint_endline ("variables that the table writes: "^(P4tPrint.str_of_private_oids written_by_tid)); *)
  let reader_tids = 
    CL.map
      (fun varid -> UseMap.find_default [] varid readmap)
      written_by_tid
    |> CL.flatten
    |> unique_list_of
    |> CL.filter (fun tidb -> tidb <> tid)
  in 
  let pred_readers = CL.filter
      (fun tidb ->
        PCheck.check_path checker tidb tid
      )
      reader_tids
  in 
  (* now get write, write dependencies *)
  (* variables written by tid *)
  let written_by_tid = write_vars_of_tbl cid_decls tid in 
  (* other tids that write these variables *)
  let writer_tids_of_write_vars = 
    CL.map
      (fun varid -> UseMap.find_default [] varid writemap)
      written_by_tid
    |> CL.flatten
    |> unique_list_of
    |> CL.filter (fun tidb -> tidb <> tid)
  in 
  let pred_writer_tids_of_write_vars = CL.filter 
      (fun tidb ->
        PCheck.check_path checker tidb tid
      )
      writer_tids_of_write_vars
  in 
  let predecessors = pred_writers@pred_readers@pred_writer_tids_of_write_vars in 
  (* make edges between the predecessors and tid *)
  let dag_edges = CL.map 
    (fun dtid -> (dtid, tid)) 
    (predecessors) 
  in   
  !dprint_endline ("[get_dependencies] done processing node: "^(P4tPrint.str_of_private_oids [tid]));
  !dprint_endline ("[get_dependencies] predecessors: "^( P4tPrint.str_of_private_oids predecessors));
  dag_edges 
;;

let data_dep_dag_of cid_decls g =
  let dag_nodes = tids_of_declmap cid_decls in 
  let dag_edges = CL.map 
    (get_dependencies 
      cid_decls 
      (build_readmap cid_decls)
      (build_writemap cid_decls)
      g
    )
    (tids_of_declmap cid_decls)
    |> CL.flatten
  in 
  (* add vertices *)
  !dprint_endline ("adding vertices to dataflow...");
  let data_dag = CL.fold_left 
    (G.add_vertex)
    G.empty
    dag_nodes
  in 
  !dprint_endline ("adding edges to dataflow...");
  let data_dag =
    CL.fold_left
      (fun data_dag (s, d) -> G.add_edge data_dag s d)
      data_dag
      dag_edges
  in
  data_dag 
;;

let do_passes df_prog =
  print_endline ("starting dataflow conversion pass.");
  let cid_decls, root_tid, g = DFSyntax.to_tuple df_prog in
  let data_dag = data_dep_dag_of cid_decls g in
  let result = DFSyntax.from_tuple (cid_decls, root_tid, data_dag) df_prog in 
  print_endline ("done with dataflow conversion pass.");
  result 
;;
