open LLSyntax
open MiscUtils
open DFSyntax
module MU = MergeUtils
module CL = Caml.List
open MiscUtils
open Printf
open DebugPrint

exception Error of string

let error s = raise (Error s)

(* logging *)
module DBG = BackendLogging

let outc = ref None
let dprint_endline = ref DBG.no_printf

(* find the first predecessors that set a variable, starting at a table or set of tables *)
let compute_ct = ref 0
let memo_ct = ref 0

let rec fold_most_recent_set_or_use_var_from_tbl
    checker_function
    (* checker function is either a test to see if the table reads the variable, 
    or a test to see if the table writes the variable *)
      cid_decls
    var_id
    (memo_assoc, setter_ids) (* memoize the output of this function. *)
    src_tbl_id
  =
  (* first step is to check to see if this result has already been computed in memo_assoc. *)
  let args = var_id, src_tbl_id, checker_function in
  (* let real_args = (var_id, src_tbl_id, checker_function) in  *)
  let memo_opt = CL.assoc_opt args memo_assoc in
  match memo_opt with
  (* found memoized result -- append to current result. *)
  | Some additional_setter_ids ->
    memo_ct := !memo_ct + 1;
    memo_assoc, unique_list_of (setter_ids @ additional_setter_ids)
  (* no memoized result, do new computation. *)
  | None ->
    compute_ct := !compute_ct + 1;
    let new_memo_assoc, new_accessor_ids =
      let res = checker_function cid_decls src_tbl_id var_id in
      !dprint_endline
        ("[fold_most_recent_set_or_use_var_from_tbl] var_id: "
        ^ Cid.to_string var_id
        ^ " tbl_id: "
        ^ Cid.to_string src_tbl_id
        ^ "tbl uses var? "
        ^ string_of_bool res);
      match res with
      (* if the tbl touches var_id, that is most recent user for this path back to root. *)
      | true ->
        let new_accessor_ids = [src_tbl_id] in
        (* memo the result of this call *)
        let new_memo_assoc =
          [(var_id, src_tbl_id, checker_function), new_accessor_ids]
        in
        !dprint_endline
          ("[fold_most_recent_set_or_use_var_from_tbl] returning table "
          ^ Cid.to_string src_tbl_id
          ^ " as an accessor table "
          ^ "");
        new_memo_assoc, new_accessor_ids
      | false ->
        let preds = pred_tids_of_tid cid_decls src_tbl_id in
        let new_memo_assoc, new_accessor_ids =
          CL.fold_left
            (fold_most_recent_set_or_use_var_from_tbl
               checker_function
               cid_decls
               var_id)
            (memo_assoc, [])
            preds
        in
        (* memo the results of this call. *)
        let new_memo_assoc =
          ((var_id, src_tbl_id, checker_function), new_accessor_ids)
          :: new_memo_assoc
        in
        new_memo_assoc, new_accessor_ids
    in
    let out_memo_assoc = memo_assoc @ new_memo_assoc |> unique_list_of in
    let out_setter_ids = setter_ids @ new_accessor_ids |> unique_list_of in
    out_memo_assoc, out_setter_ids
;;

(* find most recent list of predecessors that write to var_id, 
   starting from a src_tbl_id *)
let rec fold_most_recent_set_var_from_tbl
    cid_decls
    var_id
    (memo_assoc, setter_ids)
    src_tbl_id
  =
  !dprint_endline
    ("[fold_most_recent_set_var_from_tbl] finding setters for var "
    ^ Cid.to_string var_id
    ^ " by table "
    ^ P4tPrint.str_of_private_oid src_tbl_id
    ^ " or its predecessors. ");
  !dprint_endline
    ("[fold_most_recent_set_var_from_tbl] INITIAL setter ids:  "
    ^ P4tPrint.str_of_private_oids setter_ids);
  let memo_assoc, setter_ids =
    fold_most_recent_set_or_use_var_from_tbl
      tbl_writes_var
      cid_decls
      var_id
      (memo_assoc, setter_ids)
      src_tbl_id
  in
  !dprint_endline
    ("[fold_most_recent_set_var_from_tbl] FOUND setter ids:  "
    ^ P4tPrint.str_of_private_oids setter_ids);
  memo_assoc, setter_ids
;;

let rec fold_most_recent_use_var_from_tbl
    cid_decls
    var_id
    (memo_assoc, setter_ids)
    src_tbl_id
  =
  fold_most_recent_set_or_use_var_from_tbl
    tbl_reads_var
    cid_decls
    var_id
    (memo_assoc, setter_ids)
    src_tbl_id
;;

(* find most recent list of predecessors that write to var_id, 
   starting from a list of predecessors *)
let most_recent_set_var_from_preds cid_decls pred_tids var_id =
  memo_ct := 0;
  compute_ct := 0;
  let _, res =
    CL.fold_left
      (fold_most_recent_set_var_from_tbl cid_decls var_id)
      ([], [])
      pred_tids
  in
  !dprint_endline
    ("[most_recent_set_var_from_preds] var_id: "
    ^ Cid.to_string var_id
    ^ " predecessor tables that set this var: "
    ^ P4tPrint.str_of_private_oids res);
  !dprint_endline
    ("[most_recent_set_var_from_preds] var_id: "
    ^ Cid.to_string var_id
    ^ " UNIQUE predecessor tables that set this var: "
    ^ P4tPrint.str_of_private_oids (unique_list_of res));
  (* print_endline (sprintf "memo_ct: %i compute_ct: %i " !memo_ct !compute_ct); *)
  (* let res = CL.map (most_recent_set_var_from_tbl [] cid_decls var_id) pred_tids |> CL.flatten |> unique_list_of in  *)
  unique_list_of res
;;

(* find most recent list of predecessors that read var_id, starting from a list of predecessors *)
let most_recent_use_var_from_preds cid_decls pred_tids var_id =
  memo_ct := 0;
  compute_ct := 0;
  let _, res =
    CL.fold_left
      (fold_most_recent_use_var_from_tbl cid_decls var_id)
      ([], [])
      pred_tids
  in
  (* print_endline (sprintf "memo_ct: %i compute_ct: %i " !memo_ct !compute_ct); *)
  (* let res = CL.map (most_recent_set_var_from_tbl [] cid_decls var_id) pred_tids |> CL.flatten |> unique_list_of in  *)
  unique_list_of res
;;

(* get the edges (s -> tbl_id), where s is a table that writes a variable that tbl_id reads *)
let get_data_dep_edges cid_decls tbl_id all_data_dep_edges =
  (*   print_endline
    ("[get_data_dep_edges] on object: " ^ Printing.cid_to_string tbl_id); *)
  (* for each variable read in the table, find the most recently called predecessor table 
	that writes to it*)
  (* for each variable written by the table, find the most recently called predecessor table 
	that reads it. *)
  match is_tbl cid_decls tbl_id with
  | false -> all_data_dep_edges (* skip non-tables *)
  | true ->
    (*     print_endline
      ("[get_data_dep_edges] start processing table: "
      ^ Printing.cid_to_string tbl_id); *)
    (* get the predecessor tables *)
    let pred_tids = pred_tids_of_tid cid_decls tbl_id in
    (* print_endline "[get_data_dep_edges] got pred tids"; *)
    (* get the list of variables that the table reads *)
    let vars_used = read_vars_of_tbl cid_decls tbl_id in
    (* print_endline "[get_data_dep_edges] got vars used"; *)
    DBG.printf
      outc
      "[get_data_dep_edges] tbl %s pred_tids: [%s]\n"
      (P4tPrint.str_of_private_oid tbl_id)
      (str_of_cids pred_tids);
    DBG.printf
      outc
      "[get_data_dep_edges] tbl %s uses variables: [%s]\n"
      (P4tPrint.str_of_private_oid tbl_id)
      (str_of_cids vars_used);
    (* find the most recent predecessor that writes to every variable that 
       the table reads *)
    let use_var_dependee_tids =
      CL.map (most_recent_set_var_from_preds cid_decls pred_tids) vars_used
      |> CL.flatten
      |> unique_list_of
    in
    (* generate edges of the form (predcessor, table) for every predecessor 
       that writes to a variable that this table reads. *)
    let use_var_edges =
      CL.map (fun pred_t -> pred_t, tbl_id) use_var_dependee_tids
    in
    (* print_endline "[get_data_dep_edges] got use_var_edges"; *)
    DBG.printf
      outc
      "[get_data_dep_edges] due to the vars it uses, tbl %s must execute after \
       tbls: [%s]\n"
      (P4tPrint.str_of_private_oid tbl_id)
      (P4tPrint.str_of_private_oids use_var_dependee_tids);
    (* get the variables that this table writes *)
    let vars_set = write_vars_of_tbl cid_decls tbl_id in
    (* print_endline "[get_data_dep_edges] got vars_set"; *)
    DBG.printf
      outc
      "[get_data_dep_edges] tbl %s sets variables: [%s]\n"
      (P4tPrint.str_of_private_oid tbl_id)
      (str_of_cids vars_set);
    (* find the most recent predecessor that reads each variable written. *)
    let set_var_dependee_tids =
      CL.map (most_recent_use_var_from_preds cid_decls pred_tids) vars_set
      |> CL.flatten
      |> unique_list_of
    in
    DBG.printf
      outc
      "[get_data_dep_edges] tbl: %s predecessors that read set variables: [%s]\n"
      (P4tPrint.str_of_private_oid tbl_id)
      (str_of_cids set_var_dependee_tids);
    (* add edges of the form (predecessor, table) for every predecessor 
       that reads a variable that this table writes. *)
    let set_var_edges =
      CL.map (fun pred_t -> pred_t, tbl_id) set_var_dependee_tids
    in
    (* 		print_endline ("[get_data_dep_edges] got set_var_edges");
 *)
    DBG.printf
      outc
      "[get_data_dep_edges] due to vars it sets, tbl %s must execute after \
       tbls: [%s]\n"
      (P4tPrint.str_of_private_oid tbl_id)
      (str_of_cids set_var_dependee_tids);
    (*     print_endline
      ("[get_data_dep_edges] finished processing table: "
      ^ Printing.cid_to_string tbl_id); *)
    all_data_dep_edges @ use_var_edges @ set_var_edges
;;

(* generate a data dependent dag of the tables in g *)
let data_dep_dag_of cid_decls g =
  (* copy of g with reversed edges *)
  let rev_g = reverse_dag_of g in
  (* get the edges that go from each table to the previous tables that use those edges. *)
  print_endline "[data_dep_dag_of] getting new edges";
  let new_edges = Topo.fold (get_data_dep_edges cid_decls) rev_g [] in
  let oids, _ = CL.split cid_decls in
  (* create data_dag, adding tables as vertices *)
  print_endline "[data_dep_dag_of] creating data_dag";
  let data_dag =
    oids
    |> CL.filter (is_tbl cid_decls)
    |> CL.fold_left
         (fun data_dag tbl_id -> G.add_vertex data_dag tbl_id)
         G.empty
  in
  (* add data dependency edges *)
  print_endline "[data_dep_dag_of] adding edges";
  let data_dag =
    CL.fold_left
      (fun data_dag (s, d) -> G.add_edge data_dag s d)
      data_dag
      new_edges
  in
  data_dag
;;

(* convert a DAG with edges that represent call order 
   into a DAG with edges that represent data dependencies. *)
(* note: the dag only represents tables! *)
let do_passes df_prog =
  DBG.start_mlog __FILE__ outc dprint_endline;
  let cid_decls, root_tid, g = df_prog in
  let data_dag = data_dep_dag_of cid_decls g in
  cid_decls, root_tid, data_dag
;;
