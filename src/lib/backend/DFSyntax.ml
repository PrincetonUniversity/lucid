(* DataFlow syntax -- extension of LLSyntax 
where table nodes are (eventually) arranged by dataflow 
rather than control flow. *)
open LLSyntax
open MiscUtils
open Graph
open Format
open MiscUtils
module CL = Caml.List

exception Error of string

let error s = raise (Error s)

(* A graph-based representation of a program consisting of 
LLSyntax objects. Used for optimizations. *)

(* graph elements *)
module Node = struct
  type t = Cid.t

  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = Cid.equals
end

module Edge = struct
  type t = int

  let compare = Pervasives.compare
  let equal = ( = )
  let default = 1

  (* let default = Cid.create [""] *)
end

module G = Graph.Persistent.Digraph.ConcreteLabeled (Node) (Edge)

type dagParams = (mid * int) list
type declsMap = (oid * decl) list [@@deriving show]
type dagProg = declsMap * oid * G.t
type cid_decls = (oid * decl) list [@@deriving show]

module PathWeight = struct
  type edge = G.E.t
  type t = int

  let weight _ = 1
  let zero = 0
  let add = ( + )
  let sub = ( - )
  let compare = compare
end

module NegativePathWeight = struct
  type edge = G.E.t
  type t = int

  let weight _ = -1
  let zero = 0
  let add = ( + )
  let sub = ( - )
  let compare = Pervasives.compare
end

module ND = Path.Dijkstra (G) (NegativePathWeight)
module D = Path.Dijkstra (G) (PathWeight)
module Check = Path.Check (G)

(* module M1 = Traverse.Bfs(G) *)
module Topo = Topological.Make (G)
module DFS = Traverse.Dfs (G)

(******* graph building (from a dagProgram to a dag graph) *******)
let get_nodes (cid_decls : declsMap) : cid list =
  let cids, _ = Caml.List.split cid_decls in
  cids
;;

let get_edges (cid_decls : declsMap) : (cid * cid) list =
  let nextid_to_edges node_id next_ids =
    CL.combine (CL.init (CL.length next_ids) (fun _ -> node_id)) next_ids
  in
  let node_ids, _ = List.split cid_decls in
  let decl_to_out_edges cid_decls node_id =
    match Cid.lookup cid_decls node_id with
    | Action (_, obj_ids, next_tids) ->
      nextid_to_edges node_id (next_tids @ obj_ids)
      (* action has pointers to tables and objects *)
    | Table (_, rules, _) -> nextid_to_edges node_id (rules_to_aids rules)
    | SInstrVec (_, { sRid = reg_id; _ }) -> nextid_to_edges node_id [reg_id]
    (* Hashers and InstrVecs aren't here because they don't have any out edges. *)
    | _ -> []
  in
  let x edge_acc d = decl_to_out_edges cid_decls d @ edge_acc in
  Caml.List.fold_left x [] node_ids
;;

let build_graph nodes edges : G.t =
  let g = G.empty in
  let g = Caml.List.fold_left G.add_vertex g nodes in
  let add_edge g (s, d) = G.add_edge g s d in
  let g = Caml.List.fold_left add_edge g edges in
  g
;;

let graph_of_declsMap cid_decls =
  let nodes = get_nodes cid_decls in
  let edges = get_edges cid_decls in
  let g = build_graph nodes edges in
  g
;;

(* convert the objects into an instruction prog, which 
   just adds a graph *)
let to_dfProg iprog =
  iprog.instr_dict, iprog.root_tid, graph_of_declsMap iprog.instr_dict
;;

(**** cid_decls accessors ****)

let ids_of_decls cid_decls = CL.split cid_decls |> fst

(* destructors *)
let dmap_of_dprog dprog =
  let decl_map, _, _ = dprog in
  decl_map
;;

let dmap_rtid_of_dprog dprog =
  let decl_map, rtid, _ = dprog in
  decl_map, rtid
;;

(* debug printers *)
(* let to_node_str node_id = mid_to_str_suffix node_id
let to_edge_str (d1, d2) = sprintf "(%s --> %s)" (to_node_str d1) (to_node_str d2)  

 *)
(* type check ids *)
let is_tbl cid_decls oid =
  match Cid.lookup cid_decls oid with
  | Table _ -> true
  | _ -> false
;;

let is_reg cid_decls oid =
  match Cid.lookup cid_decls oid with
  | RegVec _ -> true
  | _ -> false
;;

let is_callable cid_decls oid =
  match Cid.lookup cid_decls oid with
  | Table _ -> true
  | SchedBlock _ -> true
  | _ -> false
;;

let is_acn cid_decls oid =
  match Cid.lookup cid_decls oid with
  | Action _ -> true
  | _ -> false
;;

let is_meta cid_decls oid =
  match Cid.lookup cid_decls oid with
  | MetaVar _ -> true
  | _ -> false
;;

let is_hasher cid_decls oid =
  match Cid.lookup cid_decls oid with
  | Hasher _ -> true
  | _ -> false
;;

let is_sInstr cid_decls oid =
  match Cid.lookup cid_decls oid with
  | SInstrVec _ -> true
  | _ -> false
;;

let decls_of_type filter_f cid_decls =
  let map_f (a, b) =
    match filter_f cid_decls a with
    | true -> Some b
    | false -> None
  in
  CL.filter_map map_f cid_decls
;;

let tbls_of_dmap = decls_of_type is_tbl
let acns_of_dmap = decls_of_type is_acn

let ids_of_type filter_f cid_decls =
  let map_f (a, _) =
    match filter_f cid_decls a with
    | true -> Some a
    | false -> None
  in
  CL.filter_map map_f cid_decls
;;

let is_acn_or_callable cid_decls oid =
  is_acn cid_decls oid || is_callable cid_decls oid
;;

let emplace assoc_list (a, b) =
  match Cid.exists assoc_list a with
  | true -> Cid.replace assoc_list a b
  | false -> (a, b) :: assoc_list
;;

(* replace or add new_decl to cid_decls *)
let emplace_decl cid_decls new_decl =
  let id = id_of_decl new_decl in
  match Cid.exists cid_decls id with
  | true -> Cid.replace cid_decls id new_decl
  | false -> cid_decls @ [id, new_decl]
;;

let batch_emplace_decl cid_decls new_decls =
  CL.fold_left emplace_decl cid_decls new_decls
;;

(***** misc helpers *****)
let tids_of_declmap cid_decls =
  let tids =
    CL.filter_map
      (fun (id, _) -> if is_tbl cid_decls id then Some id else None)
      cid_decls
  in
  tids
;;

let mids_of_declmap cid_decls =
  let mids =
    CL.filter_map
      (fun (id, _) -> if is_meta cid_decls id then Some id else None)
      cid_decls
  in
  mids
;;

let stage_of_rid cid_decls rid =
  let dec = Cid.lookup cid_decls rid in
  match dec with
  | RegVec (_, _, _, _, stage_opt) -> stage_opt
  | _ ->
    error
      ("stage_of_rid: identifier "
      ^ P4tPrint.str_of_private_oid rid
      ^ " does not reference a register")
;;

let stage_of_callable cid_decls tid =
  let dec = Cid.lookup cid_decls tid in
  match dec with
  | Table (_, _, stage_opt) -> stage_opt
  | SchedBlock _ -> None
  | _ ->
    error
      ("stage_of_callable: identifier "
      ^ P4tPrint.str_of_private_oid tid
      ^ " does not reference a callable (table or native block)")
;;

(* print a path of nodes *)
let print_nodepath (node_path : cid list) =
  match node_path with
  | [] -> ()
  | _ ->
    printf
      "path from %s --> %s: "
      (P4tPrint.str_of_private_oid (Caml.List.hd node_path))
      (P4tPrint.str_of_private_oid (Caml.List.hd (Caml.List.rev node_path)));
    Caml.List.iter
      (fun i -> printf "%s, " (P4tPrint.str_of_private_oid i))
      node_path;
    printf "\n"
;;

(***** action helpers *****)
(* successor tables of an action. *)
let succs_of_aid cid_decls aid =
  let acn = Cid.lookup cid_decls aid in
  match acn with
  | Action (_, _, successor_ids) -> successor_ids
  | _ -> []
;;

(* successor table of an action (if only 1 is expected) *)
let succ_of_aid cid_decls aid =
  match succs_of_aid cid_decls aid with
  | [successor_tbl] -> successor_tbl
  | _ -> error "unexpected number of action successors"
;;

(* registers attached to an action *)
let rids_of_acn cid_decls acn_decl =
  match acn_decl with
  | Action (_, oids, _) ->
    oids
    |> Caml.List.filter_map (fun oid ->
           match Cid.lookup cid_decls oid with
           | SInstrVec (_, { sRid = rid; _ }) -> Some rid
           | _ -> None)
  | _ -> error "rids_of_acn: acn_decl not an action"
;;

(* registers attached to an action *)
let rids_of_aid cid_decls aid =
  let acn = Cid.lookup cid_decls aid in
  rids_of_acn cid_decls acn
;;

(* compute objects attached to an action *)
let oids_of_aid cid_decls aid =
  let acn = Cid.lookup cid_decls aid in
  let _, oids, _ = from_action acn in
  oids
;;

let hids_of_aid cid_decls aid =
  let _, oids, _ = from_action (Cid.lookup cid_decls aid) in
  CL.filter (is_hasher cid_decls) oids
;;

(* action uses rid *)
let aid_uses_rid cid_decls aid rid = CL.mem rid (rids_of_aid cid_decls aid)

(***** table helpers *****)
let set_stage_of_tid cid_decls tid new_stage =
  let d = Cid.lookup cid_decls tid in
  match d with
  | Table (t, r, _) -> Cid.replace cid_decls tid (Table (t, r, Some new_stage))
  | _ ->
    error
      ("set_stage_of_tid: cannot set stage of "
      ^ P4tPrint.str_of_private_oid tid
      ^ " -- not a table")
;;

(* actions attached to a table *)
let aids_of_tid cid_decls tbl_id =
  match Cid.lookup_opt cid_decls tbl_id with
  | Some (Table tbl) ->
    let _, rules, _ = tbl in
    rules_to_aids rules
  | _ -> []
;;

let acns_of_tid cid_decls tbl_id =
  Caml.List.map (Cid.lookup cid_decls) (aids_of_tid cid_decls tbl_id)
;;

let acn_map_of_tid cid_decls tbl_id =
  let aids = aids_of_tid cid_decls tbl_id in
  (* printf "aids: [%s]\n" (str_of_cids aids); *)
  let acns = Caml.List.map (Cid.lookup cid_decls) aids in
  CL.combine aids acns
;;

let stage_of_tid cid_decls tid =
  let dec = Cid.lookup cid_decls tid in
  match dec with
  | Table (_, _, stage_opt) -> stage_opt
  | _ ->
    error
      ("stage_of_tid: identifier "
      ^ P4tPrint.str_of_private_oid tid
      ^ " does not reference a table")
;;

(* registers attached to a table *)
let rids_of_tid cid_decls tbl_id =
  let aids = aids_of_tid cid_decls tbl_id in
  CL.flatten (CL.map (rids_of_aid cid_decls) aids)
;;

(* for table tid, find all the other tables that 
use one of tid's registers. *)
let regmates_of_tid cid_decls tid =
  let rids = rids_of_tid cid_decls tid in
  let tid_rids_intersect cid_decls tid rids =
    match rids with
    | [] -> false
    | _ -> intersect rids (rids_of_tid cid_decls tid)
  in
  let regmate_cid_decls =
    CL.filter
      (fun (oid, dec) ->
        match dec with
        | Table _ -> tid_rids_intersect cid_decls oid rids
        | _ -> false)
      cid_decls
  in
  CL.split regmate_cid_decls |> fst |> remove tid
;;

(* hashers attached to a table *)
let hids_of_tid cid_decls tbl_id =
  let aids = aids_of_tid cid_decls tbl_id in
  CL.flatten (CL.map (hids_of_aid cid_decls) aids)
;;

(* table to the next tables called by its actions *)
let succs_of_tid cid_decls tid =
  let aids = aids_of_tid cid_decls tid in
  let next_tids =
    Caml.List.fold_left
      (fun (acc : oid list) a -> acc @ succs_of_aid cid_decls a)
      []
      aids
  in
  unique_list_of next_tids
;;

(* registers attached to a table *)
let regs_of_tid cid_decls tbl_id =
  acns_of_tid cid_decls tbl_id (* action decls *)
  |> Caml.List.map (rids_of_acn cid_decls) (* register id lists *)
  |> Caml.List.fold_left ( @ ) []
  |> unique_list_of
;;

(* does table tid use register rid? *)
let tid_uses_rid cid_decls tid rid = CL.mem rid (regs_of_tid cid_decls tid)

let oids_of_tid cid_decls tbl_id =
  aids_of_tid cid_decls tbl_id |> CL.map (oids_of_aid cid_decls) |> CL.flatten
;;

(* return an empty list if the action is not in the dict. *)
let oids_of_aid_opt cid_decls aid =
  let acn = Cid.lookup_opt cid_decls aid in
  match acn with
  | Some a ->
    let _, oids, _ = from_action a in
    oids
  | None -> []
;;

let oids_of_tid_opt cid_decls tbl_id =
  aids_of_tid cid_decls tbl_id
  |> CL.map (oids_of_aid_opt cid_decls)
  |> CL.flatten
;;

(* get all the declarations associated with a table: 
  actions
  action objects
*)
let decls_of_tid cid_decls tid =
  let acns = CL.map (Cid.lookup cid_decls) (aids_of_tid cid_decls tid) in
  let oids =
    unique_list_of (CL.map (Cid.lookup cid_decls) (oids_of_tid cid_decls tid))
  in
  oids @ acns
;;

(* get predecessor action ids of a table *)
let pred_aids_of_tid cid_decls tid =
  let _, decls = Caml.List.split cid_decls in
  let get_pred_opt tid d =
    match d with
    (* return the id of d if d is a predecessor of tid. *)
    | Action (pred_id, _, successor_tids) ->
      (match Caml.List.mem tid successor_tids with
      | true -> Some pred_id
      | false -> None)
    | _ -> None
  in
  Caml.List.filter_map (get_pred_opt tid) decls
;;

(* get the table id that calls an action *)
let pred_tids_of_aid cid_decls aid =
  let tids_that_call_aid tid =
    let aids = aids_of_tid cid_decls tid in
    CL.mem aid aids
  in
  let oids, _ = Caml.List.split cid_decls in
  oids (* all objs *)
  |> CL.filter (is_tbl cid_decls) (* tables *)
  |> CL.filter tids_that_call_aid
;;

(* get the predecessor table ids of a table *)
let pred_tids_of_tid cid_decls tid =
  (* for each pred aid, find the tables that calls the action. *)
  let pred_aids = pred_aids_of_tid cid_decls tid in
  let fold_f pred_tids aid = pred_tids @ pred_tids_of_aid cid_decls aid in
  CL.fold_left fold_f [] pred_aids
;;

(* replace a oid wherever it appears in a decl *)
let replace_oid_in_decl d old_oid new_oid =
  let v =
    object
      inherit [_] dataPathMap as super

      method! visit_oid _ oid =
        match old_oid = oid with
        | true -> new_oid
        | false -> oid
    end
  in
  let new_d = v#visit_decl () d in
  new_d
;;

let replace_mid_in_decl d old_mid new_mid =
  let v =
    object
      inherit [_] dataPathMap as super

      method! visit_mid _ i =
        match old_mid = i with
        | true -> new_mid
        | false -> i
    end
  in
  let new_d = v#visit_decl () d in
  new_d
;;

let replace_oper_in_decl d old_oper new_oper =
  let v =
    object
      inherit [_] dataPathMap as super

      method! visit_oper _ i =
        match old_oper = i with
        | true -> new_oper
        | false -> i
    end
  in
  let new_d = v#visit_decl () d in
  new_d
;;

(* rename old_tid to new_tid in cid_decls *)
let rename_tid cid_decls (old_tid, new_tid) =
  let change_tid decl_id old_tid new_tid =
    match decl_id = old_tid with
    | true -> new_tid
    | false -> decl_id
  in
  let cid_decls =
    CL.map
      (fun (i, d) ->
        change_tid i old_tid new_tid, replace_oid_in_decl d old_tid new_tid)
      cid_decls
  in
  cid_decls
;;

(* insert newtbl_tid before tid in the program *)
let insert_before_tid cid_decls newtbl_tid tid =
  let change_next_tid old_tid new_tid acn =
    replace_oid_in_decl acn old_tid new_tid
  in
  (* find the predecessor actions of tid, change their successor to newtbl_tid *)
  let updated_pred_acns =
    pred_aids_of_tid cid_decls tid
    |> CL.map (Cid.lookup cid_decls)
    |> CL.map (change_next_tid tid newtbl_tid)
  in
  (* add tid as the successor of all actions in newtbl *)
  let newtbl_acns =
    acns_of_tid cid_decls newtbl_tid |> CL.map (add_succ_tid tid)
  in
  (* update cid_decls with new table, new table actions, and updated predecessor actions of tbl *)
  (*   let pred_acn_ids = CL.map id_of_decl updated_pred_acns in 
  printf "[insert_before_tid] new table: %s table: %s predecessor action ids: [%s]\n" (mid_to_str_suffix newtbl_tid) (mid_to_str_suffix tid) (str_of_cids pred_acn_ids);
 *)
  let cid_decls =
    CL.fold_left emplace_decl cid_decls (newtbl_acns @ updated_pred_acns)
  in
  cid_decls
;;

(* insert newtbl_tid after tid in the program *)
let insert_after_tid cid_decls tid newtbl_tid =
  let change_next_tid old_tid new_tid acn =
    replace_oid_in_decl acn old_tid new_tid
  in
  (* set the successor of newtbl_tid to the successor of tbl *)
  let nxt_tids = succs_of_tid cid_decls tid in
  let newtbl_acns =
    match nxt_tids with
    (* tbl has one next table nt, so set the next table of all newtbl acns to nt *)
    | [nt] -> acns_of_tid cid_decls newtbl_tid |> CL.map (add_succ_tid nt)
    | [] -> []
    | _ ->
      error "cannot place a table after tid -- tid has multiple next tables... "
  in
  (* add newtbl_tid as the successor of all actions in tid *)
  let tbl_acns =
    match nxt_tids with
    | [nt] ->
      acns_of_tid cid_decls tid |> CL.map (change_next_tid nt newtbl_tid)
    | [] -> acns_of_tid cid_decls tid |> CL.map (add_succ_tid tid)
    | _ ->
      error "cannot place a table after tid -- tid has multiple next tables... "
  in
  (* update cid_decls *)
  let cid_decls =
    CL.fold_left emplace_decl cid_decls (tbl_acns @ newtbl_acns)
  in
  cid_decls
;;

(* get all the descendent tables of a table or action *)
let rec get_descendents decl_map oid : oid list =
  let dec = Cid.lookup decl_map oid in
  (* get the successor tables of the table or action. *)
  let successor_tids =
    match dec with
    | Table _ -> succs_of_tid decl_map oid
    | Action _ -> succs_of_aid decl_map oid
    | _ -> []
  in
  let successor_descendents =
    Caml.List.fold_left
      (fun tid_acc oid -> tid_acc @ get_descendents decl_map oid)
      []
      successor_tids
  in
  successor_tids @ successor_descendents
;;

(* is tbl_id the descendent of action aid? *)
let is_descendent decl_map tbl_id aid =
  Caml.List.mem tbl_id (get_descendents decl_map aid)
;;

(* is tbl_id the descendent of all actions aids? *)
let is_common_descendent decl_map tbl_id aids =
  let bools = Caml.List.map (is_descendent decl_map tbl_id) aids in
  not (Caml.List.mem false bools)
;;

(* no falses --> it is a common descendent *)

(***** action / table helpers *****)

let tid_of_aid decl_tups aid =
  let f (tbl_id, d) =
    match d with
    | Table _ ->
      let tbl_aids = aids_of_tid decl_tups tbl_id in
      CL.mem aid tbl_aids
    | _ -> false
  in
  let filtered_list = Caml.List.filter f decl_tups in
  match filtered_list with
  | [(tid, _)] -> tid
  | _ ->
    error
      ("error finding calling table for action: "
      ^ P4tPrint.str_of_private_oid aid)
;;

(* get the first common descendent of all actions aids *)
let get_first_common_descendent decl_map acn_ids =
  (* we are looking for the first table that is a descendent of all the actions *)
  (* get the descendents of the first action id. *)
  (* iterate through them, finding the first one that is the descendent of all the rest. *)
  match acn_ids with
  | acn_id :: acn_ids ->
    let descendent_tids = get_descendents decl_map acn_id in
    let common_descendent_tids =
      Caml.List.filter
        (fun tid -> is_common_descendent decl_map tid acn_ids)
        descendent_tids
    in
    (match common_descendent_tids with
    | [] -> None
    | first_descendent_tid :: _ -> Some first_descendent_tid)
  | _ ->
    error
      "trying to find the common descendent of a table with 0 or 1 actions..."
;;

(******* pathfinding *******)

(* convert a path of edges to a path of nodes *)
let to_nodepath edges =
  match edges with
  | [] -> []
  | fst_edge :: edges ->
    let fld_f node_acc e =
      let _, _, dst = e in
      node_acc @ [dst]
    in
    let tl_nodes = Caml.List.fold_left fld_f [] edges in
    let hd_node, _, hd_edge_dst = fst_edge in
    hd_node :: hd_edge_dst :: tl_nodes
;;

(* 
let to_nodepath_wrong edges = 
  match edges with 
  | [] -> []
  | fst_edge::edges -> 
    let fld_f = (fun node_acc e -> let (_, _, dst) = e in node_acc@[dst]) in 
    let tl_nodes = (Caml.List.fold_left fld_f [] edges) in 
    let (hd_node, _, _) = fst_edge in 
    hd_node::tl_nodes
;; 
*)

(* get the callables in a nodepath *)
let to_callablepath cid_decls node_path =
  (* printf "to_callablepath: %s\n" (cids_to_string node_path); *)
  let filter_f obj_id =
    match Cid.lookup cid_decls obj_id with
    | Table _ -> true
    | SchedBlock _ -> true
    | _ -> false
  in
  node_path |> Caml.List.filter filter_f
;;

let rec print_edgepath es =
  match es with
  | [] -> printf "\n"
  | e :: es ->
    let src_id, _, dst_id = e in
    printf
      "[%s -> %s]"
      (P4tPrint.str_of_private_oid src_id)
      (P4tPrint.str_of_private_oid dst_id);
    print_edgepath es
;;

(* get the shortest path of nodes from root to tgt. 
   returns empty path if none exists. *)
let find_shortest_nodepath g root_id tgt_id =
  (* printf "find_shortest_nodepath: root: %s tgt: %s\n" (mid_to_str_suffix root_id) (mid_to_str_suffix tgt_id); *)
  let edge_path, len =
    try D.shortest_path g root_id tgt_id with
    | Not_found -> [], 0
  in
  let _ = len in
  (* printf "shortest path from src to %s: %i\n" (mid_to_str_suffix tgt_id) (len); *)
  (* print_edgepath edge_path; *)
  (* let (edge_path, _) = D.shortest_path g root_id tgt_id in  *)
  (* let node_path = to_nodepath edge_path in  *)
  let node_path = to_nodepath edge_path in
  (* let node_path_old = to_nodepath_old edge_path in  *)
  (* printf "node_path output (len %i): [%s]\n" (CL.length node_path) (cids_to_string node_path);
  printf "node_path_fixed output (len %i): [%s]\n" (CL.length node_path_fixed) (cids_to_string node_path_fixed); *)
  node_path
;;

(* find the shortest table path from root to tgt *)
let find_shortest_callablepath g cid_decls root_id tgt_id =
  let nodepath = find_shortest_nodepath g root_id tgt_id in
  let callablepath = to_callablepath cid_decls nodepath in
  callablepath
;;

(******* table queries *******)

(* is there a path from src to dst? *)
let path_exists g cid_decls src_tid dst_tid =
  let res =
    match
      find_shortest_callablepath g cid_decls src_tid dst_tid |> Caml.List.length
    with
    | 0 -> false (* no path found *)
    | _ -> true
    (* path found *)
  in
  res
;;

(* is src_tid the callable that immediately preceeds dst_tid? *)
let direct_path_exists g cid_decls src_tid dst_tid =
  let res =
    match
      find_shortest_callablepath g cid_decls src_tid dst_tid |> Caml.List.length
    with
    | 2 -> true (* the path is [src; dst] *)
    | _ -> false
    (* not 2 --> not pred *)
  in
  res
;;

(* find the predecessor callables of tbl_id *)
let pred_tids_of g cid_decls tbl_id =
  let oids, _ = CL.split cid_decls in
  let cids = CL.filter (is_callable cid_decls) oids in
  let pred_ids =
    CL.filter (fun src_id -> direct_path_exists g cid_decls src_id tbl_id) cids
  in
  (* printf "pred_tids_of: %s:[%s]\n" (mid_to_str_suffix tbl_id) (str_of_cids pids); *)
  pred_ids
;;

(* find the successor callables of tbl_id *)
let succ_tids_of g cid_decls tbl_id =
  let oids, _ = CL.split cid_decls in
  let cids = CL.filter (is_callable cid_decls) oids in
  let succ_ids =
    CL.filter (fun dst_id -> direct_path_exists g cid_decls tbl_id dst_id) cids
  in
  succ_ids
;;

let is_branch_tbl g cid_decls oid =
  match Cid.lookup cid_decls oid with
  | Table _ ->
    (* does the table have multiple next tables? *)
    (match List.length (succ_tids_of g cid_decls oid) with
    | 0 | 1 -> false
    | _ -> true)
  | _ -> false
;;

let is_merge_tbl g cid_decls oid =
  match Cid.lookup cid_decls oid with
  | Table _ ->
    (* does the table have multiple predecessor tables? *)
    (match List.length (pred_tids_of g cid_decls oid) with
    | 0 | 1 -> false
    | _ -> true)
  | _ -> false
;;

(******* register queries *******)
(* find the tables that use reg_id *)
let tids_of_rid cid_decls reg_id =
  let oids, _ = CL.split cid_decls in
  let cids = CL.filter (is_callable cid_decls) oids in
  let tids =
    CL.filter (fun tbl_id -> tid_uses_rid cid_decls tbl_id reg_id) cids
  in
  tids
;;

let sInstr_uses_rid cid_decls sInstr_id reg_id =
  Cid.equals (rid_of_sInstr (Cid.lookup cid_decls sInstr_id)) reg_id
;;

(* find the stateful instructions that access reg id *)
let sInstrs_of_rid cid_decls reg_id =
  let oids, _ = CL.split cid_decls in
  let sids = CL.filter (is_sInstr cid_decls) oids in
  let filter_f sInstr_id = sInstr_uses_rid cid_decls sInstr_id reg_id in
  let sInstr_ids = CL.filter filter_f sids in
  sInstr_ids
;;

(* find the multiple tables that call this object *)
let tids_of_oid cid_decls obj_id =
  let filter_f (_, d) =
    match d with
    | Action (aid, oids, _) ->
      (match CL.mem obj_id oids with
      | true -> Some (tid_of_aid cid_decls aid)
      | false -> None)
    | _ -> None
  in
  let tids = CL.filter_map filter_f cid_decls in
  tids
;;

(* find the table that calls this object *)
let tid_of_oid cid_decls obj_id =
  let filter_f (_, d) =
    match d with
    | Action (aid, oids, _) ->
      (match CL.mem obj_id oids with
      | true -> Some (tid_of_aid cid_decls aid)
      | false -> None)
    | _ -> None
  in
  let tids = CL.filter_map filter_f cid_decls in
  match tids with
  | [] -> error "no tables call object."
  | [tid] -> tid
  | _ -> error "more than 1 table call object."
;;

(* Get the tables that a register is attached to, 
   by finding all the tables that have paths to the register. *)
(* replaced by tids_of_rid *)
(* let pred_tids_of_reg g cid_decls reg_id = 
  let pc = Check.create g in 
  let (obj_ids, _) = Caml.List.split cid_decls in 
  let attached_tables = obj_ids 
    |> (Caml.List.filter (is_tbl cid_decls)) (* tables *)
    |> (Caml.List.filter (fun tbl_id -> Check.check_path pc tbl_id reg_id)) (* tables with paths *)
  in 
  attached_tables
;; *)

(* get the mids that the table writes (l) and reads (r) *)
let lr_mids_of_tid cid_decls tbl_id : lmid list * mid list =
  let acns = acns_of_tid cid_decls tbl_id in
  let acn_decl_to_objs cid_decls acn =
    match acn with
    | Action (_, oids, _) -> CL.map (Cid.lookup cid_decls) oids
    | _ -> error "acn_decl_to_objs: not an acn decl"
  in
  let objs = CL.map (acn_decl_to_objs cid_decls) acns in
  (* traverse object decls *)
  let lr_mids_of_obj obj_decl =
    let v =
      object
        inherit [_] dataPathIter as super
        val mutable write_mids = []
        method write_mids = write_mids
        val mutable read_mids = []
        method read_mids = read_mids
        method! visit_lmid _ m = write_mids <- write_mids @ [m]

        (* super#visit_lmid env m  *)
        (* currently, all lmids are also rmids. 
            Remove this line to make the two sets distinct. 
            (can do that once SSA is integrated.) 
          *)
        method! visit_mid _ m = read_mids <- read_mids @ [m]
      end
    in
    v#visit_decl () obj_decl;
    v#write_mids, v#read_mids
  in
  let lmids_per_obj, rmids_per_obj =
    CL.flatten objs |> CL.map lr_mids_of_obj |> CL.split
  in
  let lmids, rmids = CL.flatten lmids_per_obj, CL.flatten rmids_per_obj in
  (* finally, add the vars that the table matches on to rmids *)
  match Cid.lookup cid_decls tbl_id with
  | Table (_, rules, _) ->
    let match_mids = match_vars_of_rules rules in
    lmids, match_mids @ rmids
  | _ ->
    error
      ("lr_mids_of_tid: object "
      ^ P4tPrint.str_of_private_oid tbl_id
      ^ " not a table.")
;;

(* is there a data dependency between parent and child? *)
let data_dep_of_tbls cid_decls parent_tid child_tid =
  let parent_write_vars, _ = lr_mids_of_tid cid_decls parent_tid in
  let _, child_read_vars = lr_mids_of_tid cid_decls child_tid in
  intersect parent_write_vars child_read_vars
;;

(* what vars does a table read? *)
let read_vars_of_tbl cid_decls tbl_id =
  let _, read_vars = lr_mids_of_tid cid_decls tbl_id in
  read_vars
;;

let write_vars_of_tbl cid_decls tbl_id =
  let write_vars, _ = lr_mids_of_tid cid_decls tbl_id in
  write_vars
;;

(* does a table write a var? *)
let tbl_writes_var cid_decls tbl_id var_id =
  let write_vars, _ = lr_mids_of_tid cid_decls tbl_id in
  CL.mem var_id write_vars
;;

(* does a table write a var? *)
let tbl_reads_var cid_decls tbl_id var_id =
  let _, read_vars = lr_mids_of_tid cid_decls tbl_id in
  CL.mem var_id read_vars
;;

(* a call table has 0 or 1 one successor tables *)
let is_call_tbl cid_decls tbl_id =
  match Cid.lookup cid_decls tbl_id with
  | Table _ ->
    let next_tids = succs_of_tid cid_decls tbl_id in
    (match CL.length next_tids with
    | 0 | 1 -> true
    | _ -> false)
  | _ -> false
;;

(* a join table has multiple predecessor actions *)
let is_join_tbl cid_decls tbl_id =
  match pred_aids_of_tid cid_decls tbl_id with
  | [] -> false
  | [_] -> false
  | _ -> true
;;

(* debugging: is an action ever used by any table? *)
let is_aid_used cid_decls aid =
  let all_ids, _ = CL.split cid_decls in
  let tbl_ids = CL.filter (is_tbl cid_decls) all_ids in
  let all_used_aids =
    CL.fold_left (fun aids tid -> aids @ aids_of_tid cid_decls tid) [] tbl_ids
  in
  CL.mem aid all_used_aids
;;

(* misc graph utils *)

(* return a copy of g with reversed edges *)
let reverse_dag_of (g : G.t) : G.t =
  let rev_g = G.empty in
  let rev_g = G.fold_vertex (fun v rev_g -> G.add_vertex rev_g v) g rev_g in
  let rev_g = G.fold_edges (fun s t new_g -> G.add_edge new_g t s) g rev_g in
  rev_g
;;

(* create a control flow graph with only tables as vertices. *)
let table_dag_of (cid_decls : declsMap) : G.t =
  let tbls = tbls_of_dmap cid_decls in
  let tids = CL.map id_of_decl tbls in
  let tbl_g = CL.fold_left (fun g tid -> G.add_vertex g tid) G.empty tids in
  let edge_fold_f g tid =
    let succ_tids = succs_of_tid cid_decls tid in
    let g =
      CL.fold_left (fun g succ_tid -> G.add_edge g tid succ_tid) g succ_tids
    in
    g
  in
  let tbl_g = CL.fold_left edge_fold_f tbl_g tids in
  tbl_g
;;

(* if we want a table dag from an existing graph... *)
let rec next_tables_of cid_decls g v =
  (* for every successor:
    if successor is table, include it. 
    if not, include next tables of the successor.  *)
  let map_f v_next =
    match is_tbl cid_decls v_next with
    | true -> [v_next]
    | false -> next_tables_of cid_decls g v_next
  in
  CL.flatten (CL.map map_f (G.succ g v))
;;

let table_g_of cid_decls g =
  let edge_fold_f edges v =
    let new_edges =
      next_tables_of cid_decls g v |> CL.map (fun v_next -> v, v_next)
    in
    edges @ new_edges
  in
  let tids = tbls_of_dmap cid_decls |> CL.map id_of_decl in
  let edges = CL.fold_left edge_fold_f [] tids in
  let g = CL.fold_left G.add_vertex G.empty tids in
  let g = CL.fold_left (fun g (s, d) -> G.add_edge g s d) g edges in
  g
;;

let decl_branch_tbl tbl_id selection_rules =
  Table (tbl_id, selection_rules, None)
;;

let decl_branch_tbl_with_def tbl_id selection_rules def_rule =
  Table (tbl_id, selection_rules @ [Match (Cid.fresh ["r"], [], def_rule)], None)
;;

(* new cid_decls helpers (6/21) *)
let objs_of_tid cid_decls tid =
  (tid :: aids_of_tid cid_decls tid) @ oids_of_tid cid_decls tid
  |> CL.map (Cid.lookup cid_decls)
;;

let acns_of_tid cid_decls tid =
  let aids = aids_of_tid cid_decls tid in
  aids |> CL.map (Cid.lookup cid_decls)
;;

let alus_of_tid cid_decls tid =
  oids_of_tid cid_decls tid |> CL.map (Cid.lookup cid_decls)
;;

(* remove decls from cid_decls *)
let remove_decls cid_decls decls =
  CL.map id_of_decl decls |> CL.fold_left Cid.remove cid_decls
;;

(* find functions look through a list of declarations 
   to get information about identifiers *)
(* Given the identifier that points to a struct field, 
find the struct definition. *)
let find_structdef_of_var cid_decls cid =
  (* form of cid: 
    x1.x2...x_n-1.x_n -> 
      struct instance: x1.x2...x_n-1
      field : x_n 
      return the definition of the struct *)
  let ids = Cid.to_ids cid in
  (* let field_cid = CL.rev ids |> CL.hd |> Cid.id in  *)
  let struct_instance_cid = CL.rev ids |> CL.tl |> CL.rev |> Cid.create_ids in
  let struct_instance_decl = Cid.lookup cid_decls struct_instance_cid in
  match struct_instance_decl with
  | StructVar (_, _, struct_ty_id) -> Cid.lookup cid_decls struct_ty_id
  | _ -> error "expected a struct instance, but could not find one."
;;

(* find the width of a declared variable or struct field. *)
let find_width_of_var cid_decls cid =
  match cid with
  (* non-compound identifier --> this is a builtin datatype *)
  | Id _ ->
    (match Cid.lookup cid_decls cid with
    | MetaVar (_, i) -> i
    | _ -> error "cid is not declared as a globalmeta.")
  (* compound id --> this is a field in a struct datatype *)
  | Cid.Compound _ ->
    let struct_def = find_structdef_of_var cid_decls cid in
    let field_cid = Cid.to_ids cid |> CL.rev |> CL.hd |> Cid.id in
    (match struct_def with
    | StructDef (_, _, field_defs) -> CL.assoc field_cid field_defs
    | _ -> error "expected a struct def, but got something else")
;;
