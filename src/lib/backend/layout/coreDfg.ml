(* coreDfg.ml -- 
    convert a control dependency graph into a data flow graph 
    (really a data dependency graph) 
    The control dependency graph has nodes that test 
    the conditions necessary for their execution, and 
    an edge (s, t) represents that s comes before t 
    in the control flow of the program. We _could_ 
    schedule the nodes for execution based on the CDG, 
    but just because s comes before t doesn't mean that 
    s must be executed before t -- they might operate 
    on independent data. 
    So, in the data dependency graph, nodes have the same 
    meaning as in a control dependency graph, but 
    an edge from (s, t) means that there is a data 
    dependency from s to t, and so s _must_ execute 
    before t.
    There are three kinds of data dependencies: 
    (read, write), (write, write), or (write, read).
    We should be able to eliminate the 
    (read, write) and (write, write) dependencies 
    with future optimizations. But for now, they 
    all matter for scheduling. 
*)
open Collections
open CoreSyntax
open MatchAlgebra
open CoreCfg

(* logging *)
module DBG = BackendLogging
let outc = ref None
let dprint_endline = ref DBG.no_printf
let start_logging () = DBG.start_mlog __FILE__ outc dprint_endline



(* a node is a statement and a condition that 
   must hold for it to be executed *)
module DfgNode = CfgNode 

(* an edge between (s, t) is a variable that is either 
   read or written in s and read or written in t. *)
type data_dependency = 
  | DRW
  | DWW
  | DWR
  | DNone

let string_of_data_dependency d =
  match d with 
  | DRW -> "RW"
  | DWW -> "WW"
  | DWR -> "WR"
  | DNone -> "NO DEPENDENCY"

module DfgEdge = struct 
  type t = data_dependency
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = ( = )
  let default = DNone
end

module Dfg = Graph.Persistent.Digraph.ConcreteLabeled (DfgNode) (DfgEdge)
module DfgTopo = Graph.Topological.Make (Dfg)

module DfgDotConfig = struct
  include Dfg
  let graph_attributes _ = []
  let edge_attributes (_, (d:data_dependency), _) = 
    [`Label (string_of_data_dependency d)]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes _ = [`Shape `Box]
  let vertex_name v = "\"" ^str_of_cond_stmt v^ "\""
  let default_vertex_attributes _ = []
end

module DfgDot = Graph.Graphviz.Dot (DfgDotConfig)

let print_dfg fn (g:Dfg.t) =
    DfgDot.output_graph (Caml.open_out_bin fn) g
;;

(** helpers **)
let preds_of_vertices dfg vs =
  CL.map (Dfg.pred dfg) vs |> CL.flatten |> MiscUtils.unique_list_of
;;
let preds_of_vertices dfg vs =
  CL.map (Dfg.pred dfg) vs |> CL.flatten |> MiscUtils.unique_list_of
;;

(** dependency helpers **)
let rec read_ids_of_exp exp : Cid.t list = 
  match exp.e with 
  | EVar(cid) -> [cid]
  | EOp(_, args)
  | ECall(_, args)
  | EHash(_, args) ->
    List.map read_ids_of_exp args |> List.flatten
  | EFlood(arg) -> read_ids_of_exp arg
  | EVal _ -> []
  | ETableCreate _ -> [] 
;;

let read_ids_of_exps exps = List.map read_ids_of_exp exps |> List.flatten
;;

let rec read_ids_of_stmt (stmt:CoreSyntax.statement) : Cid.t list = 
  match stmt.s with 
  | SNoop -> []
  | SRet(None) -> []
  | SLocal(_, _, exp) 
  | SAssign(_, exp)
  | SRet(Some(exp)) -> read_ids_of_exp exp
  | SPrintf(_, exps) -> List.map read_ids_of_exp exps |> List.flatten
  | SIf(exp, s1, s2) -> 
    read_ids_of_exp exp@(read_ids_of_stmt s1)@(read_ids_of_stmt s2)
  | SSeq(s1, s2) -> (read_ids_of_stmt s1)@(read_ids_of_stmt s2)
  | SMatch(exps, branches) -> 
      let ids_in_branches = List.map
        (fun (_, s) -> read_ids_of_stmt s)
        branches
        |> List.flatten
      in 
      let ids_in_exps = List.map read_ids_of_exp exps |> List.flatten in 
      ids_in_exps@ids_in_branches

  | SGen(GSingle(Some(eport)), eev) 
  | SGen(GMulti(eport), eev)
  | SGen(GPort(eport), eev) -> (
    (read_ids_of_exp eport)@(read_ids_of_exp eev)
  )
  | SGen(GSingle(None), eev) -> read_ids_of_exp eev
  (* invalidate call _writes_ to its arg fields *)
  | SUnit(exp) -> (
    match exp.e with 
    | ECall(fcn_cid, _) -> (
      match Cid.names fcn_cid with 
      | "Sys"::"invalidate"::_ -> []
      | _ -> read_ids_of_exp exp
    )
    | _ -> read_ids_of_exp exp
  )  
  | STableMatch(tm) -> 
    (read_ids_of_exps tm.keys)
    @(read_ids_of_exps tm.args)
  | STableInstall(_, entries) -> 
    List.map 
      (fun entry -> 
        (read_ids_of_exps entry.ematch)
        @(read_ids_of_exps entry.eargs))
      entries
    |> List.flatten
;;

(* return the IDs that a statement writes to *)
let rec write_ids_of_stmt (stmt:CoreSyntax.statement) : (Cid.t list) = 
  match stmt.s with 
  | SNoop
  | SRet(_)
  | SGen(_, _)
  | SPrintf(_) -> [] 
  | SLocal(id, _, _) -> [Cid.id id]
  | SAssign(id, _) -> [id]
  | SIf(_, s1, s2)
  | SSeq(s1, s2) -> 
    (write_ids_of_stmt s1)@(write_ids_of_stmt s2)
  | SMatch(_, branches) -> 
    List.map 
      (fun (_, s) -> write_ids_of_stmt s)
      branches
    |> List.flatten
  (* an invalidate call writes to its fields *)
  | SUnit(exp) -> (
    match exp.e with 
    | ECall(fcn_cid, args) -> (
      match Cid.names fcn_cid with 
      | "Sys"::"invalidate"::_ -> (
        List.map (fun arg_exp -> 
          match arg_exp.e with
          | EVar(cid) -> cid
          | _ -> error "sys.invalidate with an arg that is not an evar?"
        )
        args
      )
      | _ -> []
    )
    | _ -> []
  )  
  | STableInstall _ -> []
  | STableMatch(tm) -> CL.map Cid.id tm.outs
;;

let rec read_ids_of_pattern pattern =
  match pattern with
  | [] -> []
  | (exp, _)::pattern_remaining -> (cid_of_exp exp |> Cid.to_id)::(read_ids_of_pattern pattern_remaining)
;;

let read_ids_of_patterns patterns =
  List.map read_ids_of_pattern patterns |> List.flatten
;;

let rec read_ids_of_condition (c:edge_condition) = 
  match c with 
    CMatch(c) -> 
      let negs_read_ids = read_ids_of_patterns c.negs in   
      let pos_read_ids =  match c.pos with 
        | None -> []
        | Some pattern -> read_ids_of_pattern pattern
      in 
      ShareMemopInputs.unique_list_of_eq Id.equals (negs_read_ids@pos_read_ids)
    | CNone -> []
    | CExp(_) -> 
      error "[coreDfg] CExp edge conditions not supported"
;;

(* read ids of node, write ids of node. *)
let read_ids_of_vertex v = 
  read_ids_of_stmt v.stmt
;;
let write_ids_of_vertex v =
  write_ids_of_stmt v.stmt
;;


(* a map from an id to a list of statements that read / write that id *)
type usemap = {
  reads : (vertex_stmt list) CidMap.t;
  writes : (vertex_stmt list) CidMap.t;
}
let empty_usemap = {reads = CidMap.empty; writes = CidMap.empty} ;;

(* does cs read id *)
let is_reader usemap cs id = 
  match CidMap.find_opt id usemap.reads with 
  | None -> false
  | Some (css) -> 
      MiscUtils.contains css cs 
;;

let is_writer usemap cs id = 
  match CidMap.find_opt id usemap.writes with 
  | None -> false
  | Some (css) -> 
      MiscUtils.contains css cs 
;;


let string_of_dcons dconstr = match dconstr with 
  | DRW -> "read-write"
  | DWR -> "write-read"
  | DWW -> "write-write"
  | DNone -> "NONE"
;;

(* get dependency edges from s --> t in ts of 
   type dconstr as defined by s_var_getter and t_checker *)
let get_dependencies dconstr s_var_getter t_checker (s:vertex_stmt) (ts:vertex_stmt list) =   
  let s_varids = s_var_getter s in 
  let dependent_ts = 
    let update_for_var deps id =
      let update_for_desc deps desc =
        match t_checker desc id with 
        | false -> deps
        | true -> desc::deps
      in 
      CL.fold_left update_for_desc deps ts
    in 
    CL.fold_left update_for_var [] s_varids
  in 
  !dprint_endline@@"[get_dependencies "^(string_of_dcons dconstr)^"]";
  !dprint_endline@@"s:"^(CoreCfg.summarystr_of_stmt s.stmt true);
  !dprint_endline@@"ts:";
  List.iter (fun t -> !dprint_endline@@"DEPENDEE:"^(CoreCfg.summarystr_of_stmt t.stmt true)) dependent_ts; 
  !dprint_endline "---------";
  let res = CL.map
    (fun desc -> (s, dconstr, desc))
    (MiscUtils.unique_list_of dependent_ts)
  in
  res
;;
(* read, write *)
let get_drws usemap v descs =
  get_dependencies DRW read_ids_of_vertex (is_writer usemap) v descs
;;
(* write, read *)
let get_dwrs usemap v descs =
  get_dependencies DWR write_ids_of_vertex (is_reader usemap) v descs
;;
(* write, write *)
let get_dwws usemap v descs =
  get_dependencies DWW write_ids_of_vertex (is_writer usemap) v descs
;;

let print_usemap usemap =
  !dprint_endline ("----read map----");
  let ids = CidMap.keys usemap.reads in 
  BatEnum.iter (fun id -> 
    !dprint_endline ("------------");
    !dprint_endline("id: "^(Cid.to_string id));
    List.iter (fun v -> 
      !dprint_endline ("----");
      !dprint_endline (CorePrinting.stmt_to_string v.stmt);
      !dprint_endline ("----");
      )
      (CidMap.find id usemap.reads)
  )
  ids;
  !dprint_endline ("------------");
;;

let process cfg = 

  (* compute the variable : read / write maps *)
  let update_usemap  (v:vertex_stmt) (m:usemap) : usemap = 
    let update m id = match (CidMap.find_opt id m) with 
      | Some current_users -> (
        let new_m = CidMap.remove id m in
        CidMap.add id (v::current_users) new_m
      )
      | None -> CidMap.add id [v] m
    in 
    {
      reads = List.fold_left update m.reads (read_ids_of_vertex v);
      writes = List.fold_left update m.writes (write_ids_of_vertex v);
    }
  in 
  let usemap = Cfg.fold_vertex update_usemap cfg empty_usemap in 
  print_usemap usemap;
  (* exit 1; *)
  (* walk through the nodes in a cfg. 
      for each node: 
        1. find descendents. 
        2. filter groups of decendents by is_reader and is_writer
        3. add node -> descendent edges
  *)
  let update_dfg cfg_node dfg = 
    let descs = descendents cfg cfg_node in 
    let dfg = Dfg.add_vertex dfg cfg_node in 
    let edges = 
        (get_drws usemap cfg_node descs)
      @(get_dwrs usemap cfg_node descs)
      @(get_dwws usemap cfg_node descs)
    in 
    CL.fold_left Dfg.add_edge_e dfg edges
  in 
  let dfg = Cfg.fold_vertex update_dfg cfg (Dfg.empty) in 
  dfg 
;;
