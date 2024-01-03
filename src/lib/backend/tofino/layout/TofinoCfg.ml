(* CoreSyntax control flow graph *)
open Batteries
open Printf
open InterpHelpers
module CL = Caml.List
open CoreSyntax
open TofinoCore
open MatchAlgebra

exception Error of string

let error s = raise (Error s)

type edge_condition = 
    | CNone
    | CExp of exp
    | CMatch of condition

type vertex_stmt = {
    vuid : int;
    stmt : statement;
    solitary : bool;
    (* solitary means this statement cannot share a table with any others. *)
    (* <<refactor>> now that there are pragmas, 
       we should use the solitary pragma for this instead 
       of adding a new field.  *)
}
let cur_uid = ref 0 ;;

(* let empty_vertex_stmt ()  = 
  cur_uid := (!cur_uid) + 1;    
{
    vuid = 0;
    (* vuid = (!cur_uid); *)
    stmt = snoop;
    solitary = false;
}
 *)
let vertex_of_stmt stmt solitary = 
{
    stmt = stmt;
    solitary = solitary;
    vuid = stmt.sspan.spid;    
}

let cond_stmt_equal c1 c2 = 
    c1.stmt.sspan.spid = c2.stmt.sspan.spid
;;

(* a node in the cfg is any statement besides a sequence *)
module CfgNode = struct
  type t = vertex_stmt
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = cond_stmt_equal 
end


(* an edge in the cfg represents 
   possible control flow. 
   An edge is annotates with the condition 
   that causes the edge to be taken, either 
   a boolean expression from an if statement
   OR
   a sequence of "missed rules" from a match 
   statement, followed by the rule that triggers 
   this edge.
*) 


module CfgEdge = struct
  type t = edge_condition
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = ( = )
  let default = CNone
end

module Cfg = Graph.Persistent.Digraph.ConcreteLabeled (CfgNode) (CfgEdge)
module CfgTopo = Graph.Topological.Make (Cfg)
module CfgDom = Graph.Dominator.Make(Cfg)
module Dfs = Graph.Traverse.Dfs(Cfg)


let vertex_num v = v.stmt.sspan.spid
;;

let vertex_prags v = v.stmt.spragmas
;;

(*** graph printing ***)
(* statement --> graph node string *)
let summarystr_of_branch (pats, stmt) =
    "| "
    ^(CL.map CorePrinting.pat_to_string pats |> String.concat ", ")
    ^" -> {"^(CorePrinting.statement_to_string stmt)^"}";
;;
let summarystr_of_stmt (stmt) is_solitary_match = 
    match stmt.s with
    | SMatch(es, bs) ->
        if (is_solitary_match) then (
            (string_of_int stmt.sspan.spid)^": "
            ^"match ("^(CorePrinting.es_to_string es)^") with\n"
            ^(CL.map summarystr_of_branch bs |> (String.concat "\n"))
            (* ": match ("^(CorePrinting.es_to_string es)^") ..." *)        
        ) else (
            (string_of_int stmt.sspan.spid)^": "
            ^"match ("^(CorePrinting.es_to_string es)^") with\n"
            ^(CL.map summarystr_of_branch bs |> (String.concat "\n"))
(*             (string_of_int stmt.sspan.spid)^": "
            ^"match ("^(CorePrinting.es_to_string es)^") ...\n" *)
        )

    | SIf(e, _, _) -> 
        "{"^(string_of_int stmt.sspan.spid)^": if ("^(CorePrinting.exp_to_string e)^")..."^"}"
    | _ -> "{"^(string_of_int stmt.sspan.spid)^": "^(CorePrinting.statement_to_string stmt)^"}"
;;

let str_of_edge_condition e = 
    match e with
    | CNone -> ""
    | CExp(exp) -> CorePrinting.exp_to_string exp
    | CMatch(cond) -> MatchAlgebra.string_of_condition cond
;;

let str_of_cond_stmt cs =  
    (summarystr_of_stmt cs.stmt cs.solitary)
;;

(* full printers for text representation of graph *)
let rec is_noop stmt =
  match stmt.s with
  | SNoop -> true
  | SSeq(s1, s2) -> (is_noop s1) && (is_noop s2)
  | SIf(_, s1, s2) -> (is_noop s1) && (is_noop s2)
  | SMatch(_, bs) ->
    List.fold_left 
      (fun acc (_, b) -> 
        acc && (is_noop b))
      true
      bs
  | _ -> false
;;

let vertex_to_string v =
    let is_noop = (is_noop v.stmt) in
    Printf.sprintf 
        "%s[%s]: %s"
        (if is_noop then "//NOOP\n" else "")
        (string_of_int v.stmt.sspan.spid)
        (CorePrinting.statement_to_string v.stmt)
;;
let str_of_vertex = vertex_to_string

let str_of_edge (s, _, d) =
    Printf.sprintf 
        "[%s] -> [%s]"
        (string_of_int s.stmt.sspan.spid)
        (string_of_int d.stmt.sspan.spid)
;;




let str_of_graph g =
    let strs = 
    Cfg.fold_vertex
        (fun v strs -> strs@[str_of_vertex v])
        g
        []
    in
    Cfg.fold_edges_e 
        (fun e strs -> strs@[str_of_edge e])
        g
        strs
;;

(* node and edge helpers functions *)
let id_of_v (cs:vertex_stmt) =
  cs.stmt.sspan.spid |> string_of_int
;;
let ids_of_vs vs =
  CL.map id_of_v vs |> String.concat ","
;;

module CfgDotConfig = struct
  include Cfg
  let graph_attributes _ = []
  let edge_attributes (_, (cnd:edge_condition), _) = 
    [`Label (str_of_edge_condition cnd)]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes _ = [`Shape `Box]
  let vertex_name v = "\"" ^str_of_cond_stmt v^ "\""
  let default_vertex_attributes _ = []
end

module CfgDot = Graph.Graphviz.Dot (CfgDotConfig)

let print_cfg fn g =
    CfgDot.output_graph (Caml.open_out_bin fn) g
;;


(* more debugging...  really have to clean up the helpers in this file, 
   and the graph-based IRs in general. They are getting too complicated 
   to be so add-hoc, and now need a little more structure for maintainability. *)
let vertex_to_debugstr g vertex = 
    [vertex_to_string vertex]
    @["----predecessors----"]
    @(List.map vertex_to_string (Cfg.pred g vertex))
    @["----successors----"]
    @(List.map vertex_to_string (Cfg.succ g vertex))
    |> String.concat "\n"
;;

let debug_vertex g num node =
    if ((vertex_num node) == num)
    then (
        print_endline ("found node "^(string_of_int num));
        vertex_to_debugstr g node |> print_endline;
        exit 0;
    )

let debug_graph tagstr g num =
    print_endline ("checking for problem node "^(tagstr));
    Cfg.iter_vertex (debug_vertex g num) g;
    print_endline ("didn't find node "^(string_of_int num)^" in "^(tagstr))
;;



(*** graph building ***)

(* find the root in the cfg
   ..there must be a way to do this 
   without completely traversing the graph.. *)

let roots g = 
    let collect_roots v roots =
        if ((Cfg.in_degree g v) = 0)
        then v::roots
        else roots
    in 
    Cfg.fold_vertex collect_roots g []
;;

let leaves g = 
    let collect_leaves v roots =
        if ((Cfg.out_degree g v) = 0)
        then v::roots
        else roots
    in 
    Cfg.fold_vertex collect_leaves g []
;;

let concat_graphs g1 g2 = 
    let add_edge (src, edge, dst) g = 
        Cfg.add_edge_e g (src, edge, dst)
    in         
    let g = Cfg.fold_edges_e add_edge g1 Cfg.empty in 
    let g = Cfg.fold_edges_e add_edge g2 g in 
    g
;;


let pattern_of_pats keys pats : pattern =
    CL.combine keys pats
;;

let pattern_of_branch es b =
    pattern_of_pats 
        (es)
        (fst b)
;;

(* Some match nodes may get compiled directly into their own tables.
    User-written match statements with:
        1. many branches (>20 -- this is somewhat arbitrary)
        2. no sequences of statements 
    Also, the match statements that wrap table_matchs are solitary. *)
let is_solitary_match stmt =
    match Pragma.find_sprag_args "solitary" [] stmt.spragmas with
    | Some(_) -> true
    | _ -> false
;;

(* compute the condition statement for each branch in a match statement *)
let conditions_of_match es bs = 
    let pattern_of_branch_in_order (prev_branch_patterns, prev_branch_conditions) b =
        let b_pattern = pattern_of_branch es b in
        let b_condition = {negs=prev_branch_patterns; pos=Some(b_pattern);} in
        (prev_branch_patterns@[b_pattern], prev_branch_conditions@[b_condition])
    in
    let _, branch_conditions = CL.fold_left pattern_of_branch_in_order ([], []) bs in
    branch_conditions
;;

(* main function: convert a statement into a cfg *)
let rec cfg_of_statement_inner (st:statement) = 
    match st.s with
    | SNoop -> Cfg.add_vertex Cfg.empty (vertex_of_stmt st false) 
    | SUnit(_)
    | SLocal(_)
    | SAssign(_)
    | SPrintf(_)
    | SRet(_)  
    | SGen(_) ->
        Cfg.add_vertex Cfg.empty (vertex_of_stmt st false)
    (* | STableMatch(_) ->  *)
        (* a table match remains a table match *)
        (* error "coreCfg.cfg_of_statement_inner] table match statements should be wrapped in solitary matches" *)
        (* Cfg.add_vertex Cfg.empty (vertex_of_stmt st true) *)
    | STupleAssign _ -> 
        (* same semantics as table match, assign, and local *)
        error "[coreCfg.cfg_of_statement_inner] tuple assignments should be removed from tofino program by this point."
    | STableInstall(_) -> 
        error "[coreCfg.cfg_of_statement_inner] table installs should be removed from tofino program by this point."
    | SIf(e, s1, s2) ->
        (* 1. compute graph of s1 and s2 *)
        let g_s1 = cfg_of_statement_inner s1 in
        let g_s2 = cfg_of_statement_inner s2 in
        (* 2. add the edges and nodes from g_s1 and g_s2 *)
        let g = concat_graphs g_s1 g_s2 in 
        (* 3. add the edges from this node to root(g_s1) and root(g_s2) *)
        CL.fold_left 
            (fun g (root, e) -> Cfg.add_edge_e g ((vertex_of_stmt st false) , CExp(e), root))
            g
            (
                (List.map (fun v -> (v, e)) (roots g_s1))
                @(List.map (fun v -> (v, op_sp Neg [e] e.ety e.espan)) (roots g_s2))
            )
    | SSeq(s1, s2) ->
        let g_s1 = (cfg_of_statement_inner s1) in 
        let g_s2 = (cfg_of_statement_inner s2) in 
        let g = concat_graphs g_s1 g_s2 in 
        (* add edges from leaves s1 --> roots s2 *)
        let fold_over_leaf (g:Cfg.t) (leaf:Cfg.vertex) =
            let fold_over_root (g:Cfg.t) (root:Cfg.vertex) =
                Cfg.add_edge_e g (leaf, CNone, root)
            in
            CL.fold_left fold_over_root g (roots g_s2)
        in 
        CL.fold_left fold_over_leaf g (leaves g_s1)
    | SMatch(es, bs) -> 
        if (is_solitary_match st)
            (* a match statement that just stays as a match statement *)
        then (
            Cfg.add_vertex Cfg.empty (vertex_of_stmt st true)
        )
        (* a match statement that gets broken apart *)
        else (
            let branch_conditions = conditions_of_match es bs in 
(*             print_endline ("input match statement: ");
            print_endline (CorePrinting.stmt_to_string st); *)
            (* empty branches --> empty match that will be deleted later. *)
            let new_bs = [] in 
            let new_st = {st with s=SMatch(es, new_bs)} in 

            (* add the graph of this branch to the graph, 
               and an edge from statement --> root branch_graph *)
            let add_branch (g, prev_patss, branch_idx) (pats, b_stmt) = 
                let branch_g = cfg_of_statement_inner b_stmt in 
                let g = concat_graphs g branch_g in 
                let edge_label = CL.nth branch_conditions branch_idx in 
(*                 print_endline ("branch condition: ");
                print_endline (str_of_edge_condition (CMatch(edge_label))); *)
                let g = CL.fold_left 
                    (fun g root -> Cfg.add_edge_e g ((vertex_of_stmt new_st false), CMatch(edge_label), root))
                    g
                    (roots branch_g)
                in 
                (g, prev_patss@[pats], branch_idx+1)
            in 
            let g, _, _ = (CL.fold_left add_branch (Cfg.empty, [], 0) bs) in 
            g
        )

;;

let unique_spans st = 
    let cur_span = ref 0 in

    let refresh_span sp =
      cur_span := !cur_span + 1;
      { sp with Span.spid = !cur_span }
    in 
  let v =
    object
      inherit [_] s_map as super
      method! visit_sp _ sp = 
        let sp = refresh_span sp in 
        sp
    end
  in
  v#visit_statement () st
;;


let rec remove_single_noop_seq statement =   
    match statement.s with 
    | SSeq(s1, s2) -> (
        match s1.s, s2.s with 
        | SNoop, SNoop -> remove_single_noop_seq s1
        | SNoop, _ -> remove_single_noop_seq s2
        | _, SNoop -> remove_single_noop_seq s1
        | _, _ -> 
            {statement with s=SSeq(remove_single_noop_seq s1, remove_single_noop_seq s2);}
    )
    | SMatch(es, bs) -> (
        let new_bs = 
            CL.map 
                (fun (ps, s) -> ps, remove_single_noop_seq s)
                bs
        in 
        {statement with s=SMatch(es, new_bs);}
    ) 
    | _ -> statement
;;

let delete_noop_seqs statement =
    statement  
    |> remove_single_noop_seq
    |> remove_single_noop_seq
;;

(* main cfg building function *)
let cfg_of_statement statement =
    statement 
    |> delete_noop_seqs
    |> (sseq snoop)
    |> unique_spans
    |> cfg_of_statement_inner
;;

(*  convert back into a single statement 
    unused and out of date (7/25/23)
    (definitely doesn't handle table matches correctly. or solitary statements)
    depreciated for now *)
let statement_of_cfg cfg =
    let rec stmt_of_vertex v = 
        match v.stmt.s with
        | SNoop 
        | SUnit _ 
        | SLocal _ 
        | SAssign _ 
        | SPrintf _
        | SGen _ 
        | SRet _
        (* | STableMatch _ ->
            (
            match (Cfg.succ cfg v) with 
            | [] -> v.stmt
            | [next_node] -> sseq (v.stmt) (stmt_of_vertex next_node)
            | _ -> error "[stmt_of_vertex] expected at most 1 successor for this type of cfg node"
        ) *)
        (* same semantics as table match *)
        | STupleAssign _ -> 
            (
            match (Cfg.succ cfg v) with 
            | [] -> v.stmt
            | [next_node] -> sseq (v.stmt) (stmt_of_vertex next_node)
            | _ -> error "[stmt_of_vertex] expected at most 1 successor for this type of cfg node"
        )
        | SSeq _ -> error "[stmt_of_vertex] sseq not expected"
        | SIf _ -> error "sif not expected"
        | STableInstall(_) -> 
            error "[coreCfg.statement_of_cfg] table installs should be removed from tofino program by this point."
        | SMatch(exps, branches) -> (
            let pats = List.split branches |> fst in
            let next_vs = Cfg.succ cfg v in
            let branch_conditions = conditions_of_match exps branches in  
            (* for each branch, find the out edge with the same condition *)
            let pat_cond = List.combine pats branch_conditions in
            let edge_vertex = List.map (fun next_v -> Cfg.find_edge cfg v next_v, next_v) next_vs in 
            let vertex_of_pat (p: pat list) : Cfg.vertex =
                let cond = List.assoc p pat_cond in 
                let results = List.filter 
                    (fun (oe, (_:Cfg.vertex)) -> 
                        let _, oe, _ = oe in 
                        let out_edge_condition = match oe with
                            | CMatch(c) -> c
                            | _ -> error "[vertex_of_pat] out edge of a match should have CMatch condition"
                        in
                        equiv_conditions cond out_edge_condition
                    )
                    edge_vertex
                in
                match results with 
                    | [(_, ov)] -> ov
                    | _ -> error "[vertex_of_pat] could not find an out edge of the match statement with the same condition as one of its patterns"
            in
            let new_branches = List.map (fun pats -> pats, stmt_of_vertex (vertex_of_pat pats)) pats in
            {v.stmt with s=SMatch(exps, new_branches)}
        )
    in
    match (roots cfg) with 
        | [root] -> stmt_of_vertex root
        | _ -> error "[statement_of_cfg] expected exactly 1 root vertex"
;;


let test_cfg_builder () =
    let st1 = sassign 
        (Cid.create ["a"])
        (eval_bool false)
    in
    let st2 = sassign 
        (Cid.create ["b"])
        (eval_bool false)
    in

    let st3 = sassign 
        (Cid.create ["c"])
        (eval_bool false)
    in

    let sseq = 
    sseq 
        (sseq 
            (sseq st1 snoop) 
            st2
        ) 
        (sseq snoop st3) 
    in 
    cfg_of_statement sseq
;;


(* find all the descendents of v in g *)
let descendents g v = 
    Dfs.fold_component List.cons [] g v |> List.rev |> List.tl
    (* rev |> tl removes v itself from the list *)
;;


(* new cfg for updated tofinocore *)
open TofinoCore
let cfg_of_component_main (ds:TofinoCore.tdecls) = 
    let main_hdl = main_handler_of_decls ds in
    let main_stmt = match main_hdl.hdl_body with
        | SFlat(stmt) -> stmt
        | SPipeline _ -> error "[cfg_of_main] handler is already laid out!"
    in
    cfg_of_statement main_stmt
;;

