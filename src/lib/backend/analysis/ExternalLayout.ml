(* export/import all the data 
   necessary to do layout externally. *)
open Yojson.Basic.Util
open TofinoCore
open CoreCfg
open CoreDfg
open CorePrinting

let info str = 
  Console.show_message str ANSITerminal.Green "External Layout"
;;

exception Error of string
let error s = raise (Error s)

(*** json printers ***)
let json_of_vertices g = 
  (* format: {"iid": int; "code" : str} *)
  let json_of_vertex v = 
    let viid = vertex_num v in
    let vcodestr = CorePrinting.statement_to_string v.stmt in
    `Assoc [
      ("sid", `Int viid); 
      ("statement",`String vcodestr)
    ]
  in
  let vertex_jsons = (Dfg.fold_vertex 
    (fun v json_vs -> 
    (json_of_vertex v)::json_vs)
    g
    [])
  in
  `List vertex_jsons
;;

let json_of_edges g = 
  (* format: {"src_iid" : int; "dst_iid" : int; "dependency_type" : string; *)
  let json_of_edge e =
    let (src, dep_ty, dst) = e in
    `Assoc [
      ("src_sid", `Int (vertex_num src));
      ("dst_sid", `Int (vertex_num dst));
      ("dep_ty", `String (string_of_data_dependency dep_ty))
    ]  
  in
  let edge_jsons = (Dfg.fold_edges_e
    (fun e json_es -> 
      (json_of_edge e)::json_es)
    g
    [])
  in
  `List edge_jsons
;;

let json_of_globals tds =
  (* format: {"id" : string; "ty" : string; "size" : int *)
  let array_specs = array_dimensions tds in
  let json_of_global (id, (slot_width, num_slots)) = 
    `Assoc [
      "id", `String (Id.to_string id);
      "ty", `String "Array";
      "size", `Int (slot_width * num_slots)
    ]
  in
  `List (List.map json_of_global array_specs)
;;

let json_of_locals tds = 
  (* format: {"id" : string; "ty" : string; "size" : int *)
  let json_of_local id ty =
    `Assoc [
      "id", `String (Id.to_string id);
      "ty", `String (CorePrinting.ty_to_string ty);
      "size", `Int (InterpHelpers.width_from_ty ty)
    ]
  in
  let local_specs = ref [] in 
  let v = object 
    inherit [_] s_iter as super    
    method! visit_SLocal _ id ty _ =
      local_specs := (json_of_local id ty)::!local_specs;
    end
  in
  v#visit_tdecls () tds;
  let local_specs = !local_specs in
  let shared_local_specs = 
    List.map (fun (id, ty) -> json_of_local id ty)
    ((main tds).shared_locals)
  in
  `List (shared_local_specs@local_specs)
;;

let export tds json_fn =
  let old_verbose_types = Cmdline.cfg.verbose_types in
  Cmdline.cfg.verbose_types <- true;
  let cfg = CoreCfg.cfg_of_main tds in 
  let cdg = CoreCdg.to_control_dependency_graph cfg in        
  let dfg = CoreDfg.process cdg in   
  let json = `Assoc [
    "globals", json_of_globals tds;
    "locals", json_of_locals tds;
    "nodes", json_of_vertices dfg;
    "edges", json_of_edges dfg
    ] 
  in
  info@@"exporting dataflow graph representation of program to: "^(json_fn);
  Yojson.Basic.pretty_to_channel (open_out json_fn) json;
  Cmdline.cfg.verbose_types <- old_verbose_types;
;;
