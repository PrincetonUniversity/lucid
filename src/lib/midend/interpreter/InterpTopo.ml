(* Topology definition and json parsing for the interpreter. *)

open Batteries
open CoreSyntax
open Yojson.Basic
open InterpSpec
type json = Yojson.Basic.t

module Env = Collections.CidMap


(* 
The Lucid simulator has nodes, links, and ports. 
  - Nodes run Lucid programs
  - Links are bidirectional channels for event-based communication between nodes 
  - Ports are endpoints between a host and a link or the outside world
*)

(*** Data Types ***)
type port =
     | PLink of       {port_id : int}                  (* connects to another simulated switch *)
     | PInterface of  {port_id : int; ifname : string} (* connects to an interface; external *)
     | PRecirc of     {port_id : int}                  (* for packet recirculation *)
     (* Right now, Stdio can inject to any port, and sees all packets sent to disconnected ports or flooded.
        That is very convenient, but we could change it in the future. *)
     (* | PStdio of      {port_id : int}                  json events and control commands from stdio *)
     (* in the future, we may add a port type for queues *)

type node = {
    node_id : int;                  (* identifier of the node *)
    externs : (cid * value) list;   (* (variable name, value) bindings for externs defined in Lucid prog *)
    ports : port list;              (* a list of the ports available on the switch *)
    (* in the future, we may add fields for program name and symbolics input file (Parasol config) *)
}

type endpoint = { node_id : int; port_id : int; }

type link = {
    sidea : endpoint;
    sideb : endpoint;
    (* in the future, we may add optional fields for delay and loss rate *)
}

(* a topology is a collection of nodes and links *)
type topology = {
    nodes : node list;
    links : link list;
}

(*** JSON Parsing Functions ***)
(* 
**nodes (with inlined ports)**
The configuration below defines 2 almost identical nodes (node id 1 and node id 2). The only difference is the interface name of their second ports ("eth1" vs "eth2")
```json
"nodes": {
    1 : {
        "ports":{
            1 : {"type":"link"},
            2 : {"type":"interface", "ifname" : "eth1"},
            196 : {"type":"recirc"},
            192 : {"type":"stdio"}    
        },
        "externs":{"foo":8, "bar":9}
    },
    2 : {
        "ports":{
            1 : {"type":"link"},
            2 : {"type":"interface", "ifname" : "eth2"},
            196 : {"type":"recirc"},
            192 : {"type":"stdio"}    
        },
        "externs":{"foo":8, "bar":9}
    }
}
```
**links**
Links simply connect 2 (node, port) pairs.
```json
"links":[{"0:1":"1:0"}]
```
**link to port rules**
  - Each node must define a non-empty ports block
  - Exactly one link must bind to each PLink port
  - No links can bind to other port types

*)

(*** JSON to ADT parsers ***)

type json_dict =  (string * json) list
type json_list = json list

let parse_port (port_num_str,(port_args : json)) = 
  (* Parse a single port from json *)
  match port_args with 
    | `Assoc args -> (
      match List.assoc_opt "type" args with 
        | Some (`String "link") -> PLink {port_id = int_of_string port_num_str}
        | Some (`String "interface") -> (
          match List.assoc_opt "ifname" args with 
            | Some (`String ifname) -> PInterface {port_id = int_of_string port_num_str; ifname}
            | _ -> error "Interface port missing ifname field or has non-string ifname in specification file"
        )
        | Some (`String "recirc") -> PRecirc {port_id = int_of_string port_num_str}
        (* | Some (`String "stdio") -> PStdio {port_id = int_of_string port_num_str} *)
        | Some (`String other) -> error ("Unknown port type "^other^" in specification file")
        | _ -> error "Port entry missing type field or has non-string type in specification file"
    )
    | _ -> error "Unexpected format for port entry in specification file"
;;
let parse_ports (ports : json_dict ) = List.map parse_port ports
;;

let parse_node pp renaming (node_id_str, (node_args : json)) = 
  (* Parse a single node from json *)
  match node_args with 
    | `Assoc args -> (
      let ports = match List.assoc_opt "ports" args with 
        | Some (`Assoc ports) -> parse_ports ports
        | Some _ -> error "Unexpected format for ports field in specification file"
        | None -> error "Node entry missing ports field in specification file"
      in
      (* the main recirculation port is the first recirculation port defined, or 196 if there is none. *)
      let (main_recirc_port : int) =
        ports
        |> List.find_opt (function PRecirc _ -> true | _ -> false)
        |> Option.map (function PRecirc {port_id} -> port_id | _ -> failwith "impossible")
        |> Option.default 196
      in
      let externs_json = match List.assoc_opt "externs" args with 
        | Some (`Assoc externs) -> externs
        | Some _ -> error "Unexpected format for externs field in specification file"
        | None -> []
      in
      let externs_json = ("recirculation_port", `Int main_recirc_port)::externs_json in 
      let externs = List.map (fun (id_str, value_json) -> 
            let cid, v = InterpSpec.parse_extern pp renaming id_str value_json in
            (cid, v)
          ) externs_json
      in
      {node_id = int_of_string node_id_str; externs; ports}
    )
    | _ -> error "Unexpected format for node entry in specification file"
;;

let parse_nodes (pp : Preprocess.t) (renaming : Renaming.env) (nodes : json_dict ) = 
  List.map (parse_node pp renaming) nodes
;;

let parse_endpoint str : endpoint = 
  match String.split_on_char ':' str with
  | [id; port] ->(
    try 
      {node_id = int_of_string id; port_id = int_of_string port}
    with
      | _ -> error "Incorrect format for link endpoint."
  )
  | _ -> error "Incorrect format for link endpoint."
;;

let parse_link (link_json : (string * json)) = 
  (* Parse a single link from json *)
  let src_str, dst_json = link_json in
  let dst_str = match dst_json with 
    | `String s -> s
    | _ -> error "Unexpected format for link destination in specification file"
  in
    let sidea = parse_endpoint src_str in
    let sideb = parse_endpoint dst_str in
    {sidea; sideb}
  ;;
let parse_links (links : json list ) = 
  List.map 
      (function 
        | `Assoc [link_json] -> ( parse_link link_json )
        | _ -> error "Unexpected format for link entry in specification file"
      ) links  
;;


let parse_topology (pp : Preprocess.t) (renaming : Renaming.env) (topo : json_dict ): topology = 
  let nodes = 
    match List.assoc_opt "nodes" topo with 
      | Some(`Assoc nodes) -> parse_nodes pp renaming nodes
      | Some(_) -> error "Unexpected nodes format in specification file"
      | None -> error "Topology block missing nodes field in specification file"
  in
  let links =
    match List.assoc_opt "links" topo with 
      | Some(`List links) -> parse_links links
      | Some(_) -> error "Unexpected links format in specification file"
      | None -> error "Topology block missing links field in specification file"
  in
  {nodes; links}
;;

let other_end (links : link list) endpoint = 
  (* find the other end of an endpoint in the list of links  *)
  let rec find_other_end links endpoint = 
    match links with 
    | [] -> None
    | link::rest -> 
      if (link.sidea = endpoint) then Some(link.sideb)
      else if (link.sideb = endpoint) then Some(link.sidea)
      else find_other_end rest endpoint
  in
  find_other_end links endpoint
;;

(*** Converting topology to internal / pre-existing interpreter representation***)
let extract_linkmap (topo : topology) : InterpSim.linkmap =
  (* Extract a "linkmap", the interpreter internal representation of internal topology, 
     from the new topology representation.
     Internal links (PLink and PRecirc) are represented as Some(dst_node_id, dst_port_id);
     External links (PInterface) are represented as None in the linkmap. *)
  let linkmap_ref = ref InterpSim.empty_linkmap in
  for nodei = 0 to List.length topo.nodes - 1 do 
    let node = List.nth topo.nodes nodei in
    (* add the node to the linkmap *)
    linkmap_ref := InterpSim.IntMap.add node.node_id InterpSim.IntMap.empty !linkmap_ref;
     for porti = 0 to List.length node.ports - 1 do 
      let port = List.nth node.ports porti in
      match port with 
      | PLink {port_id} -> (
        let endpoint = {node_id = node.node_id; port_id} in
        match other_end topo.links endpoint with 
        | Some({node_id = dst_node_id; port_id = dst_port_id}) -> 

          linkmap_ref := InterpSim.add_internal_link node.node_id port_id (dst_node_id, dst_port_id) !linkmap_ref
        | None -> error "Config error: Internal port with no link in topology"
      )
      (* recirc ports are just internal links to the same port on the same switch *)
      | PRecirc {port_id} -> 
        linkmap_ref := InterpSim.add_internal_link node.node_id port_id (node.node_id, port_id) !linkmap_ref
      | PInterface {port_id; _} ->
        linkmap_ref := InterpSim.add_external_link node.node_id port_id !linkmap_ref
    done;
  done;
  !linkmap_ref
;;
  
let create_externs_env (renaming : Renaming.env) (topo : topology) : value Env.t list
  =
  let n_switches = List.length topo.nodes in
  let initial_envs = InterpSpec.builtins_env renaming n_switches in
  List.map2 (fun node initial_env -> 
    let extern_env = List.fold_left (fun env (id, v) -> Env.add id v env) initial_env (node : node).externs in
    extern_env
  )
  topo.nodes 
  initial_envs
;;

(* let update_config renaming topo (config: InterpSpec.t) : InterpSpec.t = 
  (* inline the topology configuration into the existing interpreter spec, 
     replacing the relevant parts *)
  {config with 
    externs = create_externs_env renaming topo;
    simconfig = {config.simconfig with 
      num_switches = List.length topo.nodes;
      links = extract_linkmap topo;
    }
  }
;; *)

(* main function *)
(* let parse_and_update_config (pp : Preprocess.t) (renaming : Renaming.env) (config: InterpSpec.t) filename = 
  (* parse the topology from a config file, update the configuration, which is assumed to already 
     be initialized with all the components that do not come from the topology *)
  let json = from_file filename in
  let topo_opt = match json with
    | `Assoc lst  -> (
      match List.assoc_opt "topology" lst with 
      | Some(`Assoc topo) -> Some(parse_topology pp renaming topo)
      | Some(_) -> error "Unexpected topology format in specification file"
      | _ -> None (* no topology config present *)
    )
    | _ -> error "Unexpected interpreter specification format"
  in
  match topo_opt with
  | Some topo -> update_config renaming topo config
  | None -> config
;; *)

(* parse a config file with a topology in it. *)
let contains_topology filename =
  let json = from_file filename in
  match json with
  | `Assoc lst -> List.exists (fun (k, _) -> k = "topology") lst
  | _ -> false
;;

let parse pp renaming filename = 
  let json = from_file filename in
  match json with
  | `Assoc lst ->
    (* parse all the simple global fields *)
    let default_input_gap = parse_int_entry lst "default input gap" 1 |> max 1 in
    let generate_delay = parse_int_entry lst "generate delay" 600 |> max 0 in
    let drop_chance = parse_int_entry lst "drop chance" 0 |> max 0 in
    let propagate_delay = parse_int_entry lst "propagate delay" 0 |> max 0 in
    let random_delay_range = parse_int_entry lst "random delay range" 1 |> max 1 in
    let random_propagate_range = parse_int_entry lst "random propagate range" 1 |> max 1 in
    let random_seed =      
      (* "random seed" or just "seed" *)
      parse_int_entry lst "random seed"       
        (parse_int_entry lst "seed" (int_of_float @@ Unix.time ()))
    in
    let max_time =
      match List.assoc_opt "max time" lst with
      | Some (`Int n) -> n
      | _ -> (
        (* for interactive mode, max_time is the start timestamp for incoming events *)
        (* TODO: this isn't quite right. A max_time of 0 in interactive mode 
           prevents any recirculated events from running until the simulation starts. 
           That might be what we want, but it also might not be. Hmm... *)
        if (InterpConfig.cfg.interactive)
          then 0
          else 10000
      )
    in    
    (* parse the topology *)
    let topo = match List.assoc_opt "topology" lst with 
      | Some(`Assoc topo) -> parse_topology pp renaming topo
      | Some(_) -> error "Unexpected topology format in specification file"
      | None -> error "Topology block missing from specification file"
    in
    let links = extract_linkmap topo in
    let num_switches = List.length topo.nodes in
    let externs = create_externs_env renaming topo in
    let extern_funs =
      match List.assoc_opt "python file" lst with
      | Some (`String file) ->
        create_foreign_functions renaming pp.extern_funs file
      | None ->
        if Collections.IdSet.is_empty pp.extern_funs
        then Env.empty
        else
          error
            "Extern functions were declared but no python file was provided!"
      | _ -> error "Non-string value for python_file"
    in    
    let ctl_pipe_name =
      match List.assoc_opt "control pipe" lst with
      | Some (`String filename) -> Some filename
      | _ -> None
    in    
    let _ =
      (* Initialize Python env *)
      match List.assoc_opt "python path" lst with
      | Some (`String library_name) -> Py.initialize ~library_name ()
      | Some _ -> error "Python path entry must be a string!"
      | None -> Py.initialize ()
    in
    let events =
      match List.assoc_opt "events" lst with
      | Some (`List lst) ->
        parse_interp_inputs pp renaming num_switches default_input_gap lst
      | _ -> [] (* default of no init events for interactive mode *)
    in
    let simconfig : InterpSim.simulation_config =
      { num_switches
      ; links
      ; max_time
      ; default_input_gap
      ; generate_delay
      ; propagate_delay
      ; random_delay_range
      ; random_propagate_range
      ; random_seed
      ; drop_chance
      }
    in
    {  externs; events; simconfig; extern_funs; ctl_pipe_name }    
    | _ -> error "Unexpected interpreter specification format"
;;
