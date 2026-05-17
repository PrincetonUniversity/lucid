open Batteries

exception Error of string
let error s = raise (Error s)

(*** Simulator's internal network topology ***)
(* Maps switch -> port -> (switch * port) *)
module IntMap = Map.Make (Int)

type linkmap = (int * int) option IntMap.t IntMap.t
(* Some(s, p) --> the port goes to switch s on port p *)
(* None --> the port exists in the sender, but to something external *)
let empty_linkmap = IntMap.empty
let default_linkmap num_switches (recirc_ports : int list) =
  (* 1 switch with only a recirc port *)
  List.fold_left2
    (fun acc swid recirc_port -> IntMap.add swid 
      (* add the recirculation port mapping *)
      (IntMap.add recirc_port (Some (swid, recirc_port)) IntMap.empty) acc)
    IntMap.empty
    (List.init num_switches (fun n -> n))
    recirc_ports
;;

let add_internal_link id port (dst : (int * int)) (linkmap: linkmap) =
  (* add a link from (id, port) -> dst *)
  try
    IntMap.modify
      id
      (fun map ->
        match IntMap.find_opt port map with
        | None -> IntMap.add port (Some dst) map
        | Some Some dst' when dst = dst' -> map
        | _ ->
          error
          @@ Printf.sprintf
                "Config error: switch:port pair %d:%d assigned to two different \
                destinations!"
                id
                port)
      linkmap
  with
  | Not_found -> error @@ "Invalid switch id " ^ string_of_int id
;;
let add_external_link id port linkmap =
  (* add a link from (id, port) -> None to indicate that it's external *)
  try
    IntMap.modify
      id
      (fun map ->
        match IntMap.find_opt port map with
        | None -> IntMap.add port None map
        | Some None -> map
        | _ ->
          error
          @@ Printf.sprintf
                "Config error: switch:port pair %d:%d assigned to both an internal and \
                external destination!"
                id
                port)
      linkmap
  with
  | Not_found -> error @@ "Invalid switch id " ^ string_of_int id
;;


(* for a given switch, get a list of (out_port, neighbor_switch) tuples *)
let get_internal_non_recirc_ports (topology : (int * int) option IntMap.t IntMap.t) sw = 
  (* get all the ports that are internal, except recirculation ports *)
  match IntMap.find_opt sw topology with
  | Some map ->       
    IntMap.fold (fun port recv_opt acc -> 
      match recv_opt with 
      | Some(recv_sw, _) -> 
        if (recv_sw <> sw) then port::acc else acc
      | None -> acc) map []
  | None -> []
;;

let lookup_dst_switch (top : linkmap) (sw, p) =
  (* find the switch ID connected to the other end of (sw, p)*)
  match IntMap.find_opt sw top with
  | None -> (* sender switch doesn't exist -- major error *)
    error @@ "lookup_dst_switch error -- Invalid switch id " ^ string_of_int sw
  | Some map ->
    (match IntMap.find_opt p map with
      | None -> None (* port doesnt exist on sender switch. Happens in certain cases, but would like to refactor out later *)
      | Some None -> None (* port exists on sender switch, but is external. Expected.*)
      | Some(Some (dst_sw, _)) -> Some dst_sw (* port connects to defined internal link. *)
    )
;;

let lookup_dst (top : linkmap) (sw, p) =
  (* find the switch ID and port connected to the other end of (sw, p) *)
  match IntMap.find_opt sw top with
  | Some map -> (
    match IntMap.find_opt p map with 
      | None -> None (* port doesnt exist on sender switch. Happens in certain cases, but would like to refactor out later *)
      | Some None -> None (* port exists on sender switch, but is external. Expected.*)
      | Some(Some dst) -> Some dst (* port connects to defined internal link. *)
  )
  | None -> error @@ "lookup_dst error -- Invalid switch id " ^ string_of_int sw
;;


(* config for the interpreter's simulator; comes from interp spec file, not stdin args *)
type simulation_config =
  { num_switches : int
  ; links : linkmap (* Links are only the internal links in the simulation. *)
  ; max_time : int
  ; default_input_gap : int
  ; generate_delay : int
  ; propagate_delay : int
  ; random_seed : int
  ; random_delay_range : int
  ; random_propagate_range : int
  ; drop_chance : int
  }
;;

let default_simulation_config num_switches recirc_ports = 
 { num_switches
    ; max_time=0
    ; links = default_linkmap num_switches recirc_ports
    ; default_input_gap=0
    ; generate_delay=0
    ; propagate_delay=0
    ; random_delay_range=1 (* 1 because upper bound is exclusive *)
    ; random_propagate_range=1 
    ; random_seed=0
    ; drop_chance=0
    }

;;