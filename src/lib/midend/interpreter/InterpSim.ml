open Batteries

exception Error of string
let error s = raise (Error s)

(***  Simulated network topology ***)
(* Maps switch -> port -> (switch * port) *)
module IntMap = Map.Make (Int)

type topology = (int * int) IntMap.t IntMap.t
let empty_topology num_switches (recirc_ports : int list) =
  List.fold_left2
    (fun acc swid recirc_port -> IntMap.add swid 
      (* add the recirculation port mapping *)
      (IntMap.add recirc_port (swid, recirc_port) IntMap.empty) acc)
    IntMap.empty
    (List.init num_switches (fun n -> n))
    recirc_ports
;;

let ports_to_neighbors topology sw = 
  match IntMap.find_opt sw topology with
  | Some map ->       
    IntMap.fold (fun port (dst_sw, _) acc -> (port, dst_sw)::acc) map []
  | None -> []
;;

(* Maps switch * port -> switch * port according to the topology *)
let lookup_dst (top : topology) (sw, p) =
  match IntMap.find_opt sw top with
  | Some map ->
    (match IntMap.find_opt p map with
      | None -> 
      -1, p
      | Some ret -> ret)
  (* the switch ID does not exist in the topology... *)
  | None ->
    error @@ "lookup_dst error -- Invalid switch id " ^ string_of_int sw
;;


(* config for the interpreter's simulator; comes from interp spec file, not stdin args *)
type simulation_config =
  { num_switches : int
  ; links : topology (* Links are only the internal links in the simulation. *)
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
    ; links = empty_topology 1 recirc_ports
    ; default_input_gap=0
    ; generate_delay=0
    ; propagate_delay=0
    ; random_delay_range=1 (* 1 because upper bound is exclusive *)
    ; random_propagate_range=1 
    ; random_seed=0
    ; drop_chance=0
    }

;;