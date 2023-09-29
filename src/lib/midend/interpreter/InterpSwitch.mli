open Batteries
open CoreSyntax
open InterpSyntax
open InterpControl
module Env = Collections.CidMap
module CommandQueue : BatHeap.H
module EventQueue : BatHeap.H


(* S with type t = (control_val * int) *)

(* stats counter for a switch *)
type stats_counter =
{ entries_handled : int
; total_handled : int
}

type gress = 
  | Ingress
  | Egress
type ingress_destination = 
  | Port of int
  | Switch of int
  | PExit of int

(* utility functions that the network (InterpState) 
   provides a switch (InterpSwitch) *)
type 'nst network_utils = 
{
       save_update : 'nst -> 'nst state -> unit (* for updating queues *)
     ; lookup_dst : 'nst -> (int * int) -> (int * int) (* for routing *)
     ; lookup_switch : 'nst -> int -> 'nst state (* for moving events *)
     ; get_time : 'nst -> int
     ; calc_arrival_time : 'nst -> int -> int -> int -> int
}   

and 'nst state = 
  { 
    swid : int
  ; config : InterpConfig.config
  ; global_env : 'nst ival Env.t
  ; command_queue : CommandQueue.t
  ; ingress_queue : EventQueue.t
  ; egress_queue : EventQueue.t
  ; pipeline : Pipeline.t
  ; exits : (ievent * int option * int) Queue.t
  ; drops : (ievent * int) Queue.t
  ; retval : value option ref
  ; counter : stats_counter ref
  ; utils : 'nst network_utils
  }

(* as you can see, the interface for interpSwitch is 
   kind of messy and in the middle of refactoring. *)

val create : InterpConfig.config -> 'nst network_utils -> int -> 'nst state

val mem_env : cid -> 'nst state -> bool
val lookup : cid -> 'nst state -> 'nst ival
val add_global : cid -> 'nst ival -> 'nst state -> 'nst state

val update_counter : event_sort -> 'nst state -> unit (* mutable *)

val ingress_send : 'nst -> 'nst state -> ingress_destination -> event_val -> unit
val egress_send : 'nst -> 'nst state -> int -> event_val -> unit


val load_interp_input : 'nst -> 'nst state -> int -> interp_input -> unit

val next_event : int -> 'nst state -> ('nst state * (event_val * int * gress) list) option

val ready_egress_events : int -> 'nst state -> 'nst state * ((event_val * int * gress) list)

val ready_control_commands : 'nst -> 'nst state -> int -> control_val list

val all_egress_events : 'nst state -> 'nst state * ((event_val * int * int * gress) list)

val next_time : 'nst state -> int option

val to_string : ?show_vars:bool -> ?show_pipeline:bool -> ?show_queue:bool -> ?show_exits:bool -> 'nst state -> string