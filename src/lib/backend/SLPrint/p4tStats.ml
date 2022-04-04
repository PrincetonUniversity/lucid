open Batteries
open SLSyntax
open Base
module CL = Caml.List
module Option = Caml.Option

let stat_report str =
  Console.show_message str ANSITerminal.Green "Compiled P4 stats"
;;


let stages_fn = "num_stages.txt"

(* Print statistics about the compiled program.
  right now, just the number of stages. *)

let max xs = 
  let f m x = 
    if (m > x) then (m)
    else (x)
  in   
  CL.fold_left f 0 xs
;;

let print_stats (prog : tblSeqProg) builddir =
  let max_stage = 
    stageMap_of_tbls prog.tspdecls 
    |> CL.split 
    |> fst 
    |> max 
  in 
  let num_stages = string_of_int (max_stage+1) in 
  stat_report ("max stage: "^(string_of_int max_stage));
  stat_report ("number of stages: "^(num_stages));
  IoUtils.writef (builddir ^ "/" ^ stages_fn) num_stages
;;