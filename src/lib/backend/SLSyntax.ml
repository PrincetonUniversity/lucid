(* The final low level representation: a straight line of 
  calls to tables. Generated from PipeSyntax and 
  translates almost directly into the apply block 
  of a P4 control block. 

  There is only one transformation that happens on this 
  syntax: deduplication of structurally equivalent 
  commands in the program.
*)
open Base
module CL = Caml.List
open LLSyntax


(* Statements that define the sequential control flow of a datapath. *)
type tblStmt =
  | Noop
  | CallTable of oid
  | Seq of tblStmt * tblStmt

and tblSeq =
  { tsname : oid
  ; tsstmt : tblStmt
  }

(* this is what gets printed to P4. *)
and tblSeqProg =
  { tspname : oid
  ; tspglobals : mid list
  ; (* next event type, exit event type, timestamp. 
  Should be depreciated now that we have structs *)
    tspglobal_widths : int list
  ; tspdecls : decl list
  ; tsptblseq : tblSeq
  }

(* get a mapping from stage number to tables contained *)
type stageMap = (int * oid list) list

(* get (snum, tid) tups *)
let stage_of_tbl d =
  match d with
  | Table (oid, _, Some stg_num) -> Some (stg_num, oid)
  | _ -> None
;;

(* flatten a list of (k, v) pairs to a list of lists: (k1, [v1, v2])*)
let insert_tup (assoc_list : (int * oid list) list) (tup : int * oid) =
  let stg, tid = tup in
  match CL.assoc_opt stg assoc_list with
  | None -> (stg, [tid]) :: assoc_list
  | Some tids -> (stg, tid :: tids) :: CL.remove_assoc stg assoc_list
;;

let flatten_tups tups = CL.fold_left insert_tup [] tups

let stageMap_of_tbls decls : stageMap =
  CL.filter_map stage_of_tbl decls |> flatten_tups
;;

(*** Convert PipeSyntax into straightline syntax ***)
