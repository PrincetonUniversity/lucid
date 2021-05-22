open Base
module CL = Caml.List
open InstrSyntax

(***** 
Syntax defining a block of instruction tables called in sequence. 
translates almost directly to a P4 control obj with a straightline apply block.
The tables may reference: 
  locally defined variables and objects (in tspdecls) 
  private lucid variabes (in tspglobals)
*****)

(* Statements that define the sequential control flow of a datapath. *)
type tblStmt =
  | Noop
  | CallTable of oid
  | Seq of tblStmt * tblStmt

and tblSeq = 
{
  tsname : oid;
  tsstmt     : tblStmt;
}

(* this is what gets printed to P4. *)
and tblSeqProg = 
{
  tspname : oid;
  tspglobals : mid list; (* next event type, exit event type, timestamp. 
  Should be depreciated now that we have structs *)
  tspglobal_widths : int list;
  tspdecls    : decl list;
  tsptblseq : tblSeq;
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
