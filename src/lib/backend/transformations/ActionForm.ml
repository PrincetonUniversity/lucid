(* 
  This pass converts the branches of a match statement into 
  calls to labeled block. A labeled block is just a statement with a label. Before 
  executing a program, all labeled block should be inlined.
  In the process, statements are also deduplicated. 

  Example:
    handle foo() {
      match (a) with 
        | 1-> {x = 1;}
        | 2 -> {x = 2;}
        | _ -> {x = 1;}
    }
    --> 
    labeled_statement s1 = {
      x = 1;
    }
    labeled statement s2 = {
      x = 2;
    }
    handle foo() {
      match (a) with 
        | 1-> {s1();}
        | 2 -> {s2();}
        | _ -> {s1();}
    }
*) 

open CoreSyntax
open TofinoCore
open InterpHelpers

exception Error of string
let error s = raise (Error s)

(* a cache of labeled blocks that have been created. *)
module BlockCache = Map.Make(
  struct
    type t = CoreSyntax.s
    let compare s1 s2 =
      if (CoreSyntax.equiv_stmt (CoreSyntax.statement s1) (CoreSyntax.statement s2))
      then (0)
      else (Pervasives.compare s1 s2)
  end
)
type blockcache = Id.t BlockCache.t
let empty_blockcache : blockcache = BlockCache.empty

let dblock id stmt =
  {td=TDLabeledBlock(id, stmt); tdspan=Span.default}
;;

let counter = ref 0

let next () =
  counter := !counter + 1;
  !counter
;;

let fresh_blockname () = 
  "block_"^(string_of_int (next()))
;;
(* dedup a statement in a branch, possibly creating a new labeled block *)
let dedup_branch cache stmt =
  let cache, block_id, new_block_opt = match BlockCache.find_opt stmt.s cache with 
    | Some block_id -> cache, block_id, None
    | None ->
      let block_id = Id.create (fresh_blockname ()) in
      (* this statement goes directly into the block *)
      let new_block = dblock block_id stmt in 
      BlockCache.add stmt.s block_id cache, block_id, Some new_block
  in 
  (* the new statement calls the block *)
  let new_stmt = scall_sp (Cid.id block_id) [] (ty TBool) stmt.sspan in
  cache, new_stmt, new_block_opt
;;

(* dedup a single match table 
  transform the table, create new code block declarations *)
let dedup_match match_stmt =
  match match_stmt.s with 
  | SMatch(keys, branches) -> (
    (* cache of labeled blocks created during the dedup *)
    let pats, stmts = List.split branches in 
    let _, new_stmts, new_dblocks = 
      List.fold_left 
      (fun (cache, new_stmts, new_dblocks) stmt -> 
        let cache, new_stmt, new_dblock_opt = 
          dedup_branch cache stmt 
        in 
        cache, 
        new_stmts@[new_stmt], 
        (match new_dblock_opt with 
          | Some block -> new_dblocks@[block]
          | None -> new_dblocks
        )
      )
      (empty_blockcache, [], []) 
      stmts
    in
    let new_match = {match_stmt with s = SMatch(keys, List.combine pats new_stmts)} in 
    new_dblocks, new_match
  )
  | _ -> error "[dedup_match] not a match statement!"
;;

(* dedup the match tables in a single stage *)
let rec dedup_stage stmt = 
  match stmt.s with 
  | SNoop -> [], stmt
  | SMatch _ -> dedup_match stmt
  | SSeq (s1, s2) -> 
    let s1_decls, new_s1 = dedup_stage s1 in
    let s2_decls, new_s2 = dedup_stage s2 in
    s1_decls@s2_decls, {stmt with s = SSeq(new_s1, new_s2)}
  | _ -> error "[dedup_stage] statement should be a noop, match, or seq"
;;

let rec dedup_stages stages = 
  match stages with 
  | [] -> [], []
  | hd_stage::stages -> (
    let hd_decls, hd_stage = dedup_stage hd_stage in
    let remaining_decls, remaining_stages = dedup_stages stages in
    hd_decls@remaining_decls, hd_stage::remaining_stages
  )
;;

let rec process tds = 
  let m = main tds in 
  let added_tds, new_main_body = dedup_stages m.main_body in
  let tds = update_main tds {m with main_body = new_main_body;} in 
  tds@added_tds
;;
