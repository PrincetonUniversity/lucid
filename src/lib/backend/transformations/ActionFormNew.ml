(* 
  This pass converts the branches of a match statement into 
  calls to labeled blocks. The labeled blocks represent 
  actions in P4. 

  A labeled block is just a statement with a label. Before 
  executing a program, all labeled block should be inlined.
  In the process, statements are also deduplicated. 

  To interpret the program, labeled blocks should be inlined 
  back into their match statements.
 
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

  There is a special case: statements that cannot be 
  executed inside of a P4 action. By this point, the 
  only such statement in a program should be table_match 
  statements. When we encounter a match statement with 
  a table match as a branch, this pass transforms it into 
  an IF statement that executes the table match. The 
  match statement around the table_match must have a 
  very specific form: it must match on 1 variable, 
  the table's callnum variable, and have two cases: 
  "0 -> Noop" and "1 -> {table_match(...);}". Previous 
  stages of the backend obey this restriction, if they 
  are implemented correctly. 

  Example:
    table ftbl...;
    handle foo() {
      match (ftbl_callnum) with
        | 0 -> {}
        | _ -> {table_match(ftbl, ...);}
    }
    --> 
    handle foo() {
      if (ftbl_callnum != 0) {
        table_match(ftbl, ...);  
      }
    }



*) 

open CoreSyntax
open TofinoCoreNew
open InterpHelpers

exception Error of string
let error s = raise (Error s)

(* a cache of labeled blocks that have been created. *)
module LSCache = Map.Make(
  struct
    type t = CoreSyntax.s
    let compare s1 s2 =
      if (CoreSyntax.equiv_stmt (CoreSyntax.statement s1) (CoreSyntax.statement s2))
      then (0)
      else (Pervasives.compare s1 s2)
  end
)
type lscache = Id.t LSCache.t
let empty_labeled_statement_cache : lscache = LSCache.empty

let dblock id stmt =
  {td=TDOpenFunction(id, [], stmt); tdspan=Span.default; tdpragma = None;}
;;

let counter = ref 0

let next () =
  counter := !counter + 1;
  !counter
;;

let fresh_statementlabel () = 
  "labeledstmt_"^(string_of_int (next()))
;;
(* transform a branch into a labeled-statement call and create the labeled statement 
   if an equivalent one has not already been created for this match statement. *)
let process_branch cache stmt =
  let cache, block_id, new_block_opt = match LSCache.find_opt stmt.s cache with 
    | Some block_id -> cache, block_id, None
    | None ->
      let block_id = Id.create (fresh_statementlabel ()) in
      (* this statement goes directly into the block *)
      let new_block = dblock block_id stmt in 
      LSCache.add stmt.s block_id cache, block_id, Some new_block
  in 
  (* the new statement calls the block *)
  let new_stmt = scall_sp (Cid.id block_id) [] (ty TBool) stmt.sspan in
  cache, new_stmt, new_block_opt
;;

(* table matches get transformed into if expressions *)
let process_table_match stmt : statement option =
  match stmt.s with
  | SMatch(es, branches) -> (
    match es, branches with
    | [tbl_call_var], [([PNum(nocall_val)],{s=SNoop;});([PWild],tm_stmt)] -> (
      match tm_stmt.s with
      | STableMatch(_) -> 
        if (nocall_val <> Z.of_int 0) then (error "[ActionForm.process_table_match] table_match doesn't test against zero.");
        let var_sz = size_of_tint (tbl_call_var.ety) in
        let if_exp = (op_sp Neq [tbl_call_var; (vint_exp (Z.to_int nocall_val) var_sz)]
              tbl_call_var.ety Span.default)
        in
        let if_stmt = 
            sifte
              if_exp
              (tm_stmt)
              (snoop)
        in
        Some if_stmt
      | _ -> None
    )
    | _ -> None
  )
  | _ -> None

(* transform a match statement into match-action form, 
   where each branch of the match is replaced by a 
   function call *)
let process_match match_stmt =
  match (process_table_match match_stmt) with
  (* special case -- a match statement that wraps a table_matches *)
  | Some(wrapped_tblmatch_stmt) -> [], wrapped_tblmatch_stmt
  | _ -> (
    match match_stmt.s with 
    | SMatch(keys, branches) -> (
      let pats, branch_stmts = List.split branches in 
      (* the cache ensures we don't generate duplicate functions *)
      let _, branch_stmts', labeled_stmts = 
        List.fold_left 
        (fun (cache, branch_stmts, labeled_stmts) stmt -> 
          let cache, new_stmt, labeled_stmt_opt = 
            process_branch cache stmt 
          in 
          let branch_stmts = branch_stmts@[new_stmt] in
          let labeled_stmts = 
            (match labeled_stmt_opt with 
            | Some labeled_stmt -> labeled_stmts@[labeled_stmt]
            | None -> labeled_stmts)
          in
          (cache, branch_stmts, labeled_stmts))
        (empty_labeled_statement_cache, [], []) 
        branch_stmts
      in
      let new_match = {match_stmt with s = SMatch(keys, List.combine pats branch_stmts')} in 
      labeled_stmts, new_match
    )
    | _ -> error "[process_match] not a match statement!"
  )
;;

(* dedup the match tables in a single stage *)
let rec process_stage stmt = 
  match stmt.s with 
  | SNoop -> [], stmt
  | SMatch _ -> process_match stmt
  | SSeq (s1, s2) -> 
    let s1_decls, new_s1 = process_stage s1 in
    let s2_decls, new_s2 = process_stage s2 in
    s1_decls@s2_decls, {stmt with s = SSeq(new_s1, new_s2)}
  | _ -> error "[process_stage] statement should be a noop, match, or seq"
;;

let rec process_stages stages = 
  match stages with 
  | [] -> [], []
  | hd_stage::stages -> (
    let hd_decls, hd_stage = process_stage hd_stage in
    let remaining_decls, remaining_stages = process_stages stages in
    hd_decls@remaining_decls, hd_stage::remaining_stages
  )
;;

let rec process_comp comp  = 
  (* let m = main tds in  *)
  let mhdl = main_handler_of_decls comp.comp_decls in
  let m = match mhdl.hdl_body with 
    | SPipeline(stmts) -> stmts 
    | _ -> error "[ActionFromNew.process] handler must be in laid-out form" 
  in

  let added_tds, m' = process_stages m in
  let mhdl' = {mhdl with hdl_body = SPipeline(m');} in
  (* let tds = replace_main_handler_of_decls comp.comp_decls mhdl' in *)
  let is_main_handler tdecl = match tdecl with
    | {td = TDHandler(HEvent(hdl)); _} -> hdl.hdl_id = mhdl.hdl_id
    | _ -> false
  in
  (* the added decls should come right before the main handler, not append
     to the beginning or end, because they may reference things declared before 
     the handler, and the handler references them. *)
  let comp_decls' = List.fold_left 
    (fun tds td -> 
      if is_main_handler td 
        then 
          let td = {td with td = TDHandler(HEvent(mhdl'));} in
          tds@added_tds@[td] 
        else 
          tds@[td])
    []
    comp.comp_decls
  in
  {comp with comp_decls = comp_decls'}
;;
