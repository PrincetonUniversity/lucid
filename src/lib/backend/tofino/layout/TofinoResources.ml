(* TofinoResources -- functions to get the resources used by expressions and statements *)

open CoreSyntax
open TofinoCfg

(* logging *)
module DBG = BackendLogging
let outc = ref None
let dprint_endline = ref DBG.no_printf
let start_logging () = DBG.start_mlog (!IoUtils.moduleLogDir) __FILE__ outc dprint_endline



(** Arrays **)
let arrays_of_exp exp = 
  let string_of_fcncid cid = 
    Caml.String.concat "." @@ Cid.names cid 
  in
  match exp.e with 
  | ECall(fid, args, _) -> (
    match (string_of_fcncid fid) with 
      | "Array.get"| "Array.getm" | "Array.set"
      | "Array.setm" | "Array.update" 
      | "Array.update_complex" | "PairArray.update" -> 
      [List.hd args |> InterpHelpers.name_from_exp]
      | _ -> []
  )
  | _ -> []
;;

let rec arrays_of_stmt stmt : Cid.t list = 
  let nonunique_cids = match stmt.s with
  | SLocal(_, _, exp)
  | SAssign(_, exp)
  | SUnit(exp) -> arrays_of_exp exp
  | SIf(exp, s1, s2) -> 
      (arrays_of_exp exp)
      @(arrays_of_stmt s1)
      @(arrays_of_stmt s2)
  | SMatch(es, bs) ->
    (List.map arrays_of_exp es |> List.flatten)
    @(List.map 
        (fun (_, stmt) -> arrays_of_stmt stmt)
        bs
      |> List.flatten
    )
  | SSeq(s1, s2) -> 
    (arrays_of_stmt s1)@(arrays_of_stmt s2)
  | _ -> []
  in 
  nonunique_cids |> (MatchAlgebra.unique_list_of_eq Cid.equal)
;;

(*** sram of arrays called by statement ***)
let sblocks_of_arr cell_sz num_cells =
  let base_bits = cell_sz * num_cells in
  let n_blocks = ((base_bits - 1) / (128*1024) + 1) (* ceil (kb / 128kbit) *)
    + 1 (* overhead blk *)
  in
  n_blocks
;;

let sblocks_of_stmt arr_info stmt =
  (* List.iter (fun (cid, (cell_sz, num_cells)) -> 
    print_endline ("  " ^ (fst cid) ^ ": " ^ (string_of_int cell_sz) ^ " " ^ (string_of_int num_cells)))  *)
  (* arr_info
  ; *)

  List.fold_left (fun total arr_id -> 
    let cell_sz, num_cells = List.assoc arr_id arr_info in
    total + (sblocks_of_arr cell_sz num_cells))
    0
    (arrays_of_stmt stmt |> List.map Cid.to_id)
;;

(*** hash units ***)
let rec hashers_of_exp exp = 
  match exp.e with 
  | EOp(_, es) -> List.map hashers_of_exp es |> List.flatten
  | ECall(fcn_cid, es, _) -> (
    match (Cid.names fcn_cid) with
    | "Sys"::"random"::_ -> 
      exp::(List.map hashers_of_exp es |> List.flatten)
    | _ -> List.map hashers_of_exp es |> List.flatten
  )
  | EHash(_, es) -> 
    exp::(List.map hashers_of_exp es |> List.flatten)
  | EFlood(e) -> hashers_of_exp e
  | _ -> []

(* get the statements with hash ops in them *)
let rec hashers_of_stmt stmt = 
  let hashers_in_exps es = 
    (* FIXED BUG: without flatten, 
       a list of empty lists was getting counted as an 
       exp with a hasher in it!*)
    (* should double check for that bug elsewhere too. *)
    match (List.map hashers_of_exp es |> List.flatten) with
    | [] -> false | _ -> true
  in 
  match stmt.s with
  | SAssign(_, exp)
  | SLocal(_, _, exp)
  | SUnit(exp)
  | SGen(_, exp) 
  | SRet(Some exp) -> (
    match hashers_of_exp exp with 
    | [] -> []
    | _ -> [stmt]
  )
  | SPrintf(_, exps) -> (
    match List.map hashers_of_exp exps |> List.flatten with 
    | [] -> []
    | _ -> [stmt]
  )
  | SIf(exp, s1, s2) -> (
    match hashers_of_exp exp with 
    | [] -> (hashers_of_stmt s1)@(hashers_of_stmt s2)
    | _ -> stmt::(hashers_of_stmt s1)@(hashers_of_stmt s2)
  )
  | SRet(None)
  | SNoop -> []
  | SSeq(s1, s2) -> (hashers_of_stmt s1)@(hashers_of_stmt s2)
  | SMatch(es, bs) -> (
    let hashers_of_branch (_, stmt) = hashers_of_stmt stmt in
    let branch_hashers = (List.map hashers_of_branch bs |> List.flatten) in 
    let es_has_hasher = hashers_in_exps es in 
    match es_has_hasher with 
      | true -> stmt::branch_hashers
      | false -> branch_hashers
    (* (match es_has_hasher with | true -> [stmt] | false -> [])@(List.map hashers_of_branch bs |> List.flatten) *)
  )
  (* | STableMatch(tm) ->
    if (hashers_in_exps (tm.keys@tm.args))
    then [stmt]
    else [] *)
  | STupleAssign({exp}) -> 
    if (hashers_in_exps [exp])
    then [stmt]
    else []
  (* | STableInstall(_, entries) -> 
    let hasher_in_entry ent =
      (hashers_in_exps ent.ematch || hashers_in_exps ent.eargs)
    in
    let uses_hash = List.fold_left 
      (fun in_prev entry -> in_prev || (hasher_in_entry entry))
      false
      entries
    in 
    if (uses_hash) then [stmt] else [] *)


let calc_hash_bits_block ty =
  ty_to_size ty
  (* ((((ty_to_size ty) - 1) / 16) + 1) * 16 ;; *)
let calc_salu_hash_bits ty = min 22 (ty_to_size ty)

(* count the number of hash alus needed to implement an expression *)
let rec hash_ops_of_exp exp = match exp.e with 
| EOp(_, es) -> hash_ops_of_exps es
| ECall(fcn_cid, es, _) -> (
  match (Cid.names fcn_cid) with
    | "Sys"::"random"::_ -> 
      calc_hash_bits_block exp.ety + (hash_ops_of_exps es)
    | ["Array"; "get"]
    | ["Array"; "getm"]
    | ["Array"; "set"]
    | ["Array"; "setm"]
    | ["Array"; "update"]
    | ["Array"; "update_complex"]
    | ["PairArray"; "update"] -> (
      match es with 
      | _::arr_idx::args -> (
        match arr_idx.e with
        (* if the index is already a hash, 
            then we can just count normally *)
        | EHash(_) -> hash_ops_of_exps args
        (* if it is _not_, then we have to 
            account for the fact that it will be implicitly 
          converted into a hash of the same output width *)
        | _ -> (
          (* tricky: i think it matters that 32 bit indices get hashed 
            with 22-bit hashes. That brings the total for 4 salu ops 
            down to 88 from 128. Else, we'd never be able to fit 4 salu 
            ops in a single stage. salu ops must be counted differently 
            than hash ops...  *)
            let res = calc_salu_hash_bits arr_idx.ety in
            (* print_endline ("hash_ops_of_exp");
            print_endline (CorePrinting.exp_to_string exp);
            print_endline (CorePrinting.ty_to_string exp.ety);
            print_endline (string_of_int res); *)
            res

            (* + (hash_ops_of_exps args)  *)
        )
      )
      | _ -> error "unexpected array call args"
    )    
    | _ -> hash_ops_of_exps es
)
(* one hash unit per 16 bits of data, plus whatever for nested expressions *)
| EHash(_, es) -> 
  let hash_bits =  calc_hash_bits_block exp.ety in
  let hash_bits_rest =  (hash_ops_of_exps es) in 
  (* print_endline ("--------");
  print_endline@@"exp: "^(CorePrinting.exp_to_string exp);
  print_endline (

    "hash bits (chuncks of 16): "^(string_of_int hash_bits)^" hash_bits_rest: "^(string_of_int hash_bits_rest));
  print_endline "----"; *)
  hash_bits+hash_bits_rest
| _ -> 0

and hash_ops_of_exps exps = 
  List.fold_left (fun ct e -> ct + hash_ops_of_exp e) 0 exps
;;

(* bug -- deduplicating statements is not right for array operations. 
   all the array ops in a table that use the same index variable get the same 
   hash unit (i think). So we need to deduplicate those based on 
   (arr_id, idx_exp), I think? *)
let rec hash_ops_of_stmt statement_cache stmt = 
  (* how many hash bits does a statement take, given that everything 
      in statement_cache is "free" *)
  if (List.exists (fun st -> equiv_stmt st stmt) statement_cache) then 
    statement_cache, 0
  else
    match stmt.s with
    | SAssign(_, exp)
    | SLocal(_, _, exp)
    | SUnit(exp)
    | SGen(_, exp) 
    | SRet(Some exp) -> 
      (* cache the "primitive" statements that compile to hash or salu ops *)
      (stmt::statement_cache), hash_ops_of_exp exp
    | SPrintf(_, _) -> statement_cache, 0 (*compile time compute *)
    | SIf(exp, s1, s2) -> 
      let s1_cache, hash_ops_s1 = hash_ops_of_stmt statement_cache s1 in 
      let s2_cache, hash_ops_s2 = hash_ops_of_stmt s1_cache s2 in
      let hash_ops_exp = hash_ops_of_exp exp in
      s2_cache, hash_ops_exp + hash_ops_s1 + hash_ops_s2
    | SRet(None)
    | SNoop -> statement_cache, 0
    | SSeq(s1, s2) -> 
      let s1_cache, hash_ops_s1 = hash_ops_of_stmt statement_cache s1 in 
      let s2_cache, hash_ops_s2 = hash_ops_of_stmt s1_cache s2 in
      s2_cache, (hash_ops_s1 + hash_ops_s2)
    | SMatch(es, bs) -> (
      let hash_ops_es = hash_ops_of_exps es in
      let (hash_ops_branches : int), (statement_cache : statement list) = List.fold_left
        (fun (hash_ops_branches, statement_cache) (_, st) -> 
          let statement_cache', ct_more = hash_ops_of_stmt statement_cache st in
          ((hash_ops_branches+ct_more), statement_cache')
        )
        (0, statement_cache)
        bs
      in
      statement_cache, (hash_ops_branches + hash_ops_es)
    )
    (* | STableMatch(tm) ->
      statement_cache,hash_ops_of_exps (tm.keys@tm.args) *)
    | STupleAssign({exp}) -> 
      statement_cache, hash_ops_of_exp exp
    (* | STableInstall(_, entries) -> 
      statement_cache,List.fold_left 
        (fun ct ent ->         
          ct + hash_ops_of_exps ent.ematch
          + hash_ops_of_exps ent.eargs)
        0
        entries *)
;;



(* array address expressions *)
let array_addrs_of_exp exp = 
  match exp.e with 
  | ECall(fid, args, _) -> (
    match (Cid.names fid) with
    (* for any array function, get the index/address expression,
       which is always the 2nd argument *)
    | ["Array"; "get"]
    | ["Array"; "getm"]
    | ["Array"; "set"]
    | ["Array"; "setm"]
    | ["Array"; "update"]
    | ["Array"; "update_complex"]
    | ["PairArray"; "update"] -> 
      [List.nth args 1]
    | _ -> []
  )
  | _ -> []
;;

let rec array_addrs_of_stmt stmt : exp list = 
  (* collect all the index/address expressions in the statement *)
  let all_addr_exprs = match stmt.s with
  | SLocal(_, _, exp)
  | SAssign(_, exp)
  | SUnit(exp) -> array_addrs_of_exp exp
  | SIf(exp, s1, s2) -> 
      (array_addrs_of_exp exp)
      @(array_addrs_of_stmt s1)
      @(array_addrs_of_stmt s2)
  | SMatch(es, bs) ->
    (List.map array_addrs_of_exp es |> List.flatten)
    @(List.map 
        (fun (_, stmt) -> array_addrs_of_stmt stmt)
        bs
      |> List.flatten
    )
  | SSeq(s1, s2) -> 
    (array_addrs_of_stmt s1)@(array_addrs_of_stmt s2)
  | _ -> []
  in 
  (* now make the list unique with structurally equal exps *)
  let res = all_addr_exprs |> (MatchAlgebra.unique_list_of_eq (CoreSyntax.equiv_exp)) in

  res
;;

let array_addr_nonhash_of_exp exp = 
  List.filter (fun exp -> match exp.e with 
  | EHash _ -> false | _ -> true)
  (array_addrs_of_exp exp)

let rec array_stmts_of_stmt stmt : statement list = 
  let all_array_ops = match stmt.s with
  | SLocal(_, _, exp)
  | SAssign(_, exp)
  | SUnit(exp) -> if (List.length@@array_addr_nonhash_of_exp exp) > 0 then [stmt] else []
  | SIf(exp, s1, s2) -> 
      (if (List.length@@array_addr_nonhash_of_exp exp) > 0 then [stmt] else [])
      @(array_stmts_of_stmt s1)
      @(array_stmts_of_stmt s2)
  | SMatch(_, bs) ->
    (List.map 
        (fun (_, stmt) -> array_stmts_of_stmt stmt)
        bs
      |> List.flatten
    )
  | SSeq(s1, s2) -> 
    (array_stmts_of_stmt s1)@(array_stmts_of_stmt s2)
  | _ -> []
  in 
  (* now make the list unique with structurally equal exps *)
  let res = all_array_ops |> (MatchAlgebra.unique_list_of_eq (CoreSyntax.equiv_stmt)) in
  (* if (List.length res > 1) then 
    (
    print_endline ("--------------");
    print_endline 
      ("[array_addrs_of_stmt] stmt:" 
      ^ (CorePrinting.stmt_to_string stmt));
    print_endline
      ("[array_addrs_of_stmt] all_addr_exprs:"^(Caml.String.concat ", " @@ List.map CorePrinting.exp_to_string all_addr_exprs));
    print_endline ("--------------");
    ); *)
  res
;;

(* this isn't right. pick up tomorrow. *)

let hash_bits_of_stmt dbg stmt = 
  let _, hash_ops = hash_ops_of_stmt [] stmt in
  (* let hash_stmts = hashers_of_stmt stmt |> (MatchAlgebra.unique_stmt_list) in
  (* this gets unique array ops that don't have hash ops as explicit first args *)
  let array_idx_stmts = array_stmts_of_stmt stmt in
  let hash_ops = List.fold_left (fun ct stmt -> ct + hash_ops_of_stmt stmt) 0 (hash_stmts@array_idx_stmts) in  *)
  let hbits =  hash_ops in
  if (dbg && (hbits = 22)) then (
    print_endline@@"-----\nstatement: "^(CorePrinting.statement_to_string stmt)^"\nhash bits:"^(string_of_int hbits)^"\n----";
    exit 1;  );
  hbits
;;
