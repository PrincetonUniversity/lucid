(* CoreResources -- functions to get the resources used by expressions and statements *)

open CoreSyntax
open CoreCfg

(** Arrays **)
let arrays_of_exp exp = 
  let string_of_fcncid cid = 
    Caml.String.concat "." @@ Cid.names cid 
  in
  match exp.e with 
  | ECall(fid, args) -> (
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
  | ECall(fcn_cid, es) -> (
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
       exp with a hasher in it! oh dear lort. *)
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
  | STableMatch(tm) ->
    if (hashers_in_exps (tm.keys@tm.args))
    then [stmt]
    else []
  | STableInstall(_, entries) -> 
    let hasher_in_entry ent =
      (hashers_in_exps ent.ematch || hashers_in_exps ent.eargs)
    in
    let uses_hash = List.fold_left 
      (fun in_prev entry -> in_prev || (hasher_in_entry entry))
      false
      entries
    in 
    if (uses_hash) then [stmt] else []


(* array address expressions *)
let array_addrs_of_exp exp = 
  match exp.e with 
  | ECall(fid, args) -> (
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
