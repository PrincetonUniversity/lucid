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


let rec hashers_of_stmt stmt = 
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
    let es_has_hasher = match List.map hashers_of_exp es |> List.flatten with
      | [] -> false
      | _ -> true
    in 
    match es_has_hasher with 
      | true -> stmt::branch_hashers
      | false -> branch_hashers
    (* (match es_has_hasher with | true -> [stmt] | false -> [])@(List.map hashers_of_branch bs |> List.flatten) *)
  )
