open Batteries
open CoreSyntax
open Collections

type level = int

type var_binding =
  { level : level (* Last level this variable was modified at *)
  ; body : exp option
        (* This is Some e iff we know for sure the variable is bound to e *)
  ; is_declared : bool
  }

type env = var_binding IdMap.t

(* Merge the env after a branch of an if/match statement with the prior environment.
   Discard any variables that were defined inside the branch, and add declarations for
   any variables that were mutated *)
let merge_branches base_stmt level prior_env branch_envs =
  let decls_to_add = ref base_stmt in
  let merge =
    IdMap.merge (fun id ao bo ->
        match ao, bo with
        | None, _ -> (* Variable was declared inside the branch, discard *) None
        | _, None -> failwith "Sanity check: should be impossible"
        | Some x, Some y ->
          let is_declared =
            if x.is_declared
            then true
            else if x.level < y.level && x.level = level
            then (
              (* Was declared on this level, and modified in the branch *)
              let body =
                (* Should be safe since it was undeclared before, and hence unmodfied *)
                Option.get x.body
              in
              decls_to_add
                := sseq !decls_to_add (slocal id (Option.get body.ety) body);
              true)
            else false
          in
          let level =
            if x.level = y.level then (* Not modified *) x.level else level
          in
          let body =
            match x.body, y.body with
            (* Direct equality comparison is a bit risky, but I think it's fine *)
            | Some b1, Some b2 -> if b1.e = b2.e then x.body else None
            | _ -> None
          in
          Some { level; is_declared; body })
  in
  let merged = List.fold_left merge prior_env branch_envs in
  !decls_to_add, merged
;;

(* True if it's safe to inline the variable. It's currently considered unsafe
   if the expression contains an array or hash call *)
let should_inline exp =
  ignore exp;
  (* TODO: this *)
  false
;;

let interp_exp env e =
  ignore (env, e);
  (* TODO: This *)
  e
;;

(* Partially interpret a statement. Return the new statment, the environment after
   interpreting it, and a list of changed variables that were declared at a higher
   level than the statement's scope. We only return the env so we can pass it
   between the halves of an SSeq *)
let rec interp_stmt env level s : statement * env =
  match s.s with
  | SNoop -> s, env
  | SUnit e -> { s with s = SUnit (interp_exp env e) }, env
  | SPrintf (str, es) ->
    { s with s = SPrintf (str, List.map (interp_exp env) es) }, env
  | SGen (b, e) -> { s with s = SGen (b, interp_exp env e) }, env
  | SRet eopt -> { s with s = SRet (Option.map (interp_exp env) eopt) }, env
  | SSeq (s1, s2) ->
    let s1, env1 = interp_stmt env level s1 in
    let s2, env2 = interp_stmt env1 level s2 in
    let s' =
      if s1.s = SNoop then s2.s else if s2.s = SNoop then s1.s else SSeq (s1, s2)
    in
    { s with s = s' }, env2
  (* Cases where we bind/mutate variables *)
  | SLocal (id, ty, exp) ->
    let exp = interp_exp env exp in
    if should_inline exp
    then snoop, IdMap.add id { level; body = Some exp; is_declared = false } env
    else
      ( { s with s = SLocal (id, ty, exp) }
      , IdMap.add id { level; body = None; is_declared = true } env )
  | SAssign (id, exp) ->
    let exp = interp_exp env exp in
    let old_binding = IdMap.find id env in
    let new_binding = { old_binding with level; body = Some exp } in
    { s with s = SAssign (id, exp) }, IdMap.add id new_binding env
  (* Cases where we branch *)
  | SIf (test, s1, s2) ->
    let test = interp_exp env test in
    (* TODO: Check to see if we actually need to branch *)
    let s1, env1 = interp_stmt env (level + 1) s1 in
    let s2, env2 = interp_stmt env (level + 1) s2 in
    let base_stmt = { s with s = SIf (test, s1, s2) } in
    merge_branches base_stmt level env [env1; env2]
  | SMatch (es, branches) ->
    let es = List.map (interp_exp env) es in
    (* TODO: Check if we need to branch *)
    let branches, envs =
      List.map
        (fun (p, stmt) ->
          let stmt', env' = interp_stmt env level stmt in
          (p, stmt'), env')
        branches
      |> List.split
    in
    let base_stmt = { s with s = SMatch (es, branches) } in
    merge_branches base_stmt level env envs
;;
