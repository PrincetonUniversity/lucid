(* This is a tofino-specific normalization pass that ensures all array operations 
   use addresses in a tofino-compatible way. The requirements are: 
   1. array operations placed in the same table must all access the array with the same address
   2. for array operations placed in different tables, the match condition of each table 
      must be exact (no wildcards).

  This module contains 2 passes that ensure these requirements are met:
    Pass 1 (in TofinoCore IR, before layout or conversion to graph form): 
            if any path constraint is not exact-match compatible, 
            add address variable and transform into single-access address form
    Pass 2 (in graph form, right before layout):
      1. make sure all statements that touch an array that is accessed by statements with multiple addresses are exact
      2. (after graph form)  remove all negation rules -- since they're exact, it doesn't matter
      3. (after graph form)  make nodes solitary

  Then, in the layout algorithm, the following should happen without any modifications:
    - for each statement group that accesses an array: 
      - if they have multiple addresses, each statement will be 
          1. exact
          2. solitary
        So each statement will be exact, meaning it can be implemented by an exact table. 
        And also, since each statment is solitary, we know that there will be no cross-product 
        effect that creates a need for a wildcard rule.
      - if they don't have multiple addresses, then it doesn't matter as they can be placed wherever
*)

open InterpHelpers
open CoreSyntax
open TofinoCore
open TofinoResources

let array_fun_cids = 
  List.map 
    (fun (fundef : InterpState.global_fun) -> fundef.cid)
    (Arrays.defs@PairArrays.defs)
;;

(**** helpers ****)
let list_contains lst target =
  List.exists (fun x -> x = target) lst
;;
let rec recursive_exp_filter exp f =
  if f exp then true
  else match exp.e with
    | EVal _ | EVar _ -> false
    | EOp (_, exp_list) | ECall (_, exp_list, _) | EHash (_, exp_list) ->
      List.exists (fun e -> recursive_exp_filter e f) exp_list
    | EFlood e | EProj (e, _) ->
        recursive_exp_filter e f
    | ETuple exp_list ->
      List.exists (fun e -> recursive_exp_filter e f) exp_list    
    | ERecord exp_list ->
      List.exists (fun (_, e) -> recursive_exp_filter e f) exp_list
;;

let has_inequality_exp exp = 
  (* does an expression contain an inequality? *)
  let is_inequality_op op = 
    match op with 
    | Neq | Less | More | Leq | Geq -> true
    | _ -> false
  in
  recursive_exp_filter exp (fun e -> 
    match e.e with 
    | EOp (op, _) -> is_inequality_op op
    | _ -> false
  )
;;
let unique_ct xs f_equiv =
  let rec aux acc xs =
    match xs with
    | [] -> acc
    | x::xs' ->
      if List.exists (f_equiv x) acc then
        aux acc xs'
      else
        aux (x::acc) xs'
  in
  List.length (aux [] xs)
;;

(* get array and index from args to Array method *)
let args_to_arr_addr args = match args with 
  | arr_exp::addr_exp::_ ->  InterpHelpers.name_from_exp arr_exp, addr_exp
  | _ -> raise (Failure "Array function call with less than 2 arguments")
;;


(* get the list of arrays that need to have 
   their address argument pre-computed, 
   and the type of each array's address variables *)
let find_arrays_to_transform component = 
  let array_addrs = Hashtbl.create 10 in
  let array_has_wildcard_branch = Hashtbl.create 10 in
  let v = 
    object (self)
    inherit [_] s_iter as super
    method! visit_TDGlobal _ id ty _ =
      match ty.raw_ty with 
      | TName(cid, _) when 
          ((Cid.equal cid Arrays.t_id) || 
          (Cid.equal cid PairArrays.t_id)) -> 
        (* this is an array, add an entry to the hash tables *)
        Hashtbl.add array_addrs (Cid.id id) [];
        Hashtbl.add array_has_wildcard_branch (Cid.id id) false
      | _ -> ()
    
    method! visit_SMatch in_wc_branch _ branches = 
      let rec process_branches in_wc_branch branches = 
        match branches with
        | [] -> ()
        | (pats, stmt)::rest -> 
          (* check if this branch is guarded by a wildcard *)
          let in_wc_branch = in_wc_branch || has_wildcard_pat pats in
          (* recurse on it *)
          self#visit_statement in_wc_branch stmt;
          (* process the rest of the branches *)
          process_branches in_wc_branch rest
      in
      process_branches in_wc_branch branches
    method! visit_SIf in_wc_branch econd stmt1 stmt2 = 
      self#visit_statement (in_wc_branch || has_inequality_exp econd) stmt1;
      self#visit_statement true stmt2

    method! visit_ECall in_wc_branch cid args _ = 
      (* if this is an array call *)
      if list_contains array_fun_cids cid then (
        (* add the addr exp to the list *)
        let arr_cid, addr_exp = args_to_arr_addr args in
        let arr_addrs = Hashtbl.find array_addrs arr_cid in
        Hashtbl.replace array_addrs arr_cid (arr_addrs@[addr_exp]);
        (* if this is in a branch with a wildcard, mark it *)
        if in_wc_branch then (
          Hashtbl.replace array_has_wildcard_branch arr_cid true
        )
      )
    end
  in
  (* get the array address expressions and the path condition complexity flag *)
  v#visit_component false component;

  let array_addrvar_tys = Hashtbl.fold (fun k v acc -> 
    acc@[k, (List.hd v).ety])
    array_addrs []
  in
  
  (* go through all array address expression lists, count unique using CoreSyntax.equiv_exp *)
  let array_addr_cts = Hashtbl.fold (fun k v acc -> 
    acc@[k, (unique_ct v CoreSyntax.equiv_exp)])
    array_addrs []
  in
  let array_wildcards = Hashtbl.to_seq array_has_wildcard_branch |> List.of_seq in 
  let arrays_to_transform = List.filter 
    (fun (cid, ct) -> 
      let has_wildcard = match List.assoc_opt cid array_wildcards with
        | Some b -> b
        | None -> false
      in
      ct > 1 && has_wildcard      
      )
    array_addr_cts
  in
  let arrays_to_transform = List.map fst arrays_to_transform in
  arrays_to_transform, array_addrvar_tys
;;

(* Walk the AST a second time. 
    1. For each array that needs to be transformed, add 
        a new intermediate variable for the address argument.
    2. For each Array method call that takes 
        an array that needs to be transformed: 
        - transform each array call of the form 
          ECall(arr_fcn_cid, [arr_exp; addr_exp; ...], ..) -> 
          ECall(arr_fcn_cid, [arr_exp; addr_cid_exp; ...], ..)
        - add a new assignment to the addr_cid var in the
          statement right before the ECall.
            Statement (... ECall ...) -> 
            SSeq(
              Statement(... addr_cid_exp = addr_exp ...),
              Statement(... ECall ...)
            )
   *)
let arrayid_to_addrid arr_cid = 
  Cid.to_id arr_cid |> Id.prepend_string "addr" |> Cid.id
;;

let transform_array_calls component 
  (arrays_to_transform : Cid.t list) 
  (array_addrvar_tys : (Cid.t * ty) list) = 
  (* the statement that initializes an address var before 
      an array method call that uses it as an argument. 
      This should always be empty at the start end end of
      processing each statement. 
      Before returning from a statement visitor, the 
      visitor should check to see if this is empty. If it is not,
      the visitor should return the sequence of the init statement
      and the processed statement. *)
  let init_stmts = ref [] in 
  let v = 
    
    object (_)
    inherit [_] s_map as super

    method! visit_exp () exp = 
      let exp = super#visit_exp () exp in
      match exp.e with 
      (* call to an array function *)
      | ECall (cid, args, u) when list_contains array_fun_cids cid -> (
        let arr_cid, addr_exp = args_to_arr_addr args in
        if list_contains arrays_to_transform arr_cid then (
          (* update the address argument expression *)
          let new_args = match args with 
            | arr_exp::addr_exp::rest -> 
              let new_addr_exp = evar_cid (arrayid_to_addrid arr_cid) addr_exp.ety in
              arr_exp::new_addr_exp::rest
            | _ -> raise (Failure "Array function call with less than 2 arguments")
          in
          (* create the assignment expression *)
          let init_stmt = sassign (arrayid_to_addrid arr_cid) addr_exp in
          (* update the call expression *)
          let new_exp = { exp with e = ECall (cid, new_args, u) } in
          init_stmts := init_stmt::(!init_stmts);
          new_exp
        ) (* a call to something, but not an array function *)
        else exp
      )
      (* not a call *)
      | _ -> super#visit_exp () exp

    method! visit_statement () stmt =
      (* make sure that init_stmts is empty, else throw an errpr *)
      if List.length !init_stmts > 0 then 
        raise (Failure "init_stmts not empty at start of statement processing");
      (* process the statement *)
      let stmt = super#visit_statement () stmt in
      (* prepend init stmts if necessary *)
      if List.length !init_stmts > 0 then (
        let rv = sequence_stmts ((List.rev !init_stmts)@[stmt]) in
        init_stmts := [];
        rv)
      else stmt
    end
  in
  (* update all the array calls *)
  let component = v#visit_component () component in
  (* now, add a shared local for each array *)
  let tds = List.fold_left 
    (fun tds arr_cid -> 
      let addr_ty = List.assoc arr_cid array_addrvar_tys in
      let addrcid = arrayid_to_addrid arr_cid in
      add_shared_local tds (addrcid |> Cid.to_id) addr_ty |> snd
    )
    component.comp_decls 
    arrays_to_transform
  in
  { component with comp_decls = tds }
;;

let process_component component = 
  (* first, find the arrays to transform and the array idx var tys *)
  let arrays_to_transform, array_addrvar_tys = find_arrays_to_transform component in
  (* second, transform the array calls *)
  transform_array_calls component arrays_to_transform array_addrvar_tys
;;

let process_core core_prog = 
  (* process the components individually *)
  List.map process_component core_prog
;;


(*** dependency graph node passes ***)
open TofinoCfg


let hashtbl_entry_append h k v = 
  match (Hashtbl.find_opt h k) with 
    | Some lst -> Hashtbl.replace h k (v::lst)
    | None -> Hashtbl.add h k [v]
;;
let string_of_fcncid cid = 
  Caml.String.concat "." @@ Cid.names cid 
;;

(* traverse the graph, find all the (array, addr) pairs *)
(* let get_array_addrs stmt =  *)
let acc_array_addrs = 
  object (_)
  inherit [_] s_iter as super
  method! visit_exp result exp = 
    super#visit_exp result exp;
    match exp.e with
      | ECall(fid, args, _) -> (
        match (string_of_fcncid fid) with 
          | "Array.get"| "Array.getm" | "Array.set"
          | "Array.setm" | "Array.update" 
          | "Array.update_complex" | "PairArray.update" -> 
            let arr_cid = List.hd args |> InterpHelpers.name_from_exp in
            let addr_exp = List.hd (List.tl args) in
            hashtbl_entry_append result arr_cid addr_exp;
          | _ -> ()
      )
      | _ -> ()
  end
;;


let acc_arrays_addrs_over_vertex v arr_addrs = 
  acc_array_addrs#visit_statement arr_addrs v.stmt; arr_addrs
;;

(* get the unique (array * address arg list) list from the dfg *)
let array_addrs_of_dfg dfg = 
  let arr_addrs =  
    TofinoDfg.Dfg.fold_vertex 
      acc_arrays_addrs_over_vertex 
      dfg 
      (Hashtbl.create 10)
  in
  let arr_addrs = Hashtbl.to_seq arr_addrs |> List.of_seq in
  let unique_arr_addrs = List.map (fun (cid, addrs) -> 
    cid, MiscUtils.unique_list_of_eq equiv_exp addrs)
    arr_addrs
  in
  unique_arr_addrs  
;;

let remove_exact_noop_branches statement = 
  match statement.s with 
  | SMatch(exps, branches) -> 
    let branches = List.filter (fun branch -> not 
        (is_exact_branch branch && is_noop (snd branch))
      ) branches
    in
    {statement with s = SMatch(exps, branches)}
  | _ -> statement
;;

(* for each vertex statement that accesses an array who has multiple addresses: 
    1. check that all the calls are in exact match vertex statements
    2. mark the vertex statement as solitary
    3. remove all negative branches from the vertex statement *)
let normalize_vertex_statement arr_addrs vs = 
  let arrs = TofinoResources.arrays_of_stmt vs.stmt in
  match arrs with 
  | [arr] -> (
    match List.assoc_opt arr arr_addrs with 
    | Some addrs -> 
      if (List.length addrs) > 1 then (
        if (is_exact_match vs.stmt) then (
          {vs with stmt = remove_exact_noop_branches vs.stmt; solitary = true}
        )
        else (
          let err_str = "Array with multiple addresses in non-exact match statement: "^(CorePrinting.stmt_to_string vs.stmt) in
          error err_str;
        )
      )
      else vs
    | None -> vs
  )
  | _ -> vs
;;


let process_dfg dfg = 
  let arr_addrs = array_addrs_of_dfg dfg in
  TofinoDfg.Dfg.map_vertex (normalize_vertex_statement arr_addrs) dfg
;;


