(* ShareMemopInputs

  In the Tofino, each global "array" is attached to 1 sALU. 
  Each sALU only has two input ports and one output port from/to the
  pipeline. 
  In other words, all Array.update operations to array X must have:
    1. the same index variable.
    2. at most the same two input variables. 
    3. (maybe? maybe not?) the same output variable.
  This pass will: 
    1. try to merge all the (index, input) variables of 
       the update statements together. 
    2. if that is not possible, create a new temporary variable 
       for the position of the array call and add statements 
       to load data before the array call. 
  Question:
    - what about output variable? Is that not limited?
*) 

open Core
open CoreSyntax
open TofinoCore
open InterpHelpers

exception Error of string
let error s = raise (Error s)


let var_params ds vars = 
  let rec params_of_prog ds = 
    match ds with
    | [] -> []
    | {td=TDEvent(_, _, params); _}::ds -> 
      (CL.split params |> fst)@(params_of_prog ds)
    | _::ds -> params_of_prog ds
  in
  let params = params_of_prog ds in
  let var_params = 
    let folder var_params var =
      if (MiscUtils.contains params var)
      then var::var_params
      else (var_params)
    in
    CL.fold_left folder [] vars
  in 
  var_params
;;


(* is a variable in the list a parameter of some event? *)
let some_var_is_param ds vars = 
  let rec params_of_prog ds = 
    match ds with
    | [] -> []
    | {td=TDEvent(_, _, params); _}::ds -> 
      (CL.split params |> fst)@(params_of_prog ds)
    | _::ds -> params_of_prog ds
  in
  let params = params_of_prog ds in
  let param_present = 
    let folder acc var =
      acc || MiscUtils.contains params var
    in
    CL.fold_left folder false vars
  in 
  param_present
;;

let ids_to_string ids = CL.map Id.to_string ids |> Caml.String.concat " , "


(* better conflict analysis. 

  x and y conflict if they are alive at the same time in the program. 

  at any point in a program, an alive variable is one that:
    - has been declared in the past
    - is used in the future
*)

(* which variables are used, i.e., read, in the statement? *)
let read_vars statement : cid list =
  let vars = ref [] in 
  let v =
    object
      inherit [_] TofinoCore.s_iter as super

      method! visit_EVar _ var_cid = 
        vars := var_cid::(!vars);
    end
  in
  v#visit_statement () statement;
  !vars
;;
(* which variables are written, i.e., 
   declared or assigned, in the statement? *)
let assigned_vars statement : cid list = 
  let vars = ref [] in 
  let v =
    object
      inherit [_] TofinoCore.s_iter as super

      method! visit_SLocal  _ id _ _ = 
        vars := (Cid.id id)::(!vars);
      method! visit_SAssign  _ id _ = 
        vars := (id)::(!vars);
    end
  in
  v#visit_statement () statement;
  !vars
;;

let intersection l1 l2 =
  CL.filter_map (fun l1e -> match MiscUtils.contains l2 l1e with 
    | true -> Some(l1e)
    | false -> None
  )
  l1
;;

(* build the conflict graph for the statement *)
let rec conflict_graph (declared_before : cid list) (used_after : cid list) statement : (cid * cid) list =
  match statement.s with
  | SLocal(var_id, _, _) -> (
    let var_cid = Cid.id var_id in
    (* if the var is used later, add conflicts with all the live vars *)
    let live_vars = CL.filter (fun dec_id -> MiscUtils.contains used_after dec_id) declared_before in
    if (MiscUtils.contains used_after (var_cid))
      then (CL.map (fun live_var -> (var_cid, live_var)) live_vars)
      (* if its not used later, we don't add any conflicts at this point *)
      else ([])
  )
  | SAssign(var_id, _) -> (
    (* if this is the first assignment, consider it the same as a decl *)
    (* if the var is used later, add conflicts with all the live vars *)
    let live_vars = CL.filter (fun dec_id -> MiscUtils.contains used_after dec_id) declared_before in
    (* already declared -- nothing changes here. *)
    if (MiscUtils.contains declared_before var_id)
    then ([])
    else (
      if (MiscUtils.contains used_after var_id)
        then (CL.map (fun live_var -> (var_id, live_var)) live_vars)
        (* if its not used later, we don't add any conflicts at this point *)
        else ([])  
    )
  )
  | SIf(_, s1, s2) -> (
    (* recurse in s1, s2 -- nothing changes about declared before or used after *)
    let edges_from_s1 = conflict_graph declared_before used_after s1 in
    let edges_from_s2 = conflict_graph declared_before used_after s2 in
    edges_from_s1@edges_from_s2
  )
  | SMatch(_, bs) -> (
    CL.map 
      (fun (_, stmt) -> conflict_graph declared_before used_after stmt) 
      bs
    |> CL.flatten
  )
  | SSeq(s1, s2) -> (
    let declared_in_s1 = assigned_vars s1 in
    let used_in_s2 = read_vars s2 in
    (* recurse in s1 and s2, updating the contexts. 
      s1 needs to know everything used in s2.
      s2 needs to know everything declared in s1.
    *)
    let s1_edges = conflict_graph (declared_before) (used_after@used_in_s2) s1 in
    let s2_edges = conflict_graph (declared_before@declared_in_s1) used_after s2 in
    s1_edges@s2_edges
  )
  (* no other statements add conflicts. Just update the declared before / used after sets *)
  | _ -> []
;;


let pairwise_conflict ds x y =
  let res = match (Cid.equal x y) with 
    | true -> false
    | false -> (
    (* build a conflict graph for main, check if (x, y) or (y, x) are there. *)
    let main_param_ids = (main ds).hdl_params |> CL.split |> snd |> CL.flatten  |> CL.split |> fst in 
    let main_param_cids = CL.map Cid.id main_param_ids in 
    let main_stmt = (main ds).main_body |> CL.hd in
    let conflict_pairs = conflict_graph main_param_cids [] main_stmt |> MiscUtils.unique_list_of in
    (MiscUtils.contains conflict_pairs (x, y)) || (MiscUtils.contains conflict_pairs (y, x))
    )
  in
  (* print_endline@@"[pairwise_conflict] ("^(Id.to_string x)^" , "^(Id.to_string y)^" ) : "^(string_of_bool res); *)
  res  

;;


(* are there any pairwise conflicts in vars? *)
let string_of_ids ids = Caml.String.concat ", " (CL.map Id.to_string ids) ;; 

let any_pairs_conflict ds vars = 
  let var_pairs = MiscUtils.get_unique_unordered_pairs vars vars in
  let conflict_exists = 
    let test_pair prior_conflict (x, y) =
      let res = prior_conflict || (pairwise_conflict ds x y) in
      res
    in
    CL.fold_left test_pair false var_pairs
  in
  conflict_exists
;;


let constructor_cids = 
  [(Arrays.constructors |> CL.hd |> fst)
  ;(PairArrays.constructors |> CL.hd |> fst)]
;;

let accessor_cids = 
  let defs = Arrays.defs@PairArrays.defs in 
  CL.map (fun (gf:InterpState.State.global_fun) -> gf.cid) defs
;;

let string_of_fcncid cid = 
  Caml.String.concat "." @@ Cid.names cid
;;


let cid_in_evar (ex : exp) : Cid.t option = 
  match ex.e with
  | EVar n -> Some n
  | _ -> None
;;

(* get the ids of all the arrays in the program *)
let rec arrs_in_prog (tds:tdecls) = 
  match tds with 
  | [] -> []
  | {td=TDGlobal(id, _, {e=ECall(constr_cid, _); _}); _}::ds -> 
    if (MiscUtils.contains constructor_cids constr_cid)
    then (id::(arrs_in_prog ds))
    else (arrs_in_prog ds)
  | _::ds -> arrs_in_prog ds
;;

(* is call_fcid(call_args) an array method accessing arr_id? *)
let call_accesses_arr arr_id call_fcid call_args =
  match (MiscUtils.contains accessor_cids call_fcid) with 
  | true -> (
    let arr_arg = CL.hd call_args |> cid_in_evar in 
    match arr_arg with 
    | Some arr_arg -> Cid.equals arr_arg (Cid.id arr_id)
    | None -> false
  )
  | false -> false
;;


(* replace all evar(cid) with new_e *)
let replace_evar ds cid new_exp = 
  let v = 
    object
      inherit [_] s_map as super
      method! visit_exp ctx var_exp = 
        let var_exp = super#visit_exp ctx var_exp in 
        match var_exp.e with
        | EVar(var_cid) -> 
          if (Cid.equal var_cid cid) 
          then (new_exp)
          else (var_exp)
        | _ -> var_exp 
    end
  in 
  v#visit_tdecls () ds
;;  
(* replace all local assignments to 
   old_id with assignments to new_id *)
let replace_slocal ds (old_id : cid) new_id = 
  let v = 
    object
      inherit [_] s_map as super
      method! visit_SLocal _ id ty exp = 
        if (Cid.equal (Cid.id id) old_id)
        then (SAssign(new_id, exp))
        else (SLocal(id, ty, exp))
      method! visit_SAssign _ id exp =
        if (Cid.equal id old_id)
        then (SAssign(new_id, exp))
        else (SAssign(id, exp))
      end
  in 
  v#visit_tdecls () ds
;;

(* merge vars together into the same variable *) 
let merge_vars ds (vars: cid list) argty = 
  (* 1. create the temp *)
  (* 2. for each var: *)
  (*    - replace SLocal(var) with SAssign(tmp) *)
  (*    - replace var_cid with tmp_cid *)
  let tmp_id = Id.fresh ("merged_var") in 
  let concat_idstr = Caml.String.concat "_" (CL.map (fun cid -> Cid.to_id cid |> Id.name) vars) in 
  let tmp_id = Id.create ((fst tmp_id)^"_"^concat_idstr^"_"^(snd tmp_id|> string_of_int)) in 
  let tmp_ty = argty in 
  let tmp_e, ds = add_shared_local ds tmp_id tmp_ty in     
  (* let tmp_e, tmp_d = create_intermediate tmp_id tmp_ty in  *)
  (* let ds = tmp_d::ds in  *)
  let ds = 
    let merge_var ds var_cid = 
      let ds = replace_evar ds var_cid tmp_e in 
      let ds = replace_slocal ds ( var_cid) (Cid.id tmp_id) in
      ds
    in 
    CL.fold_left merge_var ds vars
  in 
  ds
;;

(* merge vars together into a single parameter variable.*)
let merge_vars_into_param ds (vars: cid list) argty param_var_id = 
  let vars = MiscUtils.list_remove vars (Cid.id param_var_id) in 
  let tmp_id = param_var_id in 
  let tmp_ty = argty in 
  let tmp_e = var_sp (Cid.id tmp_id) tmp_ty Span.default in 
  let ds = 
    let merge_var ds var_cid = 
      (* replace every expression using var with the new var *)
      let ds = replace_evar ds var_cid tmp_e in 
      (* replace every declaration and assignment to this var with an assignment to new var *)
      let ds = replace_slocal ds ( var_cid) (Cid.id tmp_id) in
      ds
    in 
    CL.fold_left merge_var ds vars
  in 
  ds
;;


let cons_uniq_eq eq xs x = if List.mem xs x ~equal:eq then xs else x :: xs
let unique_list_of_eq eq xs = List.rev (Caml.List.fold_left (cons_uniq_eq eq) [] xs)



(* new algorithm: merge the nth _variable argument_ of each array call. *)




(* index is not a memop arg! *)
let index_args_of_array_call exp =
  match exp.e with 
  | ECall(fcnid, args) -> (
    match ((string_of_fcncid fcnid), args) with
      | "Array.get", _::idx::_ 
      | "Array.set", _::idx::_ 
      | "Array.getm", _::idx::_ 
      | "Array.setm", _::idx::_ 
      | "Array.update", _::idx::_ 
      | "Array.update_complex", _::idx::_ 
      | "PairArray.update", _::idx::_ -> idx
      | _ -> error "unexpected function call"
    )
  | _ -> error "not an ecall"
;;

let idx_var_arg_of_array_call exp = 
  match (index_args_of_array_call exp).e with
  | EVar cid -> Some(cid)
  | _ -> None

let idx_var_ty_of_array_call exp =
  (index_args_of_array_call exp).ety
;;

(* get all the index vars of array calls for arrid *)
let idx_var_args_of_array_calls ds arrid =
  let v = 
    object
      inherit [_] s_iter as super
      val mutable argty = None
      method argty = argty
      val mutable posargs = []
      method posargs = posargs
      method! visit_exp ctx exp = 
        super#visit_exp ctx exp;
        match exp.e with 
        | ECall(fcnid, args) -> (
          if (call_accesses_arr arrid fcnid args)
          then (
            match idx_var_arg_of_array_call exp with 
            | Some arg -> (
              argty <- Some(idx_var_ty_of_array_call exp);
              posargs <- arg::posargs; 
            )
            | None -> ()
          )
        )
        | _ -> ()
    end
  in
  v#visit_tdecls () ds;
  match (v#argty) with 
  | Some(argty) -> Some(v#posargs, argty)
  | None -> None
;;

(* get the arguments of the array call that are passed to the memop *)
let memop_args_of_array_call exp =
  match exp.e with 
  | ECall(fcnid, args) -> (
    match ((string_of_fcncid fcnid), args) with
    | "Array.get", _ -> []
    | "Array.getm", [_; _; _; arg1] -> [arg1]
    | "Array.set", [_; _; arg1] -> [arg1]
    | "Array.setm", [_; _; _; arg1] -> [arg1]
    | "Array.update", [_; _; _; arg1; _; arg2] -> 
      [arg1; arg2]
    | "Array.update_complex", [_; _; _; arg1; arg2; _] -> 
      [arg1; arg2]
    | "PairArray.update", [_; _; _; arg1; arg2; _] -> 
      [arg1; arg2]
    | _ -> error "[posarg_of_arrcall] either the function is not an array accessor, or it has the wrong arguments"
  )
  | _ -> error "[memop_args_of_array_call] not an array call."  
;;

let memop_var_args_of_array_call exp = 
  let is_var arg = 
    match arg.e with 
      | EVar(cid) -> Some(cid)
      | _ -> None
  in
  CL.filter_map is_var (memop_args_of_array_call exp)
;;

(* the nth unique variable in the memop args *)
let nth_memop_var_arg exp n = 
  let unique_var_args = memop_var_args_of_array_call exp |> MiscUtils.unique_list_of in
  CL.nth_opt unique_var_args n
;;

(* get a list of all the nth memop var arguments 
   in array method calls that access arrid *)
let nth_memop_var_args_of_array_calls ds arrid nth =
  let v = 
    object
      inherit [_] s_iter as super
      val mutable argty = None
      method argty = argty
      val mutable posargs = []
      method posargs = posargs
      method! visit_exp ctx exp = 
        super#visit_exp ctx exp;
        match exp.e with 
        | ECall(fcnid, args) -> (
          if (call_accesses_arr arrid fcnid args)
          then (
            match nth_memop_var_arg exp nth with 
            | Some arg -> (
              let ty = (memop_args_of_array_call exp |> CL.hd).ety in 
              argty <- Some(ty);
              posargs <- arg::posargs; 
            )
            | None -> ()
          )
        )
        | _ -> ()
    end
  in
  v#visit_tdecls () ds;
  match (v#argty) with 
  | Some(argty) -> Some(v#posargs, argty)
  | None -> None
;;

(* create a temporary variable. In every 
   call to an array method that accesses 
   arr_id, and has an nth var input:
    1. prepend: input_nth = inputs[nth]
    2. replace (inputs[nth] --> input_nth) in call

 *)


let replace_cid_in_exp cid new_cid exp =
  let v = 
    object
      inherit [_] s_map as super
      method! visit_exp ctx var_exp = 
        let var_exp = super#visit_exp ctx var_exp in 
        match var_exp.e with
        | EVar(var_cid) -> 
          if (Cid.equal var_cid cid) 
          then ({exp with e=EVar(new_cid);})
          else (var_exp)
        | _ -> var_exp 
    end
  in 
  v#visit_exp () exp
;;

(* if exp is an array method on arr_id, 
   replace the nth memop arg var with tmp_id *)
let replace_var_arg_with_tmp arr_id nth tmp_id exp =
  let tmp_cid = Cid.id tmp_id in
  match exp.e with
  | ECall(fid, args) -> (
    if (call_accesses_arr arr_id fid args)
    then (
      let unique_var_args = memop_var_args_of_array_call exp |> MiscUtils.unique_list_of in
      (* if there is an nth unique variable argument *)
      match (CL.nth_opt unique_var_args nth) with
      | None -> None
      | Some(tgt_cid) -> (
      let arg_ty = (CL.hd (memop_args_of_array_call exp)).ety in 
      (* replace all instances of that variable with tmp_cid *)
        let args = CL.map (replace_cid_in_exp tgt_cid tmp_cid) args in
        let set_stmt = sassign tmp_cid (var_sp tgt_cid arg_ty Span.default) in 
        Some(set_stmt, {exp with e=ECall(fid, args)})
      )
    )
    else (None)
)
  | _ -> None
;;

(* replace the index argument with evar(tmp_id) in exp *)
let replace_idx_var_with_tmp arr_id tmp_id exp =
  let tmp_cid = Cid.id tmp_id in
  match exp.e with
  | ECall(fid, args) -> (
    if (call_accesses_arr arr_id fid args)
    then (
      let eidx = index_args_of_array_call exp in 
      (* if the index is a variable, replace it *)
      match eidx.e with 
      | EVar(tgt_cid) -> (
        let args = CL.map (replace_cid_in_exp tgt_cid tmp_cid) args in
        let set_stmt = sassign tmp_cid eidx in 
        Some(set_stmt, {exp with e=ECall(fid, args)})
      )
      | _ -> None
    )
    else (None)
)
  | _ -> None

let create_index_var tds arr_id argty =
  (* make the variable, add a declaration of it *)
  let tmp_id = Id.fresh ((Id.name arr_id)^"_idx")  in 
  let tmp_id = Id.create ((fst tmp_id)^"_"^(string_of_int (snd tmp_id))) in 
  let tmp_ty = argty in 
  let _, tds = add_shared_local tds tmp_id tmp_ty in 
  (* update all array call statements for arr_id to use it *)
  let v = 
    object
      inherit [_] s_map as super
      method! visit_statement ctx statement = 
        let statement = super#visit_statement ctx statement in 
        match statement.s with 
        | SAssign(id, exp) -> (
          match (replace_idx_var_with_tmp arr_id tmp_id exp) with 
          | None -> statement
          | Some(tmp_assign, new_ecall) -> (
            sseq
              tmp_assign
              {statement with s=SAssign(id, new_ecall)}
          )
        )
        | SLocal(id, ty, exp) -> (
          match (replace_idx_var_with_tmp arr_id tmp_id exp) with 
          | None -> statement
          | Some(tmp_assign, new_ecall) -> (
            sseq
              tmp_assign
              {statement with s=SLocal(id, ty, new_ecall)}
          )
        )
        | SUnit(exp) -> (
          match (replace_idx_var_with_tmp arr_id tmp_id exp) with 
          | None -> statement
          | Some(tmp_assign, new_ecall) -> (
            sseq
              tmp_assign
              {statement with s=SUnit(new_ecall)}
          )
        )
        | _ -> statement
    end
  in
  let tds = v#visit_tdecls () tds in
  tds

(* create a tmp variable for array arr_id accesses, 
   replace the nth unique var argument 
   of every call with tmp *)
let create_memop_input_var tds arr_id nth argty = 
  (* make the variable, add a declaration of it *)
  let tmp_id = Id.fresh ((Id.name arr_id)^"_input")  in 
  let tmp_id = Id.create ((fst tmp_id)^"_"^(string_of_int (snd tmp_id))) in 
  let tmp_ty = argty in 
  let _, tds = add_shared_local tds tmp_id tmp_ty in 
  (* update all array call statements for arr_id to use it *)
  let v = 
    object
      inherit [_] s_map as super
      method! visit_statement ctx statement = 
        let statement = super#visit_statement ctx statement in 
        match statement.s with 
        | SAssign(id, exp) -> (
          match (replace_var_arg_with_tmp arr_id nth tmp_id exp) with 
          | None -> statement
          | Some(tmp_assign, new_ecall) -> (
            sseq
              tmp_assign
              {statement with s=SAssign(id, new_ecall)}
          )
        )
        | SLocal(id, ty, exp) -> (
          match (replace_var_arg_with_tmp arr_id nth tmp_id exp) with 
          | None -> statement
          | Some(tmp_assign, new_ecall) -> (
            sseq
              tmp_assign
              {statement with s=SLocal(id, ty, new_ecall)}
          )
        )
        | SUnit(exp) -> (
          match (replace_var_arg_with_tmp arr_id nth tmp_id exp) with 
          | None -> statement
          | Some(tmp_assign, new_ecall) -> (
            sseq
              tmp_assign
              {statement with s=SUnit(new_ecall)}
          )
        )
        | _ -> statement
    end
  in
  let tds = v#visit_tdecls () tds in
  tds
;;



let setup_nth_var_arg tds arr_id nth args argty = 
  let arg_ids = CL.map Cid.to_id args in 
  match (CL.length (unique_list_of_eq Cid.equal args)) with 
    (* there are 0 or 1 unique vars -- that is fine *)
    | 0 | 1 -> tds
    (* >= 2 vars -- we need to either merge the vars or make an input tmp *)
    | _ -> (
      (* case: there is 1 parameter variable and no conflicts -- merge *)
      (* case: more than 1 parameter variable -- make tmp *)
      (* case: conflicts -- make tmp *)
      if (any_pairs_conflict tds args)
      then (create_memop_input_var tds arr_id nth argty)
      else (
        match (var_params tds arg_ids) with
        (* case - no conflicts, 0 params - merge *)
        | [] -> merge_vars tds args argty
        (* 1 param -- merge into that param *)
        | [param_var] -> merge_vars_into_param tds args argty param_var
        (* multiple params -- make a new var *)
        | _ -> create_memop_input_var tds arr_id nth argty
      )
(* 

      if ((any_pairs_conflict tds arg_ids) || (some_var_is_param tds arg_ids))
      if there are any conflicts between the variables, or 
         one of the arguments is an event parameter, 
         then we must make an input arg var
      then (create_memop_input_var tds arr_id nth argty)
      (* no conflicts --> we can just merge the variables *)
      else (merge_vars tds args argty) *)
    )
;;

let update_idx_vars tds arrid =
  match (idx_var_args_of_array_calls tds arrid) with 
  | Some(idx_vars, idxvar_ty) -> (
    let idx_ids = CL.map Cid.to_id idx_vars in 
    match (CL.length (unique_list_of_eq Cid.equal idx_vars)) with 
    | 0 | 1 -> tds
    | _ -> (
      if ((any_pairs_conflict tds idx_vars) || (some_var_is_param tds idx_ids))
      (* if there are any conflicts between the variables, or 
         one of the arguments is an event parameter, 
         then we must make an input arg var *)
      then (create_index_var tds arrid idxvar_ty)
      (* no conflicts --> we can just merge the variables *)
      else (merge_vars tds idx_vars idxvar_ty)
    )
  )
  | None -> tds
;;

let rec process tds = 
  let arrs = arrs_in_prog tds in 
  let updated_tds = 
    (* for each array *)
    let update_by_array tds arr_id = 
      let update_by_nth tds nth =
        match (nth_memop_var_args_of_array_calls tds arr_id nth) with
        | Some(args, argty) -> setup_nth_var_arg tds arr_id nth args argty
        | None -> tds
      in
      (* update each possible argument variable index *)
      let tds = CL.fold_left update_by_nth tds [0; 1; 2] in
      (* now update for index arg *)
      update_idx_vars tds arr_id
    in
    CL.fold_left update_by_array tds arrs
  in
  updated_tds
;;




(* 

    a better algorithm: 
      - count the number of unique variable arguments to any call of the array. 
      - if it is over 2:
        - if there is a way to merge into 2 variables, do that.
      - else, create input vars for the array and 
        rewrite every call to the array, so that the array uses 
        at most 2 arguments. *)






  (* 
    Algorithm: 
      for each array a:
        for each Array method call (m) with a as the first arg:
          0. unify the index argument of all calls
          1. unify the first input argument of all calls
          2. unify the second input argument of all calls
    Algorithm unify(array, vars):
      - if any var is a parameter: add_intermediate(array, vars)
      - elif there are any pairwise conflicts between vars: add_intermediate(array, vars)
      - else: replace_vars_with_intermediate(vars)
    add_intermediate(array, vars):
      create a new intermediate v1
      for each Array method to array
        for each var in vars:
          if var an arg:
            before the array method call, add v1 = var;
            replace arg with v1
    replace_vars_with_intermediate(vars):
      create new intermediate v1
      for each var v: 
        replace local(v) with assign(v1)
      replace all uses of v with uses of v1
    
    pairwise_conflict(v1, v2):
      step through the program, record all variables declared
        - when you see v1 declared, check if v2 is declared. If it is, return conflict
        - when you see v2 declared, check if v1 is declared. If it is, return conflict
        - otherwise, return no conflict *)

;;