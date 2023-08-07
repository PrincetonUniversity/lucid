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

(* open Core *)
open CoreSyntax
open TofinoCoreNew
open InterpHelpers
open MergeHandlers
exception Error of string
let error s = raise (Error s)

module List = Core.List

(* get the variables that are parameters of the program *)
let var_params ds (vars : Cid.t list) = 
  let params = 
    (main_handler_of_decls ds).hdl_input
    |> localids_of_event_params
  in
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
  let params = 
    (main_handler_of_decls ds).hdl_input
    |> localids_of_event_params
  in
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

(* convert a list of pairs of variables that are overlaid into a 
   list of aliases for each variable *)
type alias_list = (cid * (cid list)) list
let overlaid_pairs_to_alias_list (overlaid_pairs : (cid * cid) list) : alias_list = 
  let add_to_aliases (aliases:(cid * (cid list)) list) (x, y) = 
    (* add y to the aliases of x *)
    let x_aliases = match (Core.List.Assoc.find aliases x ~equal:Cid.equal) with
      | Some(x_aliases) -> y::x_aliases
      | None -> [y]
    in
    (* add x to the aliases of y *)
    let y_aliases = match (Core.List.Assoc.find aliases y ~equal:Cid.equal) with
      | Some(y_aliases) -> x::y_aliases
      | None -> [x]
    in
    (* update the entries for x and y in aliases *)
    let aliases = Core.List.Assoc.add aliases x x_aliases ~equal:Cid.equal in
    let aliases = Core.List.Assoc.add aliases y y_aliases ~equal:Cid.equal in
    aliases
  in
  let res = Core.List.fold overlaid_pairs
    ~init:[]
    ~f:add_to_aliases
  in
  res
;;
(* build the conflict graph for the statement *)
let rec build_conflict_graph (alias_list :alias_list) (declared_before : cid list) (used_after : cid list) statement : (cid * cid) list =
  let get_aliases cid = match (Core.List.Assoc.find alias_list cid ~equal:Cid.equal) with
    | Some(aliases) -> aliases |> CL.filter (fun alias -> not (Cid.equal cid alias))
    | None -> []
  in
  (* first, find the new edges without considering any aliases *)
  let base_conflicts = match statement.s with
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
    let edges_from_s1 = build_conflict_graph alias_list declared_before used_after s1 in
    let edges_from_s2 = build_conflict_graph alias_list declared_before used_after s2 in
    edges_from_s1@edges_from_s2
  )
  | SMatch(_, bs) -> (
    CL.map 
      (fun (_, stmt) -> build_conflict_graph alias_list declared_before used_after stmt) 
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
    let s1_edges = build_conflict_graph alias_list (declared_before) (used_after@used_in_s2) s1 in
    let s2_edges = build_conflict_graph alias_list (declared_before@declared_in_s1) used_after s2 in
    s1_edges@s2_edges
  )
  (* no other statements add conflicts. Just update the declared before / used after sets *)
  | _ -> []
  in
  (* now, add alias edges *)
  (* for every edge (x, y) in base_conflicts:
      for every alias x' of x:
        for every alias y' of y:
          add (x', y') to conflicts *)
  let alias_conflicts =
    List.concat_map base_conflicts
      ~f:(fun (x, y) ->
        List.concat_map (get_aliases x)
          ~f:(fun x' ->
            List.map (get_aliases y)
              ~f:(fun y' -> (x', y'))
          )
      )      
  in
  base_conflicts@alias_conflicts
;;

(* construct the conflict graph *)
let conflict_graph alias_list params_by_event statement : (cid * cid) list =
  let get_aliases cid = match (List.Assoc.find alias_list cid ~equal:Cid.equal) with
    | Some(aliases) -> aliases |> CL.filter (fun alias -> not (Cid.equal cid alias))
    | None -> []
  in
  (* build the base conflict graph, which includes edges between all parameters in the same event (and their aliases) *)
  let param_conflicts = List.fold_left params_by_event
    ~init:[]
    ~f:(fun base_conflicts params -> 
        let param_pairs = MiscUtils.get_unique_unordered_pairs params params
         |> List.filter ~f:(fun (x, y) -> not (Cid.equal x y))
        in
        base_conflicts
        @param_pairs)
  in
  let param_alias_conflicts =
    List.concat_map param_conflicts
      ~f:(fun (x, y) ->
        List.concat_map (get_aliases x)
          ~f:(fun x' ->
            List.map (get_aliases y)
              ~f:(fun y' -> (x', y'))
          )
      )      
  in  
  let declared_before = Caml.List.flatten params_by_event in
  let used_after = read_vars statement in
  let var_conflicts = build_conflict_graph alias_list declared_before used_after statement in
  param_conflicts@param_alias_conflicts@var_conflicts

;;



let cids_to_string cids = CorePrinting.comma_sep CorePrinting.cid_to_string cids ;;

(* check if x and y conflict in the decls ds *)
let pairwise_conflict ds x y overlaid_pairs =
  let res = match (Cid.equal x y) with 
    | true -> error "[pairwise_conflict] x and y are the same -- should have been filtered out before"
    | false -> (
    (* print_endline ("[pairwise_conflict] checking for conflict between "^(cids_to_string [x; y])); *)
    (* build a conflict graph for main, check if (x, y) or (y, x) are there. *)
    (* let main_param_ids = (main ds).hdl_params |> CL.split |> snd |> CL.flatten  |> CL.split |> fst in  *)
    let main_param_cids = (main_handler_of_decls ds).hdl_input 
      |> grouped_localids_of_event_params 
    in
    (* print_endline ("[pairwise_conflict] main_param_cids: "^(cids_to_string main_param_cids)); *)
    let main_stmt = main_of_decls ds in
    let conflict_pairs = conflict_graph (overlaid_pairs_to_alias_list overlaid_pairs) main_param_cids main_stmt |> MiscUtils.unique_list_of in    
    let res = (MiscUtils.contains conflict_pairs (x, y)) || (MiscUtils.contains conflict_pairs (y, x)) in
    (* print_endline ("[pairwise_conflict] conflict for: "^(cids_to_string [x; y])^" = "^ (string_of_bool res)); *)
    res
    )
  in
  (* print_endline@@"[pairwise_conflict] ("^(Id.to_string x)^" , "^(Id.to_string y)^" ) : "^(string_of_bool res); *)
  res  

;;


(* are there any pairwise conflicts in vars? *)
let string_of_ids ids = Caml.String.concat ", " (CL.map Id.to_string ids) ;; 

(* do any of the pairs conflict? *)
let any_pairs_conflict ds vars overlay_pairs = 
  let var_pairs = MiscUtils.get_unique_unordered_pairs vars vars
    |> List.filter ~f:(fun (x, y) -> not (Cid.equal x y))
  in
  let conflict_exists =
    List.exists var_pairs (fun (x, y) -> pairwise_conflict ds x y overlay_pairs) 
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
let merge_vars_into_param ds (vars: cid list) argty param_var_cid = 
  let vars = MiscUtils.list_remove vars param_var_cid in 
  let tmp_e = var_sp param_var_cid argty Span.default in 
  let ds = 
    let merge_var ds var_cid = 
      (* replace every expression using var with the new var *)
      let ds = replace_evar ds var_cid tmp_e in 
      (* replace every declaration and assignment to this var with an assignment to new var *)
      let ds = replace_slocal ds ( var_cid) param_var_cid in
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
  match (CL.length (unique_list_of_eq Cid.equal args)) with 
    (* there are 0 or 1 unique vars -- that is fine *)
    | 0 | 1 -> tds
    (* >= 2 vars -- we need to either merge the vars or make an input tmp *)
    | _ -> (
      (* case: there is 1 parameter variable and no conflicts -- merge *)
      (* case: more than 1 parameter variable -- make tmp *)
      (* case: conflicts -- make tmp *)
      if (any_pairs_conflict tds args [])
      then (create_memop_input_var tds arr_id nth argty)
      else (
        match (var_params tds args) with
        (* case - no conflicts, 0 params - merge *)
        | [] -> merge_vars tds args argty
        (* 1 param -- merge into that param *)
        | [param_var] -> merge_vars_into_param tds args argty param_var
        (* multiple params -- make a new var. But note, this var 
           *)
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
    match (CL.length (unique_list_of_eq Cid.equal idx_vars)) with 
    | 0 | 1 -> tds
    | _ -> (
      if ((any_pairs_conflict tds idx_vars []) || (some_var_is_param tds idx_vars))
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
      (* [8/23 jsonch] no longer need to merge index variables as array ops can use separate tables. *)
      tds 
      (* update_idx_vars tds arr_id *)
    in
    CL.fold_left update_by_array tds arrs
  in
  updated_tds
;;

let process_core core_prog = 
  List.map core_prog
    (fun component -> 
      {component with comp_decls = process component.comp_decls})
;;


(* 8/23 partial refactor -- only the functions used in the below code 
   should be kept *)


let merge_nth_arg (overlaid_pairs: ((cid * cid) * ty) list) (tds:tdecl list) arr_id nth args argty : ((cid * cid) * ty) list * tdecl list = 
  match (CL.length (unique_list_of_eq Cid.equal args)) with 
    (* there are 0 or 1 unique vars -- no need to do anything *)
    | 0 | 1 -> overlaid_pairs, tds
    (* >= 2 vars -- we need to get it down to 1 var, by either overlaying the variables or creating an intermediate *)
    | _ -> (
      (* print_endline("[merge_nth_arg] merging Array method args: "^(CorePrinting.comma_sep CorePrinting.cid_to_string args)); *)
      (* if any of the variables conflict, we have to create an intermediate *)
      if (any_pairs_conflict tds args (List.map ~f:fst overlaid_pairs))
      then (overlaid_pairs, create_memop_input_var tds arr_id nth argty)
      else (
        (* print_endline("[merge_nth_arg] no conflicts (including intra-event params) -- safe to overlay"); *)
        let overlaid_pairs' = MiscUtils.get_unique_unordered_pairs args args
          |> List.filter ~f:(fun (x, y) -> not (Cid.equal x y))
        in
        let overlaid_pairs' = List.map ~f:(fun (x, y) -> ((x, y), argty)) overlaid_pairs' in
        (overlaid_pairs@overlaid_pairs'), tds
        (* add overlaid pairs for all the args *)
      )
    )
;;

(* merge all variables that appear as nth argument to a memop in 
   an array method on arr *)
let rec process_arr overlaid_pairs arr tds nths =
  match nths with
  | [] -> overlaid_pairs, tds
  | nth::nths -> (
    match nth_memop_var_args_of_array_calls tds arr nth with
    | Some(args, argty) -> 
      (* concat for this index and go on *)
      let overlaid_pairs, tds' = merge_nth_arg overlaid_pairs tds arr nth args argty in
      process_arr overlaid_pairs arr tds' nths
    | None -> overlaid_pairs, tds
      (* no more args, done *)
  )
;;

let rec process_arrs overlaid_pairs arrs tds = 
  match arrs with
  | [] -> overlaid_pairs, tds
  | arr::arrs -> 
    let overlaid_pairs, tds' = process_arr overlaid_pairs arr tds [0;1;2] in
    process_arrs overlaid_pairs arrs tds'
;;

let cidtup_to_string (a, b) = 
  "("^(CorePrinting.cid_to_string a)^" , "^(CorePrinting.cid_to_string b)^")"
;;

let cidtups_to_string cidtups =
  CorePrinting.comma_sep cidtup_to_string cidtups
;;

(* compute a minimal set of overlay pairs. 
1. construct a graph
2. find all the cliques
3. get a spanning tree from each clique 


(ingress_input.bar.bar_y , ingress_input.baz.baz_z),
(ingress_input.foo.foo_x , ingress_input.baz.baz_z),
(ingress_input.foo.foo_x , ingress_input.bar.bar_y) *)

(* generic undirected graph *)
module GenericGraph (T : sig type t end) : sig
  include Graph.Sig.P with 
    type V.t = T.t
end = struct
  include Graph.Persistent.Graph.Concrete(struct
    type t = T.t
    let compare = Pervasives.compare
    let hash = Hashtbl.hash
    let equal = (=)
  end)
end

(* cid <-> cid graph *)
module G = GenericGraph(Cid)
let check_path g a b = 
  (* print_endline ("checking for path from "^(cidtup_to_string (a, b))); *)
  let module GPath = Graph.Path.Check(G) in
  if G.mem_vertex g a && G.mem_vertex g b then
    (GPath.check_path (GPath.create g) a b)
  else
    false
;;
let find_connected_components g =
  let module GConn = Graph.Components.Make(G) in
  GConn.scc_list g
;;
let rec list_to_pairs xs = 
  match xs with
  | [] | _::[] -> []
  | x::y::xs -> (x, y)::(list_to_pairs (y::xs))
;;


(* get a minimal set of overlay pairs, that is, n-1 overlay commands for n variables  *)
let minimal_overlay_pairs (overlay_pairs: (Cid.t * Cid.t) list) = 
  (* construct a graph with no un-necessary edges *)
  let rec mk_graph edges = 
    match edges with
    | [] -> G.empty
    | (a, b)::edges -> 
      (* build the rest of the graph *)
      let g = mk_graph edges in
      (* add this edge to the graph if a path doesn't exist in the 
         rest of the graph *)
      if (check_path g a b) then g else (G.add_edge g a b)
  in
  let g = mk_graph overlay_pairs in
  (* find all the connected components *)
  let connected_components = find_connected_components g in
  (* make a pair list for each CC and concat together *)
  List.concat_map connected_components ~f:list_to_pairs
 ;;

let alias_groups (overlay_pairs : ((cid * cid) * ty) list) =  
  let rec mk_graph edges = 
    match edges with
    | [] -> G.empty
    | (a, b)::edges -> 
      (* build the rest of the graph *)
      let g = mk_graph edges in
      (* add this edge to the graph if a path doesn't exist in the 
         rest of the graph *)
      if (check_path g a b) then g else (G.add_edge g a b)
  in
  let cidpairs = List.map ~f:fst overlay_pairs in
  let g = mk_graph cidpairs in
  let ty_assoc = List.fold overlay_pairs ~init:[] ~f:(fun acc ((a, b), ty) -> 
    (a, ty)::(b, ty)::acc
  ) 
  in
  (* each connected component has a type.. *)
  (* find all the connected components *)
  let connected_components = find_connected_components g in
  (* get the type of each connected component, based on first var *)
  let ty_of_cc cc = 
    let cid = List.hd_exn cc in
    List.Assoc.find_exn ty_assoc cid ~equal:Cid.equal
  in
  let cc_tys = List.map ~f:ty_of_cc connected_components in
  (* make a pair list for each CC and concat together *)
  List.zip_exn connected_components cc_tys
;;

(* now, we have to implement the overlaying.  *)

(* is cid a parameter of the input event? *)

type vloc = | Param | Prealloc | Local
let is_param input_event cid = 
  List.exists (grouped_localids_of_event_params input_event |> Caml.List.flatten) ~f:(fun x -> Cid.equal x cid)
;;
let is_prealloc main cid =
  List.exists main.hdl_preallocated_vars ~f:(fun (x, _) -> (Cid.equal (Cid.id x) cid))
;;
let cid_to_loc main cid =
  if is_param (main.hdl_input) cid then Param
  else if (is_prealloc main cid) then Prealloc
  else Local
;;


let replace_cid tds cids cid' = 
  (* replace any cid in cids with cid' everywhere they appear (including the parser) *)
  let is_tgt_cid cidq = List.exists cids ~f:(fun x -> Cid.equal x cidq) in
  let v = 
    object
      inherit [_] s_map as super
      (* expressions *)
      method! visit_EVar _ cid = 
        if (is_tgt_cid cid) 
        then (EVar(cid'))
        else (EVar(cid))
      (* parser actions *)
      method! visit_PAssign ctx lcid rexp = 
        if (is_tgt_cid lcid) 
        then (PAssign(cid', (super#visit_exp ctx rexp)))
        else (PAssign(lcid, (super#visit_exp ctx rexp)))
      method! visit_PRead _ lcid lty = 
        if (is_tgt_cid lcid) 
        then (PRead(cid', lty))
        else (PRead(lcid, lty))
      method! visit_PPeek _ lcid lty = 
        if (is_tgt_cid lcid) 
        then (PPeek(cid', lty))
        else (PPeek(lcid, lty))
      (* statements *)
      method! visit_SAssign ctx lcid rexp = 
        if (is_tgt_cid lcid) 
        then (SAssign(cid', (super#visit_exp ctx rexp)))
        else (SAssign(lcid, (super#visit_exp ctx rexp)))
      method! visit_SLocal ctx id ty exp =
        (* if this cid is the target, we replace the declaration with an assign *)
        if (is_tgt_cid (Cid.id id)) 
        then (SAssign(cid', (super#visit_exp ctx exp)))
        else (SLocal(id, ty, (super#visit_exp ctx exp)))
    end
  in 
  v#visit_tdecls () tds
;;  


(* to overlay a group of aliases where at least one is a param, 
   we choose a param to be the name of all the vars and rename 
   them all to that master. *)
let overlay_param_aliases tds vars var_locs = 
  (* find the first param *)
  let master_cid = List.find_exn 
    (List.zip_exn vars var_locs) 
    ~f:(fun (_, loc) -> loc = Param) |> fst 
  in
  replace_cid tds vars master_cid
;;
(* to overlay a bunch of locals, we create a new preallocated 
   variable and rename all the vars to that. *)

(* remove a preallocated var from tds *)
let remove_preallocated_var tds cid = 
  let main = main_handler_of_decls tds in
  let new_preallocated_vars = List.filter main.hdl_preallocated_vars ~f:(fun (x, _) -> not (Cid.equal (Cid.id x) cid)) in
  replace_main_handler_of_decls tds {main with hdl_preallocated_vars = new_preallocated_vars}
;;
let overlay_local_aliases tds vars var_ty = 
  let tmp_id = Id.fresh ("aliased_var")  in 
  let master_cid = Cid.create_ids [tmp_id] in
  let master_ty = var_ty in
  (* replace all the variables *)
  let tds = replace_cid tds vars master_cid in
  (*  add the shared local *)
  let _, tds = add_shared_local tds (Cid.to_id master_cid) master_ty in 
  (* if any of the variables were shared locals, remove them *)
  List.fold vars 
    ~init:tds 
    ~f:(fun tds cid -> 
      match cid_to_loc (main_handler_of_decls tds) cid with
      | Local -> tds
      | Param -> tds
      | Prealloc -> remove_preallocated_var tds cid
    )
;;

(* overlay all the variables in vars in the program tds *)
let overlay_aliases tds (vars, varty) = 
  print_endline("[overlay_aliases] overlaying variables");
  print_endline(CorePrinting.comma_sep CorePrinting.cid_to_string vars);
  print_endline("[overlay_aliases] in program:");
  print_endline(TofinoCorePrinting.tdecls_to_string tds);
  let main = main_handler_of_decls tds in
  let var_locs = List.map vars ~f:(cid_to_loc main) in
  (* if any of the variables are parameters, we have to update the parser and 
     also we use one of the existing variables as the "master" *)
  let has_param = List.exists var_locs ~f:(fun x -> x = Param) in
  if (has_param) then
    overlay_param_aliases tds vars var_locs
  (* if there are no params, we create a new shared local and 
     replace all the uses of the variables with the shared local. 
      We also delete the declarations for all the variables. *)
  else
    overlay_local_aliases tds vars varty
;;

let process_component tds = 
  let overlaid_pairs, tds = process_arrs [] (arrs_in_prog tds) tds in
  (* print_endline ("---[overlaid pairs]---");
  print_endline (cidtups_to_string overlaid_pairs);
  print_endline ("----------------------"); *)
  (* let min_pairs = minimal_overlay_pairs overlaid_pairs in *)
  (* print_endline ("---[min_pairs pairs]---");
  print_endline (cidtups_to_string min_pairs);
  print_endline ("----------------------"); *)
  (* attach all the pragmas to the main handler *)
  let alias_grps = alias_groups overlaid_pairs in
  let tds = List.fold alias_grps ~f:overlay_aliases ~init:tds in
  tds
;;

let process_core core_prog = 
  List.map core_prog
    (fun component -> 
      {component with comp_decls = process_component component.comp_decls})
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
          0. merge the index argument of all calls
          1. merge the first input argument of all calls
          2. merge the second input argument of all calls
    Algorithm merge(array, vars):
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