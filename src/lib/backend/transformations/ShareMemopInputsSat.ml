(* ShareMemopInputsSat

  Encode SALU / memop register allocation as as a sat problem 
  and use z3 to try and find an overlaying of variables, if one exists. 
  
  If this fails, we fall back to creating intermediates for some of the 
  SALU register and adding copy instructions. 

  Algorithm overview: 

  1. gather a map from array id -> the list of variables used in calls that operate on that array.
      (see function: array_user_vars)
  2. build a conflict graph over variables used in any array. 
      (see functions: conflict_graph_of_handler and conflict_graph_of_parser)
      - Two variables conflict if they are alive at the same time. 
      - All the parameters within a base event conflict with each other (that is, a user defined event)
      - parameters from other events may conflict with each other due to the parser optimization of 
        reading the next header field to all of the parser variables that use it. 
        So, in this example: 
          peek<32> foo.x;
          peek<32> bar.y;
          advance<32>;
          if (...) {generate(foo);}
          else {generate(bar);}
        foo.x and bar.y conflict because they are are alive at the same time.
  2.1 turn the conflict graph into a map from variable -> all the variables it conflicts with
  3. given the array_user map var_conflict map, generate a list of constraints, 
     which are boolean expressions. 
     (see function: generate_constraints and type: term)
     In the boolean expressions, boolean variables represent two things: 
          1. An "Alias(x, y)" variable, when set to true, means that x is an alias of y.
          2. An "Alloc(x, (y, z))" variable, when set to true, means that variable x 
             uses input register z when used in operations to array z.
             The relationship between these variables is: 
               Alloc(x, (y, z)) && Alloc(x', (y, z)) ==> Alias(x, x')
      The goal of the constraints is to find a set of aliases such that all the users 
      of each array can be mapped into the arrays limited number of input registers (2 for tofino 1).
      There are 3 kinds of constraints:
        1. each variable that uses an array gets mapped to exactly 1 of the array's registers
        2. when two variables get mapped to the same register slots, they are aliases of each other
        3. two variables may not be mapped to the same slot if any of their aliases conflict
  4. next, we translate the constraints to z3, run the solver to get an assignment, and 
        extract the Alias(x, y) assignments. This part is easy. (see function: run_solver)
  5. given the list of alias pairs that we get from (4), we generate a list of alias groups, 
     where each group is a list of variables that are all aliases of each other. 
     We use a graph to do this: make a graph with alias pairs as edges and get the connected 
     components. The nodes in each connected component are an alias group. There might be 
     an easier way to do this. (see function: alias_groups)
  6. finally, for each alias group, we pick a "master variable" and replace all occurrences of 
     variables in the alias group with the "master variable". (see function: overlay_aliases)
      - after a variable is replaced, if it is a local or preallocated variable, we remove 
        its declaration.
  7. That's it! If the solver failed to find a solution, we fall back to the heuristic in 
     the ShareMemopInputBaseline submodule, which creates intermediate variables and adds copy operations. 
     Unfortunately, that one is not optimal. It is basically a copy/paste of the original ShareMemopInputs, 
     which allocates variables 1 register at a time, (e.g., it allocates all the variables 
     in the nth parameter of each array to one intermediate variable if they cannot be overlaid.)
*) 

open CoreSyntax
open TofinoCoreNew
open InterpHelpers
open MergeHandlers
open Z3
open Solver
module Z3Bool = Boolean
module Z3Int = Arithmetic.Integer

exception Error of string
let error s = raise (Error s)


type var = {vcid : cid; vuid : int; vty : ty;}
(* register is an array id and a slot number *)
type register = Register of var * int

type bool_const = 
| Alloc of var * register
| Alias of var * var


type term = 
  | BoolConst of bool_const
  | Or of term list
  | And of term list
  | Not of term
  | Implies of term * term

let var_to_string var = CorePrinting.cid_to_string var.vcid
  
let register_to_string reg = 
  match reg with
  | Register(var, slot) ->
    Printf.sprintf "%s.reg[%i]" (var_to_string var) slot
;;

let boolconst_to_string bc = 
  match bc with
  (* boolean variables *)
  | Alloc(var, register) -> Printf.sprintf "Alloc(%s, %s)" (var_to_string var) (register_to_string register)
  | Alias(var1, var2) -> Printf.sprintf "Alias(%s, %s)" (var_to_string var1) (var_to_string var2)

let rec term_to_string exp = match exp with
  | BoolConst(b) -> boolconst_to_string b
  | Or(exps) -> Printf.sprintf "(%s)" (String.concat " or " (List.map term_to_string exps))
  | And(exps) -> Printf.sprintf "(%s)" (String.concat " and " (List.map term_to_string exps))
  | Not(exp) -> Printf.sprintf "(~%s)" (term_to_string exp)
  | Implies(exp1, exp2) -> Printf.sprintf "(%s => %s)" (term_to_string exp1) (term_to_string exp2)
;;


let vartup_to_string (x, y) = Printf.sprintf "(%s, %s)" (var_to_string x) (var_to_string y) ;;

let cidpairs_to_string cidpairs = 
  List.map (fun (x, y) -> 
    Printf.sprintf "(%s,%s)" (CorePrinting.cid_to_string x) (CorePrinting.cid_to_string y))
    cidpairs
  |> String.concat "\n"
;;

(**** assignment constraints ****)
let var_uses_reg v a s = BoolConst(Alloc(v, Register(a, s)))

let var_doesnt_use_reg v a s :term = Not(var_uses_reg v a s)

let var_only_uses_reg ss v a s : term = 
  let not_s = List.filter (fun s' -> s' <>s) ss in
  And(
    var_uses_reg v a s
    ::(List.map (fun n_s -> var_doesnt_use_reg v a n_s) not_s)
  )
;;

(* each user of the array a is assigned to one of its slots *)
let assignment_constraints a users slots : term list = 
  List.map 
    (fun v -> 
      let (term : term) = 
        Or(List.map (fun s -> var_only_uses_reg slots v a s) slots)
      in term)
    users
;;

(**** alias constraints ****)

(* if v and w are allocated to the same register, it implies they are aliases *)
  let shared_alloc_implies_alias v w a s =
    Implies(And([BoolConst(Alloc(v, Register(a, s))); BoolConst(Alloc(w, Register(a, s)))]), BoolConst(Alias(v, w)))
  ;;
  
  let for_all xs fn : term = 
    And(List.map fn xs)
  ;;
  
  let alias_constraints a users slots = 
    List.map
      (fun v -> 
        (* for every variable v, for all other users of v, sharing any register implies aliasing *)
        for_all users (fun w -> for_all slots (shared_alloc_implies_alias v w a) ))
      users
  ;;
  
(**** conflict constraints ****)

(* 
  given two conflicting variables, v and w,    
    if v_a is an alias of v and w_a is an alias of w,
      then if v_a is allocated to a register, w_a is not *)
let conflicting_aliases_dont_share v v_a w w_a a slots =
  let per_slot v v_a w w_a a s = 
  Implies(
    And([
      BoolConst(Alias(v, v_a)); 
      BoolConst(Alias(w, w_a)); 
      BoolConst(Alloc(v_a, Register(a, s)))]), 
    Not(BoolConst(Alloc(w_a, Register(a, s)))))
  in
  for_all slots (per_slot v v_a w w_a a)
;;

let no_conflicting_aliases_share all_vars v w a slots =
  (* given two conflicting variables, (v, w), 
      for all pairs of aliases of (v, w), the aliases dont map 
     to the same slot in a *)
  for_all all_vars (fun v_a -> 
    for_all all_vars ((fun w_a -> 
      conflicting_aliases_dont_share v v_a w w_a a slots)))
;;

let conflict_constraints conflicts all_vars a users slots = 
  List.map 
    (fun v -> 
      let conflicting_users = match List.assoc_opt v conflicts with  
        | None -> []
        | Some(uss) -> uss
      in 
      for_all conflicting_users 
        (fun w -> no_conflicting_aliases_share all_vars v w a slots))
  users
;;

(**** generating the constraints ****)
let generate_constraints 
  (conflicts : (var * (var list)) list) (* for each variable, a list of its conflicts? *)
  (array_users : (var * (var list)) list) 
  (nslots : int)
  = 
  let all_array_users = List.flatten (List.map snd array_users) |> MiscUtils.unique_list_of in
  let slots = List.init nslots (fun x -> x) in
  let constraints = List.fold_left
    (fun constraints (a, users) -> 
      let ass_constr = assignment_constraints a users slots in
      let ali_constr = alias_constraints a users slots in
      let confl_constr = conflict_constraints conflicts all_array_users a users slots in
      constraints@ass_constr@ali_constr@confl_constr)
      (* constraints
      @(assignment_constraints a users slots)
      @(alias_constraints a users slots)
      @(conflict_constraints conflicts all_array_users a users slots)) *)
    []
    array_users
  in
  constraints
;;


(**** gathering input ****)

(* get all the arrays in the program declarations *)
let arrs_in_prog tds = 
  let constructor_cids = 
    [(Arrays.constructors |> CL.hd |> fst)
    ;(PairArrays.constructors |> CL.hd |> fst)]
  in
  let rec _arrs_in_prog (tds:tdecls) = 
    match tds with 
    | [] -> []
    | {td=TDGlobal(id, _, {e=ECall(constr_cid, _); ety=ety;}); _}::ds -> 
      if (MiscUtils.contains constructor_cids constr_cid)
      then ((id, ety)::(_arrs_in_prog ds))
      else (_arrs_in_prog ds)
    | _::ds -> _arrs_in_prog ds
  in
  _arrs_in_prog tds |> List.rev
;;

(* get all the variables passed to operations on array arr_id and the type of its args *)
let array_user_vars tds arr_id = 
  (* helpers *)
  let accessor_cids = 
    let defs = Arrays.defs@PairArrays.defs in 
    CL.map (fun (gf:InterpState.State.global_fun) -> gf.cid) defs
  in  
  let cid_in_evar (ex : exp) : Cid.t = 
    match ex.e with
    | EVar n -> n
    | _ -> error "expected an EVar as the array argument of an array operation"
  in  
  let call_accesses_arr arr_id call_fcid call_args =
    match (MiscUtils.contains accessor_cids call_fcid) with 
    | true -> 
      let arr_arg =  cid_in_evar (CL.hd call_args) in 
      Cid.equals arr_arg (Cid.id arr_id)
    | false -> false
  in    
  let memop_args_of_array_call fcnid args = 
    match ((Cid.names fcnid), args) with
    | ["Array";"get"], _ -> []
    | ["Array";"getm"], [_; _; _; arg1] -> [arg1]
    | ["Array";"set"], [_; _; arg1] -> [arg1]
    | ["Array";"setm"], [_; _; _; arg1] -> [arg1]
    | ["Array";"update"], [_; _; _; arg1; _; arg2] -> 
      [arg1; arg2]
    | ["Array";"update_complex"], [_; _; _; arg1; arg2; _] -> 
      [arg1; arg2]
    | ["PairArray";"update"], [_; _; _; arg1; arg2; _] -> 
      [arg1; arg2]
    | _ -> error "[posarg_of_arrcall] either the function is not an array accessor, or it has the wrong arguments"
  in

  let user_cids = ref [] in
  let arg_ty = ref None in
  let v = 
    object
    inherit [_] s_iter as super
    method! visit_exp ctx exp = 
      super#visit_exp ctx exp;
      match exp.e with 
      | ECall(fcnid, args) -> (
        if (call_accesses_arr arr_id fcnid args)
        then (
          let inputs = memop_args_of_array_call fcnid args in
          let input_ty = (List.hd inputs).ety in
          let input_vars = List.filter_map (fun input -> 
            match input.e with 
            | EVar v -> Some(v)
            | _ -> None) 
          inputs in
          (* add each input var to user_cids if it does not already exist there *)
          user_cids := 
            (!user_cids)
            @(List.filter (fun cid -> not (List.mem cid !user_cids)) input_vars);
          arg_ty := Some(input_ty);
        )
        else ()

      )
      | _ -> ()
    end
  in
  v#visit_tdecls () tds;
  (* unique cids! *)
  let user_cids = MiscUtils.unique_list_of !user_cids in
  (* Printf.printf "users of array %s: [%s]\n" 
    (CorePrinting.id_to_string arr_id) 
    (CorePrinting.comma_sep CorePrinting.cid_to_string user_cids); *)
  user_cids, !arg_ty    
;;

(* build a conflict graph *)

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

let rec conflict_graph_of_statement (declared_before : cid list) (used_after : cid list) statement : (cid * cid) list =
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
    let edges_from_s1 = conflict_graph_of_statement declared_before used_after s1 in
    let edges_from_s2 = conflict_graph_of_statement  declared_before used_after s2 in
    edges_from_s1@edges_from_s2
  )
  | SMatch(_, bs) -> (
    CL.map 
      (fun (_, stmt) -> conflict_graph_of_statement declared_before used_after stmt) 
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
    let s1_edges = conflict_graph_of_statement (declared_before) (used_after@used_in_s2) s1 in
    let s2_edges = conflict_graph_of_statement (declared_before@declared_in_s1) used_after s2 in
    s1_edges@s2_edges
  )
  (* no other statements add conflicts. Just update the declared before / used after sets *)
  | _ -> []
  in
  base_conflicts
  (* base_conflicts@alias_conflicts *)
;;

let conflict_graph_of_handler (var_map:(cid * var) list) ds = 
  let main_hdl = (main_handler_of_decls ds) in
  let params_by_event = main_hdl.hdl_input 
  |> grouped_localids_of_event_params 
  in
  let shared_params = 
    (main_hdl.hdl_preallocated_vars
    @main_hdl.hdl_deparse_params)
    |> (List.map (fun (x, _) -> Cid.id x)) 
  in 
  let statement = match main_hdl.hdl_body with
    | SFlat s -> s
    | SPipeline _ -> failwith "[conflict graph] expected handler body before layout" 
  in
  (* add conflicts between parameters *)
  let param_conflicts = List.fold_left 
    (fun base_conflicts params -> 
        let param_pairs = MiscUtils.get_unique_unordered_pairs params params
         |> List.filter (fun (x, y) -> not (Cid.equal x y))
        in
        base_conflicts
        @param_pairs)
    []
    params_by_event
  in
  (* parameters are declared before and nothing is used after the root statement *)
  let declared_before = (Caml.List.flatten params_by_event)@shared_params in
  let var_conflicts = conflict_graph_of_statement declared_before [] statement in
  let conflict_pairs = param_conflicts@var_conflicts |> MiscUtils.unique_list_of in
  (* we only want the conflict pairs where both elements are defined in the var map, 
     and we want vars not cids *)
  let conflict_varpairs = List.filter_map 
    (fun (x, y) -> 
      let x = List.assoc_opt x var_map in
      let y = List.assoc_opt y var_map in
      match (x, y) with
      | (Some(x), Some(y)) -> Some(x, y)
      | _ -> None
    )

    conflict_pairs
  in
  conflict_varpairs
;;

(* variable conflicts in the parser *)

let used_in_exp exp = 
  let vars = ref [] in 
  let v =
    object
      inherit [_] CoreSyntax.s_iter as super
      method! visit_EVar _ var_cid = 
        vars := var_cid::(!vars);
    end
  in
  v#visit_exp () (exp);
  !vars
;;

(* all variables used _after_ the parse block *)
let parser_used_after pb = 

  let vars = ref [] in 
  let v =
    object
      inherit [_] CoreSyntax.s_iter as super
      method! visit_EVar _ var_cid = 
        vars := var_cid::(!vars);
    end
  in
  v#visit_parser_step () (fst pb.pstep);
  (!vars)
;;

let used_in_action pa = 
  match pa with
  | PAssign(_, exp) -> (used_in_exp exp)
  | _ -> []
;;
let assigned_in_action pa = 
  match pa with 
  | PRead(cid, _) -> [cid]
  | PPeek(cid, _) -> [cid]
  | PAssign(cid, _) -> [cid]
  | _ -> []
;;

(* what are the conflicts of this parser action? *)
let rec conflicts_of_parser_actions declared_before used_after pas = 
  match pas with
  | [] -> declared_before, []
  | pa::pas -> (
    match pa with
    | PRead(cid, _) 
    | PPeek(cid, _)
    | PAssign(cid, _) -> (
      let used_after_local = used_after@(List.map assigned_in_action pas |> List.flatten) |> MiscUtils.unique_list_of in 
      let live_vars = List.filter (fun cid -> MiscUtils.contains used_after_local cid) declared_before in
      if (MiscUtils.contains used_after_local cid)
        (* if cid is used later, then update the declared_before list for the next action and 
           add a conflict between cid and every currently live variable *)
        then (
          let declared_before = (cid::declared_before |> MiscUtils.unique_list_of) in 
          let conflicts = List.map (fun live_var -> (cid, live_var)) live_vars in 
          let declared_before, conflicts' = conflicts_of_parser_actions declared_before used_after pas in 
          declared_before, conflicts@conflicts'
        )
        else (conflicts_of_parser_actions declared_before used_after pas)
    )
    | PSkip(_) -> conflicts_of_parser_actions declared_before used_after pas
  )
;;

let rec conflict_graph_of_parse_block (declared_before : cid list) (output_params : cid list) pb : (cid * cid) list = 
  (* each parser block determines the variables that are used after on its own -- just the 
     output params are propogated *)
  let used_after = output_params@(parser_used_after pb) in
  let declared_before, conflicts = conflicts_of_parser_actions declared_before used_after (List.map fst pb.pactions) in
  let future_conflicts = conflict_graph_of_pstep declared_before output_params (fst pb.pstep) in
  conflicts@future_conflicts

and conflict_graph_of_pstep declared_before output_params pstep = 
  match pstep with 
  | PMatch(_, pbs) -> 
    (* recurse on all the inner blocks *)
    List.map (fun (_, pb) -> conflict_graph_of_parse_block declared_before output_params pb) pbs
    |> List.concat
  (* none of the other steps introduce new conflicts, as they don't set anything *)
  | PCall(_) -> []
  | _ -> [] 
;;
let conflict_graph_of_parser var_map ds = 
  let parser = main_parser_of_tds ds  in  
  match parser.pret_event with
  | Some(ret_event) -> 
    let out_params = grouped_localids_of_event_params (ret_event) |> List.flatten in
    (* print_endline ("output parameters of event");
    CorePrinting.comma_sep CorePrinting.cid_to_string out_params |> print_endline; *)
    let conflict_pairs = conflict_graph_of_parse_block [] out_params parser.pblock |> 
      MiscUtils.unique_list_of 
    in
    (* print_endline ("parser");
    print_endline (TofinoCorePrinting.parser_to_string parser);
    print_endline ("conflict pairs");
    (cidpairs_to_string conflict_pairs) |> print_endline; *)
    let conflict_varpairs = Core.List.filter_map conflict_pairs
    ~f:(fun (x, y) -> 
      let x = Core.List.Assoc.find var_map ~equal:Cid.equal x in
      let y = Core.List.Assoc.find var_map ~equal:Cid.equal y in
      match (x, y) with
      | (Some(x), Some(y)) -> Some(x, y)
      | _ -> None
    )
    in
    conflict_varpairs
  (* if there's no output event, there's no conflicts. 
     But there's no program either, so seems like an error.  *)
  | None -> error "[conflict_graph_of_parser] failed to build conflict graph -- no output event in parser?"
;;


let var_uid = ref 1 ;;
let update_varmap_cid var_map cid ty = 
  match List.assoc_opt cid var_map with
  | Some(_) -> var_map
  | None ->
    let var = {vcid = cid; vuid = !var_uid; vty = ty;} in
    var_uid := !var_uid + 1;
    let var_map = (cid, var)::var_map in 
    var_map
;;
let update_varmap_id var_map id ty = 
  let cid = Cid.id id in
  update_varmap_cid var_map cid ty
;;


(* get the conflict graph from decls, where the conflict graph is 
   edges between vars. Vars are just cids with unique id tags. *)
let conflict_graph_of_decls tds = 
let arrs_tys = arrs_in_prog tds in
(* the var_map has all the array ids and array users in it *)
let var_map = List.fold_left (fun var_map (arr, ty) -> update_varmap_id var_map arr ty) [] arrs_tys in
let var_map, array_users = List.fold_left
  (fun (var_map, array_users) arr_id -> 
    let users, arg_ty = array_user_vars tds arr_id in
    match arg_ty with 
    | None -> var_map, array_users
    | Some(arg_ty) -> 
      let var_map' = List.fold_left
        (fun var_map' cid -> update_varmap_cid var_map' cid arg_ty)
        var_map
        users
      in
      (* convert everything to a var with a uid at this point *)
      let arr_var = List.assoc (Cid.id arr_id) var_map' in
      let users_vars = List.map (fun cid -> List.assoc cid var_map') users in

      var_map',array_users@[arr_var, users_vars]
    )
  (var_map, [])
  (List.split arrs_tys |> fst)
in
let parser_conflict_pairs = conflict_graph_of_parser var_map tds in
let handler_conflict_pairs = conflict_graph_of_handler var_map tds in
let conflict_pairs = parser_conflict_pairs@handler_conflict_pairs in
conflict_pairs, array_users
;;  

(* conflict graph with cids instead of vars -- for when you are not 
   doing constraint-based stuff with z3. *)
let cid_conflict_graph_of_decls tds = 
  let conflict_graph, array_users = conflict_graph_of_decls tds in
  let cid_conflict_graph = List.map (fun (v1, v2) -> v1.vcid, v2.vcid) conflict_graph in
  let cid_array_users = List.map
    (fun (v1, v2s) -> v1.vcid, 
      List.map (fun v2 -> v2.vcid) v2s)
    array_users
  in
  cid_conflict_graph, cid_array_users
;;

(*** convert the conflict graph to an associative list that maps each variable to 
    all of its conflicts ***)
let assoc_append x y base = 
  let x_entry, base = match (List.assoc_opt x base) with 
    | None -> [y], base
    | Some(x_entry) -> y::x_entry, List.remove_assoc x base
  in
  ((x, x_entry)::base)
;;

let rec conflict_pairs_to_assoc conflict_pairs = 
  match conflict_pairs with
  | [] -> []
  | (x, y)::conflict_pairs -> 
    let base = conflict_pairs_to_assoc conflict_pairs in
    let base = assoc_append x y base in  
    let base = assoc_append y x base in
    base
;;

(**** mapping to / from Z3 equations ****)

module Z3Map = Map.Make(
  struct
    type t = string
    let compare = String.compare
  end
)

let z3_ctx = 
  mk_context ["model", "true"; "proof", "true"] 
;;

type ctx = {
  const_map : bool_const Z3Map.t; (* map booleans in constraints to z3 expressions *)
}

(* 1. translating to z3 *)
let boolconst_to_z3 ctx bc = 
  let name = match bc with
  | Alloc(var, reg) -> (
    match reg with
      | Register(reg_var, reg_slot) -> 
        let name = Printf.sprintf "alloc_%i_%i_%i" var.vuid reg_var.vuid reg_slot in
        name)  
  | Alias(var1, var2) -> 
    let name = Printf.sprintf "alias_%i_%i" var1.vuid var2.vuid in
    name
  in
  let expr = Z3Bool.mk_const_s z3_ctx name in
  let const_map = Z3Map.add name bc ctx.const_map in
  {const_map}, expr    
;;

let rec term_to_z3 ctx term = 
  match term with 
  | BoolConst(bc) -> boolconst_to_z3 ctx bc
  | Or(terms) -> 
    let ctx, z3_terms = terms_to_z3 ctx terms in
    ctx, Z3Bool.mk_or z3_ctx z3_terms
  | And(terms) ->   
    let ctx, z3_terms = terms_to_z3 ctx terms in
    ctx, Z3Bool.mk_and z3_ctx z3_terms
  | Not(term) -> 
    let ctx, z3_term = term_to_z3 ctx term in
    ctx, Z3Bool.mk_not z3_ctx z3_term
  | Implies(p, s) -> 
    let ctx, z3p = term_to_z3 ctx p in
    let ctx, z3s = term_to_z3 ctx s in
    ctx, Z3Bool.mk_implies z3_ctx z3p z3s
and terms_to_z3 ctx terms = 
    let ctx, z3terms = List.fold_left
      (fun (ctx, z3terms) term -> 
        let ctx, z3term = term_to_z3 ctx term in
        ctx, z3terms@[z3term])
      (ctx, []) 
      terms
    in
    ctx, z3terms    
;;

(* 2. running the solver and get the mapping from boolean var names -> values *)
let run_solver constraints = 
  let solver = Solver.mk_simple_solver z3_ctx in
  let ctx, z3_constraints = terms_to_z3 {const_map=Z3Map.empty;} constraints in
  Solver.add solver z3_constraints;
  let ret = Solver.check solver [] in
  match ret with
  | SATISFIABLE ->
    let model = Solver.get_model solver |> Option.get in
    let decls = Model.get_decls model in
    let solution = ref [] in
    List.iter (fun decl ->
      match Model.get_const_interp model decl with
      | Some value ->
        let sort = FuncDecl.get_range decl in
        if Sort.to_string sort = "Bool" then
          let name = FuncDecl.get_name decl |> Symbol.to_string in
          (* convert the name back into a boolean variable... *)
          let boolvar = Z3Map.find name ctx.const_map in
          let v = match Expr.to_string value with
            | "true" -> true
            | "false" -> false
            | _ -> error "unexpected value string -- expected true or false"
          in
          solution := (boolvar, v)::(!solution);
        else ()
      | None -> ()
    ) decls;
    true, !solution
  | UNSATISFIABLE -> false, []
  | UNKNOWN -> false, []
;;
(* translate the z3 assignment back to a list of aliased id pairs, each pair with a type *)
let rec assignment_to_alias_pairs assignments = 
  match assignments with
  | [] -> []
  | assignment::assignments -> (
    match assignment with 
    | (Alias(v1, v2), true) -> (
      (* skip self aliases *)
      if (v1.vuid <> v2.vuid) then 
        ((v1.vcid, v2.vcid), v1.vty)::(assignment_to_alias_pairs assignments)
      else
        (assignment_to_alias_pairs assignments) 
    )
    | _ -> 
      assignment_to_alias_pairs assignments
    )
;;

(* misc helpers *)
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

let alias_groups (overlay_pairs : ((cid * cid) * ty) list) =  
  let open Core in
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

(* replacing aliases with master variables *)

type vloc = | Param | Prealloc | Local
let is_param input_event cid = 
  List.exists (fun x -> Cid.equal x cid) (grouped_localids_of_event_params input_event |> Caml.List.flatten) 
;;
let is_prealloc main cid =
  List.exists (fun (x, _) -> (Cid.equal (Cid.id x) cid)) 
    (main.hdl_preallocated_vars@main.hdl_deparse_params)
;;
let cid_to_loc main cid =
  if is_param (main.hdl_input) cid then Param
  else if (is_prealloc main cid) then Prealloc
  else Local
;;


let replace_cid tds cids cid' = 
  (* replace any cid in cids with cid' everywhere they appear (including the parser) *)
  let is_tgt_cid cidq = List.exists (fun x -> Cid.equal x cidq) cids in
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
        then (
          let pnew = PPeek(cid', lty) in
          pnew
          )
        else (PPeek(lcid, lty))
      (* statements *)
      method! visit_SAssign ctx lcid rexp = 
        if (is_tgt_cid lcid) 
        then (
          (* print_endline ("SASSIGN replacing "^(CorePrinting.cid_to_string lcid)^" with "^(CorePrinting.cid_to_string cid')^" in SAssign");
          print_endline ("old statement: "^(CorePrinting.s_to_string (SAssign(lcid, rexp))));
          print_endline ("new statement: "^(CorePrinting.s_to_string (SAssign(cid', (super#visit_exp ctx rexp))))); *)
          SAssign(cid', (super#visit_exp ctx rexp))
          )
        else (SAssign(lcid, (super#visit_exp ctx rexp)))
      method! visit_SLocal ctx id ty exp =
        (* if this cid is the target, we replace the declaration with an assign *)
        if (is_tgt_cid (Cid.id id)) 
        then (
          (* print_endline ("SLOCAL replacing "^(CorePrinting.id_to_string id)^" with "^(CorePrinting.cid_to_string cid')^" in SAssign");
          print_endline ("old statement: "^(CorePrinting.s_to_string (SLocal(id ,ty, exp))));
          print_endline ("new statement: "^(CorePrinting.s_to_string (SAssign(cid', (super#visit_exp ctx exp))))); *)
        SAssign(cid', (super#visit_exp ctx exp))
          )
        else (SLocal(id, ty, (super#visit_exp ctx exp)))
    end
  in 
  v#visit_tdecls () tds
;;  

let remove_preallocated_var tds cid = 
  let main = main_handler_of_decls tds in
  let hdl_preallocated_vars' = 
    List.filter (fun (x, _) -> not (Cid.equal (Cid.id x) cid)) 
    main.hdl_preallocated_vars 
  in
  let hdl_deparse_params' = 
    List.filter (fun (x, _) -> not (Cid.equal (Cid.id x) cid)) 
    main.hdl_deparse_params 
  in
  let main' = { main with 
    hdl_preallocated_vars = hdl_preallocated_vars';
    hdl_deparse_params = hdl_deparse_params'; }
  in
  replace_main_handler_of_decls tds main'

(* to overlay a group of aliases where at least one is a param, 
   we choose a param to be the name of all the vars and rename 
   them all to that master. *)
let overlay_param_aliases tds vars var_locs = 
(* find the first param *)
let master_cid = List.find 
  (fun (_, loc) -> loc = Param) 
  (List.combine vars var_locs) 
  |> fst 
in
replace_cid tds vars master_cid
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
  List.fold_left  
    (fun tds cid -> 
      match cid_to_loc (main_handler_of_decls tds) cid with
      | Local -> tds
      | Param -> tds
      | Prealloc -> remove_preallocated_var tds cid
    )
    tds
    vars
  ;;
  

let overlay_aliases tds (vars, varty) = 
  let main = main_handler_of_decls tds in
  let var_locs = List.map (cid_to_loc main) vars  in
  (* if any of the variables are parameters, we have to update the parser and 
     also we use one of the existing variables as the "master" *)
  let has_param = List.exists (fun x -> x = Param) var_locs  in
  if (has_param) then
    overlay_param_aliases tds vars var_locs
  (* if there are no params, we create a new shared local and 
     replace all the uses of the variables with the shared local. 
      We also delete the declarations for all the variables. *)
  else
    overlay_local_aliases tds vars varty
;;



(* The old memop -> sALU register allocator -- used when the sat version 
   cannot find an allocation, creates intermediate variables with copies. 
   Very similar to the code in "ShareMemopInputsNew", with a few tweaks. *)

module ShareMemopInputsBaseline = struct 
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

let cons_uniq_eq eq xs x = if List.mem xs x ~equal:eq then xs else x :: xs
let unique_list_of_eq eq xs = List.rev (Caml.List.fold_left (cons_uniq_eq eq) [] xs)


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
   
   (* conflict analysis
   
     x and y conflict if they are alive at the same time in the program. 
     at any point in a program, an alive variable is one that:
       - has been declared in the past or at the current statement
       - is used in the future
   *)
   
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
   
   
   (* build the conflict graph for the statement *)
   let rec build_conflict_graph (alias_list :alias_list) (declared_before : cid list) (used_after : cid list) statement : (cid * cid) list =
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
     base_conflicts
     (* base_conflicts@alias_conflicts *)
   ;;
   
   (* construct the conflict graph for a handler *)
   let conflict_graph alias_list main_hdl : (cid * cid) list =
     let shared_params = 
      (main_hdl.hdl_preallocated_vars
      @main_hdl.hdl_deparse_params) 
      |> (List.map ~f:(fun (x, _) -> Cid.id x)) in 
     let statement = match main_hdl.hdl_body with
       | SFlat s -> s
       | SPipeline _ -> error "[conflict graph] expected handler body before layout" 
     in
   
     let get_aliases cid = match (List.Assoc.find alias_list cid ~equal:Cid.equal) with
       | Some(aliases) -> aliases |> CL.filter (fun alias -> not (Cid.equal cid alias))
       | None -> []
     in
     (* build the base conflict graph, which includes edges between all parameters in the same event *)
     let params_by_event = main_hdl.hdl_input 
     |> grouped_localids_of_event_params 
     in
     (* let param_conflicts = List.fold_left params_by_event
       ~init:[]
       ~f:(fun base_conflicts params -> 
           let param_pairs = MiscUtils.get_unique_unordered_pairs params params
            |> List.filter ~f:(fun (x, y) -> not (Cid.equal x y))
           in
           base_conflicts
           @param_pairs)
     in *)
     (* actually, since this version of the code doesn't identify conflicts due to the parser, 
        we have to consider all the parameters of all the events to conflict. *)
     let all_params = List.concat params_by_event in
     let param_conflicts = MiscUtils.get_unique_unordered_pairs all_params all_params in
     (* let param_alias_conflicts =
       List.concat_map param_conflicts
         ~f:(fun (x, y) ->
           List.concat_map (get_aliases x)
             ~f:(fun x' ->
               List.map (get_aliases y)
                 ~f:(fun y' -> (x', y'))
             )
         )      
     in   *)
     (* parameters are declared before and nothing is used after the root statement *)
     let declared_before = (List.concat params_by_event)@shared_params in
     (* reset op counter *)
     let var_conflicts = build_conflict_graph alias_list declared_before [] statement in
     (* now that we got the base variable conflicts, add edges due to aliases *)
     (* for every edge (x, y) in var_conflicts:
         for every alias x' of x:
           for every alias y' of y:
             add (x', y') to conflicts *)
     let alias_conflicts = ref [] in
     List.iter var_conflicts ~f:(fun (x, y) -> 
       let x_aliases = get_aliases x in
       let y_aliases = get_aliases y in
       (* if (List.length x_aliases > 0 || List.length y_aliases > 0)
         then ( *)
       (* print_endline("deriving alias conflicts for ("^(CorePrinting.cid_to_string x)^", "^(CorePrinting.cid_to_string y)^")");
       print_endline("x aliases: "^(CorePrinting.comma_sep CorePrinting.cid_to_string x_aliases));
       print_endline("y aliases: "^(CorePrinting.comma_sep CorePrinting.cid_to_string y_aliases));); *)
       List.iter (x::x_aliases) ~f:(fun x' ->
         List.iter (y::y_aliases) ~f:(fun y' ->
           (* print_endline("\tadding alias conflict: ("^(CorePrinting.cid_to_string x')^", "^(CorePrinting.cid_to_string y')^")"); *)
           alias_conflicts := (x', y')::(!alias_conflicts)
         )
       )
     );
   
     (* let alias_conflicts =
       List.concat_map (param_conflicts@var_conflicts)
         ~f:(fun (x, y) ->
           List.concat_map (get_aliases x)
             ~f:(fun x' ->
               List.map (get_aliases y)
                 ~f:(fun y' -> (x', y'))
             )
         )      
     in *)
     let alias_conflicts = !alias_conflicts in 
     (* print_endline ("----var conflicts----");  
     (* List.filter var_conflicts ~f:(fun (x, y) -> (CorePrinting.cid_to_string x = "stg_1_src3151") or (CorePrinting.cid_to_string y = "stg_1_src3151")) *)
     List.map ~f:(fun (x, y) -> "("^(CorePrinting.cid_to_string x) ^ "," ^ (CorePrinting.cid_to_string y)^")") var_conflicts
     |> String.concat "\n" 
     |> print_endline; *)
   
     (* print_endline ("----alias conflicts----");
     (* List.filter alias_conflicts ~f:(fun (x, y) -> (CorePrinting.cid_to_string x = "stg_1_src3151") or (CorePrinting.cid_to_string y = "stg_1_src3151")) *)
     List.map ~f:(fun (x, y) -> "("^(CorePrinting.cid_to_string x) ^ "," ^ (CorePrinting.cid_to_string y)^")") var_conflicts
     |> String.concat "\n" 
     |> print_endline;
     print_endline ("----end alias conflicts----"); *)
     param_conflicts@var_conflicts@alias_conflicts
   ;;
   
   (* get all the pairwise conflicts *)
   let precompute_conflict_graph ds overlaid_pairs = 
     let main_hdl = (main_handler_of_decls ds) in
     let aliases = (overlaid_pairs_to_alias_list overlaid_pairs) in
     (* lets take a look at the alias list(s) *)
     (* let alias_entry_to_string (cid, alias_cids) = 
       let alias_cids_string = List.map ~f:(fun cid -> CorePrinting.cid_to_string cid) alias_cids |> String.concat ", " in
       (CorePrinting.cid_to_string cid)^" -> ["^alias_cids_string^"]"
     in *)
     (* let alias_list_string = List.map ~f:alias_entry_to_string aliases |> String.concat "\n" in *)
     (* print_endline ("----alias list----");
     print_endline alias_list_string;
     print_endline ("----end alias list----"); *)
     let conflict_pairs = conflict_graph 
       aliases 
       main_hdl 
       |> MiscUtils.unique_list_of     
     in
   
     conflict_pairs
   ;;
   
   let check_one_conflict conflict_pairs (x, y) =
     let conflict_exists = MiscUtils.contains conflict_pairs (x, y) or  MiscUtils.contains conflict_pairs (x, y) in
     conflict_exists
   ;;
   
   let any_pairs_conflict ds vars overlay_pairs =
     (* print_endline ("[any_pairs_conflict] aliases");
     List.iter overlay_pairs (fun (x, y) -> print_endline ((CorePrinting.cid_to_string x)^" -> "^(CorePrinting.cid_to_string y)));
     print_endline ("----"); *)
     (* compute all the conflict pairs at this point in the program *)
     let conflict_pairs = precompute_conflict_graph ds overlay_pairs in
     let conflict = ref false in
     let _ = List.iter vars (fun x ->
       List.iter vars (fun y ->
         if not (Cid.equal x y) then
           conflict := !conflict || (check_one_conflict conflict_pairs (x, y))
       )
     ) 
     in
     !conflict
     (* get all the possible pairs of variables we are checking
     let var_pairs = MiscUtils.get_unique_unordered_pairs vars vars
       |> List.filter ~f:(fun (x, y) -> not (Cid.equal x y))
     in
     (* check if any vars conflict *)
     let conflict_exists =
       List.exists var_pairs (fun (x, y) -> MiscUtils.contains conflict_pairs (x, y))
     in
     conflict_exists *)
   ;;
   
   
   (* get the ids of all the arrays in the program *)
   let rec _arrs_in_prog (tds:tdecls) = 
     match tds with 
     | [] -> []
     | {td=TDGlobal(id, _, {e=ECall(constr_cid, _); _}); _}::ds -> 
       if (MiscUtils.contains constructor_cids constr_cid)
       then (id::(_arrs_in_prog ds))
       else (_arrs_in_prog ds)
     | _::ds -> _arrs_in_prog ds
   ;;
   let arrs_in_prog tds = _arrs_in_prog tds |> List.rev
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
   
   (* get all the memop var arguments in array method 
      calls that access arrid. If there are >= 2 args total, we don't need to 
      worry about merging anything together. *)
   let all_memop_var_args_of_array_calls ds arrid = 
     let nths = [0; 1; 2] in
     let args = CL.filter_map (fun n -> nth_memop_var_args_of_array_calls ds arrid n) nths in
     match args with
     | [] -> None
     | _ -> Some(
     let ty = CL.hd args |> snd in
     let args = CL.map fst args in
     let args = CL.concat args in
     let args = MiscUtils.unique_list_of args in
     args, ty
     )
   ;;
   
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
   
   
   let merge_nth_arg (overlaid_pairs: ((cid * cid) * ty) list) (tds:tdecl list) arr_id nth args argty : ((cid * cid) * ty) list * tdecl list = 
     match (CL.length (unique_list_of_eq Cid.equal args)) with 
       (* there are 0 or 1 unique vars -- no need to do anything *)
       | 0 | 1 -> overlaid_pairs, tds
       (* >= 2 vars -- we need to get it down to 1 var, by either overlaying the variables or creating an intermediate *)
       | _ -> (
         (* print_endline("[merge_nth_arg] merging Array method args: "^(CorePrinting.comma_sep CorePrinting.cid_to_string args)); *)
         (* if any of the variables conflict, we have to create an intermediate *)
         if (any_pairs_conflict tds args (List.map ~f:fst overlaid_pairs))
         then (
           let msg = Printf.sprintf "[SALU input overlaying] variables passed to an array operation on array %s conflict, so an intermediate needs to be created. This may add a stage of overhead." (CorePrinting.id_to_string arr_id) in
           let msg = msg ^ "\nconflicting variables: " ^ (CorePrinting.comma_sep CorePrinting.cid_to_string args) in
           print_endline msg;
           (overlaid_pairs, create_memop_input_var tds arr_id nth argty))
         else (
           (* let msg = Printf.sprintf "[SALU input overlaying] variables passed to an array operation on array %s do not conflict, so they can be overlaid." (CorePrinting.id_to_string arr_id) in
           let msg = msg ^ "\nvariables: " ^ (CorePrinting.comma_sep CorePrinting.cid_to_string args) in
           print_endline msg; *)
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
   
   
   let rec list_to_pairs xs = 
     match xs with
     | [] | _::[] -> []
     | x::y::xs -> (x, y)::(list_to_pairs (y::xs))
   ;;
   
   
   let rec process_arrs overlaid_pairs arrs tds = 
     match arrs with
     | [] -> overlaid_pairs, tds
     | arr::arrs -> 
       (*  *)
       match (all_memop_var_args_of_array_calls tds arr) with  
       | None ->  process_arrs overlaid_pairs arrs tds
       | Some(all_args, _) -> (
           (* if there are >=2 args, overlay / merge, else do nothing *)
           if ((List.length all_args) >= 2)
           then (
             let overlaid_pairs, tds' = process_arr overlaid_pairs arr tds [0;1;2] in
             process_arrs overlaid_pairs arrs tds'
           )
           else (
             process_arrs overlaid_pairs arrs tds
           )
       )
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
   
   let is_param input_event cid = 
     List.exists (grouped_localids_of_event_params input_event |> Caml.List.flatten) ~f:(fun x -> Cid.equal x cid)
   ;;
   let is_prealloc main cid =
     List.exists 
     (main.hdl_preallocated_vars@main.hdl_deparse_params) 
     ~f:(fun (x, _) -> (Cid.equal (Cid.id x) cid))
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
           then (
             let pnew = PPeek(cid', lty) in
             pnew
             )
           else (PPeek(lcid, lty))
         (* statements *)
         method! visit_SAssign ctx lcid rexp = 
           if (is_tgt_cid lcid) 
           then (
             (* print_endline ("SASSIGN replacing "^(CorePrinting.cid_to_string lcid)^" with "^(CorePrinting.cid_to_string cid')^" in SAssign");
             print_endline ("old statement: "^(CorePrinting.s_to_string (SAssign(lcid, rexp))));
             print_endline ("new statement: "^(CorePrinting.s_to_string (SAssign(cid', (super#visit_exp ctx rexp))))); *)
             SAssign(cid', (super#visit_exp ctx rexp))
             )
           else (SAssign(lcid, (super#visit_exp ctx rexp)))
         method! visit_SLocal ctx id ty exp =
           (* if this cid is the target, we replace the declaration with an assign *)
           if (is_tgt_cid (Cid.id id)) 
           then (
             (* print_endline ("SLOCAL replacing "^(CorePrinting.id_to_string id)^" with "^(CorePrinting.cid_to_string cid')^" in SAssign");
             print_endline ("old statement: "^(CorePrinting.s_to_string (SLocal(id ,ty, exp))));
             print_endline ("new statement: "^(CorePrinting.s_to_string (SAssign(cid', (super#visit_exp ctx exp))))); *)
           SAssign(cid', (super#visit_exp ctx exp))
             )
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
     (* print_endline("[overlay_aliases] overlaying variables");
     print_endline(CorePrinting.comma_sep CorePrinting.cid_to_string vars);
     print_endline("[overlay_aliases] in program:");
     print_endline(TofinoCorePrinting.tdecls_to_string tds); *)
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
     (* first, calculate the overlaid pairs and also create new intermediates whenever local overlaying is not possible *)
     let overlaid_pairs, tds = process_arrs [] (arrs_in_prog tds) tds in
     (* then, rename variables to implement the overlaid pairs *)
     let alias_grps = alias_groups overlaid_pairs in
     let tds = List.fold alias_grps ~f:overlay_aliases ~init:tds in
     tds
   ;;   
end

let solve_overlay_opt conflict_pairs array_users = 
  let conflict_assoc = conflict_pairs_to_assoc conflict_pairs in
  let constraints = generate_constraints conflict_assoc array_users 2 in

  let success, assignment = run_solver constraints in
  if success then 
    let alias_pairs = assignment_to_alias_pairs assignment |> MiscUtils.unique_list_of in
    let alias_grps = alias_groups alias_pairs in
    Some(alias_grps)
else
  None
;;

module ShareMemopInputsGreedy = struct
  (* heuristic based memop input allocator that 
     processes one array at a time. If it can overlay the variables
     passed to the array's sALU, it does. Else, it creates 
     intermediate variables and adds copy operations to break 
     the conflicts, which adds stage overhead. *)
  
  let create_memop_input_var  = ShareMemopInputsBaseline.create_memop_input_var

  let process_array component (arr : Cid.t) = 
    (* note: we have to take a cid, not a var, because vars from different conflict graphs 
       will have different uids! *)
    (* get the conflict graph and users of each array. *)
    let conflict_graph, array_users = conflict_graph_of_decls (component.comp_decls) in
    (* filter array users for this array *)
    let arr_users_opt = List.find_opt 
    (fun (arr_var, _) -> Cid.equal arr_var.vcid arr)
      array_users
    in
    match arr_users_opt with
    | None -> component
    | Some(_, users) when (List.length users) <= 2 -> 
      (* if there are 2 or fewer variables, we don't need to do anything. *)
      component
    | Some(arr, users) -> 
      (* filter conflict graph so its only about conflicts between users of this array. *)
      let conflict_graph = List.filter
        (fun (x, y) -> 
          (List.exists (fun n -> n.vuid = x.vuid) users)
          &&
          (List.exists (fun n -> n.vuid = y.vuid) users)
        )
        conflict_graph
      in
      (* now see if there is an overlay solution for just this array *)
      let alias_groups_opt = solve_overlay_opt conflict_graph [arr, users] in 
      match alias_groups_opt with
      (* there's a solution, so for this array we can just overlay. *)
      | Some(alias_grps) ->
        let comp_decls' = List.fold_left overlay_aliases component.comp_decls alias_grps in 
        {component with comp_decls = comp_decls'}
      | _ ->
        (* there's no overlay solution! create intermediate variables -- 1 intermediate 
          variable for each argument position of the array. *)
        let intermediate_ty = (List.hd users).vty in
        let nths = [0; 1] in
        (* for each argument position in all calls to this array, 
          create an intermediate, replace the nth positional argument with 
          the intermediate, and create a copy operation to set the intermediate 
          before the array call. *)
        let comp_decls' = List.fold_left
          (fun comp_decls nth -> 
            create_memop_input_var comp_decls (Cid.to_id arr.vcid) nth intermediate_ty)
          component.comp_decls
          nths
        in
        {component with comp_decls = comp_decls'}

  ;;
  let process_component component = 
    let _, array_users = conflict_graph_of_decls (component.comp_decls) in
    let arrs = List.map (fun (arrvar, _) -> arrvar.vcid) array_users in
    let component = List.fold_left process_array component arrs in  
    component
  ;;

end


let process_component component = 
  let tds = component.comp_decls in
  let conflict_pairs, array_users = conflict_graph_of_decls tds in
  let alias_groups_opt = solve_overlay_opt conflict_pairs array_users in
  match alias_groups_opt with
  | Some(alias_grps) -> 
    (* print_endline ("---[alias groups]---");
    List.iter ~f:(fun aliases -> print_endline (CorePrinting.comma_sep CorePrinting.cid_to_string (fst aliases))) alias_grps;
    print_endline ("----------------------"); *)
    let tds = List.fold_left overlay_aliases tds alias_grps in 
    {component with comp_decls = tds} 
  | None -> 
    print_endline ("[sALU input allocator] failed to find a global aliasing for variables to fit all sALU input registers. Using greedy allocator with possible stage overhead.");
    ShareMemopInputsGreedy.process_component component
;;

let process_core core_prog = 
  if (Cmdline.cfg.optimal_memop_input_alloc)
    then (
      List.map (fun component -> 
        if (component.comp_sort != HControl) then
          process_component component
        else
          component
        )
        core_prog    
    )
    (* use old allocator *)
    else (
      print_endline ("[optimal_memop_input_alloc] using baseline allocator due to command line option.");
      List.map (fun component -> 
        if (component.comp_sort != HControl) then
          {component with comp_decls = ShareMemopInputsBaseline.process_component component.comp_decls}
        else
          component
        )
        core_prog        
    ) 
;;
