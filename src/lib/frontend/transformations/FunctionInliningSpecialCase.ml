(* An inlining pass for certain kinds of functions / 
   calls that are easy to inline with zero overhead. 
   To be inlined with no overhead, there are two requirements: 
    1. the function never writes to its parameters
    2. the function call appears in an assignment statement, 
       OR the function has only one return statement.
   This pass runs before the general inlining pass and does not 
   remove any function definitions from the program.
   This pass will eventually be removed when either function 
   inlining is improved or partial interpretation is complete and 
   optimized away the overhead added by inlining. 
   - jps 1/18/22
*)

open Syntax
open Batteries
open Collections

let err s = Console.error s

(* get data about functions in declarations *)
let dfuns ds = 
  let is_fcn d = match d.d with 
    | DFun(id, _, _, (params, stmt)) -> Some (id, params, stmt)
    | _ -> None
  in 
  List.filter_map is_fcn ds
;;

(* Check if a variable is never assigned in s. *)
let read_only_var (id : Id.t) (s : statement) : bool =
  let ret = ref true in
  let v =
    object
      inherit [_] s_iter as super
      method! visit_SAssign _ id2 _ = if Id.equal id id2 then ret := false
    end
  in
  v#visit_statement () s;
  !ret
;;

(* Check if no parameters are assigned to in a function *)
let read_only_params fcn_params fcn_stmt = 
  List.fold_left (fun prev p -> prev & (read_only_var (fst p) fcn_stmt)) true fcn_params 
;;

let count_returns fcn_stmt =
  let num_rets = ref 0 in 
  let finder = 
    object
      inherit [_] s_iter as super

      method ! visit_SRet () _ = 
        num_rets := !num_rets + 1;
    end
  in 
  finder#visit_statement () fcn_stmt;
  !num_rets


let single_return fcn_stmt = 
  (count_returns fcn_stmt) = 1
;;

let no_return fcn_stmt = 
  (count_returns fcn_stmt) = 0
;;


(* replace each parameter with its argument. This should only be caled 
   for functions with read_only_params! *)
let replace_params_with_args stmt (params:(id * ty) list) (args: exp list) = 
  let subst =
    object
      inherit [_] s_map as super

      method! visit_EVar (env : e IdMap.t) x =
        match IdMap.find_opt (Cid.to_id x) env with
        | Some e -> e
        | None -> EVar x
    end
  in 
  (* create map from id (param) to expression (arg) *)
  let id_exp_map = List.fold_left 
    (fun idmap (param, arg) ->  
      IdMap.add (fst param) arg.e idmap)
    IdMap.empty
    (List.combine params args)
  in 
  (* do all the substitutions *)
  subst#visit_statement id_exp_map stmt
;;

(* replace return statements with assignments and delete expression-less returns. 
   This should only be called for function call assignments! *)
let replace_return_with_assign id stmt = 
  let ret_subst =
    object
      inherit [_] s_map as super

      method! visit_SRet (id : Id.t) exp_opt =
        match exp_opt with 
          | None -> SNoop
          | Some exp -> 
            SAssign (id,exp)
    end
  in
  ret_subst#visit_statement id stmt
;;
let replace_return_with_local id ty stmt = 
  let ret_subst =
    object
      inherit [_] s_map as super

      method! visit_SRet (id : Id.t) exp_opt =
        match exp_opt with 
          | None -> SNoop
          | Some exp -> 
            SLocal (id,ty,exp)
    end
  in
  ret_subst#visit_statement id stmt
;;
(* Inline functions that have multiple return statements 
   and immutable variables wherever possible. *)
let inline_multi_return_immutable (ds:decl list) ((fcn_id:Id.t),(fcn_params : params),fcn_stmt)  = 
  match (read_only_params fcn_params fcn_stmt) with 
  | true -> 
    (* print_endline (("[inline_multi_return_immutable] all parameters immutable for function:"^(Id.to_string fcn_id))); *)
    let assign_subst = 
      object
        inherit [_] s_map as super
        method! visit_SAssign (fcn_id, fcn_params, fcn_stmt) id exp = 
          (* print_endline ("[inline_multi_return_immutable] visiting assign."); *)
          (* Printing.exp_to_string exp |> print_endline; *)
          match exp.e with 
          | ECall(cid, args) -> (
            (* print_endline ("[inline_multi_return_immutable] call in assign: "^(Cid.to_string cid)); *)
            match (Cid.equal (Cid.id fcn_id) cid) with 
            | true ->
              (* an assignment that calls a function with read only params. 
                we can inline this with zero overhead. *)
              (* step 1: replace each parameter in the function body with 
                         the appropriate argument. *)
              let fcn_stmt = replace_params_with_args fcn_stmt fcn_params args in 
              (* step 2: replace each return statement with an assignment to 
                         the caller's variable. *)
              let fcn_stmt = replace_return_with_assign id fcn_stmt in 
              (* step 3: return the transformed function body instead of the 
                         call. *)
              fcn_stmt.s
            | false -> SAssign(id, exp) (* call to different function *)
          )
          (* not a call *)
          | _ -> SAssign(id, exp)      
      end
    in 
    assign_subst#visit_decls (fcn_id, fcn_params, fcn_stmt) ds
  | false -> 
    (* print_endline ("[inline_multi_return_immutable] cannot inline -- mutable parameter."); *)
  ds (* some of the parameters in the function are written *)
;;

(* Inline functions that have one return statement and immutable variables 
   anywhere they appear. *)
let inline_no_return_or_single_return_immutable (ds:decl list) ((fcn_id:Id.t),(fcn_params : params),fcn_stmt)  = 
  match ((read_only_params fcn_params fcn_stmt) && ((single_return fcn_stmt) || (no_return fcn_stmt))) with 
  | true -> 
    let assign_subst = 
      object
        inherit [_] s_map as super
        method! visit_statement (fcn_id, fcn_params, fcn_stmt) stmt = 
          let stmt = match stmt.s with 
            | SAssign(id, exp) -> (
              match exp.e with 
                | ECall(cid, args) -> (
                  match (Cid.equal (Cid.id fcn_id) cid) with 
                  | true -> 
                    let fcn_stmt = replace_params_with_args fcn_stmt fcn_params args in 
                    replace_return_with_assign id fcn_stmt
                  | false -> stmt
                )
                | _ -> stmt
              )
            | SLocal(id, ty, exp) -> (
              match exp.e with 
                | ECall(cid, args) -> (
                  match (Cid.equal (Cid.id fcn_id) cid) with 
                  | true -> 
                    let fcn_stmt = replace_params_with_args fcn_stmt fcn_params args in 
                    replace_return_with_local id ty fcn_stmt
                  | false -> stmt
                )
                | _ -> stmt
              )
            (* for unit calls to functions with read only params, we can just 
               replace params and be done. *)
            | SUnit(exp) -> (
              match exp.e with 
                | ECall(cid, args) -> (
                  match (Cid.equal (Cid.id fcn_id) cid) with 
                  | true -> 
                    let fcn_stmt = replace_params_with_args fcn_stmt fcn_params args in 
                    fcn_stmt
                  | false -> stmt
                )
                | _ -> stmt
            )
            | _ -> stmt
          in 
          super#visit_statement (fcn_id, fcn_params, fcn_stmt) stmt
      end
    in 
    assign_subst#visit_decls (fcn_id, fcn_params, fcn_stmt) ds
  | false -> ds (* some of the parameters in the function are written *)
;;

(* zero-overhead inlining pass for a certain subset of functions / calls. *)
let inline_prog_specialcase ds =
  (* try zero-overhead inlining all function calls in assignment 
     statements. To be eligible, the function call's parameters must 
     be immutable.*)
  let ds = List.fold_left inline_multi_return_immutable ds (dfuns ds) in 
  (* all functions with 1 return statement and immutable parameters can 
  be inlined with 0 overhead, regardless of where they appear. *)
  let ds = List.fold_left inline_no_return_or_single_return_immutable ds (dfuns ds) in 
  (* finally, inline all function calls in unit statements with immutable parameters *)
  ds
;;