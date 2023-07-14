(* This is a best effort zero-overhead inlining pass. 
  It should be able to inline all calls to functions 
  in which parameters are not written. 
  The main transformer inlines each function
  one at a time with "inline_single_function".

  NOTES: 
  1.this pass creates slocal statements that have 
    the "PNoInitLocal" pragma, which indicates to 
    optimization passes in the backend that it is safe 
    to allocate the variable without initializing it.
  2.this pass is not aware of modules. If it is run 
    before module elimination, it may produce invalid 
    code when two modules have functions with the 
    same name but different bodies.
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

(* replace return statements with assignments and delete expression-less returns. *)
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
(* Inline calls to 1 function in a declaration
   Case stmt = assign(id, call(...)) -> 
    1. replace each parameter in the function body with the appropriate argument.
    2. replace each return statement with an assignment to id.
    3. return the transformed function body instead of stmt
   Case stmt = local(id, call(...)) ->
    0. construct a declaration statement for the return variable
    1. replace each parameter in the function body with the appropriate argument.
    2. replace each return statement with an assignment to the caller's variable.
    3. return the declaration statement + the transformed function body instead of stmt
   Case return(call(...)) ->
    * return statements might actually be the easiest to handle? *
    1. replace each parameter in the function body with the appropriate argument.
    2. return the transformed function body instead of the return statement.
   *)
let inline_single_fcn (decl:decl) ((fcn_id:Id.t),(fcn_params : params),fcn_stmt)  = 
  print_endline ("[inline_single_function] inlining function: "^(Id.to_string fcn_id));
  match (read_only_params fcn_params fcn_stmt) with 
  | true -> 
    print_endline (("[inline_single_function] all parameters immutable for function:"^(Id.to_string fcn_id)));
    let assign_subst = 
      object
        inherit [_] s_map as super

        method! visit_statement (fcn_id, fcn_params, fcn_stmt) stmt = 
          match stmt.s with 
          | SAssign(id, exp) -> (
            match exp.e with 
            | ECall(cid, args) when (Cid.equal (Cid.id fcn_id) cid) -> 
                print_endline ("[inline_single_function] inlining call: "^(Printing.stmt_to_string stmt));
                let fcn_stmt = replace_params_with_args fcn_stmt fcn_params args in 
                let fcn_stmt = replace_return_with_assign id fcn_stmt in 
                {stmt with s = fcn_stmt.s}
            | _ -> stmt
          )
          | SLocal(id, ty, exp) -> (
            match exp.e with 
            | ECall(cid, args) when (Cid.equal (Cid.id fcn_id) cid) -> 
                print_endline ("[inline_single_function] inlining call: "^(Printing.stmt_to_string stmt));
                let decl_stmt =  slocal_sp id ty (SyntaxUtils.default_expression ty) stmt.sspan in 
                let decl_stmt = {decl_stmt with spragmas = [PNoInitLocal]} in 
                let fcn_stmt = replace_params_with_args fcn_stmt fcn_params args in 
                let fcn_stmt = replace_return_with_assign id fcn_stmt in 
                let inlined_stmt = sseq decl_stmt fcn_stmt in 
                {stmt with s = inlined_stmt.s}
            | _ -> stmt
          )
          | SRet(Some(exp)) -> (
            match exp.e with 
            | ECall(cid, args) when (Cid.equal (Cid.id fcn_id) cid) -> 
                print_endline ("[inline_single_function] inlining call: "^(Printing.stmt_to_string stmt));
                let fcn_stmt = replace_params_with_args fcn_stmt fcn_params args in 
                {stmt with s = fcn_stmt.s}
            | _ -> stmt
          )
          (* other statements *)
          | _ -> super#visit_statement (fcn_id, fcn_params, fcn_stmt) stmt
      end
    in 
    assign_subst#visit_decl (fcn_id, fcn_params, fcn_stmt) decl
  | false -> 
    decl
;;


(* zero-overhead inlining pass for a certain subset of functions / calls. *)

let inline_prog_specialcase ds = 
  (* this is a wildly inefficient way to do this, but oh well optimize later. *)
  let ds' = List.fold_left
    (fun prev_decls decl -> 
      (* get the previous functions *)
      let prev_fcns = dfuns prev_decls in 
      (* inline each call to a previous function in this decl... *)
      let decl' = List.fold_left inline_single_fcn decl prev_fcns in
      (* thats it *)
      prev_decls@[decl']
      )
    []
    ds
  in
  ds'



(* older incorrect pass  *)


let inline_single_function (ds:decl list) ((fcn_id:Id.t),(fcn_params : params),fcn_stmt)  = 
  print_endline ("[inline_single_function] inlining function: "^(Id.to_string fcn_id));
  match (read_only_params fcn_params fcn_stmt) with 
  | true -> 
    print_endline (("[inline_single_function] all parameters immutable for function:"^(Id.to_string fcn_id)));
    let assign_subst = 
      object
        inherit [_] s_map as super

        method! visit_statement (fcn_id, fcn_params, fcn_stmt) stmt = 
          match stmt.s with 
          | SAssign(id, exp) -> (
            match exp.e with 
            | ECall(cid, args) when (Cid.equal (Cid.id fcn_id) cid) -> 
                print_endline ("[inline_single_function] inlining call: "^(Printing.stmt_to_string stmt));
                let fcn_stmt = replace_params_with_args fcn_stmt fcn_params args in 
                let fcn_stmt = replace_return_with_assign id fcn_stmt in 
                {stmt with s = fcn_stmt.s}
            | _ -> stmt
          )
          | SLocal(id, ty, exp) -> (
            match exp.e with 
            | ECall(cid, args) when (Cid.equal (Cid.id fcn_id) cid) -> 
                print_endline ("[inline_single_function] inlining call: "^(Printing.stmt_to_string stmt));
                let decl_stmt =  slocal_sp id ty (SyntaxUtils.default_expression ty) stmt.sspan in 
                let decl_stmt = {decl_stmt with spragmas = [PNoInitLocal]} in 
                let fcn_stmt = replace_params_with_args fcn_stmt fcn_params args in 
                let fcn_stmt = replace_return_with_assign id fcn_stmt in 
                let inlined_stmt = sseq decl_stmt fcn_stmt in 
                {stmt with s = inlined_stmt.s}
            | _ -> stmt
          )
          | SRet(Some(exp)) -> (
            match exp.e with 
            | ECall(cid, args) when (Cid.equal (Cid.id fcn_id) cid) -> 
                print_endline ("[inline_single_function] inlining call: "^(Printing.stmt_to_string stmt));
                let fcn_stmt = replace_params_with_args fcn_stmt fcn_params args in 
                {stmt with s = fcn_stmt.s}
            | _ -> stmt
          )
          (* other statements *)
          | _ -> super#visit_statement (fcn_id, fcn_params, fcn_stmt) stmt
      end
    in 
    assign_subst#visit_decls (fcn_id, fcn_params, fcn_stmt) ds
  | false -> 
  ds (* some of the parameters in the function are written *)
;;


let inline_prog_specialcase_old ds =
  (* bug: function bodies are not updated in the context. So this doesn't work for functions that 
     call functions! actually, its worse than that. Probably straight up broken. *)
  let ds = List.fold_left inline_single_function ds (dfuns ds) in 
  print_endline ("---- function inlining special case pass done ----");
  print_endline (Printing.decls_to_string ds);
  print_endline ("---- function inlining special case pass done ----");
  exit 0;
  ds
;;