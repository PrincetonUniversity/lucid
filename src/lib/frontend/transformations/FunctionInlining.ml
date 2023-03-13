(*
  Inline all function definitions in the program. Do the same for constructors.
  Unfortunately, due to a quirk of the syntax (namely, the fact that vector
  comprehensions contain expressions instead of statements), we have to unroll
  comprehensions at the same time as we inline functions.
*)

open Syntax
open Batteries
open Collections

let err s = Console.error s

let freshen cid suffix =
  let ids = Cid.names cid in
  let ids = List.modify_at (List.length ids - 1) (fun s -> s ^ suffix) ids in
  Cid.fresh ids
;;

(* Given a mapping of variables to expressions, return the input statement
   with each of those expressions substituted in for the corresponding
   variable. Not context-sensitive, so make sure none of the expressions
   include variables that might change between lines! *)
let subst =
  object
    inherit [_] s_map as super

    method! visit_EVar (env : e IdMap.t) x =
      match IdMap.find_opt (Cid.to_id x) env with
      | Some e -> e
      | None -> EVar x
  end
;;

(* Check if a variable is assigned to in s. We always need to make a new variable
   for a parameter if it's assigned to. *)
let is_assigned (id : Id.t) (s : statement) : bool =
  let ret = ref false in
  let v =
    object
      inherit [_] s_iter as super
      method! visit_SAssign _ id2 _ = if Id.equal id id2 then ret := true
    end
  in
  v#visit_statement () s;
  !ret
;;

(* Replace return statements with assignments to the return variable *)
let replace_returns (retvar : Id.t) (s : statement) : statement =
  let v =
    object (self)
      inherit [_] s_map as super

      method! visit_SRet dummy eopt =
        match eopt with
        | Some e -> SAssign (retvar, self#visit_exp dummy e)
        | None -> SNoop
    end
  in
  v#visit_statement () s
;;

type env =
  { funcs : (ty * body) CidMap.t
  ; constrs : (params * exp) CidMap.t
  ; sizes : int IdMap.t
  }

(* Function name, args, and return variable *)
type call =
  { fname : cid
  ; args : exp list
  ; ret : cid
  }

(* Given a function call, the values for its arguments, and the name of its
   return variable, create a block of code containing the body of the code with
   argument expressions inlined, and which sets the return variable to the
   return value (if there is one) *)
let rec inline_call inline_stmt env (call : call) =
  let fid, arg_exps, retvar = call.fname, call.args, call.ret in
  let ret_ty, params, body =
    let ret_ty, (params, body) = CidMap.find fid env.funcs in
    let maps = TyperInstGen.fresh_maps () in
    ( TyperInstGen.instantiator#visit_ty maps ret_ty
    , TyperInstGen.instantiator#visit_params maps params
    , TyperInstGen.instantiator#visit_statement maps body )
  in
  let args = List.combine params arg_exps in
  List.iter
    (fun ((_, pty), e) -> TyperUnify.unify_ty e.espan pty (Option.get e.ety))
    args;
  let body = inline_stmt env body in
  (* Replace any compound arguments with new intermediate variables, and include
     the creation statements for those variables *)
  let intermediate_vars, args =
    List.map
      (fun ((id, ty), e) ->
        if SyntaxUtils.is_compound e || is_assigned id body
        then (
          let new_id = id in
          Some (slocal new_id ty e), (id, EVar (Id new_id)))
        else None, (id, e.e))
      args
    |> List.split
  in
  (* Compute bindings for arguments and substitute them into the body *)
  let subst_map =
    List.fold_left (fun acc (id, arg) -> IdMap.add id arg acc) IdMap.empty args
  in
  let body = subst#visit_statement subst_map body in
  let body = replace_returns (Cid.to_id retvar) body in
  (* Add a declaration for the return variable if necessary *)
  let intermediate_vars =
    if ret_ty.raw_ty = TVoid
    then intermediate_vars
    else (
      let retvar_create =
        slocal (Cid.to_id retvar) ret_ty (SyntaxUtils.default_expression ret_ty)
      in
      intermediate_vars @ [Some retvar_create])
  in
  (* Finally, add all the variable declarations before the body *)
  List.fold_right
    (fun sopt acc ->
      match sopt with
      | None -> acc
      | Some s -> sseq s acc)
    intermediate_vars
    body
;;

let inliner =
  object (self)
    inherit [_] s_map as super

    method! visit_exp env e =
      match e.e with
      | ECall (cid, es) ->
        let es = List.map (self#visit_exp env) es in
        (* case: cid is a function *)
        if CidMap.mem cid env.funcs
        then (
          (* Generate a fresh retvar *)
          let retvar = freshen cid "_ret" in
          let ret_exp =
            if (Option.get e.ety).raw_ty = TVoid
            then { e with e = EVal (vbool false) }
            else { e with e = EVar retvar }
          in
          let stmt =
            inline_call
              self#visit_statement
              env
              { fname = cid; args = es; ret = retvar }
          in
          { e with e = EStmt (stmt, ret_exp) })
        (* case: cid is a constructor *)
        else if CidMap.mem cid env.constrs
        then (
          let params, body =
            let params, body = CidMap.find cid env.constrs in
            let maps = TyperInstGen.fresh_maps () in
            ( TyperInstGen.instantiator#visit_params maps params
            , TyperInstGen.instantiator#visit_exp maps body )
          in
          TyperUnify.unify_ty e.espan (Option.get e.ety) (Option.get body.ety);
          List.iter2
            (fun (_, pty) e ->
              TyperUnify.unify_ty e.espan pty (Option.get e.ety))
            params
            es;
          let subst_map =
            List.fold_left2
              (fun acc id arg -> IdMap.add id arg acc)
              IdMap.empty
              (List.map fst params)
              (List.map (fun e -> e.e) es)
          in
          (* create the body and re-annotate it immediately *)
          let body = GlobalConstructorTagging.reannotate_inlined_exp e body in
          (* recurse on the new body *)
          let inner_exp = self#visit_exp env body in 
          (* do substitutions on it *)
          let inlined_exp = subst#visit_exp subst_map inner_exp in
          inlined_exp
        )
        else
          (* Not user-defined function; we don't have to inline it *)
          { e with e = ECall (cid, es) }
      | _ -> 
        let res = super#visit_exp env e in
        res
  end
;;

let inline_body env (params, body) = params, inliner#visit_statement env body

(* Substitute into each decl as appropriate, collecting and removing DFun and
   DConstr declarations along the way. *)
let inline_decl env d =
  match d.d with
  (* Add to env, remove declaration *)
  | DFun (id, ty, _, body) ->
    { env with funcs = CidMap.add (Id id) (ty, body) env.funcs }, None
  | DConstr (id, _, params, e) ->
    { env with constrs = CidMap.add (Id id) (params, e) env.constrs }, None
    (* Substitute into body with current env *)
  | DHandler (id, s, body) ->
    let body =
      TyperInstGen.instantiator#visit_body (TyperInstGen.fresh_maps ()) body
    in
    let d' =
      TyperInstGen.generalizer#visit_decl
        ()
        { d with d = DHandler (id, s, inline_body env body) }
    in
    env, Some d'
  | DGlobal (id, ty, e) ->
    env, Some { d with d = DGlobal (id, ty, inliner#visit_exp env e) }
    (* Other stuff is unaffected *)
  | DUserTy _ | DExtern _ | DSymbolic _ | DEvent _ | DConst _ | DSize _ ->
    (* We can't inline an exp that's not part of a statement *) env, Some d
  | DMemop _ ->
    (* No function calls allowed in Memops *)
    env, Some d
  | DModule _ | DModuleAlias _ ->
    failwith "Modules should be eliminated before inlining"
  (* no function calls allowed in actions *)
  | DAction _ -> env, Some d
;;

let inline_prog ds =
  let _, ds' =
    List.fold_left
      (fun (funcs, ds) d ->
        let funcs', dopt = inline_decl funcs d in
        let ds' =
          match dopt with
          | Some d -> d :: ds
          | None -> ds
        in
        funcs', ds')
      ({ funcs = CidMap.empty; constrs = CidMap.empty; sizes = IdMap.empty }, [])
      ds
  in
  List.rev ds'
;;
