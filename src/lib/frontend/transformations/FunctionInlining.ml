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

(* Given values for each size variable, go through and replace all sizes with
   constant values. Needed so we know how much to unroll comprehensions. *)
let subst_size =
  object (self)
    inherit [_] s_map as super

    method! visit_size (env : int IdMap.t) sz =
      match sz with
      | IConst _ -> sz
      | IUser cid -> IConst (IdMap.find (Cid.to_id cid) env)
      | ISum (sizes, n) ->
        ISum (List.map (self#visit_size env) sizes, n)
        |> SyntaxUtils.normalize_size
      | IVar (QVar _) -> failwith "Found QVar during subst_size"
      | IVar (TVar { contents = Link sz }) -> self#visit_size env sz
      | IVar (TVar { contents = Unbound (id, _) }) ->
        (* This case relies on the fact that we don't change the id when we
            generalize *)
        IConst (IdMap.find id env)
  end
;;

let subst_extract env sz =
  match subst_size#visit_size env sz with
  | IConst n -> n
  | _ -> failwith "Impossible"
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

let subst_index =
  object
    inherit [_] s_map

    method! visit_IUser (target, sz) cid =
      if Cid.equal target cid then sz else IUser cid
  end
;;

(* Check if a variable is assigned to in s. We sometimes need to handle
   parameters differently if they're assigned to. *)
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
type call = cid * exp list * cid

let default_expression ty =
  let rec aux rty =
    match rty with
    | TInt size -> eint (Z.of_int 32) (Some size)
    | TBool -> value_to_exp (vbool false)
    | TVector (raw_ty, size) ->
      let sz =
        match size with
        | IConst n -> n
        | _ -> err @@ "Somehow cannot determine size during function inlining"
      in
      vector_sp (List.init sz (fun _ -> aux raw_ty)) Span.default
    | TRecord lst ->
      record_sp (List.map (fun (s, raw_ty) -> s, aux raw_ty) lst) Span.default
    | _ ->
      err
      @@ "Only functions which return ints, bools, or records or vectors of \
          such can be inlined"
  in
  aux ty.raw_ty
;;

(* True for exps which would be computed multiple
   times if we substituted them into function bodies directly *)
let rec is_compound e =
  match e.e with
  | EInt _ | EVal _ | EVar _ | ESizeCast _ -> false
  | EHash _ | EOp _ | ECall _ | EStmt _ -> true
  | EComp (e, _, _) | EIndex (e, _) | EProj (e, _) -> is_compound e
  | EVector entries | ETuple entries -> List.exists is_compound entries
  | ERecord entries -> List.exists (is_compound % snd) entries
  | EWith (base, entries) ->
    is_compound base || List.exists (is_compound % snd) entries
;;

(* Given a function call, the values for its arguments, and the name of its
   return variable, create a block of code containing the body of the code with
   argument expressions inlined, and which sets the return variable to the
   return value (if there is one) *)
let rec inline_call env (call : call) =
  let fid, arg_exps, retvar = call in
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
        if is_compound e || is_assigned id body
        then (
          (* let new_id = Id.fresh (Id.name id) in *)
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
        slocal
          (Cid.to_id retvar)
          ret_ty
          (default_expression (subst_size#visit_ty env.sizes ret_ty))
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

and inline_calls funcs (calls : call list) base =
  List.fold_right (fun c acc -> sseq (inline_call funcs c) acc) calls base

(* This function collects a list of all user-defined function calls
   within the expression, creates new variables to contain their return value,
   and replaces the calls with the new variable. It returns the list of calls
   and their associated return variables *)
and inline_exp env e : call list * exp =
  match e.e with
  | ECall (cid, es) ->
    let calls, es = inline_exps env es in
    if CidMap.mem cid env.funcs
    then (
      (* Generate a fresh retvar *)
      let retvar = freshen cid "_ret" in
      calls @ [cid, es, retvar], { e with e = EVar retvar })
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
        (fun (_, pty) e -> TyperUnify.unify_ty e.espan pty (Option.get e.ety))
        params
        es;
      let calls', body = inline_exp env body in
      let subst_map =
        List.fold_left2
          (fun acc id arg -> IdMap.add id arg acc)
          IdMap.empty
          (List.map fst params)
          (List.map (fun e -> e.e) es)
      in
      calls @ calls', subst#visit_exp subst_map body)
    else
      ( (* Not user-defined function; we don't have to inline it *)
        calls
      , { e with e = ECall (cid, es) } )
  | EOp (op, es) ->
    let calls, es = inline_exps env es in
    calls, { e with e = EOp (op, es) }
  | EHash (size, es) ->
    let calls, es = inline_exps env es in
    calls, { e with e = EHash (size, es) }
  | EProj (e1, l) ->
    let calls, e1 = inline_exp env e1 in
    calls, { e with e = EProj (e1, l) }
  | ERecord entries ->
    let calls, entries =
      let ls, es = List.split entries in
      let calls, es = inline_exps env es in
      calls, List.combine ls es
    in
    calls, { e with e = ERecord entries }
  | ETuple entries ->
    let calls, entries = inline_exps env entries in
    calls, { e with e = ETuple entries }
  | EWith (base, entries) ->
    let calls, base = inline_exp env base in
    let calls, entries =
      let ls, es = List.split entries in
      let calls', es = inline_exps env es in
      calls @ calls', List.combine ls es
    in
    calls, { e with e = EWith (base, entries) }
  | EIndex (e1, n) ->
    let calls, e1 = inline_exp env e1 in
    calls, { e with e = EIndex (e1, n) }
  | EVector entries ->
    let calls, entries = inline_exps env entries in
    calls, { e with e = EVector entries }
  | EComp (e1, i, k) ->
    let k = subst_extract env.sizes k in
    let subst_idx n = subst_index#visit_exp (Id i, IConst n) e1 in
    let vec = { e with e = EVector (List.init k subst_idx) } in
    inline_exp env vec
  | EStmt (s, e1) ->
    let calls, e1' = inline_exp env e1 in
    calls, { e with e = EStmt (inline_stmt env s, e1') }
  | EVal _ | EVar _ | EInt _ | ESizeCast _ -> [], e

and inline_exps funcs es =
  let calls, es' = List.split @@ List.map (inline_exp funcs) es in
  List.flatten calls, es'

and inline_stmt env s =
  let inline_calls = inline_calls env in
  let inline_exp = inline_exp env in
  let inline_stmt = inline_stmt env in
  match s.s with
  | SNoop | SRet None -> s
  | SUnit e ->
    let calls, e' = inline_exp e in
    let base =
      match (Option.get e'.ety).raw_ty with
      | TVoid -> snoop
      | _ -> { s with s = SUnit e' }
    in
    inline_calls calls base
  | SLocal (id, ty, e) ->
    let calls, e' = inline_exp e in
    inline_calls calls { s with s = SLocal (id, ty, e') }
  | SAssign (id, e) ->
    let calls, e' = inline_exp e in
    inline_calls calls { s with s = SAssign (id, e') }
  | SGen (b, e) ->
    let calls, e' = inline_exp e in
    inline_calls calls { s with s = SGen (b, e') }
  | SRet (Some e) ->
    let calls, e' = inline_exp e in
    inline_calls calls { s with s = SRet (Some e') }
  | SPrintf (str, es) ->
    let calls, es' = inline_exps env es in
    inline_calls calls { s with s = SPrintf (str, es') }
  | SSeq (s1, s2) -> { s with s = SSeq (inline_stmt s1, inline_stmt s2) }
  | SIf (e, s1, s2) ->
    let calls, e' = inline_exp e in
    inline_calls calls { s with s = SIf (e', inline_stmt s1, inline_stmt s2) }
  | SMatch (es, branches) ->
    let calls, es' = inline_exps env es in
    let branches' = List.map (fun (ps, s) -> ps, inline_stmt s) branches in
    inline_calls calls { s with s = SMatch (es', branches') }
  | SLoop (s, i, k) -> { s with s = SLoop (inline_stmt s, i, k) }
;;

let inline_body env (params, body) = params, inline_stmt env body

let inline_decl env d =
  match d.d with
  | DFun (id, ty, _, body) ->
    { env with funcs = CidMap.add (Id id) (ty, body) env.funcs }, None
  | DHandler (id, body) ->
    let body =
      TyperInstGen.instantiator#visit_body (TyperInstGen.fresh_maps ()) body
    in
    env, Some { d with d = DHandler (id, inline_body env body) }
  | DConstr (id, _, params, e) ->
    { env with constrs = CidMap.add (Id id) (params, e) env.constrs }, None
  | DGlobal (id, ty, e) ->
    let e' =
      match inline_exp env e with
      | [], e' -> e'
      | _ ->
        Console.error_position
          d.dspan
          "Found non-constructor function call during global initialization"
    in
    env, Some { d with d = DGlobal (id, ty, e') }
  | DSize (id, sz) ->
    let n = subst_extract env.sizes sz in
    { env with sizes = IdMap.add id n env.sizes }, Some d
  | DUserTy _ | DExtern _ | DEvent _ | DConst _ | DGroup _ ->
    (* We can't inline an exp that's not part of a statement *) env, Some d
  | DMemop _ ->
    (* No function calls allowed in Memops *)
    env, Some d
  | DModule _ -> failwith "Modules should be eliminated before inlining"
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
