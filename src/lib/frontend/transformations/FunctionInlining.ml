(*
  Inline all function definitions in the program
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
let subst (env : e IdMap.t) (s : statement) : statement =
  let v =
    object
      inherit [_] s_map as super

      (* Ideally I'd want to use the first argument here (of type 'env) instead
         of the env argument to the subst function, but I can't for the life of
         me figure out how to instantiate it as e IdMap.t *)
      method! visit_EVar _ x =
        match IdMap.find_opt (Cid.to_id x) env with
        | Some e -> e
        | None -> EVar x
    end
  in
  v#visit_statement () s
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

type funcs = (ty * body) CidMap.t
type call = cid * exp list * cid

(* This function collects a list of all user-defined function calls
   within the expression, creates new variables to contain their return value,
   and replaces the calls with the new variable. It returns the list of calls
   and their associated return variables *)
let rec inline_exp funcs e : call list * exp =
  match e.e with
  | ECall (cid, es) ->
    let calls, es = inline_exps funcs es in
    if not (CidMap.mem cid funcs)
    then
      (* Not user-defined function; we don't have to inline it *)
      calls, { e with e = ECall (cid, es) }
    else (
      (* Generate a fresh retvar *)
      let retvar = freshen cid "_ret" in
      calls @ [cid, es, retvar], { e with e = EVar retvar })
  | EOp (op, es) ->
    let calls, es = inline_exps funcs es in
    calls, { e with e = EOp (op, es) }
  | EHash (size, es) ->
    let calls, es = inline_exps funcs es in
    calls, { e with e = EHash (size, es) }
  | EProj (e, l) ->
    let calls, e = inline_exp funcs e in
    calls, { e with e = EProj (e, l) }
  | ERecord entries ->
    let calls, entries =
      let ls, es = List.split entries in
      let calls, es = inline_exps funcs es in
      calls, List.combine ls es
    in
    calls, { e with e = ERecord entries }
  | EVal _ | EVar _ | EInt _ -> [], e

and inline_exps funcs es =
  let calls, es' = List.split @@ List.map (inline_exp funcs) es in
  List.flatten calls, es'
;;

let default_expression ty =
  match ty.raw_ty with
  | TInt size -> eint (Z.of_int 32) (Some size)
  | TBool -> value_to_exp (vbool false)
  | _ -> err @@ "Only functions which return ints or bools can be inlined"
;;

(* True for exps which contain other exps, and hence would be computed multiple
   times if we substituted them into function bodies directly *)
let rec is_compound e =
  match e.e with
  | EInt _ | EVal _ | EVar _ | EProj _ -> false
  | EHash _ | EOp _ | ECall _ -> true
  | ERecord entries -> List.exists (is_compound % snd) entries
;;

(* Given a function call, the values for its arguments, and the name of its
   return variable, create a block of code containing the body of the code with
   argument expressions inlined, and which sets the return variable to the
   return value (if there is one) *)
let inline_call (funcs : funcs) (call : call) =
  let fid, arg_exps, retvar = call in
  let ret_ty, (params, body) = CidMap.find fid funcs in
  let args = List.combine params arg_exps in
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
  let body = subst subst_map body in
  let body = replace_returns (Cid.to_id retvar) body in
  (* Add a declaration for the return variable if necessary *)
  let intermediate_vars =
    if ret_ty.raw_ty = TVoid
    then intermediate_vars
    else (
      let retvar_create =
        slocal (Cid.to_id retvar) ret_ty (default_expression ret_ty)
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

let inline_calls (funcs : funcs) (calls : call list) base =
  List.fold_right (fun c acc -> sseq (inline_call funcs c) acc) calls base
;;

let rec inline_stmt funcs s =
  let inline_calls = inline_calls funcs in
  let inline_exp = inline_exp funcs in
  let inline_stmt = inline_stmt funcs in
  match s.s with
  | SNoop | SRet None -> s
  | SUnit e ->
    let calls, e' = inline_exp e in
    let base =
      match e'.e with
      | ECall _ ->
        { s with s = SUnit e' }
        (* No other exps are effectful, so no reason to keep them around *)
      | _ -> snoop
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
    let calls, es' = inline_exps funcs es in
    inline_calls calls { s with s = SPrintf (str, es') }
  | SSeq (s1, s2) -> { s with s = SSeq (inline_stmt s1, inline_stmt s2) }
  | SIf (e, s1, s2) ->
    let calls, e' = inline_exp e in
    inline_calls calls { s with s = SIf (e', inline_stmt s1, inline_stmt s2) }
  | SMatch (es, branches) ->
    let calls, es' = inline_exps funcs es in
    let branches' = List.map (fun (ps, s) -> ps, inline_stmt s) branches in
    inline_calls calls { s with s = SMatch (es', branches') }
;;

let inline_body funcs (params, body) = params, inline_stmt funcs body

let inline_decl funcs d =
  match d.d with
  | DFun (id, ty, _, body) ->
    CidMap.add (Id id) (ty, inline_body funcs body) funcs, None
  | DHandler (id, body) ->
    funcs, Some { d with d = DHandler (id, inline_body funcs body) }
  | DSize _ | DExtern _ | DEvent _ | DConst _ | DGroup _ | DGlobal _ ->
    (* We can't inline an exp by itself *) funcs, Some d
  | DMemop _ | DGlobalTy _ | DConstr _ ->
    (* No function calls allowed in Memops *)
    funcs, Some d
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
      (CidMap.empty, [])
      ds
  in
  List.rev ds'
;;
