(* sanity test: how hard is it to type check CCore? *)
open Collections
open CCoreSyntax
open Batteries

exception TypeMismatch of ty * ty
exception UnboundVariable of cid
exception LengthMismatch of int * int
exception UnboundField of id
exception InvalidSize of size
exception UnboundType of cid
exception SizeMismatch of size * size
exception SelfRefSize of size
exception TypeError of string



type constr_var = 
  | IVar of string
  | IVal of int
type constr = 
  | Eq of string * constr_var


  
type env =
{
  vars : ty CidMap.t;
  tys  : ty CidMap.t;
  idx_constrs : constr list;
  ret_ty : ty option;
  (* tys  : ty CidMap.t; *)
  (* list_sizes : int option CidMap.t  *)
}
let empty_env = 
  {
    vars = CidMap.empty;
    tys = CidMap.empty;
    idx_constrs = [];
    ret_ty = None;
  }
;;

let env_to_string env = 
  let vars =  env.vars  |> CidMap.to_seq |> List.of_seq in
  let vars = List.map (fun (cid, ty) -> (Cid.to_string cid, CCorePPrint.ty_to_string ty)) vars in
  let vars = String.concat "\n" (List.map (fun (cid, ty) -> cid^" : "^ty) vars) in
  let vars_str = "vars:\n"^vars in
  let tys =  env.tys  |> CidMap.to_seq |> List.of_seq in
  let tys = List.map (fun (cid, ty) -> (Cid.to_string cid, CCorePPrint.ty_to_string ty)) tys in
  let tys = String.concat "\n" (List.map (fun (cid, ty) -> cid^" : "^ty) tys) in
  let tys_str = "tys:\n"^tys in
  vars_str^"\n"^tys_str
;;

let add_var env id ty = 
  {env with vars = CidMap.add id ty env.vars}
;;
let add_vars env ids tys = 
  List.fold_left2 add_var env ids tys
;;


let add_ty env cid ty = 
  {env with tys = CidMap.add cid ty env.tys}
let add_extern_ty env cid = 
  {env with tys = CidMap.add cid textern env.tys}
let get_ty env cid = 
  match CidMap.find_opt cid env.tys with 
  | Some ty -> ty
  | None -> raise (UnboundType cid)


let add_constr env constr = 
  {env with idx_constrs = constr::env.idx_constrs}


let ty_err str = raise(TypeError(str));;

(* unify just adds constraints to context for arrlenes *)

let rec unify_arrlen env x y =
  match x, y with 
  | IConst x, IConst y -> 
    if (x = y) then env
    else ty_err "array index mismatch"
  | IConst x, IVar v 
  | IVar v , IConst x ->
    add_constr env (Eq(fst v, (IVal x)))
  | IVar v, IVar w -> 
    add_constr env (Eq(fst v, (IVar (fst w))))
;;

let rec unify_lists env f_unify xs ys = 
  List.fold_left2 f_unify env xs ys 
;;

let rec unify_raw_ty env rawty1 rawty2 : env = 
  match rawty1, rawty2 with
  (* abstract types unify with their inner types *)
  | TAbstract(_, {raw_ty = ty1}), TAbstract(_, {raw_ty = ty2}) -> 
    unify_raw_ty env ty1 ty2
  | TAbstract(_, {raw_ty = ty1}), ty2 
  | ty1, TAbstract(_, {raw_ty = ty2}) -> 
    unify_raw_ty env ty1 ty2
  (* named types unify if their names and types are equal *)
  | TGlobal(t1), TGlobal(t2) -> 
    unify_ty env t1 t2
  | TName cid1, TName cid2 -> 
    if (not (Cid.equal cid1 cid2)) then 
      (ty_err "named types with different names");
    (* resolve and check the types, but skip if its extern *)
    if is_textern (ty@@TName(cid1)) then env
    else 
      let ty1 = get_ty env cid1 in
      let ty2 = get_ty env cid2 in
      unify_ty env ty1 ty2
  | TUnit, TUnit -> env
  | TBool, TBool -> env
  | TEvent, TEvent -> env
  | TEnum(variants1), TEnum(variants2) -> 
      let ids1, _ = List.split variants1 in
      let ids2, _ = List.split variants2 in
      if not (List.equal (Cid.equal) ids1 ids2) then 
        (raise (TypeError("enum types have different variants")));
    env
  | TBuiltin(cid1, tys1), TBuiltin(cid2, tys2) -> 
    if (not (Cid.equal cid1 cid2)) then 
      (ty_err "named types with different names");
    unify_lists env unify_ty tys1 tys2
  | TBits{ternary=t1; len=l1}, TBits{ternary=t2; len=l2} -> 
    if (t1 <> t2) then 
      (ty_err("pat type vs bitstring type"));
    if l1 <> l2 then 
      ty_err ("pat/bitstring types with different lengths");
    env
  | TInt l1, TInt l2 -> 
    if l1 <> l2 then 
      ty_err ("int types with different lengths");
    env
  | TRecord(l1, tys1), TRecord(l2, tys2) -> 
    if (List.length l1 <> List.length tys1) then 
      (ty_err ("invalid record type: number of fields and types differ"));
    if (List.length l2 <> List.length tys2) then 
      (ty_err ("invalid record type: number of fields and types differ"));
    if (List.length l1 <> List.length l2) then 
      (ty_err ("record types have different numbers of fields"));
    if (not (List.for_all2 Id.equal l1 l2)) then 
      (ty_err("fields of record type are not the same"));
    if (List.length tys1 <> List.length tys2) then 
      (ty_err("record types have different numbers of types"));        
    unify_lists env unify_ty tys1 tys2      
  | TTuple(tys1), TTuple(tys2) -> 
    if (List.length tys1 <> List.length tys2) then 
      (ty_err ("record types have different numbers types"));    
    unify_lists env unify_ty tys1 tys2      
  | TList(t1, l1), TList(t2, l2) ->
    let env' = unify_ty env t1 t2 in
    unify_arrlen env' l1 l2    
  | TFun{arg_tys=arg_tys1; ret_ty=ret_ty1; func_kind=fk1}, TFun{arg_tys=arg_tys2; ret_ty=ret_ty2; func_kind=fk2} -> 
    if (fk1 <> fk2) then 
      ty_err "functions of different kinds";
    let env' = unify_lists env unify_ty arg_tys1 arg_tys2 in
    let env'' = unify_ty env' ret_ty1 ret_ty2 in
    env''
  | (TUnit|TBool|TEvent|TInt _|TRecord _ | TTuple _ | TName _ | TGlobal _
    |TList (_, _)|TFun _|TBits _|TEnum _|TBuiltin (_, _)), _ -> 
      print_endline@@"type mismatch:\n"^(CCorePPrint.raw_ty_to_string rawty1)^"\nand\n"^(CCorePPrint.raw_ty_to_string rawty2);
      ty_err "type mismatch"

and unify_ty env ty1 ty2 : env = 
  unify_raw_ty env ty1.raw_ty ty2.raw_ty
;;

(* derive the type of a value with constant sizes *)
let rec infer_value value : value = 
  let type_value value = (infer_value value).vty in 
  let ty = match value.v with 
  (* values may only have have const sizes *)
  | VUnit -> tunit ()
  | VInt{size} -> tint size 
  | VBool _ -> tbool
  | VRecord(labels, es) -> 
    let ts = List.map type_value es in
    trecord labels ts
  | VTuple es -> 
    let ts = List.map type_value es in
    ttuple ts
  | VList vs -> 
    let ts = List.map type_value vs in
    tlist (List.hd ts) (IConst (List.length ts))
  | VBits{ternary; bits} -> ty@@TBits{ternary; len=sz@@List.length bits}
  | VEvent _ -> tevent
  | VSymbol(_, ty) -> ty
  in
  {value with vty=ty}
;;

let rec infer_lists env f_infer xs = 
  let infer_wrapper env_outs x = 
    let (env, outs) = env_outs in
    let env, out = f_infer env x in
    env, out::outs
  in
  let env, outs = List.fold_left infer_wrapper (env, []) xs in 
  env, List.rev outs
;;

let rec infer_exp env exp : env * exp = 
  print_endline ("inferring exp: "^(CCorePPrint.exp_to_string exp));
  let infer_exps env = infer_lists env infer_exp in
  let env, exp = match exp.e with 
    | EVal value -> 
      let ety = (infer_value value).vty in
      env, {e=EVal(value); ety; espan=exp.espan}
    | EVar cid -> 
      let ety = match CidMap.find_opt cid env.vars with
        | Some ty -> ty
        | None -> 
          print_endline ("current env:\n"^(env_to_string env));      
          ty_err@@"cannot find type for unbound variable: "^(CCorePPrint.cid_to_string cid)
      in
      env, {e=EVar cid; ety; espan=exp.espan}
    | EGlobalDeref(inner_exp) -> 
      let env, inf_inner_exp = infer_exp env inner_exp in
      (* inner exp should be a global, this is the inner type *)
      env, {e=EGlobalDeref(inner_exp); ety = extract_tglobal (inf_inner_exp.ety); espan = exp.espan}
    (* | ERecord{labels=None; es} ->        *)
    | ETuple(es) -> 
      let env, es' = infer_exps env es in
      let e = ETuple(es') in
      let ety = ttuple (List.map (fun exp -> exp.ety) es') in
      env, {e; ety; espan=exp.espan}
    | ERecord(labels, es) -> 
      let env, es' = infer_exps env es in
      let e =ERecord(labels, es') in
      let ety = trecord labels (List.map (fun exp -> exp.ety) es') in
      env, {e; ety; espan=exp.espan}
    | ECall{f; call_kind=CEvent} ->
      let env, inf_f = infer_exp env f in
      (* todo: this unify is kind of weird? *)
      if (is_tevent inf_f.ety) then unify_ty env exp.ety tevent, exp
      else ty_err "event call on non-event"
    | ECall{f; call_kind=CFun;} when (Cid.equal (fst (extract_evar f)) (Cid.create ["printf"]) )-> 

        (* TODO: hole for printf.
            options: 
            1. printf statement.
            2. printf externs, one for each call,  
               implemented as extern functions that just have 
               c code as the untyped body.
            3. a single untyped printf extern
            4. a printf op. Like hash. 
              - oh. This makes sense. Its similar to hash, 
                except it doesn't return anything.*)
        env, exp
  
      | ECall{f; args; call_kind=CFun;} -> 
      let env, inf_f = infer_exp env f in
      let param_tys, ret_ty, _ = extract_func_ty inf_f.ety in
      let env, inf_args = infer_lists env infer_exp args in
      let arg_tys = List.map (fun exp -> exp.ety) inf_args in
      let env = unify_lists env unify_ty param_tys arg_tys in
      (* TODO: update unify for list lengths *)
      let e = ECall{f=inf_f; args=inf_args; call_kind=CFun} in
      env, {e; ety=ret_ty; espan=exp.espan}
    | EOp(op, args) -> 
      let env, op, inf_args, ety = infer_eop env op args in
      let e = EOp(op, inf_args) in
      env, {exp with e; ety}
    | EListIdx(list_exp, idx_exp) -> 
      let env, inf_list_exp = infer_exp env list_exp in
      let env, inf_idx_exp = infer_exp env idx_exp in
      if not (is_tint inf_idx_exp.ety) then 
        ty_err "list index must be an int";
      if not (is_tlist inf_list_exp.ety) then 
        ty_err "expected a list";
      let cell_ty, arr_len = extract_tlist inf_list_exp.ety in  
      let _ = arr_len in
      let inf_ty = cell_ty in
      (* TODO: check inf_idx_exp against the length of inf_list_exp *)
      (* the index expression must either: 
         1. evaluate to a constant.
         2. be a for loop variable bound by a size less than or equal to the size of the array
         3. be a mod op.
            - hmm... could we track an optional "max value" on tints that we infer? 
              - then we could just check that...         
         *)
      let e = EListIdx(inf_list_exp, inf_idx_exp) in
      env, {exp with e; ety=inf_ty}
  in
  print_endline@@"finished inferring expression -- "^(CCorePPrint.exp_to_string exp)^" : "^(CCorePPrint.ty_to_string exp.ety);
  env, exp

(* derive the type for an operation expression *)
and infer_eop env op (args : exp list) : env * op * exp list * ty = match op, args with 
  | Not, [exp] 
  | Neg, [exp] -> (* shouldn't either not or neg be an arith/bitwise op? *)
    let env, inf_exp = infer_exp env exp in
    unify_ty env tbool inf_exp.ety, op, [inf_exp], tbool
  | And, [exp1; exp2] | Or, [exp1; exp2] ->
    let env, inf_exp1 = infer_exp env exp1 in
    let env, inf_exp2 = infer_exp env exp2 in
    let env = unify_ty env tbool inf_exp1.ety in
    let env = unify_ty env tbool inf_exp2.ety in
    env, op, [inf_exp1; inf_exp2], tbool
  | Eq, [exp1; exp2] | Neq, [exp1; exp2] -> 
    let env, inf_exp1 = infer_exp env exp1 in
    let env, inf_exp2 = infer_exp env exp2 in
    let env = unify_ty env inf_exp1.ety inf_exp2.ety in
    env, op, [inf_exp1; inf_exp2], tbool
  | Less, [exp1; exp2] | More, [exp1; exp2] | Leq , [exp1; exp2] | Geq, [exp1; exp2] ->
    let env, inf_exp1 = infer_exp env exp1 in
    let env, inf_exp2 = infer_exp env exp2 in
    if (not (is_tint inf_exp1.ety)) then 
      ty_err "int op with non-int arg";
    let env = unify_ty env inf_exp1.ety inf_exp2.ety in
    env, op, [inf_exp1; inf_exp2], tbool
  | Plus, [exp1; exp2] | Sub, [exp1; exp2] | SatPlus, [exp1; exp2] | SatSub, [exp1; exp2]
  | BitAnd, [exp1; exp2] | BitOr, [exp1; exp2] | BitXor, [exp1; exp2] | Mod, [exp1; exp2] -> 
    let env, inf_exp1 = infer_exp env exp1 in
    let env, inf_exp2 = infer_exp env exp2 in
    if (not (is_tint inf_exp1.ety)) then 
      ty_err "int op with non-int arg";
    let env = unify_ty env inf_exp1.ety inf_exp2.ety in
    env, op, [inf_exp1; inf_exp2], inf_exp1.ety
  | BitNot, [exp] -> 
    let env, inf_exp = infer_exp env exp in
    if (not (is_tint inf_exp.ety)) then 
      raise (TypeError("bitwise not on non-int"));
    env, op, [inf_exp], inf_exp.ety
  | LShift, [exp1; exp2] | RShift, [exp1; exp2] -> 
    let env, inf_exp1 = infer_exp env exp1 in
    let env, inf_exp2 = infer_exp env exp2 in
    if (not (is_tint inf_exp1.ety)) then 
      raise (TypeError("shift on non-int"));
    if (not (is_tint inf_exp2.ety)) then 
      raise (TypeError("shift by non-int"));
    let env = unify_ty env inf_exp1.ety inf_exp2.ety in
    env, op, [inf_exp1; inf_exp2], inf_exp1.ety
  | Slice(hi, lo), [exp] -> 
    let env, inf_exp = infer_exp env exp in
    if (not (is_tint inf_exp.ety)) then 
      raise (TypeError("slice on non-int"));
    let n_extracted_bits = hi - lo + 1 in
    if (n_extracted_bits <= 0) then 
      raise (TypeError("zero or negative number of bits extracted"));
    env, op, [inf_exp], tint n_extracted_bits
  | Conc, [exp1; exp2] -> 
    let env, inf_exp1 = infer_exp env exp1 in
    let env, inf_exp2 = infer_exp env exp2 in
    if (not (is_tint inf_exp1.ety)) then 
      raise (TypeError("concatenation of non-int"));
    if (not (is_tint inf_exp2.ety)) then 
      raise (TypeError("concatenation of non-int"));
    let size1 = extract_tint_size inf_exp1.ety in
    let size2 = extract_tint_size inf_exp2.ety in
    env, op, [inf_exp1; inf_exp2], tint (size1 + size2)
  | Cast(size), [exp] -> 
    let env, inf_exp = infer_exp env exp in
    if (not (is_tint inf_exp.ety)) then 
      raise (TypeError("cast of non-int"));
    env, op, [inf_exp], ty@@TInt size
  | Hash(size), _ -> 
    (* hash arguments can be anything *)
    env, op, args, ty@@TInt size
  | PatExact, [exp] -> 
    let env, inf_exp = infer_exp env exp in
    if (not (is_tint inf_exp.ety)) then 
      raise (TypeError("int-to-pat of non-int"));
    env, op, [inf_exp], tpat (extract_tint_size inf_exp.ety)
  | PatMask, [exp_val; exp_mask] -> 
    let env, inf_exp_val = infer_exp env exp_val in
    let env, inf_exp_mask = infer_exp env exp_mask in
    if (not (is_tint inf_exp_val.ety)) then 
      raise (TypeError("int-to-pat val of non-int"));
    if (not (is_tint inf_exp_mask.ety)) then 
      raise (TypeError("int-to-pat mask of non-int"));
    let env = unify_ty env inf_exp_val.ety inf_exp_mask.ety in
    env, op, [inf_exp_val; inf_exp_mask], tpat (extract_tint_size inf_exp_val.ety)
  | Project id, [exp] -> (
    let env, inf_exp = infer_exp env exp in
    if not (is_trecord inf_exp.ety) then (
      let ty1 = inf_exp.ety in
      let ty2 = trecord [id] [inf_exp.ety] in
      print_endline@@"type mismatch in project:\n"^(CCorePPrint.ty_to_string ty1)^"\nand\n"^(CCorePPrint.ty_to_string ty2);
      ty_err "type mismatch"
    );
    let inf_labels, inf_tys = extract_trecord inf_exp.ety in
    let labels_tys = List.combine inf_labels inf_tys in
    let inf_ty = List.assoc_opt id labels_tys in
    match inf_ty with
      | Some ty -> env, op, [inf_exp], ty
      | None -> raise (UnboundField id)
  )
  | Get idx, [exp] -> (
    let env, inf_exp = infer_exp env exp in
    if not (is_ttuple inf_exp.ety) then 
      raise (TypeMismatch(inf_exp.ety, ttuple [inf_exp.ety]));
    let inf_tys = extract_ttuple inf_exp.ety in
    match (List.nth_opt inf_tys idx) with
      | Some ty -> env, op, [inf_exp], ty
      | None -> raise (UnboundField (Id.create (string_of_int idx)))
  )
  | _,_-> ty_err "error type checking Eop"
;;

let rec infer_statement env (stmt:statement) = 
  print_endline ("inferring statement: "^(CCorePPrint.statement_to_string stmt));
  match stmt.s with 
  | SNoop -> env, stmt
  | SUnit(exp) -> 
    let env, inf_exp = infer_exp env exp in
    env, {stmt with s=SUnit(inf_exp)}
  | SAssign(OLocal(cid, ty), exp) -> 
    let env, inf_exp = infer_exp env exp in
    let env = unify_ty env ty inf_exp.ety in
    let env = add_var env cid ty in
    env, {stmt with s=SAssign(OLocal(cid, ty), inf_exp)}
  (* declare multiple variables, unpack tuple, assign to variables *)
  | SAssign(OTupleLocal(ids, tys), exp) -> 
    let env, inf_exp = infer_exp env exp in
    if (not@@is_ttuple inf_exp.ety) then 
      raise (TypeError("only tuples can be unpacked with a multi-assign"));
    let inf_exps = flatten_tuple inf_exp in
    if (List.length ids <> List.length inf_exps) then 
      raise (LengthMismatch(List.length ids, List.length inf_exps));
    let env = unify_lists env unify_ty tys (List.map (fun exp -> exp.ety) inf_exps) in
    let env = add_vars env ids tys in
    let inf_exp = {inf_exp with e=ETuple(inf_exps)} in
    env, {stmt with s=SAssign(OTupleLocal(ids, tys), inf_exp)}
  | SAssign(OTupleAssign(lexps), exp) ->
    let env, inf_lexps = infer_lists env infer_exp lexps in
    (* assigning to existing multiple variables from a tuple-type expression *)
    let env, inf_exp = infer_exp env exp in
    if (not@@is_ttuple inf_exp.ety) then 
      raise (TypeError("only tuples can be unpacked with a multi-assign"));
    let inf_exps = flatten_tuple inf_exp in
    if (List.length inf_lexps <> List.length inf_exps) then 
      raise (LengthMismatch(List.length inf_lexps, List.length inf_exps));
    (* make sure all the variables are already declared in the environment *)
    let env = unify_lists env unify_ty (List.map (fun exp -> exp.ety) inf_lexps) (List.map (fun exp -> exp.ety) inf_exps) in
    let inf_exp = {inf_exp with e=ETuple(inf_exps)} in
    env, {stmt with s=SAssign(OTupleAssign(inf_lexps), inf_exp)}
  | SAssign(OAssign(lexp), exp) ->
    let env, inf_lexp = infer_exp env lexp in
    let env, inf_exp = infer_exp env exp in
    let env = unify_ty env inf_lexp.ety inf_exp.ety in
    env, {stmt with s=SAssign(OAssign(lexp), exp)}
  | SSeq(stmt1, stmt2) -> 
    let env, inf_stmt1 = infer_statement env stmt1 in
    let env, inf_stmt2 = infer_statement env stmt2 in
    env, {stmt with s=SSeq(inf_stmt1, inf_stmt2)}
  | SIf(econd, stmt1, stmt2) -> 
    let env, inf_econd = infer_exp env econd in
    if (not@@is_tbool inf_econd.ety) then 
      raise (TypeError("if condition must be a boolean"));
    let env', inf_stmt1 = infer_statement env stmt1 in
    (* process block, restore vars, keep constraints *)
    let env = {env' with vars=env.vars} in
    let env', inf_stmt2 = infer_statement env stmt2 in
    let env = {env' with vars=env.vars} in
    env, {stmt with s=SIf(inf_econd, inf_stmt1, inf_stmt2)}
  | SMatch(exps, branches) -> 
    let infer_exps env = infer_lists env infer_exp in
    let env, inf_exps = infer_exps env exps in
    let rec infer_branches env branches =
      match branches with 
      | [] -> env, []
      | (pats, statement)::branches -> 
        if (List.length pats <> List.length exps) then 
          ty_err "wrong number of patterns for expressions in match statement";
        let env', statement = infer_statement env statement in
        let env = {env' with vars=env.vars} in
        let env, rest = infer_branches env branches in
        env, (pats, statement)::rest
    in
    let env', inf_branches = infer_branches env branches in
    let env = {env' with vars=env.vars} in

    env, {stmt with s=SMatch(inf_exps, inf_branches)}
  | SRet(Some(exp)) -> (
    let env, inf_exp = infer_exp env exp in
    match env.ret_ty with
    | Some ret_ty -> 
      let env = unify_ty env ret_ty inf_exp.ety in
      env, {stmt with s=SRet(Some(inf_exp))}
    | None -> 
      env, {stmt with s=SRet(Some(inf_exp))}
  )
  | SRet(None) -> env, stmt
  | SFor{idx; bound; stmt; guard} -> 
    (* add the index to the env *)
    let env = add_var env (Cid.id idx) (tint 32) in
    (* add the guard to the env *)
    let env = match guard with 
      | None -> env
      | Some(guard_id) -> add_var env (Cid.id guard_id) tbool
    in
    (* TODO: add constraint idx < bound --  only while inside of new environment? *)
    let env', inf_stmt = infer_statement env stmt in
    let env = {env' with vars=env.vars} in
    env, {stmt with s=SFor{idx; bound; stmt=inf_stmt; guard}}
  
  | SForEver stmt -> 
    let env, inf_stmt = infer_statement env stmt in
    env, {stmt with s=SForEver(inf_stmt)}
;;

let rec infer_decl env decl : env * decl = 
  print_endline ("inferring decl: ");
  print_endline (CCorePPrint.decl_to_string decl);
  match decl.d with 
  | DVar(id, ty, Some(arg)) -> 
    (* globals: the type of the constructor expression
       is t, BUT the declared type (of the variable) must be tglobal t *)
    if (is_tglobal ty <> true) then 
      ty_err "a toplevel variable must be declared with a global type";
    let env, inf_arg = infer_exp env arg in
    let env = unify_ty env ty (tglobal inf_arg.ety) in
    let env = add_var env (id) ty in
    env, {decl with d=DVar(id, ty, Some(inf_arg))}
  | DVar(id, ty, None) ->
    if ((is_tglobal ty) <> true) then 
      ty_err "globally scoped variables must be declared as globals";
    let env = add_var env (id) ty in
    env, decl
  | DList(id, ty, Some(args)) -> (
    let env, inf_args = infer_lists env infer_exp args in
    match ty.raw_ty with 
      | TList(cellty, len) -> 
        (* TODO later: constrain list's length based on length of args *)
        let _ = len in
        let env = List.fold_left (fun env inf_arg -> 
          unify_ty env cellty inf_arg.ety
        ) env inf_args in
        let env = add_var env (Cid.id id) ty in
        env, {decl with d=DList(id, ty, Some(inf_args))}
      | _ -> ty_err "list declaration with non-list type"
  )
  | DList(id, ty, None) ->
    let env = add_var env (Cid.id id) ty in
    env, decl
  | DTy(cid, Some(ty)) -> 
    let env = add_ty env cid ty in
    env, decl
  | DTy(cid, None) ->
    let env = add_extern_ty env cid in
    env, decl
  | DEvent{evconstrid} -> 
    (* just add the event type to the var_ty table *)
    let env = add_var env (Cid.id evconstrid) tevent in
    env, decl
  | DFun(fun_kind, id, ret_ty, params, Some(statement)) -> 
    (* set up the environment *)
    let outer_env = env in 
    let env = {env with ret_ty=Some(ret_ty)} in    
    let env = add_vars env (List.map (fun f -> f|> fst |> Cid.id) params) (List.map snd params) in
    (* check the statement *)
    let env, inf_stmt = infer_statement env statement in
    (* check the return type *)
    let inf_ret_ty = match env.ret_ty with 
      | Some ty -> ty
      | None -> tunit ()
    in
    (* return to outer env *)
    let env = outer_env in
    (* unify the return type *)
    let env = unify_ty env ret_ty inf_ret_ty in
    (* update the environment with the function *)
    let fun_ty = tfun_kind fun_kind (List.map snd params) ret_ty  in
    let env = add_var env (id) fun_ty in
    env, {decl with d=DFun(fun_kind, id, inf_ret_ty, params, Some(inf_stmt))}
  | DFun(fun_kind, id, ret_ty, params, None) -> 
    let fun_ty = tfun_kind fun_kind (List.map snd params) ret_ty  in
    let env = add_var env (id) fun_ty in
    env, decl
  | DFFun{fid; fparams; fret_ty;} -> 
    let fun_ty = tfun_kind FExtern (List.map snd fparams) fret_ty in
    let env = add_var env (fid) fun_ty in
    env, decl

and infer_decls env decls : env * decl list =
  match decls with
  | [] -> env, []
  | decl::decls -> 
    let env, inf_decl = infer_decl env decl in
    let env, inf_decls = infer_decls env decls in
    env, inf_decl::inf_decls

let check_decls decls = 
  snd@@infer_decls empty_env decls
;;
