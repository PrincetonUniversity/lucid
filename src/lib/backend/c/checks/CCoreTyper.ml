(* sanity test: how hard is it to type check CCore? *)
open Collections
open CCoreSyntax
open Batteries
open CCoreExceptions
open CCoreBuiltinCheckers

let dprint_endline x = 
  if (Config.base_cfg.debug) then 
    print_endline x
  else ()
;;

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
  builtin_checkers : (cid * builtin_checker_t) list 
    (* checkers for builtin functions *)
  (* tys  : ty CidMap.t; *)
  (* list_sizes : int option CidMap.t  *)
}

let empty_env = 
  {
    vars = CidMap.empty;
    tys = CidMap.empty;
    idx_constrs = [];
    ret_ty = None;
    builtin_checkers = CCoreBuiltinCheckers.builtin_checkers;
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

let add_builtins env = 
  let params = CCoreBuiltinCheckers.builtin_vars () in
  let ids, tys = List.split params in
  let cids = List.map Cid.id ids in
  add_vars env cids tys
;;  
  
(* unify just adds constraints to context for arrlenes *)
let rec unify_arrlen env x y =
  match x, y with 
  | IConst x, IConst y -> 
    if (x = y) then env
    else ty_err "array index mismatch"
  | IConst x, IVar v 
  | IVar v , IConst x ->
    add_constr env (Eq(Cid.name v, (IVal x)))
  | IVar v, IVar w -> 
    add_constr env (Eq(Cid.name v, (IVar (Cid.name w))))
;;

let rec unify_lists env f_unify xs ys = 
  List.fold_left2 f_unify env xs ys 
;;

let rec unify_raw_ty env rawty1 rawty2 : env = 
  match rawty1, rawty2 with
  | TPtr(t1, None), TPtr(t2, None) ->
    unify_ty env t1 t2
  | TPtr(t1, Some(l1)), TPtr(t2, Some(l2)) ->
    let env' = unify_ty env t1 t2 in
    unify_arrlen env' l1 l2
  | TList(t1, l1), TList(t2, l2) ->
    let env' = unify_ty env t1 t2 in
    unify_arrlen env' l1 l2
  | TPacket, TPacket -> env
  | TName(cid1, _), TName(cid2, _) ->
    if (not (Cid.equal cid1 cid2)) then 
      (ty_err "named types with different names");
    (* resolve and check the types, but skip if its extern *)
    if is_textern (tname cid1) then env
    else
      let ty1 = get_ty env cid1 in
      let ty2 = get_ty env cid2 in
      unify_ty env ty1 ty2
  (* a named type unifies with a structural type by resolving the name to its
     definition -- e.g. a record literal checked against a named record type
     (mk_event's envelope), or a variant value against the named variant type *)
  | TName(cid, _), _ when not (is_textern (tname cid)) ->
    unify_raw_ty env (get_ty env cid).raw_ty rawty2
  | _, TName(cid, _) when not (is_textern (tname cid)) ->
    unify_raw_ty env rawty1 (get_ty env cid).raw_ty
  | TUnit, TUnit -> env
  | TBool, TBool -> env
  | TVariant _, TVariant _ -> env
  | TBuiltin(cid1, tys1), TBuiltin(cid2, tys2) -> 
    if (not (Cid.equal cid1 cid2)) then 
      (ty_err "named types with different names");
    unify_lists env unify_ty tys1 tys2
  | TInt l1, TInt l2 -> 
    if l1 <> l2 then 
      ty_err ("int types with different lengths ("^(string_of_int l1)^" vs "^(string_of_int l2));
    env
  | TRecord(l1, tys1), TRecord(l2, tys2) -> 
    if (List.length l1 <> List.length tys1) then 
      (ty_err ("invalid record type: number of fields and types differ"));
    if (List.length l2 <> List.length tys2) then 
      (ty_err ("invalid record type: number of fields and types differ"));
    if (List.length l1 <> List.length l2) then 
      (ty_err ("record types have different numbers of fields"));
    if (not (List.for_all2 Cid.equal l1 l2)) then 
      (ty_err("fields of record type are not the same"));
    if (List.length tys1 <> List.length tys2) then 
      (ty_err("record types have different numbers of types"));        
    unify_lists env unify_ty tys1 tys2      
  | TTuple(tys1), TTuple(tys2) -> 
    if (List.length tys1 <> List.length tys2) then 
      (ty_err ("record types have different numbers types"));    
    unify_lists env unify_ty tys1 tys2      
  | TFun{arg_tys=arg_tys1; ret_ty=ret_ty1; func_kind=fk1}, TFun{arg_tys=arg_tys2; ret_ty=ret_ty2; func_kind=fk2} -> 
    if (fk1 <> fk2) then 
      ty_err "functions of different kinds";
    let env' = unify_lists env unify_ty arg_tys1 arg_tys2 in
    let env'' = unify_ty env' ret_ty1 ret_ty2 in
    env''
  | TUnion(labels1, tys1), TUnion(labels2, tys2) -> 
    if (List.length labels1 <> List.length labels2) then 
      ty_err "union types with different numbers of labels";
    if (not (List.for_all2 Cid.equal labels1 labels2)) then 
      ty_err "union types with different labels";
    unify_lists env unify_ty tys1 tys2
  | (TUnit|TBool|TVariant _|TInt _|TRecord _ | TTuple _ | TName _ | TPtr _ | TList _ | TPacket | TUnion _
  | TFun _|TBuiltin (_, _)), _ ->
      dprint_endline@@"type mismatch:\n"^(CCorePPrint.raw_ty_to_string rawty1)^"\nand\n"^(CCorePPrint.raw_ty_to_string rawty2);
      ty_err "type mismatch"

and unify_ty env ty1 ty2 : env = 
  unify_raw_ty env ty1.raw_ty ty2.raw_ty
;;

(* derive the type of a value with constant sizes *)
let infer_value value : value =
  let ty = match value.v with
  (* values may only have have const sizes *)
  | VInt{size} -> tint size
  | VBool _ -> tbool
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
  dprint_endline ("inferring exp: "^(CCorePPrint.exp_to_string exp));
  let infer_exps env = infer_lists env infer_exp in
  let env, exp = match exp.e with 
    | EVal value -> 
      let ety = (infer_value value).vty in
      env, {exp with e=EVal(value); ety;}
    | EVar cid -> 
      let ety = match CidMap.find_opt cid env.vars with
        | Some ty -> ty
        | None -> 
          print_endline ("current env:\n"^(env_to_string env));      
          ty_err@@"cannot find type for unbound variable: "^(CCorePPrint.cid_to_string cid)
      in
      env, {exp with e=EVar cid; ety;}
    | EDeref(inner_exp) -> 
      let env, inf_inner_exp = infer_exp env inner_exp in
      if (is_tref inf_inner_exp.ety) then 
      (* inner exp should be a global, this is the inner type *)
      env, {exp with e=EDeref(inner_exp); ety = extract_tref (inf_inner_exp.ety); }
      else 
        ty_err (Printf.sprintf "tried to dereference a non reference type (%s : %s)" (CCorePPrint.exp_to_string inf_inner_exp) (CCorePPrint.ty_to_string inf_inner_exp.ety));
    (* | ERecord{labels=None; es} ->        *)
    | ETuple(es) ->
      let env, es' = infer_exps env es in
      let e = ETuple(es') in
      let ety = ttuple (List.map (fun exp -> exp.ety) es') in
      env, {exp with e; ety;}
    | EList(es) ->
      (* a fixed-length list literal: all elements must have the same type *)
      let env, es' = infer_exps env es in
      let ele_ty = (List.hd es').ety in
      let env = List.fold_left (fun env e -> unify_ty env ele_ty e.ety) env es' in
      env, {exp with e=EList(es'); ety=tlist ele_ty (IConst (List.length es'))}
    | ERecord(labels, es) -> 
      let env, es' = infer_exps env es in
      let e =ERecord(labels, es') in
      let ety = trecord (List.combine labels (List.map (fun exp -> exp.ety) es')) in
      env, {exp with e; ety;}
    | ECall{f; call_kind=CVariant} ->
      let env, inf_f = infer_exp env f in
      (* todo: this unify is kind of weird? *)
      if (is_tevent inf_f.ety) then unify_ty env exp.ety tevent_variant, exp
      else (
        dprint_endline ("current env: ");
        dprint_endline (env_to_string env);
        ty_err "event call on non-event")
    | ECall{f; call_kind=CFun;} when (Cid.equal (fst (extract_evar f)) (Cid.create ["printf"]) )-> 
        env, exp
    | ECall{f; args; call_kind=CFun;} -> (
      match CCoreBuiltinCheckers.get_checker f with 
      | Some(checker) -> 
        (* check the args, call the builtin checker, 
           use the return type given by the input checker *)
        let env, inf_args = infer_lists env infer_exp args in
        let exp = {exp with e=ECall{f=f; args=inf_args; call_kind=CFun}} in
        let inf_ety = checker None exp in
        env, {exp with ety=inf_ety}
      | None -> 
        let env, inf_f = infer_exp env f in
        let param_tys, ret_ty, _ = extract_func_ty inf_f.ety in
        let env, inf_args = infer_lists env infer_exp args in
        let arg_tys = List.map (fun exp -> exp.ety) inf_args in
        let env = try unify_lists env unify_ty param_tys arg_tys 
        with TypeError(str) -> 
          print_endline@@">>>> "^(CCorePPrint.exp_to_string exp)^"<<<< ";
          ty_err str
        in
        (* use the function types, in case they have names *)
        let args = List.map2 (fun exp ty -> {exp with ety=ty}) inf_args param_tys in

        (* TODO: update unify for list lengths *)
        let e = ECall{f=inf_f; args=args; call_kind=CFun} in
        env, {exp with e; ety=ret_ty}
    )
    | EOp(op, args) -> 
      let env, op, inf_args, ety = infer_eop env op args in
      let e = EOp(op, inf_args) in
      env, {exp with e; ety}
      (* TODO: for eop plus, if it is adding to a ref, we want to check the index expression:
         1. evaluate to a constant.
         2. be a for loop variable bound by a size less than or equal to the size of the array
         3. be a mod op.
            - hmm... could we track an optional "max value" on tints that we infer? 
              - then we could just check that...*)
  in
  dprint_endline@@"finished inferring expression -- "^(CCorePPrint.exp_to_string exp)^" : "^(CCorePPrint.ty_to_string exp.ety);
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
  | Plus, [exp1; exp2] -> (
    let env, inf_exp1 = infer_exp env exp1 in
    let env, inf_exp2 = infer_exp env exp2 in    
    match inf_exp1.ety.raw_ty with 
      | TPtr(_, _) -> 
        (* allow pointer arithmetic if t1 is a ref type. *)
        env, op, [inf_exp1; inf_exp2], inf_exp1.ety
      | _ -> 
        if (not (is_tint inf_exp1.ety)) then 
          ty_err "int op with non-int arg";
        let env = unify_ty env inf_exp1.ety inf_exp2.ety in
        env, op, [inf_exp1; inf_exp2], inf_exp1.ety
  )
  | Sub, [exp1; exp2] | SatPlus, [exp1; exp2] | SatSub, [exp1; exp2]
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

  | Cast(new_ty), [exp] when is_tint new_ty ->
    (* casting from int to int is allowed *)
    let env, inf_exp = infer_exp env exp in
    if (not (is_tint inf_exp.ety)) then 
      raise (TypeError("cast from int to non-int"));
    env, op, [inf_exp], new_ty
  | Cast(new_ty), [exp] when is_tref new_ty -> 
    (* casting from ref type to ref type is allowed *)
    let env, inf_exp = infer_exp env exp in
    if (not (is_tref inf_exp.ety)) then 
      raise (TypeError("cast from ref to non-ref"));
    env, op, [inf_exp], new_ty
  | Hash(size), args -> 
    (
    let infer_exps env = infer_lists env infer_exp in
    let env, inf_args = infer_exps env args in
    match inf_args with 
      | [seed; arg] -> 
        if (not@@is_tint seed.ety) then ty_err "first arg of hash must be an int seed";
        if (is_tref arg.ety) then ty_err "the hash of a reference is not allowed";
        env, op, inf_args, ty@@TInt size
      | _ -> ty_err "In the CCore backend, hash op takes a seed and 1 argument (which may be a compound)"
    )
  | Project id, [exp] -> (
    let env, inf_exp = infer_exp env exp in
    (* resolve a named record/union (e.g. projecting `ev.data` of the named
       envelope type) to its definition before reading the field *)
    let rec_ety = match inf_exp.ety.raw_ty with
      | TName(cid, _) when not (is_textern inf_exp.ety) -> get_ty env cid
      | _ -> inf_exp.ety
    in
    if not (is_trecord rec_ety or is_tunion rec_ety) then (
      let ty1 = rec_ety in
      let ty2 = trecord [id, rec_ety] in
      dprint_endline@@"type mismatch in project:\n"^(CCorePPrint.ty_to_string ty1)^"\nand\n"^(CCorePPrint.ty_to_string ty2);
      ty_err "type mismatch"
    );
    let inf_labels, inf_tys = extract_trecord_or_union (base_type rec_ety) in
    let inf_label_names = List.map Cid.name inf_labels in
    let labels_tys = List.combine inf_label_names inf_tys in
    let inf_ty = List.assoc_opt (Cid.name id) labels_tys in
    match inf_ty with
      | Some ty -> env, op, [inf_exp], ty
      | None -> 
        dprint_endline ("could not find: "^(Cid.to_string id));
        raise (UnboundField (Cid.to_id id))
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
  | Idx, [arr; idx] -> (
    let env, inf_arr = infer_exp env arr in
    let env, inf_idx = infer_exp env idx in
    if (not (is_tint inf_idx.ety)) then
      ty_err "vector index must be an int";
    if (not (is_tlist inf_arr.ety)) then
      ty_err "indexed expression is not a vector/array";
    let elem_ty = extract_tlist inf_arr.ety |> fst in
    env, op, [inf_arr; inf_idx], elem_ty
  )
  | Peek read_ty, [bs] -> (
    let env, inf_bs = infer_exp env bs in
    if (not (is_tpacket inf_bs.ety)) then ty_err "peek expects a bytes argument";
    env, op, [inf_bs], read_ty
  )
  | Read read_ty, [bs] -> (
    let env, inf_bs = infer_exp env bs in
    if (not (is_tpacket inf_bs.ety)) then ty_err "read expects a bytes argument";
    env, op, [inf_bs], read_ty
  )
  | Skip _, [bs] -> (
    let env, inf_bs = infer_exp env bs in
    if (not (is_tpacket inf_bs.ety)) then ty_err "skip expects a bytes argument";
    env, op, [inf_bs], tunit
  )
  | BytesOk, [bs] -> (
    let env, inf_bs = infer_exp env bs in
    if (not (is_tpacket inf_bs.ety)) then ty_err "bytes_ok expects a bytes argument";
    env, op, [inf_bs], tbool
  )
  | Write write_ty, [bs; v] -> (
    let env, inf_bs = infer_exp env bs in
    let env, inf_v = infer_exp env v in
    if (not (is_tpacket inf_bs.ety)) then ty_err "write expects a bytes argument";
    let env = unify_ty env write_ty inf_v.ety in
    env, op, [inf_bs; inf_v], tunit
  )
  | _,_-> ty_err "error type checking Eop"
;;

let unify_pat env exp pat = 
  match pat with 
    | PVal v -> 
      let v_sz_opt = bitsizeof_ty v.vty in
      let e_sz_opt = bitsizeof_ty exp.ety in
      (if (v_sz_opt = None || e_sz_opt = None) 
        then ty_err"unsizeable"
        else if (v_sz_opt <> e_sz_opt)
          then ty_err@@"the expression"^(CCorePPrint.exp_to_string exp)^" and the pattern matched against it ("^(CCorePPrint.pat_to_string pat)^" have different types"
          );    
      env
    | PMask{width} ->
      if (bitsizeof_ty exp.ety <> Some width) then
        ty_err "the expression and the masked pattern matched against it have different widths";
      env
    | PWild ty -> unify_ty env ty exp.ety
    | PVariant{event_id} ->
      (* a variant pattern matches the variant (the envelope's `data`), not the
         envelope -- callers project `ev.data` before matching *)
      let ev_exp = evar event_id tevent_variant in
      unify_ty env ev_exp.ety exp.ety
let unify_pats env exps pats = 
  List.fold_left2 unify_pat env exps pats

let rec infer_statement env (stmt:statement) = 
  dprint_endline ("inferring statement: "^(CCorePPrint.statement_to_string stmt));
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
  (* declare multiple variables, unpack tuple, assign to variables. We type-check
     against the RHS's tuple *type* (its component types), NOT by destructuring the
     RHS expression -- the RHS may be a call (e.g. Table.lookup) that has no literal
     components to pair with. CCoreCForm lowers the unpack to a temp + projections. *)
  | SAssign(OTupleLocal(ids, tys), exp) ->
    let env, inf_exp = infer_exp env exp in
    let comp_tys = match (base_type inf_exp.ety).raw_ty with
      | TTuple ts -> ts
      | _ -> raise (TypeError("only tuples can be unpacked with a multi-assign")) in
    if (List.length ids <> List.length comp_tys) then
      raise (LengthMismatch(List.length ids, List.length comp_tys));
    let env = unify_lists env unify_ty tys comp_tys in
    let env = add_vars env ids tys in
    env, {stmt with s=SAssign(OTupleLocal(ids, tys), inf_exp)}
  | SAssign(OTupleAssign(lexps), exp) ->
    let env, inf_lexps = infer_lists env infer_exp lexps in
    (* assigning to existing multiple variables from a tuple-type expression *)
    let env, inf_exp = infer_exp env exp in
    let comp_tys = match (base_type inf_exp.ety).raw_ty with
      | TTuple ts -> ts
      | _ -> raise (TypeError("only tuples can be unpacked with a multi-assign")) in
    if (List.length inf_lexps <> List.length comp_tys) then
      raise (LengthMismatch(List.length inf_lexps, List.length comp_tys));
    (* make sure all the variables are already declared in the environment *)
    let env = unify_lists env unify_ty (List.map (fun exp -> exp.ety) inf_lexps) comp_tys in
    env, {stmt with s=SAssign(OTupleAssign(inf_lexps), inf_exp)}
  | SAssign(OAssign(lexp), exp) ->(
    let env, inf_lexp = infer_exp env lexp in
    let env, inf_exp = infer_exp env exp in
    try 
      let env = unify_ty env inf_lexp.ety inf_exp.ety in
      env, {stmt with s=SAssign(OAssign(lexp), exp)}
    with TypeError(str) -> 
      print_endline ("statement: "^(CCorePPrint.statement_to_string stmt));
      print_endline ("ty 1: "
        ^(CCorePPrint.ty_to_string ~use_abstract_name:true inf_lexp.ety)^" ty 2:"
        ^(CCorePPrint.ty_to_string ~use_abstract_name:true inf_exp.ety));
      raise(TypeError(str))
    ) 
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
        (* unify the patterns *)
        dprint_endline
          @@"unifying expressions <<"^(String.concat ", " (List.map CCorePPrint.exp_to_string exps))
          ^">> with pats <<"^(String.concat ", " (List.map CCorePPrint.pat_to_string pats));          
        let env = unify_pats env inf_exps pats in
        (* for all the patterns that are events, we need to bind parameter types in env *)
        let inner_env = 
          List.fold_left (fun env pat -> 
            match pat with 
            | PVariant{params} -> 
              let env = add_vars env (List.map (fun f -> f|> fst) params) (List.map snd params) in
              env
            | _ -> env
          ) env pats
        in
        let env', statement = infer_statement inner_env statement in
        (* unbind parameters now *)
        let env = {env' with vars=env.vars} in
        let env, rest = infer_branches env branches in
        env, (pats, statement)::rest
    in
    let env', inf_branches = infer_branches env branches in
    let env = {env' with vars=env.vars} in

    env, {stmt with s=SMatch(inf_exps, inf_branches)}
  | SRet(Some(exp)) -> (
    let env, inf_exp = infer_exp env exp in
    (* what _should_ the return type be, according to the environment *)
    let env_ret_ty = match env.ret_ty with 
      | Some ty -> ty
      | None -> tunit
    in
    let env = unify_ty env env_ret_ty inf_exp.ety in
    env, {stmt with s=SRet(Some({inf_exp with ety=env_ret_ty}))}
  )
  | SRet(None) -> env, stmt
  | SFor{idx; bound; stmt} -> 
    (* add the index to the env *)
    let env = add_var env (idx) (tint 32) in
    let env', inf_stmt = infer_statement env stmt in
    let env = {env' with vars=env.vars} in
    env, {stmt with s=SFor{idx; bound; stmt=inf_stmt}}
  
  | SForEver stmt -> 
    let env, inf_stmt = infer_statement env stmt in
    env, {stmt with s=SForEver(inf_stmt)}
;;

let rec infer_decl env decl : env * decl option = 
  (* dprint_endline ("inferring decl: ");
  dprint_endline (CCorePPrint.decl_to_string decl);
  dprint_endline ("current env: ");
  dprint_endline (env_to_string env); *)
  match decl.d with 
  | DVar(id, ty, Some(exp)) when is_tbuiltin_any ty -> (
    let constr_exp = extract_ecall exp |> fst in
    match (get_checker constr_exp) with 
    | None -> ty_err@@"checker for builtin constructor: "^(CCorePPrint.exp_to_string constr_exp)^" not found"
    | Some(checker) -> 
      let inf_ty = checker (Some(ty)) exp in
      let env = add_var env (id) inf_ty in
      let exp = {exp with ety=inf_ty} in
      env, {decl with d=DVar(id, inf_ty, Some(exp))} |> Option.some
  )
  | DVar(id, ty, Some(arg)) -> (
    match ty.raw_ty with 
    | TPtr(_, None) -> 
      (* if this a pointer to a single cell, the other side is a single value *)
      let env, inf_arg = infer_exp env arg in
      let env = unify_ty env ty (tref inf_arg.ety) in
      let env = add_var env (id) ty in
      env, {decl with d=DVar(id, ty, Some(inf_arg))} |> Option.some
    | TPtr(_, Some(_)) -> (
      (* if this is an array, the rhs must also be an array *)
      let env, inf_arg = infer_exp env arg in
      let env = unify_ty env ty inf_arg.ety in
      let env = add_var env (id) ty in
      env, {decl with d=DVar(id, ty, Some(inf_arg))} |> Option.some
    )
    | _ -> (
      (* anything else is fine too without a special case *)
      let env, inf_arg = infer_exp env arg in
      let env = unify_ty env ty inf_arg.ety in
      let env = add_var env (id) ty in
      env, {decl with d=DVar(id, ty, Some(inf_arg))} |> Option.some
    )
    (* | _ -> ty_err "a toplevel variable must be a reference / global" *)
  )
  | DVar(id, ty, None) -> (* extern variable, nothing to check *)
    let env = add_var env (id) ty in
    env, decl |> Option.some
  | DTy(cid, Some(ty)) -> 
    let env = add_ty env cid ty in
    (* register any variant constructors so CVariant calls / PVariant patterns
       resolve. generic over any TVariant DTy (today: the event variant): a
       constructor's type is the variant type itself, which is_tevent /
       is_tevent_variant accept via their `TVariant _` arm. base_type returns the
       bare TVariant here (a TName alias would resolve via tydefs). *)
    let env = match (base_type ty).raw_ty with
      | TVariant sigs ->
        List.fold_left
          (fun env (ctor, _, _) -> add_var env ctor (base_type ty))
          env sigs
      | _ -> env
    in
    env, decl |> Option.some
  | DTy(cid, None) ->
    let env = add_extern_ty env cid in
    env, decl |> Option.some
  | DFun(fun_kind, id, ret_ty, params, BStatement(statement)) -> 
    (* set up the environment *)
    let outer_env = env in 
    let env = {env with ret_ty=Some(ret_ty)} in    
    let env = add_vars env (List.map (fun f -> f|> fst) params) (List.map snd params) in
    (* check the body *)
    let _, inf_stmt = infer_statement env statement in
    (* return to outer env *)
    let env = outer_env in
    (* update the environment with the function, except for handlers, 
       because a handler is never referenced. *)
    let fun_ty = tfun_kind fun_kind (List.map snd params) ret_ty  in
    let env = if (fun_kind <> FHandler) 
      then add_var env (id) fun_ty
      else env
    in
    env, {decl with d=DFun(fun_kind, id, ret_ty, params, BStatement(inf_stmt))} |> Option.some
  | DFun(fun_kind, id, ret_ty, params, BForiegn _) -> 
    (* foreign body, typed signature *)
    let fun_ty = tfun_kind fun_kind (List.map snd params) ret_ty in
    let env = add_var env (id) fun_ty in
    env, decl |> Option.some
  | DForiegn _ -> env, decl |> Option.some
  | DEnum(pairs) ->
    (* each member is a named int constant *)
    let env = List.fold_left (fun env (cid, _) -> add_var env cid tenum_member) env pairs in
    env, decl |> Option.some

and infer_decls env decls : env * decl list =

  match decls with
  | [] -> env, []
  | decl::decls -> 
    let env, inf_decl_opt = infer_decl env decl in
    let env, inf_decls = infer_decls env decls in
    match inf_decl_opt with
    | Some(inf_decl) -> env, inf_decl::inf_decls
    | None -> env, inf_decls


(* let pervasives =  *)

let check decls = 
  snd@@infer_decls (add_builtins empty_env) decls
;;

