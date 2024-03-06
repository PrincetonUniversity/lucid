(* sanity test: how hard is it to type check CCore? *)

open Collections
open CCoreSyntax
open Batteries

exception TypeMismatch of ty * ty
exception UnboundVariable of cid
exception LengthMismatch of int * int
exception UnboundField of id
exception InvalidSize of size
exception UnboundSize of size
exception SizeMismatch of size * size
exception SelfRefSize of size
exception TypeError of string


(* a map from sets of equivalent ids to int values *)
type idset_to_int = (IdSet.t * (int option)) list

(* merge the idsets that contain x and y together
(* check if an entry is a member of an entry  *)
let is_member x (idset, _) = IdSet.mem x idset ;; 
(* find the entry that contains x, if it exists *)
let find_entry_opt x idset_to_int = List.find_opt (is_member x) idset_to_int ;;
 *)
(* merge the entries for variables x and y together *)
let merge_entries (var_map : idset_to_int) (x : id) (y : id) : idset_to_int = 
  let rec merge_entries
    (found_entry :(IdSet.t * (int option)) option) 
    (x : id) 
    (y : id) 
    (var_map : idset_to_int) = 
    match var_map with 
    | [] -> (
      match found_entry with 
        | None -> 
        [(IdSet.add x IdSet.empty |> IdSet.add y), None]
        | Some(idset, int_opt) -> 
          [(IdSet.add x idset |> IdSet.add y), int_opt]
    )
    | entry::var_map -> (
      match found_entry with 
        | None -> (
          (* we found an idset for at least one of x or y -- 
              save and continue *)
          if (IdSet.mem x (fst entry) || IdSet.mem y (fst entry)) then 
            merge_entries (Some(entry)) x y var_map
          else 
            entry::(merge_entries None x y var_map)
        )
        | Some(found_entry) -> 
          (* an entry has already been found that has either x or y in it. 
              That entry has been deleted, so it has to be put somewhere. 
              Since this must be the second entry, we just put it here. *)
          let found_idset, found_int_opt = found_entry in
          let idset, int_opt = entry in
          match found_int_opt, int_opt with 
          (* if equal ints, merge idsets *)
          | Some(i1), Some(i2) -> if i1 == i2 
            then 
            ((IdSet.union idset found_idset), int_opt)::var_map
            else 
              raise (SizeMismatch(SVar(x), SVar(y)))
          (* if one int, use that *)
          | Some(i), None | None, Some(i) -> 
            ((IdSet.union idset found_idset), Some(i))::var_map
          (* if both non, keep unknown *)
          | None, None -> 
            ((IdSet.union idset found_idset), None)::var_map
    )
  in
  merge_entries None x y var_map
;;

let rec set_entry (var_map:idset_to_int) (var : Id.t) (size : int) : idset_to_int = 
  match var_map with 
  (* there is no alias entry for this var *)
  | [] -> [(IdSet.singleton var, Some(size))]
  | (idset, int_opt)::var_map when (IdSet.mem var idset) -> (
    match int_opt with 
    | Some(listed_size) -> (
      (* no change -- the var is already an alias for this size *)
      if (listed_size == size) then (idset, int_opt)::var_map
      (* error -- the var is listed as an alias for another size *)
      else raise (SizeMismatch(SConst(listed_size), SConst(size)))
    )
    (* the var is listed as an alias for an unknown size *)
    | None -> (idset, Some(size))::var_map
  )
  (* the var is not in this aliases entry *)
  | entry::var_map -> entry::(set_entry var_map var size)
;;


let rec sizevarmap_set (var_map:idset_to_int) (var:Id.t) (size:size) = 
  (* update the map so that var_id = size *)
  match size with 
    | SConst(i) -> set_entry var_map var i
    | SVar(var_alias) -> merge_entries var_map var var_alias 
;;

let rec sizevarmap_get (var_map : idset_to_int) (var : Id.t)  = 
  match var_map with 
  | [] -> (raise (UnboundSize(SVar(var))))
  | (idset, int_opt)::var_map -> 
      if (IdSet.mem var idset) then (
        match int_opt with 
      | None ->  (raise (UnboundSize(SVar(var))))
      | Some(i) -> i
      )
      else sizevarmap_get var_map var
  ;;
let sizevarmap_get_def var_map var = 
  try SConst(sizevarmap_get var_map var)
  with UnboundSize _ -> SVar(var)
;;


type env =
{
  vars : ty IdMap.t;
  tys  : ty CidMap.t;
  szs  : idset_to_int;
}

let env_sizevarmap_set env var_id var_val = 
  {env with szs = sizevarmap_set env.szs var_id var_val}
;;

(* check and unify sizes *)
let unify_size (env: env) sz1 sz2 : env = match sz1, sz2 with
  | SConst i1, SConst i2 -> 
    if (i1 == i2) then env else (raise (SizeMismatch(sz1, sz2)))
  | SVar sz_var, SConst sz
  | SConst sz, SVar sz_var -> env_sizevarmap_set env sz_var (SConst sz)
  | SVar sz1, SVar sz2 -> env_sizevarmap_set env sz1 (SVar sz2)
;;

(* check types and unify sizes within types that should be equal *)
let rec unify_lists env f_unify xs ys = 
  List.fold_left2 f_unify env xs ys 
;;

let ty_err str = raise(TypeError(str));;

let rec unify_raw_ty env rawty1 rawty2 : env = 
  match rawty1, rawty2 with
  | TUnit, TUnit -> env
  | TBool, TBool -> env
  | TEvent, TEvent -> env
  | TEnum(variants1), TEnum(variants2) -> 
      if not (List.equal (Id.equal) variants1 variants2) then 
        (raise (TypeError("enum types have different variants")));
    env
  | TName(cid1, tys1), TName(cid2, tys2) -> 
    if (not (Cid.equal cid1 cid2)) then 
      (ty_err "named types with different names");
    unify_lists env unify_ty tys1 tys2
  | TBits{ternary=t1; len=l1}, TBits{ternary=t2; len=l2} -> 
    if (t1 <> t2) then 
      (ty_err("pat type vs bitstring type"));
    unify_size env l1 l2  
  | TInt s1, TInt s2 -> unify_size env s1 s2
  | TRecord{labels=Some(l1); ts=tys1}, TRecord{labels=Some(l2); ts=tys2} -> 
    if (List.length l1 <> List.length l2) then 
      (ty_err ("record types have different numbers of fields"));
    if (not (List.for_all2 Id.equal l1 l2)) then 
      (ty_err("fields of record type are not the same"));
    if (List.length tys1 <> List.length tys2) then 
      (ty_err("record types have different numbers types"));    
    unify_lists env unify_ty tys1 tys2      
  | TRecord{labels=None; ts=tys1}, TRecord{labels=None; ts=tys2} -> 
    if (List.length tys1 <> List.length tys2) then 
      (ty_err ("record types have different numbers types"));    
    unify_lists env unify_ty tys1 tys2      
  | TList(t1, l1), TList(t2, l2) -> 
    let env' = unify_ty env t1 t2 in
    unify_size env' l1 l2 
  | TFun{arg_tys=arg_tys1; ret_ty=ret_ty1; func_kind=fk1}, TFun{arg_tys=arg_tys2; ret_ty=ret_ty2; func_kind=fk2} -> 
    (* size args don't matter at this point *)
    if (fk1 <> fk2) then 
      ty_err "functions of different kinds";
    let env' = unify_lists env unify_ty arg_tys1 arg_tys2 in
    let env'' = unify_ty env' ret_ty1 ret_ty2 in
    env''
  | (TUnit|TBool|TEvent|TInt _|TRecord _
    |TList (_, _)|TFun _|TBits _|TEnum _|TName (_, _)), _ -> ty_err "types do not match"

and unify_ty env ty1 ty2 : env = 
  unify_raw_ty env ty1.raw_ty ty2.raw_ty
;;

(* derive the type of a value with constant sizes *)
let rec infer_value value : value = 
  let type_value value = (infer_value value).vty in 
  let ty = match value.v with 
  (* values may only have have const sizes *)
  | VUnit -> tunit ()
  | VInt{size} -> (
      match size with
      | SConst size -> tint size
      | SVar _ -> raise (InvalidSize size)
    )
  | VBool _ -> tbool
  | VRecord{labels=None; es} -> 
    let ts = List.map type_value es in
    ttuple ts
  | VRecord{labels=Some labels; es} ->
    let ts = List.map type_value es in
    trecord labels ts
  | VList vs -> 
    let ts = List.map type_value vs in
    tlist (List.hd ts) (sz (List.length ts))
  | VBits{ternary; bits} -> ty@@TBits{ternary; len=sz@@List.length bits}
  | VEvent _ -> tevent
  | VEnum(_, ty) -> ty
  | VGlobal{global_ty} -> global_ty
  in
  {value with vty=ty}
;;





let refresh_size_var = 
  (* all the variables renamed so far *)
  let (id_map : id IdMap.t ref) = ref IdMap.empty in
  object (_)
    inherit [_] s_map as super
    method! visit_size () sz = 
      match sz with 
      | SVar id -> 
        (* use fresh name already chosen for this refresh, 
           or make a new one if none exists *)
        if (IdMap.mem id !id_map) then 
          SVar (IdMap.find id !id_map)
        else 
          let new_id = Id.refresh id in
          id_map := IdMap.add id new_id !id_map;
          SVar new_id
      | SConst _ -> sz
  end
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
(*  
Only need to check:
  - function calls
  - operations
  - assign statements
  - return statements   
*)


let rec infer_exp env exp : env * exp = 
  let infer_exps env = infer_lists env infer_exp in
  let env, exp = match exp.e with 
    | EVal value -> 
      let ety = (infer_value value).vty in
      let env = unify_ty env ety exp.ety in
      unify_ty env ety exp.ety, exp
    | EVar id -> 
      let ety = match IdMap.find_opt (Cid.to_id id) env.vars with
        | Some ty -> ty
        | None -> raise (UnboundVariable id)
      in
      unify_ty env ety exp.ety, exp
    | ERecord{labels=None; es} ->       
      let env, es' = infer_exps env es in
      let e = ERecord{labels=None; es=es'} in
      let ety = ttuple (List.map (fun exp -> exp.ety) es') in
      unify_ty env ety exp.ety, {exp with e} (* change exp because there are inner expressions *)
    | ERecord{labels=Some labels; es} -> 
      let env, es' = infer_exps env es in
      let e = ERecord{labels=Some(labels); es=es'} in
      let ety = ttuple (List.map (fun exp -> exp.ety) es') in
      unify_ty env ety exp.ety, {exp with e}
    | ECall{f; call_kind=CEvent} ->
      let env, inf_f = infer_exp env f in
      if (is_tevent inf_f.ety) then unify_ty env exp.ety tevent, exp
      else ty_err "event call on non-event"
    | ECall{f; args; call_kind=CFun;} -> 
      let _, inf_f = infer_exp env f in
      let _, arg_tys, ret_ty, _ = extract_func_ty inf_f.ety in
      (* the listed types may have size variables (as listd in arg_sizes), 
         if so, we want to refresh those sizes. *)
      (* we want each use of a function to be allowed different 
         concrete sizes for all of its size variables. So we refresh 
         the argument and return types before unification. 
         The size variables defined in the function body thus stay 
         as variables. *)
      let fresh_arg_tys = List.map (refresh_size_var#visit_ty ()) arg_tys in
      let fresh_ret_ty = refresh_size_var#visit_ty () ret_ty in

      (* infer the types of the arguments *)
      let env', inf_args = infer_exps env args in

      (* unify argument types with inferred *)
      let env' = unify_lists env' unify_ty fresh_arg_tys (List.map (fun exp -> exp.ety) inf_args) in
      (* unify return type with exp.ety *)
      let env'' = unify_ty env' fresh_ret_ty exp.ety in
      let e = ECall{f=inf_f; args=inf_args; call_kind=CFun} in
      let exp = {exp with e} in 
      env'', exp
    | EOp(op, args) -> 
      let env, op, inf_args, ety = infer_eop env op args in
      let e = EOp(op, inf_args) in
      env, {exp with e; ety}
  in
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
  | Less, [exp1; exp2] | More, [exp1; exp2] | Leq , [exp1; exp2] | Geq, [exp1; exp2] 
  | Plus, [exp1; exp2] | Sub, [exp1; exp2] | SatPlus, [exp1; exp2] | SatSub, [exp1; exp2]
  | BitAnd, [exp1; exp2] | BitOr, [exp1; exp2] | BitXor, [exp1; exp2] -> 
    let env, inf_exp1 = infer_exp env exp1 in
    let env, inf_exp2 = infer_exp env exp2 in
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
    let env = unify_ty env inf_exp1.ety inf_exp2.ety in
    if (not (is_tint inf_exp1.ety)) then 
      raise (TypeError("shift on non-int"));
    if (not (is_tint inf_exp2.ety)) then 
      raise (TypeError("shift by non-int"));
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
    let size1 = match extract_tint_size inf_exp1.ety with  
      | SConst i -> i
      | SVar _ -> ty_err "concat arguments must have concrete sizes"
    in
    let size2 = match extract_tint_size inf_exp2.ety with  
      | SConst i -> i
      | SVar _ -> ty_err "concat arguments must have concrete sizes"
    in
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
    let env = unify_ty env inf_exp_val.ety inf_exp_mask.ety in
    if (not (is_tint inf_exp_val.ety)) then 
      raise (TypeError("int-to-pat of non-int"));
    if (not (is_tint inf_exp_mask.ety)) then 
      raise (TypeError("mask of non-int"));
    env, op, [inf_exp_val; inf_exp_mask], tpat (extract_tint_size inf_exp_val.ety)
  | Project id, [exp] -> (
    let env, inf_exp = infer_exp env exp in
    if not (is_trecord inf_exp.ety) then 
      raise (TypeMismatch(inf_exp.ety, trecord [id] [inf_exp.ety]));
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
  | ListGet idx, [exp] -> (
    let env, inf_exp = infer_exp env exp in
    if not (is_tlist inf_exp.ety) then 
      raise (TypeMismatch(inf_exp.ety, tlist inf_exp.ety (idx)));
    let inf_ty, _ = extract_tlist inf_exp.ety in
    env, op, [inf_exp], inf_ty
  )
  | _,_-> ty_err "error type checking Eop"
;;

let rec infer_statement env (stmt:statement) = 
  match stmt.s with 
  | SNoop -> env, stmt
  | SUnit(exp) -> 
    let env, inf_exp = infer_exp env exp in
    env, {stmt with s=SUnit(inf_exp)}
  | SAssign{ids=[id]; tys=[ty]; new_vars=true; exp} -> 
    (* declaring a new variable *)
    let env, inf_exp = infer_exp env exp in
    let env = {env with vars = IdMap.add (Cid.to_id id) ty env.vars} in
    let env = unify_ty env ty inf_exp.ety in
    env, {stmt with s=SAssign{ids=[id]; tys=[ty]; new_vars=true; exp=inf_exp}}
  | SAssign{ids=[id]; new_vars=false; exp} -> 
    (* assigning to an existing variable *)
    let env, inf_exp = infer_exp env exp in
    let ty = match IdMap.find_opt (Cid.to_id id) env.vars with
      | Some ty -> ty
      | None -> raise (UnboundVariable id)
    in
    let env = unify_ty env ty inf_exp.ety in
    env, {stmt with s=SAssign{ids=[id]; tys=[ty]; new_vars=false; exp=inf_exp}}
  | SAssign{ids=ids; tys=tys; new_vars=true; exp} -> 
    (* declaring new multiple variables from a tuple-type expression *)
    let env, inf_exp = infer_exp env exp in
    if (not@@is_ttuple inf_exp.ety) then 
      raise (TypeError("only tuples can be unpacked with a multi-assign"));
    let inf_exps = flatten_tuple inf_exp in
    if (List.length ids <> List.length inf_exps) then 
      raise (LengthMismatch(List.length ids, List.length inf_exps));
    let env = List.fold_left2 (fun env id ty ->
      {env with vars = IdMap.add (Cid.to_id id) ty env.vars}
    ) env ids tys in
    let env = unify_lists env unify_ty tys (List.map (fun exp -> exp.ety) inf_exps) in
    let inf_exp = {inf_exp with e=ERecord{labels=None; es=inf_exps}} in
    env, {stmt with s=SAssign{ids=ids; tys=tys; new_vars=true; exp=inf_exp}}
  | SAssign{ids=ids; new_vars=false; exp} -> 
    (* assigning to existing multiple variables from a tuple-type expression *)
    let env, inf_exp = infer_exp env exp in
    if (not@@is_ttuple inf_exp.ety) then 
      raise (TypeError("only tuples can be unpacked with a multi-assign"));
    let inf_exps = flatten_tuple inf_exp in
    if (List.length ids <> List.length inf_exps) then 
      raise (LengthMismatch(List.length ids, List.length inf_exps));
    (* make sure all the variables are already declared in the environment *)
    let tys = List.map (fun id -> 
      match IdMap.find_opt (Cid.to_id id) env.vars with
      | Some ty -> ty
      | None -> raise (UnboundVariable id)
    ) ids in
    let env = unify_lists env unify_ty tys (List.map (fun exp -> exp.ety) inf_exps) in
    let inf_exp = {inf_exp with e=ERecord{labels=None; es=inf_exps}} in
    env, {stmt with s=SAssign{ids=ids; tys=tys; new_vars=false; exp=inf_exp}}
  | SListSet{arr; idx; exp} -> 
    let env, inf_arr = infer_exp env arr in
    let env, inf_exp = infer_exp env exp in
    if (not@@is_tlist inf_arr.ety) then 
      raise (TypeError("list set on non-list"));
    let inf_cell_ty, _ = extract_tlist inf_arr.ety in
    let env = unify_ty env inf_cell_ty inf_exp.ety in
    env, {stmt with s=SListSet{arr=inf_arr; idx; exp=inf_exp}}
  | SSeq(stmt1, stmt2) -> 
    let env, inf_stmt1 = infer_statement env stmt1 in
    let env, inf_stmt2 = infer_statement env stmt2 in
    env, {stmt with s=SSeq(inf_stmt1, inf_stmt2)}
  | SIf(econd, stmt1, stmt2) -> 
    let env, inf_econd = infer_exp env econd in
    if (not@@is_tbool inf_econd.ety) then 
      raise (TypeError("if condition must be a boolean"));
    let env, inf_stmt1 = infer_statement env stmt1 in
    let env, inf_stmt2 = infer_statement env stmt2 in
    env, {stmt with s=SIf(inf_econd, inf_stmt1, inf_stmt2)}
  | _ -> failwith " not done"


   
    
    



(* 

let infer_decl env decl : (env * decl) = 
  match decl.d with 
    | DVar(id, ty, [exp]) -> 
      let inf_ty = infer_


let ignore f ctx x = let _ = f ctx x in ()

let check_decl ctx decl = match decl.d with 
  | DVar(id, ty, exps) -> 
    List.iter (ignore check_exp ctx) exps;
    { ctx with vars = IdMap.add id ty ctx.vars }
  | _ -> failwith "todo"

    (* let ctx' = IdMap.add id ty ctx in *)
 *)
