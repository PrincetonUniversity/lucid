(* implementation of arrays *)
open CCoreSyntax
open CCoreTransformers

type ctx = {
  memops : (cid *(ty * params * statement)) list;
    (* return type, params, body *)
  arrays : (cid * (ty * int)) list;
    (* cell type, length *)
  generated_fcns : (cid * (cid * decl)) list;
}
let empty_ctx = {memops=[]; arrays=[]; generated_fcns=[];}

let get_array ctx arr = 
  let arr_id = arr |> eval_exp |> extract_vsymbol in
  arr_id, List.assoc arr_id ctx.arrays
;;
let get_memop ctx memop_exp = 
  let memop_id = eval_exp memop_exp |> extract_vsymbol in
  memop_id, List.assoc memop_id ctx.memops
;;
let emplace_fcn ctx ((cid : Cid.t), decl) = 
  if (List.mem_assoc cid ctx.generated_fcns) then ctx 
  else {ctx with generated_fcns=(ctx.generated_fcns@[(cid, decl)])}

(* the value type of the array (inside the global/ref type) *)
let array_value_ty cell_ty len orig_ty = 
  timpl_wrap
    (tlist cell_ty (arrlen len))
    orig_ty
;;

(* memops: memops are functions passed as arguments. *)
(* return the array declaration and an array spec *)
let array_create ctx (arr_ty : ty) (arr_id : cid) (ctor_call : exp) = 
  (* first, get the array cell size and length. *)
  (* cell size is from type *)
  let cell_ty = List.hd (extract_tbuiltin arr_ty |> snd) in
  let cell_size = sizeof_ty cell_ty in
  (* length is the argument of the call *)
  let len = extract_ecall ctor_call |> snd |> List.hd |> eval_exp |> extract_vint in
  let arr_ty = array_value_ty (tint cell_size) len arr_ty in
  let arr_val = eval (zero_list arr_ty) in
  let arr = eimpl_wrap arr_val ctor_call in
  let ctx = {ctx with 
    arrays = (arr_id, (cell_ty, len))::ctx.arrays;}
  in
  (* ctx, dglobal arr_id (tref arr_ty) (eval arr_val) *)
  ctx, (dglobal arr_id arr_ty arr)

;;

let arr_fcn_id accessor_id arr_id memop_ids = 
  (* generate a unique name for this array op 
     based on the accessor function id, the 
     array id, and the ids of the memops passed 
     to the accessor. *)
  (* let names = List.map fst (accessor_id::arr_id::memop_ids) in *)
  List.fold_left Cid.concat accessor_id (arr_id::memop_ids)
;;

(* transform an Array.memop_complex call into a declaration and a new call *)
let update_complex ctx call_id call_args = 
  let arr = List.nth call_args 0 in (* should have type TBuiltin *)
  let arr_id, (arr_cellty, arr_len) = get_array ctx arr in
  let arr_ty = array_value_ty arr_cellty arr_len arr.ety in
  let arr = {arr with ety = arr_ty} in
  (* let arr = to_ref arr in *) 

  (* no longer need to do this. *)
  let memop_id, (_, memop_params, memop_body) = get_memop ctx (List.nth call_args 2) in

  let idx_param = Cid.create ["_idx"], (List.nth call_args 1).ety in
  let idx = eop Mod [ecast (tint 32) (param_evar idx_param); eval (vint arr_len 32)] in
  
  (* replace memop_param with arr[idx]; *)
  let memval_param_id = List.hd memop_params |> fst in
  let memval_exp_transformer evar_exp = 
    match evar_exp.e with 
    | EVar(cid) -> 
      if (Cid.equal cid (memval_param_id)) 
      then (elistget arr idx)
      else evar_exp
    | _ -> failwith "expected evar"
  in
  (* replace return (x, y) with arr[idx] :=x; return y; *)
  let sret_transformer ret_stmt = 
    match ret_stmt.s with 
    | SRet(Some(tuple_exp)) -> (
      let es = extract_etuple tuple_exp in
      let exp_to_mem, exp_to_ret = match es with 
        | [e1; e2] -> e1, e2
        | _ -> failwith "expected a 2-tuple return"
      in
      sseq
        (slistset arr idx exp_to_mem)
        ({ret_stmt with s=SRet(Some(exp_to_ret))})
    )
    | _ -> failwith "expected a return statement"
  in

  (* 1. new params: (idx, local1, local2) *)
  let new_params = [idx_param; List.nth memop_params 1; List.nth memop_params 2] in 

  (* 2. new statement: old statement with inlined mem values and 
                       mem value return replaced with mem cell assign *)
  let new_body = subst_evar#visit_statement memval_exp_transformer memop_body in
  let new_body = subst_return#visit_statement sret_transformer new_body in

  (* 3. new function type: new params -> one output *)
  let new_f_ty = tfun (List.map snd new_params) arr_cellty in

  (* 4. new call: call type, array id, and memop id embedded *)
  let new_memop_id = arr_fcn_id call_id arr_id [memop_id] in
  let f = efunref new_memop_id new_f_ty in
  (* new args: index and the two forwarded locals *)
  let new_args = [List.nth call_args 1; List.nth call_args 3; List.nth call_args 4] in
  let new_call = ecall f new_args in

  (* 5. new declaration *)
  let new_decl = dfun_kind FMemop
    new_memop_id
    arr_cellty
    new_params
    new_body
  in
  let ctx = emplace_fcn ctx (new_memop_id, (arr_id, new_decl)) in
  ctx, new_call
;;

let arr_fun_cids = List.map Builtins.gfun_cid Arrays.signature.m_funs ;;

let transform_calls ctx decl = 
  let ctx = ref ctx in
  let v = object 
    inherit [_] s_map as super

      method! visit_exp () exp = 
        let exp = super#visit_exp () exp in
        match exp.e with 
        | ECall({f; args; call_kind=CFun}) -> 
          let f_cid, _ = extract_evar f in
          if (List.mem f_cid arr_fun_cids) then 
          (
            let ctx', exp' = update_complex (!ctx) f_cid args in
            let exp' = eimpl_wrap exp' exp in
            ctx := ctx';
            exp'
          )
          else exp
        | _ -> exp
  end
  in
  let res = v#visit_decl () decl in 
  !ctx, res
;;

let process_decl ctx decl = 
  match decl.d with 
  | DFun(FMemop, id, ret_ty, params, BStatement(body)) -> 
    (* remember memop and delete *)
    {ctx with memops=(id, (ret_ty, params, body))::ctx.memops}, None
  | DVar(cid, ty, Some(exp)) when is_tbuiltin Arrays.t_id ty ->
    let ctx, decl = (array_create ctx ty cid exp) in 
    ctx, Some(decl)
  | _ -> let ctx, decl = transform_calls ctx decl in 
    ctx, Some(decl)
  ;;
let process decls = 
  let ctx, decls = List.fold_left (fun (ctx, decls) decl -> 
      let ctx, decl_opt = process_decl ctx decl in
      match decl_opt with 
        | None -> ctx, decls
        | Some(decl) -> ctx, decls@[decl]   
    )
    (empty_ctx, [])
    decls 
  in
  let new_memops = (ctx.generated_fcns |> List.split |> snd) in
  (* put the memops for each array after the array declaration *)
  let decls = List.fold_left 
    (fun decls decl -> 
      match decl.d with   
      | DVar(cid, _, _) ->
        let this_arrays_memop_decls = List.filter_map 
          (fun (tgt_arr_cid, decl) -> 
            if Cid.equal cid tgt_arr_cid then 
              Some(decl) else None)
          new_memops
        in
        decls@[decl]@this_arrays_memop_decls
      | _ -> decls@[decl])
    []
    decls
  in

  decls
;;

