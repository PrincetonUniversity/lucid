(* 
  Regularize memops. 
    Convert all Array.set/get/update calls to Array.update_complex.
    Convert conditional return statements os they can be executed in parallel.
*) 
open InterpHelpers
open CoreSyntax
open TofinoCore
module CS = CoreSyntax
module C = TofinoCore


(* let pad_complex_params cell_size exps = 
  let npad = 5 - (CL.length exps) in
  exps
  @(List.init 
    npad 
    (fun _ -> (CS.default_vint cell_size |> value_to_exp))
  )
;; *)

(* convert a pair of memops in an array.update/getm/setm call into a complex memop *)
let to_complex_memop get_memop_opt set_memop_opt =
  (* replace a parameter in a memop body with an argument *)
  let replace_param_in_memop (memop_body:memop_body) ((param : (id * C.ty)), (arg:exp)) = 
    let param_id = fst param in 
    let v = 
      object
        inherit [_] s_map as super
        method! visit_exp ctx var_exp = 
          let var_exp = super#visit_exp ctx var_exp in 
          match var_exp.e with
          | EVar(var_cid) -> 
              if (Id.equal (Cid.to_id var_cid) param_id) 
              then (arg)
              else (var_exp)
          | _ -> var_exp 
      end
    in
    v#visit_memop_body () memop_body
  in
  let to_conditional_return memop_body =
    let bool_id = Id.fresh_name "mbool" in
    let bool_exp = var_sp (Cid.id bool_id) (CS.ty TBool) (Span.default) in 
    let etrue = CS.value_to_exp (CS.vbool true) in 
    match memop_body with 
      | MBReturn(exp) -> 
        (* bool assignment *)
        Some(bool_id, etrue),
        (* cell assignment *)
        (Some (bool_exp, exp), None)
      | MBIf(cexp, exp1, exp2) -> 
        (* bool assignment *)
        Some(bool_id, cexp),
        (* cell assignment *)
        (Some(bool_exp, exp1), Some(etrue, exp2))
      | MBComplex _ -> error "[to_conditional_return] attempted to translate complex memop into a complex memop..."
  in  

  match (get_memop_opt, set_memop_opt) with 
  | Some get_memop, Some set_memop -> (
    (* replace memory parameter in set memop to unify param id *)
    let get_mem_param = List.hd (get_memop.mparams) in
    let get_mem_param_exp = CS.var_sp (Cid.id (fst get_mem_param)) (snd get_mem_param) (Span.default) in 
    let set_mem_param = List.hd (set_memop.mparams) in
    let set_memop = {set_memop with 
      mbody=replace_param_in_memop set_memop.mbody (set_mem_param,get_mem_param_exp)};
    in 

    (* rename the parameter passed to the set memop so 
        all parameters in new complex memop have unique names *)
    let set_passed_param = List.hd (List.tl (set_memop.mparams)) in 
    let new_set_passed_param_id = Id.fresh_name ((fst (fst set_passed_param))^"_set") in 
    let new_set_passed_param_exp = CS.var_sp (Cid.id (new_set_passed_param_id)) (snd get_mem_param) (Span.default) in 
    let set_memop = {set_memop with 
      mbody=replace_param_in_memop set_memop.mbody (set_passed_param,new_set_passed_param_exp);
      }
    in 

    let cell_ty = snd get_mem_param in 
    (* return the content of cell2 *)
    let ret_cond_exp = 
      CS.value_to_exp (CS.vbool true),
      CS.var_sp (Cid.create ["cell2"]) cell_ty Span.default
    in 

    let set_bool, set_cell = to_conditional_return set_memop.mbody in
    let get_bool, get_cell = to_conditional_return get_memop.mbody in
    let complex_body = MBComplex{
        b1 = set_bool;
        b2 = get_bool;
        cell1 = set_cell;
        cell2 = get_cell;
        extern_calls = [];
        ret = Some (ret_cond_exp);
      }
    in 
    (* complex memop has 3 params *)
    let complex_params = get_memop.mparams@[(new_set_passed_param_id, snd set_passed_param)] in 
    let complex_id = Id.fresh_name ("combined_memop_"^(fst get_memop.mid)^"_"^(fst set_memop.mid)) in 
    {mid = complex_id; mparams = complex_params; mbody = complex_body;}
  )
  | Some get_memop, None ->
    let get_mem_param = List.hd (get_memop.mparams) in
    let cell_ty = snd get_mem_param in 
    (* return the content of cell2 *)
    let ret_cond_exp = 
      CS.value_to_exp (CS.vbool true),
      CS.var_sp (Cid.create ["cell2"]) cell_ty Span.default
    in 
    let get_bool, get_cell = to_conditional_return get_memop.mbody in
    let complex_body = MBComplex{
        b1 = None;
        b2 = get_bool;
        cell1 = None, None;
        cell2 = get_cell;
        extern_calls = [];
        ret = Some (ret_cond_exp);
      }
    in 
    {
      mid=get_memop.mid; 
      mparams = get_memop.mparams@[(Id.fresh_name "unused", cell_ty);];
      mbody = complex_body;
    }
  | None, Some set_memop ->
    let set_bool, set_cell = to_conditional_return set_memop.mbody in
    let cell_ty = snd (List.hd (set_memop.mparams)) in 
    let complex_body = MBComplex{
        b1 = set_bool;
        b2 = None;
        cell1 = set_cell;
        cell2 = None, None;
        extern_calls = [];
        ret = None;
      }
    in 
    {
      mid=set_memop.mid; 
      mparams = set_memop.mparams@[(Id.fresh_name "unused", cell_ty);];
      mbody = complex_body;
    }
  | None, None -> 
    error "[to_complex_memop] nothing to do -- got neither a set nor get memop"
;;

let memop_size ememop =
  match ememop.ety.raw_ty with 
    | TMemop(_, size) -> size
    | _ -> error "[regularize_array_calls] memop argument is not a memop expression."
;;

(* we only want to make 1 memop for get and set *)
let suffixed_id id_suffix s = Id.create (s^id_suffix) ;;
let get_memop ty = 
  let memop_size = match ty.raw_ty with 
    | TInt(sz) -> sz
    | _ -> error "Array.get's type should be a tint"     
  in
  let id_suffix = "_get_memop_"^(string_of_int memop_size)^"_bit" in
  let fname = (suffixed_id id_suffix) in
  let cell_ty = ty in 
  let mem_val_id =  fname "mem_val" in
  let complex_params = [
    (mem_val_id, cell_ty);
    (fname "unused", cell_ty);                 
    (fname "unused", cell_ty);                  
  ]
  in             
  let complex_body = MBComplex{
    b1 = None;
    b2 = None;
    cell1 = None, None;
    cell2 = None, None;
    extern_calls = [];
    ret = Some(
      CS.value_to_exp (CS.vbool true), 
      CS.var_sp (Cid.id mem_val_id) cell_ty (Span.default)
      )
    }
  in
  let complex_memop = 
    {mid = fname "get"; mparams = complex_params; mbody = complex_body;}  
  in
  complex_memop
;;

let set_memop cell_ty = 
  let memop_size = match cell_ty.raw_ty with 
    | TInt(sz) -> sz
    | _ -> error "Array.set's type should be a tint"     
  in
  let id_suffix = "_set_memop_"^(string_of_int memop_size)^"_bit" in
  let fname = (suffixed_id id_suffix) in

  let newval_param_id = fname "new_val" in
  let complex_params = [
    (fname"mem_val", cell_ty);
    (newval_param_id, cell_ty);                 
    (fname "unused", cell_ty);                  
  ]
  in             
  let complex_body = MBComplex{
    b1 = None;
    b2 = None;
    cell1 = Some(CS.value_to_exp (CS.vbool true), CS.var_sp (Cid.create_ids [newval_param_id]) (cell_ty) Span.default), None;
    cell2 = None, None;
    extern_calls = [];
    ret = None;
  }
  in
  let complex_memop = 
    {mid = fname"set"; mparams = complex_params; mbody = complex_body;}
  in 
  complex_memop

;;


(* convert every array.set/get/setm/getm/update call into an array.update_complex, creating a new memop if necessary.*)
(* need to avoid creating duplicates of the same memops... *)
let regularize_array_calls tds = 
  let memops = memops tds in 
  let string_of_fcncid cid = 
      Caml.String.concat "." @@ Cid.names cid
  in
  let new_memops = ref [] in 
  let memops_to_delete = ref [] in 
  let v = 
    object
      inherit [_] s_map as super
        method! visit_exp ctx exp = 
          let exp = super#visit_exp ctx exp in 
          let results = match exp.e with 
            | ECall(fcn_cid, args, _) -> (
              match (string_of_fcncid fcn_cid) with 
              | "Array.getm" -> (
                let aid, idx, get_memop, get_arg = match args with
                  | [aid; idx; get_memop; get_arg] -> 
                    aid, idx, get_memop, get_arg
                  | _ -> error "[translate_array_call] unexpected arguments for array method"
                in 
                let memop_size = memop_size get_memop in 
                let get_memop = 
                  List.assoc 
                    (InterpHelpers.name_from_exp get_memop |> Cid.to_id )
                    memops
                in 
                let complex_memop = to_complex_memop (Some(get_memop)) (None) in 
                let complex_memop_exp = CS.exp (EVar(Cid.id complex_memop.mid)) (ty (TMemop(3, memop_size))) in
                let old_memop_ids = [get_memop.mid] in 
                let complex_args = [
                  aid; 
                  idx; 
                  complex_memop_exp;
                  get_arg;
                  (CS.default_vint memop_size |> value_to_exp);
                  (CS.default_vint memop_size |> value_to_exp)
                  ]
                in
                let complex_call = CS.ECall ((Cid.create ["Array"; "update_complex"]), complex_args, false) in 
                Some ({exp with e = complex_call;}, complex_memop, old_memop_ids)
              )
            | "Array.get" ->  
              let aid, idx = match args with
                | [aid; idx] -> 
                  aid, idx
                | _ -> error "[translate_array_call] unexpected arguments for array method"
              in 
              let memop_size = match exp.ety.raw_ty with 
                | TInt(sz) -> sz
                | _ -> error "Array.get's type should be a tint"     
              in                    
              let cell_ty = exp.ety in 
              let complex_memop = get_memop cell_ty in
              let complex_memop_exp = CS.exp (EVar(Cid.id complex_memop.mid)) (ty (TMemop(3, memop_size))) in
              let complex_args = [
                  aid; 
                  idx;
                  complex_memop_exp;
                  (CS.default_vint memop_size |> value_to_exp);
                  (CS.default_vint memop_size |> value_to_exp);
                  (CS.default_vint memop_size |> value_to_exp)
                  ]
              in
              let complex_call = CS.ECall ((Cid.create ["Array"; "update_complex"]), complex_args, false) in 
              Some ({exp with e = complex_call;}, complex_memop, [])
              | "Array.setm" -> 
                let aid, idx, set_memop, set_arg = match args with
                  | [aid; idx; set_memop; set_arg] -> 
                    aid, idx, set_memop, set_arg
                  | _ -> error "[translate_array_call] unexpected arguments for array method"
                in 
                let memop_size = match exp.ety.raw_ty with 
                  | TInt(sz) -> sz
                  | _ -> error "Array.setm argument's type should be a tint"     
                in      
                let set_memop = 
                  List.assoc 
                    (InterpHelpers.name_from_exp set_memop |> Cid.to_id )
                    memops
                in 
                let complex_memop = to_complex_memop (None) (Some(set_memop)) in 
                let complex_memop_exp = CS.exp (EVar(Cid.id complex_memop.mid)) (ty (TMemop(3, memop_size))) in
                let old_memop_ids = [set_memop.mid] in 
                let complex_args = [
                  aid; 
                  idx; 
                  complex_memop_exp;
                  set_arg;
                  (CS.default_vint memop_size |> value_to_exp);
                  (CS.default_vint memop_size |> value_to_exp)
                  ]
                in
                let complex_call = CS.ECall ((Cid.create ["Array"; "update_complex"]), complex_args, false) in 
                Some ({exp with e = complex_call;}, complex_memop, old_memop_ids)
              | "Array.set" -> 
                let aid, idx, set_arg = match args with
                  | [aid; idx; set_arg] -> 
                    aid, idx, set_arg
                  | _ -> error "[translate_array_call] unexpected arguments for array method"
                in 
                let cell_ty = set_arg.ety in 
                let memop_size = match set_arg.ety.raw_ty with 
                  | TInt(sz) -> sz
                  | _ -> error "Array.set arg's type should be a tint"     
                in                    
                let complex_memop = set_memop cell_ty in
                let complex_memop_exp = CS.exp (EVar(Cid.id complex_memop.mid)) (ty (TMemop(3, memop_size))) in
                let complex_args = [
                    aid; 
                    idx;
                    complex_memop_exp;
                    set_arg;
                    (CS.default_vint memop_size |> value_to_exp);
                    (CS.default_vint memop_size |> value_to_exp)
                    ]
                in
                let complex_call = CS.ECall ((Cid.create ["Array"; "update_complex"]), complex_args, false) in 
                Some ({exp with e = complex_call;}, complex_memop, [])

              | "Array.update" -> (
                let aid, idx, get_memop, get_arg, set_memop, set_arg = match args with
                  | [aid; idx; get_memop; get_arg; set_memop; set_arg] -> 
                    aid, idx, get_memop, get_arg, set_memop, set_arg
                  | _ -> error "[regularize_array_calls] unexpected arguments for array method"
                in 
                let memop_size = memop_size get_memop in 
                let get_memop, set_memop = 
                  List.assoc 
                    (InterpHelpers.name_from_exp get_memop |> Cid.to_id )
                    memops,
                  List.assoc 
                    (InterpHelpers.name_from_exp set_memop |> Cid.to_id )
                    memops
                in 
                let complex_memop = to_complex_memop (Some(get_memop)) (Some(set_memop)) in 
                let complex_memop_exp = CS.exp (EVar(Cid.id complex_memop.mid)) (ty (TMemop(3, memop_size))) in
                let old_memop_ids = [set_memop.mid; get_memop.mid] in 
                let complex_args = [
                  aid; 
                  idx; 
                  complex_memop_exp;
                  get_arg;
                  set_arg;
                  (CS.default_vint memop_size |> value_to_exp)
                  ]
                in
                let complex_call = CS.ECall ((Cid.create ["Array"; "update_complex"]), complex_args, false) in 
                Some ({exp with e = complex_call;}, complex_memop, old_memop_ids)
              )
              | _ -> None
            )
            | _ -> None 
        in
        match results with 
        | Some(new_exp, new_memop, old_memop_ids) -> 
          new_memops := {td=TDMemop(new_memop); tdspan=Span.default; tdpragma=[]}::(!new_memops); 
          memops_to_delete := (!memops_to_delete)@old_memop_ids;
          new_exp
        | None -> exp
    end
  in
  let rec update_memops tds =
    match tds with 
    | [] -> []
    | td::tds -> (
      match td.td with 
      | TDMemop({mid=mid; _}) -> (
        if (MiscUtils.contains (!memops_to_delete) mid)
        then (update_memops tds)
        else (td::(update_memops tds))
      )
      | _ -> td::(update_memops tds)
    )
  in

  let new_tds = v#visit_tdecls () tds in 
  (* now add decls for new memops and delete old merged memops *)
  (update_memops new_tds)@(MiscUtils.unique_list_of (!new_memops))
;;



(* 
   condition the second return statement of both cell1 and cell2, 
   so that they can be executed as 2 parallel applied 
   conditional statements instead of an if / else: 
    if (<x>) 
      --> 
    if (!cell1_condition  && x) 
*)
let condition_snd_cr_stmts (b:CoreSyntax.complex_body) = 
  (* condition the second return statement of a single cell. *)
  let condition_snd_cr_stmt (cr_opt1, cr_opt2)  = 
    let cr_opt2 = match cr_opt1, cr_opt2 with 
      | None, None -> cr_opt2
      | Some _, None -> cr_opt2
      | None, Some _ -> cr_opt2
      | Some(c1_cond, _), Some(c2_cond, c2_exp) -> 
        (* c2_cond = !c1_cond && c2_cond 
          but, we have to simplify it for the P4 compiler
          DNF form seems to work well... *)
        (
          let not_c1 = CS.exp (C.EOp(C.Not, [c1_cond])) c1_cond.ety in 
          let not_c1_and_c2 = CS.exp (C.EOp(C.And, [not_c1; c2_cond])) c2_cond.ety in
          let s, ze = CoreZ3.exp_to_expr not_c1_and_c2 in 
          let ze_simp = CoreZ3.simplify_to_dnf s ze in 
          let simplified_c2_cond = CoreZ3.expr_to_exp s ze_simp in 
          Some(simplified_c2_cond, c2_exp)
        )
    in 
    (cr_opt1, cr_opt2)
  in
  {b with cell1 = condition_snd_cr_stmt b.cell1; cell2 = condition_snd_cr_stmt b.cell2;}
;;

let rec delete_non_complex_memops tds =
  List.filter (fun td -> 
    match td.td with
      | TDMemop({mbody=MBComplex(_);}) -> true
      | TDMemop(_) -> false
      | _ -> true )
  tds
;;

let rec regularize_memop_conditions tds =
  match tds with 
  | [] -> []
  | td::tds -> (
    match td.td with 
      | TDMemop(mo) -> (
        match mo.mbody with 
          | MBComplex(complex_body) -> 
            let new_mbody = MBComplex(condition_snd_cr_stmts complex_body) in
            let new_mo = {mo with mbody=new_mbody;} in 
            let new_td = {td with td=TDMemop(new_mo)} in 
            new_td::(regularize_memop_conditions tds)
          | _ -> 
            error "[regularize_memop_conditions] all memops must be complex by this point."
           (* td::(regularize_memop_conditions tds) *)
      )
      | _ -> td::(regularize_memop_conditions tds)
  )
;;

(* correct ordering of declarations in the program to ensure that all memop declarations 
   come before any handler declarations. *)
let correct_decl_ordering tds = 
  let memops_decls, other_decls = List.fold_left 
    (fun (memop_decls, other_decls) decl -> 
      match decl.td with 
      | TDMemop(_) -> (memop_decls@[decl], other_decls)
      | _ -> (memop_decls, other_decls@[decl]))
    ([], [])
    tds
  in
  List.fold_left (fun decls decl -> 
    match decl.td with
    | TDHandler(_) -> decls@memops_decls@[decl]
    | _ -> decls@[decl])
  []
  other_decls
;;


(* make sure that  *)

let process tds = 
  regularize_array_calls tds 
  |> delete_non_complex_memops 
  |> regularize_memop_conditions 
  |> correct_decl_ordering
;;
let process_core prog =
  List.map
    (fun component -> 
      {component with comp_decls = process component.comp_decls;})
    prog
;;