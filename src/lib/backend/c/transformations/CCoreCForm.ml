(* a few small passes to get the program into a form 
   that is compatible with C.
   - convert match statements to if statements
   - pull record and union expressions out of 
     non-local-declaration statements     
*)
open CCoreSyntax
open CCoreExceptions
open CCoreTransformers

(* Convert match statements to if statements *)
let bitstring_to_maskedint (bs : int list) : int*int = 
  let to_val_and_mask bit = 
    match bit with 
    | 0    -> (0, 1) (* val: 0, mask 1 *) 
    | 1    -> (1, 1) (* val: 1, mask 1 *) 
    | -1  -> (0, 0) (* val: 0, mask :0 *)
    | _ -> err "invalid bitstring bit"
  in
  let rec bitstring_to_int bits =  
    match bits with 
      | [] -> 0
      | hd::tl ->
        (Int.shift_left hd (List.length tl)) + (bitstring_to_int tl)
  in      
  let vbits, mbits = List.map to_val_and_mask bs |> List.split in 
  bitstring_to_int vbits, bitstring_to_int mbits
;;

let tbit_to_tint ty = 
  match ty.raw_ty with 
  | TBits{len} -> tint len
  | _ -> ty
;;


let to_bool_atom (e,p) = 
  match p with 
    | PVal {v=VBits{ternary=true; bits;};} -> 
        let value, mask = bitstring_to_maskedint bits in
        let value = vint value (List.length bits) in
        let mask = vint mask (List.length bits) in
        let rhs = eop BitAnd [eval value; eval mask] in
        let lhs = eop BitAnd [e; eval mask] in
        Some(eop Eq [lhs; rhs])
    | PVal {v=VBits{ternary=false; bits;};} ->
        let value = (bitstring_to_maskedint bits |> fst |> vint) (List.length bits) in
        Some(eop Eq [e; eval value])
    | PVal v -> Some(eop Eq [e; eval v])
    | PWild _ -> None (* no constraint *)
    | PEvent _ -> err "events must be eliminated before matches"
;;
let eand_list terms = 
  match terms with 
  | [] -> vbool true |> eval
  | [term] -> term
  | terms -> 
      List.fold_left 
        (fun acc term -> eop And [acc; term])
        (List.hd terms)
        (List.tl terms)
;;
let rec match_to_if exps branches = 
  match branches with 
  | [] -> snoop
  | [pats, stmt] -> 
    let econd = eand_list 
      (List.filter_map to_bool_atom (List.combine exps pats))
    in
    sif econd stmt snoop
  | (pats, stmt)::branches -> 
    let else_stmt = match_to_if exps branches in
    let econd = eand_list 
      (List.filter_map to_bool_atom (List.combine exps pats))
    in
    sif econd stmt else_stmt
;;



let has_ternary_bits branches = 
  let v = 
    object 
    inherit [_] s_iter as super
    val mutable has_ternary_bits = false
    method has_ternary_bits = has_ternary_bits
    method! visit_TBits () is_tern _ = 
      if is_tern then has_ternary_bits <- true
    end
  in
  List.iter (v#visit_branch ()) branches;
  v#has_ternary_bits
;;

let transform_match stmt = 
  match stmt.s with 
  | SMatch([exp], _) when is_tenum exp.ety -> stmt
  (* a match on a single integer is just a switch, as long as no ternary patterns are used *)
  | SMatch([exp], branches) when ((is_tint exp.ety) && (not@@has_ternary_bits branches)) ->
    CCoreTransformers.subst_ty#visit_statement tbit_to_tint stmt
  | SMatch(es, branches) -> match_to_if es branches
  | _ -> stmt
;;
let normalize_matches decls = 
  CCoreTransformers.subst_statement#visit_decls transform_match decls
;;


(* ensure that record, union, and tuple expressions only appear in declarations *)
let is_initializer exp = match exp.e with 
  | EUnion _ -> true 
  | ERecord _ -> true
  | ETuple _ -> true
  | EVal({v=VUnion _}) -> true
  | EVal({v=VRecord _}) -> true
  | EVal({v=VTuple _}) -> true
  | _ -> false
;;
let normalize_struct_inits decls = 
  let v = object 
    (* new statements that must be added before the current one *)
    val mutable new_stmts = []
    inherit [_] s_map as super
    
    method! visit_statement rty_opt stmt = 
      match stmt.s with 
      (* creating a new local -- nothing has to change, so don't recurse *)
      | SAssign(OLocal(a, b), exp) -> (
        (* exp can be an initializer, but its subnodes can't *)
        new_stmts <- [];
        let exp = super#visit_exp rty_opt exp in
        match new_stmts with 
        | [] -> stmt
        | _ -> 
          (* update exp *)
          let stmt = {stmt with s=SAssign(OLocal(a, b),  exp)} in
          (* combine statements *)
          let stmt' = stmts (new_stmts@[stmt]) in
          (* reset statement store *)
          new_stmts <- []; (* have to clear new_stmts, else parent will add it in too. *)          
          stmt'
      )
      (* pull initializers out of other statements *)
      | _ -> (
        new_stmts <- [];
        let stmt = super#visit_statement rty_opt stmt in
          match new_stmts with 
          | [] -> stmt
          | _ -> 
            let stmt' = 
              stmts (new_stmts@[stmt])
            in
            new_stmts <- []; (* have to clear new_stmts, else parent will add it in too. *)
            stmt'
      )
    method! visit_decl rty_opt decl = 
      match decl.d with
        | DVar _ -> decl
        | _ -> super#visit_decl rty_opt decl
    method! visit_exp rty_opt exp = 
      if (is_initializer exp) 
      then (
        let id = Id.fresh_name "tmp" in
        let stmt = slocal (Cid.id id) exp.ety exp in
        new_stmts <- new_stmts@[stmt];
        evar (Cid.id id) exp.ety)
      else (
        let exp = super#visit_exp rty_opt exp in
        exp
      )
    end
  in
  v#visit_decls None decls
;;


let delete_empty_tuples decls = 
  (* remove everything relating to empty tuples. 
      - type and variable declarations
      - SAssigns with rhs : ()
      - parameters
      - arguments to calls  
  *)
  let rec is_empty_tuple ty = 
    match ty.raw_ty with 
    | TTuple([]) -> true
    | TPtr(ty, _) -> is_empty_tuple ty
    | TAbstract(_, ty) -> is_empty_tuple ty
    | _ -> false
  in
  let v = object 
    inherit [_] s_map as super
    method! visit_TUnion () ids tys = 
      let tys = List.map (super#visit_ty ()) tys in
      let id_tys = List.combine ids tys in
      let id_tys = List.filter (fun (_, ty) -> not (is_empty_tuple ty)) id_tys in
      let ids, tys = List.split id_tys in
      TUnion(ids, tys)
    method! visit_TRecord () ids tys = 
      let tys = List.map (super#visit_ty ()) tys in
      let id_tys = List.combine ids tys in
      let id_tys = List.filter (fun (_, ty) -> not (is_empty_tuple ty)) id_tys in
      let ids, tys = List.split id_tys in
      TRecord(ids, tys)
    method! visit_TTuple () tys = 
      let tys = List.map (super#visit_ty ()) tys in
      let tys = List.filter (fun ty -> not (is_empty_tuple ty)) tys in
      TTuple(tys)
    method! visit_VRecord () ids vs = 
      let vs = List.map (super#visit_value ()) vs in
      let id_vs = List.combine ids vs in
      let id_vs = List.filter (fun (_, v) -> not (is_empty_tuple v.vty)) id_vs in
      let ids, vs = List.split id_vs in
      VRecord(ids, vs)
    method! visit_VTuple () vs = 
      let vs = List.map (super#visit_value ()) vs in
      let vs = List.filter (fun v -> not (is_empty_tuple v.vty)) vs in
      VTuple(vs)
    method! visit_func_ty () func_ty = 
      let func_ty = super#visit_func_ty () func_ty in
      let arg_tys = List.filter (fun ty -> not (is_empty_tuple ty)) func_ty.arg_tys in
      {func_ty with arg_tys}
    method! visit_vevent () vevent = 
      let evdata = List.map (super#visit_value ()) vevent.evdata in
      let evdata = List.filter (fun v -> not (is_empty_tuple v.vty)) evdata in
      super#visit_vevent () {vevent with evdata}
    method! visit_ERecord () ids es = 
      let es = List.map (super#visit_exp ()) es in
      let id_es = List.combine ids es in
      let id_es = List.filter (fun (_, e) -> not (is_empty_tuple e.ety)) id_es in
      let ids, es = List.split id_es in
      ERecord(ids, es)
    method! visit_ETuple () es = 
      let es = List.map (super#visit_exp ()) es in
      let es = List.filter (fun e -> not (is_empty_tuple e.ety)) es in
      ETuple(es)
    method! visit_OTupleLocal () cids tys = 
      let tys = List.map (super#visit_ty ()) tys in
      let tys = List.filter (fun ty -> not (is_empty_tuple ty)) tys in
      super#visit_OTupleLocal () cids tys
    method! visit_OTupleAssign () exps = 
      let exps = List.map (super#visit_exp ()) exps in
      let exps = List.filter (fun exp -> not (is_empty_tuple exp.ety)) exps in
      super#visit_OTupleAssign () exps
    method! visit_SAssign () assign_op exp = 
      if (is_ttuple exp.ety && (tuple_length exp.ety = 0)) 
        then SNoop
        else super#visit_SAssign () assign_op exp    
    method! visit_params () params = 
      let params = List.filter (fun (_, ty) -> not (is_empty_tuple ty)) params in
      params
    
    method! visit_ECall () f args call_kind = 
      let args = List.filter (fun exp -> not (is_empty_tuple exp.ety)) args in
      super#visit_ECall () f args call_kind
    end
  in

  let rec process_decls decls = 
    match decls with 
    | [] -> []
    | decl::decls -> (
      let decl = v#visit_decl () decl in
      match decl.d with 
      | DTy(_, Some(ty)) when is_empty_tuple ty -> 
        process_decls decls
      | DVar(_, ty, _) when is_empty_tuple ty -> 
        process_decls decls
      | _ -> decl::(process_decls decls)
    )

  in

  let empty_tuple_checker = object 
    inherit [_] s_iter as super
    val mutable has_empty_tuple = false
    method has_empty_tuple = has_empty_tuple
    method! visit_TTuple () tys = 
      if (List.exists is_empty_tuple tys) then has_empty_tuple <- true;
      super#visit_TTuple () tys;
    end
  in
  let result = process_decls decls in
  empty_tuple_checker#visit_decls () result;
  if empty_tuple_checker#has_empty_tuple then 
    err "empty tuple elimination failed to remove all empty tuples"
  else result
;;


let declare_tuples decls =
  (* replace tuple types with named tuple types *)
  let v = object 
    (* new statements that must be added before the current one *)
    val mutable tuple_tys = []
    val mutable tnum = 0
    inherit [_] s_map as super
    method! visit_ty () ty =
      let ty = super#visit_ty () ty in
      if is_ttuple ty then (
        let found_opt = (List.find_opt  (fun (t, _ )-> equiv_tys t ty) tuple_tys) in
        let tcid = match found_opt with 
          | Some(_, tcid) -> tcid 
          | _ -> 
            tnum <- tnum + 1;
            let tcid = cid("tuple_"^string_of_int tnum) in
            tuple_tys <- tuple_tys@[ty, tcid];
            tcid
        in
        tabstract_cid tcid ty
      ) 
      else ty
    method get_tuple_tys = tuple_tys
  end
  in
  (* add in the tuple declarations. Should be safe to put these at the top. 
     Also, because of the traversal order, inner tuple types should 
     be generated before outer types. *)
  let decls = 
    (v#visit_decls () decls) 
  in
  (List.map (fun (ty, cid) -> decl_tabstract@@tabstract_cid cid ty) (v#get_tuple_tys))
  @decls
;;

