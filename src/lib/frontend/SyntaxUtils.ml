open Syntax
open Batteries
open Collections

let cell1_id = Id.create "cell1"
let cell2_id = Id.create "cell2"
let extract_sizes gty = snd gty
let gname gty = Id.name (fst gty)

(* Turns an effect into a base effect plus a list of offsets. E.g.
   FSucc (FSucc (FProj (FSucc (FProj (FZero)))))) maps to
   FZero, [0; 1 ; 2] *)
let unwrap_effect eff =
  let rec aux eff =
    match eff with
    | FZero -> FZero, [None, 0]
    | FVar (QVar _) | FVar (TVar { contents = Unbound _ }) -> eff, [None, 0]
    | FVar (TVar { contents = Link eff }) -> aux eff
    | FSucc eff ->
      let base, lst = aux eff in
      let lst =
        match lst with
        | [] -> failwith "impossible"
        | (o, n) :: tl -> (o, n + 1) :: tl
      in
      base, lst
    | FProj eff ->
      let base, lst = aux eff in
      base, (None, 0) :: lst
    | FIndex (id, eff) ->
      let base, lst = aux eff in
      base, (Some id, 0) :: lst
  in
  let base, lst = aux eff in
  base, List.rev lst
;;

let wrap_effect base lst =
  let rec add_succs base n =
    if n < 0 then failwith @@ "Invalid effect: " ^ string_of_int n;
    if n = 0 then base else FSucc (add_succs base (n - 1))
  in
  match lst with
  | [] -> failwith "Cannot wrap an empty list!"
  | (None, hd) :: tl ->
    List.fold_left
      (fun acc (o, n) ->
        match o with
        | None -> add_succs (FProj acc) n
        | Some id -> add_succs (FIndex (id, acc)) n)
      (add_succs base hd)
      tl
  | (Some _, _) :: _ -> failwith "First element of list cannot be FIndex!"
;;

(******************************************)
(* Normalization and comparison functions *)
(******************************************)

let rec is_global_rty rty =
  match TyTQVar.strip_links rty with
  | TBool | TVoid | TGroup | TInt _ | TEvent | TFun _ | TMemop _ -> false
  | TQVar _ -> false (* I think *)
  | TName (_, _, b) | TAbstract (_, _, b, _) -> b
  | TTuple lst -> List.exists is_global_rty lst
  | TRecord lst -> List.exists (fun (_, rty) -> is_global_rty rty) lst
  | TVector (t, _) -> is_global_rty t
  | TTable (_) -> true
;;

let is_global ty = is_global_rty ty.raw_ty

(* Similar to is_global_rty, but also returns false for TQVars *)
let rec is_not_global_rty rty =
  match TyTQVar.strip_links rty with
  | TBool | TVoid | TGroup | TInt _ | TEvent | TFun _ | TMemop _ -> true
  | TQVar _ -> false (* I think *)
  | TName (_, _, b) | TAbstract (_, _, b, _) -> not b
  | TTuple lst -> List.for_all is_not_global_rty lst
  | TRecord lst -> List.for_all (fun (_, rty) -> is_not_global_rty rty) lst
  | TVector (t, _) -> is_not_global_rty t
  | TTable (_) -> false

;;

let is_not_global ty = is_not_global_rty ty.raw_ty

let add_sizes s1 s2 =
  match STQVar.strip_links s1, STQVar.strip_links s2 with
  | IConst n1, IConst n2 -> IConst (n1 + n2)
  | ISum (ss, n2), IConst n1 | IConst n1, ISum (ss, n2) -> ISum (ss, n1 + n2)
  | ISum (ss, n), ((IVar _ | IUser _) as s)
  | ((IVar _ | IUser _) as s), ISum (ss, n) -> ISum (s :: ss, n)
  | IConst n1, ((IVar _ | IUser _) as s) | ((IVar _ | IUser _) as s), IConst n1
    -> ISum ([s], n1)
  | (IVar _ | IUser _), (IVar _ | IUser _) -> ISum ([s1; s2], 0)
  | ISum (vs1, n1), ISum (vs2, n2) -> ISum (vs1 @ vs2, n1 + n2)
;;

(* If an ISum, the list is non-empty, sorted, and no entries are Links *)
let rec normalize_size s =
  match STQVar.strip_links s with
  | ISum (vs, n) ->
    (* Since elements of vs could have Links to arbitrary sizes, we recursively normalize
       each element, then add them all together. *)
    let recursively_normalize vs =
      List.fold_left
        (fun acc x -> add_sizes (normalize_size x) acc)
        (IConst n)
        vs
    in
    (* If the result is still a sum, then sort the list; otherwise, just return the result *)
    begin
      match recursively_normalize vs with
      | ISum ([], _) -> failwith "Sanity check: this should never happen"
      | ISum ([s], 0) -> s
      | ISum (vs, n) -> ISum (List.sort Pervasives.compare vs, n)
      | sz -> sz
    end
  | s -> s
;;

let extract_size_default s def =
  match normalize_size s with
  | IConst n -> n
  | _ -> def
;;

let extract_size s =
  match normalize_size s with
  | IConst n -> n
  | _ -> failwith "[extract_size] error: normalized size is not a const"
;;

let rec equiv_lists f lst1 lst2 =
  match lst1, lst2 with
  | [], [] -> true
  | hd1 :: tl1, hd2 :: tl2 -> f hd1 hd2 && equiv_lists f tl1 tl2
  | _ -> false
;;

let rec equiv_size ?(qvars_wild = false) s1 s2 =
  let equiv_size = equiv_size ~qvars_wild in
  match normalize_size s1, normalize_size s2 with
  | IConst n1, IConst n2 -> n1 = n2
  | IUser id1, IUser id2 -> Cid.equal id1 id2
  | ISum (vs1, n1), ISum (vs2, n2) -> n1 = n2 && equiv_lists equiv_size vs1 vs2
  | IConst n1, ISum (vs, n2) | ISum (vs, n2), IConst n1 ->
    (* Special case *)
    qvars_wild
    && n2 <= n1
    && List.for_all
         (function
          | IVar (QVar _) -> true
          | _ -> false)
         vs
  | IVar tqv, s | s, IVar tqv -> STQVar.equiv_tqvar ~qvars_wild equiv_size tqv s
  | _ -> false
;;

(* If s1 is "obviously" greater than s2, return the difference; otherwise None.
   "Obvious" means they are both integers, or s1 is just s2 plus something *)
let try_subtract_sizes s1 s2 =
  match normalize_size s1, normalize_size s2 with
  | IConst n, IConst m when n >= m -> Some (IConst (n - m))
  | ISum (sizes, n), IConst m when n >= m -> Some (ISum (sizes, n - m))
  | ISum (sizes1, n1), ISum (sizes2, n2) when n1 >= n2 ->
    (try
       let new_sizes =
         List.fold_left
           (fun acc sz ->
             if List.mem sz acc then List.remove acc sz else raise (Failure ""))
           sizes1
           sizes2
       in
       if List.is_empty new_sizes
       then Some (IConst (n1 - n2))
       else Some (ISum (new_sizes, n1 - n2))
     with
     | Failure _ -> None)
  | ISum (sizes, n), s2 when List.mem s2 sizes ->
    Some (ISum (List.remove sizes s2, n))
  | s1, s2 when equiv_size s1 s2 -> Some (IConst 0)
  | _ -> None
;;

let rec equiv_effect ?(qvars_wild = false) e1 e2 =
  let equiv_effect = equiv_effect ~qvars_wild in
  match e1, e2 with
  | FZero, FZero -> true
  | FSucc e1', FSucc e2' | FProj e1', FProj e2' -> equiv_effect e1' e2'
  | FIndex (id1, e1'), FIndex (id2, e2') ->
    Id.equal id1 id2 && equiv_effect e1' e2'
  | FVar tqv, e | e, FVar tqv ->
    FTQVar.equiv_tqvar ~qvars_wild equiv_effect tqv e
  | (FZero | FSucc _ | FProj _ | FIndex _), _ -> false
;;

let equiv_constraints lst1 lst2 =
  List.for_all2
    (fun (CLeq (e1, e2)) (CLeq (e1', e2')) ->
      equiv_effect e1 e1' && equiv_effect e2 e2')
    (List.sort Pervasives.compare lst1)
    (List.sort Pervasives.compare lst2)
;;

let normalizer () =
  let count = ref 0 in
  let v =
    object (self)
      inherit [_] s_map as super
      val mutable renaming : id IdMap.t = IdMap.empty

      method! visit_QVar _ _ id =
        match IdMap.find_opt id renaming with
        | Some id' -> QVar id'
        | None ->
          let new_id = Id.create ("norm" ^ string_of_int !count) in
          incr count;
          renaming <- IdMap.add id new_id renaming;
          QVar new_id

      method! visit_TQVar _ tqv =
        match tqv with
        | TVar { contents = Link x } -> self#visit_raw_ty () x
        | _ -> super#visit_TQVar () tqv

      method! visit_IVar _ tqv =
        match tqv with
        | TVar { contents = Link x } -> self#visit_size () x
        | _ -> super#visit_IVar () tqv

      method! visit_FVar _ tqv =
        match tqv with
        | TVar { contents = Link x } -> self#visit_effect () x
        | _ -> super#visit_FVar () tqv

      method! visit_size () s = super#visit_size () (normalize_size s)
    end
  in
  v
;;

let normalize_tfun func_ty = (normalizer ())#visit_func_ty () func_ty
let normalize_ty ty = (normalizer ())#visit_ty () ty

let rec equiv_raw_ty ?(ignore_effects = false) ?(qvars_wild = false) ty1 ty2 =
  let equiv_size = equiv_size ~qvars_wild in
  let equiv_effect = equiv_effect ~qvars_wild in
  let equiv_raw_ty = equiv_raw_ty ~ignore_effects ~qvars_wild in
  let equiv_ty = equiv_ty ~ignore_effects ~qvars_wild in
  match ty1, ty2 with
  | TBool, TBool | TVoid, TVoid | TGroup, TGroup | TEvent, TEvent -> true
  | TInt size1, TInt size2 -> equiv_size size1 size2
  | TMemop (n1, size1), TMemop (n2, size2) -> n1 = n2 && equiv_size size1 size2
  | TName (id1, sizes1, b1), TName (id2, sizes2, b2)
  | TAbstract (id1, sizes1, b1, _), TAbstract (id2, sizes2, b2, _) ->
    b1 = b2 && Cid.equal id1 id2 && List.for_all2 equiv_size sizes1 sizes2
  | TFun func1, TFun func2 ->
    let func1 = normalize_tfun func1 in
    let func2 = normalize_tfun func2 in
    equiv_lists equiv_ty func1.arg_tys func2.arg_tys
    && equiv_ty func2.ret_ty func2.ret_ty
    && equiv_effect func1.start_eff func2.start_eff
    && equiv_effect func1.end_eff func2.end_eff
    && equiv_constraints !(func1.constraints) !(func2.constraints)
  | TQVar tqv, ty | ty, TQVar tqv ->
    TyTQVar.equiv_tqvar ~qvars_wild equiv_raw_ty tqv ty
  | TRecord lst1, TRecord lst2 ->
    if List.length lst1 <> List.length lst2
    then false
    else
      List.for_all2
        (fun (str1, ty1) (str2, ty2) ->
          String.equal str1 str2 && equiv_raw_ty ty1 ty2)
        lst1
        lst2
  | TVector (ty1, size1), TVector (ty2, size2) ->
    equiv_size size1 size2 && equiv_raw_ty ty1 ty2
  | TTuple lst1, TTuple lst2 ->
    if List.length lst1 <> List.length lst2
    then false
    else List.for_all2 equiv_raw_ty lst1 lst2
  | TTable(ks1, as1, s1), TTable(ks2, as2, s2) -> 
    (List.for_all2 equiv_size ks1 ks2)
    && (List.for_all2 
        (fun (a1, si1, so1) (a2, si2, so2) -> 
          (String.equal a1 a2) 
          && (List.for_all2 equiv_size si1 si2)
          && (List.for_all2 equiv_size so1 so2))
        as1 as2)
    && (equiv_size s1 s2)
  | ( ( TBool
      | TMemop _
      | TInt _
      | TEvent
      | TName _
      | TFun _
      | TVoid
      | TGroup
      | TRecord _
      | TVector _
      | TTuple _
      | TAbstract _
      | TTable _ )
    , _ ) -> false

and equiv_ty ?(ignore_effects = false) ?(qvars_wild = false) ty1 ty2 =
  (ignore_effects
  || is_not_global ty1
  || equiv_effect ~qvars_wild ty1.teffect ty2.teffect)
  && equiv_raw_ty ~ignore_effects ~qvars_wild ty1.raw_ty ty2.raw_ty
;;

let max_effect e1 e2 =
  let base1, lst1 = unwrap_effect e1 in
  let base2, lst2 = unwrap_effect e2 in
  if not (equiv_effect base1 base2)
  then None
  else Some (wrap_effect base1 (max lst1 lst2))
;;

let default_expression ty =
  let rec aux rty =
    match TyTQVar.strip_links rty with
    | TInt size -> eint (Z.of_int 32) (Some size)
    | TBool -> value_to_exp (vbool false)
    | TVector (raw_ty, size) -> begin
      match size with
      | IConst n -> vector_sp (List.init n (fun _ -> aux raw_ty)) Span.default
      | _ -> comp_sp (aux raw_ty) (Id.create "_") size Span.default
    end
    | TRecord lst ->
      record_sp (List.map (fun (s, raw_ty) -> s, aux raw_ty) lst) Span.default
    | _ ->
      failwith
      @@ "Can only create default expression for types int or bool, or records \
          and vectors of such"
  in
  aux ty.raw_ty
;;

(* True for exps which involve some amount of computation (and hence should not
   be duplicated) *)
let rec is_compound e =
  match e.e with
  | EInt _ | EVal _ | EVar _ | ESizeCast _ -> false
  | EHash _ | EOp _ | ECall _ | EStmt _ -> true
  | ECreateTable _ -> true
  | EComp (e, _, _) | EIndex (e, _) | EProj (e, _) | EFlood e -> is_compound e
  | EVector entries | ETuple entries -> List.exists is_compound entries
  | ERecord entries -> List.exists (is_compound % snd) entries
  | EWith (base, entries) ->
    is_compound base || List.exists (is_compound % snd) entries
;;

(* Turn a list of statements into an SSeq (or a SNoop, if empty) *)
let sequence_stmts lst =
  match lst with
  | [] -> snoop
  | hd :: tl -> List.fold_left (fun acc s -> sseq acc s) hd tl
;;

(* Turn a SSeq into a list of statements. Only applies to top-level SSeqs *)
let flatten_stmt s =
  let rec aux acc s =
    match s.s with
    | SNoop -> acc
    | SSeq (s1, s2) -> aux (aux acc s2) s1
    | _ -> s :: acc
  in
  aux [] s
;;

(* Mostly useful for printing *)

let complex_body_to_stmt (body : complex_body) =
  let handle_bool b = Option.map (fun (id, e) -> slocal id (ty TBool) e) b in
  let handle_cell id (cro1, cro2) =
    let els =
      match cro2 with
      | None -> snoop
      | Some (cond, e) -> sifte cond (sassign id e) snoop
    in
    match cro1 with
    | None -> None
    | Some (cond, e) -> Some (sifte cond (sassign id e) els)
  in
  let b1 = handle_bool body.b1 in
  let b2 = handle_bool body.b2 in
  let c1 = handle_cell cell1_id body.cell1 in
  let c2 = handle_cell cell2_id body.cell2 in
  let calls =
    List.map
      (fun (cid, es) -> Some (scall_sp cid es Span.default))
      body.extern_calls
  in
  let ret =
    Option.map
      (fun (cond, e) -> sifte cond (statement (SRet (Some e))) snoop)
      body.ret
  in
  List.filter_map (fun x -> x) ([b1; b2; c1; c2] @ calls @ [ret])
  |> sequence_stmts
;;

let memop_body_to_stmt memop_body =
  match memop_body with
  | MBReturn e -> statement (SRet (Some e))
  | MBIf (e1, e2, e3) ->
    sifte e1 (statement (SRet (Some e2))) (statement (SRet (Some e3)))
  | MBComplex b -> complex_body_to_stmt b
;;

let simplify_constraint = function
  | CLeq (e1, e2) ->
    if equiv_effect e1 e2 || e1 = FZero
    then Some true
    else
      (* Compute max of e1, e2; tautology if we get e2, contradiction if we get e1 *)
      Option.map (equiv_effect e2) (max_effect e1 e2)
;;

(* Removes tautologies, and either ignores contradictions (for debugging)
   or replaces the entire thing with false if it finds any. *)
let prune_constraints ?(ignore_contradictions = false) constraints =
  let rec aux acc cs =
    match cs with
    | [] -> acc
    | hd :: tl ->
      (match simplify_constraint hd with
       | None -> aux (hd :: acc) tl
       | Some true -> aux acc tl
       | Some false ->
         if ignore_contradictions
         then aux (hd :: acc) tl
         else [CLeq (FSucc FZero, FZero)])
  in
  aux [] constraints |> List.rev
;;
