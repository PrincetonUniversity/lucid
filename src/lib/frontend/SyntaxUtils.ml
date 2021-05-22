open Syntax
open Batteries
open Collections

let raw_integer v =
  match v.v with
  | VInt i -> i
  | _ -> error "not integer"
;;

let raw_bool v =
  match v.v with
  | VBool b -> b
  | _ -> error "not boolean"
;;

let raw_event v =
  match v.v with
  | VEvent e -> e
  | _ -> error "not event"
;;

let extract_sizes gty = snd gty
let gname gty = Id.name (fst gty)

(* Turns an effect into a base effect plus a list of offsets. E.g.
   FSucc (FSucc (FProj (FSucc (FProj (FZero)))))) maps to
   FZero, [0; 1 ; 2] *)
let unwrap_effect eff =
  let rec aux eff =
    match eff with
    | FZero -> FZero, [0]
    | FVar (QVar _) | FVar (TVar { contents = Unbound _ }) -> eff, [0]
    | FVar (TVar { contents = Link eff }) -> aux eff
    | FSucc eff ->
      let base, lst = aux eff in
      let lst =
        match lst with
        | [] -> failwith "impossible"
        | hd :: tl -> (hd + 1) :: tl
      in
      base, lst
    | FProj eff ->
      let base, lst = aux eff in
      base, 0 :: lst
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
  | hd :: tl ->
    List.fold_left (fun acc n -> add_succs (FProj acc) n) (add_succs base hd) tl
;;

(******************************************)
(* Normalization and comparison functions *)
(******************************************)

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

(* If an ISum, the list is non-emoty, sorted, and no entries are Links *)
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
    begin
      (* If the result is still a sum, then sort the list; otherwise, just return
         the result *)
      match recursively_normalize vs with
      | ISum ([], _) -> failwith "Sanity check: this should never happen"
      | ISum ([s], 0) -> s
      | ISum (vs, n) -> ISum (List.sort Pervasives.compare vs, n)
      | sz -> sz
    end
  | s -> s
;;

let extract_size s =
  match normalize_size s with
  | IConst n -> n
  | _ -> failwith "Not yet implemented"
;;

let rec equiv_lists f lst1 lst2 =
  match lst1, lst2 with
  | [], [] -> true
  | hd1 :: tl1, hd2 :: tl2 -> f hd1 hd2 && equiv_lists f tl1 tl2
  | _ -> false
;;

let rec equiv_size s1 s2 =
  match normalize_size s1, normalize_size s2 with
  | IConst n1, IConst n2 -> n1 = n2
  | IUser id1, IUser id2 -> Cid.equal id1 id2
  | ISum (vs1, n1), ISum (vs2, n2) -> n1 = n2 && equiv_lists equiv_size vs1 vs2
  | IVar tqv, s | s, IVar tqv -> STQVar.equiv_tqvar equiv_size tqv s
  | _ -> false
;;

let rec equiv_effect e1 e2 =
  match e1, e2 with
  | FZero, FZero -> true
  | FSucc e1', FSucc e2' | FProj e1', FProj e2' -> equiv_effect e1' e2'
  | FVar tqv, e | e, FVar tqv -> FTQVar.equiv_tqvar equiv_effect tqv e
  | _ -> false
;;

let equiv_constraints lst1 lst2 =
  List.for_all2
    (fun (CLeq (e1, e2)) (CLeq (e1', e2')) ->
      equiv_effect e1 e1' && equiv_effect e2 e2')
    (List.sort Pervasives.compare lst1)
    (List.sort Pervasives.compare lst2)
;;

let rec normalize_tfun func =
  let count = ref 0 in
  let v =
    object
      inherit [_] s_map

      val mutable renaming : id IdMap.t = IdMap.empty

      method! visit_QVar _ _ id =
        match IdMap.find_opt id renaming with
        | Some id' -> QVar id'
        | None ->
          let new_id = Id.create ("norm" ^ string_of_int !count) in
          renaming <- IdMap.add id new_id renaming;
          QVar new_id
    end
  in
  v#visit_func_ty () func
;;

let equiv_global_ty (id1, sizes1) (id2, sizes2) =
  Cid.equals id1 id2 && equiv_lists equiv_size sizes1 sizes2
;;

let rec equiv_raw_ty ty1 ty2 =
  match ty1, ty2 with
  | TBool, TBool | TVoid, TVoid | TGroup, TGroup -> true
  | TInt size1, TInt size2 -> equiv_size size1 size2
  | TEvent b1, TEvent b2 -> b1 = b2
  | TMemop (size1, ty1), TMemop (size2, ty2) ->
    equiv_size size1 size2 && equiv_raw_ty ty1 ty2
  | TGlobal (gty1, eff1), TGlobal (gty2, eff2) ->
    equiv_global_ty gty1 gty2 && equiv_effect eff1 eff2
  | TFun func1, TFun func2 ->
    let func1 = normalize_tfun func1 in
    let func2 = normalize_tfun func2 in
    equiv_lists equiv_raw_ty func1.arg_tys func2.arg_tys
    && equiv_raw_ty func2.ret_ty func2.ret_ty
    && equiv_effect func1.start_eff func2.start_eff
    && equiv_effect func1.end_eff func2.end_eff
    && equiv_constraints !(func1.constraints) !(func2.constraints)
  | TQVar tqv, ty | ty, TQVar tqv -> TyTQVar.equiv_tqvar equiv_raw_ty tqv ty
  | ( ( TBool
      | TMemop _
      | TInt _
      | TEvent _
      | TGlobal _
      | TFun _
      | TVoid
      | TGroup )
    , _ ) -> false

and equiv_ty ty1 ty2 = equiv_raw_ty ty1.raw_ty ty2.raw_ty

let max_effect e1 e2 =
  let base1, lst1 = unwrap_effect e1 in
  let base2, lst2 = unwrap_effect e2 in
  if not (equiv_effect base1 base2)
  then None
  else Some (wrap_effect base1 (max lst1 lst2))
;;

(* TODO: Delete this if it doesn't end up useful

(* packets, events *)

(* a packet will be represented as a list of 2 values for now; src then dst *)
type packet = value list

let packet src dst = [vinteger src; vinteger dst]

let src p =
  match p with
  | [src; _] -> raw_integer src
  | _ -> error "bad packet; wrong number of values"
;;

let dst p =
  match p with
  | [_; dst] -> raw_integer dst
  | _ -> error "bad packet; wrong number of values"
;;

(* packet_in event name *)
let packet_in = Cid.create ["packetin"]

let packet_in_event p =
  { eid = packet_in; data = p; edelay = 0; elocations = [] }
;;
*)
