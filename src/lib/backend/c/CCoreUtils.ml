open CCoreSyntax
open CCoreExceptions

(* [xs with [n] := x] *)
let replace n x xs = 
  let rec replace' n x xs acc = 
    match xs with 
    | [] -> List.rev acc
    | y::ys -> 
      if n = 0 then 
        List.rev_append (x::acc) ys
      else 
        replace' (n-1) x ys (y::acc)
  in
  replace' n x xs []
;;
let id = Id.create

let cid s = Cid.create [s]


let n_bytes n_bits = (* number of bytes required to hold n_bits *) 
  (n_bits + 7) / 8
;;


let is_smatch statement = match statement.s with 
  | SMatch _ -> true 
  | _ -> false
;;

(* monomorphization and code gen sometimes needs type names *)
let ty_to_namestr ty = match ty.raw_ty with 
  | TInt _ | TBool | TAbstract _ -> CCorePPrint.ty_to_string ~use_abstract_name:true ty
  | _ -> err_expected_ty ty "to convert a type to a string for a generated function, the type must be an int, bool, or abstract"
;;
let cid_for_ty cid ty = 
  Cid.str_cons_plain (ty_to_namestr ty) cid
;;


(*** compound-type operation macros ***)

let rec eops op exps = 
  match exps with 
  | [] -> err "no expressions"
  | [exp] -> exp
  | exp::exps -> 
    eop op [exp; eops op exps]
;;

let rec emacro_op_fold op exps = 
  match exps with 
  | [] -> err "no expressions"
  | [exp] -> exp
  | exp::exps -> 
    eop op [exp; eops op exps]
;;
let emacro_and_fold = emacro_op_fold And
let is_tprimative ty = is_tint ty || is_tbool ty || is_tenum ty
;;

(* expand equality expressions *)
let rec compound_eq e1 e2 = 
  let ty = e1.ety in
  if (equiv_tys e1.ety e2.ety <> true) then err "cannot test equality of two different types" else
  match ty.raw_ty with 
    | TUnit -> eval@@vbool true; (* two units are always the same *)
    | TInt _ 
    | TBool  
    | TEnum _ -> (e1 /== e2)
    | TRecord(ids, tys) -> 
      let exps = List.map 
        (fun (id, ty) -> 
          (* primitive types use the equal operator *)
          if (is_tprimative ty) 
            then (e1/.id) /== (e2/.id)
          (* non-primitives get expanded *)
          else (compound_eq (e1/.id) (e2/.id)))
        (List.combine ids tys)
      in
      emacro_and_fold exps
    | TTuple(tys) -> 
      let exps = List.mapi
        (fun i ty ->
          (* primitive types use the equal operator *)
          if (is_tprimative ty) 
            then (e1/.@i) /== (e2/.@i)
          (* non-primitives get expanded *)
          else (compound_eq (e1/.@i) (e2/.@i)))
        tys
      in
      emacro_and_fold exps
    | TPtr(_, Some(IConst(n))) -> 
      emacro_and_fold (List.init (n) (fun i -> (e1/@(eval@@vint i 32) /== (e2/@(eval@@vint i 32)))))
    | TPtr(_, None) -> compound_eq (ederef e1) (ederef e2)
    | TAbstract(_, ty) -> compound_eq {e1 with ety=ty} {e2 with ety=ty}
      (* unbounded lists and unions are problematic *)
    | TPtr(_, Some(_)) -> err "cannot generate equality exp for list of unknown length"
    | TUnion _ -> err "cannot generate equality exp for untagged union"
    (* bits should be removed *)
    | TBits _ -> err "cannot generate equality exp for bitstring"
    (* events and functions -- not sure what to do yet *)
    | TEvent -> err "cannot generate equality expression for two events"
    | TFun _ -> err "no equality for function"
    (* builtins and names -- opaque, can't compare *)
    | TBuiltin _ -> err "no equality for builtins"
    | TName _ -> err "no equality for named type"
;;

(* masked equality expression (e1 & m) == (e2 & m) for all 
   comparable types or compounds of comparable types *)
let rec compound_masked_eq e1 e2 m = 
  let ty = e1.ety in
  if ((equiv_tys e1.ety e2.ety <> true) || (equiv_tys e1.ety m.ety <> true)) then err "type mismatch" else
  match ty.raw_ty with 
    | TUnit -> eval@@vbool true; (* two units are always the same *)
    | TInt _ 
    | TBool -> ((e1 /& m) /== (e2 /& m))
    | TEnum _ -> err "masked equality of enums"
    | TRecord(ids, tys) -> 
      let exps = List.map 
        (fun (id, ty) -> 
          (* primitive types use the equal operator *)
          if (is_tprimative ty) 
            then (((e1/.id) /& (m/.id)) /== ((e2/.id) /& (m/.id)))
          (* non-primitives get expanded *)
          else (compound_masked_eq (e1/.id) (e2/.id) (m/.id)))
        (List.combine ids tys)
      in
      emacro_and_fold exps
    | TTuple(tys) -> 
      let exps = List.mapi
        (fun i ty ->
          (* primitive types use the equal operator *)
          if (is_tprimative ty) 
            then (((e1/.@i) /& (m/.@i)) /== ((e2/.@i) /& (m/.@i)))
          (* non-primitives get expanded *)
        else (compound_masked_eq (e1/.@i) (e2/.@i) (m/.@i)))
        tys
      in
      emacro_and_fold exps
    | TPtr(_, Some(IConst(n))) -> 
      emacro_and_fold (List.init (n) 
        (fun i -> 
          let idx =(eval@@vint i 32)  in 
          (((e1/@idx) /& (m/@idx)) /== ((e2/@idx) /& (m/@idx)))
        ))
    | TPtr(_, None) -> compound_masked_eq (ederef e1) (ederef e2) (ederef m)
    | TAbstract(_, ty) -> compound_masked_eq {e1 with ety=ty} {e2 with ety=ty} {m with ety=ty}
      (* unbounded lists and unions are problematic *)
    | TPtr(_, Some(_)) -> err "cannot generate equality exp for list of unknown length"
    | TUnion _ -> err "cannot generate equality exp for untagged union"
    (* bits should be removed *)
    | TBits _ -> err "cannot generate equality exp for bitstring"
    (* events and functions -- not sure what to do yet *)
    | TEvent -> err "cannot generate equality expression for two events"
    | TFun _ -> err "no equality for function"
    (* builtins and names -- opaque, can't compare *)
    | TBuiltin _ -> err "no equality for builtins"
    | TName _ -> err "no equality for named type"
;;