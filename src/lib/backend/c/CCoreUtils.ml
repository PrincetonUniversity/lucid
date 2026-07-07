open CCoreSyntax
open CCoreExceptions

let id = Id.create

let cid s = Cid.create [s]




(* monomorphization and code gen sometimes needs type names *)
let ty_to_namestr ty = match ty.raw_ty with 
  | TInt _ | TBool | TName _ -> CCorePPrint.ty_to_string ~use_abstract_name:true ty
  | _ -> err_expected_ty ty "to convert a type to a string for a generated function, the type must be an int, bool, or abstract"
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
let is_tprimative ty = is_tint ty || is_tbool ty
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
    | TList(_, IConst(n)) ->
      emacro_and_fold (List.init (n)
        (fun i ->
          let idx =(eval@@vint i 32)  in
          (((e1/@idx) /& (m/@idx)) /== ((e2/@idx) /& (m/@idx)))
        ))
    | TList(_, IVar _) -> err "cannot generate masked equality exp for vector of unknown length"
    | TPacket -> err "cannot generate masked equality exp for bytes"
    | TPtr(_, None) -> compound_masked_eq (ederef e1) (ederef e2) (ederef m)
    | TName(_, def_opt) -> (match def_opt with
      | Some d -> compound_masked_eq {e1 with ety=d} {e2 with ety=d} {m with ety=d}
      | None -> err "cannot generate masked equality exp for opaque named type")
      (* unbounded lists and unions are problematic *)
    | TPtr(_, Some(_)) -> err "cannot generate equality exp for list of unknown length"
    | TUnion _ -> err "cannot generate equality exp for untagged union"
    (* events and functions -- not sure what to do yet *)
    | TVariant _ -> err "cannot generate equality expression for two events"
    | TFun _ -> err "no equality for function"
    (* builtins and names -- opaque, can't compare *)
    | TBuiltin _ -> err "no equality for builtins"
;;