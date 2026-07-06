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

(* ---- R1' bit-packed serialization layout (see ccore-refactor-notes §21) ----
   Lucid serializes a sequence of fields as one contiguous MSB-first (network)
   bitstream. Restriction R1': a field may cross a byte boundary only if it is
   byte-aligned at its start (offset%8=0) or its end ((offset+n)%8=0). That keeps
   every field within its natural container and makes the codec a single
   load/shift/mask. Validate a width sequence against R1' and require a
   byte-multiple total (rule 3). On the first offending field, return
   Error(index, reason); index = List.length widths flags a non-byte-multiple total. *)
let check_r1_widths (widths : int list) : (unit, int * string) result =
  let rec go off i = function
    | [] ->
      if off mod 8 <> 0
      then Error(i, Printf.sprintf "the fields total %d bits, not a whole number of \
                                    bytes -- pad to a multiple of 8 bits" off)
      else Ok ()
    | n :: rest ->
      let phase = off mod 8 in
      let straddles = phase + n > 8 in
      let aligned = phase = 0 || (off + n) mod 8 = 0 in
      if straddles && not aligned
      then Error(i, Printf.sprintf
        "a %d-bit field at bit-offset %d crosses a byte boundary without aligning to \
         one -- reorder or pad so it starts or ends on a byte boundary (restriction R1')"
        n off)
      else go (off + n) (i + 1) rest
  in
  go 0 0 widths
;;

let is_smatch statement = match statement.s with 
  | SMatch _ -> true 
  | _ -> false
;;

let ends_with_smatch statement = 
  let stmts = to_stmt_block statement in
  match List.rev stmts with
  | [] -> false
  | stmt::_ -> is_smatch stmt
;;

(* monomorphization and code gen sometimes needs type names *)
let ty_to_namestr ty = match ty.raw_ty with 
  | TInt _ | TBool | TName _ -> CCorePPrint.ty_to_string ~use_abstract_name:true ty
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
    | TVec(_, IConst(n)) ->
      emacro_and_fold (List.init (n) (fun i -> (e1/@(eval@@vint i 32) /== (e2/@(eval@@vint i 32)))))
    | TVec(_, IVar _) -> err "cannot generate equality exp for vector of unknown length"
    | TPacket -> err "cannot generate equality exp for bytes"
    | TPtr(_, None) -> compound_eq (ederef e1) (ederef e2)
    | TName cid -> (match tydef_opt cid with
      | Some d -> compound_eq {e1 with ety=d} {e2 with ety=d}
      | None -> err "cannot generate equality exp for opaque named type")
      (* unbounded lists and unions are problematic *)
    | TPtr(_, Some(_)) -> err "cannot generate equality exp for list of unknown length"
    | TUnion _ -> err "cannot generate equality exp for untagged union"
    (* bits should be removed *)
    | TBits _ -> err "cannot generate equality exp for bitstring"
    (* events and functions -- not sure what to do yet *)
    | TVariant _ -> err "cannot generate equality expression for two events"
    | TFun _ -> err "no equality for function"
    (* builtins and names -- opaque, can't compare *)
    | TBuiltin _ -> err "no equality for builtins"
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
    | TVec(_, IConst(n)) ->
      emacro_and_fold (List.init (n)
        (fun i ->
          let idx =(eval@@vint i 32)  in
          (((e1/@idx) /& (m/@idx)) /== ((e2/@idx) /& (m/@idx)))
        ))
    | TVec(_, IVar _) -> err "cannot generate masked equality exp for vector of unknown length"
    | TPacket -> err "cannot generate masked equality exp for bytes"
    | TPtr(_, None) -> compound_masked_eq (ederef e1) (ederef e2) (ederef m)
    | TName cid -> (match tydef_opt cid with
      | Some d -> compound_masked_eq {e1 with ety=d} {e2 with ety=d} {m with ety=d}
      | None -> err "cannot generate masked equality exp for opaque named type")
      (* unbounded lists and unions are problematic *)
    | TPtr(_, Some(_)) -> err "cannot generate equality exp for list of unknown length"
    | TUnion _ -> err "cannot generate equality exp for untagged union"
    (* bits should be removed *)
    | TBits _ -> err "cannot generate equality exp for bitstring"
    (* events and functions -- not sure what to do yet *)
    | TVariant _ -> err "cannot generate equality expression for two events"
    | TFun _ -> err "no equality for function"
    (* builtins and names -- opaque, can't compare *)
    | TBuiltin _ -> err "no equality for builtins"
;;