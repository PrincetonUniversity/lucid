open Batteries
module S = Syntax
module C = CoreSyntax

let err span str =
  Console.error_position span
  @@ "Internal error: Cannot translate "
  ^ str
  ^ " to backend syntax"
;;

let translate_size (sz : S.size) : C.size =
  SyntaxUtils.extract_size_default sz 32
;;

let rec translate_ty (ty : S.ty) : C.ty =
  let raw_ty =
    match S.TyTQVar.strip_links ty.raw_ty with
    | S.TBool -> C.TBool
    | S.TGroup -> C.TGroup
    | S.TEvent -> C.TEvent
    | S.TInt sz -> C.TInt (translate_size sz)
    | S.TName (cid, sizes, b) -> C.TName (cid, List.map translate_size sizes, b)
    | S.TMemop (sz1, sz2) -> C.TMemop (translate_size sz1, translate_size sz2)
    | S.TFun fty ->
      C.TFun
        { arg_tys = List.map translate_ty fty.arg_tys
        ; ret_ty = translate_ty fty.ret_ty
        }
    | _ -> err ty.tspan (Printing.ty_to_string ty)
  in
  { raw_ty; tspan = ty.tspan }
;;

let translate_op (op : S.op) : C.op =
  match op with
  | S.And -> C.And
  | S.Or -> C.Or
  | S.Not -> C.Not
  | S.Eq -> C.Eq
  | S.Neq -> C.Neq
  | S.Less -> C.Less
  | S.More -> C.More
  | S.Leq -> C.Leq
  | S.Geq -> C.Geq
  | S.Neg -> C.Neg
  | S.Plus -> C.Plus
  | S.Sub -> C.Sub
  | S.SatPlus -> C.SatPlus
  | S.SatSub -> C.SatSub
  | S.Cast size -> C.Cast (translate_size size)
  | S.Conc -> C.Conc
  | S.BitAnd -> C.BitAnd
  | S.BitOr -> C.BitOr
  | S.BitXor -> C.BitXor
  | S.BitNot -> C.BitNot
  | S.LShift -> C.LShift
  | S.RShift -> C.RShift
  | S.Slice (lo, hi) -> C.Slice (lo, hi)
  | S.TGet _ -> err Span.default "tuple get operator"
;;

let translate_pattern (p : S.pat) : C.pat =
  match p with
  | S.PWild -> C.PWild
  | S.PNum n -> C.PNum n
  | S.PBit ns -> C.PBit ns
;;

let rec translate_value (v : S.value) : C.value =
  let v' =
    match v.v with
    | S.VBool b -> C.VBool b
    | S.VInt z -> C.VInt z
    | S.VGlobal n -> C.VGlobal n
    | S.VGroup ls -> C.VGroup ls
    | VEvent { eid; data; edelay } ->
      C.VEvent { eid; data = List.map translate_value data; edelay }
  in
  { v = v'; vty = translate_ty (Option.get v.vty); vspan = v.vspan }
;;

let rec translate_exp (e : S.exp) : C.exp =
  let e' =
    match e.e with
    | S.EVal v -> C.EVal (translate_value v)
    | S.EInt (z, szo) ->
      let sz = Option.default 32 (Option.map translate_size szo) in
      C.EVal (C.vint (Z.to_int z) sz)
    | S.EVar cid -> C.EVar cid
    | S.EOp (op, es) -> C.EOp (translate_op op, List.map translate_exp es)
    | S.ECall (cid, es) -> C.ECall (cid, List.map translate_exp es)
    | S.EHash (sz, es) -> C.EHash (translate_size sz, List.map translate_exp es)
    | S.EFlood e -> C.EFlood (translate_exp e)
    | _ -> err e.espan (Printing.exp_to_string e)
  in
  { e = e'; ety = translate_ty (Option.get e.ety); espan = e.espan }
;;

let translate_gen_type = function
  | S.GSingle eo -> C.GSingle (Option.map translate_exp eo)
  | S.GMulti e -> C.GMulti (translate_exp e)
  | S.GPort e -> C.GPort (translate_exp e)
;;

let rec translate_statement (s : S.statement) : C.statement =
  (* (match s.s with
  | SSeq _ | SNoop -> ()
  | _ -> print_endline @@ "Translating " ^ Printing.statement_to_string s); *)
  let translate_branch (ps, s) =
    List.map translate_pattern ps, translate_statement s
  in
  let s' =
    match s.s with
    | S.SNoop -> C.SNoop
    | S.SUnit e -> C.SUnit (translate_exp e)
    | S.SLocal (id, ty, e) -> C.SLocal (id, translate_ty ty, translate_exp e)
    | S.SAssign (id, e) -> C.SAssign (id, translate_exp e)
    | S.SPrintf (str, es) -> C.SPrintf (str, List.map translate_exp es)
    | S.SIf (e, s1, s2) ->
      C.SIf (translate_exp e, translate_statement s1, translate_statement s2)
    | S.SGen (g, e) -> C.SGen (translate_gen_type g, translate_exp e)
    | S.SSeq (s1, s2) -> C.SSeq (translate_statement s1, translate_statement s2)
    | S.SMatch (es, branches) ->
      C.SMatch (List.map translate_exp es, List.map translate_branch branches)
    | S.SRet eopt -> C.SRet (Option.map translate_exp eopt)
    | _ -> err s.sspan (Printing.statement_to_string s)
  in
  { s = s'; sspan = s.sspan }
;;

let translate_params params =
  List.map (fun (id, ty) -> id, translate_ty ty) params
;;

let translate_body (params, stmt) =
  translate_params params, translate_statement stmt
;;

let translate_sort = function
  | S.EEntry b -> C.EEntry b
  | S.EExit -> C.EExit
  | S.EBackground -> C.EBackground
;;

let translate_decl (d : S.decl) : C.decl =
  let d' =
    match d.d with
    | S.DGlobal (id, ty, exp) ->
      C.DGlobal (id, translate_ty ty, translate_exp exp)
    | S.DEvent (id, sort, _, params) ->
      C.DEvent (id, translate_sort sort, translate_params params)
    | S.DHandler (id, body) -> C.DHandler (id, translate_body body)
    | S.DMemop (id, body) -> C.DMemop (id, translate_body body)
    | S.DExtern (id, ty) -> C.DExtern (id, translate_ty ty)
    | _ -> err d.dspan (Printing.decl_to_string d)
  in
  { d = d'; dspan = d.dspan }
;;

let translate_prog (ds : S.decls) : C.decls = List.map translate_decl ds
