open Batteries
module S = Syntax
module C = CoreSyntax

let err span str =
  Console.error_position span
  @@ "Internal error: Cannot translate "
  ^ str
  ^ " to backend syntax"
;;

let err_unsupported span str =
  Console.error_position span
  @@ "cannot translate to backend syntax. the construct "
  ^ str
  ^ " does not exist in the backend and should have been eliminated"
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
    | S.TMemop (n, sz) -> C.TMemop (n, translate_size sz)
    | S.TFun fty ->
      C.TFun
        { arg_tys = List.map translate_ty fty.arg_tys
        ; ret_ty = translate_ty fty.ret_ty
        }
    | S.TVoid -> C.TBool (* Dummy translation needed for foreign functions *)
    | _ -> err ty.tspan (Printing.ty_to_string ty)
  in
  { raw_ty; tspan = ty.tspan }
and translate_asig asig = 
  let (name, tys) = asig in
  (name, List.map (fun rty -> (translate_ty (S.ty rty)).raw_ty) tys;)
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
  | S.Slice (lo, hi) -> C.Slice (translate_size lo, translate_size hi)
  | S.TGet _ -> err Span.default "tuple get operator"
;;

let translate_pattern (p : S.pat) : C.pat =
  match p with
  | S.PWild -> C.PWild
  | S.PNum n -> C.PNum n
  | S.PBit ns -> C.PBit ns
  | S.PVar (_, sp) -> err sp "variable pattern"
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
    | S.ETableCreate(_) -> err e.espan "tables not implemented in backend"
    | S.ETableApply(_) -> err e.espan "tables not implemented in backend"
    | ESizeCast(_) | EStmt(_) | ERecord(_) | EWith(_) | EProj(_)
    | EVector(_) | EComp(_) | EIndex(_) | ETuple(_) -> err_unsupported e.espan (Printing.exp_to_string e)
  in
  { e = e'; ety = translate_ty (Option.get e.ety); espan = e.espan }

and translate_params params =
  List.map (fun (id, ty) -> id, translate_ty ty) params

and translate_body (params, stmt) =
  translate_params params, translate_statement stmt

and translate_acn acn =
  let (name, body) = acn in
  (name, translate_body body)
and translate_case case =
  let (pats, acn, args) = case in
  (List.map translate_pattern pats, acn, List.map translate_exp args)

and translate_gen_type = function
  | S.GSingle eo -> C.GSingle (Option.map translate_exp eo)
  | S.GMulti e -> C.GMulti (translate_exp e)
  | S.GPort e -> C.GPort (translate_exp e)


and translate_statement (s : S.statement) : C.statement =
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
  { s = s'; sspan = s.sspan; spragma = None}
;;

let translate_memop body =
  match body with
  | S.MBReturn e -> C.MBReturn (translate_exp e)
  | S.MBIf (e1, e2, e3) ->
    C.MBIf (translate_exp e1, translate_exp e2, translate_exp e3)
  | S.MBComplex { b1; b2; cell1; cell2; extern_calls; ret } ->
    let translate_b = Option.map (fun (id, e) -> id, translate_exp e) in
    let translate_cro =
      Option.map (fun (e1, e2) -> translate_exp e1, translate_exp e2)
    in
    let translate_calls calls =
      List.map (fun (cid, es) -> cid, List.map translate_exp es) calls
    in
    C.MBComplex
      { b1 = translate_b b1
      ; b2 = translate_b b2
      ; cell1 = translate_cro (fst cell1), translate_cro (snd cell1)
      ; cell2 = translate_cro (fst cell2), translate_cro (snd cell2)
      ; extern_calls = translate_calls extern_calls
      ; ret = translate_cro ret
      }
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
    | S.DMemop (id, params, body) ->
      C.DMemop (id, translate_params params, translate_memop body)
    | S.DExtern (id, ty) -> C.DExtern (id, translate_ty ty)
    | _ -> err d.dspan (Printing.decl_to_string d)
  in
  { d = d'; dspan = d.dspan }
;;

let translate_prog (ds : S.decls) : C.decls = List.map translate_decl ds
