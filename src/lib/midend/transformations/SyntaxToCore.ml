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
    | S.TTable tbl ->
      let ty_to_size (ty : S.ty) = 
        match ty.raw_ty with 
        | TInt(sz) -> sz
        | TBool -> IConst(1)
        | _ -> S.error "[rty_to_size] expected an integer, but got something else"
      in
      let tkey_sizes = List.map translate_size (List.map ty_to_size tbl.tkey_sizes) in
      let tparam_tys = List.map translate_ty tbl.tparam_tys in
      let tret_tys = List.map translate_ty tbl.tret_tys in
      C.TTable { tkey_sizes; tparam_tys; tret_tys }
    | S.TAction a ->
      let aconst_param_tys = List.map translate_ty a.aconst_param_tys in
      let aparam_tys = List.map translate_ty a.aparam_tys in
      let aret_tys = List.map translate_ty a.aret_tys in
      C.TAction { aconst_param_tys; aparam_tys; aret_tys }
    | S.TPat s -> C.TPat (translate_size s)
    | S.TBitstring -> C.TBits (1500)
    | _ -> err ty.tspan (Printing.ty_to_string ty)
  in
  { raw_ty; tspan = ty.tspan }

and translate_asig asig =
  let name, tys = asig in
  name, List.map (fun rty -> (translate_ty (S.ty rty)).raw_ty) tys
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
  | S.PatExact -> C.PatExact
  | S.PatMask -> C.PatMask
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
      C.VEvent { eid; data = List.map translate_value data; edelay; evnum = None; eserialized=false }
    | S.VPat(S.PBit bs) -> C.VPat bs
    | S.VPat(S.PWild) -> S.error "wildcard pattern should be translated as a special case of expression"
    | S.VPat _ -> err v.vspan "pattern VALUES should all be converted into PBits before backend"
  in
  { v = v'; vty = translate_ty (Option.get v.vty); vspan = v.vspan }
;;

let rec translate_exp (e : S.exp) : C.exp =
  let e' =
    match e.e with
    (* special case for wildcard value -- TODO: make wildcard a value in backend too *)
    | S.EVal v when v.v = (S.VPat(S.PWild)) -> 
      let ty = Option.get e.ety in
      let sz = match ty.raw_ty with 
        | TPat sz -> sz
        | _ -> S.error "[translate_exp] expected a pattern type, but got something else"
      in      
      (* the size must be resolved by now *)
      C.EVal (C.vwild (SyntaxUtils.extract_size (sz)))
    | S.EVal v -> C.EVal (translate_value v)
    | S.EInt (z, szo) ->
      let sz = Option.default 32 (Option.map translate_size szo) in
      C.EVal (C.vint (Z.to_int z) sz)
    | S.EVar cid -> C.EVar cid
    | S.EOp (op, es) -> C.EOp (translate_op op, List.map translate_exp es)
    | S.ECall (cid, es, unordered) -> C.ECall (cid, List.map translate_exp es, unordered)
    | S.EHash (sz, es) -> C.EHash (translate_size sz, List.map translate_exp es)
    | S.EFlood e -> C.EFlood (translate_exp e)
    | ESizeCast _
    | EStmt _
    | ERecord _
    | EWith _
    | EProj _
    | EVector _
    | EComp _
    | EIndex _
    | ETuple _ -> err_unsupported e.espan (Printing.exp_to_string e)
    | S.ETableCreate _ ->
      err
        e.espan
        "[SyntaxToCore.translate_exp] ETableCreate should be translated by \
         special function"
    | S.ETableMatch _ ->
      err e.espan "table match exps should have been eliminated before IR."
    (* | S.EPatWild (Some sz) -> C.EVal (C.vwild (translate_size sz))
    | S.EPatWild None ->
      err e.espan "wildcard patterns (_) should have a size before IR." *)
  in
  { e = e'; ety = translate_ty (Option.get e.ety); espan = e.espan }

and translate_etablecreate id (exp : S.exp) : C.exp =
  match exp.e with
  | S.ETableCreate tc ->
    let tty = translate_ty tc.tty in
    let tactions = List.map translate_exp tc.tactions in
    let tsize = translate_exp tc.tsize in
    let default_cid, default_args, _ = SyntaxUtils.unpack_default_action tc.tdefault.e in
    let tdefault = default_cid, default_args |> List.map translate_exp in
    let e' = C.ETableCreate { tid = id; tty; tactions; tsize; tdefault } in
    { e = e'; ety = translate_ty (Option.get exp.ety); espan = exp.espan }
  | _ ->
    err
      exp.espan
      "[SyntaxToCore.translate_etablecreate] non table create expressions \
       should be translated by translate_exp"

and translate_params params =
  List.map (fun (id, ty) -> id, translate_ty ty) params

and translate_body (params, stmt) =
  translate_params params, translate_statement stmt

and translate_acn acn =
  let name, body = acn in
  name, translate_body body

and translate_case case =
  let pats, acn, args = case in
  List.map translate_pattern pats, acn, List.map translate_exp args

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
  let translate_entry (entry : S.tbl_entry) : C.tbl_entry =
    let action_cid, action_args, _ = SyntaxUtils.unpack_default_action entry.eaction.e in
    { ematch = List.map translate_exp entry.ematch
    ; eprio = entry.eprio
    ; eaction = Cid.to_id action_cid
    ; eargs = List.map translate_exp action_args
    }
  in
  let s' =
    match s.s with
    | S.SNoop -> C.SNoop
    | S.SUnit e -> C.SUnit (translate_exp e)
    | S.SLocal (id, ty, e) -> C.SLocal (id, translate_ty ty, translate_exp e)
    | S.SAssign (id, e) -> C.SAssign (Cid.id id, translate_exp e)
    | S.SPrintf (str, es) -> C.SPrintf (str, List.map translate_exp es)
    | S.SIf (e, s1, s2) ->
      C.SIf (translate_exp e, translate_statement s1, translate_statement s2)
    | S.SGen (g, e) -> C.SGen (translate_gen_type g, translate_exp e)
    | S.SSeq (s1, s2) -> C.SSeq (translate_statement s1, translate_statement s2)
    | S.SMatch (es, branches) ->
      C.SMatch (List.map translate_exp es, List.map translate_branch branches)
    | S.SRet eopt -> C.SRet (Option.map translate_exp eopt)
    | S.STableMatch tm ->
      C.STableMatch
        { C.tbl = translate_exp tm.tbl
        ; C.keys = List.map translate_exp tm.keys
        ; C.args = List.map translate_exp tm.args
        ; C.outs = tm.outs
        ; C.out_tys =
            (match tm.out_tys with
             | None -> None
             | Some otys -> Some (List.map translate_ty otys))
        }
    | S.STableInstall (tbl_exp, entries) ->
      C.STableInstall (translate_exp tbl_exp, List.map translate_entry entries)
    | _ -> err s.sspan (Printing.statement_to_string s)
  in
  { s = s'; sspan = s.sspan; spragmas = s.spragmas }
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
  | S.EPacket -> C.EPacket
  | S.EBackground -> C.EBackground
;;

let translate_hsort = function
  | S.HControl -> C.HControl
  | S.HData -> C.HData
  | S.HEgress -> C.HEgress
;;

let translate_parser_action = function
  | S.PRead(id, ty, exp) -> C.PRead (Cid.id id, translate_ty ty, translate_exp exp)
  | S.PSkip ty -> C.PSkip (translate_ty ty)
  | S.PAssign (lexp, rexp) ->
    let id =
      match lexp.e with
      | EVar cid -> Cid.to_id cid
      | _ -> failwith "Internal error: SyntaxToCore PAssign"
    in
    C.PAssign (Cid.id id, translate_exp rexp)
  | S.PLocal(id, ty, exp) ->
    C.PLocal (Cid.id id, translate_ty ty, translate_exp exp) 
;;

let rec translate_branch (pat, block) =
  [translate_pattern pat], translate_parser_block block

and translate_parser_step = function
  | S.PGen e -> C.PGen (translate_exp e)
  | S.PCall e -> C.PCall (translate_exp e)
  | S.PMatch (e, bs) -> C.PMatch ([translate_exp e], List.map translate_branch bs)
  | S.PDrop -> C.PDrop

and translate_parser_block (actions, (step, step_span)) =
  {pactions=List.map (fun (a, sp) -> translate_parser_action a, sp) actions;
   pstep=(translate_parser_step step, step_span);}
;;

let translate_decl (d : S.decl) : C.decl option =
  match d.d with 
  | DUserTy _ -> None 
  | _ -> 
    let dprag = ref None in
    let d' =
      match d.d with
      | S.DGlobal (id, ty, inner_exp) ->
        (match inner_exp.e with
        | ETableCreate _ ->
          C.DGlobal (id, translate_ty ty, translate_etablecreate id inner_exp)
        | _ -> C.DGlobal (id, translate_ty ty, translate_exp inner_exp))
      | S.DEvent (id, annot, sort, _, params) ->
        C.DEvent (id, annot, translate_sort sort, translate_params params)
      | S.DHandler (id, s, body) ->
        C.DHandler (id, translate_hsort s, translate_body body)
      | S.DMemop (mid, mparams, mbody) ->
        C.DMemop
          { mid
          ; mparams = translate_params mparams
          ; mbody = translate_memop mbody
          }
      | S.DExtern (id, ty) -> C.DExtern (id, translate_ty ty)
      | S.DAction (id, tys, const_params, (params, acn_body)) ->
        C.DAction
          { aid = id
          ; artys = List.map translate_ty tys
          ; aconst_params = translate_params const_params
          ; aparams = translate_params params
          ; abody = List.map translate_exp acn_body
          }
      | S.DParser (id, params, parser_block) ->
        C.DParser
          (id, translate_params params, translate_parser_block parser_block)
      | S.DUserTy _ -> err d.dspan "user/named types not yet supported in ir"
    | S.DFun(id, ty, _, (params, stmt)) when (Pragma.exists_sprag "main" [] d.dpragmas) -> 
        (* retain "main" pragma *)
        dprag := Some(List.hd (d.dpragmas));
        C.DFun(id, translate_ty ty, (translate_params params, translate_statement stmt))
      | _ -> err d.dspan (Printing.decl_to_string d)
    in
    (* retain "main" pragma *)
    match !dprag with 
      | None -> 
        Some(C.decl_sp d' d.dspan)
      | Some(dprag) -> 
        Some({(C.decl_sp d' d.dspan) with dpragma = Some(dprag)})

;;

let translate_prog (ds : S.decls) : C.decls = List.filter_map translate_decl ds
