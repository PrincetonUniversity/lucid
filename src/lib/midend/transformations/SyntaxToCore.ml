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
  match (S.STQVar.strip_links sz) with 
  | S.IConst i -> C.Sz i
  | S.ITup is -> C.Szs (
    List.map (fun inner_sz -> match inner_sz with | S.IConst i -> i | _ -> 
      S.error@@"[translate_size] expected a constant size, but got "
        ^(Printing.size_to_string inner_sz)
      ) 
      is
    )
  | _ -> C.Sz(SyntaxUtils.extract_size_default sz 32)
;;
let sz_int sz = match sz with | S.IConst i -> i | _ -> S.error "[sz_int] expected a constant size, but got something else";;

let rec translate_raw_ty (rty : S.raw_ty) tspan : C.raw_ty = 
  match S.TyTQVar.strip_links rty with
  | S.TBool -> C.TBool
  | S.TGroup -> C.TGroup
  | S.TEvent -> C.TEvent
  | S.TInt sz -> C.TInt (translate_size sz)
  (* TABLE UPDATE hard coded translation into table type *)
  (* | S.TName(cid, sizes, _) when (Cid.equals cid Tables.t_id) -> 
    let size_to_ty (sz : S.size) = 
      C.ty (C.TInt (translate_size sz))
    in
    let tkey_sizes, tparam_tys, tret_tys = match (List.map (SyntaxUtils.normalize_size) sizes) with 
      | [ITup(skeys); ITup(sparams); ITup(srets)] -> (
        List.map translate_size skeys,
        List.map size_to_ty sparams,
        List.map size_to_ty srets
      )
      | _ -> S.error@@"[translate_raw_ty] expected 3 size arguments, each a tuple, but got something else"
    in
    C.TTable { tkey_sizes; tparam_tys; tret_tys } *)
  | S.TName (cid, sizes, _) -> C.TName (cid, List.map translate_size sizes)
  | S.TMemop (n, sz) -> C.TMemop (n, translate_size sz)
  | S.TFun fty ->
    C.TFun
      { arg_tys = List.map translate_ty fty.arg_tys
      ; ret_ty = translate_ty fty.ret_ty
      }
  | S.TVoid -> C.TBool (* Dummy translation needed for foreign functions *)
  | S.TBuiltin(cid, raw_tys, _) when Cid.equals cid Tables.t_id -> 
    (* let raw_tys = List.map (fun rty -> translate_raw_ty rty tspan) raw_tys in *)
    let key_raw_ty = List.nth raw_tys 0 |> S.TyTQVar.strip_links in
    let install_raw_ty = List.nth raw_tys 1 |> S.TyTQVar.strip_links in
    (* install time param not currently used in backend *)
    let param_raw_ty = List.nth raw_tys 2 |> S.TyTQVar.strip_links in
    let ret_raw_ty = List.nth raw_tys 3 |> S.TyTQVar.strip_links in
    let rec flatten (raw_ty : S.raw_ty) : S.raw_ty list = 
      match raw_ty with 
        | S.TTuple(tys) -> 
          List.map flatten tys |> List.concat
        | _ -> [raw_ty]
    in
    let key_raw_tys = flatten key_raw_ty in
    let install_raw_tys = flatten install_raw_ty in
    let param_raw_tys = flatten param_raw_ty in
    let ret_raw_tys = flatten ret_raw_ty in

    (* err_unsupported tspan "TBuiltin IR translation not implemented" *)
    let rawty_to_intsize (raw_ty : S.raw_ty) = 
      match S.TyTQVar.strip_links (raw_ty) with 
      | TInt(sz) -> SyntaxUtils.extract_size sz
      | TBool ->1
      | _ -> 
        print_endline (Printing.raw_ty_to_string raw_ty);
        S.error "[rty_to_size] expected an integer, but got something else"
    in

    let tkey_sizes = C.Szs (List.map rawty_to_intsize key_raw_tys) in
    let tinstall_sizes = C.Szs (List.map rawty_to_intsize install_raw_tys) in
    let tparam_sizes = C.Szs (List.map rawty_to_intsize param_raw_tys) in
    let tret_sizes = C.Szs (List.map rawty_to_intsize ret_raw_tys) in
    C.TName(Tables.t_id, [tkey_sizes; tinstall_sizes; tparam_sizes; tret_sizes])
  | S.TBuiltin _ -> 
    failwith "builtins besides tables not implemented as TBuiltin"
  | S.TTable tbl ->
    let ty_to_intsize (ty : S.ty) = 
      match ty.raw_ty with 
      | TInt(sz) -> SyntaxUtils.extract_size sz
      | TBool ->1
      | _ -> S.error "[rty_to_size] expected an integer, but got something else"
    in
    let tkey_sizes = C.Szs (List.map ty_to_intsize tbl.tkey_sizes) in
    let tparam_sizes = C.Szs (List.map ty_to_intsize tbl.tparam_tys) in
    let tret_sizes = C.Szs (List.map ty_to_intsize tbl.tret_tys) in
    C.TName(Tables.t_id, [tkey_sizes; tparam_sizes; tret_sizes])
    (* let tparam_tys = List.map translate_ty tbl.tparam_tys in
    let tret_tys = List.map translate_ty tbl.tret_tys in

    C.TTable { tkey_sizes; tparam_tys; tret_tys } *)
  | S.TActionConstr a ->
    let aconst_param_tys = List.map translate_ty a.aconst_param_tys in
    let aarg_tys = List.map translate_ty a.aacn_ty.aarg_tys in
    let aret_tys = List.map translate_ty a.aacn_ty.aret_tys in
    C.TActionConstr { aconst_param_tys; aacn_ty={aarg_tys; aret_tys }}
  | S.TPat s -> C.TPat (translate_size s)
  | S.TBitstring -> C.TBits (Sz 1500)
  | S.TRecord fields -> C.TRecord (List.map (fun (id, ty) -> (Id.create id), translate_raw_ty ty tspan) fields)
  | S.TAction a ->
    let aarg_tys = List.map translate_ty a.aarg_tys in
    let aret_tys = List.map translate_ty a.aret_tys in
    C.TAction { aarg_tys; aret_tys } 
    (* err tspan "action type should have been eliminated before backend" *)
  | S.TTuple tys -> C.TTuple (List.map (fun ty -> translate_raw_ty ty tspan) tys)
  | S.TVector(raw_ty, sz) -> 
    let len = SyntaxUtils.extract_size sz in 
    let inner_rtys = List.init len (fun _ -> raw_ty) in
    C.TTuple (List.map (fun ty -> translate_raw_ty ty tspan) inner_rtys)
  | S.TQVar _ -> err_unsupported tspan "quantified type"
  | S.TAbstract _ -> err_unsupported tspan "abstract type"    
and translate_ty (ty : S.ty) : C.ty =
  {
    raw_ty = translate_raw_ty ty.raw_ty ty.tspan;
    tspan = ty.tspan;
  }
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
  | S.Slice (lo, hi) -> C.Slice (sz_int lo, sz_int hi)
  | S.TGet _ -> err Span.default "tuple get operator"
  | S.PatExact -> C.PatExact
  | S.PatMask -> C.PatMask
;;


let rec translate_value (v : S.value) : C.value =
  let v' =
    match v.v with
    | S.VBool b -> C.VBool b
    | S.VInt z -> C.VInt z
    | S.VGlobal(id, n) -> C.VGlobal(id, n) (* note: S.VGlobal never appears as of 1/2024 *)
    | S.VGroup ls -> C.VGroup ls
    | VEvent { eid; data; edelay } ->
      C.VEvent { eid; data = List.map translate_value data; edelay; evnum = None; eserialized=false }
    | S.VPat(S.PBit bs) -> C.VPat bs
    | S.VPat(S.PWild) -> S.error "wildcard pattern should be translated as a special case of expression"
    | S.VPat _ -> err v.vspan "pattern VALUES should all be converted into PBits before backend"
  in
  { v = v'; vty = translate_ty (Option.get v.vty); vspan = v.vspan }

and translate_pattern (p : S.pat) : C.pat =
  match p with
  | S.PWild -> C.PWild
  | S.PNum n -> C.PNum n
  | S.PBit ns -> C.PBit ns
  | S.PVar (_, sp) -> err sp "variable pattern"
  | S.PEvent(eventcid, params) -> C.PEvent(eventcid, translate_params params)

and translate_exp (e : S.exp) : C.exp =
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
      let core_szo_opt = Option.map translate_size szo in
      let core_szo = Option.default 
        (C.Sz 32)
        core_szo_opt
      in
      let core_szo_int = match core_szo with 
        | C.Sz i -> i
        | _ -> S.error "[translate_exp] expected a constant size, but got something else"
      in
      C.EVal (C.vint (Z.to_int z) core_szo_int)
    | S.EVar cid -> C.EVar cid
    | S.EOp (op, es) -> C.EOp (translate_op op, List.map translate_exp es)
    | S.ECall (cid, es, unordered) when SyntaxUtils.is_ecall_builtin e -> 
      (* TODO: special processing for builtins *)
      C.ECall (cid, List.map translate_exp es, unordered)

    | S.ECall (cid, es, unordered) -> 
      (* TODO: special processing for builtins *)
      C.ECall (cid, List.map translate_exp es, unordered)
    | S.EHash (sz, es) -> C.EHash (translate_size sz, List.map translate_exp es)
    | S.EFlood e -> C.EFlood (translate_exp e)
    | S.ERecord(fields) -> C.ERecord (List.map (fun (id, e) -> (Id.create id), translate_exp e) fields)
    | S.EProj(e, id) -> C.EProj (translate_exp e, Id.create id)
    | ETuple exps -> C.ETuple(List.map translate_exp exps)
    | ESizeCast _
    | EStmt _
    | EWith _
    | EComp _
    | EIndex _ -> 
      err
        e.espan
        "[SyntaxToCore.translate_exp] unsupported construct for core IR (EStmt, EWith, EComp, EIndex)"
    | EVector(exps) ->
      (* vectors can appear as builtin type arguments. At this point, they have a known 
         length, so can be translated into tuples. *)
      C.ETuple(List.map translate_exp exps)
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

and translate_etablecreate _ (exp : S.exp) : C.exp =
  match exp.e with
  | S.ETableCreate tc ->
    (* let tty = translate_ty tc.tty in
    let tsize = translate_exp tc.tsize in
    let tactions = List.map translate_exp tc.tactions in
    let default_cid, default_args, _ = SyntaxUtils.unpack_default_action tc.tdefault.e in
    let tdefault = default_cid, default_args |> List.map translate_exp in
    let e' = C.ETableCreate { tid = id; tty; tactions; tsize; tdefault } in *)
    (* let tty = translate_ty tc.tty in *)
    let tsize = translate_exp tc.tsize in
    let tactions = C.tup_sp (List.map translate_exp tc.tactions) Span.default in
    let default_acn_constr = translate_exp tc.tdefault in
    let e' = 
      C.ECall(Cid.create ["Table"; "create"], [tsize; tactions; default_acn_constr], false) in
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
  (* let translate_entry (entry : S.tbl_entry) : C.tbl_entry =
    let action_cid, action_args, _ = SyntaxUtils.unpack_default_action entry.eaction.e in
    { ematch = List.map translate_exp entry.ematch
    ; eprio = entry.eprio
    ; eaction = Cid.to_id action_cid
    ; eargs = List.map translate_exp action_args
    }
  in *)
  let s' =
    match s.s with
    | S.SNoop -> C.SNoop
    (* TABLE UPDATE -- hard coded table install call -> table_install *)
    (* | S.SUnit {e=ECall(cid, args, _)} when ((Cid.names cid) = ["Table"; "install"]) -> 
      let tbl_exp = List.nth args 0 in
      let key_tup = List.nth args 1 in
      let match_keys = match key_tup.e with 
        | ETuple keys -> List.map translate_exp keys
        | _ -> err_unsupported key_tup.espan "keys in a table install should be a tuple"
      in
      let action_exp = List.nth args 2 in
      let action_cid, action_args = match action_exp.e with 
        | ECall(cid, args, _) -> cid, List.map translate_exp args (* call to an action constructor *)
        | _ -> err action_exp.espan "the last argument of Table.install must be a call to an action constructor"
      in 
      let tbl_entry : C.tbl_entry = {
        eprio = 10;
        ematch = match_keys;
        eaction = Cid.to_id action_cid;
        eargs = action_args;
      }
      in
      let tbl_exp = translate_exp tbl_exp in
      C.STableInstall (tbl_exp, [tbl_entry]) *)
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
      let (core_tm : Tables.core_tbl_match) = { 
          Tables.tbl = translate_exp tm.tbl
        ; Tables.keys = List.map translate_exp tm.keys
        ; Tables.args = List.map translate_exp tm.args
        ; Tables.outs = tm.outs
        ; Tables.out_tys =
            (match tm.out_tys with
            | None -> None
            | Some otys -> Some (List.map translate_ty otys))
      } in
      Tables.tbl_match_to_s core_tm
      (* C.STableMatch
        { C.tbl = translate_exp tm.tbl
        ; C.keys = List.map translate_exp tm.keys
        ; C.args = List.map translate_exp tm.args
        ; C.outs = tm.outs
        ; C.out_tys =
            (match tm.out_tys with
             | None -> None
             | Some otys -> Some (List.map translate_ty otys))
        } *)
    | S.STableInstall (tbl_exp, entries) -> (
      match entries with 
        | [{ematch; eaction}] -> (
          let tbl_exp = translate_exp tbl_exp in
          let ematch = List.map translate_exp ematch in
          let ematch_tys = List.map (fun (exp : C.exp) -> exp.ety.raw_ty) ematch in
          let ematch_ty = CoreSyntax.ty@@CoreSyntax.TTuple ematch_tys in
          let eaction = translate_exp eaction in          
          let ematch = CoreSyntax.exp (CoreSyntax.ETuple ematch) ematch_ty in
          let e = C.ECall(Cid.create ["Table"; "install"], [tbl_exp; ematch; eaction], false) in
          C.SUnit({e; ety=C.ty C.TBool; C.espan = s.sspan})
        )
        | _ -> err s.sspan "table install with >1 entries not supported have exactly one entry"
    )
    (* C.STableInstall (translate_exp tbl_exp, List.map translate_entry entries) *)
    (* TABLE UPDATE -- hard coded tuple assign -> table assign *)
    | S.STupleAssign(tup_asn) -> (
        let ids = tup_asn.ids in
        let tys = match tup_asn.tys with
          | Some tys -> Some (List.map translate_ty tys)
          | None -> None
        in
        let exp = translate_exp tup_asn.exp in
        let res = C.STupleAssign({ids; tys; exp}) in
        res
    )
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

let translate_d preserve_user_decls d dspan dpragmas = 
  match d with
  | S.DGlobal (id, ty, constr_exp) -> (
    match ty.raw_ty with 
    (* TABLE UPDATE -- hard coded translation into a decl with ETableCreate *)
    (* | (TName(cid, _, _)) when (Cid.equal cid Tables.t_id) -> (
      match constr_exp.e with
      | S.ECall(_, [size_exp; actions_exp; default_exp], _) ->
        let size = translate_exp size_exp in
        let actions = match actions_exp.e with
          | ETuple actions -> List.map translate_exp actions
          | _ -> err_unsupported dspan "actions in a table create should be a tuple"
        in
        let default_cid, default_args = match default_exp.e with
          | ECall(cid, args, _) -> cid, args (* call to an action constructor *)
          | EVar(_) ->   
            err_unsupported dspan "Tables currently must use action constructors"
            (* cid, [] *) (* an action, which isn't fully supported *)
          | _ -> err_unsupported dspan "default action in a table create should be a call"
        in
        let (tbl_def : C.tbl_def) = {
          tid = id; 
          tty = translate_ty ty;
          tactions = actions;
          tsize = size;
          tdefault = (default_cid, List.map translate_exp default_args);
          }
        in
        Some (C.DGlobal (id, translate_ty ty, {e=C.ETableCreate tbl_def; ety=translate_ty ty; espan=constr_exp.espan}))
      | _ -> err_unsupported dspan "table create should be a call"
    )      *)
    | _ -> 
      Some (match constr_exp.e with
      | S.ETableCreate _ ->
        print_endline ("etablecreate...");
        exit 1;
        C.DGlobal (id, translate_ty ty, translate_etablecreate id constr_exp)
      | _ -> C.DGlobal (id, translate_ty ty, translate_exp constr_exp))
  )
  | S.DEvent (id, annot, sort, _, params) ->
    Some (C.DEvent (id, annot, translate_sort sort, translate_params params))
  | S.DHandler (id, s, body) ->
    Some (C.DHandler (id, translate_hsort s, translate_body body))
  | S.DMemop (mid, mparams, mbody) ->
    Some (C.DMemop
      { mid
      ; mparams = translate_params mparams
      ; mbody = translate_memop mbody
      })
  | S.DExtern (id, ty) -> Some (C.DExtern (id, translate_ty ty))
  | S.DActionConstr (id, tys, const_params, (params, acn_body)) ->
    Some (C.DActionConstr
      { aid = id
      ; artys = List.map translate_ty tys
      ; aconst_params = translate_params const_params
      ; aparams = translate_params params
      ; abody = List.map translate_exp acn_body
      })
  | S.DParser (id, params, parser_block) ->
    Some (C.DParser
      (id, translate_params params, translate_parser_block parser_block))
  | S.DUserTy(id, [], ty) when preserve_user_decls -> Some (C.DUserTy(id, translate_ty ty))
  | S.DUserTy _ when not preserve_user_decls -> None
  | S.DUserTy _ -> err dspan "size polymorphism in type declarations should be eliminated before midend"    
  | S.DFun(id, ty, _, (params, stmt)) when (Pragma.exists_sprag "main" dpragmas) -> 
      (* retain functions tagged as entry points into program, with their pragmas *)
      Some (C.DFun(id, translate_ty ty, (translate_params params, translate_statement stmt)))
  | _ -> err dspan (Printing.d_to_string d)
;;

let translate_decl 
  ?(preserve_user_decls=false)
  (d : S.decl) : C.decl option =
    let dpragma = match d.dpragmas with
      | [] -> None
      | [dprag] -> Some(dprag)
      | _ -> err d.dspan "multiple pragmas on a single declaration"
    in
    match translate_d preserve_user_decls d.d d.dspan d.dpragmas with
      | None -> None
      | Some(d') -> Some({(C.decl_sp d' d.dspan) with dpragma})
;;

let translate_prog ?(preserve_user_decls=false) (ds : S.decls) : C.decls = 
  List.filter_map (translate_decl ~preserve_user_decls) ds
;;