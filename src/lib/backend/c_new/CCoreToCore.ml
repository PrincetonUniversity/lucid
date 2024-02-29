(* translate from FCore back to CoreSyntax *)
let err = Console.error ;;


module C = CoreSyntax
module F = CCoreSyntax

let rec ty_to_size (ty : F.ty) = 
  match ty.raw_ty with 
  | F.TInt(sz) -> C.Sz sz
  | F.TRecord{ts} -> 
    C.Szs (List.map ty_to_size ts |> List.map (function | C.Sz sz -> sz | _ -> err "bug"))
  | _ -> failwith "not done"
;;

let rec ints_to_bits = function
  | 0::is -> BitString.B0 :: ints_to_bits is
  | 1::is -> BitString.B1 :: ints_to_bits is
  | [] -> []
  | _ -> err "invalid int to convert into a bit"
;;

let detuple_ty (ty : F.ty) = match ty.raw_ty with 
  | F.TRecord{labels=None; ts} -> ts
  | _ -> [ty]
;;

let is_tbuiltin tycid = 
  match (List.assoc_opt tycid Builtins.builtin_tycid_to_ty) with 
  | Some(ty) -> SyntaxUtils.is_tbuiltin ty
  | None -> false
;;

let rec translate_raw_ty (raw_ty : F.raw_ty) : C.raw_ty = 
  match raw_ty with 
  (* basic types *)
  | F.TUnit -> err "you shouldn't have to translate a unit type back to CoreSyntax"
  | F.TBool -> C.TBool
  | F.TInt(sz) -> C.TInt(C.Sz sz)
  | F.TEvent -> C.TEvent
  | F.TBits{ternary=true; len= sz} -> C.TPat(C.Sz sz)
  | F.TBits{ternary=false; len= sz} -> C.TBits(C.Sz sz)
  (* named types *)
  | F.TName(tcid, []) when (Cid.equal tcid F.tgroup_cid) -> C.TGroup
  | F.TName(ty_id, []) -> C.TName(ty_id, [])
  | F.TName(ty_cid, ty_args) when is_tbuiltin ty_cid -> C.TBuiltin(ty_cid, List.map (fun ty -> (translate_ty ty).raw_ty) ty_args)
  | F.TName(ty_cid, ty_args) -> C.TName(ty_cid, List.map ty_to_size ty_args)
  (* functions types for functions, action constructors, actions, and memops *)
  | F.TFun{arg_tys; ret_ty; func_kind} when (func_kind = F.FNormal) -> (
        (* if it returns an action, its an action constructor, else its a function *)
        match ret_ty.raw_ty with 
        | F.TFun{arg_tys=inner_arg_tys; ret_ty = inner_ret_ty; func_kind=F.FAction; _} -> 
          C.TActionConstr{
            aconst_param_tys = List.map translate_ty arg_tys;
            aacn_ty = {
              aarg_tys = List.map translate_ty inner_arg_tys;
              aret_tys = List.map translate_ty (detuple_ty inner_ret_ty);
            }
          }
        | _ -> 
          C.TFun{
            arg_tys = List.map translate_ty arg_tys;
            ret_ty = translate_ty ret_ty
          }
      )
  | F.TFun{arg_tys; ret_ty; func_kind} when (func_kind = F.FAction) ->
    C.TAction{
      aarg_tys = List.map translate_ty arg_tys;
      aret_tys = List.map translate_ty (detuple_ty ret_ty);}
  | F.TFun{arg_tys; func_kind} when (func_kind = F.FMemop) ->
      (match (List.hd arg_tys).raw_ty with 
        | F.TInt( sz) -> C.TMemop(List.length arg_tys, C.Sz sz)
        | _ -> err "memop function doesn't have an int type arg?")
  | F.TFun _ -> err "unknown function kind in function type"
  (* collection types *)
  | F.TRecord{labels=None; ts} -> C.TTuple(List.map translate_ty ts |> List.map (fun (ty : C.ty) -> ty.raw_ty))
  | F.TRecord{labels=Some labels; ts} -> 
    let raw_tys = List.map translate_ty ts |> List.map (fun (ty : C.ty) -> ty.raw_ty) in
    let label_rawty_pairs = List.combine labels raw_tys in
    C.TRecord(label_rawty_pairs)
  | F.TEnum(tags) ->
     C.TInt(
      if (List.length tags <= 256) 
        then (C.Sz 8) 
        else (C.Sz 16))

  and translate_ty (ty : F.ty) : C.ty = 
    C.ty_sp (translate_raw_ty ty.raw_ty) ty.tspan
;;

(* inverse of translate_op in comment *)
let translate_op op = 
  match op with 
  | F.And -> C.And
  | F.Or -> C.Or
  | F.Not -> C.Not
  | F.Eq -> C.Eq
  | F.Neq -> C.Neq
  | F.Less -> C.Less
  | F.More -> C.More
  | F.Leq -> C.Leq
  | F.Geq -> C.Geq
  | F.Neg -> C.Neg
  | F.Plus -> C.Plus
  | F.Sub -> C.Sub
  | F.SatPlus -> C.SatPlus
  | F.SatSub -> C.SatSub
  | F.Cast( sz) -> C.Cast(C.Sz sz)
  | F.Conc -> C.Conc
  | F.BitAnd -> C.BitAnd
  | F.BitOr -> C.BitOr
  | F.BitXor -> C.BitXor
  | F.BitNot -> C.BitNot
  | F.LShift -> C.LShift
  | F.RShift -> C.RShift
  | F.Slice(i, j) -> C.Slice(i, j)
  | F.PatExact -> C.PatExact
  | F.PatMask -> C.PatMask
  | F.Hash _ -> err "hash op must translate into hash expression"
  | F.Project _ -> err "project op must translate into project expression"
  | F.Get _ -> err "there is no get op in CoreSyntax"
;;

(* left off here -- at translate_v *)
let value_to_int (value : F.value) : int = 
  match value.v with 
  | F.VInt{value; size=(_)} -> value
  | _ -> err "value is not an int"
;;
let value_to_bool (value : F.value) : bool = 
  match value.v with 
  | F.VBool b -> b
  | _ -> err "value is not a bool"
;;
let uv (value : C.value) : C.v = value.v

let rec translate_value (value : F.value) : C.value = 
  match value.v with 
  | F.VBool b -> C.value_sp (C.VBool b) value.vspan
  | F.VInt{value=ival; size=(size)} -> 
    C.value_sp (C.VInt(Integer.create ival size)) value.vspan
  | F.VEvent(event_val) -> 
    C.value_sp (C.VEvent(translate_event_val event_val)) value.vspan
  | F.VGlobal{global_id; global_pos} -> 
    C.value_sp (C.VGlobal(global_id, global_pos)) value.vspan
  | F.VBits {ternary=true; bits} -> 
    C.value_sp (C.VPat(bits)) value.vspan
  | F.VBits {ternary=false; bits} ->
    C.value_sp (C.VBits(ints_to_bits bits)) value.vspan
  | F.VRecord{labels=None; es} -> 
    C.value_sp (C.VTuple(List.map (fun value -> value |> translate_value |> uv) es)) value.vspan
  | F.VRecord{labels=Some labels; es} ->
    let label_value_pairs = List.combine labels es in
    C.value_sp (C.VRecord(List.map (fun (label, value) -> (label, translate_value value |> uv)) label_value_pairs)) value.vspan
  | F.VUnit -> err "unit values cannot be translated back to CoreSyntax"
  (* | F.VClosure _ -> err "closure values cannot be translated back to CoreSyntax" *)
  | F.VEnum(str, enum_ty) -> 
    (* translate to an int *)
    let tags = match enum_ty.raw_ty with 
      | F.TEnum(tags) -> tags
      | _ -> err "enum type is not an enum"
    in
    let ival = List.assoc str tags in
    let size = if (List.length tags <= 256) then 8 else 16 in
    C.value_sp (C.VInt(Integer.create ival size)) value.vspan



and translate_event_val (ev : F.vevent) : C.event_val = 
  {
    eid = ev.evid;
    evnum =  Option.map translate_value ev.evnum;
    data = List.map translate_value ev.evdata;
    edelay = (List.assoc "edelay" ev.meta) |> value_to_int;
    eserialized = (List.assoc "eserialized" ev.meta) |> value_to_bool;
  }
;;

(* define an error for the case of "generate_self" *)

exception MadeStatement of C.s
let rec translate_exp (exp: F.exp) = 
  match exp.e with
  | F.EVal(value) -> 
    C.aexp (C.EVal(translate_value value)) (translate_ty exp.ety) exp.espan
  | F.EVar(cid) -> 
    C.aexp (C.EVar(cid)) (translate_ty exp.ety) exp.espan
  (* operations *)
  | F.EOp(F.Hash( sz), eargs) -> 
    let sz = C.Sz sz in
    let eargs = List.map (translate_exp) eargs in
    C.aexp (C.EHash(sz, eargs)) (translate_ty exp.ety) exp.espan
  | F.EOp(F.Project(label), [earg]) ->
    let earg = translate_exp earg in
    C.aexp (C.EProj(earg, label)) (translate_ty exp.ety) exp.espan
  | F.EOp(op, args) -> 
    let op = translate_op op in
    let args = List.map (translate_exp) args in
    C.aexp (C.EOp(op, args)) (translate_ty exp.ety) exp.espan
  | F.ERecord{labels=None; es} -> 
    C.aexp (C.ETuple(List.map (fun exp -> exp |> translate_exp) es)) (translate_ty exp.ety) exp.espan
  | F.ERecord{labels=Some labels; es} ->
    let label_exp_pairs = List.combine labels es in
    C.aexp (C.ERecord(List.map (fun (label, exp) -> (label, translate_exp exp)) label_exp_pairs)) (translate_ty exp.ety) exp.espan
  (* | F.EClosure _ -> err "closure expressions cannot be translated back to CoreSyntax" *)
  | F.ECall{f={e=EVar(cid)}; args=[port_arg];} when (Cid.names cid = ["flood"]) -> 
    C.aexp (C.EFlood(translate_exp port_arg)) (translate_ty exp.ety) exp.espan
  | F.ECall{f={e=EVar(cid)}; args=[ev_exp];} when (Cid.names cid = ["generate_self"]) -> 
    let ev_exp = translate_exp ev_exp in
    let s = C.SGen(GSingle(None), ev_exp) in
    raise (MadeStatement(s))
  | F.ECall{f={e=EVar(cid)}; args=[port_exp; ev_exp]} when (Cid.names cid = ["generate_port"]) -> 
    let port_exp = translate_exp port_exp in
    let ev_exp = translate_exp ev_exp in
    let s = C.SGen(GPort(port_exp), ev_exp) in
    raise (MadeStatement(s))
  | F.ECall{f={e=EVar(cid)} ;args=[port_exp; ev_exp]} when (Cid.names cid = ["generate_switch"]) -> 
    let port_exp = translate_exp port_exp in
    let ev_exp = translate_exp ev_exp in
    let s = C.SGen(GSingle(Some(port_exp)), ev_exp) in
    raise (MadeStatement(s))
  | F.ECall{f={e=EVar(cid)}; args=[group_exp; ev_exp]} when (Cid.names cid = ["generate_group"]) -> 
    let group_exp = translate_exp group_exp in
    let ev_exp = translate_exp ev_exp in
    let s = C.SGen(GMulti(group_exp), ev_exp) in
    raise (MadeStatement(s))
  | F.ECall{f={e=EVar(cid)}; args=str_arg::args} when (Cid.names cid = ["printf"]) -> 
    let str_val = match str_arg.e with 
      | EVal(value) -> value
      | _ -> err "printf with non-string argument"
    in
    let str = F.value_to_string str_val in
    let args = List.map (translate_exp) args in
    let s = C.SPrintf(str, args) in
    raise (MadeStatement(s))
  | F.ECall{f={e=EVar(cid)}; args=eargs;call_kind} -> 
    (match call_kind with
    | F.CNormal -> 
      let eargs = List.map (translate_exp) eargs in
      C.aexp 
        (C.ECall(cid, eargs, false)) 
        (translate_ty exp.ety) 
        exp.espan
    | F.CEvent -> 
      let eargs = List.map (translate_exp) eargs in
      C.aexp 
        (C.ECall(cid, eargs, false)) 
        (translate_ty exp.ety) 
        exp.espan
  )   
  | F.ECall _ -> err "call expression with non-var function"


and translate_pat (pat : F.pat) : C.pat = 
  match pat with 
  | F.PVal({v=F.VBits{ternary=true; bits}}) -> C.PBit(bits)
  | F.PVal({v=F.VInt{value}}) -> C.PNum(Z.of_int value)
  | F.PVal _ -> err "pattern with non-int or non-bit value"
  | F.PEvent{event_id; params;} -> 
    let params = List.map (fun (id, ty) -> id, translate_ty ty) params in
    C.PEvent(event_id, params)

and translate_stmt in_parser (stmt : F.statement) = 
  match in_parser, stmt.s with
  | _, SNoop -> C.statement_sp (C.SNoop) stmt.sspan
  | _, SUnit exp -> 
    let s = try 
        C.SUnit(translate_exp exp)    
      with MadeStatement(s) -> s
    in
    C.statement_sp s stmt.sspan
  (* assignment *)
  | _, SAssign({ids=[cid]; tys=[ty]; new_vars=true; exp}) -> 
    C.statement_sp (C.SLocal(Cid.to_id cid, translate_ty ty, translate_exp exp)) stmt.sspan
  | _, SAssign({ids=[cid]; new_vars=false; exp}) ->
    C.statement_sp (C.SAssign(cid, translate_exp exp)) stmt.sspan
  | _, SAssign({ids; tys; new_vars; exp;}) -> 
    let tys = if new_vars then Some(List.map translate_ty tys) else None in
    let ids = List.map Cid.to_id ids in
    let exp = translate_exp exp in
    C.statement_sp (C.STupleAssign{ids; tys; exp}) stmt.sspan
  | _, SIf(exp, stmt1, stmt2) -> 
    let exp = translate_exp exp in
    let stmt1 = translate_stmt in_parser stmt1 in
    let stmt2 = translate_stmt in_parser stmt2 in
    C.statement_sp (C.SIf(exp, stmt1, stmt2)) stmt.sspan
  | _, SMatch(exp, branches) -> 
    let exps = match exp.e with 
      | ERecord{labels=None; es} -> es
      | _ -> [exp]
    in
    let exps = List.map translate_exp exps in
    let branches = List.map 
      (fun (pats, branch_tgt) -> 
        match branch_tgt with 
        | stmt -> 
          (List.map translate_pat pats, translate_stmt in_parser stmt))
      branches
    in
    C.statement_sp (C.SMatch(exps, branches)) stmt.sspan
  | _, SSeq(stmt1, stmt2) ->
    let stmt1 = translate_stmt in_parser stmt1 in
    let stmt2 = translate_stmt in_parser stmt2 in
    C.statement_sp (C.SSeq(stmt1, stmt2)) stmt.sspan
  | _, SRet(exp_opt) -> 
    let exp_opt = Option.map translate_exp exp_opt in
    C.statement_sp (C.SRet(exp_opt)) stmt.sspan
;;

let translate_decl (decl : F.decl) : C.decl = 
  match decl.d with
  (* variables can be globals or externs *)
  | F.DVal(id, ty, Some(exp)) -> 
    let ty = translate_ty ty in
    let exp = translate_exp exp in
    C.decl_sp (C.DGlobal(id, ty, exp)) decl.dspan
  | F.DVal(id, ty, None) ->
    let ty = translate_ty ty in
    C.decl_sp (C.DExtern(id, ty)) decl.dspan
  | F.DEvent{evconstrid; evconstrnum; evparams; is_parsed} -> 
    let ev_sort = if is_parsed then C.EPacket else C.EBackground in
    let params = List.map (fun (id, ty) -> id, translate_ty ty) evparams in
    C.decl_sp (C.DEvent(evconstrid, evconstrnum, ev_sort, params)) decl.dspan
  (* functions can be handlers, memops, parsers, or functions *)
  | F.DFun(F.FHandler, id, _, params, Some(body)) ->
    let params = List.map (fun (id, ty) -> id, translate_ty ty) params in
    let body = translate_stmt false body in
    let d = C.DHandler(id, C.HData, (params, body)) in
    C.decl_sp d decl.dspan
  | F.DFun(F.FMemop, id, _, params, Some(body)) ->
    (* translate to core, then specialize as memop *)
    let params = List.map (fun (id, ty) -> id, translate_ty ty) params in
    let body = translate_stmt false body in
    let memop = Despecialization.specialize_memop id params body in
    let d = C.DMemop(memop) in
    C.decl_sp d decl.dspan    
  | F.DFun(F.FParser, id, _, params, Some(body)) -> (    
    (* translate to core, then specialize parser *)
    let params = List.map (fun (id, ty) -> id, translate_ty ty) params in
    let body = translate_stmt false body in
    let parse_block = Despecialization.specialize_parser_block body in
    C.decl_sp (C.DParser(id, params, parse_block)) decl.dspan
  )
  | F.DFun(F.FAction, id, rty, params, Some(body)) -> 
    (* detuple return type, first param, and second param *)
    let detuple_ty (ty: F.ty) = match ty.raw_ty with 
      | TRecord{ts} -> ts
      | _ -> [ty]
    in
    let detuple_param = function 
      | (base_id, {F.raw_ty=TRecord{ts}}) -> 
        List.mapi 
          (fun i ty -> (
            let name = 
              (Id.name base_id ^ "_"^(string_of_int i))
            in
            Id.create(name), ty))
          ts
      (* its already not a tuple *)
      | (base_id, ty) -> [base_id, ty]
    in
    let rtys = detuple_ty rty in
    let const_params, params = match params with 
      | [const_param; param] ->  (
        match (F.is_trecord (snd const_param), F.is_trecord (snd param)) with 
        | true, true -> [const_param], [param]
        | false, false -> 
          detuple_param const_param, detuple_param param
        | _, _ -> err "action has inconsistent parameter for conversion back to CoreIR"
      )
      | _ -> 
        err@@"action has invalid number of arguments for conversion back to CoreIR "
          ^"action: "^CCorePrinting.show_decl decl
    in
    let field_replacer = 
      (* replace tuple get ops with evars  *)
      object 
      inherit [_] F.s_map as super
      method! visit_exp  () exp = 
        match exp.e with 
        | EOp(Get(field_idx), [{e=F.EVar(cid);}]) -> 
          let field_name = (Cid.to_id cid |> Id.name)^"_"^(string_of_int field_idx) in
          {exp with e=F.EVar(Cid.create [field_name])}
        | _ -> super#visit_exp () exp
      end
    in
    let body = field_replacer#visit_statement () body in
    let action_body_exps = match body.s with 
    | F.SRet(Some(exp)) when F.etup_form exp ->
      List.map translate_exp (F.flatten_tuple exp)
    | F.SRet(Some(exp)) ->[translate_exp exp]
      | _ -> err "action has more than just a return statement"
    in 
    C.decl_sp (C.DActionConstr{
      aid = id;
      artys = List.map translate_ty rtys;
      aconst_params = List.map (fun (id, ty) -> id, translate_ty ty) const_params;
      aparams = List.map (fun (id, ty) -> id, translate_ty ty) params;
      abody = action_body_exps;
    }) decl.dspan
  | F.DFun(F.FNormal, id, rty, params, Some(body)) ->
    let params = List.map (fun (id, ty) -> id, translate_ty ty) params in
    let body = translate_stmt false body in
    let rty = translate_ty rty in
    let d = C.DFun(id, rty, (params, body)) in
    C.decl_sp d decl.dspan
  | F.DFun(_, _, _, _, None) -> err "extern functions are not supported in CoreSyntax"
  | F.DFun(_, _, _, _, _) -> 
    CCorePrinting.show_decl decl |> print_endline;
    err "Unknown function kind"
  (* types *)
  | F.DTy(cid, Some(ty)) -> 
    let ty = translate_ty ty in
    C.decl_sp (C.DUserTy(Cid.to_id cid, ty)) decl.dspan
  | F.DTy(_, None) -> 
    failwith "type declaration without type definition"
  (* events *)
;;

let translate_prog (ds : F.decls) : C.decls = 
  List.map translate_decl ds
;;