(* translate from FCore back to CoreSyntax *)
let err = Console.error ;;




module C = CoreSyntax
module F = FCoreSyntax

let rec ty_to_size (ty : F.ty) = 
  match ty.raw_ty with 
  | F.TInt(F.SConst sz) -> C.Sz sz
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

let rec translate_raw_ty (raw_ty : F.raw_ty) : C.raw_ty = 
  match raw_ty with 
  (* basic types *)
  | F.TUnit -> err "you shouldn't have to translate a unit type back to CoreSyntax"
  | F.TBool -> C.TBool
  | F.TInt(F.SConst sz) -> C.TInt(C.Sz sz)
  | F.TEvent -> C.TEvent
  | F.TBits{ternary=true; len=F.SConst sz} -> C.TBits(C.Sz sz)
  | F.TBits{ternary=false; len=F.SConst sz} -> C.TPat(C.Sz sz)
  | F.TBits{len=_} -> err "bitpattern with unspecified length"
  (* This _should_ be true. Also, group types should be removed from core syntax anyway *)
  | F.TInt(F.SPlatformDependent) -> err "platform dependent int type"
  (* named types *)
  | F.TPrimitive(ty_id, ty_args, true) -> C.TName(ty_id, List.map ty_to_size ty_args, true)
  | F.TPrimitive(_, _::_, false)  ->  err "primitive type with arguments that's not global?"
  | F.TPrimitive(ty_id, [], false) -> C.TName(ty_id, [], false) 
  | F.TName(ty_id) -> C.TName(ty_id, [], false)
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
        | F.TInt(F.SConst sz) -> C.TMemop(List.length arg_tys, C.Sz sz)
        | _ -> err "memop function doesn't have an int type arg?")
  | F.TFun _ -> err "unknown function kind in function type"
  (* collection types *)
  | F.TRecord{labels=None; ts} -> C.TTuple(List.map translate_ty ts |> List.map (fun (ty : C.ty) -> ty.raw_ty))
  | F.TRecord{labels=Some labels; ts} -> 
    let raw_tys = List.map translate_ty ts |> List.map (fun (ty : C.ty) -> ty.raw_ty) in
    let label_rawty_pairs = List.combine labels raw_tys in
    C.TRecord(label_rawty_pairs)

  and translate_ty (ty : F.ty) : C.ty = 
  if (F.has_annot ty.ty_annot "group") 
  then C.ty_sp C.TGroup ty.tspan
  else C.ty_sp (translate_raw_ty ty.raw_ty) ty.tspan
;;


(* 
let translate_op (op : C.op) : F.op = 
  match op with 
  | C.And -> F.And
  | C.Or -> F.Or
  | C.Not -> F.Not
  | C.Eq -> F.Eq
  | C.Neq -> F.Neq
  | C.Less -> F.Less
  | C.More -> F.More
  | C.Leq -> F.Leq
  | C.Geq -> F.Geq
  | C.Neg -> F.Neg
  | C.Plus -> F.Plus
  | C.Sub -> F.Sub
  | C.SatPlus -> F.SatPlus
  | C.SatSub -> F.SatSub
  | C.Cast(Sz sz) -> F.Cast(F.sz sz)
  | C.Cast(_) -> err "Cast size should be a singleton"
  | C.Conc -> F.Conc
  | C.BitAnd -> F.BitAnd
  | C.BitOr -> F.BitOr
  | C.BitXor -> F.BitXor
  | C.BitNot -> F.BitNot
  | C.LShift -> F.LShift
  | C.RShift -> F.RShift
  | C.Slice(i, j) -> F.Slice(i, j)
  | C.PatExact -> F.PatExact
  | C.PatMask -> F.PatMask   
*)
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
  | F.Cast(F.SConst sz) -> C.Cast(C.Sz sz)
  | F.Cast(_) -> err "Cast size should be a singleton"
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
  | F.VInt{value; size=F.SConst(_)} -> value
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
  | F.VInt{value=ival; size=F.SConst(size)} -> 
    C.value_sp (C.VInt(Integer.create ival size)) value.vspan
  | F.VInt{size=_} -> err "int value with unspecified size"
  | F.VEvent(event_val) -> 
    C.value_sp (C.VEvent(translate_event_val event_val)) value.vspan
  | F.VGlobal(id, addr, _) -> 
    C.value_sp (C.VGlobal(id, addr)) value.vspan
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
  | F.VClosure _ -> err "closure values cannot be translated back to CoreSyntax"


and translate_event_val (ev : F.vevent) : C.event_val = 
  {
    eid = ev.evid;
    evnum =  Option.map translate_value ev.evnum;
    data = List.map translate_value ev.evdata;
    edelay = (List.assoc "edelay" ev.meta) |> value_to_int;
    eserialized = (List.assoc "eserialized" ev.meta) |> value_to_bool;
  }
;;

let rec translate_exp (exp: F.exp) = 
  match exp.e with
  | F.EVal(value) -> 
    C.aexp (C.EVal(translate_value value)) (translate_ty exp.ety) exp.espan
  | F.EVar(cid, _) -> 
    C.aexp (C.EVar(cid)) (translate_ty exp.ety) exp.espan
  (* operations *)
  | F.EOp(F.Hash(F.SConst sz), eargs) -> 
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
  | F.EClosure _ -> err "closure expressions cannot be translated back to CoreSyntax"
  | F.ECall({e=EVar(cid, _)}, [port_arg]) when (Cid.names cid = ["flood"]) -> 
    C.aexp (C.EFlood(translate_exp port_arg)) (translate_ty exp.ety) exp.espan
  | F.ECall({e=EVar(cid, _)}, _) when (Cid.names cid = ["generate_self"]) -> 
    err "generate_self translates into a statement"
    (* C.aexp (C.EFlood(translate_exp in_parser port_arg)) (translate_ty exp.ety) exp.espan *)
  | F.ECall({e=EVar(cid, _)}, _) when (Cid.names cid = ["generate"]) -> 
    err "generate translates into a statement"
  | F.ECall({e=EVar(cid, _)}, eargs) -> C.aexp 
    (C.ECall(cid, List.map (translate_exp) eargs, F.has_annot exp.exp_annot "unordred")) 
    (translate_ty exp.ety) 
    exp.espan
  | F.EEvent{event_id; args} -> 
    C.aexp (C.ECall(Cid.id(event_id), List.map (translate_exp) args, false)) (translate_ty exp.ety) exp.espan
  | F.ECall(_, _) -> err "call expression with non-var function"


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
  | _, SUnit exp -> C.statement_sp (C.SUnit(translate_exp exp)) stmt.sspan
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
        | F.S stmt -> 
          (List.map translate_pat pats, translate_stmt in_parser stmt)
        | _ -> err "match statement with non-statement branch")       
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



let translat_parser body = 
  let body = translate_stmt true body in
  failwith "not done"  

let translate_decl (decl : F.fdecl) : C.decl = 
  match decl.d with
  | F.DFun(F.FParser, id, _, params, Some(body)) -> (
    let body = translate_stmt true body in
    failwith "not done"

  )
  | _ -> failwith "not done"

;;

let translate_prog (ds : F.fdecls) : C.decls = 
  List.map translate_decl ds
;;