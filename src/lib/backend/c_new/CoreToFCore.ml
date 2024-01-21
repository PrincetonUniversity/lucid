module C = CoreSyntax

module F = struct 
  (* FCoreSyntax with extra nodes for translation from Core IR *)
  include FCoreSyntax
  (* extensions for translation to / from CoreIr *)
  type ty_annot += | TGroup
  type exp_annot += ECallUnordered of bool
  (* Statements and handlers with statement bodies are only for translation to / from the IR *)
  type s = 
    | SNoop
    | SUnit of exp
    | SAssign of assign
    | SSeq of statement * statement
    | SIf of exp * statement * statement
    | SMatch of exp * sbranch list
    | SGen of {loc: exp option; ev:exp; }
    | SRet of exp option
  and assign = {ids : cid list; tys : ty list; new_vars : bool; exp : exp}
  and sbranch = pat list * statement
  and statement = {s:s; sspan : sp; spragmas : pragma list;}
  type d += DHandler of {hdl_id : id; hdl_params: id list; hdl_body : statement;}
  type decl = {d:d; dspan : sp; dpragma : pragma option}
  type decls = decl list

  let new_local id ty exp = SAssign{ids=[id]; tys=[ty]; new_vars=true; exp}
  let update_local id exp = SAssign{ids=[id]; tys=[]; new_vars=false; exp}
  let new_locals ids tys exp = SAssign{ids; tys; new_vars=true; exp}
  let update_locals ids exp = SAssign{ids; tys=[]; new_vars=false; exp}
end

(* ops that are calls to builtins in FCore:
    flood
    printf
    event generation functions
*)

let err = Console.error ;;

(* small helpers to transform representations *)
(* a singleton size is an int; 
   a list of sizes is a tuple *)
let size_to_ty = function 
  | C.Sz(sz) -> F.ty@@F.TInt(F.sz sz)
  | C.Szs(szs) -> F.ty@@F.TRecord{labels=None; ts=List.map (fun sz -> F.ty@@F.TInt(F.sz sz)) szs}
;;
let rec bits_to_ints = function 
  | BitString.B0::bs -> 0::(bits_to_ints bs)
  | BitString.B1::bs -> 1::(bits_to_ints bs)
  | [] -> []
;;


let rec translate_raw_ty (raw_ty : C.raw_ty) : F.raw_ty = 
  match raw_ty with 
  | C.TBool -> F.TBool
  | C.TInt(Sz sz) ->  F.TInt(F.sz sz)
  | C.TInt(_) -> err "TInt size should be a singleton"
  | C.TEvent -> F.TEvent
  | C.TFun {arg_tys; ret_ty} -> 
    let fty : F.func_ty = {
      F.arg_tys = List.map translate_ty arg_tys; 
      F.ret_ty = translate_ty ret_ty;
      F.func_kind = F.FNormal;}
    in
    F.TFun fty
  | C.TName(cid, sizes, is_global) -> 
    (* convert sizes, which may be singletons or lists of ints, 
       into int types or tuple types *)
    F.TName(cid, List.map size_to_ty sizes, is_global)
  | C.TMemop(n_args, Sz(arg_size)) -> 
    let arg_tys = List.init n_args (fun _ -> F.ty@@F.TInt(F.sz arg_size)) in
    let ret_ty = F.ty@@F.TInt(F.sz arg_size) in
    let fty : F.func_ty = {
      F.arg_tys = arg_tys; 
      F.ret_ty = ret_ty;
      F.func_kind = F.FMemop;}
    in
    F.TFun fty
  | C.TMemop(_, _) -> err "TMemop size should be a singleton"
  | C.TAction(aty) -> F.TFun(translate_acn_ty aty)
  | C.TActionConstr{aconst_param_tys; aacn_ty} -> 
    (* an action constructor is a normal function that returns an action *)
    let fty : F.func_ty = {
      F.arg_tys = List.map translate_ty aconst_param_tys; 
      F.ret_ty = F.ty@@F.TFun(translate_acn_ty aacn_ty);
      F.func_kind = F.FNormal;}
    in
    F.TFun fty
  | C.TRecord(id_rawty_pairs) -> 
    let ids, raw_tys = List.split id_rawty_pairs in
    let raw_tys = List.map translate_raw_ty raw_tys in
    let tys = List.map F.ty raw_tys in
    F.TRecord{labels=Some ids; ts=tys}
  | C.TTuple(raw_tys) ->
    let raw_tys = List.map translate_raw_ty raw_tys in
    let tys = List.map F.ty raw_tys in
    F.TRecord{labels=None; ts=tys}
  | C.TGroup -> F.TInt(F.sz_platform)
  | C.TPat(Sz(sz)) -> F.TBits{ternary=true; len=F.sz sz}
  | C.TPat(_) -> err "TPat size should be a singleton"
  | C.TBits(Sz(sz)) -> F.TBits{ternary=false; len=F.sz sz}
  | C.TBits(_) -> err "TBits size should be a singleton"
and translate_acn_ty (aty : C.acn_ty) = 
  {
    F.arg_tys = List.map translate_ty aty.aarg_tys; 
    F.ret_ty = F.ttuple @@ List.map translate_ty aty.aret_tys;
    F.func_kind = F.FAction;
  }
and translate_ty (ty : C.ty) : F.ty = 
  {raw_ty = translate_raw_ty ty.raw_ty; 
   tspan = ty.tspan;
   (* tag "group" types because they are represented in FCore 
      as a non-unique Int type *)
   ty_annot = match ty.raw_ty with 
      | C.TGroup -> Some(F.TGroup) | _ -> None}
;;

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
;;

(*** values ***)
let rec translate_v (v : C.v) (vty:C.ty) : F.v = 
  match vty, v with 
  | _, C.VBool(b) -> F.VBool(b)
  | _, C.VInt({value; size}) -> (F.vint (Z.to_int value) (Z.to_int size)).v
  | _, C.VEvent event_val -> F.VEvent(translate_event_val event_val)
  | vty, C.VGlobal(id, addr) -> F.VTyRef(id, addr, translate_ty vty)
  | _, C.VGroup(locs) -> (F.vtup (List.map F.vint_unsized locs)).v
  | _, C.VPat(tbits) -> (F.vpat tbits).v
  | _, C.VBits(bits) -> (F.vbits (bits_to_ints bits)).v
  | {raw_ty=C.TTuple(raw_tys)}, C.VTuple(vs) -> 
    let tys = List.map C.ty raw_tys in
    let vs = List.map2 translate_v vs tys in
    let values = List.map F.value vs in
    (F.vtuple values).v
  | _, C.VTuple(_) -> err "VTuple type should be a tuple"
  | {raw_ty=C.TRecord(id_rawty_pairs)}, C.VRecord(id_v_pairs) -> 
    let labels, vs = List.split id_v_pairs in
    let tys = List.map (fun id -> List.assoc id id_rawty_pairs |> C.ty) labels in
    let vs = List.map2 translate_v vs tys in
    let values = List.map F.value vs in
    (F.vrecord labels values).v
  | _, C.VRecord(_) -> err "VRecord type should be a record"

and translate_event_val (ev : C.event_val) : F.vevent = 
  {
    evid = ev.eid;
    evnum = (match ev.evnum with 
      | Some({C.v=VInt({value})}) -> Some(Z.to_int value)
      | Some(_) -> err "event number should be an integer"
      | None -> None);
    evdata = List.map translate_value ev.data;
    meta = [
      "edelay", F.vint_unsized ev.edelay;
      "eserialized", F.vbool ev.eserialized;
    ]
  }  
and translate_value (value : C.value) : F.value = 
  {v = translate_v value.v value.vty; 
   vty = translate_ty value.vty;
   vspan = value.vspan}
;;

(*** expressions ***)
let rec translate_e (e : C.e) ety : F.e = 
  match ety, e with 
  | _, C.EVal(v) -> F.EVal(translate_value v)
  | _, C.EVar(c) -> F.EVar(c)
  | _, C.EOp(op, es) -> F.EOp(translate_op op, List.map translate_exp es)
  | ret_ty, C.ECall(cid, es, _) -> 
    (* reconstruct the functions assumed type based on arg and expression types *)
    let arg_tys = List.map (fun e -> e.C.ety) es in
    let fty = F.tfun 
      (List.map translate_ty arg_tys)
      (translate_ty ret_ty)
      F.FNormal
    in
    let fexp = F.efunref cid fty in
    F.ECall(fexp, List.map translate_exp es)
  | _, EHash((Sz size), es) -> 
    (* hash is an op in F *)
    F.EOp(F.Hash(F.sz size), List.map translate_exp es)
  | _, EHash(_, _) -> err "Hash size should be a singleton"
  | ret_ty, EFlood(port_exp) -> 
    (* flood is a call to a builtin *)
    let arg_tys = [translate_ty port_exp.ety] in
    let ret_ty = translate_ty ret_ty in
    let fty = F.tfun arg_tys ret_ty F.FNormal in
    let fexp = F.efunref (Cid.create ["flood"]) fty in
    F.ECall(fexp, [translate_exp port_exp])
  | _, ERecord(label_exp_pairs) -> 
    let labels, es = List.split label_exp_pairs in
    let es = List.map translate_exp es in
    (F.erecord labels es).e
  | _, ETuple(exps) -> 
    let es = List.map translate_exp exps in
    (F.etuple es).e
  | _, EProj(rec_exp, label) -> 
    (* project is an op *)
    (F.eop (F.Project(label)) [translate_exp rec_exp]).e
and translate_exp (exp : C.exp) : F.exp = 
  {
    e = translate_e exp.e exp.ety;
    ety = translate_ty exp.ety;
    espan = exp.espan;
    exp_annot =(match exp.e with 
      | ECall(_, _, is_unordered) -> Some(F.ECallUnordered(is_unordered))
      | _ -> None);
  }

let translate_pat (pat:C.pat) : F.pat = 
  match pat with 
  | PBit(ints) -> F.PVal(F.vpat ints)
  | C.PNum(z) -> 
    let sz = Z.size z in
    let v = Z.to_int z in
    F.PVal(F.vint v sz)
  | PEvent(cid, params) -> 
    let params = List.map (fun (id, ty) -> id, translate_ty ty) params in
    F.PEvent{event_id=cid; params;}
  | PWild -> err "wildcard patterns should be translated 
    into wildcard bitstrings before translation into IR"
;;
let rec translate_s (s:C.s) : F.s = 
  match s with
  | C.SNoop -> F.SNoop
  | C.SUnit(exp) -> F.SUnit(translate_exp exp)
  | C.SLocal(id, ty, exp) -> F.new_local (Cid.id id) (translate_ty ty) (translate_exp exp)
  | C.SAssign(id, exp) -> F.update_local id (translate_exp exp)
  | C.SPrintf(fmt_str, args) -> 
    (* printf  *)
    let str_exp = F.eval (F.string_to_value fmt_str) in
    let args = str_exp::List.map (fun arg -> (translate_exp arg)) args in
    let arg_tys = List.map (fun (arg:F.exp) -> arg.ety) args in
    let ret_ty = F.ty@@TUnit in
    let fty = F.tfun arg_tys ret_ty F.FNormal in
    let fexp = F.efunref (Cid.create ["printf"]) fty in
    let print_call = F.ecall fexp args in
    F.SUnit(print_call)
  | C.SIf(exp, stmt1, stmt2) -> 
    let exp = translate_exp exp in
    let stmt1 = translate_statement stmt1 in
    let stmt2 = translate_statement stmt2 in
    F.SIf(exp, stmt1, stmt2)
  | C.SGen(GSingle(None), ev) -> 
    F.SGen{loc=None; ev=translate_exp ev}
  | C.SGen(GSingle(_), _) -> 
    err "generate to switch addresses not supported"
  | C.SGen(GPort(port), ev) -> 
    F.SGen{loc=Some(translate_exp port); ev=translate_exp ev}
  | C.SGen(GMulti(port), ev) -> 
    F.SGen{loc=Some(translate_exp port); ev=translate_exp ev}
  | C.SSeq(s1, s2) -> F.SSeq(translate_statement s1, translate_statement s2)
  | C.SMatch(exps, branches) -> 
    let exps = List.map translate_exp exps in
    (* if there's more than one expression, wrap it in a tuple *)
    let exp = match exps with 
      | [exp] -> exp
      | exps -> F.etuple exps
    in
    let branches = List.map 
      (fun (pats, stmt) -> 
        let pats = List.map translate_pat pats in
        let stmt = translate_statement stmt in
        (pats, stmt))
      branches
    in
    F.SMatch(exp, branches)
  | C.SRet(None) -> F.SRet(None)
  | C.SRet(Some(exp)) -> F.SRet(Some(translate_exp exp))
  | STupleAssign{ids; tys; exp;} -> 
    let cids = List.map (Cid.id) ids in
    let tys, new_vars = match tys with 
      | None -> [], false
      | Some(tys) -> List.map translate_ty tys, true
    in
    let exp = translate_exp exp in
    F.SAssign{ids=cids; tys; new_vars; exp}
and translate_statement (stmt : C.statement) : F.statement = 
  {
    s = translate_s stmt.s;
    sspan = stmt.sspan;
    spragmas = stmt.spragmas;
  }






(* let translate_decl (decl:C.decl) : F.decl = 
  match decl.d with 
  | C.DGlobal(id, ty, exp) -> 


;; *)
let translate_prog (ds : C.decls) : F.decls = 
  
  []
;;
