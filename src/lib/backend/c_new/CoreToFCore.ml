module C = CoreSyntax
module F = FCoreSyntax

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
      F.func_kind = FNormal;}
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
      F.func_kind = FMemop;}
    in
    F.TFun fty
  | C.TMemop(_, _) -> err "TMemop size should be a singleton"
  | C.TAction(aty) -> F.TFun(translate_acn_ty aty)
  | C.TActionConstr{aconst_param_tys; aacn_ty} -> 
    (* an action constructor is a normal function that returns an action *)
    let fty : F.func_ty = {
      F.arg_tys = List.map translate_ty aconst_param_tys; 
      F.ret_ty = F.ty@@F.TFun(translate_acn_ty aacn_ty);
      F.func_kind = FNormal;}
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
    F.func_kind = FAction;
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
      FNormal
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





(* let translate_decl (decl:C.decl) : F.decl = 
  match decl.d with 
  | C.DGlobal(id, ty, exp) -> 


;; *)
let translate_prog (ds : C.decls) : F.decls = 
  
  []
;;
