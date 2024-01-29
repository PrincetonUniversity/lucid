module C = CoreSyntax

module F = FCoreSyntax
(* ops that are calls to builtins in FCore:
    flood
    printf
    parse.skip
    parse.peek
    parse.read
    Array.*
    PairArray.*
    Sys.*
*)
let cid s = s |> Id.create |> Cid.id
let err = Console.error ;;

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
  | C.TName(cid, sizes, true) -> (F.tglobal cid (List.map size_to_ty sizes) true).raw_ty
  | C.TName(cid, [], false) -> (F.tname cid).raw_ty
  | C.TName(_, _, false) -> err "non-global named types should not have arguments"
  | C.TFun {arg_tys; ret_ty} -> 
    let fty : F.func_ty = {
      F.arg_tys = List.map translate_ty arg_tys; 
      F.ret_ty = translate_ty ret_ty;
      F.func_kind = F.FNormal;}
    in
    F.TFun fty
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
  | C.TGroup -> F.tgroup
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
   }
;;
let translate_params (params: C.params) : F.params = 
  List.map (fun ((id: Id.t), ty) -> id, translate_ty ty) params
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
  | _, C.VBool(b) -> (F.vbool b).v
  | _, C.VInt({value; size}) -> (F.vint (Z.to_int value) (Z.to_int size)).v
  | _, C.VEvent event_val -> F.VEvent(translate_event_val event_val)
  | vty, C.VGlobal(id, addr) -> (F.vglobal id addr (translate_ty vty)).v
  | _, C.VGroup(locs) -> (F.vtup (List.map (fun i -> F.vint 32 i ) locs)).v
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
      | Some(value) -> Some(translate_value value)
      | None -> None);
    evdata = List.map translate_value ev.data;
    meta = [
      "edelay", F.vint ev.edelay 16;
      "eserialized", F.vbool ev.eserialized;
    ]
  }  
and translate_value (value : C.value) : F.value = 
  {v = translate_v value.v value.vty; 
   vty = translate_ty value.vty;
   vspan = value.vspan}
;;

(*** expressions ***)
let rec translate_exp (exp : C.exp) : F.exp = 
  let exp' = match exp.ety, exp.e with 
  | _, C.EVal(v) -> (F.eval (translate_value v))
  | _, C.EVar(c) -> F.local_var c (translate_ty exp.ety)
  | _, C.EOp(op, es) -> F.eop (translate_op op) (List.map translate_exp es)
  | {raw_ty=C.TEvent}, C.ECall(cid, es, _) -> 
    let fexp = F.efunref cid (F.tevent) in 
    F.eevent fexp (List.map translate_exp es)
  (* if its not an event, its an ordered or unordered call *)
  | ret_ty, C.ECall(cid, es, unordered_flag) -> (
    (* reconstruct the functions assumed type based on arg and expression types *)
    let arg_tys = List.map (fun e -> e.C.ety) es in
    let fty = F.tfun 
      (List.map translate_ty arg_tys)
      (translate_ty ret_ty)
    in
    let fexp = F.efunref cid fty in 
    match unordered_flag with 
    | true -> F.ecall_unordered fexp (List.map translate_exp es)
    | false -> F.ecall fexp (List.map translate_exp es)
  )
  | _, EHash((Sz size), es) -> 
    (* hash is an op in F *)
    F.eop (F.Hash(F.sz size)) (List.map translate_exp es)
  | _, EHash(_, _) -> err "Hash size should be a singleton"
  | ret_ty, EFlood(port_exp) -> 
    (* flood is a call to a builtin *)
    let arg_tys = [translate_ty port_exp.ety] in
    let ret_ty = translate_ty ret_ty in
    let fty = F.tfun arg_tys ret_ty in
    let fexp = F.efunref (Cid.create ["flood"]) fty in
    F.ecall fexp [translate_exp port_exp]
  | _, ERecord(label_exp_pairs) -> 
    let labels, es = List.split label_exp_pairs in
    let es = List.map translate_exp es in
    (F.erecord labels es)
  | _, ETuple(exps) -> 
    let es = List.map translate_exp exps in
    (F.etuple es)
  | _, EProj(rec_exp, label) -> 
    (* project is an op *)
    (F.eop (F.Project(label)) [translate_exp rec_exp])
  in
  {exp' with espan = exp.espan}
;;
let translate_pat (pat:C.pat)  (pat_sz : int) : F.pat = 
  match pat with 
  | PBit(ints) -> F.PVal(F.vpat ints)
  | C.PNum(z)  -> F.PVal(F.vint (Z.to_int z) pat_sz)
  | PEvent(cid, params) -> 
    let params = List.map (fun (id, ty) -> id, translate_ty ty) params in
    F.PEvent{event_id=cid; params;}
  | PWild ->
    F.PVal(F.vpat (List.init pat_sz (fun _ -> -1)))
;;

let rec translate_statement (stmt:C.statement) : F.statement = 
  let stmt' = match stmt.s with 
  | C.SNoop -> F.snoop |> F.swrap stmt.sspan
  | C.SUnit(exp) -> translate_exp exp |> F.sunit |> F.swrap stmt.sspan
  | C.SAssign(id, exp) -> 
    F.sassign (id) (translate_exp exp) |> F.swrap stmt.sspan
  | C.SLocal(id, ty, exp) -> 
    F.slocal (Cid.id id) (translate_ty ty) (translate_exp exp) |> F.swrap stmt.sspan
  | C.SPrintf(fmt_str, args) -> 
    (* printf  *)
    let str_exp = F.eval (F.string_to_value fmt_str) in
    let args = str_exp::List.map (fun arg -> (translate_exp arg)) args in
    let arg_tys = List.map (fun (arg:F.exp) -> arg.ety) args in
    let ret_ty = F.ty@@TUnit in
    let fty = F.tfun arg_tys ret_ty in
    let fexp = F.efunref (Cid.create ["printf"]) fty in
    let print_call = F.ecall fexp args in
    F.sunit print_call |> F.swrap stmt.sspan
  | C.SIf(exp, stmt1, stmt2) -> 
    let exp = translate_exp exp in
    let stmt1 = translate_statement stmt1 in
    let stmt2 = translate_statement stmt2 in
    F.sif exp stmt1 stmt2 |> F.swrap stmt.sspan
  | C.SGen(GSingle(None), ev) -> 
    F.egen_self (translate_exp ev) |> F.sunit |> F.swrap stmt.sspan
  | C.SGen(GSingle(Some(loc)), ev) -> 
    F.egen_switch (translate_exp loc) (translate_exp ev) |> F.sunit |> F.swrap stmt.sspan
  | C.SGen(GPort(port), ev) -> 
    F.egen_port (translate_exp port) (translate_exp ev) |> F.sunit |> F.swrap stmt.sspan
  | C.SGen(GMulti(port), ev) -> 
    F.egen_group (translate_exp port) (translate_exp ev) |> F.sunit |> F.swrap stmt.sspan
  | C.SSeq(s1, s2) -> 
    F.sseq (translate_statement s1) (translate_statement s2) |> F.swrap stmt.sspan
  | C.SMatch(exps, branches) -> 
    let pat_lens = List.map (fun (exp : C.exp) -> 
      InterpHelpers.intwidth_from_raw_ty exp.ety.raw_ty)
      exps 
    in    
    let exps = List.map translate_exp exps in
    (* if there's more than one expression, wrap it in a tuple *)
    let exp = match exps with 
      | [exp] -> exp
      | exps -> F.etuple exps
    in
    (* we have to expand a single wildcard into multiple wildcards *)
    let num_pats = List.length pat_lens in
    let rec extend_single_wild_pats branches = 
      match branches with 
      | [] -> []
      | ([C.PWild], stmt)::branches when num_pats > 1 -> 
        let new_pats = List.init num_pats (fun _ -> C.PWild) in
        let branches = extend_single_wild_pats branches in
        (new_pats, stmt)::branches
      | branch::branches -> branch::(extend_single_wild_pats branches)
    in
    let branches = extend_single_wild_pats branches in
    let branches = List.map 
      (fun (pats, stmt) -> 
        let pats = List.map2 translate_pat pats pat_lens in
        let stmt = translate_statement stmt in
        (pats, F.S(stmt)))
      branches
    in
    F.smatch exp branches |> F.swrap stmt.sspan
  | C.SRet(None) -> F.sret_none |> F.swrap stmt.sspan
  | C.SRet(Some(exp)) -> F.sret (translate_exp exp) |> F.swrap stmt.sspan
  | STupleAssign{ids; tys; exp;} -> 
    let cids = List.map Cid.id ids in
    let tys, new_vars = match tys with 
      | None -> [], false
      | Some(tys) -> List.map translate_ty tys, true
    in
    let exp = translate_exp exp in
    F.smultiassign cids tys new_vars exp |> F.swrap stmt.sspan
  in
  {stmt' with sspan = stmt.sspan}
;;

let translate_memop (m : CoreSyntax.memop) = 
  let (id, params, body) = Despecialization.despecialize_memop m in
  (* return type is the same as the parameter type *)
  let rty = (List.hd params |> snd) in
  (* translate this as a function, but with type FMemop *)
  F.dmemop id (translate_ty rty) (translate_params params) (translate_statement body)
;;

let translate_decl (decl:C.decl) : F.fdecl = 
 let decl' =  match decl.d with 
  | C.DGlobal(id, ty, exp) -> F.dglobal id (translate_ty ty) (translate_exp exp)
  | DExtern(id, ty) -> F.dextern id (translate_ty ty)
  | C.DEvent((evid, evnum_opt,ev_sort, params)) -> 
    let is_parsed = match ev_sort with 
      | C.EPacket -> true 
      | C.EBackground -> false
    in
    F.devent evid evnum_opt (List.map (fun (id, ty) -> id, translate_ty ty) params) is_parsed
  | C.DHandler(id, hdl_sort, (params, body)) -> 
  begin
    let params = List.map (fun (id, ty) -> id, translate_ty ty) params in
    let hdl_body = translate_statement body in
    let decl = F.dhandler id (F.tunit ()) params hdl_body in
    match hdl_sort with
      | C.HControl -> err "control handlers not supported"
      | C.HEgress ->  err "egress handlers not supported"
      | _ -> decl
  end
  | DMemop(memop) -> translate_memop memop
  | DUserTy(id, ty) -> F.dty (Cid.id id) (translate_ty ty)
  | DActionConstr(acn_constr) -> 
    (* first, build the inner action function *)
    let acn_body = match acn_constr.abody with 
      | [exp] -> translate_exp exp
      | exps -> F.etuple (List.map translate_exp exps)
    in
    let acn_fun = F.eaction (translate_params acn_constr.aparams) acn_body in
    (* now build the action constructor, which is just a regular function that returns the action *)
    F.dfun acn_constr.aid (acn_fun.ety) (translate_params acn_constr.aconst_params) (F.sret acn_fun)
    (* action translation strategy: there are two options for an action constructor. 
        an "action constructor" is a function that returns an action function.
        To translate into c: 
          the inner action is a function that takes its action parameter, 
          and a pointer to a context. The context holds the 
          values of the action's constructor parameter. 
          the action constructor is a function that takes its parameters, 
          it returns a pointer to the inner action and a pointer to the context. 
          Actually, we want those things to be pre-allocated. So it takes 
          a pointer to the context that it is supposed to fill. 
          It returns a pointer to the inner action and populates the 
          context. 
        option 2 is more complicated, and I think it boils down to something 
        like option 1. *)
  | DParser(id, params, parser_block) -> 
    (* despecialize parser with Core pass *)
    let parse_stmt = Despecialization.despecialize_parser_block parser_block in
    (* translate the statement *)
    F.dparser id (F.tunit ()) (translate_params params) (translate_statement parse_stmt)
  | DFun(id, rty, (params, body)) -> 
    F.dfun id (translate_ty rty) (translate_params params) (translate_statement body)
  in
  {decl' with dspan = decl.dspan}
    
;;
let translate_prog (ds : C.decls) : F.fdecls = 
  List.map translate_decl ds
;;
