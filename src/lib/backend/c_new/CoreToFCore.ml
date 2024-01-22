module C = CoreSyntax

module F = struct 
  (* FCoreSyntax with extra nodes for translation from Core IR *)
  include FCoreSyntax
  (* extensions for translation to / from CoreIr *)
  type ty_annot += | TGroup
  type exp_annot += ECallUnordered of bool
  type handler_kind = 
    | HData 
    | HControl
    | HEgress
  type decl_annot += EgressHandler
  (* Statements and handlers with statement bodies are only for translation to / from the IR *)

end
(* ops that are calls to builtins in FCore:
    flood
    printf
    event generation functions
    "update cell" -- a placeholder representing an update to a memory address
    Payload.skip
    Payload.peek
    Payload.read
*)
let cid s = s |> Id.create |> Cid.id
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
  | C.TName(cid, sizes, true) -> (F.tglobal cid (List.map size_to_ty sizes) ).raw_ty
  | C.TName(cid, [], false) -> (F.tname cid).raw_ty
  | C.TName(_, _, false) -> err "non-global named types should not have arguments"
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

let rec translate_statement (stmt:C.statement) : F.exp = 
  match stmt.s with 
  | C.SNoop -> F.eunit () |> F.ewrap stmt.sspan 
  | C.SUnit(exp) -> translate_exp exp |> F.ewrap stmt.sspan
  | C.SAssign(id, exp) -> 
    F.eassign (id) (translate_exp exp) |> F.ewrap stmt.sspan
  | C.SLocal(id, ty, exp) -> 
    F.elocal (Cid.id id) (translate_ty ty) (translate_exp exp) |> F.ewrap stmt.sspan
  | C.SPrintf(fmt_str, args) -> 
    (* printf  *)
    let str_exp = F.eval (F.string_to_value fmt_str) in
    let args = str_exp::List.map (fun arg -> (translate_exp arg)) args in
    let arg_tys = List.map (fun (arg:F.exp) -> arg.ety) args in
    let ret_ty = F.ty@@TUnit in
    let fty = F.tfun arg_tys ret_ty F.FNormal in
    let fexp = F.efunref (Cid.create ["printf"]) fty in
    let print_call = F.ecall fexp args in
    print_call |> F.ewrap stmt.sspan
  | C.SIf(exp, stmt1, stmt2) -> 
    let exp = translate_exp exp in
    let stmt1 = translate_statement stmt1 in
    let stmt2 = translate_statement stmt2 in
    F.eif exp stmt1 stmt2 |> F.ewrap stmt.sspan
  | C.SGen(GSingle(None), ev) -> 
    F.egen_self (translate_exp ev) |> F.ewrap stmt.sspan
  | C.SGen(GSingle(_), _) -> 
    err "generate to switch addresses not supported"
  | C.SGen(GPort(port), ev) 
  | C.SGen(GMulti(port), ev) -> 
    F.egen (translate_exp port) (translate_exp ev) |> F.ewrap stmt.sspan
  | C.SSeq(s1, s2) -> 
    F.eseq (translate_statement s1) (translate_statement s2) |> F.ewrap stmt.sspan
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
    F.ematch exp branches |> F.ewrap stmt.sspan
  | C.SRet(None) -> F.eret (F.eunit ()) |> F.ewrap stmt.sspan
  | C.SRet(Some(exp)) -> F.eret (translate_exp exp) |> F.ewrap stmt.sspan
  | STupleAssign{ids; tys; exp;} -> 
    let cids = List.map Cid.id ids in
    let tys, new_vars = match tys with 
      | None -> [], false
      | Some(tys) -> List.map translate_ty tys, true
    in
    let exp = translate_exp exp in
    F.emultiassign cids tys new_vars exp |> F.ewrap stmt.sspan
;;
(* memop translation is trickier. We don't know the address 
   that we're writing to until the call... hmm... 
   but the call is different in different contexts? 
   Is there not a compositional way to do this translation... *)

let translate_memop (m : CoreSyntax.memop) = 
  (* a basic memop is a function that takes two arguments 
     and returns its expression *)
  (* construct the args *)
  let args = List.map (fun (id, ty) -> id, translate_ty ty) m.mparams in
  (* construct the function type *)
  let ret_ty = match args with 
    | [] -> err "a memop must have arguments"
    | (_, ty)::_ -> ty
  in
  (* construct the function body *)
  let body_exp = match m.mbody, List.length args with 
    | C.MBReturn(exp), _ -> translate_exp exp
    | C.MBIf(expc, expl, expr), _ -> 
      F.eif (translate_exp expc) (translate_exp expl) (translate_exp expr)
    | C.MBComplex(body), n_args -> 
      let bool_update_exp = function 
        | None -> None 
        | Some(id, exp) -> 
          let exp = translate_exp exp in
          Some(F.elocal (Cid.id id) exp.ety exp)
      in 
      let cell_update_exp ecurval = function 
        | None -> None 
        | Some(econd, eval) -> 
          let econd = translate_exp econd in
          let enewval = translate_exp eval in
          Some(F.eif econd enewval ecurval)
      in 
      let (cell1_var : F.exp) = F.evar (List.nth args 0 |> fst |> Cid.id) ret_ty in
      (* cell2 is a local if there are only 3 args *)
      let cell2_id = match n_args with 
        | 3 -> cid "cell2"
        | 4 -> Cid.id (List.nth args 1 |> fst)
        | _ -> err "wrong number of args for memop"
      in
      (* cell2 might need to be initialized *)
      let cell2_init = match n_args with 
        | 3 -> 
          let init_val = F.eval (F.vint_ty 0 ret_ty) in
          Some(F.elocal cell2_id ret_ty init_val)
        | 4 -> None
        | _ -> err "wrong number of args for memop"
      in
      let cell2_var = F.evar cell2_id ret_ty in
      (* the return variable is always a local *)
      let ret_id  = cid "ret" in
      let ret_init = 
        let init_val = F.eval (F.vint_ty 0 ret_ty) in
        Some(F.elocal ret_id ret_ty init_val)
      in
      let ret_var = F.evar ret_id ret_ty in

      (* the body is a sequence of assignments, some optional *)
      let update_exps = List.filter_map (fun x -> x ) [
        cell2_init;
        ret_init;
        bool_update_exp body.b1;
        bool_update_exp body.b2;
        cell_update_exp cell1_var (fst body.cell1);
        cell_update_exp cell1_var (snd body.cell1);
        cell_update_exp cell2_var (fst body.cell2);
        cell_update_exp cell2_var (snd body.cell2);
        cell_update_exp ret_var body.ret;
      ]      
      in
      let body = List.fold_left 
        (fun acc exp -> F.eseq acc exp) 
          (List.hd update_exps)
          (List.tl update_exps)
      in
      F.eseq body ret_var
    in 
    (* construct the function *)
    F.dmemop m.mid args body_exp

;;

(*** parser translation ***)

let rec translate_parser_action (pa : C.parser_action) =
  match pa with 
  | PRead(cid, ty, exp) -> 
    let exp = translate_exp exp in
    F.elocal cid (translate_ty ty) exp
  | PPeek(cid, ty, exp) -> 
    let exp = translate_exp exp in
    F.elocal cid (translate_ty ty) exp
  | PSkip(ty) -> 
    let skip_arg = F.eval (F.vint_ty 0 (translate_ty ty)) in
    let skip_funref = F.efunref (Cid.create ["Payload"; "skip"]) (F.tfun [] (translate_ty ty) F.FNormal) in
    F.ecall skip_funref [skip_arg]
  | PAssign(cid, exp) -> 
    let exp = translate_exp exp in
    F.eassign cid exp
  | PLocal(cid, ty, exp) -> 
    let exp = translate_exp exp in
    F.elocal cid (translate_ty ty) exp
and translate_parser_branch ((pats, parser_block) : C.parser_branch) : F.branch = 
  let pats = List.map translate_pat pats in
  let block_exp = translate_parser_block parser_block in
  (pats, block_exp)

and translate_parser_step (pstep : C.parser_step) = 
  match pstep with 
  | PMatch([exp], branches) -> 
    let exp = translate_exp exp in
    let branches = List.map translate_parser_branch branches in
    F.ematch exp branches
  | PMatch(exps, branches) -> 
    let exps = List.map translate_exp exps in
    let exp = F.etuple exps in
    let branches = List.map translate_parser_branch branches in
    F.ematch exp branches
  | PGen(event_exp) -> 
    F.egen_self (translate_exp event_exp)
  | PCall(call_exp) -> 
    (* the exp _is_ a call *)
    translate_exp call_exp
  | PDrop -> F.eunit ()

and translate_parser_block (pb : C.parser_block) = 
  let action_exps = List.map (fun (a, span) -> translate_parser_action a |> F.ewrap span) pb.pactions in
  let step_exp = translate_parser_step (fst pb.pstep) |> F.ewrap (snd pb.pstep) in
  let exps = action_exps @ [step_exp] in
  let rec build_exp = function 
    | [] -> err "empty parser block"
    | [exp] -> exp
    | exp::exps -> F.eseq exp (build_exp exps)
  in
  build_exp exps
;;

let translate_decl (decl:C.decl) : F.fdecl = 
  match decl.d with 
  | C.DGlobal(id, ty, exp) -> F.dglobal id (translate_ty ty) (translate_exp exp)
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
    let decl = F.dhandler id params hdl_body in
    match hdl_sort with
      | C.HControl -> err "control handlers not supported"
      | C.HEgress ->  F.annot_decl decl (F.EgressHandler)
      | _ -> decl
  end
  | DMemop(memop) -> translate_memop memop
  | DExtern(id, ty) -> F.dextern id (translate_ty ty)
  | DUserTy(id, ty) -> F.dty (Cid.id id) (translate_ty ty)
  | DActionConstr(acn_constr) -> 
    (* first, build the inner action function *)
    let acn_body = match acn_constr.abody with 
      | [exp] -> translate_exp exp
      | exps -> F.etuple (List.map translate_exp exps)
    in
    let acn_fun = F.eaction (translate_params acn_constr.aparams) acn_body in
    (* now build the action constructor, which is just a regular function that returns the action *)
    F.dfun acn_constr.aid (translate_params acn_constr.aconst_params) acn_fun

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
    F.dparser id (translate_params params) (translate_parser_block parser_block)
  | DFun(id, _, (params, body)) -> 
    F.dfun id (translate_params params) (translate_statement body)
;;
let translate_prog (ds : C.decls) : F.fdecls = 
  List.map translate_decl ds
;;
