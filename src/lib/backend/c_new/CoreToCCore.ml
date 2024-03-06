module C = CoreSyntax

module F = CCoreSyntax

let printf = Printf.printf
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
  | C.TName(cid, []) -> (F.tname cid).raw_ty
  | C.TName(cid, sizes) -> (F.tglobal cid (List.map size_to_ty sizes)).raw_ty
  | C.TBuiltin(cid, raw_tys) -> (F.tglobal cid (List.map (fun raw_ty -> F.ty (translate_raw_ty raw_ty)) raw_tys )).raw_ty
  | C.TFun {arg_tys; ret_ty} -> 
    let fty : F.func_ty = {
      F.arg_sizes = [];
      F.arg_tys = List.map translate_ty arg_tys; 
      F.ret_ty = translate_ty ret_ty;
      F.func_kind = F.FNormal;}
    in
    F.TFun fty
  | C.TMemop(n_args, Sz(arg_size)) -> 
    let arg_tys = List.init n_args (fun _ -> F.ty@@F.TInt(F.sz arg_size)) in
    let ret_ty = F.ty@@F.TInt(F.sz arg_size) in
    let fty : F.func_ty = {
      F.arg_sizes = [];
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
      F.arg_sizes = [];
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
  | C.TGroup -> (F.tgroup).raw_ty
  | C.TPat(Sz(sz)) -> F.TBits{ternary=true; len=F.sz sz}
  | C.TPat(_) -> err "TPat size should be a singleton"
  | C.TBits(Sz(sz)) -> F.TBits{ternary=false; len=F.sz sz}
  | C.TBits(_) -> err "TBits size should be a singleton"
and translate_acn_ty (aty : C.acn_ty) = 
  {
    F.arg_sizes = [];
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
  | _, C.EVar(c) -> F.evar c (translate_ty exp.ety)
  | _, C.EOp(op, es) -> F.eop (translate_op op) (List.map translate_exp es)
  | {raw_ty=C.TEvent}, C.ECall(cid, es, _) -> 
    let fexp = F.efunref cid (F.tevent) in 
    F.eevent fexp (List.map translate_exp es)
  (* if its not an event, its an ordered or unordered call *)
  | ret_ty, C.ECall(cid, es, ignores_ordering) -> (
    (* NOTE: we lose information about whether or not the user 
       asked us to ignore ordering constraints. If that is important later, 
       we can add a special call kind. *)
    let _ = ignores_ordering in (* we don't care if it ignores ordering *)
    (* reconstruct the functions assumed type based on arg and expression types *)
    let arg_tys = List.map (fun e -> e.C.ety) es in
    let fty = F.tfun 
      (List.map translate_ty arg_tys)
      (translate_ty ret_ty)
    in
    let fexp = F.efunref cid fty in 
    F.ecall fexp (List.map translate_exp es)
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
        (pats, (stmt)))
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

(* if the parameters are a flattened list 
   (not a single-element tuple or record), 
    wrap them in a tuple. *)
let is_flattened_tuple params : bool =
     List.length params > 1
  && List.for_all 
      (fun (_, (ty : C.ty)) -> match ty.raw_ty with 
        | TTuple _ 
        | TRecord _ -> false
        | _ -> true)
      params
;;

(* given a list of non-container parameters, pack them into a tuple 
   and return a map from each parameter's id to the 
   id and position of the new tuple. *)
let tuple_params(params : C.params) : 
    (C.id * C.ty)                 (* tuple name and type *)
  * ((C.id * (C.id * int)) list)  (* dict from param name to tuple name and index *)
=
  if ((is_flattened_tuple params) = false) then 
      err "[tuple_params] not a flattened tuple!";
  let split_at_last_underscore str =
    let i = String.rindex str '_' in
    let base = String.sub str 0 i in
    let idx = String.sub str (i + 1) ((String.length str) - i - 1) in
    (base, int_of_string idx)
  in
  let tup_name, tup_inner_rawtys, subst_dict = List.fold_left 
    (fun (_, tup_inner_rawtys, subst_dict) (field_id, (field_ty : C.ty))  -> 
      let tup_name, field_index = split_at_last_underscore (Id.name field_id) in
      let tup_inner_rawty = field_ty.raw_ty in
      let rename_entry = field_id, (Id.create tup_name, field_index) in 
      tup_name, (tup_inner_rawtys@[tup_inner_rawty]), subst_dict@[rename_entry]      
    )
    ("", [], [])
    params
  in
  ((Id.create tup_name), (C.ttuple tup_inner_rawtys)), (subst_dict)
;;


let translate_decl (decl:C.decl) : F.decl = 
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
    (* requirement: action constructor is only allowed to have 1 parameter in 
       each of its parameter set. 1 install-time parameter, 1 run-time parameter, 
       1 return parameter. The parameters may be tuples or records. *)

    (* pack parameter lists into tuples, updating 
       action body as needed. *)
    let pack_params params  =
      match params with 
      | [] -> (Id.fresh "empty", C.ttuple []), []
      | params when is_flattened_tuple params -> 
        print_endline ("[pack_params] packing flattened tuple into tuple:");
        print_endline (CorePrinting.params_to_string params);
        tuple_params params
      | param::[] -> param, []
      | _ -> err "[pack_params] invalid action arguments for translating to C Core."
    in
    let const_param, const_rename_map = pack_params acn_constr.aconst_params in
    let param, param_rename_map      = pack_params acn_constr.aparams in
    let ret_ty      = C.ttuple (List.map (fun (ty : C.ty) -> ty.raw_ty) acn_constr.artys) in 


    let field_replacer = 
      (* replace a evar reference to a field with a tuple get op *)
      object 
      inherit [_] F.s_map as super
      method! visit_exp  tup_ty_param_rename_map exp = 
        let tup_ty, param_rename_map = tup_ty_param_rename_map in 
        match exp.e with 
        | EVar(cid) -> (
          let id = Cid.to_id cid in
          match (List.assoc_opt id param_rename_map) with 
          | None -> {exp with e=EVar(cid)}
          | Some(tup_id, field_idx) -> 
            {exp with 
              e=((F.eop (Get(field_idx)) [F.evar (Cid.id tup_id) tup_ty]).e)}
        )
        | _ -> super#visit_exp tup_ty_param_rename_map exp
      end
    in
    (* build the action body and replace fields with tuple get ops *)
    let acn_body = match acn_constr.abody with 
      | [exp] -> translate_exp exp
      | exps -> F.etuple (List.map translate_exp exps)
    in
    let acn_body = List.fold_left2 
      (fun acn_body (_, ty) rename_map -> 
        field_replacer#visit_exp (translate_ty ty, rename_map) acn_body)
      acn_body
      [const_param; param]
      [const_rename_map; param_rename_map]
    in
    (* finally, build the action: a function with two arguments 
       and a single return that has the translated version of the renamed body *)
    F.daction 
      acn_constr.aid 
      (translate_ty ret_ty)
      (translate_params [const_param; param])
      (F.sret acn_body)
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
let translate_prog (ds : C.decls) : F.decls = 
  List.map translate_decl ds
;;
