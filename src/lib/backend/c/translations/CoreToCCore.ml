module C = CoreSyntax
module F = CCoreSyntax

(* 
FIX:
  - group values translate into tuples, but group type is port type (some sized int)   

*)

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

(* declare builtin functions as externs in CCore *)
(* TODO: add more *)
(* note that config for port and switch sizes is inferred during translation *)
let builtin_externs config =
  let open F in
  let open CCoreConfig in
  let cid = Cid.create in
  [
    dfun_extern (cid ["generate_self"]) FNormal [tevent] tunit Span.default;
    dfun_extern (cid ["generate_port"]) FNormal [tint config.port_id_size; tevent] tunit Span.default;
    dfun_extern (cid ["generate_switch"]) FNormal [tint config.switch_id_size; tevent] tunit Span.default;
    dfun_extern (cid ["generate_group"]) FNormal [(F.tint config.port_id_size); tevent] tunit Span.default;    
    (* dvar_extern (Cid.id Builtins.ingr_port_id) (F.tint config.port_id_size); *)
    (* recirc port id is a constant from a config file *)
    dvar_const  (Cid.id Builtins.recirc_id) (F.tint config.port_id_size) (F.eval (F.vint config.recirc_port config.port_id_size));
    dvar_const (Cid.id Builtins.self_id) (F.tint config.switch_id_size) (F.eval (F.vint config.self_id_num config.switch_id_size));
    
  ] 
;;
let builtin_cids = List.filter_map 
  (fun (decl: F.decl) -> 
    match decl.d with
    | F.DFun _ -> F.extract_dfun_cid decl
    | F.DVar _ -> F.extract_dvar_cid decl
    | _ -> None)
  (builtin_externs (CCoreConfig.cfg))
;;
(* a singleton size is an int; 
   a list of sizes is a tuple *)
let size_to_ty = function 
  | C.Sz(sz) -> F.ty@@F.TInt(F.sz sz)
  | C.Szs(szs) -> F.ttuple @@ List.map (fun sz -> F.ty@@F.TInt(F.sz sz)) szs
;;
let rec bits_to_ints = function 
  | BitString.B0::bs -> 0::(bits_to_ints bs)
  | BitString.B1::bs -> 1::(bits_to_ints bs)
  | [] -> []
;;


(* helpers for actions and action types *)
(* given a list of non-container parameters, pack them into a tuple 
   and return a map from each parameter's id to the 
   id and position of the new tuple. *)
let retuple_params(params : C.params) : 
    (C.id * C.ty)                 (* tuple name and type *)
  * ((C.id * (C.id * int)) list)  (* dict from param name to tuple name and index *)
=
  let split_at_last_underscore str =
    (* assumes that parameters came from a tuple and were given their names by 
       the convention in TupleElimination.ml *)
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
let retuple_tys tys = 
  match tys with
    | [ret_ty] -> ret_ty
    | _ -> 
      C.ttuple (List.map (fun (ty : C.ty) -> ty.raw_ty) tys) 
;; 

let rec translate_raw_ty (raw_ty : C.raw_ty) : F.raw_ty = 
  match raw_ty with 
  | C.TBool -> F.TBool
  | C.TInt(Sz sz) ->  F.TInt(F.sz sz)
  | C.TInt(_) -> err "TInt size should be a singleton"
  | C.TEvent -> F.TEvent
  | C.TName(cid, sizes) when 
    List.exists 
      (fun (builtin_ty_cid, _) -> Cid.equal builtin_ty_cid cid) 
      Builtins.builtin_tycid_to_ty ->
        (F.tbuiltin cid (List.map size_to_ty sizes)).raw_ty
  | C.TName(cid, []) -> (F.tname cid).raw_ty (* user-defined type *)
  | C.TName(_, _) -> 
      err "unexpected: a TName with size args that is not a builtin"
      (* (F.tbuiltin cid (List.map size_to_ty sizes)).raw_ty *)
      (* builtin types with size args *)
  | C.TBuiltin(cid, raw_tys) -> 
    (F.tbuiltin cid (List.map (fun raw_ty -> F.ty (translate_raw_ty raw_ty)) raw_tys )).raw_ty
      (* builtin types with type args *)
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
    (* let pack_action_param_tys param_tys = 
      match param_tys with 
      | [] -> F.ttuple [] 
      | 

    in *)
    (* an action constructor is a normal function that returns an action *)
    (* hm... this does not agree with the definition of an action, 
       where we translate an action as a function with 2 arguments and a return. *)
    (* let param_tys = List.map translate_ty aconst_param_tys in
    let arg_tys = List.map translate_ty aacn_ty.aarg_tys in
    let ret_tys = List.map translate_ty aacn_ty.aret_tys in *)

    let param_ty = retuple_tys aconst_param_tys |> translate_ty in
    let arg_ty = retuple_tys aacn_ty.aarg_tys |> translate_ty in
    let ret_ty = retuple_tys aacn_ty.aret_tys |> translate_ty in
    F.TFun {
      F.arg_tys = [param_ty; arg_ty];
      F.ret_ty = ret_ty;
      F.func_kind = F.FAction;
    }
    (* translate type lists to *)
    (* let fty : F.func_ty = {

    } *)
    (* let fty : F.func_ty = {
      F.arg_tys = List.map translate_ty aconst_param_tys; 
      F.ret_ty = F.ty@@F.TFun(translate_acn_ty aacn_ty);
      F.func_kind = F.FNormal;}
    in
    F.TFun fty *)
  | C.TRecord(id_rawty_pairs) -> 
    let ids, raw_tys = List.split id_rawty_pairs in
    let cids = List.map Cid.id ids in
    let raw_tys = List.map translate_raw_ty raw_tys in
    let tys = List.map F.ty raw_tys in
    F.TRecord(cids, tys)
  | C.TTuple(raw_tys) ->
    let raw_tys = List.map translate_raw_ty raw_tys in
    let tys = List.map F.ty raw_tys in
    F.TTuple(tys)
  | C.TGroup -> (F.tint CCoreConfig.cfg.port_id_size).raw_ty
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
   timplements = None}
;;
let translate_params (params: C.params) : F.params = 
  List.map (fun ((id: Id.t), ty) -> Cid.id id, translate_ty ty) params
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
  | C.Cast(Sz sz) -> F.Cast(F.tint sz)
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
  | _, C.VGlobal(_) -> err "VGlobals should not appear outside of the interpreter's execution"
  | _, C.VGroup(locs) -> (F.vtup (List.map (fun i -> F.vint 32 i ) locs)).v
  | _, C.VPat(tbits) -> (F.vpat tbits).v
  | _, C.VBits(bits) -> (F.vbits (bits_to_ints bits)).v
  | {raw_ty=C.TTuple(raw_tys)}, C.VTuple(vs) -> 
    let tys = List.map C.ty raw_tys in
    let vs = List.map2 translate_v vs tys in
    let values = List.map2 F.value vs (List.map translate_ty tys) in
    (* let values = List.map F.value vs in *)
    (F.vtuple values).v
  | _, C.VTuple(_) -> err "VTuple type should be a tuple"
  | {raw_ty=C.TRecord(id_rawty_pairs)}, C.VRecord(id_v_pairs) -> 
    let labels, vs = List.split id_v_pairs in
    let tys = List.map (fun id -> List.assoc id id_rawty_pairs |> C.ty) labels in
    let labels = List.map Cid.id labels in
    let vs = List.map2 translate_v vs tys in
    let values = List.map2 F.value vs (List.map translate_ty tys) in
    (F.vrecord (List.combine labels values)).v
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
  | _, EFlood(port_exp) -> 
    (* flood is a call to a builtin *)
    let arg_tys = [translate_ty port_exp.ety] in
    (* let ret_ty = translate_ty ret_ty in *)
    let ret_ty = (F.tint CCoreConfig.cfg.port_id_size) in
    let fty = F.tfun arg_tys ret_ty in
    let fexp = F.efunref (Cid.create ["flood"]) fty in
    (* cast "flood" calls, which should just be banned, to the right type *)
    F.ecall fexp [translate_exp port_exp]
  | _, ERecord(label_exp_pairs) -> 
    let labels, es = List.split label_exp_pairs in
    let labels = List.map Cid.id labels in
    let es = List.map translate_exp es in
    (F.erecord (List.combine labels es))
  | _, ETuple(exps) -> 
    let es = List.map translate_exp exps in
    (F.etuple es)
  | _, EProj(rec_exp, label) -> 
    (* project is an op *)
    let label = Cid.id label in
    (F.eop (F.Project(label)) [translate_exp rec_exp])
  in
  {exp' with espan = exp.espan}
;;
let translate_pat (pat:C.pat) (ty : C.ty) : F.pat = 
  match pat with 
  | PBit(ints) -> F.PVal(F.vpat ints)
  | C.PNum(z)  -> 
    let pat_sz = InterpHelpers.intwidth_from_raw_ty ty.raw_ty in
    F.PVal(F.vint (Z.to_int z) pat_sz)
  | PEvent(cid, params) -> 
    let params = List.map (fun (id, ty) -> Cid.id id, translate_ty ty) params in
    F.PEvent {event_id=cid; params;}
  | PWild -> F.PWild (translate_ty ty)
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
    let loc = translate_exp loc in
    let loc = if CCoreSyntax.bitsizeof_ty_exn loc.ety < CCoreConfig.cfg.switch_id_size 
      then CCoreSyntax.ecast (F.tint CCoreConfig.cfg.switch_id_size) loc
      else loc
    in
    F.egen_switch (loc) (translate_exp ev) |> F.sunit |> F.swrap stmt.sspan
  | C.SGen(GPort(port), ev) -> 
    let port = translate_exp port in
    print_endline("port type: " ^ (CCorePPrint.ty_to_string port.ety));
    let port = if CCoreSyntax.bitsizeof_ty_exn port.ety < CCoreConfig.cfg.switch_id_size 
      then CCoreSyntax.ecast (F.tint CCoreConfig.cfg.switch_id_size) port
      else port
    in
    print_endline("here");
    F.egen_port (port) (translate_exp ev) |> F.sunit |> F.swrap stmt.sspan
  | C.SGen(GMulti(port), ev) -> 
    let port = translate_exp port in
    let port = if CCoreSyntax.bitsizeof_ty_exn port.ety < CCoreConfig.cfg.switch_id_size 
      then CCoreSyntax.ecast (F.tint CCoreConfig.cfg.switch_id_size) port
      else port
    in
    F.egen_group (port) (translate_exp ev) |> F.sunit |> F.swrap stmt.sspan
  | C.SSeq(s1, s2) -> 
    F.sseq (translate_statement s1) (translate_statement s2) |> F.swrap stmt.sspan
  | C.SMatch(exps, branches) -> 
    let pat_tys = List.map (fun (exp : C.exp) -> exp.ety) exps in
    let exps = List.map translate_exp exps in
    (* we have to expand a single wildcard into multiple wildcards *)
    let num_pats = List.length pat_tys in
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
        let pats = List.map2 translate_pat pats pat_tys in
        let stmt = translate_statement stmt in
        (pats, (stmt)))
      branches
    in
    F.smatch exps branches |> F.swrap stmt.sspan
  | C.SRet(None) -> F.sret_none |> F.swrap stmt.sspan
  | C.SRet(Some(exp)) -> F.sret (translate_exp exp) |> F.swrap stmt.sspan
  | STupleAssign{ids; tys=None; exp} -> 
    let cids = List.map Cid.id ids in
    let exp = translate_exp exp in
    let cid_tys = F.extract_ttuple exp.ety in 
    let exps = List.map2 F.evar cids cid_tys in
    F.stupleassign exps exp |> F.swrap stmt.sspan
  | STupleAssign{ids; tys=Some(tys); exp;} -> 
    let cids = List.map Cid.id ids in
    let tys=List.map translate_ty tys in 
    let exp = translate_exp exp in
    F.stuplelocal cids tys exp |> F.swrap stmt.sspan
  in
  {stmt' with sspan = stmt.sspan}
;;

let translate_memop (m : CoreSyntax.memop) = 
  let (id, params, body, rty) = Despecialization.despecialize_memop m in
  (* translate this as a function, but with type FMemop *)
  F.dmemop (Cid.id id) (translate_ty rty) (translate_params params) (translate_statement body)
;;


let is_record (param : (C.id * C.ty)) = 
  match (snd param).raw_ty with 
  | C.TRecord _ -> true
  | _ -> false
;;


let translate_decl (decl:C.decl) : F.decl = 
 let decl' =  match decl.d with 
  | C.DGlobal(cid, ty, exp) -> F.dglobal (Cid.id cid) (translate_ty ty) (translate_exp exp)
  | DExtern(cid, ty) -> F.dextern (Cid.id cid) (translate_ty ty)
  | C.DEvent((evid, evnum_opt,ev_sort, params)) -> 
    let is_parsed = match ev_sort with 
      | C.EPacket -> true 
      | C.EBackground -> false
    in
    F.devent (Cid.id evid) evnum_opt (List.map (fun (id, ty) -> Cid.id id, translate_ty ty) params) is_parsed
  | C.DHandler(id, hdl_sort, (params, body)) -> 
  begin
    let params = List.map (fun (id, ty) -> Cid.id id, translate_ty ty) params in
    let hdl_body = translate_statement body in
    let decl = F.dhandler (Cid.id id) (F.tunit) params hdl_body in
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
      | [] -> (Id.fresh "empty", C.ttuple []), [] (* no params -- empty tuple *)
      | [param] -> param, [] (* one param -- no change *)
      | params ->
        (* The only way we could possibly have multiple parameters 
           is if tuple elimination unpacked a tuple parameter. *)
        (* print_endline ("[pack_params] packing flattened tuple into tuple:");
        print_endline (CorePrinting.params_to_string params); *)
        retuple_params params
      (* | _ -> err "[pack_params] action arguments must either be a singleton or a list of non-containers (not tuple or record)" *)
    in
    let const_param, const_rename_map = pack_params acn_constr.aconst_params in
    let param, param_rename_map      = pack_params acn_constr.aparams in
    (* wrap in a tuple if there's not exactly one type. 
       This matches what retuple_params does, but its simpler because 
       we're only operating on types and there are no parameters to rename. *)
    (* also, there's no "empty return" for an action. *)
    let ret_ty = match acn_constr.artys with
      | [ret_ty] -> ret_ty
      | _ -> 
        C.ttuple (List.map (fun (ty : C.ty) -> ty.raw_ty) acn_constr.artys) 
    in 
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
      (Cid.id acn_constr.aid) 
      (translate_ty ret_ty)
      (translate_params [const_param; param])
      (F.sret acn_body)
  | DParser(id, params, parser_block) -> 
    (* despecialize parser with Core pass *)
    let parse_stmt = Despecialization.despecialize_parser_block parser_block in
    (* translate the statement *)
    F.dparser (Cid.id id) (F.tunit) (translate_params params) (translate_statement parse_stmt)
  | DFun(id, rty, (params, body)) -> 
    F.dfun (Cid.id id) (translate_ty rty) (translate_params params) (translate_statement body)
  in
  {decl' with dspan = decl.dspan}
;;

(* infer the sizes of location identifiers in the program, which includes port_id and switch_id. *)
   let infer_loc_id_sizes = 
    let v = object inherit [_] C.s_iter as super
      method! visit_exp () exp = 
        match exp.e with
        | C.EVar(cid) when (Cid.equal (Cid.id Builtins.ingr_port_id) cid) -> 
          CCoreConfig.cfg.port_id_size <- C.size_of_tint exp.ety;
        | _ -> super#visit_exp () exp
      
      method! visit_statement () stmt = 
        match stmt.s with 
        | C.SGen(GSingle(Some(loc)), _) -> 
          CCoreConfig.cfg.switch_id_size <- C.size_of_tint loc.ety;
        | C.SGen(GPort(port), _) -> 
          CCoreConfig.cfg.port_id_size <- C.size_of_tint port.ety;
        | _ -> super#visit_statement () stmt
    end
    in
    v#visit_decls ()
  ;;

let translate (ds : C.decls) : F.decls = 
  (* translate declarations first in case something in there uses a port.. *)
  (* if (CCoreConfig.cfg.driver == "interp") then  *)  
  (* infer_loc_id_sizes ds; *)
  let ds = List.map translate_decl ds in
  (builtin_externs (CCoreConfig.cfg))@ds
;;
