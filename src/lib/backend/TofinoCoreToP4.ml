(* translate new tofinocore (6/24) into p4tofino syntax -- hopefully more direct... *)

open TofinoCoreNew
open P4TofinoSyntax
open InterpHelpers
(* strategy:
    convert each component separately, then do a small pass 
    to wire the components up (basically, generate the main call) *)

module CL = Caml.List
module CS = TofinoCoreNew
module T = P4TofinoSyntax

(*** basic syntax ***)
let rec translate_ty ty = match ty.raw_ty with 
  | CS.TBool -> T.TBool
  | CS.TInt (s) -> T.TInt (s)
  | CS.TAction(_) -> error "[translate_ty] action types should be eliminated in IR..."
  | CS.TFun(fty) -> tfun (translate_ty fty.ret_ty) (List.map translate_ty fty.arg_tys)
  | _ -> error "[translate_ty] translation for this type is not implemented"
;;

let translate_rty rty =
  match rty with 
  | CS.TInt(s) -> T.TInt(s)
  | _ -> error "[translate_rty] not a TInt"
;;
let translate_value v =
  match v with 
  | CS.VBool(b) -> T.VBool(b)
  | CS.VInt(z) -> vint (Integer.to_int z) (Some (Integer.size z))
  (* | CS.VInt(z) -> vint (Integer.to_int z) None *)
  | _ -> error "[translate_value] values must be bools or ints"
;;

let translate_op o = match o with 
  | CS.And -> T.And
  | CS.Or -> T.Or
  | CS.Not -> T.Not
  | CS.Eq -> T.Eq
  | CS.Neq -> T.Neq
  | CS.Less -> T.Lt
  | CS.More -> T.Gt
  | CS.Leq -> T.Lte
  | CS.Geq -> T.Gte
  | CS.Neg -> T.Not
  | CS.Plus -> T.Add
  | CS.Sub -> T.Sub
  | CS.SatPlus -> T.SatAdd
  | CS.SatSub -> T.SatSub
  | CS.Cast(_)-> T.Cast
  | CS.Conc -> T.Concat
  | CS.BitAnd -> T.BAnd
  | CS.BitOr -> T.BOr
  | CS.BitXor -> T.BXor
  | CS.BitNot -> T.Not
  | CS.LShift -> T.LShift
  | CS.RShift -> T.RShift
  | CS.Slice(_) ->T.Slice
  | CS.PatExact -> error "[coreToP4Tofino.translate_op] patterns are not implemented!"
  | CS.PatMask -> error "[coreToP4Tofino.translate_op] patterns are not implemented!"
;;

let translate_pat pat = 
  match pat with
    | CS.PWild -> T.PWild
    | PNum z -> T.PNum (Z.to_int z)
    | PBit bits -> (
      let bits = List.map 
        (fun i -> match i with 0 -> T.B0 | 1 -> T.B1 | _ -> T.BANY)
        bits
      in 
      PBitstring bits
    )
;;
let translate_sunit_ecall stmt =
  match stmt.s with 
  | SUnit({e=ECall(acn_id, _); _}) -> sunit (ecall acn_id [])
  | _ -> error "[translate_branch] branch statement must be a method call to a labeled block, which represents an action"
;;
let translate_branch complete_branches 
    ((pats:CS.pat list),(stmt:CS.statement))
    : ((T.pat list * T.statement) list) =    

  complete_branches@[(List.map translate_pat pats, translate_sunit_ecall stmt)]
;;



let dotstring_of_cid cid = 
    Caml.String.concat "." @@ Cid.names cid
let string_of_fcncid = dotstring_of_cid
let cell_struct_id rid = 
  (id ((Id.name rid)^("_fmt")))
let cell_ty_of_array rid : T.ty = 
    tstruct (cell_struct_id rid)
;;

module CidMap = Collections.CidMap
module IdMap = Collections.IdMap

(*** environment  ***)
type prog_env = {
  memops : memop IdMap.t;
  (* defined_fcns : tdecl CidMap.t; *)
  tables : (CS.tbl_def * Span.t) CidMap.t;
  vars   : exp CidMap.t;
}

let empty_env = {memops = IdMap.empty; 
(* defined_fcns = CidMap.empty;  *)
tables = CidMap.empty; vars=CidMap.empty}
;;

let bind_var env (x, y) : prog_env = 
  {env with vars=CidMap.add x y env.vars}
;;
let bind_tbl env (x, y) = 
  {env with tables=CidMap.add x y env.tables}
;;
let bind_memop env (x, y) =
  {env with memops=IdMap.add x y env.memops}
;;

let find_tbl env x =
  match CidMap.find_opt (Cid.id x) env.tables with 
   | Some(o) -> o
   | _ -> error ("[find_tbl] could not find a table for variable "^(Id.to_string x))
;;

(*** expression translation... ***)

(* memop cell names *)
let cell1_local = id "cell1_local" ;; (* read everywhere. *)
let cell2_local = id "cell2_local" ;; (* *)
let cell1_remote = id "cell1_remote" ;; (* written *)
let cell2_remote = id "cell2_remote" ;;
let ret_remote = id "ret_remote" ;; 
let remote_pair_id = id "remote"
let local_pair_id = id "local"

let exp_of_cellid cell_id cell_ty = 
CoreSyntax.var_sp 
    (cell_id) 
    (CoreSyntax.ty cell_ty) 
    (Span.default)
;;


let rec translate_exp (prog_env:prog_env) exp : (T.decl list * T.expr) =
  match exp.e with 
  | ECall(fcn_cid, args) -> (
    (* at this point, all call expressions are to builtin modules *)
    translate_module_call prog_env fcn_cid args )
  | CS.EHash (size, args) -> translate_hash prog_env size args 
  | CS.EFlood _ -> error "[translate_memop_exp] flood expressions inside of memop are not supported"
  | CS.EVal v -> [], T.eval_ty_sp (translate_value v.v) (translate_ty exp.ety) exp.espan
  (* for variables, check renaming first *)
  | CS.EVar cid -> (
    match (CidMap.find_opt cid prog_env.vars) with 
      | Some (new_exp) -> translate_exp prog_env new_exp
      (* no renaming to do, use the given id *)
      | None -> [], T.evar_ty_sp cid (translate_ty exp.ety) exp.espan
  )
  | CS.EOp(op, args) -> (
    let args = match op with 
      (* cast and slice have arguments in a different form *)
      | CS.Cast(sz) -> 
        let _, exps = translate_exps prog_env args in 
        (eval_int sz)::(exps)
      | CS.Slice(s, e) -> 
        let _, exps = translate_exps prog_env args in 
        (eval_int s)::(eval_int e)::(exps)
      | _ -> translate_exps prog_env args |> snd
    in
    let op = translate_op op in 
    [], eop_ty_sp op args (translate_ty exp.ety) exp.espan
  )
  | CS.ETableCreate(_) ->
    error "[coreToP4Tofino.translate_exp] got an etablecreate expression. This should have been handled by the declaration translator."

and translate_exps (prog_env:prog_env) exps = 
  let translate_exp_wrapper exp = translate_exp prog_env exp in
  let decls, exps = List.map translate_exp_wrapper exps |> List.split in
  List.flatten decls, exps

and translate_hash prog_env size args = 
  let hasher_id = Id.fresh_name ("hash") in
  let hasher_id = Id.create ((fst hasher_id)^(string_of_int (snd hasher_id))) in 
  let poly, args = match args with
    | poly::args -> poly, args
    | _ -> error "[translate_hash] invalid arguments to hash call" 
  in
  let args = (snd (translate_exps prog_env args)) in 
  let arg = elist args in 
  let dhash = decl (DHash{
    id = hasher_id;
    poly = int_from_exp poly;
    out_wid = size;
    })
  in
  let hasher_call = ecall 
    (Cid.create_ids [hasher_id; id "get"]) 
    [arg]
  in
  [dhash], hasher_call



and translate_sys_call fcn_id _ = 
  (* TODO: renaming pass before translation *)
  let e_ts = eop 
    (Slice) 
    [eval_int 47; eval_int 16; T.evar (Cid.create ["ig_intr_md"; "ingress_mac_tstamp"])]
  in 
  let fcn_name = List.nth (Cid.names fcn_id) 1 in 
  match fcn_name with
  | "time" -> [], e_ts
  | "random" -> (
    (* declare rng_###, call rng_###.get(); *)
    let rng_id = Id.fresh_name "rng" in
    let decl = drandom 32 rng_id in
    let expr = ecall (Cid.create_ids [rng_id; id "get"]) [] in
    [decl], expr
  )
  | s -> error ("[translate_sys_call] unknown sys function: "^s)

and translate_module_call (env:prog_env) fcn_id args = 
  match (List.hd (Cid.names fcn_id)) with 
      | "Array" -> 
        let regacn_id, decl = translate_array_call env fcn_id args in
        let idx_arg = translate_memop_exp env
          (List.hd (List.tl args))
        in        
        [decl], ecall (Cid.create_ids [regacn_id; (id "execute")]) [idx_arg]
      | "PairArray" -> 
        let regacn_id, decl = translate_pairarray_call env fcn_id args in 
        let idx_arg = translate_memop_exp env
          (List.hd (List.tl args))
        in        
        [decl], ecall (Cid.create_ids [regacn_id; (id "execute")]) [idx_arg]
      | "Sys" -> translate_sys_call fcn_id args
      | s -> error ("[translate_memop_call] unknown module: "^s)


(*** array calls and memop exps ***)

(* translate exp in a memop -- this used to be its own method, 
   but now we re-use the rename context *)
and translate_memop_exp env exp = 
  translate_exp env exp |> snd 
and translate_memop_exps env exps =
  List.map (translate_memop_exp env) exps


(* translate a conditional return expression into an if statement 
   with a single branch for the backend that writes to var lhs_id. *)
and translate_conditional_return env lhs_id condition_return_opt =
  match condition_return_opt with 
  | None -> T.Noop
  | Some(econd, erhs) -> (
    sif 
      (translate_memop_exp env econd)
      (sassign (lhs_id) (translate_memop_exp env erhs))
  )

(* translate a complex memop used for an array into an apply statement *)
and translate_array_memop_complex env (returns:bool) cell_ty (args: exp list) (memop : memop) =
  (* args are those passed to the memop, NOT the Array method call *)
  match memop.mbody with 
  | MBReturn _ -> error "not supported"
  | MBIf _ -> error "not supported"
  | MBComplex(body) -> (
    (* step 1: bind param := arg *)
    let cell1_in_arg = exp_of_cellid (Cid.id cell1_local) cell_ty in 
    let args = cell1_in_arg::args in 
    let param_ids = memop.mparams |> List.split |> fst |> List.map Cid.id in 

    (* bind parameters *)
    let ctx = List.fold_left
      (fun ctx (param_id, arg) -> 
        bind_var ctx (param_id, arg))
      env
      (List.combine param_ids args)
    in 
    (* bind "cell1" and "cell2", which may appear in the return statement. *)
    let ctx = List.fold_left
      (fun ctx (param_id, arg) -> 
        bind_var ctx (param_id, arg))
      ctx
      [
        (Cid.create ["cell1"], exp_of_cellid (Cid.id cell1_remote) cell_ty);
        (Cid.create ["cell2"], exp_of_cellid (Cid.id cell2_local) cell_ty);
      ]
    in 
      (* bind booleans *)
      let ctx = List.fold_left
        (fun ctx booldef -> 
          match booldef with 
          | Some(id, exp) ->
            bind_var ctx ((Cid.id id),exp)
          | None -> ctx
        )
        ctx
        [body.b1; body.b2]
      in 
      (* translate the rest of the expression using the context.*)
      (* the main statements of the body *)
      let statements = List.map
        (fun (var_id, cr_exp) -> translate_conditional_return ctx var_id cr_exp)
        (
          [
          (Cid.id cell1_remote, fst body.cell1);
          (Cid.id cell1_remote, snd body.cell1);
          (Cid.id cell2_local, fst body.cell2);
          (Cid.id cell2_local, snd body.cell2)
          ]
        (* only add the returns statement if... the call returns *)
        @(if returns then [(Cid.id ret_remote, body.ret)] else [])
        )
      in 
      (* now, we need to add initialization statements *)
      let cell_ty = translate_rty cell_ty in 
      let init_stmts = [
        (* copy the value of cell 1 to a local variable *)
        local cell1_local cell_ty (T.evar (Cid.id cell1_remote));
        (* there is no remote for cell 2 in a regular array *)
        local cell2_local cell_ty (eval_int 0)
        ]
      in 
      let statements = init_stmts@statements in
      (* params are references to remote cell1 and the return variable, if the function returns*)
      let params = if (returns)
        then [
          inoutparam cell_ty cell1_remote;
          outparam cell_ty ret_remote;]
        else [
          inoutparam cell_ty cell1_remote;
        ]
      in 
      (params, sseq statements)
  )


and translate_pairarray_memop_complex env slot_ty cell_ty (args: exp list) (memop : memop) =
  match memop.mbody with 
  | MBReturn _ -> error "not supported"
  | MBIf _ -> error "not supported"
  | MBComplex(body) -> (
    (* tricky part: 
      1. the register's type is NOT cell_ty. It is some struct 
         that contains a pair of cell_tys
      2. cell1_remote and cell2_remote are fields of that struct.
        *)
    (* update the remote cells, so that they are part of the remote var *)
    let cell1_remote_cid = Cid.create_ids [remote_pair_id; id "lo"] in  
    let cell2_remote_cid = Cid.create_ids [remote_pair_id; id "hi"] in  

    (* 1. add cell1 and cell2 args *)
    let cell1_in_field = exp_of_cellid (Cid.create_ids [local_pair_id; id "lo"]) cell_ty in
    let cell2_in_field = exp_of_cellid (Cid.create_ids [local_pair_id; id "hi"]) cell_ty in
    let args = cell1_in_field::cell2_in_field::args in 
    let param_ids = memop.mparams |> List.split |> fst |> List.map Cid.id in 
    (* bind parameters to arguments *)
    let ctx = List.fold_left
      (fun ctx (param_id, arg) -> 
        bind_var ctx (param_id, arg))
      env
      (List.combine param_ids args)
    in 
    (* rename cell1 and cell2 to remote.lo and remote.hi 
      bind "cell1" and "cell2", which may appear in the return statement. *)
    let ctx = List.fold_left
      (fun ctx (param_id, arg) -> 
        bind_var ctx (param_id, arg))
      ctx
      [
        (Cid.create ["cell1"], exp_of_cellid cell1_remote_cid cell_ty);
        (Cid.create ["cell2"], exp_of_cellid cell2_remote_cid cell_ty);
      ]
    in 
    (* bind booleans *)
    let ctx = List.fold_left
      (fun ctx booldef -> 
        match booldef with 
        | Some(id, exp) ->
          bind_var ctx ((Cid.id id),exp)
        | None -> ctx
      )
      ctx
      [body.b1; body.b2]
    in 
    (* translate the rest of the expression using the context.*)
    (* the main statements of the body *)
    let statements = List.map
      (fun (var_id, cr_exp) -> translate_conditional_return ctx var_id cr_exp)
        [
        (cell1_remote_cid, fst body.cell1);
        (cell1_remote_cid, snd body.cell1);
        (cell2_remote_cid, fst body.cell2);
        (cell2_remote_cid, snd body.cell2);
        (Cid.id ret_remote, body.ret)
        ]
    in 
    (* now, we need to add initialization statements *)
    let cell_ty = translate_rty cell_ty in 
    let init_stmts = [
      (* copy the value of cell 1 and cell2 to a local variable *)
      local local_pair_id slot_ty (T.evar (Cid.id remote_pair_id))
      ]
    in     
    let statements = init_stmts@statements in
    (* params are references to remote cell1 and the return variable, if the function returns*)
    let params =[
        inoutparam slot_ty remote_pair_id;
        outparam cell_ty ret_remote;]
    in 
    (params, sseq statements)
  )

(* translate an array call expression into an object that can be called in an assign or local *)
and translate_array_call env fcn_id args =
  let reg = name_from_exp (List.hd args) in 
  let idx = List.hd (List.tl args) in 
  let reg_id =  reg |> Cid.to_id in 
  let reg_acn_id = Id.fresh_name ((fst reg_id)^"_regaction") in 
  let memop, args, cell_ty, returns = match (string_of_fcncid fcn_id) with 
    | "Array.update_complex" -> (
      let  memop, arg1, arg2 = match args with
        | [_; _; memop; arg1; arg2; _] -> memop, arg1, arg2
        | _ -> error "[translate_array_call] unexpected arguments for array.update_complex"
      in 
      let memop = IdMap.find 
        (InterpHelpers.name_from_exp memop |> Cid.to_id )
        env.memops 
      in
      let cell_ty = raw_ty_of_exp arg1 in 
      memop, [arg1; arg2], cell_ty, true
    )
    | "Array.update" 
    | "Array.getm"
    | "Array.setm"
    | "Array.get"
    | "Array.set" -> error "[coreToP4Tofino] All array method calls should have been converted to Array.update_complex by this point."
    | s -> error ("[translate_array_call] unknown array method: "^s)
  in
  reg_acn_id, decl (DRegAction{
    id = reg_acn_id;
    reg = reg_id;
    idx_ty = InterpHelpers.raw_ty_of_exp idx |> translate_rty;
    mem_fcn = translate_array_memop_complex env returns cell_ty args memop;
    })

and translate_pairarray_call env fcn_id (args:CS.exp list) =
  let reg = name_from_exp (List.hd args) in 
  let slot_ty = cell_ty_of_array (Cid.to_id reg) in 
  let idx = List.hd (List.tl args) in 
  let reg_id =  reg |> Cid.to_id in 
  let reg_acn_id = Id.fresh_name ((fst reg_id)^"_regaction") in 
  let memop, args, cell_ty = match (string_of_fcncid fcn_id) with 
    | "PairArray.update" -> (
      let  memop, arg1, arg2 = match args with
        | [_; _; memop; arg1; arg2; _] -> memop, arg1, arg2
        | _ -> error "[translate_pairarray_call] unexpected arguments for array.update"
      in 
      let memop = IdMap.find 
        (InterpHelpers.name_from_exp memop |> Cid.to_id )
        env.memops 
      in
      let cell_ty = raw_ty_of_exp arg1 in 
      memop, [arg1; arg2], cell_ty
    )
    | s -> error ("[translate_pairarray_call] unknown pairarray method: "^s)
  in
  reg_acn_id, decl (DRegAction{
    id = reg_acn_id;
    reg = reg_id;
    idx_ty = InterpHelpers.raw_ty_of_exp idx |> translate_rty;
    mem_fcn = translate_pairarray_memop_complex env slot_ty cell_ty args memop;
    })
;;

(*** statements ***)

let declared_vars prev_decls = CL.filter_map (fun dec -> 
  match dec.d with 
    | DVar(id, _, _) -> Some id
    | _ -> None
  )
  prev_decls

(* translate a statement that appears inside of an action. 
   generate a list of decls and a statement. *)
let rec translate_statement env prev_decls stmt =
  match stmt.s with
  | SNoop -> [], Noop
  | CS.SUnit(e) ->
    let decls, expr = translate_exp env e in 
    decls, sunit expr
  | CS.SLocal(id, ty, e) -> (
    (* variables can't be declared inside of an action, so produce a global declaration 
       with a default value and return an assign statement. *)
    match (List.exists (fun idb -> Id.equals id idb) (declared_vars prev_decls)) with
    | true -> (
      (* let vardecl = dvar_uninit id (translate_rty ty.raw_ty) in  *)
      let decls, expr = translate_exp env e in 
      decls, sassign (Cid.id id) expr
    )
    | false -> (
      let vardecl = dvar_uninit id (translate_rty ty.raw_ty) in 
      let decls, expr = translate_exp env e in 
      vardecl::decls, sassign (Cid.id id) expr
    )
  )
  | CS.SAssign(id, e) -> 
    let decls, expr = translate_exp env e in 
    (* use the new name, if there is one *)
    decls, sassign id expr
  | SPrintf _ -> error "[translate_statement] printf should be removed by now"
  | SIf _ -> error "[translate_statement] if statement cannot appear inside action body"
  | SMatch _ -> error "[translate_statement] match statement cannot appear inside action body"
  | SRet _ -> error "[translate_statement] ret statement cannot appear inside action body"
  | SGen(_) -> 
      error "[translate_statement] generates should have been eliminated"
  | SSeq (s1, s2) -> 
    let s1_decls, s1_stmt = translate_statement env prev_decls s1 in
    let s2_decls, s2_stmt = translate_statement env prev_decls s2 in
    s1_decls@s2_decls, sseq [s1_stmt; s2_stmt]
  | STableMatch _ | STableInstall _ -> error "[coreToP4Tofino.translate_statement] tables not implemented"
;;

(* generate an action, and other compute objects, from an open function *)
let translate_openfunction env p4tdecls tdecl =
  let new_decls = match tdecl.td with 
    | TDOpenFunction(id, const_params, stmt) -> (
      let params = List.map
        (fun (id, ty) -> (None, translate_ty ty, id))
        const_params
      in
      let body_decls, body = translate_statement env p4tdecls stmt in
      let action = daction id params body in
      body_decls@[action]
    )
    | _ -> []
  in
  p4tdecls@new_decls
;;


(*** threaded control flow statements (tables, etc) ***)

let fresh_table_id _ = Id.fresh_name "table"
;;

(* translate a table object declaration into a P4 table. 
   Note: we need the match statement that calls the table 
   because it contains the key *)
let translate_table env tdef tspan pragmas keys =
  let tid = tdef.tid in
  let size = CoreSyntax.exp_to_int tdef.tsize in
  let actions = translate_exps env tdef.tactions |> snd in
  (* let actions = List.map CoreSyntax.id_of_exp tdef.tactions in *)
  let default_aid, default_args = tdef.tdefault in 
  let default = Some(
    scall 
      default_aid 
      (translate_exps env default_args |> snd))
  in
  dtable_sp tid keys actions [] default (Some(size)) pragmas tspan
;;

(* translate a match or if statement into a table *)
(* An if statement always translates to calling a dynamically updateable user table. 
   A match statement always translates to calling a static, compiler-generated table. 
   No other statements should appear in the body of the main handler at this point. *)
let stmt_to_table env (_:pragma) (ignore_pragmas: (id * pragma) list) (tid, stmt) : (T.decl * T.statement) = 
  let ignore_parallel_tbls_pragmas = ((List.remove_assoc tid ignore_pragmas |> List.split |> snd;)) in 
(*   let action_expr_of_branch (_, stmt) = 
    match stmt.s with
    | SUnit({e=ECall(acn_id, _); _}) -> evar_noretmethod acn_id
    | _ -> error "[action_expr_of_branch] branch should call an action function"  
  in *)
  let action_cid_of_branch (_, stmt) =
    match stmt.s with
      | SUnit({e=ECall(acn_id, _); _}) -> acn_id  
      | _ -> error "[action_id_of_branch] branch does not call an action function"
  in
  match stmt.s with
  | SIf(e, {s=STableMatch(tm);}, _) ->   
    (* match keys are specified in the statement *)
    let keys = translate_exps env tm.keys 
      |> snd 
      |> List.map exp_to_ternary_key
    in
    let tdef, tspan = find_tbl env tid in 
    let tbl_decl = translate_table env tdef tspan ignore_parallel_tbls_pragmas keys in
    let _, e' = translate_exp env e in
    let tbl_call = sif e' (sunit (ecall_table tdef.tid)) in
    tbl_decl, tbl_call
  | SMatch(exps, branches) -> 
    let _, keys = translate_exps env exps in 
    let keys = List.map exp_to_ternary_key keys in 
    let actions = List.map action_cid_of_branch branches 
      |> MiscUtils.unique_list_of 
      |> List.map evar_noretmethod
    in 
    let rules, default_opt = match branches with
      (* a single empty branch means there are no const rules, just a default action *)
      | [([], call_stmt)] -> ([], Some(translate_sunit_ecall call_stmt))
      (* anything else means there are const rules and no default *)
      | branches -> (
        (List.fold_left 
          (fun branches' (pats, stmt) -> 
            branches'
            @[(List.map translate_pat pats, translate_sunit_ecall stmt)])
          []
          branches)
        , None)
    in
    let tbl_dec = 
      dtable tid keys actions rules default_opt None ignore_parallel_tbls_pragmas
    in
    let tbl_call = sunit (ecall_table tid) in 
    (tbl_dec, tbl_call)
  | _ -> error "[generate_table] not a match statement!"
;;


(* translate a sequence of match and if statements into 
   a vector of tables executing in the same stage. *)
let sseq_to_stage stage_num env block_id seq_stmt =
  let stage_pragma = {pname="//stage "^(string_of_int stage_num); pargs = [];} in   
  let stmts = InterpHelpers.unfold_stmts seq_stmt in
  (* user tables already have ids *)
  let table_ids = List.map 
    (fun call_stmt -> match call_stmt.s with
      | SIf(_, {s=STableMatch(tm);}, _) -> CoreSyntax.id_of_exp tm.tbl
      | SMatch(_) -> fresh_table_id ()
      | _ -> error "[sseq_to_stage] a statement in this stage is something besides a conditional table_match or an unconditional match statement.")
    stmts
  in
  let ignore_dep_pragmas = List.map
    (fun tbl_id ->
      (tbl_id, 
        {pname="ignore_table_dependency";
        pargs=["\""^Id.name block_id^"."^(Id.name tbl_id)^"\""];}
    ))
    table_ids
  in 
  (* table declarations and calls *)
  let decls_calls = List.map 
    (stmt_to_table env stage_pragma ignore_dep_pragmas) 
    (List.combine table_ids stmts)
  in 
  decls_calls
;;


(* generate stages from a list of sequence statements in the main handler *)
let rec statements_to_stages stage_num env block_id (stages:CS.statement list) =
  match stages with 
  | [] -> []
  | hd_stage::stages ->
    (sseq_to_stage stage_num env block_id hd_stage)@(statements_to_stages (stage_num + 1) env block_id stages)
;;

(*** unsorted translators / constructors ***)
let generate_added_var_decls tds = List.map 
  (fun (id, ty) -> dvar_uninit id (translate_ty ty))
  (main_handler_of_decls tds).hdl_preallocated_vars
;;

let includes = [dinclude "<core.p4>"; dinclude "<tna.p4>"]


let mc_recirc_decls evids recirc_port =
  let num_events = List.length evids in
  let possible_rids = MiscUtils.range 1 (1 + num_events) in 
  let mc_group_n_recirc gid =
    let replicas = List.map (fun rid -> (recirc_port, rid)) (MiscUtils.range 1 (gid+1)) in 
    decl (DMCGroup{gid; replicas})
  in
  List.map mc_group_n_recirc possible_rids
;;


let rec constr_env env tdecls = 
  match tdecls with
  | [] -> env
  | tdecl::tdecls' -> (
    let env' = match tdecl.td with
      | TDMemop(m) -> bind_memop env (m.mid, m)
      | TDGlobal(tbl_id, _, {e=ETableCreate(tbl_def); espan=espan;}) -> bind_tbl env ((Cid.id tbl_id), (tbl_def, espan))
      (* | TDOpenFunction(id, _, _) -> {prog_env with defined_fcns=(Cid.id id, tdecl)::prog_env.defined_fcns;} *)
      | _ -> env
    in
    constr_env env' tdecls')
;;

let params_of_main (_:hevent) =
  (* TODO *)
  error "[params_of_main] TODO: parameters of a main handler "
;;

let translate_ingress comp = 
  (* set up the environment for main *)
  let env = constr_env empty_env comp.comp_decls in
  (* declarations for added variables and open functions *)
  let decls = 
    (generate_added_var_decls comp.comp_decls)
    @(List.fold_left
      (translate_openfunction env)
      []
      comp.comp_decls)
  in
  (* declarations and statements from main handler *)
  let hmain = (main_handler_of_component comp) in
  let main_body = match hmain.hdl_body with 
    | SPipeline(stmts) -> stmts
    | _ -> error "shoulda been pipeliend by now"
  in
  let table_decls, table_calls = statements_to_stages 0 env hmain.hdl_id main_body |> List.split in 
  let apply_body = sseq table_calls in 

  (* why all this separation? *)
  let mc_decls = mcgroups_of_decls decls in 
  let action_decls = non_mcgroups_of_decls decls in 
  let ingress_control = decl (DControl{
    id = hmain.hdl_id;
    params = params_of_main hmain;
    decls = action_decls@table_decls;
    body = Some(apply_body);
    })
  in
  []

;;



(* given a context of previous tdecls and a 
   tdecl, translate the tdecl into a decl and optionally 
   update the context.  *)
let translate_decl tdecls tdecl : (tdecl list * decl list) = 
  match tdecl.td with 
  | TDAction _ -> error "there should be no decls by now"
  (* these just get saved *)
  | TDEvent _
  | TDMemop _
  | TDExtern _ -> tdecls@[tdecl], []
  (* these should be translated  *)
  | TDGlobal _ -> tdecls@[tdecl], []
  | TDOpenFunction _ -> tdecls@[tdecl], []
  | TDHandler _ -> tdecls@[tdecl], []
  | TDParser _ -> tdecls@[tdecl], []
;;

(*   | TDAction _ 
  | TDVar _
  | 
 *)  
;;

(* core task: translate a component into a 
  list of p4 declarations... (or is that 1 p4 declaration? UGH) *)
let translate_component component : decl list = 
  let (_, p4decls) = List.fold_left 
    (fun (ldecls, p4decls) ldecl -> 
      let ldecls_new, p4decls_new = translate_decl ldecls ldecl in
      ldecls@ldecls_new,
      p4decls@p4decls_new)
    ([], [])
    component.comp_decls
  in
  p4decls
;;

let translate_prog prog =
  List.map translate_component prog
;;



