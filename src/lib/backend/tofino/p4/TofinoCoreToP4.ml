(* translate new tofinocore (6/24/23) into p4tofino syntax *)

open TofinoCore
open P4TofinoSyntax
open InterpHelpers

module CL = Caml.List
module CS = TofinoCore
module T = P4TofinoSyntax

module CidMap = Collections.CidMap
module IdMap = Collections.IdMap
module CidSet = Collections.CidSet

let fresh_numbered_id s =
  let id = Id.fresh_name s in
  let id = Id.create ((fst id)^(string_of_int (snd id))) in 
  id
;;


(*** basic syntax translation ***)
let rec translate_ty ty = match ty.raw_ty with 
  | CS.TBool -> T.TBool
  | CS.TInt (s) -> T.TInt (s)
  | CS.TAction(_) -> error "[translate_ty] action types should be eliminated in IR..."
  | CS.TFun(fty) -> tfun (translate_ty fty.ret_ty) (List.map translate_ty fty.arg_tys)
  | CS.TName(cid, _, _) -> tstruct (Cid.to_id cid)
  | _ -> error "[translate_ty] translation for this type is not implemented"
;;

let size_of_tint ty = 
  match ty.raw_ty with 
  | TInt(sz) -> sz
  | _ -> error "[size_of_tint] not a tint"
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
  | SUnit({e=ECall(acn_id, _, _); _}) -> sunit (ecall acn_id [])
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

let cell_struct_of_array_ty module_name cell_width rid : T.decl =
  decl (match module_name with
      | "Array" -> DStructTy{
        id=cell_struct_id rid; 
        sty=TMeta;
        fields=[id "lo", tint cell_width];
        } 
      | "PairArray" -> DStructTy{
        id=cell_struct_id rid; 
        sty=TMeta;
        fields=[id "lo", tint cell_width; id "hi", tint cell_width];
        } 
      | _ -> error "[cell_struct_of_array_ty] unsupported module -- expected Array or PairArray")
;;


(*** helper functions ***)
(*** environment  ***)
type prog_env = {
  memops : memop IdMap.t;
  actions : CidSet.t;
  (* defined_fcns : tdecl CidMap.t; *)
  tables : (CS.tbl_def * Span.t) CidMap.t;
  vars   : exp CidMap.t;
  (* hashers that have already been constructed, 
     because we can't deduplicate hash expressions 
     that appear inside of Array ops in coreIr *)
  generated_hashers : ((int * int * string) * id) list
}

let empty_env = {memops = IdMap.empty; 
actions = CidSet.empty;
(* defined_fcns = CidMap.empty;  *)
tables = CidMap.empty; vars=CidMap.empty;
generated_hashers = [];}
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

(* inside of register actions, we need to fold constants in relational 
   operations. These constants do not show up until this pass, 
   because we don't generate register actions until this point, and we 
   generate them from memops which have unbound parameters in their bodies. 
    e.g.: if (mem - 1 < 10) needs to be converted into if (mem < 9)     
      
   to fold these constants, the translate_exp_in_memop function does:
   1. bind parameter (with evar_replacer)
   2. do const folding (with relation_const_folding)
   3. translate resulting expression normally (with translate_exp)
   - typically, we would bind parameters inline with the translation, 
     but the expressions with the consts only show up after binding, 
     so we do it in its own step first.*)
    
let evar_replacer = 
  object 
  inherit [_] CoreSyntax.s_map as super
  method! visit_exp prog_env exp = 
    let exp = super#visit_exp prog_env exp in 
    match exp.e with
    | EVar(cid) -> (
      match (CidMap.find_opt cid prog_env.vars) with
      | Some(exp') -> super#visit_exp prog_env exp'
      | _ -> exp
    )
    | _ -> exp
  end
;;

(* constant folding across relationals *)
let rec relation_const_folding exp = 
  (* this handles a few cases of const folding across relational operations. 
     For inside of memops. *)
  let empty_ctx = IdMap.empty in
  match exp.e with
  (* vleft + eleft <relop> vright *)
  | EOp(op, [{e=EOp(Plus, [{e=EVal(vleft)}; eleft])}; {e=EVal(vright); ety=ety;}])
  | EOp(op, [{e=EOp(Plus, [eleft; {e=EVal(vleft)}])}; {e=EVal(vright); ety=ety;}]) -> (
    match op with 
    | Less | More | Leq | Geq | Eq | Neq -> (
      let new_right = CoreSyntax.op Sub [eval vright ety; eval vleft ety] ety in 
      let eright' = PartialInterpretation.interp_exp empty_ctx new_right in 
      let res = {exp with e = EOp(op, [eleft; eright'])} in
      res
    )
    | _ -> exp
  )
  (* eleft - vleft <relop> vright *)
  | EOp(op, [{e=EOp(Sub, [eleft; {e=EVal(vleft)}])}; {e=EVal(vright); ety=ety;}]) -> (
    match op with 
    | Less | More | Leq | Geq | Eq | Neq -> (
      let new_right = CoreSyntax.op Plus [eval vright ety; eval vleft ety] ety in 
      let eright' = PartialInterpretation.interp_exp empty_ctx new_right in 
      let res = {exp with e = EOp(op, [eleft; eright'])} in
      res
    )
    | _-> exp
  )
  (* relationals nested in booleans.. *)
  | EOp(boolop, args) -> (
    match boolop with 
    | And | Or | Not -> (
      let args' = List.map relation_const_folding args in
      {exp with e=EOp(boolop, args')}
    )
    | _ -> exp
  )
  | _ -> exp
;;

let fst_snd ((_, y), _) = y

let rec translate_exp (prog_env:prog_env) exp : (T.decl list * T.expr) * prog_env =
  match exp.e with 
  | ECall(fcn_cid, args, _) ->  translate_ecall prog_env fcn_cid args
  | CS.EHash (size, args) -> translate_hash prog_env size args 
  | CS.EVal v -> 
    ([], T.eval_ty_sp (translate_value v.v) (translate_ty exp.ety) exp.espan), prog_env
  | CS.EFlood _ -> error "[translate_memop_exp] flood expressions inside of memop are not supported"
  | CS.EVar cid -> (
    match (CidMap.find_opt cid prog_env.vars) with 
      (* we may have just replaced a var with a val, which could add a constant that needs to be folded.. *)
      | Some (new_exp) -> translate_exp prog_env new_exp
      (* no renaming to do, use the given id *)
      | None -> ([], T.evar_ty_sp cid (translate_ty exp.ety) exp.espan), prog_env
  )
  | CS.EOp(op, args) -> (
    let args = match op with 
      (* cast and slice have arguments in a different form *)
      | CS.Cast(sz) -> 
        let (_, exps), _ = translate_exps prog_env args in 
        (eval_int sz)::(exps)
      | CS.Slice(s, e) -> 
        let (_, exps), _ = translate_exps prog_env args in 
        (eval_int s)::(eval_int e)::(exps)
      | _ -> 
        let (_, exps), _ = translate_exps prog_env args in 
        exps
    in
    let op = translate_op op in 
    ([], eop_ty_sp op args (translate_ty exp.ety) exp.espan), prog_env
  )
  | CS.ETableCreate(_) ->
    error "[coreToP4Tofino.translate_exp] got an etablecreate expression. This should have been handled by the declaration translator."

and translate_exps (prog_env:prog_env) exps : (decl list * expr list) * prog_env = 
  let translate_exp_wrapper exp = translate_exp prog_env exp in
  let decls_exps, _ = List.map translate_exp_wrapper exps |> List.split in
  let decls, exps = List.split decls_exps in
  (List.flatten decls, exps), prog_env

and translate_hash prog_env size args : (decl list * expr) * prog_env = 
  match (List.hd args).e with 
  (* special case: checksum hash, which may appear in the deparser *)
  | EVar(cid) when (Cid.equal (Cid.id Builtins.checksum_id) cid) -> 
    translate_checksum prog_env (List.tl args)
  (* base case: just a hash *)
  | _ -> 
    let poly, args = match args with
      | poly::args -> poly, args
      | _ -> error "[translate_hash] invalid arguments to hash call" 
    in
    let args = (fst_snd (translate_exps prog_env args)) in 
    let arg = elist args in 
    let poly = int_from_exp poly in
    let hasher_id_opt = List.assoc_opt (poly, size, (P4TofinoPrinting.string_of_expr arg |> P4TofinoPrinting.doc_to_string)) prog_env.generated_hashers in
    (* hasher with same args is already declared, re-use it *)
    match hasher_id_opt with 
    | Some(hasher_id) -> (
      (* print_endline ("RE USING HASHER! "^(Id.to_string hasher_id)); *)
      ([], ecall (Cid.create_ids [hasher_id; id "get"]) [arg]), prog_env)
    | None -> (
    (* hasher is not declared -- construct a new one and update the environment *)
    let hasher_id = fresh_numbered_id "hash" in
    let dhash = decl (DHash{
      id = hasher_id;
      poly = poly;
      out_wid = size;
      })
    in
    let hasher_call = ecall 
      (Cid.create_ids [hasher_id; id "get"]) 
      [arg]
    in
    let prog_env = {prog_env with generated_hashers = ((poly, size, (P4TofinoPrinting.string_of_expr arg |> P4TofinoPrinting.doc_to_string)), hasher_id)::prog_env.generated_hashers} in
    ([dhash], hasher_call), prog_env
    )

and translate_checksum prog_env args =
  (* translate a checksum. This version should only be called for the deparser.  *)
  (* ah. and the way that we translate checksums is different in each. Of course it is. *)
  let checksum_obj_id = fresh_numbered_id "checksum" in
  let checksum_obj = dobj (id "Checksum") [] [] checksum_obj_id (Span.default) in
  let arglist = elist (fst_snd (translate_exps prog_env args)) in 
  let checksum_call = ecall
    (Cid.create_ids [checksum_obj_id; id "update"])
    [arglist]
  in
  ([checksum_obj], checksum_call), prog_env

and translate_ecall env fcn_cid args = 
  (* first, check if its an action *)
  if (CidSet.mem fcn_cid env.actions) then 
    ([], ecall_action fcn_cid []), env
  else (
    match (Cid.names fcn_cid) with
    (* system functions *)
    | ["Sys"; "enable"]
    | ["Sys"; "time"]
    | ["Sys"; "random"] 
    | ["Sys"; "dequeue_depth"] ->
      translate_sys_call (List.hd (List.tl (Cid.to_ids fcn_cid))) args, env
    (* Array functions *)
    | "Array"::_ -> 
      (* generate the memop *)
      let (regacn_id, decl), env = translate_array_call env fcn_cid args in
      (* translate the index, generating a hash decl if the arg is a hash op *)
      let (idx_decls, idx_arg), env = translate_exp env (List.hd (List.tl args)) in
      (idx_decls@[decl], ecall (Cid.create_ids [regacn_id; (id "execute")]) [idx_arg]), env
    | "PairArray"::_ -> 
      let (regacn_id, decl), env = translate_pairarray_call env fcn_cid args in 
      let (idx_decls, idx_arg), env = translate_exp env (List.hd (List.tl args)) in
      (idx_decls@[decl], ecall (Cid.create_ids [regacn_id; (id "execute")]) [idx_arg]), env
    | _ -> error "[translate_ecall] unknown function")

and translate_sys_call fcn_id args = 
      match (Id.name fcn_id) with
      | "enable" -> 
      (* make a header valid, used to serialize egress *)
      let method_cid = match (List.hd args) with
        | {e=CS.EVar(cid)} -> (Cid.names cid)@["setValid"] |> Cid.create
        | _ -> error "[translate_ecall] invalid argument to enable"
      in
      ([], ecall_method method_cid)
      | "time" -> 
        [], 
        eop 
          (Slice) 
          [eval_int 47; eval_int 16; T.evar (Cid.create ["ingress_intrinsic_metadata"; "ingress_mac_tstamp"])]  
      | "random" -> 
        (* declare rng_###, call rng_###.get(); *)
        let rng_id = Id.fresh_name "rng" in
        let decl = drandom 32 rng_id in
        let expr = ecall (Cid.create_ids [rng_id; id "get"]) [] in
        [decl], expr      
      | "dequeue_depth" ->
         (* just a rename to the intrinsic variable *)
        [], T.evar (Cid.create ["egress_intrinsic_metadata"; "deq_qdepth"])
      | _ -> error "[translate_sys_call] unknown lucid system call"


(*** array calls and memop exps ***)
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
  (reg_acn_id, decl (DRegAction{
    id = reg_acn_id;
    reg = reg_id;
    idx_ty = InterpHelpers.raw_ty_of_exp idx |> translate_rty;
    mem_fcn = translate_array_memop_complex env returns cell_ty args memop;
    })), env

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
  (reg_acn_id, decl (DRegAction{
    id = reg_acn_id;
    reg = reg_id;
    idx_ty = InterpHelpers.raw_ty_of_exp idx |> translate_rty;
    mem_fcn = translate_pairarray_memop_complex env slot_ty cell_ty args memop;
    })), env



(* translate exp in a memop -- this used to be its own method, 
   but now we re-use the rename context *)
and translate_memop_exp env exp = 
  (* replace param evars with arguments first, so we can fold constants *)
  let folded_exp = 
    evar_replacer#visit_exp env exp
    |> relation_const_folding
  in
  (* translate the expression. This shouldn't do any other replacements *)
  translate_exp env folded_exp |> fst_snd

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
let rec translate_statement env prev_decls stmt : (decl list * T.statement) * prog_env =
  match stmt.s with
  | SNoop -> ([], Noop), env
  | CS.SUnit(e) ->
    let (decls, expr), env = translate_exp env e in 
    (decls, sunit expr), env
  | CS.SLocal(id, ty, e) -> (
    (* variables can't be declared inside of an action, so if a variable has not been 
      declared yet, produce a global declaration 
       with a default value and return an assign statement. *)
    match (List.exists (fun idb -> Id.equals id idb) (declared_vars prev_decls)) with
    (* I think this case should be an error -- if the variable is already declared, 
       we shouldn't see another declaration statement. *)
    | true -> (
      (* let vardecl = dvar_uninit id (translate_rty ty.raw_ty) in  *)
      let (decls, expr), env = translate_exp env e in 
      (decls, sassign (Cid.id id) expr), env
    )
    | false -> (
      let vardecl = dvar_uninit id (translate_rty ty.raw_ty) in 
      let (decls, expr), env = translate_exp env e in 
      (vardecl::decls, sassign (Cid.id id) expr), env
    )
  )
  | CS.SAssign(id, e) -> 
    let (decls, expr), env = translate_exp env e in 
    (* use the new name, if there is one *)
    (decls, sassign id expr), env
  | SPrintf _ -> error "[translate_statement] printf should be removed by now"
  | CS.SIf(exp, s1, s2) -> (
    (* this is used in the deparser block for checksum ops *)
    let (exp_decls, expr), env = translate_exp env exp in
    let (s1_decls, s1_stmt), s1_env = translate_statement env prev_decls s1 in
    match s2.s with 
    | CS.SNoop -> 
      (exp_decls@s1_decls, sif expr s1_stmt), s1_env
    | _ -> 
      let (s2_decls, s2_stmt), s2_env = translate_statement s1_env prev_decls s2 in
      (exp_decls@s1_decls@s2_decls, sifelse expr s1_stmt s2_stmt), s2_env
  )
  | SMatch _ -> error "[translate_statement] match statement cannot appear inside action body"
  | SRet _ -> error "[translate_statement] ret statement cannot appear inside action body"
  | SGen(_) -> 
      error "[translate_statement] generates should have been eliminated"
  | SSeq (s1, s2) -> 
    let (s1_decls, s1_stmt), s1_env = translate_statement env prev_decls s1 in
    let (s2_decls, s2_stmt), s2_env = translate_statement s1_env prev_decls s2 in
    (s1_decls@s2_decls, sseq [s1_stmt; s2_stmt]), s2_env
  | STableMatch _ | STableInstall _ -> error "[coreToP4Tofino.translate_statement] tables not implemented"
;;

(* generate an action, and other compute objects, from an open function
let translate_openfunction env tdecl =
  let new_env, new_decls = match tdecl.td with 
    | TDOpenFunction(id, const_params, stmt) -> (
      let params = List.map
        (fun (id, ty) -> (None, translate_ty ty, id))
        const_params
      in
      let body_decls, body = translate_statement env [] stmt in
      let action = daction id params body in
      (* add the open function to the actions context *)
      let env = {env with actions = CidSet.add (Cid.id id) env.actions} in
      env, body_decls@[action]
    )
    | _ -> env, []
  in
  new_env, new_decls
;; *)


(*** threaded control flow statements (tables, etc) ***)

let fresh_table_id _ = Id.fresh_name "table"
;;

(* translate a table object declaration into a P4 table. 
   Note: we need the match statement that calls the table 
   because it contains the key *)
let translate_table env tdef tspan pragmas keys =
  let tid = tdef.tid in
  let size = CoreSyntax.exp_to_int tdef.tsize in
  let action_ids = translate_exps env tdef.tactions |> fst_snd in
  (* let actions = List.map CoreSyntax.id_of_exp tdef.tactions in *)
  let default_aid, default_args = tdef.tdefault in 
  let default = Some(
    scall 
      default_aid 
      (translate_exps env default_args |> fst_snd))
  in
  dtable_sp tid keys action_ids [] default (Some(size)) pragmas tspan
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
      | SUnit({e=ECall(acn_id, _, _); _}) -> acn_id  
      | _ -> error "[action_id_of_branch] branch does not call an action function"
  in
  match stmt.s with
  | SIf(e, {s=STableMatch(tm);}, _) ->   
    (* match keys are specified in the statement *)
    let keys = translate_exps env tm.keys 
      |> fst_snd 
      |> List.map exp_to_ternary_key
    in
    let tdef, tspan = find_tbl env tid in 
    let tbl_decl = translate_table env tdef tspan ignore_parallel_tbls_pragmas keys in
    let (_, e'), _ = translate_exp env e in
    let tbl_call = sif e' (sunit (ecall_table tdef.tid)) in
    tbl_decl, tbl_call
  | SMatch(exps, branches) -> (
    let (_, keys), _ = translate_exps env exps in 
    (* small optimization: if there's only 1 key and all the patterns matching that key are exact (except for a final default case) emit an exact table. Otherwise, a ternary table. *)      
    let patses = List.map fst branches in
    let last_pats = List.rev patses |> List.hd in
    let fst_patses = List.rev patses |> List.tl in
    let is_exact = 
      (List.for_all 
        (fun pats -> 
          match pats with
          | [TofinoCore.PNum _] -> true
          | _ -> false)
        fst_patses)
      &&
      (match last_pats with 
      | [TofinoCore.PWild] -> true | _ -> false)
    in
    match is_exact with
    | true -> (
      let keys = List.map exp_to_exact_key keys in 
      let actions = List.map action_cid_of_branch branches 
        |> MiscUtils.unique_list_of 
        |> List.map evar_noretmethod
      in 
      let rec branches_to_rules branches = 
        match branches with
        | [] -> [], None
        (* expect the last branch to be a wildcard *)
        | [([TofinoCore.PWild], call_stmt)] -> (
          [], Some(translate_sunit_ecall call_stmt)
        )
        | _::[] -> (error "[stmt_to_table] invalid final branch in table")
        | (pats, call_stmt)::branches -> (
          let pats' = List.map translate_pat pats in
          let call_stmt' = translate_sunit_ecall call_stmt in
          let rules, default = branches_to_rules branches in
          (pats', call_stmt')::rules, default
        )
      in
      let rules, default_opt = branches_to_rules branches in 
      let tbl_dec = 
        dtable tid keys actions rules default_opt None ignore_parallel_tbls_pragmas
      in
      let tbl_call = sunit (ecall_table tid) in 
      (tbl_dec, tbl_call)
    )
    | false -> 
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
  )
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
(* let generate_added_var_decls tds = List.map 
  (fun (id, ty) -> dvar_uninit id (translate_ty ty))
  (main_handler_of_decls tds).hdl_preallocated_vars.
;; *)

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


let tyid_of_fieldid struct_ty fieldid = 
  let hdrty_id evid = ((fst evid)^"_h", snd evid) in
  let structty_id evid = ((fst evid)^"_s", snd evid) in
  match struct_ty with 
  | THdr -> hdrty_id fieldid
  | TMeta -> structty_id fieldid
;;


let tyid_of_event struct_ty event = 
  let evid = id_of_event event in 
  match (event) with 
    | EventSingle(_) -> tyid_of_fieldid struct_ty evid
    | (_) -> tyid_of_fieldid TMeta evid
;;

(* generate the struct type for an event. 
   output events are headers, input events are metadata *)
(* we need to avoid creating multiple copies of the same event... 
   and, we need to create different structs for single events depending on 
   whether they are encoded as headers or structs *)
   
(* update previously translated while recursing on inner *)
let rec translate_members struct_ty prevs members = 
  let (prevs, new_member_decls) = List.fold_left
    (fun (prevs, new_ev_decls) event -> 
      let prevs', new_decls, outer_decl_opt = translate_event prevs struct_ty event in
      match outer_decl_opt with 
      | Some(outer_decl) -> 
        prevs', new_ev_decls@new_decls@[outer_decl]
      | None -> prevs, new_ev_decls@new_decls)
    (prevs, [])
    members
  in
  prevs, new_member_decls


(* given an event's header, generate a p4 struct or header type *)
and translate_header struct_ty (prevs : id list) header =  
  let in_prevs = List.exists (Id.equal header.header_id) prevs in
  if (not in_prevs)
    then (
      let fields = match header.header_ty.raw_ty with
        | TRecord(fields) -> (List.map (fun (x, y) -> x, CoreSyntax.ty y |> translate_ty ) fields) 
        | _ -> error "expected a TRecord for a header struct"
      in      
      let p4struct = dstruct header.header_tyid struct_ty fields in 
    
      (header.header_id::prevs, [p4struct])
    )
  else (prevs, [])
and translate_headers prevs hdrs struct_ty = 
  (* print_endline ("[translate_headers] start in "^(fst evid)); *)
  let res = List.fold_left 
    (fun (prevs, p4hdrs) hdr -> 
      let prevs', p4hdrs' = translate_header struct_ty prevs hdr in
      (* print_endline ("translating header"^(Id.to_string hdr.header_id)); *)
      prevs', (p4hdrs@p4hdrs'))
    (prevs, [])
    hdrs
  in
  (* print_endline ("[translate_headers] stop in "^(fst evid)); *)
  res

(* translate a parameter, whose type may be an anonymous record *)
and translate_param_with_anon_recty struct_kind (id, ty) = 
  match ty.raw_ty with
  | TRecord(fields) -> (
    (* first, we need a name for this type. *)
    let ty_id = Id.fresh_name ((fst id)^"_t") in
    (* now we can build the struct *)
    let struct_decl = dstruct
      ty_id
      struct_kind
      (List.map (fun (id, rty) -> id, translate_ty (CoreSyntax.ty rty)) fields)
    in
    let ref_ty = translate_ty (CoreSyntax.ty (CoreSyntax.TName(Cid.id ty_id, [], false))) in
    (* give back the translated param and the struct declaration *)
    (id, ref_ty), [struct_decl]
  )
  (* for everything else, just translate the type directly *)
  | _ -> (id, translate_ty ty), []




and translate_event (prevs:id list) struct_ty event : id list * T.decl list * (T.decl option) =
  let this_event_struct_id = tyid_of_event struct_ty event in 
  (* check if this event's struct declarations have already been generated *)
  let in_prevs = List.exists (fun prev_evid -> Id.equal this_event_struct_id prev_evid) prevs in
  let (ret:id list * T.decl list * T.decl option ) = if (in_prevs) 
    then (prevs, [], None)
    else (
    match event with
    | EventSingle({evparams;}) -> 
      (* declare as header or struct depending on encoding type *)
      let decl = dstruct (tyid_of_event struct_ty event) struct_ty 
        (List.map (fun (pid, pty) -> pid, translate_ty pty) evparams)
      in
      prevs@[this_event_struct_id], [], Some(decl)
    (* special case: the event union's members are all unions. This happens for 
    the egress's output event. In this case, we don't give this outer event a tag *)
    | EventUnion({hdrs; members;}) when (is_union_of_unions event) -> 

    (* translate the event's headers and make fields *)
    let prevs, header_decls = translate_headers prevs hdrs struct_ty in
    let hdr_fields = 
      (List.map (fun hdr -> (hdr.header_id, hdr.header_tyid |> tstruct)) hdrs)
    in
    (* translate the members headers and make fields *)
    let member_to_field_param event = 
      id_of_event event, tyid_of_event struct_ty event
    in
    let field_params = List.map member_to_field_param members in
    let (member_fields: (id * ty) list) = 
      List.map
        (fun (field_id,field_ty_id) -> 
          (field_id, tstruct field_ty_id))
        field_params
    in
    let union_decl = dstruct 
      (tyid_of_event struct_ty event) 
      TMeta
      (hdr_fields@member_fields)
    in
    (* translate the members *)
    let prevs, member_decls = translate_members struct_ty prevs members in 
    prevs@[this_event_struct_id], (header_decls@member_decls), Some(union_decl)

    | EventUnion({members; tag;}) -> 
    (* a union is a struct with fields: 
        tag : evid_tag_t; 
        ev : ev_t; for ev in members
        in addition to the union type, return the struct for tag and evs*)
      (* type of the tag header: outer_tag *)
      let tag_outer, (tag_inner, tag_ty) = tag in 
      let tag_outer_ty_id = tyid_of_fieldid struct_ty tag_outer in
      let tagty_decl = dstruct tag_outer_ty_id struct_ty
        [tag_inner, translate_ty tag_ty]
      in
      let prevs, member_decls = translate_members struct_ty prevs members in 
      (* fields of the union type are all other structs. *)
      (* tricky: are those fields header types or struct types? 
        if the members are events, then it depends on struct_ty. 
        if the members are event containers, then it is always TMeta *)
      let member_to_field_param event = 
        id_of_event event, tyid_of_event struct_ty event
      in
      let field_params =
        [tag_outer, tag_outer_ty_id] (* first field is the tag header (todo: put into headers) *)
        @List.map member_to_field_param members (* last fields are the members *)
      in
      let (union_fields: (id * ty) list) = 
        List.map
          (fun (field_id,field_ty_id) -> 
            (field_id, tstruct field_ty_id))
          field_params
      in
      let union_decl = dstruct 
        (tyid_of_event struct_ty event) 
        TMeta
        union_fields
      in
      prevs@[this_event_struct_id], ([tagty_decl]@member_decls), Some(union_decl)
      | EventSet({members; flags;}) -> 
        let flag_struct_id, flag_fields, flag_pad = flags in

        (* flags header with padding *)
        let flag_outer_ty_id = tyid_of_fieldid struct_ty flag_struct_id in
        let flag_pad_field = match flag_pad with
          | None -> []
          | Some(id, ty) -> [id, ty]
        in
        let flag_field_decls = List.map 
          (fun (pid, pty) -> pid, translate_ty pty) 
          (flag_fields@flag_pad_field) 
        in
        let flagty_decl = dstruct flag_outer_ty_id struct_ty flag_field_decls in 
        (* fields of the set type *)
        let prevs, member_decls = translate_members struct_ty prevs members in
        let field_params = 
          (flag_struct_id, flag_outer_ty_id) (* first field is the flags header *)
          ::(List.map (fun event -> (id_of_event event, tyid_of_event struct_ty event)) members)
        in
        let (union_fields: (id * ty) list) = List.map
          (fun (field_id,field_ty_id) -> 
            (field_id, tstruct field_ty_id))
          field_params
        in
        let set_decl = dstruct 
          (tyid_of_event struct_ty event) 
          TMeta
          union_fields
        in
        prevs@[this_event_struct_id], flagty_decl::member_decls, Some(set_decl)
      | EventWithMetaParams({event; params;}) -> (
        (* translate the inner event *)
        let prevs, event_decls, event_decl_opt = translate_event prevs struct_ty event in
        (* if any of the parameters have anonymous record types, generate named record 
           types for them now. *)
        let translated_params = List.map (translate_param_with_anon_recty struct_ty) params in
        let translated_params, param_anon_hdr_decls = List.split translated_params in
        let param_anon_hdr_decls = List.flatten param_anon_hdr_decls in 
        (* add the params to the declaration for the outer event (event_decl_opt) *)
        let event_decl_opt = match event_decl_opt with
          | None -> None
          | Some(in_event) -> 
          begin 
            let in_event = match in_event.d with 
              | DStructTy{id; sty; fields;} ->
              begin
                {in_event with 
                  d = DStructTy({id; sty; fields = fields@translated_params;})}
              end
              | _ -> in_event
            in
            Some(in_event)
          end
        in
        (* the declarations returned are all the new inner event declarations, 
           plus any declarations generated for the anonymous types in 
           in the parameters (which, at the time of writing, was just checksum arguments)*)
        prevs, event_decls@param_anon_hdr_decls, event_decl_opt
        
        
      )
    )
    in
    ret
;;

(* let rec qualified_params_of_event (prefix : Id.t list) event = 
  match event with 
  | EventSingle({evid; evparams;}) -> (
    List.map (fun (id, _) -> Cid.create_ids (prefix@[evid; id])) evparams
  )
  | EventUnion({evid; members;})
  | EventSet({evid; members;}) -> (
      let prefix = prefix@[evid] in
    List.map (qualified_params_of_event prefix) members
    |> List.flatten
  )
;; *)
(* let param_pragma pname gress_string param_cid = 
  let pragma = {
    pname;
    pargs = ["\""^gress_string^"\""; "\""^CorePrinting.cid_to_string param_cid^"\""];
    }
  in
  {d=DPragma(pragma); dpragma=[]; dspan = Span.default}
;; *)
(* 
let no_overlay_pragma = param_pragma "pa_no_overlay" ;;
let solitary_pragma = param_pragma "solitary" ;; *)

(* let rec pragmas_of_event gress event =   
  List.map (no_overlay_pragma gress) (qualified_params_of_event [] event)

;; *)

let translate_params (comp : CS.component) (main_hdl:hevent) =
  (*  construct parameters for the main control flow of a 
      component. 
control IngressControl(
    inout hdr_t hdr,   // derived from "ingress_output"
    inout meta_t meta, // derived from "ingress_input"
    // in params
    in ingress_intrinsic_metadata_t ig_intr_md,
    in ingress_intrinsic_metadata_from_parser_t ig_prsr_md,
    // out params
    inout ingress_intrinsic_metadata_for_deparser_t ig_dprsr_md,
    inout ingress_intrinsic_metadata_for_tm_t ig_tm_md)
    {
  *)
  let ein = CS.get_event comp main_hdl.hdl_input in
  let eout = CS.get_event comp main_hdl.hdl_output in
  let event_params = [
    inoutparam (tstruct (tyid_of_event TMeta eout)) (id_of_event eout);
    inoutparam (tstruct (tyid_of_event TMeta ein)) (id_of_event ein);
  ] in
  let in_params = List.map 
    (fun (i, t) -> inparam (translate_ty t) i) 
    main_hdl.hdl_params
  in
  let out_params = List.map 
    (fun (i, t) -> inoutparam (translate_ty t) i) 
    main_hdl.hdl_retparams
  in
  event_params@in_params@out_params  
;;

(*** declaration translation, starting with an outer context / env 
    that includes an inner "program" environment (for statements) ***)
type translate_decl_env = {
  component : CS.component;
  penv : prog_env;
  (* some declarations end up going outside of the control block, 
     others end up going inside the control block...  *)
  globals : T.decl list; (* global wrt main handler *)
  locals  : T.decl list; (* local wrt main handler *)
  prev_events : id list; (* ids of events that have already been translated *)
  extern_tdecls : tdecls; 
  (*tofinocore declarations that are deleted because they are externs, 
    but we need to keep track of them for field names and types. *)
}

(* context helpers *)
let new_denv component penv = 
  {component; penv; globals = []; locals = []; prev_events = []; extern_tdecls = [];}
;;
(* _d_env  _w_rapper -- apply a function that expects a 
   penv to a denv -- usage: dw bind_memop penv ... *)
let dw fcn denv arg = 
  {denv with penv = (fcn denv.penv arg); }


(* find the id of the extern type that is declared to contain 
   the field with the given field_id *)
let find_extern_tys_with_field denv field_id = 
  let externs = denv.extern_tdecls in
  let matching_extern_ty_ids = List.filter_map
    (fun tdecl -> 
      (* print_endline ("find_extern_ty_with_field checking extern: "^(TofinoCorePrinting.tdecl_to_string tdecl)); *)
      
      match tdecl.td with 
      | TDExtern(ty_id, ty) -> (
        match ty.raw_ty with 
        | TRecord(fields) -> (
          if List.exists (fun (fid, _) -> (fst fid) = (fst field_id)) fields 
            then Some(ty_id) 
          else None
        )
        | _ -> None)
      | _ -> None)
    externs
  in
  matching_extern_ty_ids
;;


(*** parsing translators ***)

let last_block_id = ref 0
let fresh_blockid () = 
  last_block_id := (!last_block_id) +1;
  ("block_"^(string_of_int (!last_block_id)), 0)
;;
(* some helpers for constructing the parser *)
let transition id = sunit (ejump (Cid.create_ids [id]))
let transition_accept = transition (id "accept")
let dparsestate id stmt =
  decl (DParseState {id; body = stmt})
;;

(* let pkt_arg = id "pkt" *)
let extract pkt_arg field_inst = 
  let extract_fcn = 
    Cid.create_ids [pkt_arg; id"extract"]
  in
  sunit (ecall extract_fcn [T.evar field_inst])
;;
let sadvance pkt_arg nbits =
  let advance_fcn =
    Cid.create_ids [pkt_arg; id"advance"] 
  in
  sunit (ecall advance_fcn [T.eval_int nbits])
;;
let lookahead pkt_arg ty = 
  (* notice its a tycall *)
  etycall (Cid.create_ids [pkt_arg; id"lookahead"]) [ty] []
;;
let slocal_lookahead pkt_arg cid ty =
  slocal (Cid.to_id cid) ty (lookahead pkt_arg ty)
;;
let sassign_lookahead pkt_arg cid ty = 
  sassign cid (lookahead pkt_arg ty)
;;


(* construct parameters for the parser that feeds given handler *)
let translate_parser_params (comp : component) (parser:parser) = 
(* P4 params: 
  TODO: the packet in parameter is now in the parser's parameter list. Use it. 
        - need to carry the packet peek commands first, though.
  directionless packet_in pkt *implicit*
  out hdr_t hdr               *_handler's_ out event...*
  out meta_t meta             *parser's out event*
  out ingress_intrinsic_metadata_t ig_intr_md   *parser's out param*
*)
  let pkt_arg, pkt_t = match parser.pparams with 
    | (pkt_arg, {raw_ty=TBits(_)})::_ ->
        pkt_arg, tstruct (id"packet_in")
    | _ -> 
      print_endline ("pkt_args: "^(CorePrinting.params_to_string parser.pparams));
      error "[translate_parser_params] expected a packet_in parameter"
  in
  (* let pkt_t, pkt_arg = id"packet_in", id"pkt" in  *)
  let pkt_param = param pkt_t pkt_arg in
  let parser_out_event = get_event comp (parser.pret_event |> Option.get) in
  let handler_out_event = get_event comp (parser.phdlret_event |> Option.get) in
  let hdr_param = outparam 
    (tstruct (tyid_of_event TMeta handler_out_event)) 
    (id_of_event handler_out_event) 
  in 
  let meta_param = outparam
    (tstruct (tyid_of_event TMeta parser_out_event))
    (id_of_event parser_out_event)
  in
  let out_params = List.map 
    (fun (i, t) -> outparam (translate_ty t) i) 
    parser.pret_params
  in
  pkt_arg, [pkt_param; hdr_param; meta_param]@out_params
;;

(* translate a checksum expression that appears in a parser. 
   These have to be converted to a sequence of "subtract" 
   checksum operations, each followed by an invalidate operation 
   to make sure the header doesn't propagate to egress, and finally 
   at the end a checksum.get that replaces the exp *)
let translate_parser_checksum penv exp = 
  let args = match exp.e with 
    | EHash(_, _::args) -> args
    | _ -> error "[translate_parser_checksum] expected hash exp"
  in
  let checksum_obj_id = fresh_numbered_id "checksum" in
  let checksum_obj = dobj (id "Checksum") [] [] checksum_obj_id (Span.default) in
  let arg_exprs = fst_snd (translate_exps penv args) in
  let arg_cids = List.map (CoreSyntax.exp_to_cid) args in
  (* let arglist = elist (snd (translate_exps penv args)) in  *)
  (* now... one subtract and invalidate for each arg *)
  let pre_stmts, post_stmts = List.fold_left2
    (fun (pre_stmts, post_stmts) arg_expr arg_cid -> 
      let arglist = elist [arg_expr] in
      let sub_call = scall (Cid.create_ids [checksum_obj_id; id "subtract"]) [arglist] in
      let arg_hdr_cid = Cid.to_ids arg_cid |> List.rev |> List.tl |> List.rev |> Cid.create_ids  in 
      let invalidate_call = 
        scall
        (Cid.concat arg_hdr_cid (Cid.create ["setInvalid"]))
        []
      in

      (pre_stmts@[sub_call], post_stmts@[invalidate_call]))
    ([], [])
    arg_exprs
    arg_cids
  in
  let pre_stmt = sseq pre_stmts in
  let post_stmt = sseq post_stmts in 
  (* finally, a get call for the expression *)
  let get_call = ecall
    (Cid.create_ids [checksum_obj_id; id "get"])
    []
  in
  [checksum_obj], pre_stmt, get_call, post_stmt
;;

let is_checksum exp = match exp.e with 
  | EHash(_, poly::_) -> (
    match poly.e with 
    | EVar(cid) -> Cid.equal cid (Cid.id Builtins.checksum_id)
    | _ -> false
  )
  | _ -> false
;;

let translate_parser denv parser = 
  let parser_out_event = get_event denv.component (parser.pret_event |> Option.get) in
  let handler_out_event = get_event denv.component (parser.phdlret_event |> Option.get) in
  (* vars declared in the handler's output as headers (checksum args) *)
  let handler_out_cids = match handler_out_event with
    | EventWithMetaParams{params} -> 
      List.map (fun param -> 
        let param_cid, _ = ParserHoisting.recwrap_extract param in
        (* if recwrap_extract returned, it is a header *)
        let hdr_field_only_cid = 
          Cid.to_ids param_cid |> List.rev |> List.tl |> List.rev |> Cid.create_ids 
        in 
        Cid.compound 
          (id_of_event handler_out_event)
          hdr_field_only_cid
        ) 
        params
    | _ -> []
  in

  (* header parameters are all the intrinsics in pret_params, plus all the 
     params in handler_out_event *)
  let param_cids = 
    (List.map (fun (id, _) -> Cid.id id) parser.pret_params)
    @handler_out_cids
  in

  let pkt_arg, params = translate_parser_params denv.component parser in 
  let is_param cid : bool = 
    List.exists (Cid.equal cid) param_cids
  in
  let is_event_field cid : bool = 
    let fst_id = Cid.to_ids cid |> List.hd in
    let out_ev_id = parser_out_event |> id_of_event in
    Id.equal fst_id out_ev_id
  in
  let translated_parser_blocks = ref [] in
  let compute_decls = ref [] in
  let parse_states = ref [] in
  let start_block = ref true in 

  let rec translate_parser_block parser_block :id =
    (* first, check if there is an already-translated 
       parser block that is equivalant. To test equivalence
       of two parser blocks b1, b2, we just check if the 
       string representations are equal using
       CorePrinting.parser_block_to_string *)
    let block_str = CorePrinting.parser_block_to_string parser_block in
    let cached_block_id_opt = 
        List.find_map (fun (id, str) -> 
          if (String.equal str block_str) 
            then Some(id) 
            else None) 
        (!translated_parser_blocks)
    in
    let am_start = !start_block in
    start_block := false;
    (* a parse block is basically an anonymous parse state, 
       with actions and steps both translating into statements. *)

    (* translate the actions *)
    let decls_stmts = List.map fst parser_block.pactions |> 
      List.map translate_parse_action
    in
    let decls, stmts = List.split decls_stmts in 
    let decls = List.flatten decls in 

    let sstep = translate_parser_step (fst parser_block.pstep) in 
    let block_id = if (am_start)
      then (id "start")
      else (
        match cached_block_id_opt with 
        | Some(id) -> id
        | None -> fresh_blockid ()
      ) 
    in  
    (* add the block to the cache, and the parser states list if 
       it gets added to the cache *)
    (match (cached_block_id_opt) with
    | Some(_) -> ()
    | None -> 
      (* construct new parser state and update mutables *)
      let new_parser_state = dparsestate
        block_id
        (sseq (stmts@[sstep]))
      in
      compute_decls := (!compute_decls)@decls;
      translated_parser_blocks := (!translated_parser_blocks)@[(block_id, block_str)];
      parse_states := (!parse_states)@[new_parser_state];  
    );
    
    block_id

  and translate_parse_action parser_action : (T.decl list * T.statement) = 
    match parser_action with 
    | PRead(cid, ty, _) -> (
      (* case -- cid is param -> read is an extract
         case -- cid is not param -> read is a slocal lookahead; advance *)
      match (is_param cid) with
      | true -> [], extract pkt_arg cid
      | false -> [], sseq [slocal_lookahead pkt_arg cid (translate_ty ty); sadvance pkt_arg (size_of_tint ty)]
    )
    | PPeek(cid, ty, _) -> (
      (* case -- cid is an event field (first id is name of output event param) sassign lookahead -> 
         case -- cis is not a param -> slocal lookahead *)
      match (is_event_field cid) with 
      | true -> [], sassign_lookahead pkt_arg cid (translate_ty ty)
      | false -> [], slocal_lookahead pkt_arg cid (translate_ty ty)
    )
    | PSkip(ty) -> [], sadvance pkt_arg (size_of_tint ty)
    | PAssign(cid, exp) when (is_checksum exp) -> 
      (* invalidate statements come after the use of the headers *)
      let decls, sub_stmt, get_expr, invalid_stmt = translate_parser_checksum denv.penv exp in
      decls, (sseq [sub_stmt;(sassign cid get_expr); invalid_stmt])
    | PLocal(cid, ty, exp) when (is_checksum exp) ->
      let decls, sub_stmt, get_expr, invalid_stmt = translate_parser_checksum denv.penv exp in
      decls, (sseq [sub_stmt;(slocal (Cid.to_id cid) (translate_ty ty) get_expr); invalid_stmt])
    | PAssign(cid, exp) ->         
      let (decls, expr), _ = translate_exp denv.penv exp in
      decls, sassign cid expr
    | PLocal(cid, ty, exp) ->
      let (decls, expr), _ = translate_exp denv.penv exp in
      decls, slocal (Cid.to_id cid) (translate_ty ty) expr
  
  and translate_parser_branch (pats, block) = 
    let pats' = List.map translate_pat pats in
    let block_id = translate_parser_block block in
    pats', transition block_id

  and translate_parser_step parser_step = 
    match parser_step with
    | PMatch(exps, parser_branches) -> (
      let exps' = translate_exps denv.penv exps |> fst_snd in
      let branches' = List.map translate_parser_branch parser_branches in
      smatch exps' branches'
    )
    | PGen _ -> error "[translate_parser_step] pgens should have been elminiateasd"
    | PCall({e=CS.ECall(fcid, _, _)}) -> 
      if ((Cid.names fcid |> List.hd) = "exit")
        then transition_accept
        else error "[translate_parser_step] the only supported call is to exit/accept"
    | PCall _ -> error "[translate_parser_step] the only supported call is to exit/accept"
    | PDrop -> transition (id "drop")
  in
  (* back in outer translate_parser *)
  (* print_endline ("------ translating parser ------"); *)
  (* TofinoCorePrinting.parser_to_string parser |> print_endline; *)
  (* print_endline ("------------"); *)
  let _ = translate_parser_block parser.pblock in
  let parse_states = !parse_states in
  let compute_decls = !compute_decls in 
  let parser_id = (fst denv.component.comp_id)^"_"^(fst parser.pid) |> Id.create in 
  let res = dparse parser_id params (compute_decls@parse_states) in
  (* let out_str = P4TofinoPrinting.string_of_decl res |> P4TofinoPrinting.doc_to_string in *)
  (* print_endline ("-----------p4 parser-------");
  print_endline out_str; *)
  res 
;;

(* construct a deparser for a handler *)
let construct_deparser denv hevent : decl = 
  let deparser_id = (fst denv.component.comp_id)^"_deparser" |> Id.create in
  (* P4 params: 

  directionless packet_out pkt *implicit*
  inout hdr_t hdr               *handlers out event*
  in meta_t meta             *handlers _in_ event... (is this really necessary in P4?)*
  in intrinsic_metadata_for_deparser_t ... *first out parameter of handler*
*)
  let pkt_t, pkt_arg = id"packet_out", id"pkt" in 
  (* the header holds the output event *)
  let hdl_output = CS.get_event denv.component hevent.hdl_output in
  let hdl_input = CS.get_event denv.component hevent.hdl_input in
  let hdr_param_id = id_of_event hdl_output in
  let hdr_param_ty = (tstruct (tyid_of_event TMeta hdl_output)) in
  let params = 
    let pkt_param = param (tstruct pkt_t) pkt_arg in
    let hdr_param = inoutparam 
      hdr_param_ty 
      (hdr_param_id) 
    in 
    let meta_param = inparam
      (tstruct (tyid_of_event TMeta hdl_input))
      (id_of_event (hdl_input))
    in
    (* the deparser metadata parameter's type name may vary, but its always 
      the one with the "drop_ctl" field. *)
    let possible_intrinsic_param_ty_ids = find_extern_tys_with_field denv (id"drop_ctl") in
    let possible_intrinsic_param_ty_names = List.map fst possible_intrinsic_param_ty_ids in
    (* go through the list of out parameters in the handler (hevent.hdl_outparams), find the out parameter 
      that is of type TName(cid, _, _) where (hd (Cid.names cid)) = fst intrinsic_param_ty_id *)
    let intrinsic_param = List.find_map (fun (id, ty) -> 
      match ty.raw_ty with 
      | TName(tcid, _, _) -> (
        match (Cid.names tcid) with 
        | [name] -> (
          let (matching_ty_names : string list) = List.filter (String.equal name) possible_intrinsic_param_ty_names in
          (* if we found exactly 1 matching name, then this is the intrinsic parameter, else error *)
          if (List.length matching_ty_names) = 1 
            then Some(id, ty) 
          else (error "[construct_deparser] could not find the intrinsic metadata parameter")          
        )
          (* if (name = (fst intrinsic_param_ty_id)) then Some(id, ty) else None *)
        | _ -> None
      )
      | _ -> None
      ) hevent.hdl_retparams
    in
    let intrinsic_param = match intrinsic_param with 
      | Some(ip) -> ip
      | None -> error "[construct_deparser] could not find the intrinsic metadata parameter"
    in
    let intrinsic_param = inparam (translate_ty (snd intrinsic_param)) (fst intrinsic_param) in
    [pkt_param; hdr_param; meta_param; intrinsic_param]
  in
  (* finally, translate the body of the deparser *)
  let (decls, stmt), _ = translate_statement denv.penv [] hevent.hdl_deparse in
  (* the emit call is the final statement. *)
  let emit_call = sunit (T.ecall (Cid.create_ids [pkt_arg; id"emit"]) [T.evar (Cid.id hdr_param_id)]) in

  decl (DDeparse{
    id=deparser_id;
    params=params;
    decls=decls;
    body=Some(P4TofinoSyntax.sseq (stmt::emit_call::[]));
  })
;;

(* let translate_pragma (tpragma : Pragma.t) : pragma option = 
  match tpragma with
  | POverlay(cida, cidb) ->  *)

let translate_tdecl (denv : translate_decl_env) tdecl : (translate_decl_env) = 
  match tdecl.td with
  (* memops just go into the context *)
  | TDMemop(m) -> (dw bind_memop denv (m.mid, m))
  (* tables just go into context *)
  | TDGlobal(tbl_id, _, {e=ETableCreate(tbl_def); espan=espan;}) -> 
    dw bind_tbl denv ((Cid.id tbl_id), (tbl_def, espan))
  (* arrays get translated into registerarrays, which are global. *)
  | TDGlobal(rid, ty, exp) -> (
    match ty.raw_ty with 
    | TName(tcid, [cell_size], true) -> (
        let module_name = List.hd (Cid.names tcid) in 
        let len, default_opt = match exp.e with 
          | ECall(_, args, _) -> (
            match (translate_exps denv.penv args |> fst_snd) with 
            | [elen; edefault] -> elen, Some(edefault)
            | [elen] -> elen, None
            | _ -> error "[generate_reg_array] expected 1 or 2 arguments in global constructor"
          )
          | _ -> error "[generate_reg_array] right hand side of global mutable declaration must be a constructor call";
        in
        match (module_name) with 
        | "Array" -> 
          (* take the span from the expression, 
             because the constructor expression tracks source ids *)
          let reg_decl = 
            dreg_sp 
              rid (tint cell_size) [cell_size] TAuto len default_opt [] exp.espan
          in
          let globals = denv.globals @ [reg_decl] in
          {denv with globals}
        | "PairArray" -> 
          (* pair arrays get their own cell type struct *)
          let cell_struct = cell_struct_of_array_ty module_name cell_size rid in 
          (* id, cell ty (struct); idx ty; len; default (None) *)
          let reg_decl = 
            dreg_sp 
              rid (cell_ty_of_array rid) [cell_size;cell_size] TAuto len default_opt [] exp.espan
          in
          let globals = denv.globals @ [cell_struct; reg_decl] in
          {denv with globals}
        | _ -> denv)
    | _ -> error "[translate_array_decl] wrong type"
  )
  (* open functions get translated into actions, and their names go into the context *)
  | TDOpenFunction(id, const_params, stmt) ->
    let params = List.map
      (fun (id, ty) -> (None, translate_ty ty, id))
      const_params
    in
    let (body_decls, body), penv = translate_statement denv.penv [] stmt in
    let action = daction id params body in
    (* add the open function to the actions context, 
       the updated penv from translate_statement should also have the 
       list of hashers that have been generates so far in the program *)
    let penv = {penv with actions = CidSet.add (Cid.id id) penv.actions} in
    let locals = denv.locals@body_decls@[action] in
    {denv with penv; locals}
  | TDEvent(_) -> denv
    (* we do nothing with events, because we don't know whether to encode 
       them as a header or as a struct without knowing whether they are 
      input or output parameters to the handler. Instead, we translate the 
      events used in handler declarations. *)
  | TDExtern _ -> {denv with extern_tdecls = denv.extern_tdecls @ [tdecl]} 
    (* externs are declared elsewhere, but we keep track of them for identifying
       parameters based on their extern types *)
  | TDHandler(HEvent(hevent)) -> (
      (* note: we assume there's only 1 handler per list of declarations! *)
      (* make some decls for the variables created after handler merging. *)
      let added_var_decls = (List.map 
        (fun (id, ty) -> dvar_uninit id (translate_ty ty))
        hevent.hdl_preallocated_vars)
      in
      (* translate the body, which makes some more local decls for tables *)
      let main_body = match hevent.hdl_body with 
        | SPipeline(stmts) -> stmts
        | _ -> error "shoulda been pipeliend by now"
      in      
      let table_decls, table_calls = statements_to_stages 0 denv.penv hevent.hdl_id main_body |> List.split in 
      let apply_body = sseq table_calls in 
      (* put together the control decl *)
      let control_decl = decl (DControl{
        id = hevent.hdl_id;
        params = translate_params denv.component hevent;
        (* local_decls are: variables; actions; tables *)
        decls = added_var_decls@denv.locals@table_decls;
        body = Some(apply_body);
        })
      in
      let hdl_input = CS.get_event denv.component hevent.hdl_input in
      let hdl_output = CS.get_event denv.component hevent.hdl_output in
      (* translate the input and output events *)
      let prev_events, in_event_decls, main_in_opt = translate_event denv.prev_events TMeta hdl_input in
      let prev_events, out_event_decls, main_out_opt = translate_event prev_events THdr hdl_output in
      let in_event_decls = match main_in_opt with
        | Some(in_event) -> in_event_decls@[in_event]
        | None -> in_event_decls
      in
      let out_event_decls = match main_out_opt with
        | Some(out_event) -> out_event_decls@[out_event]
        | None -> out_event_decls
      in
      let solitary_input_tag_pragma = match (hdl_input) with
        | EventUnion({tag;}) 
        | EventWithMetaParams({event=EventUnion({tag;})}) -> (
          let tag_cid = Cid.create_ids [id_of_event hdl_input; fst tag; fst (snd tag)] in 
          let gress_string = match hevent.hdl_sort with 
            | HData -> "ingress"
            | HEgress -> "egress"
            | HControl -> error "trying to translate a control component to p4?"
          in
          let pragma = {
            pname = "pa_solitary";
            pargs = ["\""^gress_string^"\""; "\""^CorePrinting.cid_to_string tag_cid^"\""];
          }
          in
          {d=DPragma(pragma); dpragma = []; dspan = Span.default}
        )
        | _ -> error "the input event to a handler should always be a unions..."
      in 
      (* another hack: in the egress pipeline, make sure no event parameter variables get put into the same containers, 
         either by slicing or overlay.*)
      (* let overlay_pragmas = match hevent.hdl_sort with
      | HEgress -> pragmas_of_event "egress" hevent.hdl_input
      | _ -> []
      in  *)
      (* if there are any overlay pragmas attached to the handler's decl, translate them *)
      let overlay_pragmas = [] in 
      let globals =
        denv.globals@in_event_decls@out_event_decls@overlay_pragmas@[solitary_input_tag_pragma; control_decl]
      in
      (* consume all the locals! *)
      let locals = [] in
      (* finally, construct the deparser that goes along with this control block. 
         The deparser simply emits the packet, but we have to include it in the p4 anyway, 
         and the deparser's parameters are a subset of the control's parameters. *)
      (* update globals and previously generated events *)
      let deparser = construct_deparser denv hevent in 
      let globals = globals@[deparser] in
      {denv with globals; locals; prev_events}
  )
  | TDHandler(HParams(_)) -> error "param-based handles should have been eliminated"
  | TDParser(p) -> let globals = denv.globals@[translate_parser denv p] in {denv with globals}
  | TDAction _ -> error "[translate_tdecl] actions should have been converted into functions"
  | TDMulticastGroup(group) -> 
    let decl = T.decl_full (DMCGroup{gid=group.gnum; replicas =group.gcopies;}) [] tdecl.tdspan in 
    {denv with globals = denv.globals@[decl]}
  | TDFun(_) -> error "[translate_tdecl] function compilation is not implemented for tofino backend"
;;

(* move parsers to the end of the decls *)
let rec sort_globals parsers decls = 
  match decls with 
    | [] -> parsers
    | decl::decls -> (
      match decl.d with 
      | DParse _ -> sort_globals (parsers@[decl]) decls
      | _ -> decl::(sort_globals parsers decls)
    )
;;

let build_gress_ctx comp = 
  match (comp.comp_sort) with 
  | HData -> 
    let full_ingr_port_cid = Cid.create ["ingress_intrinsic_metadata"; "ingress_port"] in
    let ingr_env = bind_var empty_env
       ((Builtins.ingr_port_id |> Cid.id), CoreSyntax.var_sp full_ingr_port_cid (SyntaxToCore.translate_ty Builtins.tofino_builtin_tys.ingr_port_ty) Span.default)
    in
    let denv = new_denv comp ingr_env in
    denv
  | HEgress -> 
    let denv = new_denv comp empty_env in
    denv
  | HControl -> 
    let denv = new_denv comp empty_env in
    denv
;;


(* we can represent a p4 program as a map of components. 
   Eventually, this will be the output of this pass, 
   because it is simlper and more flexible. For now, 
   we convert into a "tofino_prog", which has 
   hard-coded fields for ingress and egress pipes and control 
   commands. *)
type p4_prog = (decl list) IdMap.t

let p4str_of_decls decls = 
  P4TofinoPrinting.string_of_decls decls
  |> P4TofinoPrinting.doc_to_string
;;
let p4_prog_to_string (p4_prog:p4_prog) = 
  IdMap.bindings p4_prog
  |> List.map (fun (id, decls) -> 
    Printf.sprintf "component %s {\n%s\n}\n" (fst id) (p4str_of_decls decls)
  )
  |> String.concat "\n"
;;

(* convert a tofinocore program into a 
   dictionary of p4 program components *)
let prog_to_p4_prog (prog : prog) : p4_prog = 
  List.fold_left
    (fun p4_prog component -> 
      (* the contexts for ingress and egress are slightly different *)
      let denv = build_gress_ctx component in
      let penv = List.fold_left translate_tdecl denv component.comp_decls in
      let decls = sort_globals [] penv.globals in
      let includes = [dinclude "<core.p4>"; dinclude "<tna.p4>"] in 
      let decls = includes@decls in
      IdMap.add component.comp_id decls p4_prog)
    IdMap.empty
    prog
;;


(* convert the declarations of a component into a pipe. 
   basically, extact the parser, control, and deparser, 
   put them into a pipe, and return the rest of the decls 
   as the globals *)
   let component_decls_to_pipe decls : (decl list * pipe) = 
    let (globals, parser, control, deparser) = List.fold_left 
      (fun (gs, p, c, d) decl -> 
        match decl.d with
        | DParse _ -> (gs, Some(decl), c, d)
        | DControl _ -> (gs, p, Some(decl), d)
        | DDeparse _ -> (gs, p, c, Some(decl))
        | _ -> (gs@[decl], p, c, d))
      ([], None, None, None) 
      decls
    in
    match (parser, control, deparser) with
      | (Some(p), Some(c), Some(d)) -> (globals, {parse=p; process=c; deparse=d;})
      | _ -> error "component_decls_to_pipe: missing parser, control, or deparser"
  ;;
  
  (* convert a dictionary of p4 program components into a "tofino_prog" *)
  let p4_prog_to_tofino_prog (p4_prog : p4_prog) : tofino_prog =
  
    let ingress = IdMap.find (id "ingress") p4_prog in
    let egress = IdMap.find (id "egress") p4_prog in
    let control_decls = IdMap.find (id "control") p4_prog in
    let (ingress_globals, ingress_pipe) = component_decls_to_pipe ingress in
    let (egress_globals, egress_pipe) = component_decls_to_pipe egress in
    let control_config = control_decls in
  
    let decl_to_string d = P4TofinoPrinting.string_of_decl d |> P4TofinoPrinting.doc_to_string in
    let global_exists d ds = List.exists (fun d' -> (decl_to_string d) = (decl_to_string d')) ds in
    (* for the globals, we make sure not do add duplicate declarations, e.g., for type definitions. 
       For now, instead of writing equivalence testers, we just use the output from the P4 printer. *)
    let globals = List.fold_left 
      (fun gs d -> if global_exists d gs then gs else gs@[d])
      []
      (ingress_globals@egress_globals)
    in
    {globals; ingress=ingress_pipe; control_config; egress=egress_pipe;}
  ;;

let translate_prog prog =
  (* print_endline ("translating prog: " ^ (TofinoCorePrinting.prog_to_string prog)); *)
  let p4_prog = prog_to_p4_prog prog in
  (* print_endline "----- p4_prog -----";
  print_endline (p4_prog_to_string p4_prog);
  print_endline "-------------------"; *)
  let tofino_prog = p4_prog_to_tofino_prog p4_prog in
  tofino_prog
;;



