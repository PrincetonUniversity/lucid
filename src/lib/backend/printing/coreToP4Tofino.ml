(* tofinoCore --> minimalTofinoSyntax *)
(* 
  Todo: 
  - change layout to only permit 1 generate per stage
  - or eliminate generate statements sooner? 
    - would need to define output parameters for each event, then replace those output 
      parameters with the event fields...
  - support event values
*)
open TofinoCore
open InterpHelpers
open P4TofinoSyntax
module T = P4TofinoSyntax 
module C = TofinoCore
module CS = CoreSyntax
(* open P4TofinoParsing *)
open CoreToP4TofinoHeaders
open P4TofinoPrinting


(*** Generate the ingress control block. ***)

(** params **)
let ingress_control_params = [
  inoutparam (tstruct hdr_t_id) hdr_t_arg;
  inoutparam (tstruct md_t_id) md_t_arg;
  inparam (tstruct ig_intr_md_t) ig_intr_md_arg;
  inparam (tstruct ig_prsr_md_t) ig_prsr_md_arg;
  inoutparam (tstruct ig_dprsr_md_t) ig_dprsr_md_arg;
  inoutparam (tstruct ig_tm_md_t) ig_tm_md_arg;
] ;;

(*** parameter initialization ***)

(*** action generation ***)

(* variable renaming context, e.g., self_event_count *)
module CidMap = Collections.CidMap

type ctx_entry =
  | CCid of CS.cid
  | CExp of CS.exp

type renames = ctx_entry CidMap.t

let empty_renames = CidMap.empty

let rename (x, y) renames = 
  CidMap.add x (CCid(y)) renames
;;

let name renames x : Cid.t =
  match (CidMap.find_opt x renames) with 
  | None -> x
  | Some(CCid(y)) -> y
  | Some(CExp(_)) -> error "[rename.name] expected cid, got exp"
;;

let bind ctx (x, y) : renames = 
  CidMap.add x (CExp(y)) ctx
;;
let bind_lists ctx xs ys = 
  List.fold_left bind ctx (List.combine xs ys)
;;
let find ctx x = 
  match CidMap.find_opt x ctx with 
   | Some(CExp(o)) -> o
   | _ -> error ("[Memop context] could not find expr for variable"^(Cid.to_string x))
;;


(* memop context -- should merge with renaming context? *)
type mctx = renames
(* = CS.exp CidMap.t *)
(* let bind ctx (x, y) : mctx = 
  CidMap.add x y ctx
;;

let bind_lists ctx xs ys = 
  List.fold_left bind ctx (List.combine xs ys)
;;
let find ctx x = 
  match CidMap.find_opt x ctx with 
   | Some(o) -> o
   | _ -> error ("[Memop context] could not find expr for variable"^(Cid.to_string x))
;;
 *)
let cell1_local = id "cell1_local" ;; (* read everywhere. *)
let cell2_local = id "cell2_local" ;; (* *)
let cell1_remote = id "cell1_remote" ;; (* written *)
let cell2_remote = id "cell2_remote" ;;
let ret_remote = id "ret_remote" ;; 
let remote_pair_id = id "remote"

let exp_of_cellid cell_id cell_ty = 
CS.var_sp 
    (cell_id) 
    (CS.ty cell_ty) 
    (Span.default)
;;


let translate_ty ty = match ty with 
  | CS.TBool -> T.TBool
  | CS.TInt (s) -> T.TInt (s)
  | _ -> error "[translate_ty] type translation not implemented"
;;

let translate_value v =
  match v with 
  | CS.VBool(b) -> T.VBool(b)
  | CS.VInt(z) -> T.VInt(Integer.to_int z)
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
;;


let dotstring_of_cid cid = 
    Caml.String.concat "." @@ Cid.names cid
;;
let string_of_fcncid = dotstring_of_cid ;;

let cell_struct_id rid = 
  (id ((Id.name rid)^("_fmt")))
;;

let cell_ty_of_array rid : T.ty = 
    tstruct (cell_struct_id rid)
;;
(* generate cell type struct for each register array. *)
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


type prog_env = {
  memops : (id * memop) list;
  user_fcns : (cid * tdecl) list;
}
let empty_prog_env = {memops = []; user_fcns = [];}
;;

let mk_prog_env tds = 
  List.fold_left (fun prog_env tdecl -> 
    match tdecl.td with
      | TDMemop(m) -> {prog_env with memops=(m.mid, m)::prog_env.memops;}
      | TDLabeledBlock(id, _) -> {prog_env with user_fcns=(Cid.id id, tdecl)::prog_env.user_fcns;}
      | _ -> prog_env
    )
    empty_prog_env
    tds
;;

let rec translate_exp (renames:renames) (prog_env:prog_env) exp : (T.decl list * T.expr) =
  match exp.e with 
  | ECall(fcn_cid, args) -> (
    (* if it is a user function, just add a call *)
    match (CL.mem_assoc fcn_cid prog_env.user_fcns) with
    | true -> [], (ecall fcn_cid [])
    (* if not a user function call, its a builtin module *)
    | false -> translate_module_call renames prog_env.memops fcn_cid args
  )
  | CS.EHash (size, args) -> translate_hash renames size args 
  | CS.EFlood _ -> error "[translate_memop_exp] flood expressions inside of memop are not supported"
  | CS.EVal v -> [], T.EVal (translate_value v.v)
  (* for variables, check renaming first *)
  | CS.EVar cid -> (
    match (CidMap.find_opt cid renames) with 
      (* a cid might resolve to another cid *)
      | Some (CCid (cid)) -> [], T.EVar (cid)
      (* inside a memop, a cid might resolve to an exp. 
        If the exp is a var, it might itself resolve to something else. *)
      | Some (CExp(new_exp)) -> (
        match new_exp.e with 
          | EVar(cid) -> [], T.EVar (name renames cid) 
          | _ -> translate_exp renames prog_env new_exp        
      )
      | None -> [], T.EVar(cid)
  )
  | CS.EOp(op, args) -> (
    let args = match op with 
      | CS.Cast(sz) -> 
        let _, exps = translate_exps renames prog_env args in 
        (eval_int sz)::(exps)
      | CS.Slice(s, e) -> 
        let _, exps = translate_exps renames prog_env args in 
        (eval_int s)::(eval_int e)::(exps)
      | _ -> translate_exps renames prog_env args |> snd
    in
    let op = translate_op op in 
    [], eop op args
  )

and translate_exps renames (prog_env:prog_env) exps = 
  let translate_exp_wrapper exp = translate_exp renames prog_env exp in
  let decls, exps = List.map translate_exp_wrapper exps |> List.split in
  List.flatten decls, exps

and translate_hash renames size args = 
  let hasher_id = Id.fresh_name ("hash") in
  let hasher_id = Id.create ((fst hasher_id)^(string_of_int (snd hasher_id))) in 
  let poly, args = match args with
    | poly::args -> poly, args
    | _ -> error "[translate_hash] invalid arguments to hash call" 
  in
  let args = (snd (translate_exps renames empty_prog_env args)) in 
  let arg = EList(args) in 
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

(* translate exp in a memop -- this used to be its own method, 
   but now we re-use the rename context *)
and translate_memop_exp renames exp = 
  translate_exp renames empty_prog_env exp |> snd 
and translate_memop_exps renames exps =
  List.map (translate_memop_exp renames) exps


(* translate a conditional return expression into an if statement 
   with a single branch for the backend that writes to var lhs_id. *)
and translate_conditional_return renames lhs_id condition_return_opt =
  match condition_return_opt with 
  | None -> T.Noop
  | Some(econd, erhs) -> (
    sif 
      (translate_memop_exp renames econd)
      (sassign (lhs_id) (translate_memop_exp renames erhs))
  )

(* translate a complex memop used for an array into an apply statement *)
and translate_array_memop_complex renames (returns:bool) cell_ty (args: exp list) (memop : memop) =
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
        bind ctx (param_id, arg))
      renames
      (List.combine param_ids args)
    in 
    (* bind "cell1" and "cell2", which may appear in the return statement. *)
    let ctx = List.fold_left
      (fun ctx (param_id, arg) -> 
        bind ctx (param_id, arg))
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
            bind ctx ((Cid.id id),exp)
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
        local cell1_local cell_ty (evar (Cid.id cell1_remote));
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


and translate_pairarray_memop_complex renames slot_ty cell_ty (args: exp list) (memop : memop) =
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
    let cell1_remote_cid = Cid.create_ids [remote_pair_id; cell1_remote] in  
    let cell2_remote_cid = Cid.create_ids [remote_pair_id; cell2_remote] in  

    (* 1. add cell1 and cell2 args *)
    let cell1_in_arg = exp_of_cellid (Cid.id cell1_local) cell_ty in 
    let cell2_in_arg = exp_of_cellid (Cid.id cell2_local) cell_ty in 
    let args = cell1_in_arg::cell2_in_arg::args in 
    let param_ids = memop.mparams |> List.split |> fst |> List.map Cid.id in 
    (* bind parameters *)
    let ctx = List.fold_left
      (fun ctx (param_id, arg) -> 
        bind ctx (param_id, arg))
      renames
      (List.combine param_ids args)
    in 
    (* bind "cell1" and "cell2", which may appear in the return statement. *)
    let ctx = List.fold_left
      (fun ctx (param_id, arg) -> 
        bind ctx (param_id, arg))
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
          bind ctx ((Cid.id id),exp)
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
      local remote_pair_id slot_ty (evar cell1_remote_cid)
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
and translate_array_call renames (memops:(Id.t * memop) list) fcn_id args =
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
      let memop = List.assoc 
        (InterpHelpers.name_from_exp memop |> Cid.to_id )
        memops 
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
    mem_fcn = translate_array_memop_complex renames returns cell_ty args memop;
    })


and translate_pairarray_call renames (memops:(Id.t * memop) list) fcn_id (args:CS.exp list) =
  let reg = name_from_exp (List.hd args) in 
  let slot_ty = cell_ty_of_array (Cid.to_id reg) in 
  let idx = List.hd (List.tl args) in 
  let reg_id =  reg |> Cid.to_id in 
  let reg_acn_id = Id.fresh_name ((fst reg_id)^"_regaction") in 
  let memop, args, cell_ty = match (string_of_fcncid fcn_id) with 
    | "PairArray.update_complex" -> (
      let  memop, arg1, arg2 = match args with
        | [_; _; memop; arg1; arg2; _] -> memop, arg1, arg2
        | _ -> error "[translate_pairarray_call] unexpected arguments for array.update_complex"
      in 
      let memop = List.assoc 
        (InterpHelpers.name_from_exp memop |> Cid.to_id )
        memops 
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
    mem_fcn = translate_pairarray_memop_complex renames slot_ty cell_ty args memop;
    })


and translate_sys_call fcn_id _ = 
  let e_ts = eop 
    (Slice) 
    [evar (Cid.create ["ig_intr_md"; "ingress_mac_tstamp"]); 
      eval_int 47; eval_int 16]
  in 
  let fcn_name = List.nth (Cid.names fcn_id) 1 in 
  match fcn_name with
  | "time" -> [], e_ts
  | "invalidate" -> (
    let var_cid = match Cid.to_ids fcn_id with 
      | _::_::vcid -> Cid.create_ids vcid
      | _ -> error "[sys.invalidate translation] Sys.invalidate function call Cid should have a third component -- the variable to invalidate."
    in   
    [], ecall (Cid.concat (handler_struct_arg (Cid.to_id var_cid)) (cid "setInvalid")) []
  )
  | "random" -> (
    (* declare rng_###, call rng_###.get(); *)
    let rng_id = Id.fresh_name "rng" in
    let decl = drandom 32 rng_id in
    let expr = ecall (Cid.create_ids [rng_id; id "get"]) [] in
    [decl], expr
  )
  | s -> error ("[translate_sys_call] unknown sys function: "^s)

and translate_module_call renames (memops:(Id.t * memop) list) fcn_id args = 
  match (List.hd (Cid.names fcn_id)) with 
      | "Array" -> 
        let regacn_id, decl = translate_array_call renames memops fcn_id args in
        let idx_arg = translate_memop_exp renames
          (List.hd (List.tl args))
        in        
        [decl], ecall (Cid.create_ids [regacn_id; (id "execute")]) [idx_arg]
      | "PairArray" -> 
        let regacn_id, decl = translate_pairarray_call renames memops fcn_id args in 
        let idx_arg = translate_memop_exp renames
          (List.hd (List.tl args))
        in        
        [decl], ecall (Cid.create_ids [regacn_id; (id "execute")]) [idx_arg]
      | "Sys" -> translate_sys_call fcn_id args
      | s -> error ("[translate_memop_call] unknown module: "^s)
;;


let validate_headers evids = 
  List.map
      (fun evid -> validate (handler_struct_arg evid))
      evids
;;

let other_evids ev_enum evid =
  MiscUtils.remove evid (List.split ev_enum |> fst)
;;

let validate_all ev_enum = 
  validate_headers (List.split ev_enum |> fst)
;;


let translate_generate (renames:renames) (prev_decls:T.decl list) hdl_enum hdl_params gty ev_exp =
  let evid, param_ids, args = match ev_exp.e with 
    | ECall(ev_cid, args)  -> (
      let evid = Cid.to_id ev_cid in 
      let param_ids = 
        List.assoc evid hdl_params 
        |> CL.split 
        |> fst 
      in
      evid, param_ids, args
    )
    | _ -> error "[translate_generate] events should all be ECalls by this point"
  in
  let event_num = List.assoc (evid) hdl_enum in         
  (* always set event present flag and enable event header *)
  let common_stmts = [
    sassign (handler_multi_ev_flag_arg evid) (eval_int 1);
    validate (ev_arg evid)
    ]
  in
  (* always set event params to args *)
  let param_assignments = 
    let set_event_param ev_id (param_id,arg_exp) =
      let param_arg = handler_param_arg ev_id param_id in 
      let _, translated_arg_exps = translate_exp renames empty_prog_env arg_exp in 
      sassign param_arg translated_arg_exps
    in
    List.map 
    (set_event_param evid) 
    (List.combine param_ids args)
  in


  let new_mc_decls, gty_stmts = match gty with 
  (* nothing special to do for recircs *) 
  | GSingle(None) -> [], []
  (* set egress port variable and port_out_event *)
  | GPort(eport) -> 
    [], [
      sassign (port_out_event_arg) (eval_int event_num);
      sassign (egr_port_arg) ((translate_exp renames empty_prog_env eport )|> snd)
    ]
  (* create multicast group, set mc_group_b and port_out_event*)
  | GMulti(egroup) -> (
    let mc_num_exp, new_mc_decls = match (egroup.e) with 
      | EVal({v=VGroup(ports);_}) -> (
        (* each port's copy has a replica ID of 0 for a ports multicast *)
        let replicas = List.map (fun p -> (p, 0)) ports in 
        let mc_group, new_mc_decls = match find_mcgroup prev_decls ports with 
          | Some(gid) -> gid, []
          | None -> 
            let gid = 
              (List.length (mcgroups_of_decls prev_decls)) 
              + mcast_locations_start 
            in 
            gid, [decl (DMCGroup{gid; replicas;})]
        in
        eval_int mc_group, new_mc_decls
      )
      | EFlood(eport) -> 
        let _, eport = translate_exp renames empty_prog_env eport in 
        let eport = eop Cast [eval_int 16; eport] in (* mcid is 16 bit *)
        eop Add [eport; eval_int (mcast_flood_start)], []
      | _ -> error "[translate_generate.GMulti] group expression must be group value or flood call"    
    in
    new_mc_decls, [
      sassign (port_out_event_arg) (eval_int event_num);
      sassign (mcast_grp_b_arg) mc_num_exp 
    ]
  )
  | _ -> error "[translate_generate] unknown generate form"
  in
  new_mc_decls, sseq (common_stmts@param_assignments@gty_stmts)
;;


let declared_vars prev_decls = CL.filter_map (fun dec -> 
  match dec.d with 
    | DVar(id, _, _) -> Some id
    | _ -> None
  )
  prev_decls

(* let new_ctx = {mc_groups = [];} *)

(* translate a statement that appears inside of an action. 
   generate a list of decls and a statement. *)
let rec translate_statement (renames:renames) prev_decls hdl_enum hdl_params prog_env stmt =
  match stmt.s with
  | SNoop -> [], Noop
  | C.SUnit(e) ->
    let decls, expr = translate_exp renames prog_env e in 
    decls, sunit expr
  | C.SLocal(id, ty, e) -> (
    (* variables can't be declared inside of an action, so produce a global declaration 
       with a default value and return an assign statement. *)
    match (List.exists (fun idb -> Id.equals id idb) (declared_vars prev_decls)) with
    | true -> (
      (* let vardecl = dvar_uninit id (translate_rty ty.raw_ty) in  *)
      let decls, expr = translate_exp renames prog_env e in 
      decls, sassign (Cid.id id) expr
    )
    | false -> (
      let vardecl = dvar_uninit id (translate_rty ty.raw_ty) in 
      let decls, expr = translate_exp renames prog_env e in 
      vardecl::decls, sassign (Cid.id id) expr
    )
  )
  | C.SAssign(id, e) -> 
    let decls, expr = translate_exp renames prog_env e in 
    (* use the new name, if there is one *)
    decls, sassign (name renames (Cid.id id)) expr 
  | SPrintf _ -> error "[translate_statement] printf should be removed by now"
  | SIf _ -> error "[translate_statement] if statement cannot appear inside action body"
  | SMatch _ -> error "[translate_statement] match statement cannot appear inside action body"
  | SRet _ -> error "[translate_statement] ret statement cannot appear inside action body"
  | SGen(gty, ev_exp) -> translate_generate renames prev_decls hdl_enum hdl_params gty ev_exp
  | SSeq (s1, s2) -> 
    let s1_decls, s1_stmt = translate_statement renames prev_decls hdl_enum hdl_params prog_env s1 in
    let s2_decls, s2_stmt = translate_statement renames prev_decls hdl_enum hdl_params prog_env s2 in
    s1_decls@s2_decls, sseq [s1_stmt; s2_stmt]
;;

(* generate an action, and other compute objects, from a labeled statement *)
let translate_labeled_block (renames, hdl_enum,hdl_params,prog_env) p4tdecls tdecl =
  let new_decls = match tdecl.td with 
    | TDLabeledBlock(id, stmt) -> (
      let decls, action_body = translate_statement renames p4tdecls hdl_enum hdl_params prog_env stmt in
      let action_decl = decl (DAction({id=id; body=action_body;})) in 
      decls@[action_decl]
    )
    | _ -> []
  in
  p4tdecls@new_decls
;;

(* the variable renaming map for ingress *)
let mk_ingress_rename_map tds =  
  let m = main tds in 
  (* parameter renaming *)
  let renames = List.fold_left 
    (fun names (evid, params) -> 
      List.fold_left
        (fun names (param_id, _) -> 
          rename (Cid.id param_id, ev_param_arg evid param_id) names)
        names
        params
      )
    empty_renames
    m.hdl_params
  in
  (* builtin renaming: 
    ingress_port 
    hdl_selector
    events count
  *)
  rename ((Cid.id Builtins.ingr_port_id), igr_port_arg) renames |> 
  rename ((Cid.id (m.hdl_selector |> fst)), cur_ev_arg) |> 
  rename ((Cid.id (m.event_output.recirc_mcid_var |> fst)), mcast_grp_a_arg)
;;

(** table generation **)
let fresh_table_id _ = Id.fresh_name "table"
;;

let translate_pat pat = 
  match pat with
    | C.PWild -> T.PWild
    | PNum z -> T.PNum (Z.to_int z)
    | PBit bits -> (
      let bits = List.map 
        (fun i -> match i with 0 -> T.B0 | 1 -> T.B1 | _ -> T.BANY)
        bits
      in 
      PBitstring bits
    )
;;

(* translate IR branches into backend branches, 
   plus a default statement when necessary. 
   A default statement is necessary in a table with 
   no rules. So we don't use it for now.
*)
let translate_branch (complete_branches, default_rule) 
    ((pats:C.pat list),(stmt:C.statement))
    : ((T.pat list * T.statement) list * T.statement option) =    

  let translate_call stmt =
    match stmt.s with 
    | SUnit({e=ECall(acn_id, _); _}) -> sunit (ecall acn_id [])
    | _ -> error "[translate_branch] branch statement must be a method call to a labeled block, which represents an action"
  in 
  complete_branches@[(List.map translate_pat pats, translate_call stmt)],
  default_rule
;;

(* translate a match statement into a table *)
let generate_table renames (stage_pragma:pragma) (ignore_pragmas: (id * pragma) list) (tid, stmt) : (T.decl * T.statement) = 
  let _ = stage_pragma in (* not needed *)
  let action_id_of_branch (_, stmt) =
    match stmt.s with
      | SUnit({e=ECall(acn_id, _); _}) -> Cid.to_id acn_id  
      | _ -> error "[translate_branch] branch must be a call to a labeled block."
  in
  match stmt.s with
  | SMatch(exps, branches) -> 
    let _, keys = translate_exps renames empty_prog_env exps in 
    let keys = List.map tern_key_of_exp keys in 
    let actions = List.map action_id_of_branch branches |> MiscUtils.unique_list_of in 

    let tbl_call = sunit (ecall_table tid) in 
    (* note: we don't use default rules because the compiler doesn't 
             generate keyless tables. *)
    let branches, default_opt = List.fold_left 
      translate_branch
      ([], None)
      branches
    in 
    let tbl_dec = 
      dtable 
        tid
        keys
        actions
        branches
        default_opt
        ((List.remove_assoc tid ignore_pragmas |> List.split |> snd;))
        (* (stage_pragma::(List.remove_assoc tid ignore_pragmas |> List.split |> snd;)) *)
    in
    (tbl_dec, tbl_call)
  | _ -> error "[generate_table] not a match statement!"
;;

(* translate a statement that represents 
   a vector of tables executing in the same stage. *)
let generate_stage stage_num renames block_id stmt =
  let table_stmts = InterpHelpers.unfold_stmts stmt in
  let table_ids = List.map (fresh_table_id) table_stmts in 
  let stage_pragma = {pname="//stage "^(string_of_int stage_num); pargs = [];} in 
  let ignore_dep_pragmas = List.map
    (fun tbl_id ->
      (tbl_id, 
        {pname="ignore_table_dependency";
        pargs=["\""^Id.name block_id^"."^(Id.name tbl_id)^"\""];}
    ))
    table_ids
  in 
  let decls_calls = List.map 
    (generate_table renames stage_pragma ignore_dep_pragmas) 
    (List.combine table_ids table_stmts)
  in 
  decls_calls
;;

let rec generate_stages stage_num renames block_id (stages:C.statement list) =
  match stages with 
  | [] -> []
  | hd_stage::stages ->
    (generate_stage stage_num renames block_id hd_stage)@(generate_stages (stage_num + 1) renames block_id stages)
;;


(* declare the compiler-added variables -- "shared globals" *)
let generate_added_var_decls tds = List.map 
  (fun (id, ty) -> dvar_uninit id (translate_ty ty.raw_ty))
  (main tds).shared_locals
;;
let includes = [dinclude "<core.p4>"; dinclude "<tna.p4>"]

(* create multicast group for cloning n generated recirc events *)
let mc_recirc_decls evids recirc_port =
  let num_events = List.length evids in
  let possible_rids = MiscUtils.range 1 (1 + num_events) in 
  let mc_group_n_recirc gid =
    let replicas = List.map (fun rid -> (recirc_port, rid)) (MiscUtils.range 1 (gid+1)) in 
    decl (DMCGroup{gid; replicas})
  in
  List.map mc_group_n_recirc possible_rids
;;

let generate_ingress_control prog_env block_id tds =
  let renames = mk_ingress_rename_map tds in 
  (* declare local copies of all event parameters *)
  (* let tds, param_decls = initialize_params tds in  *)
  (* let init_decls = initialize_fields () in  *)
  let m = main tds in 
  let added_var_decls = generate_added_var_decls tds in 
  let table_decls, table_calls = generate_stages 0 renames block_id m.main_body |> List.split in 
  let apply_body = sseq table_calls in 

  let action_decls = List.fold_left
    (translate_labeled_block 
      (renames, m.hdl_enum,m.hdl_params,
        prog_env
      )
    )
    []
    tds
  in
  let mc_decls = mcgroups_of_decls action_decls in 
  let action_decls = non_mcgroups_of_decls action_decls in 
  let ingress_control = decl (DControl{
    id = block_id;
    params = ingress_control_params;
    decls = added_var_decls@action_decls@table_decls;
    body = Some(apply_body);
    })
  in

  ingress_control, mc_decls
;;


let generate_mc_config created_decls =
  mcgroups_of_decls created_decls
;;

(*** egress control ***)

(*** recirculated event-packet handling ***)
let recirc_acn_id evid = id ((fst evid)^"_recirc") ;;

(* reset the multi-event header*)
let reset_multi_event ev_enum =
  let evids = List.split ev_enum |> fst in
  set_int (port_out_event_arg) 0
  ::(List.map (fun evid -> set_int (handler_multi_ev_flag_arg evid) 0) evids)
;;

(*note: this is currently identical to extract_internal_port_event_action, 
  because the recirc port is treated the same as an internal port *)
let extract_recirc_event_action ev_enum evid =
  (* 
    1. disable all event headers except evid
    2. set single-event header.event_id = evid 
    3. wipe multi-event header *)
  let evids = List.split ev_enum |> fst in 
  let other_evid = MiscUtils.remove evid evids in
  let invalidate_other_headers = 
    List.map
      (fun evid -> invalidate (handler_struct_arg evid))
      other_evid
  in
  let single_ev_setup = [
    sassign (cur_ev_arg) (eval_int (List.assoc evid ev_enum))
    ]
  in
  let multi_event_reset = reset_multi_event ev_enum in 
  daction (recirc_acn_id evid) (sseq (invalidate_other_headers@single_ev_setup@multi_event_reset))
;;

let extract_recirc_event_actions ev_enum =
  List.map 
    (extract_recirc_event_action ev_enum)
    (List.split ev_enum |> fst)
;;

(* create the table to extract recirculation events from the packet *)
let extract_recirc_event_table tbl_id tds (evids : Id.t list) =
  let hdl_enum = (main tds).hdl_enum in 
  let hdl_enum_rev = List.map (fun (x, y) -> (y, x)) hdl_enum in 
  let event_seqs = (main tds).event_output.ev_gen_seqs in 
  (* keys: rid, port_event_id, event flags *)
  let keys = Ternary(rid_local)::Ternary(port_out_event_arg)
    ::(List.map 
        (fun evid -> Ternary(handler_multi_ev_flag_arg evid)) 
        evids) 
  in
  (* action ids:  one for each event *)
  let action_ids = List.map recirc_acn_id evids in  


  (* let string_of_ids ids = List.map Id.to_string ids |> String.concat ", " in  *)
  (* branches: 
    in human-friendly code, we want:
        for rid in rids:
          for port_out_evid in evids:
            for event_seq in event_seqs:
              create_branch(rid, port_out_event, event_seq
  *)
  let branches = 
    let recirc_acn_call evid = sunit (ecall (Cid.id (recirc_acn_id evid)) []) in
    let possible_rids = MiscUtils.range 1 (1 + (List.length evids)) in 
    (* evnum 0 means that no recirc event was generated *)
    let possible_evnums = MiscUtils.range 0 (1 + (List.length evids)) in 

    let create_branch rid port_out_evnum event_seq =
      let ev_idx = rid - 1 in (* because rid starts at 1, and we may want the 0th event *)
      let flag_pats = intpats_of_evseq evids event_seq
        |> List.map pnum
      in
      let pats = (pnum rid)::(pnum port_out_evnum)::flag_pats in
      let recirc_event_seq_opt = match port_out_evnum with 
        | 0 -> Some(event_seq) (* no port out event, all recircs*)
        | _ -> (
          let port_out_evid = (List.assoc (port_out_evnum) hdl_enum_rev) in
          match (MiscUtils.contains event_seq port_out_evid) with 
          (* if the generated event sequence doesn't contain the port 
             out event, then this port out event cannot occur for 
             this event sequence. *)
          | false -> None
          | true -> 
            Some (
            MiscUtils.list_remove 
              event_seq 
              port_out_evid)
        )
      in
(*       print_endline (
        "[create_branch]"
        ^" rid: "^(string_of_int rid)
        ^" seq_idx: "^(string_of_int ev_idx)
        ^" port_out_evnum: "^(string_of_int port_out_evnum)
        ^" event_seq: "^(string_of_ids event_seq)
        ^" recirc_event_seq: "^(string_of_ids recirc_event_seq)
      );
 *)
      match recirc_event_seq_opt with 
      | Some(recirc_event_seq) -> (
        match (List.nth_opt recirc_event_seq (ev_idx)) with 
          | Some(ev) -> 
            (* print_endline ("event to extract: "^(Id.to_string ev)); *)
            Some(pats, recirc_acn_call ev)
          | None -> None
      )
      | None -> None
    in
    (* for each rid *)
    List.fold_left (fun branches rid -> 
      (* for each event that may appear in a port out (all events may) *)
      List.fold_left (fun branches port_out_evid -> 
        (* for each event_seq in event_seqs *)
        List.fold_left (fun branches event_seq -> 
          match (create_branch rid port_out_evid event_seq) with
          | None -> branches
          | Some(b) -> branches@[b]
        )
        branches event_seqs
      )
      branches possible_evnums
    )
    [] possible_rids
  in
  dtable tbl_id keys action_ids branches None []
;;



(*** port-event-packet handling ***)
let port_external_action evid =
  id@@(fst evid)^"_to_external"
;;
let port_internal_action evid =
  id@@(fst evid)^"_to_internal"
;;

let extract_port_event_action (ev_enum:(Id.t * int) list) (evid:Id.t) =
  let evids = List.split ev_enum |> fst in 
  let other_evids = MiscUtils.remove evid evids in 
  let invalidate_events = (List.map (fun evid -> invalidate (handler_struct_arg evid)) other_evids) in 
  let external_port_stmts = [
    invalidate eth_arg;
    invalidate single_ev_arg;
    invalidate multi_ev_arg ]
    @invalidate_events
  in
  let internal_port_stmts = [
    (* invalidate multi_ev_arg; *)
    sassign (cur_ev_arg) (eval_int (List.assoc evid ev_enum))]
    @(reset_multi_event ev_enum)
    @invalidate_events
  in
  let to_external = daction
    (port_external_action evid)
    (sseq external_port_stmts)
  in
  let to_internal = daction
    (port_internal_action evid)
    (sseq internal_port_stmts)
  in
  (to_external, to_internal)
;;

let extract_port_event_actions ev_enum =
  let exts, ints = List.map 
    (extract_port_event_action ev_enum)
    (List.split ev_enum |> fst)
    |> List.split
  in
  exts@ints
;;

let extract_port_event_table tbl_id ev_enum lucid_internal_ports =
  let evids = List.split ev_enum |> fst in 
  let keys = [Ternary(port_out_event_arg); Ternary(egress_port_local)] in
  let actions = List.fold_left
    (fun aids evid -> aids@[port_external_action evid; port_internal_action evid])
    []
    evids
  in
  let rules = 
    let foreach_evid evid = 
      let evnum = List.assoc evid ev_enum in 
      let foreach_internal_port port = 
        ([pnum evnum; pnum port],scall_action (port_internal_action evid))
      in
      let internal_rules = List.map foreach_internal_port lucid_internal_ports in 
      let external_rule = ([pnum evnum; pwild], scall_action(port_external_action evid) ) in 
      internal_rules@[external_rule]
    in
    List.map foreach_evid evids |> List.flatten
  in
  dtable tbl_id keys actions rules None []
;;

let egress_control_params = [
  inoutparam (tstruct hdr_t_id) hdr_t_arg;
  inoutparam (tstruct md_t_id) md_t_arg;
  inparam (tstruct eg_intr_md_t) eg_intr_md;
  inparam (tstruct eg_prsr_md_t) eg_prsr_md;
  inoutparam (tstruct eg_dprsr_md_t) eg_dprsr_md;
  inoutparam (tstruct eg_oport_md_t) eg_oport_md;
] ;;


let generate_egress_control block_id tds lucid_internal_ports =
  let ev_enum = (main tds).hdl_enum in 
  let t_extract_port_event = id "t_extract_port_event" in
  let t_extract_recirc_event = id "t_extract_recirc_event" in 
  let decls = 
    (extract_recirc_event_actions ev_enum)
    @(extract_port_event_actions ev_enum)
    @[(extract_recirc_event_table t_extract_recirc_event tds (List.split ev_enum |> fst));
    extract_port_event_table t_extract_port_event ev_enum lucid_internal_ports]
  in
  let stmt = 
    sifelse (eeq rid_local 0)
      (scall_table t_extract_port_event)
      (scall_table t_extract_recirc_event)
  in
  dcontrol block_id egress_control_params decls stmt
;;

(*** register arrays and their slot types ***)
let generate_reg_arrays tds =
  let generate_reg_array tdecl = 
    match tdecl.td with 
    | TDGlobal(rid, ty, exp) -> (
      match ty.raw_ty with 
      | TName(tcid, [cell_size], true) -> (
          let module_name = List.hd (Cid.names tcid) in 
          let len, default_opt = match exp.e with 
            | ECall(_, args) -> (
              match (translate_exps empty_renames empty_prog_env args |> snd) with 
              | [elen; edefault] -> elen, Some(edefault)
              | [elen] -> elen, None
              | _ -> error "[generate_reg_array] expected 1 or 2 arguments in global constructor"
            )
            | _ -> error "[generate_reg_array] right hand side of global mutable declaration must be a constructor call";
          in
          match (module_name) with 
          | "Array" -> 
            let reg_decl = (decl (DReg{
              id=rid; 
              cellty=tint cell_size; 
              idxty=TAuto; 
              len=len; 
              def=default_opt})) 
            in
            [reg_decl]
          | "PairArray" -> 
            (* pair arrays get their own cell type struct *)
            let cell_struct = cell_struct_of_array_ty module_name cell_size rid in 
            (* id, cell ty (struct); idx ty; len; default (None) *)
            let reg_decl = (decl (DReg{id=rid; cellty=cell_ty_of_array rid; idxty=TAuto; len=len; def=default_opt})) in
            [cell_struct; reg_decl]
          | _ -> []
      )
      (* not a mutable global *)
      | _ -> []
    )
    (* not a global *)
    | _ -> []
  in
  List.map generate_reg_array tds |> List.flatten
;;


(* create multicast groups for flooding events out of ports *)
let mc_flood_decls external_ports = 
  let flood_port_decl port =
    let other_ports = MiscUtils.remove port external_ports in 
    let replicas = List.map (fun port -> (port, port_event_rid)) other_ports in 
    decl (DMCGroup{gid=port+mcast_flood_start; replicas})
  in
  List.map flood_port_decl external_ports
;;

let translate tds (portspec:ParsePortSpec.port_config) = 
  let prog_env = mk_prog_env tds in 
  let port_defs = Caml.List.map 
    (fun (port:ParsePortSpec.port) -> decl (DPort{dpid=port.dpid; speed=port.speed;})) 
    (portspec.internal_ports@portspec.external_ports)
  in 
  let lucid_aware_dpids = portspec.recirc_dpid::(List.map (fun (port:ParsePortSpec.port) -> port.dpid) portspec.internal_ports) in
  let external_dpids = (List.map (fun (port:ParsePortSpec.port) -> port.dpid) portspec.external_ports) in
  (* translate multicast groups first, 
      because we need them for ingress control translation *)
  (* let mcgroup_enum, mc_branches = generate_mc_config tds in  *)
  let ingress_control, mc_decls = generate_ingress_control prog_env (id "IngressControl") tds in 
  let mc_defs = mc_decls@mc_flood_decls external_dpids@(mc_recirc_decls ((main tds).hdl_enum |> List.split |> fst) portspec.recirc_dpid) in 
  (*     List.map (fun port -> decl (DPort{dpid=port; speed=40;})) ports_40g
    @(List.map (fun port -> decl (DPort{dpid=port; speed=10;})) ports_10g)
  in   *)
  let tofino_prog = {
    globals = 
      includes
      @(generate_structs tds)
      @(generate_reg_arrays tds);
    ingress = {
      parse = generate_ingress_parser (id "IngressParser") (main tds) lucid_aware_dpids;
      process = ingress_control;
      deparse = generate_ingress_deparser (id "IngressDeparser") tds;
      };
    control_config = mc_defs@port_defs;
    egress = {
      parse = generate_egress_parser (id "EgressParser") tds;
      process = generate_egress_control (id "EgressControl") tds lucid_aware_dpids;
      deparse = generate_egress_deparse (id "EgressDeparse") tds;
    }
  } in
  tofino_prog
;;


let control_block_lib_params tds =
  let params = ((main tds).hdl_params |> List.hd |> snd) in 
  let params = List.map (fun (pid, pty) -> 
    inoutparam (translate_ty pty.raw_ty) (pid))
    params
  in
  params
;;

let generate_control_lib prog_env tds =
  let renames = empty_renames in 
  let m = main tds in 
  let block_id = m.hdl_enum |> List.hd |> fst in 
  let added_var_decls = generate_added_var_decls tds in 
  let table_decls, table_calls = generate_stages 0 renames block_id m.main_body |> List.split in 
  let apply_body = sseq table_calls in 
  let action_decls = List.fold_left
    (translate_labeled_block 
      (renames, m.hdl_enum,m.hdl_params,
        prog_env
      )
    )
    []
    tds
    |> non_mcgroups_of_decls  
  in 
  let control_block = decl (DControl{
    id = block_id;
    params = control_block_lib_params tds;
    decls = added_var_decls@(generate_reg_arrays tds)@action_decls@table_decls;
    body = Some(apply_body);
    })
  in
  includes@[control_block]
;;


(* translate a single handler program into a control block. *)
let translate_to_control_block tds = 
  generate_control_lib (mk_prog_env tds) tds
;;
