(* Tables as a builtin module *)
(* TODO: test with nested types for keys, args *)
(* TODO: update documentation *)
(* TODO: add an install_mask_priority function *)
(* TODO: add a remove function *)
(* TODO: add an update function *)
(* TODO: simplify action / action constructor syntax *)
(* TODO: remove all the pattern syntax and 
         special table syntax from the frontend *)
open Batteries
open Syntax
open InterpSwitch
open LibraryUtils
open Pipeline
open InterpSyntax


(*** masked-key helpers ***)

(* translate a value into a (value, mask) tuple 
  representing an exact match pattern *)


let bitstring_to_maskedint (bs : int list) : int*int = 
  let module Int = Caml.Int in
  let to_val_and_mask bit = 
    match bit with 
    | 0    -> (0, 1) (* val: 0, mask 1 *) 
    | 1    -> (1, 1) (* val: 1, mask 1 *) 
    | -1  -> (0, 0) (* val: 0, mask :0 *)
    | _ -> error "invalid bitstring bit"
  in
  let rec bitstring_to_int bits =  
    match bits with 
      | [] -> 0
      | hd::tl ->
        (Int.shift_left hd (List.length tl)) + (bitstring_to_int tl)
  in      
  let vbits, mbits = List.map to_val_and_mask bs |> List.split in 
  bitstring_to_int vbits, bitstring_to_int mbits
;;
  

let rec v_to_flat_exact (v : CoreSyntax.v) = 
match v with 
  | VTuple(vs) -> 
    CoreSyntax.VTuple(List.map v_to_flat_exact vs)
  | VRecord(id_vs) -> 
    VRecord(
      List.combine
        ((List.split id_vs) |> fst)
        (List.split id_vs |> snd |> List.map v_to_flat_exact))
  | VBool _ -> CoreSyntax.VTuple([v; CoreSyntax.VBool true])
  | VInt(z) -> 
    let mask = Integer.max_int (Integer.size z) in
    CoreSyntax.VTuple([v; CoreSyntax.VInt(mask)])
  | VBits b -> 
    let v = BitString.bits_to_int b  in 
    let v = Integer.create ~value:v ~size:(List.length b) in
    let m  = Integer.max_int (List.length b) in
    CoreSyntax.VTuple([CoreSyntax.VInt(v); CoreSyntax.VInt(m)])
  | VGlobal _ -> Syntax.error "a global cannot appear as a key in a table"
  | VEvent _ -> Syntax.error "an event cannot appear as a key in a table"
  | VPat xs  -> 
    (* for VPat we keep the mask because that is only used by table
  install in the depreciated table syntax. *)
    (* let v_strs = List.map string_of_int xs in *)
    let v, m = bitstring_to_maskedint xs in
    let v = Integer.create ~value:v ~size:(List.length xs) in
    let m = Integer.create ~value:m ~size:(List.length xs) in
    CoreSyntax.VTuple([CoreSyntax.VInt(v); CoreSyntax.VInt(m)])
    (* print_endline ("v_strs: "^(String.concat ", " v_strs)); *)

    
    (* Console.error "pat values are depreciated" *)
  | VGroup _ -> Syntax.error "group values cannot appear as a key in a table"
;;


(* check if a list of values matches a pattern, where the 
   pattern is a list of (value, mask) tuples *)
let matches_pat_vals (vs : CoreSyntax.value list) (pats : CoreSyntax.value list) =
  List.for_all2
    (fun (v : CoreSyntax.value) (p: CoreSyntax.value) ->
      match v.v, p.v with
      | VInt n, VTuple([VInt(v);VInt(m)]) -> 
        Integer.equal (Integer.bitand n m) (Integer.bitand v m)
      | _ -> false)
    vs
    pats
;;

  
let error= Syntax.error

let name = "Table"
let module_error fun_name msg =
  error (name ^ ": " ^ fun_name ^ ": " ^ msg)
;;
let id = Id.create name
let module_id = id
let t_id = Cid.create_ids [id; Id.create "t"]

let sizes = 0 (* key dimensions, action args, action ret *)
let ty_args = 4
let global = true


(* Table.t<<'key_ty, 'installarg_ty, 'matcharg_ty, 'ret_ty>> *)
let fresh_t (key_ty, iarg_ty, marg_ty, ret_ty) = 
  TBuiltin(t_id, [key_ty; iarg_ty; marg_ty; ret_ty], true)
;;

let mk_match_t arg_lens = 
  (* arg_counts tells you how many elements are in each argument
     its for after tuple elimination *)
  if List.length arg_lens != ty_args
    then error "wrong number of arg lens used with table type constructor"
  else 
    error "not done"


let create_id = Cid.create_ids [id; Id.create "create"]
let create_sig =
  let key_rty, iarg_rty, marg_rty, ret_rty = 
    fresh_rawty "table_key_ty",
    fresh_rawty "table_iarg_ty",
    fresh_rawty "table_marg_ty",
    fresh_rawty "table_ret_ty"
  in
  let acn_rty = taction iarg_rty marg_rty ret_rty in
  let acns_rty = TVector(acn_rty, fresh_size "acn_vec_len") in
  let start_eff = FVar (QVar (Id.fresh "tbl_create_eff")) in
  let arg_tys = [
      fresh_tint () ;                            (* number of entries *)
      ty @@ acns_rty;                            (* actions bound to the table *)
      ty @@ acn_rty;                             (* default action *)
      ty @@ iarg_rty                              (* default action argument *)
    ] 
  in
  let tbl_eff = FVar (QVar (Id.fresh "tbl_eff")) in
  let ret_ty = ty_eff (fresh_t (key_rty, iarg_rty, marg_rty, ret_rty)) tbl_eff in
  let ctor_ty = {
      arg_tys
    ; ret_ty
    ; start_eff
    ; end_eff = start_eff
    ; constraints = ref []
  } in
  ctor_ty 
;;

(* interpreter implementation *)
let create_ctor (nst : InterpSwitch.network_state) swid args = 
  match args with 
  (* the table value arg is added by interpcore *)
  | [tbl_v; tbl_len; tbl_acn_ctors; tbl_def_acn; tbl_def_args] -> 
    let _ = tbl_acn_ctors in
    let st = nst.switches.(swid) in
    let p = st.pipeline in
    let tbl_id = match tbl_v with 
      | V { v = VGlobal(tbl_id, _) } -> tbl_id
      | _ -> error"Table.create: expected a global for the table id"
    in
    let def_acn_cid, def_acn_ctor = 
      ival_fcn_to_internal_action nst swid tbl_def_acn 
    in
    let flat_default_args = match tbl_def_args with 
      | V({v=VTuple(vs)}) -> List.map CoreSyntax.value vs
      | V(v) -> [v]
      | _ -> error"Table.create: expected a tuple for the default action args"
    in

    let def_acn remaining_args =
      (* print_endline ("inside of partially_applied_def_acn");
      print_endline ("default args: "^(CorePrinting.comma_sep CorePrinting.value_to_string flat_default_args));
      print_endline ("remaining args: "^(CorePrinting.comma_sep CorePrinting.value_to_string remaining_args)); *)
      def_acn_ctor (flat_default_args@remaining_args)
    in
    let tbl_size = match tbl_len with 
      | V(v) -> (
        match v.v with 
          | VInt(i) -> Integer.to_int i
          | _ -> error"Table.create: expected an integer for the table size"
      )
      | _ -> error"Table.create: expected an integer for the table size"
    in

    let new_pipe = Pipeline.append p
      (Pipeline.mk_table tbl_id tbl_size def_acn def_acn_cid flat_default_args)
    in
    new_pipe  
  | _ -> error"wrong number of arguments to Table.create"
;;


(* Table.install : Table.t<<'k, iarg, marg, ret>> -> 'k -> (iarg -> marg -> ret) -> iarg -> () *)
let install_name = "install"
let install_id = Id.create install_name
let install_cid = Cid.create_ids [id; install_id]
let install_error msg = module_error install_name msg

let install_ty = 
  let key_rty, iarg_rty, marg_rty, ret_rty = 
    fresh_rawty "table_key_ty",
    fresh_rawty "table_iarg_ty",
    fresh_rawty "table_marg_ty",
    fresh_rawty "table_ret_ty"
  in
  let acn_rty = taction iarg_rty marg_rty ret_rty in  
  let start_eff = FVar (QVar (Id.fresh "eff")) in (* effect where this call begins *)
  (* table argument *)
  let tbl_eff = FVar (QVar (Id.fresh "eff")) in (* effect attached to table arg *)
  let tbl_t = ty_eff (fresh_t (key_rty, iarg_rty, marg_rty, ret_rty)) tbl_eff in
  let arg_tys = [
      tbl_t; 
      ty @@ key_rty;
      ty @@ acn_rty;
      ty @@ iarg_rty
    ]
  in
  let ret_ty = ty @@ TVoid in
  ty @@ TFun
    { arg_tys
    ; ret_ty
    ; start_eff
    ; end_eff = FSucc tbl_eff
    ; constraints = ref [CLeq (start_eff, tbl_eff)]
    }
;;

(* install an exact pattern, with key value equal to mask *)
let install_fun nst swid args =
  let _, _ = nst, swid in
  let open CoreSyntax in
  match args with
  | [vtbl; vkey; vaction; vaction_const_arg_tup] -> 
    let target_pipe = nst.switches.(swid).pipeline in    
    let stage = match (extract_ival vtbl).v with 
      | VGlobal(_, stage) -> stage
      | _-> error "Table.install: table arg didn't eval to a global"
    in

    let keys = flatten_v (extract_ival vkey).v in 
    let pat_tuple_keys = List.map v_to_flat_exact keys in
    let keys = List.map value pat_tuple_keys in 
    let vaction_const_args = match vaction_const_arg_tup with 
      | V({v=VTuple(vs)}) -> List.map CoreSyntax.value vs
      | V(v) -> [v]
      | _ -> error "Table.create: expected a tuple for the default action args"
    in
    let acn_cid, acn_ctor = 
      ival_fcn_to_internal_action nst swid vaction
    in

    let acn remaining_args = 
      acn_ctor (vaction_const_args@remaining_args)
    in

    (* install to the pipeline *)
    Pipeline.install_table_entry 
      stage
      10 
      keys
      acn
      acn_cid
      vaction_const_args
      target_pipe
    ; 
    V(vtup [] Span.default) (* no return *)
  | _ ->
    install_error "Incorrect number of arguments to Table.install"



(* Table.install_ternary : Table.t<<'k, iarg, marg, ret>> -> 'k -> 'k -> (iarg -> marg -> ret) -> iarg -> () *)
let install_ternary_name = "install_ternary"
let install_ternary_id = Id.create install_ternary_name
let install_ternary_cid = Cid.create_ids [id; install_ternary_id]
let install_ternary_error msg = module_error install_ternary_name msg

let install_ternary_ty = 
  let key_rty, iarg_rty, marg_rty, ret_rty = 
    fresh_rawty "table_key_ty",
    fresh_rawty "table_iarg_ty",
    fresh_rawty "table_marg_ty",
    fresh_rawty "table_ret_ty"
  in
  let mask_rawty = key_rty in
  let acn_rty = taction iarg_rty marg_rty ret_rty in  
  let start_eff = FVar (QVar (Id.fresh "eff")) in (* effect where this call begins *)
  (* table argument *)
  let tbl_eff = FVar (QVar (Id.fresh "eff")) in (* effect attached to table arg *)
  let tbl_t = ty_eff (fresh_t (key_rty, iarg_rty, marg_rty, ret_rty)) tbl_eff in
  let arg_tys = [
      tbl_t; 
      ty @@ key_rty;
      ty @@ mask_rawty;
      ty @@ acn_rty;
      ty @@ iarg_rty
    ]
  in
  let ret_ty = ty @@ TVoid in
  ty @@ TFun
    { arg_tys
    ; ret_ty
    ; start_eff
    ; end_eff = FSucc tbl_eff
    ; constraints = ref [CLeq (start_eff, tbl_eff)]
    }
;;

(* install an exact pattern, with key value equal to mask *)
let install_ternary_fun nst swid args =
  let _, _ = nst, swid in
  let open CoreSyntax in
  match args with
  | [vtbl; vkey; vmask; vaction; vaction_const_arg_tup] -> 
    let target_pipe = nst.switches.(swid).pipeline in    
    let stage = match (extract_ival vtbl).v with 
      | VGlobal(_, stage) -> stage
      | _-> error "Table.install: table arg didn't eval to a global"
    in

    let keys = flatten_v (extract_ival vkey).v in 
    let masks = flatten_v (extract_ival vmask).v in
    if (List.length keys <> List.length masks) then 
      error "Table.install_ternary: key and mask lists are not the same length";
    let pat_tuple_keys = List.map2 
      (fun k m -> CoreSyntax.VTuple([k; m])) 
      keys 
      masks 
    in
    let keys = List.map value pat_tuple_keys in 
    let vaction_const_args = match vaction_const_arg_tup with 
      | V({v=VTuple(vs)}) -> List.map CoreSyntax.value vs
      | V(v) -> [v]
      | _ -> error "Table.create: expected a tuple for the default action args"
    in
    let acn_cid, acn_ctor = 
      ival_fcn_to_internal_action nst swid vaction
    in
    let acn remaining_args = 
      acn_ctor (vaction_const_args@remaining_args)
    in
    (* install to the pipeline *)
    Pipeline.install_table_entry 
      stage
      10 
      keys
      acn
      acn_cid
      vaction_const_args
      target_pipe
    ; 
    V(vtup [] Span.default) (* no return *)
  | _ ->
    install_error "Incorrect number of arguments to Table.install"



(* Table.lookup *)
let lookup_name = "lookup"
let lookup_id = Id.create lookup_name
let lookup_cid = Cid.create_ids [id; lookup_id]
let lookup_error msg = module_error lookup_name msg
(* Table.lookup : Table.t<<'k, iarg, marg, ret>> -> 'k -> iarg -> ret *)
let lookup_ty = 
  let key_rty, iarg_rty, marg_rty, ret_rty = 
    fresh_rawty "table_key_ty",
    fresh_rawty "table_iarg_ty",
    fresh_rawty "table_marg_ty",
    fresh_rawty "table_ret_ty"
  in
  let start_eff = FVar (QVar (Id.fresh "eff")) in (* effect where this call begins *)
  let tbl_eff = FVar (QVar (Id.fresh "eff")) in (* effect attached to table arg *)
  let tbl_t = ty_eff (fresh_t (key_rty, iarg_rty, marg_rty, ret_rty)) tbl_eff in
  let arg_tys = [
      tbl_t; 
      ty @@ key_rty;
      ty @@ marg_rty
    ]
  in
  let ret_ty = ty ret_rty in 
  ty @@ TFun
    { arg_tys
    ; ret_ty
    ; start_eff
    ; end_eff = FSucc tbl_eff
    ; constraints = ref [CLeq (start_eff, tbl_eff)]
    }
;;

let lookup_fun nst swid args =
  let _, _ = nst, swid in
  let open InterpSyntax in
  let open CoreSyntax in
  match args with
  | [V { v = VGlobal(_, tbl_pos); }; V { v = vkey }; V { v = vargs }] ->  
    let keys = flatten_v vkey |> List.map value in  
    (* get all the entries from the table *)
    let default, entries = Pipeline.get_table_entries tbl_pos nst.switches.(swid).pipeline in
    (* find the first matching case *)
    let fst_match =
      List.fold_left
        (fun fst_match (pat_vs, eaction) ->
          (* print_endline ("checking entry: "
            ^(CorePrinting.comma_sep CorePrinting.value_to_string pat_vs)); *)
          match fst_match with
          | None ->
            if matches_pat_vals keys pat_vs
              then 
                (
                  (* print_endline ("pat vals match"); *)
                Some eaction) 
              else None
          | Some _ -> fst_match)
        None
        entries
    in
    (* if there's no matching entry, use the default action. *)
    let acn =
      match fst_match with
      | Some (acn) -> acn
      | None -> default
    in
    (* call the action function *)
    let arg_values =match vargs with 
      | VTuple(arg_vs) -> List.map CoreSyntax.value arg_vs 
      | v -> [CoreSyntax.value v]
    in
    let ret_values = acn arg_values in
    let ret_vs = List.map (fun (valu: CoreSyntax.value) -> valu.v) ret_values in
    (* wrap the return values in a tuple, if more than 1 return *)
    if (List.length ret_vs = 1) then (InterpSwitch.V (value_sp (List.hd ret_vs) Span.default))
    else 
      InterpSwitch.V(vtup ret_vs Span.default)
  | _ ->
    lookup_error "Incorrect number or type of arguments to Table.lookup"


let constructors = [create_id, create_sig]

let defs : global_fun list =
  [
    { cid = install_cid; body = install_fun; ty = install_ty };
    { cid = install_ternary_cid; body = install_ternary_fun; ty = install_ternary_ty };
    { cid = lookup_cid; body = lookup_fun; ty = lookup_ty }  
  ]
;;

let signature =  
  let key_rty, iarg_rty, marg_rty, ret_rty = 
    fresh_rawty "table_key_ty",
    fresh_rawty "table_iarg_ty",
    fresh_rawty "table_marg_ty",
    fresh_rawty "table_ret_ty"
  in
  LibraryInterface.tup_to_sigty
  ( module_id
  , [Cid.last_id t_id, [], ty (fresh_t (key_rty, iarg_rty, marg_rty, ret_rty))]
  , defs
  , constructors )
;;


(*** helpers to convert to old syntax for tofino backend ***)
let function_cids = 
  (fst (List.split constructors))@(List.map 
    (fun (def: InterpSwitch.global_fun ) -> def.cid) 
    (defs)
  )
;;

let is_table_lookup cid = Cid.equal cid lookup_cid

let rec flatten_core_exp (exp: CoreSyntax.exp) = match exp.e with 
  | ETuple(es) -> List.map flatten_core_exp es |> List.flatten
  | ERecord(label_exps) -> List.map (fun (_, exp) -> flatten_core_exp exp) label_exps |> List.flatten
  | _ -> [exp]
;;

let is_tbl_ty (raw_ty : CoreSyntax.raw_ty) = 
  match raw_ty with 
  | TName(cid, _) -> Cid.equal cid t_id
  | _ -> false
;;

type core_tbl_ty = 
  { tkey_sizes : CoreSyntax.size list
  (* ; tinstallparam_tys : CoreSyntax.ty list *)
  ; tparam_tys : CoreSyntax.ty list
  ; tret_tys : CoreSyntax.ty list
  }
;;

type core_tbl_def = 
{ tid : CoreSyntax.id (* for convenience *)
; tty : CoreSyntax.ty
; tactions : CoreSyntax. exp list
; tsize : CoreSyntax.exp
; tdefault : CoreSyntax.cid * CoreSyntax.exp list
}

type core_tbl_match = 
{
  tbl : CoreSyntax.exp; 
  keys : CoreSyntax.exp list;
  args : CoreSyntax.exp list;
  outs : Id.t list; 
  out_tys : CoreSyntax.ty list option;
}

let size_ints (sz : CoreSyntax.size) = match sz with 
  | Sz sz -> [sz]
  | Szs szs -> szs
;;
let size_to_ty (sz: int) = (CoreSyntax.ty) @@ TInt(CoreSyntax.Sz sz) ;;

let tname_to_ttable (rty : CoreSyntax.raw_ty) : core_tbl_ty =
  match rty with
  | TName(cid, [key_sizes; _; param_sizes; ret_sizes])
  | TName(cid, [key_sizes; param_sizes; ret_sizes]) -> 
    if (Cid.equal cid t_id) then (
      (* tofino compiler actually doesn't need install param sizes -- it infers them. *)
    (* let install_tys = List.map size_to_ty (size_ints install_param_sizes) in *)
    let param_tys = List.map size_to_ty (size_ints param_sizes) in
    let ret_tys = List.map size_to_ty (size_ints ret_sizes) in
    { 
      tkey_sizes = List.map (fun sz -> (CoreSyntax.Sz sz)) (size_ints key_sizes); 
      (* tinstallparam_tys = install_tys; *)
      tparam_tys = param_tys;
      tret_tys = ret_tys;
    })
    else 
      error@@"[tname_to_ttable] table does not have expected type >>"^(CorePrinting.raw_ty_to_string rty)^"<<"
  | _ ->     
    error@@"[tname_to_ttable] table does not have expected type >>"^(CorePrinting.raw_ty_to_string rty)^"<<"
;;


let dglobal_params_to_tbl_def tid (exp : CoreSyntax.exp) : core_tbl_def = 
  match exp.e with 
  | CoreSyntax.ECall(_, [size; actions_tup; default_acn_ctor; default_arg], _) -> (
    let default_args = flatten_core_exp default_arg in 
    let default_cid  = match default_acn_ctor.e with 
      | EVar(cid) -> cid
      | _ -> error "[Tables.dglobal_params_to_tbl_def] unexpected: default action argument is not an evar"
    in
    match actions_tup.e with
    | ETuple(actions) ->
      let tsize = size in
      let tactions = actions in
      let tdefault = (default_cid, default_args) in
      let tty = exp.ety in
      {tid; tty; tactions; tsize; tdefault}
    | _ -> 
      let tsize = size in
      let tactions = [actions_tup] in
      let tdefault = (default_cid, default_args) in
      let tty = exp.ety in
      {tid; tty; tactions; tsize; tdefault}
  )
  | _ -> error@@"got invalid table declaration: "^(CorePrinting.exp_to_string exp)
;;

let tbl_def_to_econstr (tbl_def : core_tbl_def) : CoreSyntax.exp = 
  let open CoreSyntax in
  let {tty; tactions; tsize; tdefault} = tbl_def in
  let actions_tup = {CoreSyntax.e = CoreSyntax.ETuple(tactions); ety = tty; espan = Span.default} in
  let default_action = {CoreSyntax.e = EVar(fst tdefault); ety = (List.hd tactions).ety; espan = Span.default} in
  let default_arg = CoreSyntax.tup_sp (snd tdefault) Span.default in
  let exp = {CoreSyntax.e = ECall(create_id, [tsize; actions_tup; default_action; default_arg], false); ety = tty; espan = Span.default} in
  exp
;;


(* translate a coresyntax statement into a table match command *)
let s_to_tbl_match (s : CoreSyntax.s) : core_tbl_match = 
  match s with 
    | CoreSyntax.STupleAssign({ids; tys; exp={e=CoreSyntax.ECall(_, [tbl; keys_exp; args_exp], _)}}) -> 
      let keys = flatten_core_exp keys_exp in
      let args = flatten_core_exp args_exp in
      let outs = ids in
      let out_tys = tys in
      {tbl; keys; args; outs; out_tys}      
    | CoreSyntax.SNoop -> 
      error "[s_to_tbl_match] called on a noop?"
    | _ -> 
      
      error @@ "[Table.s_to_tbl_match] only a STupleAssign can be translated into a table match, got:\n"^(CorePrinting.s_to_string s)
;;

let s_to_tbl_match_opt (s : CoreSyntax.s) : core_tbl_match option = 
  try Some(s_to_tbl_match s) with _ -> None
;;


let tbl_match_to_s ({tbl; keys; args; outs; out_tys} : core_tbl_match) : CoreSyntax.s = 
  let keys_exp = CoreSyntax.tup_sp keys Span.default in
  let args_exp = CoreSyntax.tup_sp args Span.default in    
  let ids = outs in
  let tys = out_tys in
  (* BUG: the tofino backend assumes that Table.match only appears in a TupleAssign statements. *)
  let exp = {CoreSyntax.e =CoreSyntax.ECall(lookup_cid, [tbl; keys_exp; args_exp], false); ety = tbl.ety; espan = Span.default} in
  CoreSyntax.STupleAssign({ids; tys; exp})
;;
