(* implementation of Tables in CCore 
  Implements: 

    1. Type TBuiltin(Cid["Table"; "t"], [key_type; const_arg_type; arg_type; result_type]);

    2. Constructor Table.create(length, actions, default_action, default_action_const_arg);

    3. Method Table.lookup(Table_t, key, arg) -> result;

    4. Method Table.install(Table_t, key, action, const_arg);

*)

open CCoreSyntax
open CCoreUtils

[@@@ocaml.warning "-21"]
[@@@ocaml.warning "-27"]

(*** defunctionalize actions ***)
let tag cid = Cid.str_cons_plain "tag" cid
let untag cid = Cid.tl cid 

let defunctionalize_actions decls = 
  (* collect the action tag constants (declared as a DEnum by the caller), then
     go through the program and replace every action variable with its tag --
     a VSymbol at the tag type (a plain int), which prints as the enum member *)
  let action_ids = List.filter_map extract_daction_id_opt decls in 
  let action_tags = List.map tag action_ids in
  (* replace action variables with action tag constants *)
  let action_var_replacer = 
    object 
      inherit [_] s_map as super
      method! visit_exp () exp = 
        try 
          let var_id, _ = extract_evar exp in
          if (List.mem var_id action_ids) then 
            eval (venum (tag var_id) tenum_member)
          else
            super#visit_exp () exp  
        with _ -> super#visit_exp () exp 
    end
  in
  let decls = action_var_replacer#visit_decls () decls in
  (* return the tag names for use later *)
  decls, action_tags
;;

(*** generate monomorphic table types and functions from Builtins ***)

(* All the information we need about a table to generate its methods. 
   This can be derived from the declaration. *)
type table_spec = 
{
  tbl_id : cid; 
  len    : arrlen;
  tbl_ty : ty;  
    key_ty : ty;
    const_arg_ty : ty;
    arg_ty : ty; 
    ret_ty : ty;
  action_tags : cid list;
}
let table_cell_type tbl_id key_ty const_action_arg_ty : ty = 
  let tblcellty_id = Cid.str_cons_plain "cellty" tbl_id in
  tabstract_cid 
    tblcellty_id
    (trecord [
      cid"valid", tbool;
      cid"key", key_ty;
      cid"mask", key_ty;
      cid"action_tag", tenum_member;
      cid"action_arg", const_action_arg_ty;      
    ])
;;

let table_cell tbl_id key_param mask_param action_param const_arg_param =
  let exp = erecord [
         cid "valid", eval (vbool true); 
          cid "key", key_param; 
          cid "mask", mask_param;
          cid "action_tag", action_param; 
          cid "action_arg", const_arg_param]
  in
  {exp with ety=table_cell_type tbl_id key_param.ety const_arg_param.ety;}
;;

(* all-ones constant of a scalar type; -1 converts to all-ones in C for
   widths too wide for an OCaml int *)
let ones_scalar ty =
  match (base_type ty).raw_ty with
  | TBool -> vbool true
  | TInt sz when sz >= 63 -> vint (-1) sz
  | TInt sz -> vint ((1 lsl sz) - 1) sz
  | _ -> failwith "[ones_scalar] expected a scalar type"
;;

(* all-ones expression of a key type: the exact-match mask.
   (k & m) == (k' & m) iff k == k' when every bit of m is set. *)
let rec ones_exp ty : exp =
  let e = match (base_type ty).raw_ty with
    | TBool | TInt _ -> eval (ones_scalar ty)
    | TRecord(labels, tys) ->
      erecord (List.map2 (fun l t -> l, ones_exp t) labels tys)
    | TTuple tys -> etuple (List.map ones_exp tys)
    | TList(ele_ty, IConst n) ->
      elist (List.init n (fun _ -> ones_exp ele_ty))
    | _ -> failwith "[ones_exp] unsupported table key type"
  in
  {e with ety=ty}
;;

let table_instance_type tbl_id const_action_arg_ty tbl_cell_ty tbl_len =
  (* a table is a struct variable with a default and a list of entries *)
  (* tref@@ *) (* no longer a tref *)
  tabstract_cid
    (Cid.str_cons_plain "ty" tbl_id)
    (
      trecord
        [cid "_default", trecord [cid "action_tag", tenum_member; 
                                cid "action_arg", const_action_arg_ty];
        cid "entries",tlist tbl_cell_ty tbl_len])
;;

(* the table's initializer expression (a constant record) *)
let table_create (tbl_ty : ty) (def_enum_id : cid) (def_arg : exp) =
  let fields, tys = extract_trecord tbl_ty in
  let field_ty = List.combine fields tys in
  let entries_ty = List.assoc (cid"entries") field_ty in
  let default = erecord [
    cid"action_tag", eval (venum def_enum_id tenum_member);
    cid"action_arg", def_arg
  ] in
  erecord [
    cid"_default", default;
    cid"entries", eval (zero_list entries_ty)
  ]
;;



(* tbl_lookup(tbl_ty t, key_ty k, arg_ty arg) *)
(* note: the length and the types are all part of tbl_ty, 
   but its easier to just pass them in from the 
   function builder. *)
let lookup_id tbl_id = Cid.str_cons_plain "lookup" tbl_id ;;
let table_lookup spec =
  let action_tags = spec.action_tags in
  (* note: the table is hard coded into the function, not a parameter. *)
  (* (so it doesn't need to be a ref) *)
  let tbl = (* ederef *) (evar (spec.tbl_id) (spec.tbl_ty)) in
  let key_param = evar (cid "key") spec.key_ty in
  let arg_param = evar (cid "arg") spec.arg_ty in
  (* reconstruct the action type for the match branches *)
  let action_ty = tfun_kind FAction [spec.const_arg_ty; spec.arg_ty] spec.ret_ty in
  let idx = evar (cid "_idx") (tint 32) in
  (* per-entry: if (valid && masked_eq) { switch (tag) {..return action(..)..} }
     the valid check excludes empty entries (a zeroed entry has mask = 0, which
     matches every key, and tag = 0, which is a real action's tag); the switch
     runs only on a hit, not once per entry scanned, and a hit returns from
     inside the loop. actions are pure, so running the default action only
     after the scan misses (below) is equivalent to running it up front. *)
  let apply_branch entry action_tag =
    let action_evar = efunref ((untag action_tag)) action_ty in
    case tenum_member
    action_tag
      (sret (action_evar /** [entry/.cid"action_arg"; arg_param]))
  in
  let s_loop =
    sfor (cid "_idx") spec.len
      (
        let entry = (tbl/.cid"entries")/@idx in
        let hit = emacro_and_fold [
          entry/.cid"valid";
          compound_masked_eq key_param (entry/.cid"key") (entry/.cid"mask")]
        in
        sif hit
          (smatch [(entry/.cid"action_tag")]
            (List.map (apply_branch entry) action_tags))
          snoop
      )
  in
  (* no entry matched: dispatch to the default action. the last action's arm
     is the switch's `default:` case, so every path through the function
     returns. *)
  let apply_default_branch is_last action_tag =
    let action_evar = efunref (untag action_tag) action_ty in
    let s_ret_default = sret (action_evar /** [tbl/.cid"_default"/.cid"action_arg"; arg_param]) in
    if is_last then ([PWild tenum_member], s_ret_default)
    else case tenum_member action_tag s_ret_default
  in
  let n_tags = List.length action_tags in
  let s_apply_default = smatch
    [(tbl/.cid"_default"/.cid"action_tag")]
    (List.mapi (fun i tag -> apply_default_branch (i = n_tags - 1) tag) action_tags)
  in
  let params = List.map extract_evar
    [key_param; arg_param]
  in
  dfun
    (lookup_id spec.tbl_id)
    spec.ret_ty
    params
    (stmts [
            s_loop;
            s_apply_default;])
;;

(* Table.install(Table_t, key, action, const_arg) *)
let install_id tbl_id = Cid.str_cons_plain "install" tbl_id ;;
let table_install spec = 
  (* note: the table is hard coded into the function, not a parameter. *)
  let tbl = (* ederef *) (evar (spec.tbl_id) (spec.tbl_ty)) in
  (* let tbl_param = evar (cid "tbl") spec.tbl_ty in *)
  let key_param = evar (cid "key") spec.key_ty in
  (* note: call has to be transformed from an action variable to an action tag value *)
  let action_param = evar (cid "action") tenum_member in
  let const_arg_param = evar (cid "const_arg") spec.const_arg_ty in
  (* exact match: mask = all ones (only the exact key matches) *)
  let mask = ones_exp spec.key_ty in
  let new_slot = table_cell spec.tbl_id key_param mask action_param const_arg_param in
  let idx = cid "_idx" in
  let idx_var = evar (idx) (tint 32) in
  let cont = cid "_continue" in
  let body = swhile idx spec.len cont
    (
      let entries = eop (Project(cid"entries")) [tbl] in
      let entry = elistget entries idx_var in
      (* let entry = (entries/@idx) in     *)
      sif (eop Eq [entry/.cid"valid";eval@@vbool false])
        (stmts [
            sassign (cont)  (eval (vbool false));
            (tbl/.cid"entries", idx_var)/<-new_slot;            
          ])
        snoop
    )
  in
  let params = List.map extract_evar 
    [key_param; action_param; const_arg_param]
  in
  let sret = sret_none in
  dfun 
    (install_id spec.tbl_id)
    (tunit)
    params
    (sseq body sret)
;;

(* Table.install_ternary(Table_t, key, mask, action, const_arg)  *)
(* LEFT OFF HERE. 
    decision: do we want ternary install to be a method of the table, 
    or do we want a new ternary table type?   
*)
let install_ternary_id tbl_id = Cid.str_cons_plain "install_ternary" tbl_id ;;
let table_ternary_install spec = 
  (* note: the table is hard coded into the function, not a parameter. *)
  let tbl = (* ederef *) (evar (spec.tbl_id) (spec.tbl_ty)) in
  (* let tbl_param = evar (cid "tbl") spec.tbl_ty in *)
  let key_param = evar (cid "key") spec.key_ty in
  let mask_param = evar (cid"mask") spec.key_ty in
  let action_param = evar (cid "action") tenum_member in 
  let const_arg_param = evar (cid "const_arg") spec.const_arg_ty in
  let new_slot = table_cell spec.tbl_id key_param mask_param action_param const_arg_param in 
  let idx = cid "_idx" in
  let idx_var = evar ( idx) (tint 32) in
  let cont = cid "_continue" in
  let body = swhile idx spec.len cont
    (
      let entries = eop (Project(cid"entries")) [tbl] in
      let entry = elistget entries idx_var in
      (* let entry = (entries/@idx) in     *)
      sif (eop Eq [entry/.cid"valid";eval@@vbool false])
        (stmts [
            sassign ( cont)  (eval (vbool false));
            (tbl/.cid"entries", idx_var)/<-new_slot;            
          ])
        snoop
    )
  in
  let params = List.map extract_evar
    [key_param; mask_param; action_param; const_arg_param]
  in
  let sret = sret_none in
  dfun 
    (install_ternary_id spec.tbl_id)
    (tunit)
    params
    (sseq body sret)
;;




let monomorphic_table_decls action_tags decl : decls = 
  match decl.d with 
  | DVar(tbl_id, builtin_tbl_ty, Some(builtin_constr_call_exp)) when is_tbuiltin Tables.t_id builtin_tbl_ty -> 
    let key_ty, const_arg_ty, arg_ty, ret_ty = 
      match extract_tbuiltin builtin_tbl_ty with 
      | _, [key_ty; const_arg_ty; arg_ty; ret_ty] -> key_ty, const_arg_ty, arg_ty, ret_ty
      | _, _ -> failwith "unexpected type"      
    in
    (* destructure the Table.create(len, actions, default, default_arg) call:
       len and the default action reduce to constants; the default's arg stays
       an expression and is embedded in the initializer as-is *)
    let len, default_action_enum_id, default_action_arg = match extract_ecall builtin_constr_call_exp |> snd with
      | [len; _; default_action; default_action_arg] ->
        eval_exp len |> extract_vint |> arrlen,
        eval_exp default_action |> extract_vsymbol, (* here, the id is the symbol in the enum *)
        default_action_arg
      | _ -> failwith "unexpected table declaration"
    in
    let tbl_cell_ty = table_cell_type tbl_id key_ty const_arg_ty in
    let tbl_ty = table_instance_type tbl_id const_arg_ty tbl_cell_ty len in

    let tbl_constructor = table_create tbl_ty default_action_enum_id default_action_arg in

    let tbl_spec = {tbl_id; len; tbl_ty; key_ty; const_arg_ty; arg_ty; ret_ty; action_tags;} in
    let new_decls = [
      decl_tabstract tbl_cell_ty;               (* cell type within a table *)
      decl_tabstract tbl_ty;                    (* the table's type *)
      dglobal tbl_id tbl_ty tbl_constructor; (* table declaration *)
      table_install tbl_spec;                   (* table install function *)
      table_ternary_install tbl_spec; 
      table_lookup tbl_spec                     (* table lookup function *)
      ] 
    in
    new_decls
  | _ -> [decl]
;;

let monomorphic_table_calls = 
  let table_fun_cids = List.map InterpSwitch.gfun_cid Tables.signature.m_funs in
  object 
    inherit [_] s_map as super

    method! visit_exp () exp = 
      match exp.e with 
      | ECall({f; args; call_kind=CFun}) -> 
        (* replace generic table function call with table-specific function call, 
           then remove the table argument *)
        let f_cid, _ = extract_evar f in
        if (List.mem f_cid table_fun_cids) then 
        (
          let tbl_id, _ = extract_evar (List.hd args) in 
          let fun_id = match (Cid.names f_cid) with 
            | ["Table";"install"] -> ( install_id tbl_id )
            | ["Table";"install_ternary"] -> ( install_ternary_id tbl_id )
            | ["Table";"lookup"] -> ( lookup_id tbl_id )
            | _ -> failwith "unexpected table method"
          in
          let f_ety = match f.ety.raw_ty with 
            TFun{arg_tys; ret_ty; func_kind} -> 
              let arg_tys = List.tl arg_tys in (* remove table arg *)
              {f.ety with raw_ty=TFun{arg_tys; ret_ty; func_kind}}
            | _ -> failwith "unexpected type"
          in
          let f = efunref (fun_id) f_ety in
          let args = List.tl args in
          {exp with e=ECall{f; args; call_kind=CFun}}
        )
        else
          super#visit_exp () exp
      | _ -> super#visit_exp () exp
  end
;; 
let process decls = 
  if (List.filter_map extract_daction_id_opt decls) = [] 
  then decls (* no actions, nothing to do here. *)
  else
    let decls, action_tags = defunctionalize_actions decls in
    (* declare the tag constants: enum members are integer constant
       expressions, so the tags are valid in case labels and in the tables'
       static initializers *)
    let decls = (denum (List.mapi (fun i t -> (t, i)) action_tags))::decls in 
    let decls = List.flatten (List.map (monomorphic_table_decls action_tags) decls) in
    let decls = monomorphic_table_calls#visit_decls () decls in
    decls
;;



(*
(*** tables with function pointers instead of defunctionalization ***)
type table_spec2 =
{
  tbl_id : cid;
  len    : arrlen;
  tbl_ty : ty;
    key_ty : ty;
    const_arg_ty : ty;
    arg_ty : ty;
    ret_ty : ty;
  acn_fty : ty; (* (const_arg_ty, arg_ty) -> ret_ty *)
}

let table_cell_type2 tbl_id key_ty acn_fty const_arg_ty : ty =
  let tblcellty_id = Cid.str_cons_plain "cellty" tbl_id in
  tabstract_cid
    tblcellty_id
    (trecord [
      cid"valid", tbool;
      cid"key", key_ty;
      cid"mask", key_ty;
      cid"action", acn_fty;
      cid"action_arg", const_arg_ty;
    ])
;;

let table_cell2 tbl_id key_param mask_param action_param const_arg_param =
  let exp = erecord [
         cid "valid", eval (vbool true);
          cid "key", key_param;
          cid "mask", mask_param;
          cid "action", action_param;
          cid "action_arg", const_arg_param]
  in
  {exp with ety=table_cell_type2 tbl_id key_param.ety action_param.ety const_arg_param.ety;}
;;

let table_instance_type2 tbl_id acn_fty const_action_arg_ty tbl_cell_ty tbl_len =
  tabstract_cid
    (Cid.str_cons_plain "ty" tbl_id)
    (
      trecord
        [cid "_default", trecord [cid "action", acn_fty;
                                cid "action_arg", const_action_arg_ty];
        cid "entries",tlist tbl_cell_ty tbl_len])
;;

(* the table's initializer expression. default_acn is the default action as a
   function-pointer value (a VSymbol holding the action's name at the action's
   type); def_arg is the default action's constant argument expression *)
let table_create2 (tbl_ty : ty) (default_acn : value) (def_arg : exp) =
  let fields, tys = extract_trecord tbl_ty in
  let field_ty = List.combine fields tys in
  let entries_ty = List.assoc (cid"entries") field_ty in
  let default = erecord [
    cid"action", eval default_acn;
    cid"action_arg", def_arg
  ] in
  erecord [
    cid"_default", default;
    cid"entries", eval (zero_list entries_ty)
  ]
;;

(* tbl_lookup(key_ty k, arg_ty arg):
    scan the entries; on the first valid entry whose masked key matches,
    return its action's result, calling through the stored pointer. if no
    entry matches, return the default action's result. (actions are pure,
    so deferring the default call to the miss path is equivalent to running
    it up front.)
    the pointer is bound to a local before the call because later passes
    (and the typer's printf case) assume every call target is a variable. *)
let table_lookup2 (spec : table_spec2) =
  (* note: the table is hard coded into the function, not a parameter. *)
  let tbl = evar (spec.tbl_id) (spec.tbl_ty) in
  let key_param = evar (cid "key") spec.key_ty in
  let arg_param = evar (cid "arg") spec.arg_ty in
  let idx = evar (cid "_idx") (tint 32) in
  let s_loop =
    sfor (cid "_idx") spec.len
      (
        let entry = (tbl/.cid"entries")/@idx in
        let hit = emacro_and_fold [
          entry/.cid"valid";
          compound_masked_eq key_param (entry/.cid"key") (entry/.cid"mask")]
        in
        let acn = evar (cid"_acn") spec.acn_fty in
        sif hit
          (stmts [
            (cid"_acn") /::= (entry/.cid"action");
            sret (acn /** [entry/.cid"action_arg"; arg_param]);])
          snoop
      )
  in
  (* no entry matched: return the default action's result *)
  let default_acn = evar (cid"_default_acn") spec.acn_fty in
  let s_default = stmts [
    (cid"_default_acn") /::= (tbl/.cid"_default"/.cid"action");
    sret (default_acn /** [tbl/.cid"_default"/.cid"action_arg"; arg_param]);
  ] in
  dfun
    (lookup_id spec.tbl_id)
    spec.ret_ty
    (List.map extract_evar [key_param; arg_param])
    (stmts [s_loop; s_default])
;;

(* Table.install(key, action, const_arg) -- action is a function pointer *)
let table_install2 (spec : table_spec2) =
  (* note: the table is hard coded into the function, not a parameter. *)
  let tbl = evar (spec.tbl_id) (spec.tbl_ty) in
  let key_param = evar (cid "key") spec.key_ty in
  let action_param = evar (cid "action") spec.acn_fty in
  let const_arg_param = evar (cid "const_arg") spec.const_arg_ty in
  (* exact match: mask = all ones (only the exact key matches) *)
  let mask = ones_exp spec.key_ty in
  let new_slot = table_cell2 spec.tbl_id key_param mask action_param const_arg_param in
  let idx = cid "_idx" in
  let idx_var = evar idx (tint 32) in
  let cont = cid "_continue" in
  let body = swhile idx spec.len cont
    (
      let entries = eop (Project(cid"entries")) [tbl] in
      let entry = elistget entries idx_var in
      sif (eop Eq [entry/.cid"valid";eval@@vbool false])
        (stmts [
            sassign (cont) (eval (vbool false));
            (tbl/.cid"entries", idx_var)/<-new_slot;
          ])
        snoop
    )
  in
  dfun
    (install_id spec.tbl_id)
    (tunit)
    (List.map extract_evar [key_param; action_param; const_arg_param])
    (sseq body sret_none)
;;

(* Table.install_ternary(key, mask, action, const_arg) *)
let table_ternary_install2 (spec : table_spec2) =
  let tbl = evar (spec.tbl_id) (spec.tbl_ty) in
  let key_param = evar (cid "key") spec.key_ty in
  let mask_param = evar (cid"mask") spec.key_ty in
  let action_param = evar (cid "action") spec.acn_fty in
  let const_arg_param = evar (cid "const_arg") spec.const_arg_ty in
  let new_slot = table_cell2 spec.tbl_id key_param mask_param action_param const_arg_param in
  let idx = cid "_idx" in
  let idx_var = evar idx (tint 32) in
  let cont = cid "_continue" in
  let body = swhile idx spec.len cont
    (
      let entries = eop (Project(cid"entries")) [tbl] in
      let entry = elistget entries idx_var in
      sif (eop Eq [entry/.cid"valid";eval@@vbool false])
        (stmts [
            sassign (cont) (eval (vbool false));
            (tbl/.cid"entries", idx_var)/<-new_slot;
          ])
        snoop
    )
  in
  dfun
    (install_ternary_id spec.tbl_id)
    (tunit)
    (List.map extract_evar [key_param; mask_param; action_param; const_arg_param])
    (sseq body sret_none)
;;

let monomorphic_table_decls2 decl : decls =
  match decl.d with
  | DVar(tbl_id, builtin_tbl_ty, Some(builtin_constr_call_exp)) when is_tbuiltin Tables.t_id builtin_tbl_ty ->
    let key_ty, const_arg_ty, arg_ty, ret_ty =
      match extract_tbuiltin builtin_tbl_ty with
      | _, [key_ty; const_arg_ty; arg_ty; ret_ty] -> key_ty, const_arg_ty, arg_ty, ret_ty
      | _, _ -> failwith "unexpected type"
    in
    let acn_fty = tfun_kind FAction [const_arg_ty; arg_ty] ret_ty in
    (* destructure the Table.create(len, actions, default, default_arg) call:
       the default action is an EVar reference to the action function (eval_exp
       turns it into a VSymbol holding the action's cid); the default's arg
       stays an expression and is embedded in the initializer as-is *)
    let len, default_action_cid, default_action_arg = match extract_ecall builtin_constr_call_exp |> snd with
      | [len; _; default_action; default_action_arg] ->
        eval_exp len |> extract_vint |> arrlen,
        eval_exp default_action |> extract_vsymbol,
        default_action_arg
      | _ -> failwith "unexpected table declaration"
    in
    let tbl_cell_ty = table_cell_type2 tbl_id key_ty acn_fty const_arg_ty in
    let tbl_ty = table_instance_type2 tbl_id acn_fty const_arg_ty tbl_cell_ty len in

    let tbl_constructor = table_create2 tbl_ty (vsymbol default_action_cid acn_fty) default_action_arg in

    let tbl_spec : table_spec2 = {tbl_id; len; tbl_ty; key_ty; const_arg_ty; arg_ty; ret_ty; acn_fty;} in
    [
      decl_tabstract tbl_cell_ty;               (* cell type within a table *)
      decl_tabstract tbl_ty;                    (* the table's type *)
      dglobal tbl_id tbl_ty tbl_constructor;    (* table declaration *)
      table_install2 tbl_spec;                  (* table install function *)
      table_ternary_install2 tbl_spec;
      table_lookup2 tbl_spec                    (* table lookup function *)
    ]
  | _ -> [decl]
;;

(* like process, but: no defunctionalization, no actions enum, and
   actions are stored / called as function pointers.
   monomorphic_table_calls is reused as-is: it only rewrites Table.*
   calls to the table-specific functions and drops the table argument;
   an action argument at an install site is already a function reference.
   note: calls must be rewritten *before* the table functions are generated --
   monomorphic_table_calls assumes every call target is a variable, but the
   generated lookup calls an action through a record projection. *)
let process2 decls =
  let decls = monomorphic_table_calls#visit_decls () decls in
  List.flatten (List.map monomorphic_table_decls2 decls)
;;
*)
