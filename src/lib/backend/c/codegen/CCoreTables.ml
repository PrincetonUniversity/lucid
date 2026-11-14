(* implementation of Tables and TernaryTables in CCore 
  Implements, for the builtin types Table.t and TernaryTable.t: 

    1. Type TBuiltin(Cid["TernaryTable"; "t"], [key_type; const_arg_type; arg_type; result_type]);

    2. Constructor TernaryTable.create(length, actions, default_action, default_action_const_arg);

    3. Method TernaryTable.lookup(Table_t, key, arg) -> result;

    4. Method TernaryTable.install(Table_t, key, mask, prio, action, const_arg);

    5. Method TernaryTable.update(Table_t, key, mask, prio, action, const_arg);

    6. Method TernaryTable.remove(Table_t, key, mask, prio);

  A table is an array of entries, sorted by priority (highest first) and then 
  by install order, with no gaps. A lookup uses the first entry that matches. 

  A Table (an exact table) is implemented as a ternary table: its entries all 
  have a full mask and the same priority. 
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
      cid"key", key_ty;
      cid"mask", key_ty;
      cid"prio", tint 32;
      cid"action_tag", tenum_member;
      cid"action_arg", const_action_arg_ty;      
    ])
;;

let table_cell tbl_id key_param mask_param prio_param action_param const_arg_param =
  let exp = erecord [
          cid "key", key_param; 
          cid "mask", mask_param;
          cid "prio", prio_param;
          cid "action_tag", action_param; 
          cid "action_arg", const_arg_param]
  in
  {exp with ety=table_cell_type tbl_id key_param.ety const_arg_param.ety;}
;;

(* all-ones constant of a scalar type *)
let ones_scalar ty =
  match (base_type ty).raw_ty with
  | TBool -> vbool true
  | TInt sz -> vbig (Z.pred (Z.shift_left Z.one sz)) sz
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
    | TList(ele_ty, n) ->
      elist (List.init n (fun _ -> ones_exp ele_ty))
    | _ -> failwith "[ones_exp] unsupported table key type"
  in
  {e with ety=ty}
;;

let table_instance_type tbl_id const_action_arg_ty tbl_cell_ty tbl_len =
  (* a table is a struct variable with a default, a list of entries, and max_idx, 
     the number of entries. The entries are entries[0] ... entries[max_idx - 1], 
     in the order that a lookup checks them. *)
  (* tref@@ *) (* no longer a tref *)
  tabstract_cid
    (Cid.str_cons_plain "ty" tbl_id)
    (
      trecord
        [cid "_default", trecord [cid "action_tag", tenum_member; 
                                cid "action_arg", const_action_arg_ty];
        cid "max_idx", tint 32;
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
    cid"max_idx", eval (vint 0 32);
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
  (* per-entry: if (masked_eq) { switch (tag) {..return action(..)..} }
     the entries are in priority order, so the first hit is the entry to use: 
     a hit returns from inside the loop. the switch runs only on a hit, not 
     once per entry scanned. the scan ends at max_idx, the end of the entries 
     (past it, a zeroed slot has mask = 0, which matches every key). actions 
     are pure, so running the default action only after the scan misses 
     (below) is equivalent to running it up front. *)
  let apply_branch entry action_tag =
    let action_evar = efunref ((untag action_tag)) action_ty in
    case tenum_member
    action_tag
      (sret (action_evar /** [entry/.cid"action_arg"; arg_param]))
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
  let s_loop =
    sfor (cid "_idx") spec.len
      (
        let entry = (tbl/.cid"entries")/@idx in
        let hit = compound_masked_eq key_param (entry/.cid"key") (entry/.cid"mask") in
        stmts [
          sif (eop Geq [idx; tbl/.cid"max_idx"]) s_apply_default snoop;
          sif hit
            (smatch [(entry/.cid"action_tag")]
              (List.map (apply_branch entry) action_tags))
            snoop]
      )
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

(*** install, update, and remove ***)

(* the entry at _idx, inside of a loop over a table's entries *)
let idx_id = cid "_idx"
let idx = evar idx_id (tint 32)
let entry_at_idx spec = 
  let tbl = evar (spec.tbl_id) (spec.tbl_ty) in
  (tbl/.cid"entries")/@idx
;;

(* an entry is identified by its priority, its mask, and its masked key *)
let is_entry entry key mask prio =
  emacro_and_fold [
    (entry/.cid"prio") /== prio;
    compound_eq (entry/.cid"mask") mask;
    compound_masked_eq (entry/.cid"key") key mask]
;;

(* a statement that runs s for each entry of the table: 
   for (_idx = 0; _idx < len; _idx++) { if (_idx < max_idx) { s } } *)
let foreach_entry spec s = 
  let tbl = evar (spec.tbl_id) (spec.tbl_ty) in
  sfor idx_id spec.len (sif (eop Less [idx; tbl/.cid"max_idx"]) s snoop)
;;

(* A table warns on stderr the first time that it rejects an operation: 
   an install when it is full, or an update or remove of an entry that it 
   does not have. Rejected installs are also counted, for the driver to 
   report when the program exits. *)
let table_stats = dforiegn {|
/* installs that a full table rejected */
static uint64_t table_full_installs = 0;
static void report_table_stats(void) {
    if (table_full_installs)
        fprintf(stderr, "table installs rejected (table full): %llu\n", (unsigned long long)table_full_installs);
}|}
;;

let warn_id op tbl_id = Cid.str_cons_plain ("warn_"^op) tbl_id ;;
let table_warn spec op count msg = 
  dfun_foriegn (warn_id op spec.tbl_id) tunit []
    (Printf.sprintf "{\n  static int warned = 0;%s\n  if (!warned) { warned = 1; fprintf(stderr, \"warning: table %s %s\\n\"); }\n}"
      (if count then "\n  table_full_installs++;" else "")
      (Cid.name spec.tbl_id) 
      msg)
;;
let swarn spec op = sunit ((efunref (warn_id op spec.tbl_id) (tfun [] tunit)) /** []) ;;

(* TernaryTable.install(Table_t, key, mask, prio, action, const_arg) *)
let install_id tbl_id = Cid.str_cons_plain "install" tbl_id ;;
let table_install spec = 
  (* note: the table is hard coded into the function, not a parameter. *)
  let tbl = (* ederef *) (evar (spec.tbl_id) (spec.tbl_ty)) in
  let key_param = evar (cid "key") spec.key_ty in
  let mask_param = evar (cid "mask") spec.key_ty in
  let prio_param = evar (cid "prio") (tint 32) in
  (* note: call has to be transformed from an action variable to an action tag value *)
  let action_param = evar (cid "action") tenum_member in
  let const_arg_param = evar (cid "const_arg") spec.const_arg_ty in
  let entry = entry_at_idx spec in
  let cell_ty = table_cell_type spec.tbl_id spec.key_ty spec.const_arg_ty in
  (* an entry that is already in the table stays as it is *)
  let s_find = foreach_entry spec 
    (sif (is_entry entry key_param mask_param prio_param) sret_none snoop)
  in
  let s_full = sif ((tbl/.cid"max_idx") /== eval (vint spec.len 32))
    (stmts [swarn spec "full"; sret_none])
    snoop
  in
  (* the new entry goes before the first entry with a lower priority, i.e., 
     after the entries with the same priority. From there on, each slot takes 
     the entry that was in the slot before it. 
     "carry" is the entry to put into the next slot. *)
  let carry_id = cid "carry" in
  let carry = evar carry_id cell_ty in
  let tmp_id = cid "tmp" in
  let s_insert = stmts [
    slocal carry_id cell_ty 
      (table_cell spec.tbl_id key_param mask_param prio_param action_param const_arg_param);
    sfor idx_id spec.len (stmts [
      sif (emacro_and_fold [
            eop Less [idx; tbl/.cid"max_idx"]; 
            eop Less [entry/.cid"prio"; prio_param]])
        (stmts [
          slocal tmp_id cell_ty entry;
          (tbl/.cid"entries", idx)/<-carry;
          sassign carry_id (evar tmp_id cell_ty)])
        snoop;
      sif (idx /== (tbl/.cid"max_idx"))
        (stmts [
          (tbl/.cid"entries", idx)/<-carry;
          sassign_exp (tbl/.cid"max_idx") (idx /+ eval (vint 1 32));
          sret_none])
        snoop])]
  in
  let params = List.map extract_evar 
    [key_param; mask_param; prio_param; action_param; const_arg_param]
  in
  dfun 
    (install_id spec.tbl_id)
    (tunit)
    params
    (stmts [s_find; s_full; s_insert; sret_none])
;;

(* TernaryTable.update(Table_t, key, mask, prio, action, const_arg) *)
let update_id tbl_id = Cid.str_cons_plain "update" tbl_id ;;
let table_update spec = 
  let key_param = evar (cid "key") spec.key_ty in
  let mask_param = evar (cid "mask") spec.key_ty in
  let prio_param = evar (cid "prio") (tint 32) in
  let action_param = evar (cid "action") tenum_member in
  let const_arg_param = evar (cid "const_arg") spec.const_arg_ty in
  let entry = entry_at_idx spec in
  (* replace the action of the entry and return; fall through if there is no such entry *)
  let s_update = foreach_entry spec 
    (sif (is_entry entry key_param mask_param prio_param) 
      (stmts [
        sassign_exp (entry/.cid"action_tag") action_param;
        sassign_exp (entry/.cid"action_arg") const_arg_param;
        sret_none])
      snoop)
  in
  let params = List.map extract_evar 
    [key_param; mask_param; prio_param; action_param; const_arg_param]
  in
  dfun 
    (update_id spec.tbl_id)
    (tunit)
    params
    (stmts [s_update; swarn spec "update"; sret_none])
;;

(* TernaryTable.remove(Table_t, key, mask, prio) *)
let remove_id tbl_id = Cid.str_cons_plain "remove" tbl_id ;;
let table_remove spec = 
  let tbl = (* ederef *) (evar (spec.tbl_id) (spec.tbl_ty)) in
  let key_param = evar (cid "key") spec.key_ty in
  let mask_param = evar (cid "mask") spec.key_ty in
  let prio_param = evar (cid "prio") (tint 32) in
  let entry = entry_at_idx spec in
  let next_idx = idx /+ eval (vint 1 32) in
  let removed_id = cid "removed" in
  let removed = evar removed_id tbool in
  (* from the removed entry on, each slot takes the entry that is in the slot after it *)
  let s_remove = stmts [
    slocal removed_id tbool (eval (vbool false));
    foreach_entry spec (stmts [
      sif (is_entry entry key_param mask_param prio_param) 
        (sassign removed_id (eval (vbool true)))
        snoop;
      sif (emacro_and_fold [removed; eop Less [next_idx; tbl/.cid"max_idx"]])
        ((tbl/.cid"entries", idx)/<-((tbl/.cid"entries")/@next_idx))
        snoop]);
    sif removed
      (stmts [
        sassign_exp (tbl/.cid"max_idx") ((tbl/.cid"max_idx") /- eval (vint 1 32));
        sret_none])
      snoop]
  in
  let params = List.map extract_evar 
    [key_param; mask_param; prio_param]
  in
  dfun 
    (remove_id spec.tbl_id)
    (tunit)
    params
    (stmts [s_remove; swarn spec "remove"; sret_none])
;;

let monomorphic_table_decls action_tags decl : decls = 
  match decl.d with 
  | DVar(tbl_id, builtin_tbl_ty, Some(builtin_constr_call_exp)) 
    when is_tbuiltin Tables.t_id builtin_tbl_ty || is_tbuiltin TernaryTables.t_id builtin_tbl_ty -> 
    let key_ty, const_arg_ty, arg_ty, ret_ty = 
      match extract_tbuiltin builtin_tbl_ty with 
      | _, [key_ty; const_arg_ty; arg_ty; ret_ty] -> key_ty, const_arg_ty, arg_ty, ret_ty
      | _, _ -> failwith "unexpected type"      
    in
    (* destructure the create(len, actions, default, default_arg) call:
       len and the default action reduce to constants; the default's arg stays
       an expression and is embedded in the initializer as-is *)
    let len, default_action_enum_id, default_action_arg = match extract_ecall builtin_constr_call_exp |> snd with
      | [len; _; default_action; default_action_arg] ->
        eval_exp len |> extract_vint,
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
      table_warn tbl_spec "full" true "is full, an entry was not installed";
      table_warn tbl_spec "update" false "has no such entry to update";
      table_warn tbl_spec "remove" false "has no such entry to remove";
      table_install tbl_spec;                   (* table install function *)
      table_update tbl_spec;                    (* table update function *)
      table_remove tbl_spec;                    (* table remove function *)
      table_lookup tbl_spec                     (* table lookup function *)
      ] 
    in
    new_decls
  | _ -> [decl]
;;

(* the arguments of a call to a Table function, as arguments to the 
   TernaryTable function of the same name: an exact entry is a ternary 
   entry with a full mask, and every exact entry has the same priority. *)
let exact_to_ternary_args fname (args : exp list) = 
  let prio = eval (vint Tables.exact_priority 32) in
  match fname, args with 
  | ("install" | "update"), [key; action; const_arg] -> 
    [key; ones_exp key.ety; prio; action; const_arg]
  | "remove", [key] -> [key; ones_exp key.ety; prio]
  | _ -> args
;;

let monomorphic_table_calls = 
  let table_fun_cids = List.map InterpSwitch.gfun_cid 
    (Tables.signature.m_funs@TernaryTables.signature.m_funs) 
  in
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
          let args = List.tl args in
          let fun_id, args = match (Cid.names f_cid) with 
            | ["Table"; fname] -> Cid.str_cons_plain fname tbl_id, exact_to_ternary_args fname args
            | ["TernaryTable"; fname] -> Cid.str_cons_plain fname tbl_id, args
            | _ -> failwith "unexpected table method"
          in
          let f_ety = match f.ety.raw_ty with 
            TFun{ret_ty; func_kind} -> 
              let arg_tys = List.map (fun (arg : exp) -> arg.ety) args in
              {f.ety with raw_ty=TFun{arg_tys; ret_ty; func_kind}}
            | _ -> failwith "unexpected type"
          in
          let f = efunref (fun_id) f_ety in
          {exp with e=ECall{f; args; call_kind=CFun}}
        )
        else
          super#visit_exp () exp
      | _ -> super#visit_exp () exp
  end
;; 
let process decls = 
  (* the drivers report the table stats, in a program with or without tables *)
  let decls = table_stats::decls in
  if (List.filter_map extract_daction_id_opt decls) = [] 
  then decls
  else
    let decls, action_tags = defunctionalize_actions decls in
    (* declare the tag constants: *)
    let decls = (denum (List.mapi (fun i t -> (t, i)) action_tags))::decls in 
    let decls = List.flatten (List.map (monomorphic_table_decls action_tags) decls) in
    let decls = monomorphic_table_calls#visit_decls () decls in
    decls
;;
