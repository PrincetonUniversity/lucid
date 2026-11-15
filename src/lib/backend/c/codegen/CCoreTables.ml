(* implementation of Tables and TernaryTables in CCore 
  Implements, for the builtin types Table.t and TernaryTable.t: 

    1. Type TBuiltin(Cid["TernaryTable"; "t"], [key_type; const_arg_type; arg_type; result_type]);

    2. Constructor TernaryTable.create(length, actions, default_action, default_action_const_arg);

    3. Method TernaryTable.lookup(Table_t, key, arg) -> result;

    4. Method TernaryTable.install(Table_t, key, mask, prio, action, const_arg);

    5. Method TernaryTable.update(Table_t, key, mask, prio, action, const_arg);

    6. Method TernaryTable.remove(Table_t, key, mask, prio);

  and the same for Table, whose install, update, and remove have no mask or prio.

  A ternary table is an array of entries, sorted by priority (highest first) 
  and then by install order. A lookup uses the first entry that matches. 

  An exact table (Table) is a hash table with open addressing: a key's entry 
  is in the first free slot at or after the key's home slot (its hash).
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

(*** running actions (in a lookup) ***)

(* run the action of an entry and return its result: 
   switch (entry.action_tag) { case tag: return action(entry.action_arg, arg); ... } *)
let s_apply_entry spec entry arg_param = 
  (* reconstruct the action type for the match branches *)
  let action_ty = tfun_kind FAction [spec.const_arg_ty; spec.arg_ty] spec.ret_ty in
  let apply_branch action_tag =
    let action_evar = efunref ((untag action_tag)) action_ty in
    case tenum_member
    action_tag
      (sret (action_evar /** [entry/.cid"action_arg"; arg_param]))
  in
  smatch [(entry/.cid"action_tag")] (List.map apply_branch spec.action_tags)
;;

(* no entry matched: dispatch to the default action. the last action's arm
   is the switch's `default:` case, so every path through the function
   returns. *)
let s_apply_default spec arg_param = 
  let tbl = evar (spec.tbl_id) (spec.tbl_ty) in
  let action_ty = tfun_kind FAction [spec.const_arg_ty; spec.arg_ty] spec.ret_ty in
  let apply_default_branch is_last action_tag =
    let action_evar = efunref (untag action_tag) action_ty in
    let s_ret_default = sret (action_evar /** [tbl/.cid"_default"/.cid"action_arg"; arg_param]) in
    if is_last then ([PWild tenum_member], s_ret_default)
    else case tenum_member action_tag s_ret_default
  in
  let n_tags = List.length spec.action_tags in
  smatch
    [(tbl/.cid"_default"/.cid"action_tag")]
    (List.mapi (fun i tag -> apply_default_branch (i = n_tags - 1) tag) spec.action_tags)
;;

(*** ternary tables ***)

(* tbl_lookup(tbl_ty t, key_ty k, arg_ty arg) *)
(* note: the length and the types are all part of tbl_ty, 
   but its easier to just pass them in from the 
   function builder. *)
let lookup_id tbl_id = Cid.str_cons_plain "lookup" tbl_id ;;
let table_lookup spec =
  (* note: the table is hard coded into the function, not a parameter. *)
  (* (so it doesn't need to be a ref) *)
  let tbl = (* ederef *) (evar (spec.tbl_id) (spec.tbl_ty)) in
  let key_param = evar (cid "key") spec.key_ty in
  let arg_param = evar (cid "arg") spec.arg_ty in
  let idx = evar (cid "_idx") (tint 32) in
  (* per-entry: if (masked_eq) { switch (tag) {..return action(..)..} }
     the entries are in priority order, so the first hit is the entry to use: 
     a hit returns from inside the loop. the switch runs only on a hit, not 
     once per entry scanned. the scan ends at max_idx, the end of the entries 
     (past it, a zeroed slot has mask = 0, which matches every key). actions 
     are pure, so running the default action only after the scan misses 
     (below) is equivalent to running it up front. *)
  let s_apply_default = s_apply_default spec arg_param in
  let s_loop =
    sfor (cid "_idx") spec.len
      (
        let entry = (tbl/.cid"entries")/@idx in
        let hit = compound_masked_eq key_param (entry/.cid"key") (entry/.cid"mask") in
        stmts [
          sif (eop Geq [idx; tbl/.cid"max_idx"]) s_apply_default snoop;
          sif hit (s_apply_entry spec entry arg_param) snoop]
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

(*** exact tables ***)

(* An exact table is a hash table with open addressing and linear probing. 
   The table has at least twice as many slots as its maximum number of 
   entries (spec.len) to keep contention low. *) 

(* power of 2 number of slots *)
let exact_capacity len = 
  let rec pow2 c = if c >= 2 * len then c else pow2 (2 * c) in
  pow2 1
;;

let exact_cell_type tbl_id key_ty const_action_arg_ty : ty = 
  let tblcellty_id = Cid.str_cons_plain "cellty" tbl_id in
  tabstract_cid 
    tblcellty_id
    (trecord [
      cid"valid", tbool;
      cid"key", key_ty;
      cid"action_tag", tenum_member;
      cid"action_arg", const_action_arg_ty;      
    ])
;;

let exact_cell tbl_id key_param action_param const_arg_param =
  let exp = erecord [
          cid "valid", eval (vbool true); 
          cid "key", key_param; 
          cid "action_tag", action_param; 
          cid "action_arg", const_arg_param]
  in
  {exp with ety=exact_cell_type tbl_id key_param.ety const_arg_param.ety;}
;;

let exact_table_type tbl_id const_action_arg_ty tbl_cell_ty tbl_len =
  (* an exact table is a struct variable with a default, the number of 
     entries, and a list of slots. A slot with valid = false is free. *)
  tabstract_cid
    (Cid.str_cons_plain "ty" tbl_id)
    (
      trecord
        [cid "_default", trecord [cid "action_tag", tenum_member; 
                                cid "action_arg", const_action_arg_ty];
        cid "num_entries", tint 32;
        cid "entries",tlist tbl_cell_ty (exact_capacity tbl_len)])
;;

(* the table's initializer expression (a constant record) *)
let exact_table_create (tbl_ty : ty) (def_enum_id : cid) (def_arg : exp) =
  let fields, tys = extract_trecord tbl_ty in
  let field_ty = List.combine fields tys in
  let entries_ty = List.assoc (cid"entries") field_ty in
  let default = erecord [
    cid"action_tag", eval (venum def_enum_id tenum_member);
    cid"action_arg", def_arg
  ] in
  erecord [
    cid"_default", default;
    cid"num_entries", eval (vint 0 32);
    cid"entries", eval (zero_list entries_ty)
  ]
;;

(* A simple hash function the internet recommended, based on MurmerHash3 *)
let table_hash_mix = dforiegn {|
/* mix a field of a key into the hash of a table key (the 64-bit finalizer of MurmurHash3) */
static inline uint64_t table_hash_mix(uint64_t h, uint64_t v) {
    h ^= v;
    h ^= h >> 33; h *= 0xff51afd7ed558ccdULL;
    h ^= h >> 33; h *= 0xc4ceb9fe1a85ec53ULL;
    h ^= h >> 33;
    return h;
}|}
;;

let rec key_fields (key : exp) : exp list = 
  match key.ety.raw_ty with 
  | TInt _ | TBool -> [key]
  | TRecord(ids, _) -> List.concat_map (fun id -> key_fields (key/.id)) ids
  | TTuple(tys) -> List.concat (List.mapi (fun i _ -> key_fields (key/.@i)) tys)
  | TList(_, n) -> List.concat (List.init n (fun i -> key_fields (key/@(eval@@vint i 32))))
  | TName(_, Some d) -> key_fields {key with ety=d}
  | _ -> failwith "[key_fields] unsupported table key type"
;;

(* hash_tbl(f0, f1, ...) : the home slot of the key (f0, f1, ...) *)
let hash_id tbl_id = Cid.str_cons_plain "hash" tbl_id ;;
let exact_hash_ty spec = 
  let field_tys = List.map (fun (field : exp) -> field.ety) (key_fields (evar (cid "key") spec.key_ty)) in
  tfun field_tys (tint 32)
;;
let exact_hash spec = 
  let field_tys, _, _ = extract_func_ty (exact_hash_ty spec) in
  let params = List.mapi (fun i fty -> cid ("f"^string_of_int i), fty) field_tys in
  let mixes = List.mapi 
    (fun i _ -> Printf.sprintf "  h = table_hash_mix(h, (uint64_t)f%d);" i) 
    field_tys 
  in
  dfun_foriegn (hash_id spec.tbl_id) (tint 32) params
    (Printf.sprintf "{\n  uint64_t h = 0;\n%s\n  return (uint32_t)h & %d;\n}" 
      (String.concat "\n" mixes) 
      (exact_capacity spec.len - 1))
;;
let ehash spec key = (efunref (hash_id spec.tbl_id) (exact_hash_ty spec)) /** (key_fields key) ;;

(* slot being probed and the entry it stores *)
let slot_id = cid "slot"
let slot = evar slot_id (tint 32)
let entry_at_slot spec = 
  let tbl = evar (spec.tbl_id) (spec.tbl_ty) in
  (tbl/.cid"entries")/@slot
;;
let slot_is_free spec = eop Eq [(entry_at_slot spec)/.cid"valid"; eval@@vbool false] ;;
let slot_has_key spec key = compound_eq ((entry_at_slot spec)/.cid"key") key ;;

(* statements to probe one slot s for a key
   home = hash(key); 
   for (_idx = 0; _idx < capacity; _idx++) { slot = (home + _idx) & (capacity - 1); s } 
   (s returns when it reaches the key's entry or a free slot) *)
let home_id = cid "home"
let home = evar home_id (tint 32)
let probe spec key s = 
  let capacity = exact_capacity spec.len in
  let slot_mask = eval (vint (capacity - 1) 32) in
  stmts [
    slocal home_id (tint 32) (ehash spec key);
    sfor idx_id capacity (stmts [
      slocal slot_id (tint 32) ((home /+ idx) /& slot_mask);
      s])]
;;

(* Table.lookup(Table_t, key, arg) -> result *)
let exact_lookup spec =
  let key_param = evar (cid "key") spec.key_ty in
  let arg_param = evar (cid "arg") spec.arg_ty in
  let s_apply_default = s_apply_default spec arg_param in
  (* a free slot means the key isn't in the table, so run the default *)
  let s_probe = probe spec key_param (stmts [
    sif (slot_is_free spec) s_apply_default snoop;
    sif (slot_has_key spec key_param) 
      (s_apply_entry spec (entry_at_slot spec) arg_param) 
      snoop])
  in
  let params = List.map extract_evar
    [key_param; arg_param]
  in
  dfun
    (lookup_id spec.tbl_id)
    spec.ret_ty
    params
    (stmts [s_probe; s_apply_default])
;;

(* Table.install(Table_t, key, action, const_arg) *)
let exact_install spec = 
  let tbl = evar (spec.tbl_id) (spec.tbl_ty) in
  let key_param = evar (cid "key") spec.key_ty in
  (* note: call has to be transformed from an action variable to an action tag value *)
  let action_param = evar (cid "action") tenum_member in
  let const_arg_param = evar (cid "const_arg") spec.const_arg_ty in
  (* an entry that is already in the table doesn't change 
     Otherwise, the new entry goes into the first free slot, 
     unless the table is full. *)
  let s_probe = probe spec key_param (stmts [
    sif (slot_is_free spec) 
      (stmts [
        sif ((tbl/.cid"num_entries") /== eval (vint spec.len 32))
          (stmts [swarn spec "full"; sret_none])
          snoop;
        (tbl/.cid"entries", slot)/<-(exact_cell spec.tbl_id key_param action_param const_arg_param);
        sassign_exp (tbl/.cid"num_entries") ((tbl/.cid"num_entries") /+ eval (vint 1 32));
        sret_none])
      snoop;
    sif (slot_has_key spec key_param) sret_none snoop])
  in
  let params = List.map extract_evar 
    [key_param; action_param; const_arg_param]
  in
  dfun 
    (install_id spec.tbl_id)
    (tunit)
    params
    (stmts [s_probe; sret_none])
;;

(* Table.update(Table_t, key, action, const_arg) *)
let exact_update spec = 
  let key_param = evar (cid "key") spec.key_ty in
  let action_param = evar (cid "action") tenum_member in
  let const_arg_param = evar (cid "const_arg") spec.const_arg_ty in
  let entry = entry_at_slot spec in
  let s_probe = probe spec key_param (stmts [
    sif (slot_is_free spec) (stmts [swarn spec "update"; sret_none]) snoop;
    sif (slot_has_key spec key_param) 
      (stmts [
        sassign_exp (entry/.cid"action_tag") action_param;
        sassign_exp (entry/.cid"action_arg") const_arg_param;
        sret_none])
      snoop])
  in
  let params = List.map extract_evar 
    [key_param; action_param; const_arg_param]
  in
  dfun 
    (update_id spec.tbl_id)
    (tunit)
    params
    (stmts [s_probe; sret_none])
;;

(* Table.remove(Table_t, key) *)
let exact_remove spec = 
  let tbl = evar (spec.tbl_id) (spec.tbl_ty) in
  let key_param = evar (cid "key") spec.key_ty in
  let entry = entry_at_slot spec in
  let slot_mask = eval (vint (exact_capacity spec.len - 1) 32) in
  let removed_id = cid "removed" in
  let removed = evar removed_id tbool in
  (* after removing an entry, need to re-arrange the subsequent entries *) 
  let hole_id = cid "hole" in
  let hole = evar hole_id (tint 32) in
  let entry_home_id = cid "entry_home" in
  let entry_home = evar entry_home_id (tint 32) in
  (* the number of slots from a to b, going forward *)
  let slots_from a b = (b /- a) /& slot_mask in
  let s_free_hole = stmts [
    sassign_exp (((tbl/.cid"entries")/@hole)/.cid"valid") (eval (vbool false));
    sassign_exp (tbl/.cid"num_entries") ((tbl/.cid"num_entries") /- eval (vint 1 32));
    sret_none]
  in
  let s_fill_hole = stmts [
    sif (slot_is_free spec) s_free_hole snoop;
    slocal entry_home_id (tint 32) (ehash spec (entry/.cid"key"));
    sif (eop Geq [slots_from entry_home slot; slots_from hole slot])
      (stmts [
        (tbl/.cid"entries", hole)/<-entry;
        sassign hole_id slot])
      snoop]
  in
  let s_find = stmts [
    sif (slot_is_free spec) (stmts [swarn spec "remove"; sret_none]) snoop;
    sif (slot_has_key spec key_param) 
      (stmts [
        sassign removed_id (eval (vbool true));
        sassign hole_id slot])
      snoop]
  in
  let params = List.map extract_evar 
    [key_param]
  in
  dfun 
    (remove_id spec.tbl_id)
    (tunit)
    params
    (stmts [
      slocal removed_id tbool (eval (vbool false));
      slocal hole_id (tint 32) (eval (vint 0 32));
      probe spec key_param (sif removed s_fill_hole s_find);
      sret_none])
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
    let is_ternary = is_tbuiltin TernaryTables.t_id builtin_tbl_ty in
    let tbl_cell_ty = 
      if is_ternary then table_cell_type tbl_id key_ty const_arg_ty 
      else exact_cell_type tbl_id key_ty const_arg_ty
    in
    let tbl_ty = 
      if is_ternary then table_instance_type tbl_id const_arg_ty tbl_cell_ty len 
      else exact_table_type tbl_id const_arg_ty tbl_cell_ty len 
    in
    let tbl_constructor = 
      if is_ternary then table_create tbl_ty default_action_enum_id default_action_arg 
      else exact_table_create tbl_ty default_action_enum_id default_action_arg 
    in
    let tbl_spec = {tbl_id; len; tbl_ty; key_ty; const_arg_ty; arg_ty; ret_ty; action_tags;} in
    let new_decls = [
      decl_tabstract tbl_cell_ty;               (* cell type within a table *)
      decl_tabstract tbl_ty;                    (* the table's type *)
      dglobal tbl_id tbl_ty tbl_constructor; (* table declaration *)
      table_warn tbl_spec "full" true "is full, an entry was not installed";
      table_warn tbl_spec "update" false "has no such entry to update";
      table_warn tbl_spec "remove" false "has no such entry to remove"]
      @(if is_ternary then [
        table_install tbl_spec;                 (* table install function *)
        table_update tbl_spec;                  (* table update function *)
        table_remove tbl_spec;                  (* table remove function *)
        table_lookup tbl_spec]                  (* table lookup function *)
      else [
        exact_hash tbl_spec;                    (* the hash of the table's keys *)
        exact_install tbl_spec;
        exact_update tbl_spec;
        exact_remove tbl_spec;
        exact_lookup tbl_spec])
    in
    new_decls
  | _ -> [decl]
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
          let fun_id = match (Cid.names f_cid) with 
            | ["Table"; fname] 
            | ["TernaryTable"; fname] -> Cid.str_cons_plain fname tbl_id
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
    let has_exact_table = List.exists 
      (fun decl -> match decl.d with 
        | DVar(_, ty, Some(_)) -> is_tbuiltin Tables.t_id ty 
        | _ -> false) 
      decls 
    in
    let decls = if has_exact_table then table_hash_mix::decls else decls in
    let decls = List.flatten (List.map (monomorphic_table_decls action_tags) decls) in
    let decls = monomorphic_table_calls#visit_decls () decls in
    decls
;;
