(* implementation of Tables in CCore
   
  Implements: 

    1. Type TBuiltin(Cid["Table"; "t"], [key_type; const_arg_type; arg_type; result_type])

    2. Constructor Table.create(length, actions, default_action, default_action_const_arg);

    3. Method Table.lookup(Table_t, key, arg) -> result;

    4. Method Table.install(Table_t, key, action, const_arg);

*)

open CCoreSyntax

[@@@ocaml.warning "-21"]
[@@@ocaml.warning "-27"]

(*** defunctionalize actions ***)
let tag id = Id.append_string "_tag" id
let untag id = 
  (* remove the suffix "_tag" from (fst id) *)
  let s = String.split_on_char '_' (fst id) |> List.hd in
  (s, snd id)
let cid s = Cid.create [s]
let id = Id.create

let defunctionalize_actions decls = 
  (* construct the action enum type, then go through the 
     program and replace every action variable with an action enum tag *)
  let action_ids = List.filter_map extract_daction_id_opt decls in 
  let action_tags = List.map tag action_ids in
  let action_enum_ty = tenum action_tags in
  (* replace action variables with action enum tags *)
  let action_var_replacer = 
    object 
      inherit [_] s_map as super
      method! visit_exp () exp = 
        try 
          let var_id, _ = extract_evar_id exp in
          if (List.mem var_id action_ids) then 
            eval (venum (tag var_id) action_enum_ty)
          else
            super#visit_exp () exp  
        with _ -> super#visit_exp () exp 
    end
  in
  let decls = action_var_replacer#visit_decls () decls in
  (* return separate enum type for use later *)
  decls, tabstract_cid (cid "action_enum") action_enum_ty
;;

(*** generate monomorphic table types and functions from Builtins ***)

(* All the information we need about a table to generate its methods. 
   This can be derived from the declaration. *)
type table_spec = 
{
  tbl_id : id; 
  len    : arridx;
  tbl_ty : ty;  
  key_ty : ty;
  const_arg_ty : ty;
  arg_ty : ty; 
  ret_ty : ty;
  action_tags : id list;
  actions_enum_ty : ty;
}

let table_cell_type tbl_id key_ty acns_enum_ty const_action_arg_ty : ty = 
  let tblcellty_id = Id.append_string "_cellty" tbl_id in
  tabstract_id 
    tblcellty_id
    (trecord 
      [id "valid"; id "key"; id "action_tag"; id "action_arg"] 
      [
        tbool; (* is entry valid or not *)
        key_ty; (* the key *)
        acns_enum_ty; (* tag of the action *)
        const_action_arg_ty; (* constant argument to the action *)
      ])
;;

let table_instance_type tbl_id acns_enum_ty const_action_arg_ty tbl_cell_ty tbl_len =   
  (* a table is a global struct with a default and a list of entries *)
  tglobal (
    tabstract_id
    (Id.append_string "_ty" tbl_id)
    (
      trecord
        [id "default"; id "entries"]
        [
          trecord [id "action_tag"; id "action_arg"] [acns_enum_ty; const_action_arg_ty];
          tlist tbl_cell_ty tbl_len
        ]
    )
  )
;;

let vrecord pairs = 
  let a, b = List.split pairs in
  vrecord a b
;;
let table_create (tbl_ty : ty) (def_enum_id : id) (acn_enum_ty : ty) (def_arg : value) = 
  let fields, tys = extract_trecord tbl_ty in
  let field_ty = List.combine fields tys in 
  let entries_ty = List.assoc (id"entries") field_ty in
  let default = vrecord [
    id"action_tag", venum def_enum_id acn_enum_ty;
    id"action_arg", def_arg
  ] in
  let base_ty = vrecord [
    id"default", default;
    (* its a global value. Maybe this should just be 
       baked into a special declaration for globals? *)
    id"entries", (zero_list entries_ty)
  ] 
  in
  let abstr_ty = abstr_cast_value (alias_type tbl_ty |> extract_tname) base_ty in
  vglobal abstr_ty
;;

(* tbl_lookup(tbl_ty t, key_ty k, arg_ty arg) *)
(* note: the length and the types are all part of tbl_ty, 
   but its easier to just pass them in from the 
   function builder. *)
let lookup_id tbl_id = Id.append_string "_lookup" tbl_id ;;
let table_lookup spec = 
  let tbl_param = evar (cid "tbl") spec.tbl_ty in
  let key_param = evar (cid "key") spec.key_ty in
  let arg_param = evar (cid "arg") spec.arg_ty in
  (* reconstruct the action type for the match branches *)
  let action_ty = tfun_kind FAction [spec.const_arg_ty; spec.arg_ty] spec.ret_ty in  
  (* first, declare a local variable that calls the default action *)
  let s_decl_rv = (cid"rv") /::= default_exp spec.ret_ty in
  let apply_default_branch action_tag =    
    let action_evar = efunref (Cid.id (untag action_tag)) action_ty in
    (case spec.actions_enum_ty)
    action_tag
      ( id"rv" /:= (action_evar /** [tbl_param/->"default"/->"action_arg"; arg_param]))
  in
  let s_apply_default = smatch
    (tbl_param/->"default"/->"action_tag")
    (List.map apply_default_branch spec.action_tags)
  in
  let idx = id "_idx" in
  let cont = id "_cont" in
  let apply_branch  action_tag = 
    (* tbl->entries[idx]->action_arg *)
    let action_evar = efunref (Cid.id (untag action_tag)) action_ty in    
    (case spec.actions_enum_ty) 
    action_tag
      ( id"rv" /:= (action_evar /** [((tbl_param/->"entries")/@idx)/->"action_arg"; arg_param]))
  in

  let s_loop = 
    swhile idx spec.len cont 
      (
        let entry = (tbl_param/->"entries"/@idx) in        
        sif (entry/->"valid")
          (
            sif (eop Eq [key_param; entry/->"key"])
              (stmts [
              sassign (Cid.id cont)  (eval (vbool false));
              smatch (entry/->"action_tag")
                (List.map apply_branch spec.action_tags);
              ]
              )
              snoop
          )
          snoop
      )
  in
  let s_ret = sret (evar (cid"rv") spec.ret_ty) in
  
  let params = List.map extract_evar_id 
    [tbl_param; key_param; arg_param]
  in
  dfun 
    (lookup_id spec.tbl_id)
    spec.ret_ty 
    params
    (stmts [
            s_decl_rv; 
            s_apply_default; 
            s_loop; 
            s_ret;])
;;

(* Table.install(Table_t, key, action, const_arg) *)
let install_id tbl_id = Id.append_string "_install" tbl_id ;;
let table_install spec = 
  let tbl_param = evar (cid "tbl") spec.tbl_ty in
  let key_param = evar (cid "key") spec.key_ty in
  (* note: call has to be transformed from an action variable to an action tag value *)
  let action_param = evar (cid "action") (spec.actions_enum_ty) in 
  let const_arg_param = evar (cid "const_arg") spec.const_arg_ty in
  let new_slot = erecord 
    [id "valid"; id "key"; id "action_tag"; id "action_arg"] 
    [eval (vbool true); key_param; action_param; const_arg_param]
  in

  let idx = id "_idx" in
  let cont = id "_cont" in
  let body = swhile idx spec.len cont
    (
      let entries = eop (Project(id"entries")) [tbl_param] in
      let entry = elistget entries (idxvar idx) in
      (* let entry = (entries/@idx) in     *)
      sif (eop Eq [entry/->"valid";eval@@vbool false])
        (stmts [
            sassign (Cid.id cont)  (eval (vbool false));
            (tbl_param/->"entries", idxvar idx)/<-new_slot;            
          ])
        snoop
    )
  in
  let params = List.map extract_evar_id 
    [tbl_param; key_param; action_param; const_arg_param]
  in
  let sret = sret_none in
  dfun 
    (install_id spec.tbl_id)
    (tunit ())
    params
    (sseq body sret)
;;

let monomorphic_table_decls actions_enum_ty decl : decls = 
  match decl.d with 
  | DVar(tbl_id, ty, Some(exp)) when is_tbuiltin Tables.t_id ty -> 
    let key_ty, const_arg_ty, arg_ty, ret_ty = 
      match extract_tbuiltin ty with 
      | _, [key_ty; const_arg_ty; arg_ty; ret_ty] -> key_ty, const_arg_ty, arg_ty, ret_ty
      | _, _ -> failwith "unexpected type"      
    in
    let len, action_tags, default_action_enum_id, default_action_arg = match (eval_exp exp |> extract_vevent).evdata with 
      | [len; actions; default_action; default_action_arg] ->  
        extract_vint len |> arridx, 
        extract_vtuple actions |> List.map extract_vsymbol,
        extract_vsymbol default_action, (* here, the id is the symbol in the enum *) 
        default_action_arg
      | _ -> failwith "unexpected table declaration"
    in
    let tbl_cell_ty = table_cell_type tbl_id key_ty (actions_enum_ty) const_arg_ty in
    print_endline ("table cell type: ");
    print_endline (CCorePPrint.ty_to_string tbl_cell_ty);
    let tbl_ty = table_instance_type tbl_id (actions_enum_ty) const_arg_ty tbl_cell_ty len in
    let tbl_value = table_create tbl_ty default_action_enum_id (actions_enum_ty) default_action_arg in
    let tbl_spec = {tbl_id; len; tbl_ty; key_ty; const_arg_ty; arg_ty; ret_ty; action_tags; actions_enum_ty;} in
    let new_decls = [
      decl_tabstract tbl_cell_ty;   (* cell type within a table *)
      decl_tabstract tbl_ty;        (* the table's type *)
      dglobal tbl_id (tbl_ty) (eval tbl_value); (* table declaration *)
      table_install tbl_spec;       (* table install function *)
      table_lookup tbl_spec         (* table lookup function *)
      ] 
    in
    print_endline ("source table declaration: ");
    print_endline (CCorePPrint.decl_to_string decl); 
    print_endline ("constructed declarations:");
    String.concat "\n" (List.map CCorePPrint.decl_to_string new_decls) |> print_endline;
    new_decls
  | _ -> [decl]
;;

let monomorphic_table_calls = 
  let table_fun_cids = List.map Builtins.gfun_cid Tables.signature.m_funs in
  object 
    inherit [_] s_map as super

    method! visit_exp () exp = 
      match exp.e with 
      | ECall({f; args; call_kind=CFun}) -> 
        (* replace generic table function call with table-specific function call *)
        let f_cid, _ = extract_evar f in
        if (List.mem f_cid table_fun_cids) then 
        (
          let tbl_id, _ = extract_evar_id (List.hd args) in 
          let fun_id = match (Cid.names f_cid) with 
            | ["Table";"install"] -> ( install_id tbl_id )
            | ["Table";"install_ternary"] -> ( failwith "ternary install not implemented" )
            | ["Table";"lookup"] -> ( lookup_id tbl_id )
            | _ -> failwith "unexpected table method"
          in
          let f = efunref (Cid.id fun_id) f.ety in
          {exp with e=ECall{f; args; call_kind=CFun}}
        )
        else
          {exp with e=ECall{f; args; call_kind=CFun}}
      | _ -> super#visit_exp () exp
  end
;; 
let transform_decls decls = 
  let decls, actions_enum_ty = defunctionalize_actions decls in
  (decl_tabstract actions_enum_ty)
    ::List.flatten (List.map (monomorphic_table_decls actions_enum_ty) decls)
;;


let process_decls decls = 
  let decls, actions_enum_ty = defunctionalize_actions decls in
  let decls = (decl_tabstract actions_enum_ty)::decls in 
  let decls = List.flatten (List.map (monomorphic_table_decls actions_enum_ty) decls) in
  let decls = monomorphic_table_calls#visit_decls () decls in
  let decls = CCoreTyper.check_decls decls in
  decls
;;