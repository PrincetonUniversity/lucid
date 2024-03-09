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

let cid s = Cid.create [s]
let id = Id.create
let tag id = Id.append_string "_tag" id

let create_action_enum_ty decls = 
  let action_ids = List.filter_map extract_daction_id_opt decls
  in 
  let action_tags = List.map tag action_ids in
  let action_enum_ty = tenum action_tags in
  tabstract_cid (cid "action_enum") action_enum_ty
;;

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

let table_create (tbl_ty : ty) (def_enum_id : id) (acn_enum_ty : ty) (def_arg : value) = 
  let def_entry = vtuple 
    [
      venum def_enum_id acn_enum_ty;
      def_arg
    ]
  in
  vrecord 
    [id "default"; id "entries"]
    [def_entry; zero_list tbl_ty]
;;

(* tbl_lookup(tbl_ty t, key_ty k, arg_ty arg) *)
(* note: the length and the types are all part of tbl_ty, 
   but its easier to just pass them in from the 
   function builder. *)
let table_lookup tbl_id tbl_ty tbl_len key_ty const_arg_ty arg_ty ret_ty action_ids actions_enum_ty = 
  let tbl_param = evar (cid "tbl") tbl_ty in
  let key_param = evar (cid "key") key_ty in
  let _ = key_param in
  let arg_param = evar (cid "arg") arg_ty in
  (* reconstruct the action type for the match branches *)
  let action_ty = tfun_kind FAction [const_arg_ty; arg_ty] ret_ty in  
  (* first, declare a local variable that calls the default action *)
  let s_decl_rv = (cid"rv") /::= default_exp ret_ty in
  let apply_default_branch action_id =    
    let action_evar = efunref (Cid.id action_id) action_ty in
    (case actions_enum_ty) 
      (tag action_id)
      ( id"rv" /:= (action_evar /** [tbl_param/->"default"/->"action_arg"; arg_param]))
  in
  let s_apply_default = smatch
    (tbl_param/->"default"/->"action_tag")
    (List.map apply_default_branch action_ids)
  in
  let idx = id "idx" in
  let apply_branch  action_id = 
    (* tbl->entries[idx]->action_arg *)
    let action_evar = efunref (Cid.id action_id) action_ty in
    let entry = tbl_param/->"entries"/@idx in
    print_endline ("entry type: ");
    print_endline (CCorePPrint.ty_to_string entry.ety);
    let _ = entry/->"action_arg" in
    (case actions_enum_ty) 
      (tag action_id)
      ( id"rv" /:= (action_evar /** [((tbl_param/->"entries")/@idx)/->"action_arg"; arg_param]))
  in
  let s_apply_entry = smatch 
    ((tbl_param/->"entries"/@idx)/->"action_tag")
    (List.map apply_branch action_ids)
  in
  let s_loop = sfor idx tbl_len s_apply_entry in

  let s_ret = sret (evar (cid"rv") ret_ty) in
  
  let params = List.combine 
    [id "tbl"; id"key"; id"arg"]
    (List.map alias_type [tbl_ty; key_ty; arg_ty])
  in
  let fcn_id = Id.append_string "_lookup" tbl_id in
  dfun 
    fcn_id
    ret_ty 
    params
    (stmts [s_decl_rv; s_apply_default; s_loop; s_ret])



  (* let apply_default = 
    smatch 
      (tbl_param-->"default"-->"action_tag")
      [

      ]
  in *)

;;

let transform_decl actions_enum_tname decl : decls = 
  match decl.d with 
  | DVar(tblid, ty, Some(exp)) when is_tbuiltin Tables.t_id ty -> 
    let key_ty, const_arg_ty, arg_ty, ret_ty = 
      match extract_tbuiltin ty with 
      | _, [key_ty; const_arg_ty; arg_ty; ret_ty] -> key_ty, const_arg_ty, arg_ty, ret_ty
      | _, _ -> failwith "unexpected type"      
    in
    let len, action_ids, default_action_enum_id, default_action_arg = match (eval_exp exp |> extract_vevent).evdata with 
      | [len; actions; default_action; default_action_arg] ->  
        extract_vint len |> arridx, 
        extract_vtuple actions |> List.map extract_vsymbol,
        extract_vsymbol default_action, (* here, the id is the symbol in the enum *) 
        default_action_arg
      | _ -> failwith "unexpected table declaration"
    in
    let tbl_cell_ty = table_cell_type tblid key_ty actions_enum_tname const_arg_ty in
    print_endline ("table cell type: ");
    print_endline (CCorePPrint.ty_to_string tbl_cell_ty);
    let tbl_ty = table_instance_type tblid actions_enum_tname const_arg_ty tbl_cell_ty len in
    let tbl_value = table_create (alias_type tbl_ty) default_action_enum_id actions_enum_tname default_action_arg in
    (* output: 
      1. table cell type declaration;
      2. table type declaration;
      3. table declaration;
      4. table install function;
      5. table match function
    *)
    let dty_abs ty = 
      let tname = alias_type ty in
      let ty = base_type ty in
      let name = extract_tname tname in
      dty name ty
    in
    let new_decls = [
      dty_abs tbl_cell_ty;
      dty_abs tbl_ty;
      dglobal tblid (alias_type tbl_ty) (eval tbl_value);
      table_lookup tblid tbl_ty len key_ty const_arg_ty arg_ty ret_ty action_ids actions_enum_tname 
      ] 
    in
    print_endline ("source table declaration: ");
    print_endline (CCorePPrint.decl_to_string decl); 
    print_endline ("constructed declarations:");
    String.concat "\n" (List.map CCorePPrint.decl_to_string new_decls) |> print_endline;
    new_decls
  | _ -> [decl]
;;

let transform_decls decls = 
  (* generate the actions enum decl *)
  let actions_enum_ty = create_action_enum_ty decls in
  let actions_enum_tname, actions_enum_tenum = split_tabstract actions_enum_ty in  
  let actions_enum_decl = dty (extract_tname actions_enum_tname) actions_enum_tenum in
  print_endline "actions enum decl: ";
  print_endline (CCorePPrint.decl_to_string actions_enum_decl);
  List.flatten (List.map (transform_decl actions_enum_tname) decls)

;;

(* let type_transformer = 
  object 
    inherit [_] s_map as super

    method! visit_decl () decl = 

        super#visit_decl () decl
      | _ -> super#visit_decl () decl
  end
;; *)

let process_decls decls = 
  transform_decls decls 
  (* |>
  type_transformer#visit_decls () decls *)
;;