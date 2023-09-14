(* construct the deparser for each component *)

open TofinoCore

exception Error of string
let error s = raise (Error s)

module DBG = BackendLogging
let outc = ref None
let dprint_endline = ref DBG.no_printf
let start_logging () = DBG.start_mlog (!IoUtils.moduleLogDir) __FILE__ outc dprint_endline

let is_output output_cids cid = 
  List.exists (fun cid' -> Cid.equal cid cid') output_cids
;;
let is_checksum_hash exp = 
  match exp.e with 
  | EHash(size, args) -> (
    match size, args with
    | 16, epoly::_ -> (
      match epoly.e with
      | EVar(cid) -> Cid.equal cid (Cid.id Builtins.checksum_id)
      | _ -> false
    )
    | _ -> false
  )
  | _ -> false
;;
let args_of_ehash exp = 
  match exp.e with 
  | EHash(_, args) -> List.map 
    (fun exp -> match exp.e with 
      | EVar(cid) -> cid, exp.ety
      | _-> error "[args_of_ehash] checksum hash arguments must all be variables")
    (List.tl args)
  | _ -> error "[args_of_ehash] not an ehash"

type var_sort = | VInput | VSharedLocal | VLocal 
let var_sort input_cids shared_local_cids var = 
  if List.exists (fun cid -> Cid.equal cid var) input_cids then VInput
  else if List.exists (fun cid -> Cid.equal cid var) shared_local_cids then VSharedLocal
  else VLocal

let extract_checksums output_cids stmts = 
  let open CoreSyntax in
  let checksum_flag_params = ref [] in (*collect the flag parameters for updated handler output *)
  let deparser_stmts = ref [] in (* the deparser statements that have been added, for updated handler deparser *)
  let checksum_flag_num = ref 1 in
  let fresh_checksum_flag () = 
    let id = ("do_checksum_op_"^(string_of_int !checksum_flag_num), !checksum_flag_num) in
    let ty = CoreSyntax.tint 1 in
    checksum_flag_num := !checksum_flag_num + 1;
    id, ty
  in
  let arg_cids = ref [] in
  let v = 
    object
    inherit [_] s_map as super
    method! visit_statement () stmt = 
      match stmt.s with
      | SAssign(cid, exp) -> (
        if ((is_output output_cids cid) & (is_checksum_hash exp)) then (
          !dprint_endline ("found a checksum statement that can be moved to deparser " ^ (CorePrinting.statement_to_string stmt));
          (* first, make the checksum flag parameter  *)
          let csumflag_id, csumflag_ty = fresh_checksum_flag () in
          (* then, add the deparser statement *)
          (* construct the replacement statement: set the flag to 1. *)
          let s' = SAssign(Cid.id csumflag_id, CoreSyntax.vint_exp 1 1) in
          (* construct the deparser statement: if (flag == 1) {stmt} *)
          let deparse_s = CoreSyntax.sifte 
            (op Eq [var (Cid.id csumflag_id) csumflag_ty; CoreSyntax.vint_exp 1 1] (ty TBool))
            stmt
            snoop
          in
          (* update state *)
          checksum_flag_params := (csumflag_id, csumflag_ty)::!checksum_flag_params;
          deparser_stmts := deparse_s::!deparser_stmts;
          checksum_flag_num := !checksum_flag_num + 1;
          arg_cids := !arg_cids @ (args_of_ehash exp);
          (* return the replacement statement *)
          {stmt with s=s'}
        ) else (
          super#visit_statement () stmt
        )
      )
      | _ -> super#visit_statement () stmt
    end
  in
  let stmts = List.map (v#visit_statement ()) stmts in
  (stmts, !checksum_flag_params, !deparser_stmts, !arg_cids)
;;

(* replace declarations of var with assigns to it *)
let undeclare cid stmt = 
  let open CoreSyntax in
  let v = 
    object
    inherit [_] s_map as super
    method! visit_statement () stmt = 
      match stmt.s with
      | SLocal(id, _, exp) -> 
        if Cid.equal (Cid.id id) cid then (
          {stmt with s = SAssign(Cid.id id, exp)}
        ) else (
          stmt
        )
      | _ -> super#visit_statement () stmt
    end
  in
  v#visit_statement () stmt

let renamer = 
  object 
  inherit [_] s_map as super
  method! visit_EVar rename_map cid = 
    if (List.assoc_opt cid rename_map) <> None then (
      EVar(List.assoc cid rename_map)
    ) else (
      EVar(cid)
    )
  method! visit_SAssign rename_map cid exp = 
    let exp = super#visit_exp rename_map exp in
    if (List.assoc_opt cid rename_map) <> None then (
      SAssign(List.assoc cid rename_map, exp)
    ) else (
      SAssign(cid, exp)
    )
end
;;

let scope_deparser_params comp = 
  (* here, we have to change an event defined in the component and return the updated component *)
  let main = main_handler_of_component comp in
  let hdl_input = get_event comp main.hdl_input in

  (* change the input event to an event with meta params that includes the deparser params
     and correct the scope of all the params when used in the body of the handler or its deparser. *)
  let hdl_input' = EventWithMetaParams({
    event = hdl_input;
    params = main.hdl_deparse_params;
  })
  in
  (* rename all the event parameters when used in the handler and deparser *)
  let rename_map = List.map 
    (fun (param_id, _) -> 
      Cid.id param_id, Cid.create_ids [id_of_event hdl_input; param_id]) 
    main.hdl_deparse_params 
  in
  let hdl_body' = match main.hdl_body with
    | SPipeline(stmts) -> SPipeline(List.map (renamer#visit_statement rename_map) stmts)
    | SFlat(stmt) -> SFlat(renamer#visit_statement rename_map stmt)
  in
  let hdl_deparse' = renamer#visit_statement rename_map main.hdl_deparse in
  (* update main *)
  let main' = {main with 
    hdl_body = hdl_body';
    hdl_deparse = hdl_deparse';
    hdl_deparse_params = [];
  } in
  let comp' = replace_main_handler_of_component comp main' in
  (* finally, update the input event, setting it to the created one *)
  set_event comp' hdl_input'
;;

let process_component comp = 
  let open CoreSyntax in 
  match comp.comp_sort with 
  | HControl -> comp 
  | _ -> 
    let main = main_handler_of_component (comp) in
    let hdl_output = get_event comp main.hdl_output in
    let output_cids = fields_of_event hdl_output in 
    (* extract deparser statements, replace with flag setting statements *)
    let main', flag_params, deparser_stmts, arg_cids = match main.hdl_body with
      | SPipeline(stmts) -> 
        let stmts', checksum_flag_params, deparser_stmts, arg_cids = extract_checksums output_cids stmts in
        {main with hdl_body = SPipeline(stmts')}, checksum_flag_params, deparser_stmts, arg_cids
      | SFlat(stmt) -> 
        let stmts', checksum_flag_params, deparser_stmts, arg_cids = extract_checksums output_cids [stmt] in
        {main with hdl_body = SFlat(List.hd stmts')}, checksum_flag_params, deparser_stmts, arg_cids
    in
    (* got the updated hdl_body, now we need to:
        1) add in the flag params
        2) add meta params
        3) add deparser statement        *)
    (* 1. add in the flag params *)
    let main' = {main' with 
      hdl_deparse_params = 
        main'.hdl_deparse_params@flag_params} 
    in
    (* 2. add in meta params. Cases: *)
    (*param is an input: no change
      param is a shared local: remove from shared local and put into meta params
      param is a local: put into meta params and change the local declaration to an assignment *)
    let hdl_input = get_event comp main.hdl_input in

    let param_cids = fields_of_event hdl_input in 
    let prealloc_local_cids = main'.hdl_preallocated_vars |> List.map (fun (id, _) -> Cid.id id) in 
    let arg_kinds = List.map (fun (cid, _) -> var_sort param_cids prealloc_local_cids cid) arg_cids in
    let main' = List.fold_left
      (fun main ((arg_cid, arg_ty), arg_kind) -> 
        match arg_kind with
        | VInput -> main
        | VSharedLocal ->
          (* remove the variable from the shared local list and put it into the param list *)
          let var_ty = List.assoc (Cid.to_id arg_cid) main.hdl_preallocated_vars in
          {main with
            hdl_preallocated_vars = List.remove_assoc (Cid.to_id arg_cid) main.hdl_preallocated_vars;
            hdl_deparse_params = (Cid.to_id arg_cid, var_ty)::main.hdl_deparse_params;
          }
        | VLocal ->
          (* change declarations to assignments *)
          let hdl_body' = match main.hdl_body with 
            | SPipeline(stmts) -> SPipeline(List.map (undeclare arg_cid) stmts)
            | SFlat(stmt) -> SFlat(undeclare arg_cid stmt) 
          in
          (* add to paramvars *)
          {main with 
            hdl_body = hdl_body';
            hdl_deparse_params = (Cid.to_id arg_cid, arg_ty)::main.hdl_deparse_params;
          })
      (main')
      (List.combine arg_cids arg_kinds)
    in
    (* 3. add deparser statement *)
    let main' = {main' with hdl_deparse = sequence_stmts deparser_stmts;} in 
    (* 4. initialize deparser inputs in the parser *)
    let parser = main_parser_of_component comp in
    let init_actions = List.map
      (fun (cid, ty) -> 
        PAssign(cid, vint_exp_ty 0 ty))
      (List.map (fun (id, ty) -> Cid.create_ids [id_of_event hdl_input; id], ty) main'.hdl_deparse_params)
    in
    let parser_block' = {parser.pblock with
      pactions= parser.pblock.pactions@(List.map (fun a -> (a, Span.default)) init_actions);
    } in
    let parser' = {parser with pblock = parser_block'} in

    (* 5. move the deparser params into the event (that means we can remove the deparser params..)*)
    let comp' = replace_main_handler_of_component comp main' in
    let comp' = replace_main_parser_of_component parser' comp' in
    let comp' = scope_deparser_params comp' in
    (* let main' = scope_deparser_params main' in *)
      
    (* 5. replace the main handler and parser *)
    !dprint_endline (Printf.sprintf "----- initial main handler for %s -----" (CorePrinting.id_to_string comp.comp_id));
    !dprint_endline (TofinoCorePrinting.hevent_to_string main);
    !dprint_endline (Printf.sprintf "----- updated main handler for %s -----" (CorePrinting.id_to_string comp.comp_id));
    !dprint_endline (TofinoCorePrinting.hevent_to_string main');
    !dprint_endline ("-------------------------------------------");
    comp'
    (* replace_main_handler_of_component comp main'
    |> replace_main_parser_of_component parser' *)

;;

let process core_prog = 
  List.map 
    (fun comp -> 
      if (comp.comp_sort = HControl) then comp
      else (
        !dprint_endline ("processing component " ^ (CorePrinting.id_to_string comp.comp_id));
        process_component comp))
    core_prog
;;
      
