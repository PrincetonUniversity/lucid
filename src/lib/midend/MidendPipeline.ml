(* The midend pipeline for the interpreter. 
   Mainly does partial interpretation. *)
let print_if_debug ds =
  if Cmdline.cfg.debug
  then (
    print_endline "decls: ";
    let str = CorePrinting.decls_to_string ds in
    Console.report str)
;;

let print_if_verbose str = if Cmdline.cfg.verbose then Console.report str
let report str = Console.show_message str ANSITerminal.Green "compiler"
let cfg = Cmdline.cfg
let enable_compound_expressions = true
let do_ssa = false
let optimize_simple_calls = ref true
let set_no_call_optimize () = optimize_simple_calls := false

(* a little pass to make action constructor names not have a ~# component at the end, 
   so they can be resolved from interpcontrol *)
let stringify_action_constructor_names ds = 
  let open CoreSyntax in
  let replace_one_id = object 
    inherit [_] s_map as super
    method! visit_id (tgt_id, new_id) id = 
      if (Id.equal id tgt_id) then new_id else id
    end
  in
  let replace_all_ids idmap = List.fold_left 
    (fun ds id_pair -> replace_one_id#visit_decls id_pair ds) 
    ds idmap
  in
  let id_map = List.filter_map 
    (function DActionConstr({aid;}) -> Some(aid, Id.create (Id.name aid)) | _ -> None) 
    (List.map (fun decl -> decl.d) ds) 
  in
  let ds = replace_all_ids id_map in
  ds
;;


let process_prog ds =
  print_if_verbose "-------Translating to core syntax---------";
  let ds = SyntaxToCore.translate_prog ds in
  let ds = AddEthStartMain.process ds in

  (* let ds = Despecialization.test_parser_despecialization ds in *)
  (* let ds = Despecialization.test_memop_despecialization ds in *)
  (* let ds = stringify_action_constructor_names ds in *)
  (* uncomment to test if core -> fcore -> core is correct *)
  (* let ds = CCorePasses.test_core_translation ds in *)
  (* TBuiltins have to be converted back to TName's 
     for the interpreter backend, for now,
     as we haven't ensured that they are implemented throughout. *)
  let ds = EliminateTBuiltin.process_prog ds in
  (* let fds = CoreToCCore.translate_prog ds in
  let ds = CCoreToCore.translate_prog fds in *)
  print_if_debug ds;
  let ds =
    if cfg.partial_interp
    then (
      (* print_if_verbose "-------Adding default parser for single-packet programs---------"; *)
      let ds = InlineEventVars.set_event_nums ds in 
      let ds = AddIngressParser.add_simple_parser None ds in      
      print_if_verbose "-------Partial interpreting---------";
      let ds = PartialInterpretation.interp_prog ds in
      print_if_debug ds;
      ds)
    else ds
  in
  ds
;;
