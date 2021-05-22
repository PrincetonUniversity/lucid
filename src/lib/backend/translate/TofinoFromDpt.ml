(* DPT to Tofino machine grammar 
    new version, following style of fromIr... *)
(* 
    Simple implementation. 
    1. convert each handler into an operation-statement graph.
    2. translate each operation-statement into a graph. 
*)
open SyntaxUtils
open Batteries
open Format
open OpGraph
open InterpHelpers
open TofinoConstants
open TofinoContext
open TofinoOp
module CL = Caml.List

open Syntax
module IS = InstrSyntax

(* logging *)
module DBG = BackendLogging
let outc = ref None 
let dprint_endline = ref DBG.no_printf

(* code generators for builtin functions *)
let dpt_builtin_fcns = 
[
  (TofinoArray.array_create_cid, TofinoArray.create_array);
  (TofinoArray.array_update_cid, TofinoArray.update_array);
  (TofinoArray.array_set_cid, TofinoArray.set_array);
  (TofinoArray.array_get_cid, TofinoArray.get_array);
  (TofinoArray.array_setm_cid, TofinoArray.setm_array);
  (TofinoArray.array_getm_cid, TofinoArray.getm_array);
  (TofinoConstants.event_generate_cid, TofinoEvent.generate_event);
  (TofinoEvent.event_delay_cid, TofinoEvent.delay_event);
  (TofinoSys.time_cid, TofinoSys.get_time)
  (* (IrTranslate.hash_builtin, IrBuiltinToDag.do_hash) *)
]

module TranslateEvents = struct 
  (* save a record of the event's definition to context, 
  using the same struct instance for input and output *)
  (* patch to support bg events with shared io structs *)
  let remember_event_def event_iid dec = 
    match dec.d with     
      | DEvent(evid, ev_sort, _, params) -> (
        let ev_cid = Cid.id evid in 
        let field_defs = vardefs_from_params params in         
        let instruct_name = TofinoStructs.full_in_struct_from_ev evid ev_sort in 
        let outstruct_name = instruct_name in 
        match ev_sort with 
          | EEntry _ ->
            ctx_add_eventrec ev_cid event_iid ev_sort field_defs (Some instruct_name) None
          | EExit ->
            ctx_add_eventrec ev_cid event_iid ev_sort field_defs (None) (Some outstruct_name)
          | EBackground ->
            ctx_add_eventrec ev_cid event_iid ev_sort field_defs (Some instruct_name) (Some outstruct_name)
      )
      | _ -> ()

  (* save a record of the event's definition to context, 
  using separate struct instances for input and output *)
  let remember_event_def_separate_bg_io event_iid dec = 
    match dec.d with     
      | DEvent(evid, ev_sort, _, params) -> (
        let ev_cid = Cid.id evid in 
        let field_defs = vardefs_from_params params in         
        let instruct_name = TofinoStructs.full_in_struct_from_ev evid ev_sort in 
        let outstruct_name = TofinoStructs.full_out_struct_from_ev evid ev_sort in 
        match ev_sort with 
          | EEntry _ ->
            ctx_add_eventrec ev_cid event_iid ev_sort field_defs (Some instruct_name) None
          | EExit ->
            ctx_add_eventrec ev_cid event_iid ev_sort field_defs (None) (Some outstruct_name)
          | EBackground ->
            ctx_add_eventrec ev_cid event_iid ev_sort field_defs (Some instruct_name) (Some outstruct_name)
      )
      | _ -> ()
  (* Generate all the data structures for an event, based on its record. 
      - entry events --> metadata struct + one input instance
      - exit events --> metadata struct + one output instance
      - bg events --> header struct + one input and one output instance 
      + all events --> event id const *)
  let structs_from_eventrec erec = 
      let struct_name = TofinoStructs.structname_from_evid erec.event_id in       
      let field_defs = erec.field_defs in 
      let ev_sort = erec.event_sort in 
      (* add the event metadata fields *)
      let ev_name_field = (event_id_field, event_id_width) in 
      let ev_delay_field = (event_delay_field, event_delay_width) in 
      let ev_loc_field = (event_loc_field, event_loc_width) in 
      let ev_mc_field = (event_mc_field, event_mc_width) in 
      let field_defs = ev_name_field::ev_mc_field::ev_loc_field::ev_delay_field::field_defs in 
      let instruct_opt = erec.in_struct in 
      let outstruct_opt = erec.out_struct in

      (* build the struct def *)
      trans_info ("generating struct def for event: "^(Id.to_string erec.event_id));
      let struct_def = match ev_sort with   
        | EBackground -> IS.new_header_structdef struct_name field_defs
        | _ -> IS.new_meta_structdef struct_name field_defs
      in 
      (* build the struct instances *)
      let in_structs = match instruct_opt with 
        | Some instruct_name -> [IS.new_struct struct_name Input instruct_name]
        | None -> []
      in 
      let out_structs = match outstruct_opt with 
        | Some outstruct_name -> [IS.new_struct struct_name Input outstruct_name]
        | None -> []
      in 
      (* patch to support bg events with shared io structs *)
      let iostructs = match (in_structs <> out_structs) with 
        | true -> in_structs@out_structs 
        | false -> in_structs
      in  
      (* build the const event iid def *)
      let event_iid_const = IS.new_public_constdef (TofinoStructs.defname_from_evid erec.event_id) event_id_width erec.event_iid in 
      event_iid_const::struct_def::iostructs
  ;;
  let structs_from_events () = 
      ctx_get_event_recs () |> CL.map structs_from_eventrec |> CL.flatten
  ;;

  let pnode_name_from_evid evid = 
    Cid.id (Id.prepend_string "parse_" evid)
  ;;

  let parsetree_from_events ds = 
    (*  1. for each entry event, 
        write a parse tree node with one statement: 
        that has one statement: 
     *)
    let to_pstate dec = 
      match dec.d with     
      | DEvent(evid, EBackground, _, _) -> (
        (* 
          generate the parse state. Currently very simple. 
            state parse_dpt_extra_processing_in_t {
                pkt.extract(hdr.dpt_extra_processing_in);
                transition accept;
             }
        *)
        let pnode_name = pnode_name_from_evid evid in 
        let in_struct_name = TofinoStructs.full_in_struct_from_ev evid EBackground in 
        let parse_instr = IS.new_PStruct in_struct_name in 
        let next_instr = IS.new_PNext (None) in 
        let pnode = IS.new_parse_node pnode_name [parse_instr] next_instr in 
        Some ((evid, pnode_name), pnode)
      )
      | _ -> None
    in 
    let ev_tups = CL.filter_map to_pstate ds in 

    (* construct the root node. *)
    (* peek at the next n bits to find the event ID to execute. *)
    let root_parse_instr = IS.new_PPeek handle_selector_name event_id_width in   
    (* branch on the extracted handle selector *)
    let to_root_branch ((evid, pnode_name), _) = 
      (* this should be matching on the event's iid, not its name *)
      let ev_iid = ctx_find_event_iid (Cid.id evid) in 
      IS.new_SConst_branch ev_iid (Some pnode_name)
      (* IS.new_SConstVar_branch (Cid.id evid) (Some pnode_name) *)
    in 
    let root_branches = CL.map to_root_branch ev_tups in 
    let root_parse_transition = IS.new_PSelect 
      handle_selector_name 
      root_branches
    in 
    (* assemble root node *)
    let root_node = IS.new_parse_node
      parse_root_name
      [root_parse_instr]
      root_parse_transition
    in 
    IS.new_ParseTree lucid_parser_name (root_node::(snd (CL.split ev_tups)))
  ;;
end (* TranslateEvents *)

module TranslateHandlers = struct 

  let rename_handler_params_inner hdl_id params stmt = 
    (* build a map from handler parameter name to qualified input struct field name *)
    let param_cids = CL.split params |> fst |> CL.map Cid.id in 
    let qual_fieldnames = TofinoStructs.qual_in_fieldnames_of_event hdl_id in 
    let qual_fieldname_exps = CL.map (fun fname -> exp (EVar fname)) qual_fieldnames in 
    let paramcid_to_qualfieldname = CL.combine param_cids qual_fieldname_exps in 

    (* replace the parameter cids wherever they appear in the handler's body *)
    let fold_f stmt (paramcid, fnameexp) = 
      replace_in_stmt stmt paramcid fnameexp
    in 
    (* problem: this doesn't replace parameter cids that appear on the left hand side of an assign. 
        do we need to? 
          - 
    *)
    let orig_stmt = stmt in 
    let stmt = CL.fold_left fold_f stmt paramcid_to_qualfieldname in 
    !dprint_endline ("----------------");
    !dprint_endline ("renamed handler statement for "^(Id.to_string hdl_id)^"("^(Printing.params_to_string params)^"){");
    !dprint_endline (Printing.stmt_to_string orig_stmt);
    !dprint_endline (" --> ");
    !dprint_endline (Printing.stmt_to_string stmt);
    !dprint_endline ("----------------");
    stmt
  ;;

  (* rename the parameters of the handler with references to the event's input struct's fields *)
  let rename_handler_params dec = 
    match dec.d with 
      | DHandler(hdl_id, (params, stmt)) -> 
        handler_sp hdl_id params 
          (rename_handler_params_inner hdl_id params stmt) 
          dec.dspan
      | _ -> dec 
  ;;
end 

(**** register array declarations ****)
let regdec_from_decl dec = 
  match dec.d with 
    | DGlobal(reg_id, (_, [sz]), _, args) -> (
      print_endline ("translating register declaration: "^(Printing.decl_to_string dec));
      let reg_id = Cid.id reg_id in 
      (* reg, width, length, ??? *)
      let arg_name = exp (EVar(reg_id)) in 
      let arg_width = exp (EVal(vint  (extract_size sz) 8)) in 
      print_endline ("arg_width expr used as arg: "^(Printing.exp_to_string arg_width));
      let args = arg_name::arg_width::args in 
      let result = ctx_call_codegen 
        TofinoArray.array_create_cid
        {
          basename = None;
          retname = Some reg_id;
          args = args;
        }
      in 
      let decl = CL.hd result.objs in 
      print_endline ("regdec: "^(IS.show_decl decl ));
      [decl]
    )
    | _ -> []
;;

let constdec_from_decl dec = 
  match dec.d with 
    | DConst(const_id, ty, exp) ->
      let width = width_from_ty ty in 
      let rhs_val = Integer.to_int (TofinoOp.zint_from_evalue exp) in 
      Some (IS.new_private_constdef (Cid.Id const_id) width rhs_val)
    | _ -> None
;;

(* declare groups as 16-bit ints. (todo: integrate groups and mc in distribution layer) *)
let cur_group_iid = ref 0 ;;
let groupdec_from_decl dec = 
  match dec.d with 
    | DGroup(group_id, _) -> 
      let width = TofinoConstants.event_loc_width in 
      cur_group_iid := (!cur_group_iid + 1);
      let giid = !cur_group_iid in 
      Some (IS.new_private_constdef (Cid.Id group_id) width giid)
    | _ -> None
;;

(* generate structure definitions and instances for private DPT metadata and headers. *)
let gen_dpt_structs () = 
  (* dptMeta *)
  let struct_cid = Cid.create ["dptMeta_t"] in
  let cid = Cid.create [dpt_meta_str] in 
  let fnames = CL.map (fun f -> Cid.create [f]) [timestamp_str; handle_selector_str; exit_event_str; next_event_str] in  
  let fwidths = [32; 8; 8; 8] in 
  let fdefs = CL.combine fnames fwidths in 
  let dptMeta_struct = IS.new_meta_structdef struct_cid fdefs in 
  let dptMeta_instance = IS.new_struct struct_cid Temporary cid in 
  (* the p4t printer will automatically link the instance to the struct based on struct_cid *)
  [dptMeta_struct; dptMeta_instance]
;;

let log_current_prog fn ds = 
  let full_fn = (!BackendLogging.irLogDir)^"/"^fn in 
  Printf.fprintf (open_out full_fn) "%s" (Printing.decls_to_string ds)
;;

let from_dpt ds : IS.instrProg = 
    (* translation to IR currently does many passes over the backend, with each pass 
    translating a different part of the syntax tree. 5/18 -- now that the final 
    structure of the generated code is more concretely defined, we can redo this 
    to translate in a single pass. This would make the code clearer and also make it 
    easier to reason about the translation's completeness. *)
    DBG.start_mlog __FILE__ outc dprint_endline;
    TofinoOp.start_logging ();
    TofinoContext.start_logging ();

    (* make sure all the event parameters have unique ids *)
    let ds = refresh_event_param_ids ds in 

    log_current_prog "after_refresh_event_params.dpt" ds;

    (* put builtin function generators into the context *)
    ctx_add_codegens dpt_builtin_fcns;

    (* put decls, including memops, into the context *)
    ctx_add_decls ds;

    (* put event records in the context *)
    CL.iteri TranslateEvents.remember_event_def ds;

    (* generate backend defs for event structs *)
    let struct_defs = TranslateEvents.structs_from_events () in 

    let dpt_struct_defs = gen_dpt_structs () in 

    (* generate parser for entry event instances. *)
    let parse_def = TranslateEvents.parsetree_from_events ds in 

    (* generate backend defs for register arrays *)
    let regarray_defs = CL.map regdec_from_decl ds |> CL.flatten in 

    (* generate constants *)    
    let const_defs = CL.filter_map constdec_from_decl ds in 

    (* generate constants for groups *)
    let group_defs = CL.filter_map groupdec_from_decl ds in 

    (* In the body of each handler, replace parameter variables with struct instance fields. *)
    let ds = CL.map TranslateHandlers.rename_handler_params ds in 

    log_current_prog "after_rename_handler_params.dpt" ds;


    (* convert handler statement trees into operation-statement graphs *)
    let opgraph_recs = CL.filter_map opgraph_from_handler ds in 

    (* translate operation statements into backend compute objects, 
       use the opgraphs to set control flow between objects. *)
    let backend_handler_defs = CL.map dpahandler_from_handler opgraph_recs in 

    (* combine the backend handler definitions into a single program *)
    let tofino_prog = merge_handler_defs backend_handler_defs in 

    (* generate the hand-written lucid scheduler blocks *)
    let sched_defs = TofinoScheduler.generate ds in 

    (* generate the controller-installed configurations *)
    let control_defs = TofinoConfig.generate ds in 

    (* put combine all the ir compute objects in the main instr dict *)
    let out_prog = {tofino_prog with 
      instr_dict = 
        (tofino_prog.instr_dict)
        @(IS.dict_of_decls const_defs)
        @(IS.dict_of_decls group_defs)
        @(IS.dict_of_decls regarray_defs)
        @(IS.dict_of_decls struct_defs)
        @(IS.dict_of_decls dpt_struct_defs)
        @(IS.dict_of_decls [parse_def])
        @(IS.dict_of_decls sched_defs)
        @(IS.dict_of_decls control_defs);
    } in 

    let iter_f dec = 
      !dprint_endline ("------declaration------- ");
      !dprint_endline (IS.show_decl dec);
      !dprint_endline ("------object-------------");
      PrintUtils.open_block ();
      fprintf str_formatter " @,";
      P4tPrint.PrintComputeObject.print_decls [dec];
      !dprint_endline (PrintUtils.close_block ())
    in 
    !dprint_endline ("----OBJECTS GENERATED----");
    let ir_objs = snd (CL.split out_prog.instr_dict) in 
    CL.iter iter_f ir_objs;
    

    (*let print_decl_dict_entry (cid, dec) = 
      let obj_id_str = ("object id: "^(P4tPrint.str_of_private_oid cid)) in 
      let obj_str = PrintComputeObject.print_decls decls
      let outs = Printf.sprintf "object id: %s object: %s" (P4tPrint.str_of_private_oid cid) (IS.show_decl dec) in 
      !dprint_endline (outs)
    in  *)
    (* CL.iter print_decl_dict_entry out_prog.instr_dict; *)
    !dprint_endline ("----END OBJECTS GENERATED----");

    out_prog 
;;