(* DPT to Tofino machine grammar
    new version, following style of fromIr... *)
(*
    Simple implementation.
    1. convert each handler into an operation-statement graph.
    2. translate each operation-statement into a graph.
*)
open MiscUtils
open Batteries
open Format
open OGSyntax
open InterpHelpers
open LLConstants
open LLContext
open LLOp
open LLValidate
open LogIr
module CL = Caml.List
open CoreSyntax
module Printing = CorePrinting
module IS = LLSyntax

(* logging *)
module DBG = BackendLogging

let outc = ref None
let dprint_endline = ref DBG.no_printf

(* code generators for builtin functions *)
let dpt_builtin_fcns =
  [ LLArray.array_create_cid, LLArray.create_array
  ; LLArray.array_update_cid, LLArray.update_array
  ; LLArray.array_set_cid, LLArray.set_array
  ; LLArray.array_get_cid, LLArray.get_array
  ; LLArray.array_setm_cid, LLArray.setm_array
  ; LLArray.array_getm_cid, LLArray.getm_array
  ; LLConstants.event_generate_cid, LLEvent.generate_event
  ; LLEvent.event_delay_cid, LLEvent.delay_event
  ; LLSys.time_cid, LLSys.get_time
    (* (IrTranslate.hash_builtin, IrBuiltinToDag.do_hash) *) ]
;;

module TranslateEvents = struct
  (* save a record of the event's definition to context,
  using the same struct instance for input and output.
  Also, remember the parameter ids of the event's handler. *)
  let remember_event_def event_iid dec =
    let event_iid = event_iid + 1 in
    (* start at event id 1, not 0 *)
    match dec.d with
    | DEvent (evid, ev_sort, params) ->
      let ev_cid = Cid.id evid in
      let field_defs = vardefs_from_params params in
      let struct_cid = TofinoStructs.full_struct_from_ev evid ev_sort in
      ctx_add_eventrec ev_cid event_iid ev_sort field_defs struct_cid
    (*       (match ev_sort with
      | EEntry _ ->
        ctx_add_eventrec
          ev_cid
          event_iid
          ev_sort
          field_defs
          (Some instruct_name)
          None
      | EExit ->
        ctx_add_eventrec
          ev_cid
          event_iid
          ev_sort
          field_defs
          None
          (Some outstruct_name)
      | EBackground ->
        ctx_add_eventrec
          ev_cid
          event_iid
          ev_sort
          field_defs
          (Some instruct_name)
          (Some outstruct_name)) *)
    | DHandler (hdl_id, (params, _)) ->
      ctx_set_hdl_param_ids (Cid.id hdl_id) (CL.split params |> fst)
    | _ -> ()
  ;;

  (* Generate all the data structures for an event, based on its record.
      1. a struct.
      2. a struct instance.
      3. a constant e_<eventname>. *)
  let structs_from_eventrec erec =
    let struct_name = TofinoStructs.structname_from_evid erec.event_id in
    let field_defs = erec.field_defs in
    let ev_sort = erec.event_sort in
    (* add the event metadata fields *)
    let ev_name_field = event_id_field, event_id_width in
    let ev_delay_field = event_delay_field, event_delay_width in
    let ev_loc_field = event_loc_field, event_loc_width in
    let ev_mc_field = event_mc_field, event_mc_width in
    let field_defs =
      ev_name_field
      :: ev_mc_field
      :: ev_loc_field
      :: ev_delay_field
      :: field_defs
    in
    (* build the struct def *)
    trans_info ("generating struct def for event: " ^ Id.to_string erec.event_id);
    let struct_def =
      match ev_sort with
      | EBackground -> IS.new_header_structdef struct_name field_defs
      | _ -> IS.new_meta_structdef struct_name field_defs
    in
    (* build the struct instance *)
    let structs = [IS.new_struct struct_name SPublic erec.event_struct] in
    (* build the const event iid def *)
    let event_iid_const =
      IS.new_public_constdef
        (TofinoStructs.defname_from_evid erec.event_id)
        event_id_width
        erec.event_iid
    in
    event_iid_const :: struct_def :: structs
  ;;

  let structs_from_events () =
    ctx_get_event_recs () |> CL.map structs_from_eventrec |> CL.flatten
  ;;

  let pnode_name_from_evid evid = Cid.id (Id.prepend_string "parse_" evid)

  let parsetree_from_events ds =
    (*  1. for each entry event,
        write a parse tree node with one statement:
        that has one statement:
     *)
    let to_pstate dec =
      match dec.d with
      | DEvent (evid, EBackground, _) ->
        (*
          generate the parse state. Currently very simple.
            state parse_dpt_extra_processing_in_t {
                pkt.extract(hdr.dpt_extra_processing_in);
                transition accept;
             }
        *)
        let pnode_name = pnode_name_from_evid evid in
        let in_struct_name =
          TofinoStructs.full_in_struct_from_ev evid EBackground
        in
        let parse_instr = IS.new_PStruct in_struct_name in
        let next_instr = IS.new_PNext None in
        let pnode = IS.new_parse_node pnode_name [parse_instr] next_instr in
        Some ((evid, pnode_name), pnode)
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
      (* IS.new_SDConst_branch (Cid.id evid) (Some pnode_name) *)
    in
    let root_branches = CL.map to_root_branch ev_tups in
    let root_parse_transition =
      IS.new_PSelect handle_selector_name root_branches
    in
    (* assemble root node *)
    let root_node =
      IS.new_parse_node parse_root_name [root_parse_instr] root_parse_transition
    in
    IS.new_ParseTree lucid_parser_name (root_node :: snd (CL.split ev_tups))
  ;;
end

let get_dglobal_info ty e =
  let sz =
    match ty.raw_ty with
    | TName (_, [sz], _) -> sz
    | _ -> error "Bad DGlobal"
  in
  let args =
    match e.e with
    | ECall (_, args) -> args
    | _ -> error "Bad DGlobal"
  in
  sz, args
;;

(**** register array declarations ****)
let regdec_from_decl dec =
  match dec.d with
  | DGlobal (reg_id, ty, e) ->
    print_endline
      ("translating register declaration: " ^ Printing.decl_to_string dec);
    let sz, args = get_dglobal_info ty e in
    let reg_id = Cid.id reg_id in
    (* reg, width, length, ??? *)
    let arg_name = exp (EVar reg_id) in
    let arg_width = exp (EVal (vint sz 8)) in
    print_endline
      ("arg_width expr used as arg: " ^ Printing.exp_to_string arg_width);
    let args = arg_name :: arg_width :: args in
    let result =
      ctx_call_codegen
        LLArray.array_create_cid
        { hdl_id = None
        ; (* global code, no handler context. *)
          basename = None
        ; retname = Some reg_id
        ; args
        }
    in
    let decl = CL.hd result.objs in
    print_endline ("regdec: " ^ IS.show_decl decl);
    [decl]
  | _ -> []
;;

(* declare groups as 16-bit ints. (todo: integrate groups and mc in distribution layer) *)
let cur_group_iid = ref 0

let groupdec_from_decl dec =
  match dec.d with
  | DGroup (group_id, _) ->
    let width = LLConstants.event_loc_width in
    cur_group_iid := !cur_group_iid + 1;
    let giid = !cur_group_iid in
    Some (IS.new_private_constdef (Cid.Id group_id) width giid)
  | _ -> None
;;

(* generate structure definitions and instances for private DPT metadata and headers. *)
let gen_internal_structs () =
  (* dptMeta *)
  let struct_cid = Cid.create ["dptMeta_t"] in
  let fnames =
    CL.map
      (fun f -> Cid.create [f])
      [ timestamp_str
      ; handle_selector_str
      ; exit_event_str
      ; next_event_str
      ; events_count_str ]
  in
  let fwidths = [32; 8; 8; 8; 8] in
  let fdefs = CL.combine fnames fwidths in
  let dptMeta_struct = IS.new_meta_structdef struct_cid fdefs in
  let dptMeta_instance =
    IS.new_struct struct_cid SPrivate dpt_meta_struct_instance
  in
  (* the p4t printer will automatically link the instance to the struct based on struct_cid *)
  [dptMeta_struct; dptMeta_instance]
;;

(* Give all the spans in a program a unique id. *)
let cur_span = ref 0

let refresh_span sp =
  cur_span := !cur_span + 1;
  { sp with Span.spid = !cur_span }
;;

let make_unique_spans ds =
  let v =
    object
      inherit [_] s_map as super

      method! visit_sp _ sp = refresh_span sp
    end
  in
  v#visit_decls () ds
;;

let public_cid_id = ref 0

let fresh_pad_cid () =
  public_cid_id := !public_cid_id + 1;
  "pad_" ^ string_of_int !public_cid_id ^ "_meta" |> Id.fresh |> Cid.id
;;

let byte_align_header_structs (prog : IS.llProg) : IS.llProg =
  (* add pads so that every 8-bit field
     in every header struct starts on a byte-boundary, and
     every header struct ends on an byte-boundary. *)

  (* pad fields so they end on byte boundary *)
  let pad_tail fields =
    let fields_width = CL.split fields |> snd |> sum in
    match fields_width mod 8 with
    | 0 -> fields
    (* pad by 8-n *)
    | n ->
      let pad_id, pad_width = fresh_pad_cid (), 8 - n in
      fields @ [pad_id, pad_width]
  in
  let byte_align_fields fields field =
    match snd field mod 8 with
    (* field is byte aligned, make sure prior fields are aligned before adding*)
    | 0 -> pad_tail fields @ [field]
    | _ ->
      (* field is not byte aligned, just add it *)
      fields @ [field]
  in
  let byte_align_header_struct (cid, decl) =
    match decl with
    | IS.StructDef (mid, struct_type, fields) ->
      (match struct_type with
      | IS.SHeader ->
        let aligned_fields =
          CL.fold_left byte_align_fields [] fields |> pad_tail
        in
        cid, IS.StructDef (mid, struct_type, aligned_fields)
      | IS.SMeta -> cid, decl)
    | _ -> cid, decl
  in
  let cid_decls = CL.map byte_align_header_struct prog.instr_dict in
  { prog with IS.instr_dict = cid_decls }
;;

let from_dpt (ds : decls) (opgraph_recs : prog_opgraph) : IS.llProg =
  (* translation to IR currently does many passes over the backend, with each pass
    translating a different part of the syntax tree.
    5/18/21 -- now that the final structure of the generated code
    is more concretely defined, we can redo this to translate in a single pass.
    This would make the code clearer and easier to extend. *)
  DBG.start_mlog __FILE__ outc dprint_endline;
  LLOp.start_logging ();
  LLContext.start_logging ();
  (* put builtin function generators into the context *)
  ctx_add_codegens dpt_builtin_fcns;
  (* put decls, including memops, into the context *)
  ctx_add_decls ds;
  (* put event records in the context *)
  CL.iteri TranslateEvents.remember_event_def ds;
  (* generate struct declarations, struct instances, and constants from events. *)
  let struct_defs = TranslateEvents.structs_from_events () in
  (* generate struct declarations and instances for private Lucid-runtime only data. *)
  let dpt_struct_defs = gen_internal_structs () in
  (* generate parser for entry event instances. *)
  let parse_def = TranslateEvents.parsetree_from_events ds in
  (* generate backend defs for register arrays *)
  let regarray_defs = CL.map regdec_from_decl ds |> CL.flatten in
  (* generate constants for groups *)
  let group_defs = CL.filter_map groupdec_from_decl ds in
  (* translate operation statements into backend compute objects,
       use the opgraphs to set control flow between objects. *)
  let backend_handler_defs = CL.map dpahandler_from_handler opgraph_recs in
  (* combine the backend handler definitions into a single program *)
  let tofino_prog = merge_handler_defs backend_handler_defs in
  (* generate the hand-written lucid scheduler blocks *)
  let sched_defs = LLScheduler.generate ds in
  (* generate the controller-installed configurations *)
  let control_defs = LLStartup.generate ds in
  (* put combine all the ir compute objects in the main instr dict *)
  let out_prog =
    { tofino_prog with
      instr_dict =
        tofino_prog.instr_dict
        @ IS.dict_of_decls group_defs
        @ IS.dict_of_decls regarray_defs
        @ IS.dict_of_decls struct_defs
        @ IS.dict_of_decls dpt_struct_defs
        @ IS.dict_of_decls [parse_def]
        @ IS.dict_of_decls sched_defs
        @ IS.dict_of_decls control_defs
    }
  in
  (* pad header structs *)
  let out_prog = byte_align_header_structs out_prog in
  (*   !dprint_endline "-----object ids in cid decls at end of LLTranslate-----";
  !dprint_endline (DebugPrint.ids_in_cid_decls out_prog.instr_dict);
  !dprint_endline "-----object decls in cid decls at end of LLTranslate-----";
  !dprint_endline (DebugPrint.str_of_cid_decls out_prog.instr_dict);
  !dprint_endline
    "-----end object decls in cid decls at end of LLTranslate-----"; *)
  LLValidate.validate_cid_decls out_prog.instr_dict "[LLTranslate] end";
  out_prog
;;
