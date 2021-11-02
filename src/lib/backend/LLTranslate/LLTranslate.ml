(* DPT to Tofino machine grammar
    new version, following style of fromIr... *)
(*
    Simple implementation.
    1. convert each handler into an operation-statement graph.
    2. translate each operation-statement into a graph.
*)
open SyntaxUtils
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
open Syntax
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

(* code generators for events *)
module TranslateEvents = struct
  let event_out_flags_struct = Cid.create ["ev_out_flags_t"]
  (* assumes that we link into the struct 
     with id md_instance, which is declared externally. *)
  let event_out_flags_instance = Cid.create_ids [md_instance;  Id.create "ev_out_flags"]
  ;;
  
  (* translate the ith event declaration into LL code, 
     and save details about the event in the LL syntax. 
     This should be the ONLY FUNCTION that writes to event records. *)
  let translate (ll_decls, last_eviid) dec = 
    match dec.d with
    | DEvent (evid, ev_sort, _, params) ->
      (* all the details about the event that the translator 
         needs, in one context record. *)
      let erec =
        { event_id = evid
        ; field_defs = vardefs_from_params params
        ; event_iid = last_eviid + 1 (* start event ids at 1 *)
        ; hdl_param_ids = [] (* filled in by DHandler match *)
        ; event_sort = ev_sort
        ; event_struct = TofinoStructs.structname_from_evid evid
        ; event_struct_instance = TofinoStructs.full_struct_from_ev evid ev_sort 
        ; event_generated_flag = Cid.concat event_out_flags_instance (Cid.id evid)
        }
      in 
      (* add the event to the context. *)
      ctx_add_erec erec;
      (* generate IR code for the event *)
      (* all the fields in the event's struct *)
      let all_event_fields = 
          (* hidden event parameters for lucid runtime *)
           (event_id_field, event_id_width)
        :: (event_mc_field, event_mc_width)
        :: (event_loc_field, event_loc_width)
        :: (event_delay_field, event_delay_width)
        (* user-declared event parameters *)
        :: erec.field_defs
      in 
      let struct_ty = match erec.event_sort with 
        | EBackground -> IS.SHeader  (* backround events get serialized to packet *)
        | EEntry _ | EExit -> IS.SMeta (* entry and exit events don't. kind of backwards seeming. *)
      in 
      (* declaration of the struct, instance, and enum #define *)
      let ev_struct_decl = IS.new_structdef erec.event_struct struct_ty all_event_fields in 
      let ev_struct_inst = IS.new_struct erec.event_struct SPublic erec.event_struct_instance in 
      let ev_enum = IS.new_public_constdef
        (TofinoStructs.defname_from_evid erec.event_id)
        event_id_width
        erec.event_iid
      in 
      ll_decls@[ev_struct_decl; ev_struct_inst; ev_enum], erec.event_iid

    (* record the mapping from event to handler *)
    | DHandler (hdl_id, (params, _)) ->
      ctx_set_hdl_param_ids (Cid.id hdl_id) (CL.split params |> fst);
      ll_decls, last_eviid (* nothing to generate, just updating context *)
    | _ -> ll_decls, last_eviid (* nothing new *)
  ;;

  (* create the struct and instance of the event_generated bitvector *)
  let event_triggered_bv () = 
    let to_generate_flag_field ev = 
      (Cid.last_id ev.event_generated_flag |> Cid.id), 1
    in 
    let field_defs = ctx_get_event_recs ()
      |> CL.map to_generate_flag_field 
    in 
    [ IS.new_meta_structdef event_out_flags_struct field_defs
    ; IS.new_struct event_out_flags_struct SPrivate event_out_flags_instance
    ] 
  ;;

  (* translate all the event declarations and fill the context. *)
  let translate_all ds = 
    let event_decls = CL.fold_left translate ([], 0) ds |> fst in 
    (* generate the event_out flag struct *)
    let event_out_bv_decls = event_triggered_bv () in 
    event_decls@event_out_bv_decls
  ;;

  (*** parse generator for background events ***)
  let pnode_name_from_evid evid = Cid.id (Id.prepend_string "parse_" evid)

  let parsetree_from_events ds =
    (*  1. for each entry event,
        write a parse tree node with one statement:
        that has one statement:
     *)
    let to_pstate dec =
      match dec.d with
      | DEvent (evid, EBackground, _, _) ->
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
    match TyTQVar.strip_links ty.raw_ty with
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
    let arg_width = exp (EVal (vint (extract_size sz) 8)) in
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

let constdec_from_decl dec =
  match dec.d with
  | DConst (const_id, ty, exp) ->
    let width = width_from_ty ty in
    let rhs_val = Integer.to_int (LLOp.zint_from_evalue exp) in
    Some (IS.new_private_constdef (Cid.Id const_id) width rhs_val)
  | _ -> None
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

(* generate the bitvector metadata that indicate which 
   subset of events were generated. *)
let gen_event_triggered_bitvec () = 
  let struct_cid = Cid.create ["outEvents_t"] in 
  let instance_cid = Cid.create [md_instance_prefix; "outEvents"] in
  let event_id_to_bitfield er = 
    Cid.create [Id.to_string er.event_id^"_generated"]
  in   
  let fields = ctx_get_event_recs ()
    |> CL.map event_id_to_bitfield
  in 
  let widths = ctx_get_event_recs () |> CL.map (fun _ -> 1) in 
  let field_defs = CL.combine fields widths in
  let newstruct = IS.new_meta_structdef struct_cid field_defs in
  let newinstance = IS.new_struct struct_cid SPrivate instance_cid in 
  [newstruct; newinstance]  
;;

(* generate structure definitions and instances for private DPT metadata and headers. *)
let gen_internal_structs () =
  (* dptMeta *)
  let struct_cid = Cid.create ["dptMeta_t"] in
  let fnames =
    CL.map
      (fun f -> Cid.create [f])
      [timestamp_str; handle_selector_str; exit_event_str; next_event_str; events_count_str]
  in
  let fwidths = [32; 8; 8; 8; 8] in
  let fdefs = CL.combine fnames fwidths in
  let dptMeta_struct = IS.new_meta_structdef struct_cid fdefs in
  let dptMeta_instance =
    IS.new_struct struct_cid SPrivate dpt_meta_struct_instance
  in
  (* the p4t printer will automatically link the instance to the struct based on struct_cid *)
  (* the event active bit vector *)
  (* *)

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


let public_cid_id = ref 0;;

let fresh_pad_cid () = 
  public_cid_id := !public_cid_id + 1;
  "pad_"^(string_of_int (!public_cid_id))^"_meta" |> Id.fresh |> Cid.id
;;

let byte_align_header_structs (prog : IS.llProg) : IS.llProg = 
  (* add pads so that every 8-bit field 
     in every header struct starts on a byte-boundary, and 
     every header struct ends on an byte-boundary. *) 



  (* pad fields so they end on byte boundary *)
  let pad_tail fields = 
    let fields_width = CL.split fields |> snd |> sum in 
    match (fields_width mod 8) with 
    | 0 ->  fields
    (* pad by 8-n *)
    | n ->
      let (pad_id, pad_width) = (fresh_pad_cid (), (8 - n)) in 
      fields@[(pad_id, pad_width)]    
  in 
  let byte_align_fields fields field = 
    match ((snd field) mod 8) with 
      (* field is byte aligned, make sure prior fields are aligned before adding*)
      | 0 ->
          (pad_tail fields)@[field]
      | _ -> 
        (* field is not byte aligned, just add it *)    
        fields@[field]
  in 
  let byte_align_header_struct (cid, decl) = 
    match decl with 
      | IS.StructDef(mid, struct_type, fields) -> (
        match struct_type with 
        | IS.SHeader -> 
          let aligned_fields = 
            CL.fold_left byte_align_fields [] fields
            |> pad_tail
           in 
        (cid, IS.StructDef(mid, struct_type, aligned_fields))
        | IS.SMeta -> (cid, decl)
      )
      | _ -> (cid, decl)
  in 
  let cid_decls = CL.map byte_align_header_struct prog.instr_dict in 
  {prog with IS.instr_dict=cid_decls}
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
  (* put source decls into the context *)
  ctx_add_decls ds;
  (* put event records in the context and generate 
     all the LL code for event declarations. *)
  let event_decls = TranslateEvents.translate_all ds in 
  (* generate struct declarations and instances for 
     other private Lucid-runtime only data. *)
  let dpt_struct_defs = gen_internal_structs () in
  (* generate parser for entry event instances. *)
  let parse_def = TranslateEvents.parsetree_from_events ds in
  (* generate backend defs for register arrays *)
  let regarray_defs = CL.map regdec_from_decl ds |> CL.flatten in
  (* generate constants *)
  let const_defs = CL.filter_map constdec_from_decl ds in
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
        @ IS.dict_of_decls const_defs
        @ IS.dict_of_decls group_defs
        @ IS.dict_of_decls regarray_defs
        @ IS.dict_of_decls event_decls
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
  LLValidate.validate_cid_decls
    out_prog.instr_dict
    "[LLTranslate] end";  

  out_prog

;;
