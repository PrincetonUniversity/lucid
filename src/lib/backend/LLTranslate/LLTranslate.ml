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
let backend_report str = Console.show_message str ANSITerminal.Green "Backend"

module DBG = BackendLogging
let outc = ref None
let dprint_endline = ref DBG.no_printf
let start_logging () = DBG.start_mlog __FILE__ outc dprint_endline

(* code generators for builtin functions *)
let dpt_builtin_fcns =
  [ LLArray.array_create_cid, LLArray.create_array
  ; LLArray.array_set_cid, LLArray.set_array
  ; LLArray.array_get_cid, LLArray.get_array
  ; LLArray.array_setm_cid, LLArray.setm_array
  ; LLArray.array_getm_cid, LLArray.getm_array
  ; LLArray.array_update_cid, LLArray.update_array
  ; LLArray.array_update_complex_cid, LLArray.update_complex
  ; LLConstants.generate_self_cid, LLEvent.generate_self
  ; LLConstants.generate_port_cid, LLEvent.generate_port
  ; LLEvent.event_delay_cid, LLEvent.delay_event
  ; LLSys.time_cid, LLSys.get_time
  ; LLSys.random_cid, LLSys.get_random
    (* (IrTranslate.hash_builtin, IrBuiltinToDag.do_hash) *) ]
;;

(* lucid's internal metadata struct for ingress processing *)
let lucid_internal_struct = 
  let struct_cid = Cid.create ["dptMeta_t"] in
  let struct_instance_cid = dpt_meta_struct_instance in 
  let struct_fields =
    CL.map
      (fun (f, w) -> (Cid.create [f], w))
      (* field names and widths are defined in LLConstants. *)
      [ timestamp_str, timestamp_width
      ; handle_selector_str, handle_selector_width
      ; exit_event_str, exit_event_width
      ; next_event_str, next_event_width
      ; events_count_str, events_count_width ]
  in 
  let dptMeta_struct = IS.new_meta_structdef struct_cid struct_fields in
  let dptMeta_instance =
    IS.new_struct struct_cid SPrivate struct_instance_cid
  in
  [dptMeta_struct; dptMeta_instance]
;;  

let hidden_event_fields = [
  (event_id_field, event_id_width); 
  (event_loc_field, event_loc_width); 
  (event_delay_field, event_delay_width)
]



(* code generators for events *)
module TranslateEvents = struct
  let undeclared_instance_name = P4tPrint.str_of_public_varid

  (* translate the ith event declaration into LL code,
     and save details about the event in the LL syntax.
     This should be the ONLY FUNCTION that writes to event records. *)
  let translate (ll_decls, last_eviid) dec =
    match dec.d with
    | DEvent (evid, ev_sort, params) ->
      (* all the details about the event that the translator
         needs, in one context record. *)
      let erec =
        { event_id = evid
        ; field_defs = vardefs_from_params params
        ; hidden_fields = hidden_event_fields
        ; event_iid = last_eviid + 1 (* start event ids at 1 *)
        ; hdl_param_ids = [] (* filled in by DHandler match *)
        ; event_sort = ev_sort
        ; event_struct = TofinoStructs.structname_from_evid evid
        ; event_struct_instance = TofinoStructs.full_struct_from_ev evid ev_sort
        ; event_generated_flag =
            Cid.concat event_out_flags_instance (Cid.id evid)
        }
      in
      (* add the event to the context. *)
      ctx_add_erec erec;
      (* generate IR code for the event *)
      (* all the fields in the event's struct *)
      let all_event_fields =
        (* hidden event parameters for lucid runtime *)
        (event_id_field, event_id_width)
        :: (event_loc_field, event_loc_width)
        :: (event_delay_field, event_delay_width)
        (* user-declared event parameters *)
        :: erec.field_defs
      in
      let struct_ty =
        match erec.event_sort with
        | EBackground ->
          IS.SHeader (* backround events get serialized to packet *)
        | EEntry _ | EExit -> IS.SMeta
        (* entry and exit events don't. kind of backwards seeming. *)
      in
      (* declaration of the struct, instance, and enum #define *)
      let ev_struct_decl =
        IS.new_structdef erec.event_struct struct_ty all_event_fields
      in
      let ev_struct_inst =
        IS.new_struct erec.event_struct SPublic erec.event_struct_instance
      in
      let ev_enum =
        IS.new_public_constdef
          (TofinoStructs.defname_from_evid erec.event_id)
          event_id_width
          erec.event_iid
      in
      ll_decls @ [ev_struct_decl; ev_struct_inst; ev_enum], erec.event_iid
    (* record the mapping from event to handler *)
    | DHandler (hdl_id, (params, _)) ->
      ctx_set_hdl_param_ids (Cid.id hdl_id) (CL.split params |> fst);
      ll_decls, last_eviid (* nothing to generate, just updating context *)
    | _ -> ll_decls, last_eviid
  ;;

  (* nothing new *)

  (* create the struct and instance of the event_generated bitvector *)
  let event_triggered_bv () =
    let to_generate_flag_field ev =
      Cid.last_id ev.event_generated_flag |> Cid.id, 1
    in
    let field_defs = ctx_get_event_recs () |> CL.map to_generate_flag_field in
    [ IS.new_header_structdef event_out_flags_struct field_defs
    ; IS.new_struct event_out_flags_struct SPrivate event_out_flags_instance ]
  ;;

  (* translate all the event declarations and fill the context. *)
  let translate_all ds =
    (* generate headers and header instances for events *)
    let event_decls = CL.fold_left translate ([], 0) ds |> fst in
    (* generate the event_out flag struct *)
    let event_out_bv_decls = event_triggered_bv () in
    event_decls @ [footer_struct; footer_instance] @ event_out_bv_decls
  ;;

  (*** parse generator for background events ***)
  let parse_start_cid = Cid.create ["start"]
  let parse_end_cid = Cid.create ["finish"]

  (* new *)
  let evrec_hdrvar evrec =
    TofinoStructs.full_struct_from_ev evrec.event_id EBackground
  ;;

  let evrec_pnodecid pos evrec =
    Cid.id
      (Id.prepend_string
         ("event_" ^ string_of_int pos ^ "_parse_")
         evrec.event_id)
  ;;

  let pos_selectorcid pos = Cid.create ["selector_" ^ string_of_int pos]

  let parsetree_from_events _ =
    (* create a parser to extract the evrec
       from the i'th event in the lucid header. *)
    let evrec_parse_node max_layers layer evrec =
      let parse_instr = IS.new_PStruct (evrec_hdrvar evrec) in
      let next_instr =
        match max_layers - 1 = layer with
        | true -> IS.new_PNext (Some parse_end_cid)
        | false -> IS.new_PNext (Some (pos_selectorcid (layer + 1)))
      in
      IS.new_parse_node (evrec_pnodecid layer evrec) [parse_instr] next_instr
    in
    (* create the ith selector node *)
    let pos_selector layer evrecs =
      let selector_cid =
        match layer with
        | 0 -> Cid.create ["start"]
        | _ -> pos_selectorcid layer
      in
      let peek_target_decl_cid, peek_target_cid =
        match layer with
        | 0 -> handle_selector_name, handle_selector_name
        | _ -> Cid.create ["bit<8> tmp"], Cid.create ["tmp"]
      in
      let extract_eventType =
        IS.new_PPeek peek_target_decl_cid event_id_width
      in
      let evrec_branch evrec =
        IS.new_SConst_branch evrec.event_iid (Some (evrec_pnodecid layer evrec))
      in
      let evrec_branches =
        IS.new_SConst_branch 0 (Some parse_end_cid)
        :: CL.map evrec_branch evrecs
      in
      IS.new_parse_node
        selector_cid
        [extract_eventType]
        (IS.new_PSelect peek_target_cid evrec_branches)
    in
    (* create all nodes for a layer *)
    let layer_nodes max_layers evrecs layer =
      pos_selector layer evrecs
      :: CL.map (evrec_parse_node max_layers layer) evrecs
    in
    (* create num_evs selectors and num_ev layers of parse nodes *)
    let evrecs =
      ctx_get_event_recs ()
      |> CL.filter (fun evr ->
             match evr.event_sort with
             | EBackground -> true
             | _ -> false)
    in
    let nodes =
      CL.map
        (layer_nodes max_generated_events evrecs)
        (range 0 max_generated_events)
      |> CL.flatten
    in
    (* end state just parses a single 8 bit header and exits (no next instr) *)
    let end_state =
      let parse_instr =
        IS.new_PStruct (TofinoStructs.qualify_struct footer IS.SHeader)
      in
      let flags_parse_instr = IS.new_PStruct event_out_flags_instance in
      let next_instr = IS.new_PNext None in
      IS.new_parse_node
        parse_end_cid
        [parse_instr; flags_parse_instr]
        next_instr
    in
    (*     print_endline ("HERE");
    printf "number of node states: %i\n" (CL.length nodes);
    exit 1;
 *)
    IS.new_ParseTree lucid_parser_name (nodes @ [end_state])
  ;;

  (* OLD *)
  let parsestate_name_of evid = Cid.id (Id.prepend_string "parse_" evid)

  let old_parsetree_from_events ds =
    (*
      Given lucid events a and b, construct a parse graph:

          <-- start -->
        /     / | \    \
       a --->   |  <--- b
                v
               end

      - the start node peeks at the next 8 bits to determine the event type.
      - an event type of 0 indicates "no more events".
      - node a and b extract the headers for events a and, then return to start.
      - the end node extracts the footer, whose first 8 bits should always be 0.

      new parse tree design:
      parse up to a fixed number of events. The graph to parse events n events with a
      maximum of k events per packet has O(nk) nodes and transitions. For example,
      the graph for 3 events (a, b, c) with a maximum of 2 events per packet
      looks like:
          start
            |
            v
        selector_1
        |   |   |
        v   v   v
        a1  b1  c1
        |   |   |
        v   v   v
        selector_2
        |   |   |
        v   v   v
        a2  b2  c2
        |   |   |
        v   v   v
        footer/end
    *)

    (* construct the end state. Just parses a single 8 bit header and exits (no next instr) *)
    let end_state =
      let parse_instr =
        IS.new_PStruct (TofinoStructs.qualify_struct footer IS.SHeader)
      in
      let flags_parse_instr = IS.new_PStruct event_out_flags_instance in
      let next_instr = IS.new_PNext None in
      IS.new_parse_node
        parse_end_cid
        [parse_instr; flags_parse_instr]
        next_instr
    in
    (* construct the start node. input is a mapping from enum values to parse states. *)
    let start_state enum_to_state =
      (* peek at the next n bits to find the event ID to execute. *)
      let root_parse_instr = IS.new_PPeek handle_selector_name event_id_width in
      (* branch on the extracted handle selector *)
      let to_root_branch (evid, pnode_name) =
        let ev_iid = ctx_find_event_iid (Cid.id evid) in
        IS.new_SConst_branch ev_iid (Some pnode_name)
      in
      let event_branches = CL.map to_root_branch enum_to_state in
      let branches =
        IS.new_SConst_branch 0 (Some parse_end_cid) :: event_branches
      in
      let selector = IS.new_PSelect handle_selector_name branches in
      (* assemble node *)
      IS.new_parse_node parse_start_cid [root_parse_instr] selector
    in
    (*
      generate parse state for evid and transition to accept.
        state parse_<evid> {
            pkt.extract(hdr.dpt_<evid>);
            transition start;
         }
    *)
    let event_state dec =
      match dec.d with
      | DEvent (evid, EBackground, _) ->
        let parse_state_id = parsestate_name_of evid in
        let in_struct_name =
          TofinoStructs.full_struct_from_ev evid EBackground
        in
        let parse_instr = IS.new_PStruct in_struct_name in
        (* always transition back to start *)
        let next_instr = IS.new_PNext (Some parse_start_cid) in
        (* let next_instr = IS.new_PNext None in *)
        let pnode = IS.new_parse_node parse_state_id [parse_instr] next_instr in
        Some ((evid, parse_state_id), pnode)
      | _ -> None
    in
    let enum_map, event_states = CL.filter_map event_state ds |> CL.split in
    let start_state = start_state enum_map in
    IS.new_ParseTree
      lucid_parser_name
      ((start_state :: event_states) @ [end_state])
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
    let sz, args = get_dglobal_info ty e in
    let reg_id = Cid.id reg_id in
    (* reg, width, length, ??? *)
    let arg_name = exp (EVar reg_id) ty in
    let arg_width = exp (EVal (vint sz 8)) (TInt sz |> CoreSyntax.ty) in
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
    [decl]
  | _ -> []
;;

(* generate the bitvector metadata that indicate which
   subset of events were generated. *)
let gen_event_triggered_bitvec () =
  let struct_cid = Cid.create ["outEvents_t"] in
  let instance_cid = Cid.create [md_instance_prefix; "outEvents"] in
  let event_id_to_bitfield er =
    Cid.create [Id.to_string er.event_id ^ "_generated"]
  in
  let fields = ctx_get_event_recs () |> CL.map event_id_to_bitfield in
  let widths = ctx_get_event_recs () |> CL.map (fun _ -> 1) in
  let field_defs = CL.combine fields widths in
  let newstruct = IS.new_header_structdef struct_cid field_defs in
  let newinstance = IS.new_struct struct_cid SPrivate instance_cid in
  [newstruct; newinstance]
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

(* Compiles ds, a program with a single event/handler named "main" and no 
   generate statements, into P4. 
    Changes: 
      1. don't generate the event selection table at the beginning. 
      2. print the parameters of main as local variables, 
         instead of fields of md.main.
      3. don't generate the final / exit lucid table. 
 *)
 let from_handler (ds : decls) (hog_rec : handler_opgraph_rec) : IS.llProg = 
  (* put builtin function generators into the context *)
  ctx_add_codegens dpt_builtin_fcns;
  ctx_add_decls ds; (* we probably don't need decls in context any more. *)
  (* Events are not used in this compilation mode, so there's 
     nothing to translate. Skipping this step also makes the handler's 
     parameters get printed as local variables -- which is what we want. *)
  (* let event_decls = TranslateEvents.translate_all ds in *)

  (* translate the array declarations into LL decls *)
  let regarray_defs = CL.map regdec_from_decl ds |> CL.flatten in
  (* translate the one handler into LL program *)
  let tofino_prog = llprog_from_single_handler hog_rec in 
  let out_prog =
    { tofino_prog with
      instr_dict =
          tofino_prog.instr_dict 
        @ IS.dict_of_decls regarray_defs
    }
  in
  out_prog
;;

let from_dpt (ds : decls) (opgraph_recs : handler_opgraph_rec list) : IS.llProg =
  (* translation to IR currently does many passes over the backend, with each pass
    translating a different part of the syntax tree.
    5/18/21 -- now that the final structure of the generated code
    is more concretely defined, we can redo this to translate in a single pass.
    This would make the code clearer and easier to extend. *)
  (* put builtin function generators into the context *)
  ctx_add_codegens dpt_builtin_fcns;
  (* put source decls into the context *)
  ctx_add_decls ds;
  (* put event records in the context and generate
     all the LL code for event declarations. *)
  let event_decls = TranslateEvents.translate_all ds in
  (* generate struct declarations and instances for
     other private Lucid-runtime only data. *)
  (* generate parser for entry event instances. *)
  let parse_def = TranslateEvents.parsetree_from_events ds in
  (* generate backend defs for register arrays *)
  let regarray_defs = CL.map regdec_from_decl ds |> CL.flatten in
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
        (* @ IS.dict_of_decls group_defs *)
        @ IS.dict_of_decls regarray_defs
        @ IS.dict_of_decls event_decls
        @ IS.dict_of_decls lucid_internal_struct
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
