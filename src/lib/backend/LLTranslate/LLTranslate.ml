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
  ; LLConstants.generate_ports_cid, LLEvent.generate_ports
  ; LLEvent.event_delay_cid, LLEvent.delay_event
  ; LLSys.time_cid, LLSys.get_time
  ; LLSys.random_cid, LLSys.get_random
    (* (IrTranslate.hash_builtin, IrBuiltinToDag.do_hash) *) ]
  @LLPairArray.builtins
;;

(**** register array declarations ****)
let regdec_from_decl dec =
  let get_dglobal_info ty e =
    let sz =
      match ty.raw_ty with
      | TName (_, [sz], _) -> sz
      | _ -> error "Bad DGlobal"
    in
    let fcn_cid, args =
      match e.e with
      | ECall (fcn_cid, args) -> fcn_cid, args
      | _ -> error "Bad DGlobal"
    in
    sz, fcn_cid, args
  in
  match dec.d with
  | DGlobal (reg_id, ty, e) ->
    let sz, create_fcn_cid, args = get_dglobal_info ty e in
    let reg_id = Cid.id reg_id in
    (* reg, width, length, ??? *)
    let arg_name = exp (EVar reg_id) ty in
    let arg_width = exp (EVal (vint sz 8)) (TInt sz |> CoreSyntax.ty) in
    let args = arg_name :: arg_width :: args in
    let result =
      ctx_call_codegen
        create_fcn_cid
        { hdl_id = None
        ; (* global code, no handler context. *)
          basename = None
        ; retname = Some reg_id
        ; args
        }
    in
    result.objs
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
  let newinstance = IS.new_struct struct_cid () instance_cid in
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
    | IS.StructDef {sdId=mid; sdType=struct_type;sdFields=fields;} ->
      (match struct_type with
      | IS.SHeader ->
        let aligned_fields =
          CL.fold_left byte_align_fields [] fields |> pad_tail
        in
        cid, IS.StructDef {sdId=mid; sdType=struct_type;sdFields=aligned_fields;}
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
  !dprint_endline "-----object ids in regarray_defs-----";
  !dprint_endline (DebugPrint.ids_in_cid_decls (IS.dict_of_decls regarray_defs));
  !dprint_endline "-----object decls in regarray_defs-----";
  !dprint_endline (DebugPrint.str_of_cid_decls (IS.dict_of_decls regarray_defs));

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
  let event_decls = LLEvent.TranslateEvents.translate_events ds in
  (* generate struct declarations and instances for
     other private Lucid-runtime only data. *)
  (* generate parser for entry event instances. *)
  let parse_def = LLEvent.TranslateEvents.parsetree_from_events () in
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
        @ IS.dict_of_decls LLConstants.lucid_internal_struct
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
