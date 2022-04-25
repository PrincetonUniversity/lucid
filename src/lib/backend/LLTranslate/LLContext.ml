open CoreSyntax
module Printing = CorePrinting
open Batteries
module CL = Caml.List
open InterpHelpers

(* logging *)
module DBG = BackendLogging

let outc = ref None
let dprint_endline = ref DBG.no_printf
let start_logging () = DBG.start_mlog __FILE__ outc dprint_endline

(*** context for translation from source to LLSyntax instructions. ***)
(*** nothing in this context should be necessary to optimize the LLSyntax
     or translate it to P4. ***)

(**** input and output of code generator functions ****)
type codegenInput =
  { hdl_id : Id.t option
  ; (* the handler for which code generation happens. *)
    basename : Cid.t option
  ; retname : Cid.t option
  ; (* the variable to write return value to, if any *)
    args : exp list (* a tuple of arguments *)
  }

type codegenOutput =
  { names : Cid.t list
  ; objs : LLSyntax.decl list
  }

type codegenFcn = codegenInput -> codegenOutput

(* Events *)
type event_rec =
  { event_id : id
  ; event_iid : int
  ; field_defs : (Cid.t * int) list
  ; hidden_fields : (Cid.t * int) list
  ; hdl_param_ids : Id.t list
  ; event_sort : id option (* TODO:HEADERS Fix this *)
  ; event_struct : Cid.t
  ; event_struct_instance : Cid.t
  ; event_generated_flag : Cid.t
  }

(*** context ***)
module CtxTbl = struct
  type t = Cid.t

  let compare = Cid.compare
end

module TofinoCtx = BatMap.Make (CtxTbl)

type ctx_entry =
  | CodeGen of codegenFcn
  | Decl of decl (* for memops *)
  | EventRec of event_rec

let tofinoCtx : ctx_entry TofinoCtx.t ref = ref TofinoCtx.empty

(**** context decl operations ****)
let ctx_add_decl n d = tofinoCtx := TofinoCtx.add n (Decl d) !tofinoCtx

let ctx_find_decl n =
  match TofinoCtx.find_opt n !tofinoCtx with
  | Some (Decl d) -> d
  | _ -> error ("did not find a decl in context (" ^ Cid.to_string n ^ ")")
;;

let ctx_find_decl_opt n =
  match TofinoCtx.find_opt n !tofinoCtx with
  | Some (Decl d) -> Some d
  | _ -> None
;;

(* add all the toplevel declarations to the context. *)
let ctx_add_decls (ds : decls) =
  let iter_f dec =
    match dec.d with
    | DGlobal (id, _, _) -> ctx_add_decl (Cid.id id) dec
    | DMemop (id, _, _) -> ctx_add_decl (Cid.id id) dec
    | _ -> ()
  in
  CL.iter iter_f ds
;;

let ctx_bdy_of_memop n =
  match ctx_find_decl n with
  | { d = DMemop (_, params, memop_body); _ } -> params, memop_body
  | _ -> error "could not find memop in decl context"
;;

(* match ctx_find_decl n with
  | { d = DMemop (_, (params, stmt)); _ } -> cids_from_params params, stmt
  | _ -> error "could not find memop in decl context" *)

let ctx_width_of_garr n =
  match ctx_find_decl n with
  | { d = DGlobal (_, ty, _); _ } ->
    begin
      match ty.raw_ty with
      | TName (_, [sz], _) -> sz
      | _ -> error "Bad DGlobal type"
    end
  | _ -> error "could not find memop in decl context"
;;

(**** context code generator functions ****)
let ctx_add_codegen n c = tofinoCtx := TofinoCtx.add n (CodeGen c) !tofinoCtx

let ctx_add_codegens ns_cs =
  let iter_f (n, c) = ctx_add_codegen n c in
  CL.iter iter_f ns_cs
;;

let ctx_find_codegen n =
  match TofinoCtx.find n !tofinoCtx with
  | CodeGen g -> g
  | _ -> error "did not find a decl in context"
;;

let ctx_call_codegen name args =
  let fcn = ctx_find_codegen name in
  fcn args
;;

(* context event definition functions *)
let ctx_add_erec erec =
  let key = Cid.id erec.event_id in
  let entry = EventRec erec in
  tofinoCtx := TofinoCtx.add key entry !tofinoCtx
;;

let ctx_find_eventrec (cid : Cid.t) =
  match TofinoCtx.find cid !tofinoCtx with
  | EventRec r -> r
  | _ -> error "did not find event rec in context"
;;

let ctx_find_eventrec_opt (cid : Cid.t) =
  match TofinoCtx.find_opt cid !tofinoCtx with
  | Some (EventRec r) -> Some r
  | Some _ -> None
  | None -> None
;;

let ctx_find_event_fields id =
  (ctx_find_eventrec (Cid.id id)).field_defs |> CL.split |> fst
;;

(* get the struct name for the event, by id *)
let ctx_find_event_struct_instance id =
  let cid = Cid.id id in
  (ctx_find_eventrec cid).event_struct_instance
;;

let ctx_find_event_struct_instance_cid cid =
  (ctx_find_eventrec cid).event_struct_instance
;;

let ctx_find_event_struct_cid cid = (ctx_find_eventrec cid).event_struct
let ctx_find_event_iid cid = (ctx_find_eventrec cid).event_iid

(* remember the parameters of the event's handler. *)
let ctx_set_hdl_param_ids id hdl_param_ids =
  let erec = ctx_find_eventrec id in
  let new_erec = { erec with hdl_param_ids } in
  tofinoCtx := TofinoCtx.add id (EventRec new_erec) !tofinoCtx
;;

(* a map from handler param id to event param cid *)
let ctx_get_hdl_param_map hdl_id =
  let erec = ctx_find_eventrec (Cid.id hdl_id) in
  let recs =
    CL.map
      (fun field_cid -> Cid.concat erec.event_struct_instance field_cid)
      (CL.split erec.field_defs |> fst)
    |> CL.combine erec.hdl_param_ids
  in
  recs
;;

let ctx_find_event_iid cid = (ctx_find_eventrec cid).event_iid

let ctx_get_event_recs () =
  let fm_f _ v =
    match v with
    | EventRec r -> Some r
    | _ -> None
  in
  let filtered_ctx = TofinoCtx.filter_map fm_f !tofinoCtx in
  TofinoCtx.values filtered_ctx |> BatList.of_enum
;;

(*** context table for unique names ***)
module UniqueNameTbl = struct
  type t = statement

  let compare = Pervasives.compare
end

module UniqueNameCtx = BatMap.Make (UniqueNameTbl)

let uniqueNameCtx : Cid.t UniqueNameCtx.t ref = ref UniqueNameCtx.empty

(* give each statement a unique ID. Save the statement ID for future use. *)
let uname_of_stmt (st : statement) =
  match UniqueNameCtx.find_opt st !uniqueNameCtx with
  | None ->
    let uname = Cid.fresh ["opstmt"] in
    !dprint_endline "--------------------------------";
    !dprint_endline ("[uname_of_stmt] opstatement id: " ^ Cid.to_string uname);
    !dprint_endline ("[uname_of_stmt] statement:\n" ^ Printing.stmt_to_string st);
    uniqueNameCtx := UniqueNameCtx.add st uname !uniqueNameCtx;
    uname
  | Some uname -> uname
;;

let prefix_of_stmt st pref = Cid.compound (Id.create pref) (uname_of_stmt st)
let acnname_of_stmt (st : statement) = prefix_of_stmt st "acn"
let tblname_of_stmt (st : statement) = prefix_of_stmt st "tbl"
let aluname_of_stmt (st : statement) = prefix_of_stmt st "alu"
let saluname_of_stmt (st : statement) = prefix_of_stmt st "salu"
