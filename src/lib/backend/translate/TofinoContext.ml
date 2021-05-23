open Syntax
open SyntaxUtils
open Batteries
module CL = Caml.List
open InterpHelpers


(* logging *)
module DBG = BackendLogging
let outc = ref None 
let dprint_endline = ref DBG.no_printf
;;


(*** context for translation from source to Tofino instruction syntax. ***)
(*** nothing in this context should be necessary to convert the 
Tofino instruction syntax into P4. ***)


(**** input and output of code generator functions ****)
type codegenInput = {
  basename       : Cid.t option;
  retname        : Cid.t option; (* the variable to write return value to, if any *)
  args           : Syntax.exp list; (* a tuple of arguments *)
}
type codegenOutput = {
  names   : Cid.t list;
  objs    : InstrSyntax.decl list;  
}
type codegenFcn = codegenInput -> codegenOutput

(* information about an event. *)
type event_rec = 
{ 
    event_id      : id;
    event_iid     : int;
    field_defs    : (Cid.t * int) list;
    event_sort    : event_sort;
    in_struct     : Cid.t option;
    out_struct    : Cid.t option;
}

(*** context ***)
module CtxTbl = struct 
  type t = Cid.t
  let compare = Cid.compare
end
module TofinoCtx = BatMap.Make(CtxTbl)
type ctx_entry = 
  | CodeGen of codegenFcn
  | Decl of decl (* for memops *)
  | EventRec of event_rec 

let tofinoCtx : (ctx_entry TofinoCtx.t)ref = ref TofinoCtx.empty


(**** context decl operations ****)
let ctx_add_decl n d = 
  tofinoCtx := TofinoCtx.add n (Decl(d)) !tofinoCtx
let ctx_find_decl n = 
  match (TofinoCtx.find_opt n !tofinoCtx) with 
    | Some (Decl(d)) -> d
    | _ -> error ("did not find a decl in context ("^(Cid.to_string n)^")")

let ctx_find_decl_opt n = 
  match (TofinoCtx.find_opt n !tofinoCtx) with 
    | Some (Decl(d)) -> Some d
    | _ -> None
;;
(* add all the toplevel declarations to the context. *)
let ctx_add_decls (ds:Syntax.decls) = 
  let iter_f dec = match dec.d with 
    | DGlobal(id, _, _, _)
    | DMemop(id, _) -> ctx_add_decl (Cid.id id) dec
    | DConst(id, _, _) -> ctx_add_decl (Cid.id id) dec
    | _ -> ()
  in 
  CL.iter iter_f ds
;;
let ctx_bdy_of_memop n = match (ctx_find_decl n) with 
  | {d=DMemop(_, (params, stmt)); _} -> (cids_from_params params, stmt)
  | _ -> error "could not find memop in decl context"
;;
let ctx_width_of_garr n = match (ctx_find_decl n) with 
  | {d=DGlobal(_, (_, [sz]), _ , _); _} -> extract_size sz
  | _ -> error "could not find memop in decl context"
;;

let ctx_int_of_const var_id = match (ctx_find_decl var_id) with 
  | {d=DConst(_, _, val_exp); _} -> int_from_exp val_exp
  | _ -> error "could not find const in decl context"
;;

let ctx_var_is_const var_id = match (ctx_find_decl_opt var_id) with 
  | Some {d=DConst(_, _, _); _} -> true
  | _ -> false
;;

(**** context code generator functions ****)
let ctx_add_codegen n c = 
  print_endline ("adding code generator: "^(Cid.to_string n));
  tofinoCtx := TofinoCtx.add n (CodeGen(c)) !tofinoCtx
;;
let ctx_add_codegens ns_cs = 
  let iter_f (n, c) = ctx_add_codegen n c in 
  CL.iter iter_f ns_cs
;;
let ctx_find_codegen n = 
  print_endline ("calling code generator: "^(Cid.to_string n));
  match (TofinoCtx.find n !tofinoCtx) with 
    | CodeGen(g) -> g
    | _ -> error "did not find a decl in context"
;;
let ctx_call_codegen name args = 
  let fcn = ctx_find_codegen name in 
  fcn args
;;


(* context event definition functions *)
let ctx_add_eventrec evname eviid ev_sort field_defs in_struct out_struct = 
  print_endline ("[ctx_add_eventrec] adding event record for "^(Cid.to_string evname));
  let erec = 
  EventRec ({
    event_id = Cid.to_id evname;
    field_defs = field_defs;
    event_iid = eviid;
    event_sort = ev_sort;
    in_struct = in_struct;
    out_struct = out_struct;
  })
  in 
  tofinoCtx := TofinoCtx.add evname erec !tofinoCtx
;;

let ctx_find_eventrec n = 
  match (TofinoCtx.find n !tofinoCtx) with 
    | EventRec(r) -> r
    | _ -> error "did not find event rec in context"
;;
let ctx_find_event_fields id = 
  let cid = Cid.id id in 
  print_endline ("[ctx_find_event_fields] looking for fields of "^(Cid.to_string cid));
  (ctx_find_eventrec (Cid.id id)).field_defs |> CL.split |> fst
let ctx_find_event_instruct id =
  let cid = Cid.id id in 
  (ctx_find_eventrec cid).in_struct
let ctx_find_event_outstruct id =
  let cid = Cid.id id in 
  (ctx_find_eventrec cid).out_struct
let ctx_find_event_outstruct_cid cid =
  (ctx_find_eventrec cid).out_struct
;;

let ctx_find_event_iid cid =
  (ctx_find_eventrec cid).event_iid
;;


let ctx_get_event_recs () = 
  let fm_f _ v = 
    match v with 
      | EventRec (r) -> Some r
      | _ -> None 
  in 
  let filtered_ctx = 
    TofinoCtx.filter_map fm_f !tofinoCtx 
  in
  TofinoCtx.values filtered_ctx |> BatList.of_enum
;;


(*** context table for unique names ***)
module UniqueNameTbl = struct 
  type t = statement 
  let compare = Pervasives.compare
end 
module UniqueNameCtx = BatMap.Make(UniqueNameTbl)
let uniqueNameCtx : (Cid.t UniqueNameCtx.t) ref = ref UniqueNameCtx.empty
(* give each statement a unique ID. Save the statement ID for future use. *)
let uname_of_stmt (st:statement) = 
  match (UniqueNameCtx.find_opt st (!uniqueNameCtx)) with 
    | None ->
      let uname = Cid.fresh ["opstmt"] in 
      !dprint_endline ("--------------------------------");
      !dprint_endline ("[uname_of_stmt] opstatement id: "^(Cid.to_string uname));
      !dprint_endline ("[uname_of_stmt] statement:\n"^(Printing.stmt_to_string st));
      uniqueNameCtx := UniqueNameCtx.add st uname (!uniqueNameCtx);
      uname
    | Some uname -> uname 
;;
let prefix_of_stmt st pref = Cid.compound (Id.create pref) (uname_of_stmt st)
;;
let acnname_of_stmt (st:statement) = prefix_of_stmt st "acn"
let tblname_of_stmt (st:statement) = prefix_of_stmt st "tbl"
let aluname_of_stmt (st:statement) = prefix_of_stmt st "alu"
let saluname_of_stmt (st:statement) = prefix_of_stmt st "salu"
;;

let start_logging () =   DBG.start_mlog __FILE__ outc dprint_endline
;;
