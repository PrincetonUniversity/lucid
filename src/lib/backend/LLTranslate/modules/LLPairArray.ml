open CoreSyntax
module Printing = CorePrinting
open LLSyntax
open InterpHelpers
open LLContext
open LLOp
open LLMemop
open BatFormat

(* interface for this module -- the functions implemented here. *)
let module_name = "PairArray"
let module_id = Id.create module_name
let cid_of_array_fname name_str = Cid.create_ids [module_id; Id.create name_str]
let create_cid = cid_of_array_fname "create"
let update_cid = cid_of_array_fname "update"


(* the only difference between this and LLArray.create is that this makes an array with 2 cells. *)
let create (args : codegenInput) : codegenOutput =
  let arrname, width, length =
    match args.args with
    | gname :: width :: length :: _ ->
      name_from_exp gname, int_from_exp width, int_from_const_exp length
    | _ ->
      error
        ("ir create array: wrong number of args: "
        ^ string_of_int (CL.length args.args))
  in
  let fmt_cid = ctx_fmt_of_reg arrname in 
  let fmt_struct = new_meta_structdef fmt_cid [(P4tPrint.loFieldCid, width); (P4tPrint.hiFieldCid, width)] in 
  let rv = new_regvec arrname width length 2 fmt_cid in
  { names = []; objs = [rv; fmt_struct] }
;; 

let update (args : codegenInput) : codegenOutput =
  let hdl_id = Option.get args.hdl_id in
  let reg_id = CL.nth args.args 0 |> name_from_exp in 
  let reg_wid = ctx_width_of_garr reg_id in 
  let idx = oper_from_immediate hdl_id (CL.nth args.args 1) in 
  let memop_name_ex = CL.nth args.args 2 in 
  let arg1_ex = CL.nth args.args 3 in 
  let arg2_ex = CL.nth args.args 4 in 
  let default_ex = CL.nth args.args 5 in 
  let instr_body = translate_complex_memop hdl_id memop_name_ex arg1_ex arg2_ex default_ex in 
  let sinstr = {
      sRid =  reg_id; 
      sFmt = (ctx_fmt_of_reg reg_id);
      sIdx = idx;
      sWid = reg_wid;
      sNumCells = 2; (* this line is the only difference from LLArray.Update *)
      sExprs = [];
      sInstrBody = Some(instr_body);
      sOut = args.retname;
    } 
  in 
let obj_id = Cid.compound (Id.create "salu") (Option.get args.basename) in
let salu_dec = SInstrVec(obj_id, sinstr) in 
{ names = [obj_id]; objs = [salu_dec] }
;;

let builtins = [(create_cid, create); (update_cid, update)]