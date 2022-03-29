(* more straightforward DPT to Tofino translator for arrays. *)
open CoreSyntax
module Printing = CorePrinting
open LLSyntax
open InterpHelpers
open LLContext
open LLOp
open LLMemop

(* "interface" for this module -- the functions implemented here. *)
let array_name = "Array"
let array_id = Id.create array_name
let cid_of_array_fname name_str = Cid.create_ids [array_id; Id.create name_str]
let array_create_cid = cid_of_array_fname "create"
let array_get_cid = cid_of_array_fname "get"
let array_getm_cid = cid_of_array_fname "getm"
let array_set_cid = cid_of_array_fname "set"
let array_setm_cid = cid_of_array_fname "setm"
let array_update_cid = cid_of_array_fname "update"
let array_update_complex_cid = cid_of_array_fname "update_complex"

let create_array (args : codegenInput) : codegenOutput =
  let arrname, width, length =
    match args.args with
    | gname :: width :: length :: _ ->
      (* 5/18 -- the length arg can be a globally declared const *)
      name_from_exp gname, int_from_exp width, int_from_const_exp length
    | _ ->
      error
        ("ir create array: wrong number of args: "
        ^ string_of_int (CL.length args.args))
  in
  { names = []; objs = [new_regvec arrname width length] }
;;

let get_array (args : codegenInput) : codegenOutput =
  let hdl_id = Option.get args.hdl_id in
  (* x = array.get(A, idx); *)
  let regname_ex, idx_ex =
    match args.args with
    | [regname_ex; idx_ex] -> regname_ex, idx_ex
    | _ -> error "unexpected arg form in get array."
  in
  (* build the instruction vector. *)
  let sInstr = [new_readmem_instr None] in
  (* build the object that uses it. *)
  let reg_id = name_from_exp regname_ex in
  let reg_cell_width = ctx_width_of_garr reg_id in
  let oid = Cid.compound (Id.create "salu") (Option.get args.basename) in
  let idx_ex = oper_from_immediate hdl_id idx_ex in
  let salu_dec =
    new_dsalu oid reg_id reg_cell_width sInstr args.retname idx_ex
  in
  { names = [oid]; objs = [salu_dec] }
;;



let getm_array (args : codegenInput) : codegenOutput =
  let hdl_id = Option.get args.hdl_id in
  (* x = array.get(A, idx, memop, memop_arg2); *)
  let regname_ex, idx_ex, memop_name_ex, memop_arg_ex =
    match args.args with
    | [regname_ex; idx_ex; memop_name_ex; memop_arg_ex] ->
      regname_ex, idx_ex, memop_name_ex, memop_arg_ex
    | _ -> error "unexpected arg form in get array."
  in
  (* translate the memop into instructions *)
  let instrs = translate_simple_memop
    hdl_id
    Get
    memop_name_ex
    memop_arg_ex
  in 
  (* build the complete instruction. *)
  let reg_id = name_from_exp regname_ex in
  let reg_cell_width = ctx_width_of_garr reg_id in
  let oid = Cid.compound (Id.create "salu") (Option.get args.basename) in
  let idx_ex = oper_from_immediate hdl_id idx_ex in
  let salu_dec =
    new_dsalu oid reg_id reg_cell_width instrs args.retname idx_ex
  in
  { names = [oid]; objs = [salu_dec] }
;;

let set_array (args : codegenInput) : codegenOutput =
  let hdl_id = Option.get args.hdl_id in
  (* Array.set(arr, idx, v); *)
  let arr, idx, v =
    match args.args with
    | [arr; idx; v] -> arr, idx, v
    | _ -> error "unexpected arg form in get array."
  in
  (* build the instruction vector for arr[idx] = v;*)
  let write_oper = soper_from_immediate hdl_id None v in
  let sInstr = [new_writemem_instr None write_oper] in
  (* build the object that uses it. *)
  let reg_id = name_from_exp arr in
  let reg_cell_width = ctx_width_of_garr reg_id in
  let oid = Cid.compound (Id.create "salu") (Option.get args.basename) in
  let idx = oper_from_immediate hdl_id idx in
  let salu_dec = new_dsalu oid reg_id reg_cell_width sInstr None idx in
  { names = [oid]; objs = [salu_dec] }
;;

let setm_array (args : codegenInput) : codegenOutput =
  let hdl_id = Option.get args.hdl_id in
  (* array.setm(A, idx, memop, memop_arg2); *)
  let regname_ex, idx_ex, memop_name_ex, memop_arg_ex =
    match args.args with
    | [regname_ex; idx_ex; memop_name_ex; memop_arg_ex] ->
      regname_ex, idx_ex, memop_name_ex, memop_arg_ex
    | _ -> error "unexpected arg form in get array."
  in
  (* translate the memop into instructions *)
  let instrs = translate_simple_memop
    hdl_id
    Set
    memop_name_ex
    memop_arg_ex
  in 
  (* build the object that uses the memop. *)
  let reg_id = name_from_exp regname_ex in
  let reg_cell_width = ctx_width_of_garr reg_id in
  let oid = Cid.compound (Id.create "salu") (Option.get args.basename) in
  let idx_ex = oper_from_immediate hdl_id idx_ex in
  let salu_dec = new_dsalu oid reg_id reg_cell_width instrs None idx_ex in
  { names = [oid]; objs = [salu_dec] }
;;

let update_array (args : codegenInput) : codegenOutput =
  let hdl_id = Option.get args.hdl_id in
  let ( regname_ex
      , idx_ex
      , read_memop_name_ex
      , read_memop_arg_ex
      , write_memop_name_ex
      , write_memop_arg_ex )
    =
    match args.args with
    | [ regname_ex
      ; idx_ex
      ; read_memop_name_ex
      ; read_memop_arg_ex
      ; write_memop_name_ex
      ; write_memop_arg_ex ] ->
      ( regname_ex
      , idx_ex
      , read_memop_name_ex
      , read_memop_arg_ex
      , write_memop_name_ex
      , write_memop_arg_ex )
    | _ -> error "unexpected arg form in get array."
  in
  (* translate the read and write memops into a vector of 
     operations for the instruction to perform *)
  let read_instrs = translate_simple_memop
    hdl_id
    Get
    read_memop_name_ex
    read_memop_arg_ex
  in 
  let write_instrs = translate_simple_memop
    hdl_id
    Set
    write_memop_name_ex
    write_memop_arg_ex
  in 
  (* now put the read and write instructions together. *)
  let instrs = read_instrs @ write_instrs in 
  let reg_id = name_from_exp regname_ex in
  let reg_cell_width = ctx_width_of_garr reg_id in
  let oid = Cid.compound (Id.create "salu") (Option.get args.basename) in
  let idx_ex = oper_from_immediate hdl_id idx_ex in
  let salu_dec =
    new_dsalu oid reg_id reg_cell_width instrs args.retname idx_ex
  in     
  { names = [oid]; objs = [salu_dec] }
;;

let update_complex (args : codegenInput) : codegenOutput =
  let _ = args in 
  print_endline ("in update_complex");
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
      sIdx = idx;
      sWid = reg_wid;
      sNumCells = 1;
      sExprs = [];
      sInstrBody = Some(instr_body);
      sOut = args.retname;
    } 
  in 
let obj_id = Cid.compound (Id.create "salu") (Option.get args.basename) in
let salu_dec = SInstrVec(obj_id, sinstr) in 
{ names = [obj_id]; objs = [salu_dec] }
;;