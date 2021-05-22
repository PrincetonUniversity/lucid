(* more straightforward DPT to Tofino translator for arrays. *) 
open Syntax
open InstrSyntax
open InterpHelpers
open TofinoContext
open TofinoOp
open TofinoMemop

(* "interface" for this module -- the functions implemented here. *)
let array_name = "Array"
let array_id = Id.create array_name
let cid_of_array_fname name_str = Cid.create_ids [array_id; Id.create name_str]
;;
let array_create_cid = cid_of_array_fname "create"
let array_update_cid = cid_of_array_fname "update"
let array_get_cid = cid_of_array_fname "get"
let array_getm_cid = cid_of_array_fname "getm"
let array_set_cid = cid_of_array_fname "set"
let array_setm_cid = cid_of_array_fname "setm"
;;


let create_array (args : codegenInput) : codegenOutput = 
  let (arrname, width, length) = match args.args with 
    | gname::width::length::_ -> 
    print_endline ("got width expr: "^(Printing.exp_to_string width));
    (* 5/18 -- the length arg can be a globally declared const *)
    (name_from_exp gname, int_from_exp width, int_from_const_exp length)
    | _ -> error ("ir create array: wrong number of args: "^(string_of_int (CL.length args.args)))
  in 
  print_endline ("creating array of width: "^(string_of_int width));
  {names=[]; objs=[new_regvec arrname width length];}
;;

let get_array (args : codegenInput) : codegenOutput = 
  (* x = array.get(A, idx); *)
  let (regname_ex, idx_ex) = match args.args with 
    | [regname_ex; idx_ex] -> (regname_ex, idx_ex)
    | _ -> error "unexpected arg form in get array."
  in 
  (* build the instruction vector. *)
  let sivec = [new_readmem_instr None] in 
  (* build the object that uses it. *)
  let reg_id = name_from_exp regname_ex in 
  let reg_cell_width = ctx_width_of_garr reg_id in
  let oid = Cid.compound (Id.create "salu") (Option.get args.basename) in 
  let idx_ex = oper_from_immediate idx_ex in
  let salu_dec = new_dsalu oid reg_id reg_cell_width sivec args.retname idx_ex in 
  trans_info "generated get_array salu dec:";
  trans_info (show_decl salu_dec);
  {names = [oid]; objs = [salu_dec];}
;;

let getm_array (args : codegenInput) : codegenOutput = 
  (* x = array.get(A, idx, memop, memop_arg2); *)
  let (regname_ex, idx_ex, memop_name_ex, memop_arg_ex) = match args.args with 
    | [regname_ex; idx_ex; memop_name_ex; memop_arg_ex] -> (regname_ex, idx_ex, memop_name_ex, memop_arg_ex)
    | _ -> error "unexpected arg form in get array."
  in 
  (* translate the memop. *)
  let sivec = from_memop_name (name_from_exp memop_name_ex) Get memop_arg_ex in 
  (* build the object that uses the memop. *)
  let reg_id = name_from_exp regname_ex in 
  let reg_cell_width = ctx_width_of_garr reg_id in
  let oid = Cid.compound (Id.create "salu") (Option.get args.basename) in 
  let idx_ex = oper_from_immediate idx_ex in
  let salu_dec = new_dsalu oid reg_id reg_cell_width sivec args.retname idx_ex in 
  trans_info "generated getm_array salu dec:";
  trans_info (show_decl salu_dec);
  {names = [oid]; objs = [salu_dec];}
;;

let set_array (args : codegenInput) : codegenOutput = 
  (* Array.set(arr, idx, v); *)
  let (arr, idx, v) = match args.args with 
    | [arr; idx; v] -> (arr, idx, v)
    | _ -> error "unexpected arg form in get array."
  in 
  (* build the instruction vector for arr[idx] = v;*)
  let write_oper = soper_from_immediate None v in 
  let sivec = [new_writemem_instr None write_oper] in 
  (* build the object that uses it. *)
  let reg_id = name_from_exp arr in 
  let reg_cell_width = ctx_width_of_garr reg_id in
  let oid = Cid.compound (Id.create "salu") (Option.get args.basename) in 
  let idx = oper_from_immediate idx in
  let salu_dec = new_dsalu oid reg_id reg_cell_width sivec None idx in 
  trans_info "generated set_array salu dec:";
  trans_info (show_decl salu_dec);
  {names = [oid]; objs = [salu_dec];}
;;

let setm_array (args : codegenInput) : codegenOutput = 
  (* array.setm(A, idx, memop, memop_arg2); *)
  let (regname_ex, idx_ex, memop_name_ex, memop_arg_ex) = match args.args with 
    | [regname_ex; idx_ex; memop_name_ex; memop_arg_ex] -> (regname_ex, idx_ex, memop_name_ex, memop_arg_ex)
    | _ -> error "unexpected arg form in get array."
  in 
  (* translate the memop. *)
  let sivec = from_memop_name (name_from_exp memop_name_ex) Set memop_arg_ex in 
  (* build the object that uses the memop. *)
  let reg_id = name_from_exp regname_ex in 
  let reg_cell_width = ctx_width_of_garr reg_id in
  let oid = Cid.compound (Id.create "salu") (Option.get args.basename) in 
  let idx_ex = oper_from_immediate idx_ex in
  let salu_dec = new_dsalu oid reg_id reg_cell_width sivec None idx_ex in 
  trans_info "generated setm_array salu dec:";
  trans_info (show_decl salu_dec);
  {names = [oid]; objs = [salu_dec];}
;;

let update_array (args : codegenInput) : codegenOutput = 
  (* x = array.update(A, idx, readmemop, readmemop_arg2, writememop, writememop_arg2); *)
  let (
        regname_ex, idx_ex, 
        read_memop_name_ex, read_memop_arg_ex, 
        write_memop_name_ex, write_memop_arg_ex
      ) = match args.args with 
            | [ 
                regname_ex; idx_ex; 
                read_memop_name_ex; read_memop_arg_ex; 
                write_memop_name_ex; write_memop_arg_ex
              ] -> (
                    regname_ex, 
                    idx_ex, 
                    read_memop_name_ex, 
                    read_memop_arg_ex, 
                    write_memop_name_ex, 
                    write_memop_arg_ex
                    )
            | _ -> error "unexpected arg form in get array."
  in 
  (* translate the memops. *)
  let read_sivec  = from_memop_name (name_from_exp read_memop_name_ex) Get read_memop_arg_ex in 
  let write_sivec = from_memop_name (name_from_exp write_memop_name_ex) Set write_memop_arg_ex in 
  (* combine the memops, read then write *)
  let sivec = read_sivec@write_sivec in 

  (* build the object that uses the memop. *)
  let reg_id = name_from_exp regname_ex in 
  let reg_cell_width = ctx_width_of_garr reg_id in
  let oid = Cid.compound (Id.create "salu") (Option.get args.basename) in 
  let idx_ex = oper_from_immediate idx_ex in
  let salu_dec = new_dsalu oid reg_id reg_cell_width sivec args.retname idx_ex in 
  trans_info "generated getm_array salu dec:";
  trans_info (show_decl salu_dec);
  {names = [oid]; objs = [salu_dec];}
;;
