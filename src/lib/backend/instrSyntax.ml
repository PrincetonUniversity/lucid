(* 
  Syntax for ALU-level operations on the Tofino.
  ALU instructions
  stateful ALU instructions 
  Actions (sets of instructions applied in parallel)
  Tables (maps from variable values to actions)

  There is control flow: each action points to a next table. 
*)

(* 
TODO: 
- simplify / clean up expressions and values
- add records/named tuples and record indexing
*)

open Base
open MiscUtils

exception Error of string
let error s = raise (Error s)

(* type ti = [%import: Id.t] *)
type id = [%import: Id.t]

and cid = [%import: Cid.t]

(* DPA types. *)
(* constant. *)
and const = [%import: (Integer.t[@with Z.t := (Z.t [@opaque])])]

(* local variable *)
and mid = cid

(* callable object *)
and oid = cid 

(* memory / register array *)
and rid = cid 

(* var that gets written *)
and lmid = mid

(* var that gets read *)
and rmid = mid

(* ALU operations in the dataplane. *)
and binOp =
  | Add
  | Sub (* A - B *)
  | SubR (* B - A *)
  | Cast

(* cast A to width of B *)
and cmpOp =
  | Eq
  | Neq
  | Lt
  | Gt

and boolOp =
  | And
  | Or

(* An operand to an ALU instruction. *)
and oper =
  | Const of const
  | Meta of mid

(* Expressions that "stateless" ALUs can evaluate. *)
and expr =
  | Oper of oper
  | BinOp of binOp * oper * oper
  | HashOp of mid list (* not implemented yet -- use Hasher*)

(* A stateless ALU instruction *)
and instr = 
  | IAssign of lmid * expr
  | IValidate of mid (* validate an output header struct instance *)
  | IInvalidate of mid (* invalidate an output header struct instance*)

(* A vector of stateless ALUs that operate in parallel. *)
and instrVec = instr list

and regSlice =
  | Lo

(* an operand to an SALU instruction *)
(* TODO: this really wants to be combined with oper. Call it a value or immediate. *)
and sOper =
  | RegVar of regSlice (* regslice is actually an address, but we don't need to know what address at this point. *)
  | MetaVar of mid
  | ConstVar of const 
  | NoOper


(* The left hand side of an salu instruction. *)
and sExpr = 
  | SVar of sOper
  | SBinOp of binOp * sOper * sOper

and sCompExpr = sOper * binOp * sOper * cmpOp * sOper 
and sPredExpr =
  | Comp of sCompExpr
  | Neg of sCompExpr

(* a single ALU instruction *)
and sInstr = 
  | MemInstr of sPredExpr option * sExpr
  | RetInstr of sPredExpr option * sExpr

(* a stateful ALU instruction vector *)
and sInstrVec = sInstr list 

(* a header structure, mainly for events. *)
and structType = 
  | MetaStruct 
  | HeaderStruct

and varType = 
  | Input
  | Temporary
  | Output
and scope = 
  | SPrivate | SPublic

and decl =
  (* consts are inlined by the typer, so we don't need to represent them here. *)
  | GlobalMeta of mid * int (* meta<int (width)> myVar; *)
  | RegVec of rid * int * int * const * int option
  (* | Routine of routine *)
  (* tables select actions, actions call instruction vectors, instruction vectors do compute over meta vars and reg vecs. *)
  | Table of table
  | Action of action
  | InstrVec of oid * instrVec (* InstrVec myIvec(mid list (args)) = {instrVec}*)
  | SInstrVec of sivec
  | Hasher of oid * int * int * lmid * oper list (* lmid = hash<int[width], int[poly]>(mid list) *)
  | StructDef of mid * structType * ((mid * int) list) (* a header or metadata structure *)
  | Struct of  mid * varType * mid (* nput Mystruct X; output Mystruct Y; an instance of a structure. *)
  | DConst of mid * scope * int * int (* a defined constant *)
  | ParseTree of oid * (parse_node list)
  | SchedBlock of oid * scheduler_block (* P4 code generators for lucid's scheduler. *)
  | ConfigBlock of oid * config_block (* control plane configurations at startup *)

(*** parse tree ***)
and parse_node = oid * (parse_instr list) * parse_next
and parse_instr = (* note: current instructions all assume that the var is already declared. *)
  | PPeek of mid * int (* declared field = lookahead<bit<int>>; *)
  | PStruct of mid (* parse a struct *)
  | PSet of mid * mid (* x = parsed_header.y; *)
and parse_next = 
  | PNext of oid option
  | PSelect of mid * (select_pat * oid option) list (* None -> accept *)
and select_pat = 
  | SConst of int 
  | SConstVar   of mid (* a constant variable *)
  | SDefault


(* a full stateful instruction *)
and sivec = oid * rid * int (* register width *) * sInstrVec * lmid option * oper (* mid: output var; oper: reg index. *)

(* an action calls a set of local objects, in parallel, 
then invokes all its successor tables (also in parallel). 
Successors (oid) are filled in by the compiler. *)
and action = oid * oid list * oid list

(* a table branches, calling the first action (oid) whose 
pattern matches vars (mid list). If none match, it calls a default action. *)
and table = oid * rule list * int option

(* a rule has an identifier, pattern, and an action *)
(* match constructor in case we want to add different types of rules in the future *)
and rule = 
  | Match of cid * pattern * oid
  | OffPath of pattern
(*  | OffPath (* matching an offpath rule means that a packet is not handled by this code path. These should not occur in final programs. *)
*)
and pattern = (mid * condition) list

and condition =
  | Exact of const
  | Any


(*** scheduler blocks  ***)
(* A block of P4 that appears at a fixed 
point in the P4 pipeline and implements part of 
lucid's event scheduling layer. A scheduler block has: 
  1. a printing function that takes a table id 
  and prints a P4 block including the table. 
  2. a location. *)
and scheduler_block = {
  sb_print : p4_printer;
  sb_loc : p4_loc;
}
and p4_printer = (oid -> string[@opaque])
and p4_loc = | LIgrStart | LIgrEnd | LEgr

(*** control plane configuration ***)
and config_block = 
  | McConfig of mc_group list (* a list of multicast groups *)
  
(* a multicast group has an id and packet copy instructions *)
and mc_group = 
{
  mc_id : int;
  mc_instrs: mc_copy_instr list;
}
(* copy a packet to a port and annotates it with an id.*)
and mc_copy_instr = 
{
  port : int;
  pkt_copy_id : int; 
}

(* an instruction program is a dictionary of table / action / alu declarations *)
and instrProg =  
{
  root_tid : oid;
  instr_dict : (oid * decl) list;
}

[@@deriving
  visitors
    { name = "dataPathIter"
    ; variety = "iter"
    ; polymorphic = true
    ; data = true
    ; concrete = true
    }
  , visitors
      { name = "dataPathMap"
      ; variety = "map"
      ; polymorphic = true
      ; data = true
      ; concrete = true
      }
  , show]

(* filtering functions *)
let is_action dec = 
  match dec with 
    | Action _ -> true
    | _ -> false
;;

(* constructors for stateful alu stuff *)
let to_meminstr (pred, expr) = MemInstr (pred, expr)
let to_retinstr (pred, expr) = RetInstr (pred, expr)
let sinstr_dec si_id reg_id reg_wid sivec out_opt idx_oper : decl = 
  SInstrVec(si_id, reg_id, reg_wid, sivec, out_opt, idx_oper)
;;
let lo_sExpr : sExpr = SVar(RegVar(Lo))

let binop_sexpr_of op rs var = SBinOp(op, rs, var)
;;

let to_action a b c = Action (a, b, c)
let from_action d = 
  match d with 
  | Action a -> a
  | _ -> error "from_action: not an action";
;;

let from_sivec d = 
  match d with
  | SInstrVec s -> s
  | _ -> error "from_sivec: not an sivec"
;;

let fresh_any_rule acn_id = Match(Cid.fresh ["r"], [], acn_id)
;;

let to_globalmeta id sz = GlobalMeta(id, sz)

(* alu call that sets a variable to a value *)
let alucall_of_assign ivec_id tgt_var tgt_val = InstrVec(ivec_id, [IAssign(tgt_var, Oper tgt_val)])
;;

(* tgt_var = tgt_val *)
let tbl_of_assign tbl_id tgt_var tgt_val  = 
  let ivec_id = Cid.concat (Cid.create ["alu"]) tbl_id in 
  let ivec = alucall_of_assign ivec_id tgt_var tgt_val in 
  let acn_id = Cid.concat (Cid.create ["acn"]) tbl_id in 
  let acn = Action(acn_id, [ivec_id], []) in 
  let tbl = Table(tbl_id, [fresh_any_rule acn_id], None) in 
  [ivec; acn; tbl]
;;

(* modifiers *)
(* update acn_decl, adding tid as a successor *)
let add_succ_tid tid acn_decl = 
  let (oid, oids, next_tids) = from_action acn_decl in 
  to_action oid oids (tid::next_tids)
;;

(* accessors *)
let oid_of_sivec d : oid = 
  let (salu_id, _, _, _, _, _) = from_sivec d in 
  salu_id
;;

let rid_of_sivec d : rid = 
  let (_, reg_id, _, _, _, _) = from_sivec d in 
  reg_id
;;

(* the index of a register that the sivec accesses *)
let idx_of_sivec d : oper = 
  let (_, _, _, _, _, idx_in) = from_sivec d in 
  idx_in
;;  
(* the variable that an sivec writes *)
let outarg_of_sivec d : mid option = 
  let (_, _, _, _, out_opt, _) = from_sivec d in 
  out_opt
;;

(* the variables that an sivec reads *)
let readargs_of_sivec d : mid list  = 
  let (_, _, _, sivec_inner, _, _) = from_sivec d in 
  let arg_finder = 
    object
      inherit [_] dataPathIter as super
      val mutable vars = []
      method vars = vars
      method !visit_mid _ m = 
        vars <- vars @ [m]
        (* Caml.Printf.printf "[readargs_of_sivec] visiting mid in %s\n" (mid_to_str_suffix sivec_id); *)
    end
  in 
  arg_finder#visit_sInstrVec () sivec_inner;
  let in_arg = arg_finder#vars in 
  in_arg
;;

let id_of_decl d = match d with 
  | GlobalMeta (i, _)           
  | RegVec (i, _, _, _, _)    
  | Table (i, _, _)    
  | Action (i, _, _)     
  | InstrVec(i, _)       
  | SInstrVec(i, _, _, _, _, _) 
  | Hasher(i, _, _, _, _)     
  | SchedBlock(i, _) 
  | StructDef(i, _, _)
  | Struct(i, _, _)
  | DConst(i, _, _, _)
  | ParseTree(i, _)
  | ConfigBlock(i, _)
      -> i
;;

(* associative object lists *)
let decl_to_tup d = (id_of_decl d, d)
;;

let dict_of_decls ds = 
  Caml.List.map decl_to_tup ds
;;

let aid_of_acn a = id_of_decl a
let tid_of_tbl t = id_of_decl t
;;

(**** table helpers ****)
let rename_tbl new_tid tbl =
  match tbl with
  | Table (_, rules, stg_opt) ->
    (* printf "renaming table %s --> %s\n" (mid_to_str_suffix tid) (mid_to_str_suffix new_tid); *)
    Table (new_tid, rules, stg_opt)
  | _ -> error "not a table"
;;


(**** rule helpers ****)
let rules_to_match_tups rules =
  Caml.List.filter_map
    (fun r ->
      match r with
      | Match (_, p, a) -> Some (p, a)
      | OffPath _ -> None)
    rules
;;

let rules_to_aids (rules : rule list) : oid list =
  let _, aids = Caml.List.split (rules_to_match_tups rules) in
  unique_list_of aids
;;

(* get the match variables from a rule *)
let match_vars_of_rule rule =
  match rule with
  | Match (_, pattern, _) ->
    let match_vars, _ = CL.split pattern in
    match_vars
  | _ -> error "[match_vars_of_rule] offpath?" 
;;

let match_vars_of_rules rules =
  rules
  |> CL.fold_left
       (fun cur_mvs rule ->
         match rule with
         | Match _ -> cur_mvs @ match_vars_of_rule rule
         | OffPath _ -> cur_mvs)
       []
  |> unique_list_of
;;
let pat_of_rule r =
  match r with
  | Match (_, pat, _) -> pat
  | OffPath pat -> pat
;;

let aid_of_rule r =
  match r with
  | Match (_, _, s_aid) -> s_aid
  | OffPath _ -> Cid.create ["OffPath"]
;;


let int_of_expr e = 
  match e with 
    | Oper (Const x) -> Integer.to_int x
    | _ -> error "[int_of_expr] not a stateful alu expression with an int"
;;

(**** new constructors used by IR (3/16/21) ****)

let new_iconst i = Const (i)
let new_eoper o = Oper(o)
;;

let new_const_of_int i : const = Integer.of_int i
let new_oconst_of_int i = new_iconst (new_const_of_int i)
let new_expr_of_int i = new_eoper (new_oconst_of_int i)
let new_expr_of_mid mid = new_eoper (Meta mid)
;;

(* Hasher of oid * int * int * lmid * oper list *)
let new_hasher name width poly outvar args = 
  Hasher (name, width, poly, outvar, args)
;;

let new_regvec name width length = RegVec(name, width, length, new_const_of_int 0, None)
;;

let new_iinvalidate mid = IInvalidate(mid)
let new_iinvalidate_vec mids = CL.map new_iinvalidate mids

(* declare an instruction vector for 1 instruction *)
let new_iassign lhs rhs = 
  IAssign(lhs, rhs)
;;
let new_iassign_int lhs rhs_int = 
  IAssign(lhs, new_expr_of_int rhs_int)
;;


let new_dsingleinstr oid lhs rhs = 
  InstrVec(oid, [IAssign(lhs, rhs)])
;;
let new_dinstr oid ivec = 
  InstrVec(oid, ivec)
;;

let new_ebinop o a b = BinOp(o, a, b)
;;

let memcell_operand = SVar(RegVar(Lo))

let new_meminstr pred_opt instr_rhs = 
  MemInstr(pred_opt, instr_rhs)
;;
let new_retinstr pred_opt instr_rhs = 
  RetInstr(pred_opt, instr_rhs)
;;
let new_readmem_instr cond_opt = new_retinstr cond_opt memcell_operand
;;

let new_writemem_instr cond_opt operand = new_meminstr cond_opt (SVar(operand))
;;

let new_dsalu obj_id reg_id reg_wid sivec outvar_opt reg_idx = 
  SInstrVec(obj_id, reg_id, reg_wid, sivec, outvar_opt, reg_idx)
;;

let new_globalmeta name width = GlobalMeta(name, width)
;;

let new_action acn_name compute_objs next_tbls = 
  Action(acn_name, compute_objs, next_tbls)
;;

let new_rule rule_name patterns action_name = 
  Match(rule_name, patterns, action_name)
;;

let new_table tblname rules = Table(tblname, rules, None)
;;


(* 
  | StructDef of mid * structType * ((mid * int) list) (* a header or metadata structure *)
  | Struct of mid * mid (* an instance of a structure. *)
  | Def of mid * int (* a defined constant *)
*)

let new_meta_structdef name params = StructDef(name, MetaStruct, params)
let new_header_structdef name params = StructDef(name, HeaderStruct, params)
let new_struct sname stype iname = Struct(iname, stype, sname)
let new_public_constdef name width i = DConst(name, SPublic, width, i)
let new_private_constdef name width i = DConst(name, SPrivate, width, i)
;;


let new_ParseTree name root = ParseTree(name, root)
let new_parse_node name stmts next = (name, stmts, next)
let new_PStruct sname = PStruct(sname)
let new_PPeek varname width = PPeek(varname, width)
let new_PSet rvar lvar = PSet(rvar, lvar)
let new_PNext pnodeid_opt = PNext(pnodeid_opt)
let new_PSelect select_var branches = PSelect(select_var, branches)
let new_SConst_branch i nid_opt = (SConst(i), nid_opt)
let new_SConstVar_branch v nid_opt = (SConstVar(v), nid_opt)
let new_SDefault_branch nid_opt = (SDefault, nid_opt)
;;

(* generate a native block that is not bound 
   to a particular point in the control flow. 
   this is for objects that are placed into 
   the program manually and not optimized. *)
let new_native_block oid loc print_fcn = 
  SchedBlock(oid, 
    {
      sb_print = print_fcn;
      sb_loc = loc;
    }
  )
;;

let new_config_block oid cb = 
    ConfigBlock(oid, cb)
;;

(* 
let filter_end_native_blocks decls = 
  let filter_f dec = 
    match dec with 
      | SchedBlock(_, def) -> (
        match def.sb_loc with 
          | LIgrEnd -> true
          | _ -> false
        )
      | _ -> false
  in 
  CL.filter filter_f decls
;; *)

(* new filter functions used to distinguish between data defs and compute defs *)

let is_fixedloc_decl dec = 
  match dec with 
    | RegVec _ 
    | GlobalMeta _
    | StructDef _ 
    | Struct _ 
    | DConst _
    | ParseTree _ 
    | ConfigBlock _
    | SchedBlock _ -> true
    | Table _ 
    | Action _ 
    | InstrVec _ 
    | SInstrVec _ 
    | Hasher _ -> false
;;


(* new destructors *)

let name_of_structdef dec =
  match dec with 
    | StructDef(mid, _, _) -> mid
    | _ -> error "Not a struct def" 
;;

let ty_of_structdef dec = 
  match dec with 
    | StructDef(_, s_ty, _) -> s_ty
    | _ -> error "not a struct def"
;;

let name_of_structinst dec = 
  match dec with 
    | Struct (structname, _, _) -> structname
    | _ -> error "not a struct instance decl"
;;

let width_of_regvec dec = 
  match dec with 
    | RegVec(_, wid, _, _, _) -> wid
    | _ -> error "not a regvec decl"
;;