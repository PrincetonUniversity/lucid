(* 
  Syntax for ALU-level operations on the Tofino.
  ALU instructions
  stateful ALU instructions 
  Actions (sets of instructions applied in parallel)
  Tables (maps from variable values to actions)

  The objects above are organized into an acyclic control flow graph. 
    - a table has rules that point to actions.
    - an action points to ALU and stateful ALU instructions, which are executed. 
    - an action also points to a successor table. 
    - there are no loops in the graph (acyclic)

  Conventions:   
    - each table represents a single operation. 
      - a branch, the selection between multiple empty actions 
      that point to different successor tables. 
      - a single alu or stateful alu instruction. 
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

(* ALU operations *)
and binOp =
  | Add
  | Sub (* A - B *)
  | SubR (* B - A *)
  | SatSub (* A |-| B (i.e., max(A - B, 0)) *)
  | RShift
  | LShift
  | BAnd
  | BOr
  | BXor
  | Cast (* cast A to width of B *)
  | Slice
(* A[X:Y] -- the slice of A from X to Y. *)

(* non binary operations. *)
(* and op = 
  | Slice
 *)
and cmpOp =
  | Eq
  | Neq
  | Lt
  | Gt

and boolOp =
  | And
  | Or

and regSlice = Lo

(* An operand to an ALU or sALU instruction. *)
and oper =
  | Const of const
  | Meta of mid
  | RegVar of regSlice
  | NoOper

(* Right hand side of a stateless instruction (i.e., 
   the output is not set) *)
and expr =
  | Oper of oper
  | BinOp of binOp * oper list
  (* | BinOp of binOp * oper * oper *)
  (* | GenericOp of op * (oper list) *)
  | HashOp of mid list

(* stateless instructions *)
and instr =
  (*   | IMove of lmid * oper
  | IBinOp of lmid * binOp * oper * oper
  | IHashOp of lmid * mid list  *)
  (* not implemented yet -- use Hasher *)
  | IAssign of lmid * expr
  | IValidate of mid (* validate an output header struct instance *)
  | IInvalidate of mid

(* invalidate an output header struct instance*)

(* A vector of parallel stateless instructions. *)
and instrVec = instr list

(* stateful instructions *)
and sEvalExpr =
  | SVar of oper
  | SBinOp of binOp * oper * oper

and sCompExpr = oper * binOp * oper * cmpOp * oper

and sPredExpr =
  | Comp of sCompExpr
  | Neg of sCompExpr

(* Right hand side of an salu instruction. *)
and sExpr =
  | MemExpr of sPredExpr option * sEvalExpr
  | RetExpr of sPredExpr option * sEvalExpr

(* a stateful instruction evaluates multiple expressions 
   that update a return variable and multiple words of a 
   single array cell. *)
and sInstr =
  { sRid : rid
  ; sWid : int
  ; sExprs : sExpr list
  ; sOut : lmid option
  ; sIdx : oper
  }

and sInstrOld = rid * int (* register width *) * sExpr list * lmid option * oper

(* a header structure, mainly for events. *)
and structType =
  | SMeta
  | SHeader

and scope =
  | SPrivate
  | SPublic

and decl =
  (* Local state *)
  | DConst of mid * scope * int * int (* a defined constant *)
  | MetaVar of mid * int (* meta<int (width)> myVar; *)
  | StructDef of mid * structType * (mid * int) list (* a header or metadata structure *)
  | StructVar of mid * scope * mid (* nput Mystruct X; output Mystruct Y; an instance of a structure. *)
  (* Persistent state *)
  | RegVec of rid * int * int * const * int option
  (* Control flow *)
  | Table of table
  | Action of action
  (* Computation *)
  | InstrVec of oid * instrVec (* InstrVec myIvec(mid list (args)) = {instrVec}*)
  | SInstrVec of oid * sInstr
  | Hasher of oid * int * int * lmid * oper list (* lmid = hash<int[width], int[poly]>(mid list) *)
  (* I/O *)
  | ParseTree of oid * parse_node list
  | SchedBlock of oid * scheduler_block (* P4 code generators for lucid's scheduler. *)
  | ConfigBlock of oid * config_block

(*** the parse graph is just a list of nodes that point to each other ***)
and parse_node = oid * parse_instr list * parse_next

and parse_instr =
  (* note: current instructions all assume that the var is already declared. *)
  | PPeek of mid * int (* declared field = lookahead<bit<int>>; *)
  | PStruct of mid (* parse a struct *)
  | PSet of mid * mid

(* x = parsed_header.y; *)
and parse_next =
  | PNext of oid option
  | PSelect of mid * (select_pat * oid option) list

(* None -> accept *)
and select_pat =
  | SConst of int
  | SDConst of mid (* a constant variable *)
  | SDefault

(* mid: output var; oper: reg index. *)

(* an action calls a set of local objects, in parallel, 
then invokes all its successor tables (also in parallel). *)
and action = oid * oid list * oid list

(* a table branches, calling the first action (oid) whose 
pattern matches vars (mid list). If none match, it calls a default action. *)
and table = oid * rule list * int option

(* a rule has an identifier, pattern, and an action *)
(* match constructor in case we want to add different types of rules in the future *)
(* 8/1/21 -- Rules should have identifiers like everything else. 
   Right now, Match rules have identifiers but OffPath rules don't. 
   That's annoyingly irregular. (also, are OffPaths even used anywhere anymore?)*)
and rule =
  | Match of cid * pattern * oid
  | OffPath of pattern (* an "OffPath rule" just means noop. Used for default rules. *)

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
and scheduler_block =
  { sb_print : p4_printer
  ; sb_loc : p4_loc
  }

and p4_printer = (oid -> string[@opaque])

and p4_loc =
  | LIgrStart
  | LIgrEnd
  | LEgr

(*** control plane configuration ***)
and config_block = McConfig of mc_group list

(* a list of multicast groups *)

(* a multicast group has an id and packet copy instructions *)
and mc_group =
  { mc_id : int
  ; mc_instrs : mc_copy_instr list
  }

(* copy a packet to a port and annotates it with an id.*)
and mc_copy_instr =
  { port : int
  ; pkt_copy_id : int
  }

and cid_decl = oid * decl

(* an instruction program is a dictionary of table / action / alu declarations *)
and llProg =
  { root_tid : oid
  ; instr_dict : cid_decl list
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
let is_table dec =
  match dec with
  | Table _ -> true
  | _ -> false
;;

let is_action dec =
  match dec with
  | Action _ -> true
  | _ -> false
;;

let is_hash dec =
  match dec with
  | Hasher _ -> true
  | _ -> false
;;

(* constructors for stateful alu stuff *)
let to_meminstr (pred, expr) = MemExpr (pred, expr)
let to_retinstr (pred, expr) = RetExpr (pred, expr)
let lo_sExpr : sEvalExpr = SVar (RegVar Lo)
let binop_sexpr_of op rs var = SBinOp (op, rs, var)
let to_action a b c = Action (a, b, c)

let from_action d =
  match d with
  | Action a -> a
  | _ -> error "from_action: not an action"
;;

let from_sInstr d =
  match d with
  | SInstrVec (_, s) -> s
  | _ -> error "from_sInstr: not an sInstr"
;;

let fresh_any_rule acn_id = Match (Cid.fresh ["r"], [], acn_id)
let to_globalmeta id sz = MetaVar (id, sz)

(* alu call that sets a variable to a value *)
let alucall_of_assign ivec_id tgt_var tgt_val =
  InstrVec (ivec_id, [IAssign (tgt_var, Oper tgt_val)])
;;

(* tgt_var = tgt_val *)
let tbl_of_assign tbl_id tgt_var tgt_val =
  let ivec_id = Cid.concat (Cid.create ["alu"]) tbl_id in
  let ivec = alucall_of_assign ivec_id tgt_var tgt_val in
  let acn_id = Cid.concat (Cid.create ["acn"]) tbl_id in
  let acn = Action (acn_id, [ivec_id], []) in
  let tbl = Table (tbl_id, [fresh_any_rule acn_id], None) in
  [ivec; acn; tbl]
;;

(* modifiers *)
(* update acn_decl, adding tid as a successor *)
let add_succ_tid tid acn_decl =
  let oid, oids, next_tids = from_action acn_decl in
  to_action oid oids (tid :: next_tids)
;;

(* accessors *)
let oid_of_sInstr d : oid =
  match d with
  | SInstrVec (oid, _) -> oid
  | _ -> error "not an sInstr"
;;

let rid_of_sInstr d : rid = (from_sInstr d).sRid

(* the index of a register that the sInstr accesses *)
let idx_of_sInstr d : oper = (from_sInstr d).sIdx

(* the variable that an sInstr writes *)
let outarg_of_sInstr d : mid option = (from_sInstr d).sOut

(* get a unique list of variables that a sInstruction reads in 
   its expressions (i.e., don't include the index) *)
let readvars_of_sInstr d : mid list =
  let arg_finder =
    object
      inherit [_] dataPathIter as super
      val mutable vars = []
      method vars = vars

      (* only search in the expression *)
      method! visit_sInstr ctx s = CL.iter (super#visit_sExpr ctx) s.sExprs
      method! visit_mid _ m = vars <- vars @ [m]
      (* Caml.Printf.printf "[readargs_of_sInstr] visiting mid in %s\n" (mid_to_str_suffix sInstr_id); *)
    end
  in
  let sInstr = from_sInstr d in
  arg_finder#visit_sInstr () sInstr;
  arg_finder#vars |> unique_list_of
;;

(* the non-memory-cell operands of an sInstr *)
let opers_of_sInstr d : oper list =
  let oper_finder =
    object
      inherit [_] dataPathIter as super
      val mutable opers = []
      method opers = opers
      method! visit_Meta _ m = opers <- opers @ [Meta m]
      method! visit_Const _ c = opers <- opers @ [Const c]
    end
  in
  let sInstr = from_sInstr d in
  oper_finder#visit_sInstr () sInstr;
  (* arg_finder#visit_sExprVec () sInstr_inner; *)
  let in_arg = oper_finder#opers in
  in_arg
;;

let id_of_decl d =
  match d with
  | MetaVar (i, _)
  | RegVec (i, _, _, _, _)
  | Table (i, _, _)
  | Action (i, _, _)
  | InstrVec (i, _)
  | SInstrVec (i, _)
  | Hasher (i, _, _, _, _)
  | SchedBlock (i, _)
  | StructDef (i, _, _)
  | StructVar (i, _, _)
  | DConst (i, _, _, _)
  | ParseTree (i, _)
  | ConfigBlock (i, _) -> i
;;

(* associative object lists *)
let decl_to_tup d = id_of_decl d, d
let dict_of_decls ds = Caml.List.map decl_to_tup ds
let aid_of_acn a = id_of_decl a
let tid_of_tbl t = id_of_decl t

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

let new_aid_of_rule r = 
  match r with 
  | Match (_, _, s_aid) -> Some s_aid
  | OffPath _ -> None
;;
let aid_of_rule r =
  match r with
  | Match (_, _, s_aid) -> s_aid
  | OffPath _ -> Cid.create ["OffPath"]
;;

(* This should only be used 
   for printing! Very dangerous 
   to use in actual code, because 
   there is no offpath object 
   in cid_decls. *)
let debug_aid_of_rule r =
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

let new_const_of_int i : const = Integer.of_int i
let new_iconst i = Const i

(* operands *)
let new_oper_of_int i = Const (new_const_of_int i)
let new_oper_of_meta m = Meta m
(* let new_oconst_of_int i = new_iconst (new_const_of_int i) *)

(* expressions *)
let new_eoper o = Oper o
let new_expr_of_int i = new_eoper (new_oper_of_int i)
let new_expr_of_mid mid = new_eoper (Meta mid)

(* Hasher of oid * int * int * lmid * oper list *)
let new_hasher name width poly outvar args =
  Hasher (name, width, poly, outvar, args)
;;

let new_regvec name width length =
  RegVec (name, width, length, new_const_of_int 0, None)
;;

let new_iinvalidate mid = IInvalidate mid
let new_iinvalidate_vec mids = CL.map new_iinvalidate mids

(* declare an instruction vector for 1 instruction *)
let new_iassign lhs rhs = IAssign (lhs, rhs)
let new_iassign_int lhs rhs_int = IAssign (lhs, new_expr_of_int rhs_int)
let new_dsingleinstr oid lhs rhs = InstrVec (oid, [IAssign (lhs, rhs)])
let new_dinstr oid ivec = InstrVec (oid, ivec)
let new_ebinop o a b = BinOp (o, [a; b])
let new_eop o args = BinOp (o, args)
let memcell_operand = SVar (RegVar Lo)
let new_meminstr pred_opt instr_rhs = MemExpr (pred_opt, instr_rhs)
let new_retinstr pred_opt instr_rhs = RetExpr (pred_opt, instr_rhs)
let new_readmem_instr cond_opt = new_retinstr cond_opt memcell_operand
let new_writemem_instr cond_opt operand = new_meminstr cond_opt (SVar operand)

let new_dsalu obj_id reg_id reg_wid sInstr outvar_opt reg_idx =
  SInstrVec
    ( obj_id
    , { sRid = reg_id
      ; sWid = reg_wid
      ; sExprs = sInstr
      ; sOut = outvar_opt
      ; sIdx = reg_idx
      } )
;;

let new_globalmeta name width = MetaVar (name, width)

let new_action acn_name compute_objs next_tbls =
  Action (acn_name, compute_objs, next_tbls)
;;

let new_rule rule_name patterns action_name =
  Match (rule_name, patterns, action_name)
;;

let new_table tblname rules = Table (tblname, rules, None)

(* 
  | StructDef of mid * structType * ((mid * int) list) (* a header or metadata structure *)
  | Struct of mid * mid (* an instance of a structure. *)
  | Def of mid * int (* a defined constant *)
*)

let new_meta_structdef name params = StructDef (name, SMeta, params)
let new_header_structdef name params = StructDef (name, SHeader, params)
let new_structdef name struct_ty params = StructDef (name, struct_ty, params)
let new_struct sname scope iname = StructVar (iname, scope, sname)
let new_public_constdef name width i = DConst (name, SPublic, width, i)
let new_private_constdef name width i = DConst (name, SPrivate, width, i)
let new_ParseTree name root = ParseTree (name, root)
let new_parse_node name stmts next = name, stmts, next
let new_PStruct sname = PStruct sname
let new_PPeek varname width = PPeek (varname, width)
let new_PSet rvar lvar = PSet (rvar, lvar)
let new_PNext pnodeid_opt = PNext pnodeid_opt
let new_PSelect select_var branches = PSelect (select_var, branches)
let new_SConst_branch i nid_opt = SConst i, nid_opt
let new_SDConst_branch v nid_opt = SDConst v, nid_opt
let new_SDefault_branch nid_opt = SDefault, nid_opt

(* generate a native block that is not bound 
   to a particular point in the control flow. 
   this is for objects that are placed into 
   the program manually and not optimized. *)
let new_native_block oid loc print_fcn =
  SchedBlock (oid, { sb_print = print_fcn; sb_loc = loc })
;;

let new_config_block oid cb = ConfigBlock (oid, cb)

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
  | MetaVar _
  | StructDef _
  | StructVar _
  | DConst _
  | ParseTree _
  | ConfigBlock _
  | SchedBlock _ -> true
  | Table _ | Action _ | InstrVec _ | SInstrVec _ | Hasher _ -> false
;;

(* new destructors *)

let name_of_structdef dec =
  match dec with
  | StructDef (mid, _, _) -> mid
  | _ -> error "Not a struct def"
;;

let ty_of_structdef dec =
  match dec with
  | StructDef (_, s_ty, _) -> s_ty
  | _ -> error "not a struct def"
;;

let name_of_structinst dec =
  match dec with
  | StructVar (structname, _, _) -> structname
  | _ -> error "not a struct instance decl"
;;

let width_of_regvec dec =
  match dec with
  | RegVec (_, wid, _, _, _) -> wid
  | _ -> error "not a regvec decl"
;;

let rules_of_table dec = 
  match dec with
  | Table (_, rules, _) -> rules 
  | _ -> error "not a table"
;;

let keys_of_table dec =
  match dec with
  | Table (_, rules, _) -> match_vars_of_rules rules
  | _ -> error "not a table"
;;

let conditions_eq (x : condition) y =
  match x, y with
  | Any, Any -> true
  | Exact xc, Exact yc -> Integer.equal xc yc
  | _ -> false
;;

let pat_ele_eq ex ey =
  Cid.equal (fst ex) (fst ey) && conditions_eq (snd ex) (snd ey)
;;

let rec patterns_contains ex y =
  match y with
  | [] -> false
  | hd :: y ->
    (match pat_ele_eq ex hd with
    | true -> true
    | false -> patterns_contains ex y)
;;

let patterns_remove ex y =
  let filter_f ex ey = not (pat_ele_eq ex ey) in
  CL.filter (filter_f ex) y
;;

let rec pattern_eq (x : pattern) (y : pattern) =
  match x with
  | [] ->
    (match y with
    | [] -> true
    | _ -> false)
  | hd :: tl ->
    (match patterns_contains hd y with
    | true -> pattern_eq tl (patterns_remove hd y)
    | false -> false)
;;

let rule_eq x y =
  match x, y with
  | Match (_, patx, oidx), Match (_, paty, oidy) ->
    pattern_eq patx paty & Cid.equals oidx oidy
  | OffPath patx, OffPath paty -> pattern_eq patx paty
  | _ -> false
;;

(* more helpers... *)
let rule_pattern rule = match rule with 
  | Match(_, pat, _) -> pat
  | OffPath(pat) -> pat
;;

module Generators = struct
  let int_const i : const = Integer.of_int i
  let const_oper i = Const i
  let int_oper i = int_const i |> const_oper
  let cid_oper c = Meta c
  let oper_expr o = Oper o
  let int_expr i = int_oper i |> oper_expr
  let cid_expr c = cid_oper c |> oper_expr

  (*o1 <op> o2*)
  let binop_expr op o1 o2 = BinOp (op, [o1; o2])

  (*c:cid + i:int*)
  let incr_expr c i = binop_expr Add (cid_oper c) (int_oper i)

  (* c := o:oper *)
  let oper_assign_instr c o = IAssign (c, o)

  let oper_assign_instrs cs os = 
    CL.map2 oper_assign_instr cs os 
  ;;

  (* c := i:int *)
  let int_assign_instr c i = int_expr i |> oper_assign_instr c

  (* c := c2:cid + i:int *)
  let incr_assign_instr c1 c2 i = incr_expr c2 i |> oper_assign_instr c1
  let validate_instr c = IValidate c

  (* create a header struct h_t with fields fs of widths ws *)
  (* | StructDef of mid * structType * (mid * int) list  *)
  (* a header or metadata structure *)

  let hdr_struct h_t (fs, ws) = StructDef (h_t, SHeader, CL.combine fs ws)

  (* Create an empty instance of s with id h. *)
  let struct_inst s h =
    match s with
    | StructDef (struct_id, _, _) -> StructVar (h, SPublic, struct_id)
    | _ -> error "[struct_inst] s is not a struct definition"
  ;;

  (* parse helpers *)
  let parse_node name stmts next = name, stmts, next


  let noop_rule = OffPath([])
  let concrete_noop pat = 
    let acnid = Cid.fresh ["noop"] in 
    let ruleid = Cid.fresh ["noop_rule"] in 
    let acn = Action(acnid, [], []) in 
    let rule = Match(ruleid, pat, acnid) in 
    rule, acn  
  ;; 

  let noop_action idstr = Action((Cid.fresh [idstr]), [], [])


  ;;

end