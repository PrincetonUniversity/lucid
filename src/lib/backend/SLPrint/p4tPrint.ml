open Batteries
open LLSyntax
open MiscUtils
open Format
open Base
open Consts
open PrintUtils
open SLSyntax
open P4tContext
open LLConstants

(* new, simpler printing of a P4 program from the IR. *)

(* logging *)
module DBG = BackendLogging

let outc = ref None
let dprint_endline = ref DBG.no_printf
let start_logs () = DBG.start_mlog __FILE__ outc dprint_endline


let memName = "memCell"
let retName = "retCell"
let tmpName = "tmp"

(* general helpers. *)
let print_fmt_list fmt objs obj_printer =
  let ppSep fmt () = fprintf fmt "@," in
  pp_print_list ~pp_sep:ppSep obj_printer fmt objs
;;

(**** identifier printing ****)
(* print the name of a variable. 
  Assumes that compound names are globally unique handler params. 
  Assumes that non-compound names are not globally unique, and so need ids. *)
let str_of_varid mid =
  match mid with
  | Compound _ ->
    let names = Cid.names mid in
    String.concat ~sep:"." names
  | Id i -> Id.to_string_delim "_" i
;;

let str_of_varids mids = CL.map str_of_varid mids |> String.concat ~sep:", "

(* variables that are public and unique *)
let str_of_public_varid mid =
  let names = Cid.names mid in
  String.concat ~sep:"." names
;;

let rec list_of_cid cid =
  match cid with
  | Id (s, i) -> [s, i]
  | Compound ((s, i), cid) -> (s, i) :: list_of_cid cid
;;

let print_optional_mid rm =
  match rm with
  | Some rm -> fprintf str_formatter "%s" (str_of_varid rm)
  | None -> ()
;;

let args_to_str (ms : mid list) : string =
  String.concat ~sep:", " (List.map ms ~f:str_of_varid)
;;

(* private objects that are not seen outside of DPT. *)
let rec str_of_private_oid (m : mid) : string =
  let delim = "_" in
  let id_list = list_of_cid m in
  match Caml.List.rev id_list with
  | [] -> error "[str_of_private_oid] cid that converted into empty list..."
  | [(last_n, last_i)] -> "dpt" ^ delim ^ string_of_int last_i ^ delim ^ last_n
  | (last_n, last_i) :: front_rev ->
    let map_f (n, i) = n ^ delim ^ string_of_int i in
    let str_list = Caml.List.rev front_rev |> Caml.List.map map_f in
    "dpt"
    ^ delim
    ^ string_of_int last_i
    ^ delim
    ^ Caml.String.concat delim str_list
    ^ delim
    ^ last_n
;;

let str_of_private_oids ms =
  String.concat ~sep:", " (CL.map str_of_private_oid ms)
;;

(* public objects that are globally scoped, like the parse block *)
let str_of_public_globalid m : string =
  let names = Cid.names m in
  String.concat ~sep:"_" names
;;

(* the unqualified variable name *)
let strbase_of_varid mid =
  let names = Cid.names mid in
  CL.hd (CL.rev names)
;;

let str_of_vardec field_name field_width =
  sprintf "bit<%i> %s;" field_width (str_of_public_globalid field_name)
;;

module PrimitiveString = struct
  let str_of_oper oper =
    match oper with
    | Meta m -> str_of_varid m
    | Const c -> string_of_int (Integer.to_int c)
    | RegVar _ -> memName
    | NoOper -> ""
  ;;

  let str_of_opers opers =
    let opers = Core.List.map opers ~f:str_of_oper in
    Core.String.concat ~sep:", " opers
  ;;

  let sized_str_of_oper oper =
    match oper with
    | Const c ->
      string_of_int (Integer.to_int c) ^ "w" ^ string_of_int (Integer.size c)
    | _ -> str_of_oper oper
  ;;

  let sized_str_of_opers opers =
    let opers = Core.List.map opers ~f:sized_str_of_oper in
    Core.String.concat ~sep:", " opers
  ;;

  let str_of_const c = Integer.value_string c

  let str_of_const_oper i =
    match i with
    | Const v -> Integer.value_string v
    | _ -> error "[str_of_const_oper] not a Const"
  ;;

  (* operators *)
  let str_of_binop ao =
    match ao with
    | Add -> "+"
    | Sub -> "-"
    | SubR -> "-"
    | SatSub -> "|-|"
    | RShift -> ">>"
    | LShift -> "<<"
    | BAnd -> "&"
    | BOr -> "|"
    | _ -> error "unsupported binop"
  ;;

  let str_of_cmpop co =
    match co with
    | Eq -> "=="
    | Neq -> "!="
    | Lt -> "<"
    | Gt -> ">"
  ;;

  let str_of_boolop bo =
    let _ = bo in
    "UNIMPLEMENTED BOOL OP"
  ;;

  let str_of_cast (i : oper) (w : oper) =
    "(bit<" ^ str_of_const_oper w ^ ">) " ^ str_of_oper i
  ;;

  (* i[s:e] *)
  let str_of_slice (i : oper) (s : oper) (e : oper) = 
    (str_of_oper i)^"["^(str_of_const_oper s)^":"^(str_of_const_oper e)^"]"
  ;;  

  let rec str_of_expr (e : expr) : string =
    match e with
    | Oper i -> str_of_oper i ^ ";"
    | HashOp _ -> error "UNIMPLEMENTED_HASH(...);"
    (* Ops have the most cases *)
    | BinOp (op, args) -> (
      match op with 
        (* SubR gets its operands flipped. *)
        | SubR -> ( match args with 
          | [i1; i2] -> str_of_expr (BinOp (Sub, [i2; i1])) (* reverse operands *)
          | _ -> error "[str_of_expr] SubR opcode must have 2 operands.";
        )
        (* all the binary operations that compile to instructions *)
        | Add | Sub | SatSub | RShift | LShift | BAnd | BOr -> ( match args with 
            | [i1; i2] -> str_of_oper i1 ^ " " ^ str_of_binop op ^ " " ^ str_of_oper i2 ^ ";"
            | _ -> error "[str_of_expr] Binary operation with wrong number of operands.."
        )
        (* Cast is a binary operation, but it doesn't compile to an instruction 
           so we keep it separate for now. *)
        | Cast -> ( match args with 
            | [i1; w] -> str_of_cast i1 w ^ ";" 
            | _ -> error "[str_of_expr] Cast operation must have 2 operands."
        )
        | Slice -> ( match args with 
          | [i; s; e] -> str_of_slice i s e
          | _ -> error "[str_of_expr] Slice operation must have 3 operands."
        )
    )
  ;;

  let str_of_instr instr =
    match instr with
    | IAssign (lhs, rhs) -> str_of_varid lhs ^ " = " ^ str_of_expr rhs
    | IValidate lhs -> str_of_varid lhs ^ ".setValid();"
    | IInvalidate lhs -> str_of_varid lhs ^ ".setInvalid();"
  ;;

  let str_of_cond c =
    match c with
    | Exact z -> string_of_int (Integer.to_int z)
    | Any -> "_"
  ;;

  let str_of_pat pat =
    let pat_strs =
      match CL.length pat with
      | 0 -> str_of_cond Any (* this is the equivalent of a default rule. *)
      | 1 -> str_of_cond (snd (CL.hd pat))
      | _ ->
        let field_strs =
          Caml.List.map (fun (_, cond) -> str_of_cond cond) pat
        in
        "(" ^ Caml.String.concat ", " field_strs ^ ")"
    in
    pat_strs
  ;;

  let str_of_rule r =
    match r with
    | Match (_, pat, acn_id) ->
      str_of_pat pat ^ " : " ^ str_of_private_oid acn_id ^ "();"
    | OffPath pat -> str_of_pat pat ^ " : NOOP();"
  ;;
end

module PrintSalu = struct
  (**** salu monster... This needs to be seriously cleaned up. ****)

  let str_of_rslice rs =
    match rs with
    | Lo -> fprintf str_formatter "%s" memName
  ;;

  (* | Hi -> error "unsupported register_hi" *)

  let str_of_sbinop op rs oper : string =
    sprintf
      "%s %s %s"
      (PrimitiveString.str_of_oper rs)
      (PrimitiveString.str_of_binop op)
      (PrimitiveString.str_of_oper oper)
  ;;

  let string_of_sexpr sExpr =
    match sExpr with
    | SVar v -> PrimitiveString.str_of_oper v
    | SBinOp (op, rs, oper) -> str_of_sbinop op rs oper
  ;;

  let print_spred_instr fmt (spred_expr : sPredExpr) =
    match spred_expr with
    | Comp (sv1, bin_op, sv2, cmp_op, sv3) ->
      let sv1 = PrimitiveString.str_of_oper sv1 in
      let sv2 = PrimitiveString.str_of_oper sv2 in
      let sv3 = PrimitiveString.str_of_oper sv3 in
      fprintf
        fmt
        "if (%s %s %s %s %s){@,"
        sv1
        (PrimitiveString.str_of_binop bin_op)
        sv2
        (PrimitiveString.str_of_cmpop cmp_op)
        sv3
    | Neg (sv1, bin_op, sv2, cmp_op, sv3) ->
      let sv1 = PrimitiveString.str_of_oper sv1 in
      let sv2 = PrimitiveString.str_of_oper sv2 in
      let sv3 = PrimitiveString.str_of_oper sv3 in
      fprintf
        fmt
        "if (!(%s %s %s %s %s)){@,"
        sv1
        (PrimitiveString.str_of_binop bin_op)
        sv2
        (PrimitiveString.str_of_cmpop cmp_op)
        sv3
  ;;

  (* print the body of a return instruction with known output variable *)
  let print_sinstr_with_outvar fmt outVarName predExp opExp =
    match predExp with
    | None -> fprintf fmt "%s=%s;" outVarName (string_of_sexpr opExp)
    | Some predExp ->
      pp_open_vbox fmt 4;
      print_spred_instr fmt predExp;
      fprintf fmt "%s=%s;" outVarName (string_of_sexpr opExp);
      pp_close_box fmt ();
      fprintf fmt ";@,}"
  ;;

  (* fprintf fmt "@," *)

  (* pair workaround may be needed here. [1/19/21] *)
  let rec has_ret_istr sIv =
    match sIv with
    | [] -> false
    | RetExpr _ :: _ -> true
    | _ :: sIv -> has_ret_istr sIv
  ;;

  let use_tmp = false

  (* print a sExpr *)
  let print_sinstr fmt (si : sExpr) =
    match si with
    | MemExpr (predExp, opExp) ->
      print_sinstr_with_outvar fmt memName predExp opExp
    | RetExpr (predExp, opExp) ->
      (match use_tmp with
      | true -> print_sinstr_with_outvar fmt tmpName predExp opExp
      | false -> print_sinstr_with_outvar fmt retName predExp opExp)
  ;;

  let print_sinstrs (statefulIv : sExpr list) rid_width =
    (* We may need to add a temporary variable for the return value. See section on array.update in research notebook. *)
    (* print all instructions. We expect all the ret instructions to come first. *)
    (* gotta write output to a tmp variable, or p4 gets confused... *)
    let ppSep fmt () = fprintf fmt "@," in
    if use_tmp
    then (
      match has_ret_istr statefulIv with
      | true ->
        fprintf str_formatter "bit<%s> %s;@," (string_of_int rid_width) tmpName
      | false -> ());
    pp_print_list ~pp_sep:ppSep print_sinstr str_formatter statefulIv;
    if use_tmp
    then (
      match has_ret_istr statefulIv with
      | true -> fprintf str_formatter "@,%s=%s;@," retName tmpName
      | false -> ())
  ;;

  let print_sInstr fmt callable_id rid rid_width siv out_var_opt idx_oper =
    (* print_endline ("printing sInstr: " ^ qstr_of_mid callable_id); *)
    let salu_routine_id = Cid.str_cons "sprog" callable_id in
    (* print the register action *)
    pp_open_vbox str_formatter 4;
    (* <type, index type, output type> *)
    fprintf
      fmt
      "RegisterAction<bit<%s>,bit<%s>,bit<%s>>(%s) %s = {@,"
      (string_of_int rid_width)
      (string_of_int defWidth)
      (string_of_int rid_width)
      (str_of_private_oid rid)
      (str_of_private_oid salu_routine_id);
    pp_open_vbox str_formatter 4;
    fprintf
      fmt
      "void apply(inout bit<%s> %s, out bit<%s> %s) {@,"
      (string_of_int rid_width)
      memName
      (string_of_int rid_width)
      retName;
    print_sinstrs siv rid_width;
    pp_close_box str_formatter ();
    fprintf fmt "@,}";
    pp_close_box str_formatter ();
    fprintf fmt "@,};@,";
    (* print the callable outer action. *)
    tab ();
    fprintf fmt "action %s() {@," (str_of_private_oid callable_id);
    let print_execute_si_stmt out_var_opt salu_routine_id idx_oper =
      match out_var_opt with
      | Some _ ->
        print_optional_mid out_var_opt;
        fprintf
          str_formatter
          "=%s.execute((bit<%s>)%s);"
          (str_of_private_oid salu_routine_id)
          (string_of_int defWidth)
          (PrimitiveString.str_of_oper idx_oper)
      | None ->
        fprintf
          str_formatter
          "%s.execute((bit<%s>)%s);"
          (str_of_private_oid salu_routine_id)
          (string_of_int defWidth)
          (PrimitiveString.str_of_oper idx_oper)
    in
    print_execute_si_stmt out_var_opt salu_routine_id idx_oper;
    untab ();
    fprintf fmt "@,}@,"
  ;;
end

module PrintTable = struct
  (* table declarations *)

  (** this needs *some* refactoring **)
  let print_tbl_key fmt match_vars =
    match match_vars with
    | [] -> ()
    | _ ->
      tab ();
      fprintf fmt "key = {@,";
      pp_print_list
        ~pp_sep:lineSep
        (fun fmt key -> fprintf fmt "%s : ternary;" (str_of_varid key))
        fmt
        match_vars;
      untab ();
      fprintf fmt "@,}@,"
  ;;

  (* print the actions associated with a table *)
  let print_tbl_actions fmt rules =
    tab ();
    fprintf fmt "actions = {@,";
    let aids = rules_to_aids rules in
    pp_print_list
      ~pp_sep:lineSep
      (fun fmt aid -> fprintf fmt "%s;" (str_of_private_oid aid))
      fmt
      aids;
    (* Caml.List.iter (fun aid -> printf "%s;@," (Cid.to_string aid)) aids; *)
    untab ();
    fprintf fmt "@,}@,"
  ;;

  (* print a default rule for a table with no entries *)
  let print_tbl_default fmt rules =
    let is_wildcard r =
      match r with
      | Match (_, [(_, Any)], _) -> true
      | Match (_, [], _) ->
        true (* if there's no pattern at all, its a wildcard. *)
      | _ -> false
    in
    let wildcard_rules = CL.filter is_wildcard rules in
    (* this is a default rule. *)
    let rules = CL.filter (fun r -> not (is_wildcard r)) rules in
    match wildcard_rules with
    | [] ->
      fprintf fmt "//no default action@,";
      rules
    | [r] ->
      let _ = r in
      (* fprintf fmt "const default_action = %s();@," (str_of_private_oid (aid_of_rule r)); *)
      rules
    | _ ->
      error
        "[print_tbl_rules] more than 1 wildcard rule in single-column table."
  ;;

  (* print the rule entries (map keys to actions) *)
  let print_tbl_rules fmt rules =
    (* 
      - if rules has a single entry, we expect it to be a default entry to print as a default action. 
      This must be a default action for tables with no keys. 
      - otherwise, if the table has multiple entries, it must have keys, so we can print 
      the rules as a list of const entries. 
    *)
    match List.length rules with
    | 0 -> error "a table with no rules. This should not be possible."
    | 1 ->
      fprintf
        fmt
        "const default_action = %s();@,"
        (str_of_private_oid (aid_of_rule (CL.hd rules)))
    | _ ->
      tab ();
      fprintf fmt "const entries = {@,";
      let rule_strs = Caml.List.map PrimitiveString.str_of_rule rules in
      pp_print_list
        ~pp_sep:lineSep
        (fun fmt s -> fprintf fmt "%s" s)
        fmt
        rule_strs;
      (* Caml.List.iter (fun s -> fprintf fmt "%s@," s) rule_strs; *)
      untab ();
      fprintf fmt "@,}@,"
  ;;

  (* print pragmas for the table: 
    stage pragma
    ignore table deps pragmas 
    (one for every other table in the same stage)
  *)
  let print_tbl_pragmas fmt tbl_id stage_opt stage_tbl_map =
    match stage_opt with
    | Some stage_num ->
      fprintf fmt "@pragma stage %i@," stage_num;
      let same_stage_tbls = CL.assoc stage_num stage_tbl_map in
      let other_ss_tbls =
        CL.filter (fun t -> not (Cid.equals tbl_id t)) same_stage_tbls
      in
      let ignore_dep_printer other_tid =
        fprintf
          fmt
          "@ignore_table_dependency(\"Ingress.%s\")@,"
          (str_of_private_oid other_tid)
      in
      CL.iter ignore_dep_printer other_ss_tbls
    | None -> fprintf fmt "// Stage not set by dptc@,"
  ;;

  let print_tbl fmt tbl_id match_vars rules stage_opt stage_tbl_map =
    (*   fprintf fmt "//table %s@," (qstr_of_mid tbl_id); *)
    print_tbl_pragmas fmt tbl_id stage_opt stage_tbl_map;
    tab ();
    fprintf fmt "table %s {@," (str_of_private_oid tbl_id);
    print_tbl_key fmt match_vars;
    print_tbl_actions fmt rules;
    print_tbl_rules fmt rules;
    untab ();
    fprintf fmt "@,}@,"
  ;;

  (*   fprintf fmt "//end table %s@," (qstr_of_mid tbl_id) *)
end

(*** P4 objects inside of the ingress block ***)
module PrintComputeObject = struct
  let print_reg fmt rid wid len def =
    fprintf
      fmt
      "Register<bit<%i>, bit<%i>>(%i, %i) %s;@,"
      wid
      defWidth
      len
      (Integer.to_int def)
      (str_of_private_oid rid)
  ;;

  let print_ivec fmt alu_id iVec =
    (* print a vector of alu instructions *)
    tab ();
    fprintf fmt "action %s( ){@," (str_of_private_oid alu_id);
    let iter_f instr =
      let st = PrimitiveString.str_of_instr instr in
      fprintf fmt "%s@," st
    in
    CL.iter iter_f iVec;
    untab ();
    fprintf fmt "@,}@,"
  ;;

  let print_hasher fmt hasher_id out_width poly out_var in_vars =
    let obj_id = Cid.str_cons "hasher" hasher_id in
    let poly_name = str_of_private_oid (Cid.fresh ["poly"]) in
    fprintf
      fmt
      "CRCPolynomial<bit<%s>>(%s, true, false, false, 0, 0) %s;@,"
      (string_of_int out_width)
      (string_of_int poly)
      poly_name;
    fprintf
      fmt
      "Hash<bit<%s>>(HashAlgorithm_t.CUSTOM, %s) %s;@,"
      (string_of_int out_width)
      poly_name
      (str_of_private_oid obj_id);
    (* print_endline (args_to_str in_vars); *)
    let call_hasher_stmt_str =
      let hasher_str =
        sprintf
          "%s = %s.get({%s});"
          (str_of_varid out_var)
          (str_of_private_oid obj_id)
          (PrimitiveString.sized_str_of_opers in_vars)
      in
      (* print_endline ("[HASHER STR] "^(hasher_str)); *)
      hasher_str
    in
    (* print the action wrapper *)
    tab ();
    fprintf fmt "action %s() {@," (str_of_private_oid hasher_id);
    fprintf fmt "%s@," call_hasher_stmt_str;
    untab ();
    fprintf fmt "}@,"
  ;;

  let print_var fmt midWidth midId =
    (* print_var_pragmas fmt newMid; *)
    (* allow P4 to overlay variables by commenting out this line *)
    fprintf fmt "bit<%i> %s;@," midWidth (str_of_varid midId)
  ;;

  let print_action fmt actn_id obj_ids next_tids =
    (* get the object i Should an action have "parallel statements" in it instead? *)
    let print_obj_call obj_id =
      fprintf fmt "%s();@," (str_of_private_oid obj_id)
    in
    tab ();
    fprintf fmt "action %s( ){@," (str_of_private_oid actn_id);
    Caml.List.iter print_obj_call obj_ids;
    fprintf fmt "//next tables: [%s]@," (str_of_private_oids next_tids);
    untab ();
    fprintf fmt "@,}@,"
  ;;

  let print_native_block fmt blockid blockdef =
    print_endline "got a native block to print!";
    (* call the block printer to get the string *)
    let block_str = blockdef.sb_print blockid in
    print_endline "---- native block ----";
    print_endline block_str;
    print_endline "---- native block ----";
    fprintf fmt "%s@," block_str
  ;;

  let print_decl stage_tbl_map fmt (decl : decl) =
    match decl with
    | RegVec (rid, wid, len, def, _) -> print_reg fmt rid wid len def
    | InstrVec (iid, iVec) -> print_ivec fmt iid iVec
    | SInstrVec (sid, {sRid=rid; sWid=rid_width; sExprs=siv; sOut=out_var_opt; sIdx=idx_oper}) ->
      PrintSalu.print_sInstr fmt sid rid rid_width siv out_var_opt idx_oper
    | Hasher (hasher_id, out_width, poly, out_var, in_vars) ->
      print_hasher fmt hasher_id out_width poly out_var in_vars
    | MetaVar (newMid, midWidth) -> print_var fmt midWidth newMid
    | Action (actn_id, obj_ids, next_tids) ->
      print_action fmt actn_id obj_ids next_tids
    | Table (tbl_id, branches, stage_opt) ->
      PrintTable.print_tbl
        fmt
        tbl_id
        (match_vars_of_rules branches)
        branches
        stage_opt
        stage_tbl_map
    | SchedBlock (_, _) ->
      ()
      (* 5/18 -- don't print the scheduler blocks here *)
      (* print_native_block fmt oid blockdef  *)
    | _ -> ()
  ;;

  let filter_decl decl =
    match decl with
    | RegVec _
    | InstrVec _
    | SInstrVec _
    | Hasher _
    | MetaVar _
    | Action _
    | Table _
    | SchedBlock _ -> true
    | _ -> false
  ;;

  let print_decls decls =
    (* get a mapping from stage number to table, for 
       printing the ignore table dependency pragmas *)
    let printable_decls = CL.filter filter_decl decls in
    print_fmt_list
      str_formatter
      printable_decls
      (print_decl (stageMap_of_tbls decls))
  ;;
end

module PrintControlFlow = struct
  let rec print_dp_stmt (st : tblStmt) =
    match st with
    | Noop -> fprintf str_formatter ""
    | CallTable table_id ->
      fprintf str_formatter "%s.apply();" (str_of_private_oid table_id)
    | Seq (st1, st2) ->
      print_dp_stmt st1;
      fprintf str_formatter "@,";
      print_dp_stmt st2
  ;;

  let print_apply_calls tbl_seq =
    tab ();
    fprintf str_formatter " @,";
    print_dp_stmt tbl_seq.tsstmt;
    untab ()
  ;;

  let p4_final_call_str = "dptContinueHandler.apply();"
end

(**** struct defs and instances ****)
module PrintStruct = struct
  let str_of_type s_ty =
    match s_ty with
    | SMeta -> "struct"
    | SHeader -> "header"
    (* | SHeader -> "@flexible header" *)
  ;;

  let print_def fmt decl =
    match decl with
    | StructDef (s_name, s_ty, s_fields) ->
      tab ();
      fprintf fmt "%s %s {@," (str_of_type s_ty) (str_of_public_globalid s_name);
      let print_f fmt (fname, fwid) =
        fprintf fmt "%s" (str_of_vardec fname fwid)
      in
      print_fmt_list fmt s_fields print_f;
      untab ();
      fprintf fmt "@,}"
    | _ -> ()
  ;;

  let print_inst fmt decl =
    match decl with
    | StructVar (s_inst_name, _, s_def_name) ->
      fprintf
        fmt
        "%s %s;"
        (str_of_public_globalid s_def_name)
        (strbase_of_varid s_inst_name)
    | _ -> error "not a struct instance decl"
  ;;
end

(**** parse tree ****)
module PrintParseTree = struct
  let print_instr fmt parse_instr =
    match parse_instr with
    | PPeek (varname, varwidth) ->
      fprintf
        fmt
        "%s = %s.lookahead<bit<%i>>();@,"
        (str_of_varid varname)
        pkt_instance_prefix
        varwidth
    | PStruct structname ->
      fprintf
        fmt
        "%s.extract(%s);@,"
        pkt_instance_prefix
        (str_of_varid structname)
    | PSet (l_mid, r_mid) ->
      fprintf fmt "%s = %s;@," (str_of_varid l_mid) (str_of_varid r_mid)
  ;;

  let string_of_select_pat s_pat =
    match s_pat with
    | SConst i -> string_of_int i
    | SDConst mid ->
      print_endline ("[string_of_select_pat]" ^ show_select_pat s_pat);
      str_of_public_globalid mid
    | SDefault -> "default"
  ;;

  let string_of_nextid_opt nid_opt =
    match nid_opt with
    | None -> "accept"
    | Some node_id -> str_of_public_globalid node_id
  ;;

  let print_branch fmt (s_pat, nid_opt) =
    fprintf
      fmt
      "%s : %s;@,"
      (string_of_select_pat s_pat)
      (string_of_nextid_opt nid_opt)
  ;;

  let print_transition fmt parse_transition =
    match parse_transition with
    | PNext (Some pnode_id) ->
      fprintf fmt "transition %s;@," (str_of_public_globalid pnode_id)
    | PNext None -> fprintf fmt "transition accept;@,"
    | PSelect (select_var, branches) ->
      fprintf fmt "transition select(%s) {@," (str_of_varid select_var);
      new_tab ();
      print_fmt_list fmt branches print_branch;
      untab ();
      fprintf fmt "@,}"
  ;;

  (* print a single P4 parse node *)
  let print_node fmt parse_node =
    let name, stmts, next = parse_node in
    (* header *)
    fprintf fmt "state %s {@," (str_of_public_globalid name);
    new_tab ();
    (* parse instructions *)
    print_fmt_list fmt stmts print_instr;
    (* transition *)
    print_transition fmt next;
    (* end *)
    untab ();
    fprintf fmt "@,}"
  ;;
end

(**** top level interface ****)
(* print the objects defined inside of the ingress control block *)
let str_of_igr_objs decls =
  open_block ();
  fprintf str_formatter " @,";
  PrintComputeObject.print_decls decls;
  close_block ()
;;

(* print the straightline sequence of table calls in the apply block. *)
let print_p4_calls tbl_seq =
  open_block ();
  PrintControlFlow.print_apply_calls tbl_seq;
  close_block ()
;;

let print_p4_struct_defs decls =
  let struct_decls =
    CL.filter
      (fun d ->
        match d with
        | StructDef _ -> true
        | _ -> false)
      decls
  in
  open_block ();
  fprintf str_formatter "@,";
  print_fmt_list str_formatter struct_decls PrintStruct.print_def;
  close_block ()
;;

let print_const_def fmt (name, scope, width, valu) =
  print_endline ("emitting const def for " ^ str_of_varid name);
  (* constant definitions are public variables (i.e., used in P4) *)
  let id_str =
    match scope with
    | SPublic -> str_of_public_varid name
    | SPrivate -> str_of_varid name
  in
  (* fprintf fmt "const bit<%i> %s=%i;" width id_str valu *)
  (* use defines instead of consts, so that the P4 program can use 
     the event id names in table rules. *)
  let _ = width in 
  fprintf fmt "#define %s %i" id_str valu

;;

let print_p4_const_defs decls =
  new_open_block ();
  let constdef_decls =
    CL.filter_map
      (fun d ->
        match d with
        | DConst (name, scope, width, valu) -> Some (name, scope, width, valu)
        | _ -> None)
      decls
  in
  print_fmt_list str_formatter constdef_decls print_const_def;
  close_block ()
;;

(* format all struct definitions of type SHeader *)
let print_p4_hdr_insts decls =
  let filter_f dec =
    match dec with
    | StructVar (_, _, s_def_name) ->
      (match ctx_get_structdef_ty s_def_name with
      | SHeader -> true
      | _ -> false)
    | _ -> false
  in
  let hdrinst_decls = CL.filter filter_f decls in
  new_open_block ();
  print_fmt_list str_formatter hdrinst_decls PrintStruct.print_inst;
  close_block ()
;;

let print_p4_md_insts decls =
  let filter_f dec =
    match dec with
    | StructVar (_, _, s_def_name) ->
      print_endline ("[print_p4_md_insts] printing metadata struct: " ^ Cid.to_string s_def_name);
      (match ctx_get_structdef_ty s_def_name with
      | SMeta -> true
      | _ -> false)
    | _ -> false
  in
  let hdrinst_decls = CL.filter filter_f decls in
  new_open_block ();
  print_fmt_list str_formatter hdrinst_decls PrintStruct.print_inst;
  close_block ()
;;

let print_p4_parser decls =
  (* The P4 parser function *)
  let filter_f dec =
    match dec with
    | ParseTree (parser_name, nodes) -> Some (parser_name, nodes)
    | _ -> None
  in
  let parser_name, nodes = CL.filter_map filter_f decls |> CL.hd in
  print_endline ("parser name: " ^ str_of_public_globalid parser_name);
  open_block ();
  (* block header *)
  fprintf
    str_formatter
    "parser %s (packet_in %s, out header_t %s, out metadata_t %s) {@,"
    (str_of_public_globalid parser_name)
    pkt_instance_prefix
    hdr_instance_prefix
    md_instance_prefix;
  (* body *)
  print_fmt_list str_formatter nodes PrintParseTree.print_node;
  (* tail *)
  untab ();
  fprintf str_formatter "@,}";
  close_block ()
;;

let print_p4_prototype_egr () =
  (* this only supports one event generation per input packet and only 
    local background events. *)
  let egr_apply_body_str =
    indent_block
      ~nspaces:8
      "\n\
       if (eg_intr_md.egress_port != DPT_RECIRC_PORT){\n\
      \   if (hdr.ethernet.ether_type == DPT_ETYPE) {\n\
      \     //this is a non-local DPT packet. Drop it for now.\n\
      \     eg_dprsr_md.drop_ctl = 1;\n\
      \   } // packets not on the DPT port and not DPT_ETYPE are regular \
       packets, do nothing.\n\
      \ } else {\n\
      \   if (hdr.ethernet.ether_type != DPT_ETYPE) {\n\
      \     // this is a non dpt packet on the DPT recirc port -- should not \
       happen.\n\
      \     eg_dprsr_md.drop_ctl = 1;\n\
      \   }\n\
       }\n"
  in
  open_block ();
  fprintf str_formatter "%s" egr_apply_body_str;
  close_block ()
;;

(* printers for scheduler blocks *)
(* these functions do all the printing for blocks with a specific 
location. Put their output at the appropriate place in the string. *)
let print_located_sched_block tgt_loc d =
  match d with
  | SchedBlock (oid, { sb_print; sb_loc }) ->
    (match sb_loc == tgt_loc with
    | true -> fprintf str_formatter "%s@," (sb_print oid)
    | false -> ())
  | _ -> ()
;;

let str_of_located_sched_block tgt_loc decls =
  open_block ();
  fprintf str_formatter " @,";
  CL.iter (print_located_sched_block tgt_loc) decls;
  close_block ()
;;

let print_located_sched_call tgt_loc d =
  match d with
  | SchedBlock (oid, { sb_print = _; sb_loc }) ->
    (match sb_loc == tgt_loc with
    | true -> fprintf str_formatter "%s.apply();" (str_of_private_oid oid)
    | false -> ())
  | _ -> ()
;;

let callstr_of_located_scheds tgt_loc decls =
  open_block ();
  fprintf str_formatter " @,";
  CL.iter (print_located_sched_call tgt_loc) decls;
  close_block ()
;;

let str_of_igr_scheds = str_of_located_sched_block LIgrEnd
let callstr_of_igr_scheds = callstr_of_located_scheds LIgrEnd
let str_of_egr_scheds = str_of_located_sched_block LEgr
let callstr_of_egr_scheds = callstr_of_located_scheds LEgr

let from_straightline (blk : tblSeqProg) =
  (* set up context for event structs. *)
  ctx_add_structdefs blk.tspdecls;
  let dpt_igr_obj_str =
    str_of_igr_objs blk.tspdecls ^ str_of_igr_scheds blk.tspdecls
  in
  let dpt_igr_call_str =
    print_p4_calls blk.tsptblseq ^ callstr_of_igr_scheds blk.tspdecls
  in
  let dpt_hdr_decls_str =
    print_p4_const_defs blk.tspdecls
    (* defines go in the same block as struct defs *)
    ^ print_p4_struct_defs blk.tspdecls
  in
  let dpt_hdr_inst_str = print_p4_hdr_insts blk.tspdecls in
  let dpt_meta_inst_str = print_p4_md_insts blk.tspdecls in
  let dpt_parser_str = print_p4_parser blk.tspdecls in
  (* let dpt_egr_call_str = print_p4_prototype_egr () in  *)
  let dpt_egr_obj_str = str_of_egr_scheds blk.tspdecls in
  let dpt_egr_call_str = callstr_of_egr_scheds blk.tspdecls in
  let generated_blocks =
    [ dpt_igr_objs, dpt_igr_obj_str
    ; dpt_igr_call, dpt_igr_call_str
    ; dpt_igr_raw_call, dpt_igr_call_str
    ; dpt_hdr_decls, dpt_hdr_decls_str
    ; dpt_hdr_inst, dpt_hdr_inst_str
    ; dpt_meta_inst, dpt_meta_inst_str
    ; dpt_parser, dpt_parser_str
    ; dpt_egr_objs, dpt_egr_obj_str
    ; dpt_egr_call, dpt_egr_call_str ]
  in
  (*   print_endline ("---- ingress decls----");
  print_endline (dpt_igr_obj_str);
  print_endline ("---- ingress calls----");
  print_endline (dpt_igr_call_str);
  print_endline ("---- header decls ----");
  print_endline dpt_hdr_decls_str;
  print_endline ("---- header instances ----");
  print_endline dpt_hdr_inst_str;
  print_endline ("---- metadata instances ----");
  print_endline dpt_meta_inst_str;
  print_endline ("---- parser ----");
  print_endline dpt_parser_str;
  print_endline ("---- egress objects ----");
  print_endline dpt_egr_obj_str;
  print_endline ("---- egress calls ----");
  print_endline dpt_egr_call_str; *)
  generated_blocks
;;
