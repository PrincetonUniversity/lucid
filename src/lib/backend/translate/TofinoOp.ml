(* translate individual operation statements from the
source syntax to tofino compute objects *)
open Syntax
module Pr = Printing
module IS = InstrSyntax
open InterpHelpers
open TofinoContext
open TofinoConstants
open OpGraph

(* logging *)
module DBG = BackendLogging

let outc = ref None
let dprint_endline = ref DBG.no_printf

(* 5/18 added these quickly, they belong... somewhere else... *)
(* get the variables that are not consts. Used in keys. *)
let dynamic_vars_in_exp (ex : exp) = 
  vars_in_exp ex |> 
  CL.filter (fun v -> not (ctx_var_is_const v))
;;
(* get an integer from an expression. If the expression is a const, get the computed value. *)
let int_from_const_exp (ex : exp) =
  print_endline ("[int_from_exp]: " ^ Printing.exp_to_string ex);
  match ex.e with
  | EVal { v = VInt zint; _ } -> Integer.to_int zint
  | EInt (z, _) ->
    Z.to_int z (* Integer.create_z ~value:z ~size:(extract_size sz)  *)
  | EVar(cid) -> TofinoContext.ctx_int_of_const cid
  | _ -> trans_err "could not evaluate expression to an int" ex
;;


module TofinoStructs = struct
  (**** translate structure names ****)

  let defname_from_evid evid = Cid.id ("e_" ^ fst evid, snd evid)

  (* struct names from event ids *)
  let structname_from_evid evid =
    let ev_name, ev_num = evid in
    let meta_struct_ty = Cid.id (ev_name ^ event_structdef_suffix, ev_num) in
    meta_struct_ty
  ;;

  (* struct instance name from event id. *)
  let in_struct_from_ev evid = Cid.id evid

  let out_struct_from_ev evid =
    Cid.id (out_structname_prefix ^ fst evid, snd evid)
  ;;

  (* struct instance name from event id, with fully qualified prefix *)
  let full_in_struct_from_ev evid evsort =
    match evsort with
    | EEntry _ | EExit ->
      Cid.compound (Id.create md_instance_prefix) (in_struct_from_ev evid)
    | EBackground ->
      Cid.compound (Id.create hdr_instance_prefix) (in_struct_from_ev evid)
  ;;

  let full_out_struct_from_ev evid evsort =
    match evsort with
    | EEntry _ | EExit ->
      Cid.compound (Id.create md_instance_prefix) (out_struct_from_ev evid)
    | EBackground ->
      Cid.compound (Id.create hdr_instance_prefix) (out_struct_from_ev evid)
  ;;

  let qual_in_fieldnames_of_event hdl_id =
    let struct_instance =
      match ctx_find_event_instruct hdl_id with
      | Some instance_cid -> instance_cid
      | None ->
        error
          ("did not find an input struct in context for handler: "
          ^ Id.to_string hdl_id)
    in
    let qual_fieldnames =
      ctx_find_event_fields hdl_id
      |> CL.map (fun field_cid -> Cid.concat struct_instance field_cid)
    in
    qual_fieldnames
  ;;

  let qual_out_fieldnames_of_event hdl_id =
    let struct_instance =
      match ctx_find_event_outstruct hdl_id with
      | Some instance_cid -> instance_cid
      | None ->
        error
          ("did not find an input struct in context for handler: "
          ^ Id.to_string hdl_id)
    in
    let qual_fieldnames =
      ctx_find_event_fields hdl_id
      |> CL.map (fun field_cid -> Cid.concat struct_instance field_cid)
    in
    qual_fieldnames
  ;;
end

(**** translate operation statements to DPA objects ****)

(* atomic nodes of the syntax tree *)
let const_from_z (z : Z.t) = Integer.of_int (Z.to_int z)

let binOp_from_op op =
  match op with
  | Plus -> IS.Add
  | Sub -> IS.Sub
  | _ -> error "[binOp_from_op] unsupported op"
;;

(*** immediates ***)
let zint_from_evalue (immediate_exp : exp) = 
  match immediate_exp.e with
  | EVal value -> (
    match value.v with 
      | VInt zint -> zint
      | _ -> error "[oper_from_immediate] got value that cannot be translated directly into IR."
  )
  | EInt (z, _) ->
    Integer.of_int (Z.to_int z)
  | _ -> error "[int_from_immediate] got value that cannot be translated directly into IR."
  ;;

let oper_from_immediate (immediate_exp : exp) =
  match immediate_exp.e with
  | EVal _ | EInt _ -> IS.Const (zint_from_evalue immediate_exp)
  | EVar cid -> Meta cid
  | _ -> error "[oper_from_immediate] not a backend-recognized immediate."
;;

let soper_from_immediate (memcell_name : Cid.t option) (immediate_exp : exp) =
  match immediate_exp.e with
  | EVal _ | EInt _  -> IS.ConstVar (zint_from_evalue immediate_exp)
  | EVar n ->
    (match memcell_name with
    | Some memcell_name ->
      (match Cid.equals memcell_name n with
      | true -> IS.RegVar Lo
      | false -> IS.MetaVar n)
    | None -> IS.MetaVar n)
  | _ -> error "[soper_from_immediate] expression is not an immediate"
;;

let eoper_from_immediate immediate_exp =
  IS.Oper (oper_from_immediate immediate_exp)
;;

(**** match conditions ****)
let condition_from_pat pat =
  match pat with
  | PWild -> IS.Any
  | PNum z -> IS.Exact (const_from_z z)
  | PBit _ -> error "bit-vectors in conditions not yet supported in backend."
;;

let pattern_from_branchguard keylist patlist : IS.pattern =
  let keys_pats = CL.combine keylist patlist in
  let map_f (key, pat) = key, condition_from_pat pat in
  CL.map map_f keys_pats
;;

let rule_from_branchguard keylist patlist acn_name =
  IS.new_rule
    (Cid.fresh ["match_stmt_rule"])
    (pattern_from_branchguard keylist patlist)
    acn_name
;;

let tblentry_from_branch keylist (patlist, stmt) =
  (* tricky: the statement here might not be an opstatement. *)
  let stmt = fst_op_of_branch stmt in 
  match stmt.s with
  | SNoop ->
    (* if the action in this branch is a noop, there is no successor.*)
    let acn_name = Cid.fresh ["match_stmt_action"] in
    let acn = IS.new_action acn_name [] [] in
    let rule = rule_from_branchguard keylist patlist acn_name in
    rule, acn
  | _ ->
    let acn_name = Cid.fresh ["match_stmt_action"] in
    let succ_tbl = tblname_of_stmt stmt in
    !dprint_endline
      ("successor statement in match: " ^ Printing.stmt_to_string stmt);
    !dprint_endline ("successor statement table: " ^ Cid.to_string succ_tbl);
    let acn = IS.new_action acn_name [] [succ_tbl] in
    let rule = rule_from_branchguard keylist patlist acn_name in
    rule, acn
;;

let successortbls_of_stmt opgraph opstmt =
  let succ_opstmts = StGraph.succ opgraph opstmt in
  CL.map tblname_of_stmt succ_opstmts
;;

open Format
let obj_decl_str obj = 
  PrintUtils.open_block ();
  fprintf str_formatter " @,";
  P4tPrint.PrintComputeObject.print_decls [obj];
  PrintUtils.close_block ()
;;

let log_objs opstmt objs =
  !dprint_endline "-------";
  !dprint_endline ("table name:  " ^ Cid.to_string (tblname_of_stmt opstmt));
  let iter_f obj = !dprint_endline (obj_decl_str obj) in
  CL.iter iter_f objs;
  !dprint_endline "-------"
;;

module TofinoAlu = struct
  (**** generate an (alu_name, alu_declaration) pair from an op statement  ****)
  let from_assign var_name val_exp opstmt =
    let base_name = uname_of_stmt opstmt in
    let alu_name = aluname_of_stmt opstmt in
    let alu_name, alu_obj =
      match val_exp.e with
      | EVal _ | EInt _ | EVar _ ->
        ( alu_name
        , IS.new_dsingleinstr alu_name var_name (eoper_from_immediate val_exp) )
      (* binary operations are alus as an action *)
      | EOp (op, [a; b]) ->
        let a = oper_from_immediate a in
        let b = oper_from_immediate b in
        let op = binOp_from_op op in
        alu_name, IS.new_dsingleinstr alu_name var_name (IS.new_ebinop op a b)
      | EOp (_, _) ->
        error "operations must all be binary for translation to tofino."
      | ECall (fcn_id, args) -> (
        (* a call could either be a call, or an event declaration. *)
        match (raw_ty_of_exp val_exp) with 
        | TEvent _ -> 
          let call_result = 
            ctx_call_codegen
              TofinoConstants.event_generate_cid
              { basename = Some base_name; retname = Some var_name; args = [val_exp] }
          in 
          CL.hd call_result.names, CL.hd call_result.objs
        | _ -> 
          t_info "[from_assign: ECall]";
          let call_result =
            ctx_call_codegen
              fcn_id
              { basename = Some base_name; retname = Some var_name; args }
          in
          CL.hd call_result.names, CL.hd call_result.objs
      )
      | EHash (size, exps) ->
        let width = SyntaxUtils.extract_size size in 
        let epoly = CL.hd exps in 
        let poly = int_from_const_exp epoly in 
        (* poly can be an integer or a const *)
        (* let poly = Integer.to_int (zint_from_evalue (CL.hd exps)) in  *)
        let args = CL.map oper_from_immediate (CL.tl exps) in 
        alu_name, (IS.new_hasher alu_name width poly var_name args)

      | EProj _ | ERecord _ ->
        error "Should be eliminated long before this point."
    in
    alu_name, alu_obj
  ;;

  let from_unit val_exp opstmt =
    t_info "[from_unit]";
    let base_name = uname_of_stmt opstmt in
    let alu_name, alu_obj =
      match val_exp.e with
      | ECall (name, args) ->
        let call_result =
          ctx_call_codegen
            name
            { basename = Some base_name; retname = None; args }
        in
        CL.hd call_result.names, CL.hd call_result.objs
      | _ -> error "unsupported expression in unit statement"
    in
    alu_name, alu_obj
  ;;

  (* TODO: add delay and location meta fields, put at the beginning, set to 0 initially *)
  let event_init_meta_instrs ev_id =
    let outstruct_id_opt = ctx_find_event_outstruct_cid ev_id in
    match outstruct_id_opt with
    | Some outstruct_id ->
      (* set the event id field in the packet header *)
      let init_event_id_instr = IS.new_iassign_int 
        (Cid.concat outstruct_id event_id_field)
        (ctx_find_event_iid ev_id)
      in 
      let init_event_mc_instr = IS.new_iassign_int 
        (Cid.concat outstruct_id event_mc_field)
        0
      in 
      let init_event_loc_instr = IS.new_iassign_int 
        (Cid.concat outstruct_id event_loc_field)
        0
      in 
      let init_event_delay_instr = IS.new_iassign_int 
        (Cid.concat outstruct_id event_delay_field)
        0
      in 
      (* let event_delay_field = Cid.concat outstruct_id event_ *)
      [init_event_id_instr; init_event_mc_instr; init_event_loc_instr; init_event_delay_instr]
    | _ -> []
  ;;

  let set_next_event_instr ev_id = 
    print_endline ("[set_next_event_instr] event id: "^(Cid.to_string ev_id));
    let event_iid = ctx_find_event_iid ev_id in
    (* set the event id field in lucid metadata *)
    let nextevent_id_field = Cid.create [md_instance_prefix; dpt_meta_str; next_event_str] in 
    IS.IAssign (nextevent_id_field, IS.new_expr_of_int event_iid)
  ;;
  let set_exit_event_instr ev_id = 
    let event_iid = ctx_find_event_iid ev_id in
    (* set the event id field in lucid metadata *)
    let exitevent_id_field = Cid.create [md_instance_prefix; dpt_meta_str; exit_event_str] in 
    IS.IAssign (exitevent_id_field, IS.new_expr_of_int event_iid)
  ;;
  (* generate an alu instruction from the instantiation of an event *)
  let from_event_instantiation alu_basename ev_id ev_args =
    !dprint_endline ("[generate_event] event id: " ^ Cid.to_string ev_id);
    !dprint_endline "[generate_event] event args: ";
    let iter_f ev_arg = !dprint_endline (Printing.exp_to_string ev_arg) in
    CL.iter iter_f ev_args;
    (* get a list of qualified out struct field parameters *)
    (* generate alu instructions of the form: field_param := ev_arg *)
    let out_struct_fields =
      TofinoStructs.qual_out_fieldnames_of_event (Cid.to_id ev_id)
    in
    let alu_rhs_exps = CL.map eoper_from_immediate ev_args in
    let to_ass_f (lhs, rhs) = IS.IAssign (lhs, rhs) in
    let (ivec : IS.instrVec) =
      CL.map to_ass_f (CL.combine out_struct_fields alu_rhs_exps)
    in
    let outstruct_id = ctx_find_event_outstruct_cid ev_id in
    (* add instructions to set metadata fields, e.g., event name *)
    let event_meta_instrs = event_init_meta_instrs ev_id in
    (* generate validate and continuation instructions, which differ for event types. *)
    let validate_instrs = match (ctx_find_eventrec ev_id).event_sort with 
      | EBackground -> [IS.IValidate (Option.get outstruct_id); set_next_event_instr ev_id]
      | _ -> [set_exit_event_instr ev_id] 
    in 
    let ivec = event_meta_instrs @ ivec @ validate_instrs in 
    (* return a declaration of an alu with this vector of instructions *)
    let alu_id = Cid.compound (Id.create "generate_alu") alu_basename in
    let alu_obj = IS.new_dinstr alu_id ivec in
    alu_id, alu_obj
  ;;
end

module TofinoControl = struct
  (* wrap a compute alu in an action and call table. *)
  let wrap_alu_in_call_table opgraph opstmt alu_name alu_obj =
    let acn_name = acnname_of_stmt opstmt in
    let tbl_name = tblname_of_stmt opstmt in
    let succ_tbls = successortbls_of_stmt opgraph opstmt in
    CL.iter (fun t -> !dprint_endline (Cid.to_string t)) succ_tbls;
    (* make the action, pointing to the compute object and any successor tables *)
    let acn = IS.Action (acn_name, [alu_name], succ_tbls) in
    (* make the table with one action *)
    let tbl = IS.Table (tbl_name, [IS.fresh_any_rule acn_name], None) in
    (* return the updated name context and the table, action, and (s)alu *)
    let new_objs = [tbl; acn; alu_obj] in
    log_objs opstmt new_objs;
    new_objs
  ;;

  (* generate control flow objects that wrap compute objects *)
  let from_assign opgraph opstmt =
    let var_name, val_exp = unpack_assign opstmt in
    let var_name = Cid.Id var_name in
    (* make the ALU that does the computation. *)
    let alu_name, alu_obj = TofinoAlu.from_assign var_name val_exp opstmt in
    (* wrap it in a table that calls the ALU and points to the next table. *)
    wrap_alu_in_call_table opgraph opstmt alu_name alu_obj
  ;;

  (* almost the same as assign, but there's also an extra declaration before control objects. *)
  let from_local opgraph opstmt =
    let var_name, var_ty, val_exp = unpack_local opstmt in
    let var_name = Cid.Id var_name in
    print_endline ("FROM_LOCAL: "^(Printing.stmt_to_string opstmt));
    (* make the ALU that does the computation. *)
    let alu_name, alu_obj = TofinoAlu.from_assign var_name val_exp opstmt in
    (* wrap it in a table that calls the ALU and points to the next table. *)
    let assign_objs = wrap_alu_in_call_table opgraph opstmt alu_name alu_obj in
    (* add an object to declare the new local *)
    (* this needs to be updated for event variables. *)
    let meta_obj = IS.new_globalmeta var_name (width_from_ty var_ty) in
    meta_obj :: assign_objs
  ;;

  (* unit statements can only be calls, so handle them separately. *)
  let from_unit opgraph opstmt =
    let val_exp = unpack_unit opstmt in
    (* generate alu *)
    let alu_name, alu_obj = TofinoAlu.from_unit val_exp opstmt in
    (* wrap in a table *)
    wrap_alu_in_call_table opgraph opstmt alu_name alu_obj
  ;;

  let from_gen opgraph opstmt =
    !dprint_endline "generate handler.";
    let base_name = uname_of_stmt opstmt in
    (* generate the ALU *)
    let args = unpack_generate opstmt in
    let fcn_name = TofinoConstants.event_generate_cid in
    let generated_alus =
      ctx_call_codegen
        fcn_name
        { basename = Some base_name
        ; retname = None
        ; (* generate doesn't return*)
          args = [args]
        }
    in
    (* the generate statement can translate to a no-op if it is generating from
    an event variable. *)
    match CL.length generated_alus.names with
    | 0 -> []
    | _ ->
      let alu_name = CL.hd generated_alus.names in
      let alu_obj = CL.hd generated_alus.objs in
      (* wrap in control flow object *)
      wrap_alu_in_call_table opgraph opstmt alu_name alu_obj
  ;;

  (* generators for statements that are _JUST_ control flow operations. *)

  let print_eop exp = 
    match exp.e with 
    | EOp(op, args) -> "[op: "
      ^(Printing.op_to_string op)
      ^"] args: "
      ^(CL.map Printing.exp_to_string args |> String.concat ", ")
    | _ -> "[not an eop]"
  ;;

  (* get the mapping from key (which appears in the atom) to value *)
  let field_pat_of_atom_exp exp =
    match exp.e with
    | EOp (Eq, [evar; eval]) | EOp (Neq, [evar; eval]) ->
      (* name_from_exp evar, IS.Exact (Integer.of_int (int_from_exp eval)) *)
      name_from_exp evar, IS.Exact (Integer.of_int (int_from_const_exp eval))
    | _ -> error "unexpected form of expression to convert into a pattern. "
  ;;

  (* get a pattern that encodes exact matches for the keys in the atoms,
  and wildcard for the other keys. *)
  let pattern_of_atoms keys atoms : IS.pattern =
    let pattern = CL.map field_pat_of_atom_exp atoms in
    let condition_of_key key =
      match CL.assoc_opt key pattern with
      | Some cond -> cond
      | None -> Any
    in
    let key_conditions = CL.map condition_of_key keys in
    CL.combine keys key_conditions
  ;;

  (* get an entry that applies stmt when all the atoms over keys eval to true 
  if there are no atoms, that means we are generating an entry where there 
  are no constraints on any of the variables. *)
  let entry_of_atoms opgraph stmt keys atoms : IS.rule * IS.decl =
    let _ = opgraph in 
    let pattern = pattern_of_atoms keys atoms in
    match stmt.s with
    | SNoop ->
      (* if the action in this branch is a noop, there is no successor.*)
      (* actually, there is. It is the successor of the NOOP in the graph. *)
      (* this happens when you have an empty else branch *)
      let acn_name = Cid.fresh ["if_action"] in
      (* here, the successor is the successor for the table _after_ the 
      noop, because the noop doesn't create a table. *)
      let succ_tbls = successortbls_of_stmt opgraph stmt in  
      ( match succ_tbls with 
        | [] -> print_endline ("NO SUCCESSOR TABLE FOR NOOP: "^(stmt.sspan.fname));
        | _ -> ()
      );
      (* let succ_tbl = tblname_of_stmt stmt in *)
      let acn = IS.new_action acn_name [] succ_tbls in
      let rule = IS.new_rule (Cid.fresh ["if_rule"]) pattern acn_name in
      rule, acn
    | _ ->
      let acn_name = Cid.fresh ["if_action"] in
      (* here, the successor is the table for the first statement of the 
      branch, which we haven't created yet. *)
      let succ_tbl = tblname_of_stmt stmt in
      let acn = IS.new_action acn_name [] [succ_tbl] in
      let rule = IS.new_rule (Cid.fresh ["if_rule"]) pattern acn_name in
      rule, acn
  ;;

  let dprint_entry_rule (r, _) = 
    (!dprint_endline) (P4tPrint.PrimitiveString.str_of_rule r)
  ;;

  let entries_of_conj_exp opgraph true_stmt false_stmt keys conj_exp
      : (IS.rule * IS.decl) list
    =
    (* generate one entry for every negative atom in the conj_exp,
    then one entry for all the positive atoms in the conj exp *)    
    let atom_exps = flatten_conjunction conj_exp in
    !dprint_endline ("[entries_of_conj_exp] eop: "^(print_eop (CL.hd atom_exps)));
    let neg_atoms = CL.filter (filter_eop_kind Neq) atom_exps in
    !dprint_endline ("number of negative test atoms: "^(string_of_int (CL.length neg_atoms)));
    let pos_atoms = CL.filter (filter_eop_kind Eq) atom_exps in
    !dprint_endline ("number of positive test atoms: "^(string_of_int (CL.length pos_atoms)));
    (* entries to perform all negative tests enforce negations *)
    let neg_entries =
      CL.map (fun a -> entry_of_atoms opgraph false_stmt keys [a]) neg_atoms
    in
    !dprint_endline "neg_entries rules: ";
    CL.iter dprint_entry_rule neg_entries;
    !dprint_endline "----";
    (* entries to test all the variables that are true. *)
    (* there is only one positive entry for all positive atoms. 
    If there are no positive atoms, this part does not generate a 
    positive entry. *)    
    let pos_entries = match pos_atoms with 
      | [] -> [entry_of_atoms opgraph true_stmt keys []] (* if there are no equality conditions, 
      then missing the neg_entries means that the expr evals to true. *)
      | _ -> [entry_of_atoms opgraph true_stmt keys pos_atoms; entry_of_atoms opgraph false_stmt keys []]
      (* if there are equality conditions, then they all must be met to eval to true. 
      Otherwise, fall through to a false eval. *)
    in
    !dprint_endline "pos_entry rules: ";
    CL.iter dprint_entry_rule pos_entries;
    neg_entries @ pos_entries
  ;;

  (* The algorithm for converting a conjunction 
    of atoms of the form (x < == | != > y) into tcam rules: 
      - example: (a == C1 && b != C2 && c != C3 && d == C4)
      1. generate one inequality-test rule for every negative condition 
        (a : _; b : C2; c : _;  d = _;) --> do_false
        (a : _; b : _;  c : C3; d = _;) --> do_false
      2. generate one equality test rule for _all_ the positive conditions
        - This rules is tested after the inequality tests, so 
          we know that all the inequality atoms evaluate to "true".
        (a : C1; b : _; c : _;  d = C4;) --> do_true
      3. generate a final negative condition rule, which represents the 
         case where a positive condition fails.
        (a : _; b : _; c : _;  d = _;) --> do_false
        * note that if there are no positive conditions, (3.) should 
        not get generated. 
    To convert a disjunction of conjunctions to rules, just run 
    the above algorithm for every conjunction. *)

  let from_if opgraph opstmt =
    let _, _ = opgraph, opstmt in
    (* TODO: either implement if or translate to match in an earlier pass. *)
    let exp, true_stmt, false_stmt, _ = unpack_if opstmt in
    (* true_stmt and false_stmt may be sequences, we need the op statements *)
    let true_stmt, false_stmt = fst_op_of_branch true_stmt, fst_op_of_branch false_stmt in 
    (* get the key fields *)
    let keys = dynamic_vars_in_exp exp in 
    CL.iter (fun k -> print_endline (Cid.to_string k)) keys;
    (* at this point, the expression is a disjunction of conjunctions. *)
    (* get a list of the conjunctions *)
    let conj_exps = flatten_disjunction exp in
    (* generate the (rule, action) entries for each conjunction.  *)
    let entries =
      CL.map (entries_of_conj_exp opgraph true_stmt false_stmt keys) conj_exps
      |> CL.flatten
    in
    let rules, acns = CL.split entries in
    let tblname = tblname_of_stmt opstmt in
    let tbl = IS.new_table tblname rules in
    let new_objs = acns @ [tbl] in
    !dprint_endline "-----[from_if]-----";
    !dprint_endline ("entire statement:");
    !dprint_endline (Pr.stmt_to_string opstmt);
    !dprint_endline ("true_stmt:\n"^(Printing.stmt_to_string true_stmt));
    !dprint_endline ("false_stmt:\n"^(Printing.stmt_to_string false_stmt));
    !dprint_endline ("new objects:");
    log_objs opstmt new_objs;
    !dprint_endline "generated table: ";
    !dprint_endline (IS.show_decl tbl);
    new_objs
  ;;

  let from_match opgraph opstmt =
    let _ = opgraph in
    let keys, branches = unpack_match opstmt in
    let key_names = CL.map name_from_exp keys in
    (* make a rule and action for each entry. *)
    let tbl_entries = CL.map (tblentry_from_branch key_names) branches in
    (* may need to make a default rule too, if the last one is not PWild,
    and link it to the first statement after the match block ends.
    (I think this is why we passed in opgraph) *)
    let rules, acns = CL.split tbl_entries in
    let tblname = tblname_of_stmt opstmt in
    (* make the table with the rules *)
    let tbl = IS.new_table tblname rules in
    (* return table and actions *)
    !dprint_endline "handling match:";
    !dprint_endline (Pr.stmt_to_string opstmt);
    !dprint_endline "generated table: ";
    !dprint_endline (IS.show_decl tbl);
    let new_objs = acns @ [tbl] in
    log_objs opstmt new_objs;
    new_objs
  ;;

  let from_opstmt opgraph opstmt objs =
    !dprint_endline "-----[from_opstmt] ------";
    !dprint_endline ("creating table: "^ Cid.to_string (tblname_of_stmt opstmt));
    !dprint_endline (Printing.stmt_to_string opstmt);
    print_endline (Printing.stmt_to_string opstmt);
    !dprint_endline ("op statement: " ^ OpGraph.print_op_stmt opstmt);
    !dprint_endline "-----------";
    match opstmt.s with
    | SAssign _ -> objs @ from_assign opgraph opstmt
    | SUnit _ ->
      objs @ from_unit opgraph opstmt
      (* a local is a variable declaration followed by an assign. *)
    | SLocal _ -> objs @ from_local opgraph opstmt
    | SGen _ -> objs @ from_gen opgraph opstmt
    | SIf _ -> objs @ from_if opgraph opstmt
    | SMatch _ -> objs @ from_match opgraph opstmt
    | SSeq _ -> error "Seq is not an opstatement."
    | _ -> objs
  ;;

  (* main function. make a tofino control graph from an op statement graph. *)
  let from_opstmt_graph opgraph =
    StGraph.fold_vertex (from_opstmt opgraph) opgraph []
  ;;
end

(**** logging ir and graphs to files ****)
module DotConfig = struct
  include DagSyntax.G (* use the graph module from above *)

  let graph_attributes _ = []
  let edge_attributes (_, _, _) = []
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes _ = [`Shape `Box]
  let vertex_name v = P4tPrint.str_of_private_oid v
  let default_vertex_attributes _ = []
end

module Dot = Graph.Graphviz.Dot (DotConfig)

let dump_dag g fn =
  (* printf "dumping dag to: %s\n" fn; *)
  Dot.output_graph (open_out_bin fn) g
;;

(**** logging ir and graphs to files ****)

let dump_dag_dbg hdl_id tofino_objs =
  dump_dag
    (DagSyntax.graph_of_declsMap (IS.dict_of_decls tofino_objs))
    (!BackendLogging.graphLogDir ^ "/" ^ (Id.to_string hdl_id) ^ ".dag.dot")
;;

(* convert a handler's statement graph into a tofino control graph *)
type tofino_control_g =
  { hid : id
  ; cid_decls : (Cid.t * IS.decl) list
  ; root_tid : Cid.t
  }

let dpahandler_from_handler hog_rec : tofino_control_g =
  (* 1: map each operation statement to an object *)
  let tofino_objs = TofinoControl.from_opstmt_graph hog_rec.h_opgraph in
  dump_dag_dbg hog_rec.h_name tofino_objs;
  { hid = hog_rec.h_name
  ; cid_decls = IS.dict_of_decls tofino_objs
  ; root_tid = tblname_of_stmt hog_rec.h_root
  }
;;

(* merge the handler definitions together into a complete
program for the backend syntax. *)
let merge_handler_defs (hdl_defs : tofino_control_g list) : IS.instrProg =
  (* merge all the cid_decls together. *)
  let cid_decls =
    CL.map (fun (r : tofino_control_g) -> r.cid_decls) hdl_defs |> CL.flatten
  in
  (* make a rule and action for each handler. *)
  let root_rule_acn_from_hdl hdl_def =
    let event_num = ctx_find_event_iid (Cid.id hdl_def.hid) in 
    let acn_name = Cid.id hdl_def.hid in
    let acn = IS.new_action acn_name [] [hdl_def.root_tid] in
    let pattern : IS.pattern =
      [handle_selector_name, IS.Exact (Integer.of_int event_num)]
    in
    let rule = IS.new_rule (Cid.fresh ["rule"]) pattern acn_name in
    rule, acn
  in
  let rules_acns = CL.map root_rule_acn_from_hdl hdl_defs in
  let rules, acns = CL.split rules_acns in
  (* make the table from the rules and actions *)
  let root_tblname = Consts.branchTbl in
  let tbl = IS.new_table root_tblname rules in
  (* add the root table and actions to the object dict. *)
  let cid_decls = IS.dict_of_decls (tbl :: acns) @ cid_decls in
  (* return the complete instruction prog, with the cid_decls
  merged.  *)
  dump_dag_dbg (Id.create "MERGED_HANDLER") (snd (CL.split cid_decls));
  { root_tid = root_tblname; instr_dict = cid_decls }
;;

let start_logging () = DBG.start_mlog __FILE__ outc dprint_endline
