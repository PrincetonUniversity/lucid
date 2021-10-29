(* translate individual operation statements from the
source syntax to tofino compute objects *)
open Syntax
module Pr = Printing
module IS = LLSyntax
open InterpHelpers
open LLContext
open LLConstants
open OGSyntax
open LogIr

(* logging *)
module DBG = BackendLogging

let outc = ref None
let dprint_endline = ref DBG.no_printf

(* 5/18 added these quickly, they belong... somewhere else... *)
(* get the variables that are not consts. Used in keys. *)
let dynamic_vars_in_exp (ex : exp) : Cid.t list =
  vars_in_exp ex |> CL.filter (fun v -> not (ctx_var_is_const v))
;;

(* get an integer from an expression. If the expression is a const, get the computed value. *)
let int_from_const_exp (ex : exp) =
  print_endline ("[int_from_exp]: " ^ Printing.exp_to_string ex);
  match ex.e with
  | EVal { v = VInt zint; _ } -> Integer.to_int zint
  | EInt (z, _) ->
    Z.to_int z (* Integer.create_z ~value:z ~size:(extract_size sz)  *)
  | EVar cid -> LLContext.ctx_int_of_const cid
  | _ -> trans_err "could not evaluate expression to an int" ex
;;

module TofinoStructs = struct
  (**** translate structure names ****)
  let defname_from_evname evname = "e_" ^ evname
  let defname_from_evid evid = Cid.id (fst evid |> defname_from_evname, snd evid)

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

  (* fully qualified struct instance name from event id and type*)
  let full_struct_from_ev evid evsort =
    let prefix = match evsort with 
      | EEntry _ | EExit -> (Id.create md_instance_prefix)
      | EBackground -> (Id.create hdr_instance_prefix)
    in
    Cid.compound prefix (in_struct_from_ev evid)
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
    let struct_instance = ctx_find_event_struct hdl_id in 
    let qual_fieldnames =
      ctx_find_event_fields hdl_id
      |> CL.map (fun field_cid -> Cid.concat struct_instance field_cid)
    in
    qual_fieldnames
  ;;

  let qual_out_fieldnames_of_event = qual_in_fieldnames_of_event
  ;;
end

(**** translate operation statements to DPA objects ****)

(* given that you are in handler hdl_id,
translate identifiers into IR local variable
identifiers. These functions are mainly here
for handler parameter ids, which must change
to event struct field ids. *)
let mid_from_id hdl_id id =
  let id_map = ctx_get_hdl_param_map hdl_id in
  match CL.assoc_opt id id_map with
  | Some qualified_cid -> qualified_cid
  | None -> Cid.id id
;;

let mid_from_cid hdl_id cid =
  match cid with
  | Id id -> mid_from_id hdl_id id
  | _ -> cid
;;

(* atomic nodes of the syntax tree *)
let const_from_int i = Integer.of_int i
let const_from_z (z : Z.t) = Integer.of_int (Z.to_int z)

let binOp_from_op op =
  match op with
  | Plus -> IS.Add
  | Sub -> IS.Sub
  | SatSub -> IS.SatSub
  | RShift -> IS.RShift
  | LShift -> IS.LShift
  | BitAnd -> IS.BAnd
  | BitOr -> IS.BOr
  | _ -> error "[binOp_from_op] unsupported op"
;;

(*** immediates ***)
let zint_from_evalue (immediate_exp : exp) =
  match immediate_exp.e with
  | EVal value ->
    (match value.v with
    | VInt zint -> zint
    | _ ->
      let expstr = Printing.exp_to_string immediate_exp in
      error
        ("[zint_from_evalue] got value that cannot be translated directly into \
          IR: "
        ^ expstr))
  | EInt (z, _) -> Integer.of_int (Z.to_int z)
  | _ ->
    error
      "[int_from_immediate] got value that cannot be translated directly into \
       IR."
;;

let oper_from_immediate hdl_id (immediate_exp : exp) =
  match immediate_exp.e with
  | EVal _ | EInt _ -> IS.Const (zint_from_evalue immediate_exp)
  | EVar cid -> Meta (mid_from_cid hdl_id cid)
  | _ ->
    let dstr =
      "[oper_from_immediate] not a backend-recognized immediate: "
      ^ Printing.exp_to_string immediate_exp
    in
    error dstr
;;

let oper_from_size (sz : size) : IS.oper =
  match extract_size_opt sz with
  | Some sz_int -> IS.Const (const_from_int sz_int)
  | None -> error "[oper_from_size] -- could not extract int from size"
;;

let soper_from_immediate
    hdl_id
    (memcell_name : Cid.t option)
    (immediate_exp : exp)
  =
  match immediate_exp.e with
  | EVal _ | EInt _ -> IS.Const (zint_from_evalue immediate_exp)
  | EVar n ->
    (match memcell_name with
    | Some memcell_name ->
      (match Cid.equals memcell_name n with
      | true -> IS.RegVar Lo
      | false -> IS.Meta (mid_from_cid hdl_id n))
    | None -> IS.Meta (mid_from_cid hdl_id n))
  | _ -> error "[soper_from_immediate] expression is not an immediate"
;;

let eoper_from_immediate hdl_id immediate_exp =
  IS.Oper (oper_from_immediate hdl_id immediate_exp)
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
  let from_assign hdl_id outvar_id val_exp opstmt =
    let outvar_mid = mid_from_id hdl_id outvar_id in
    let base_name = uname_of_stmt opstmt in
    let alu_name = aluname_of_stmt opstmt in
    let alu_name, alu_obj =
      match val_exp.e with
      | EVal _ | EInt _ | EVar _ ->
        ( alu_name
        , IS.new_dsingleinstr
            alu_name
            outvar_mid
            (eoper_from_immediate hdl_id val_exp) )
      (* binary operations are alus as an action *)
      | EOp (op, [a; b]) ->
        let a = oper_from_immediate hdl_id a in
        let b = oper_from_immediate hdl_id b in
        let op = binOp_from_op op in
        alu_name, IS.new_dsingleinstr alu_name outvar_mid (IS.new_ebinop op a b)
      | EOp (Cast sz, cast_args) ->
        let src = oper_from_immediate hdl_id (CL.hd cast_args) in
        let width = oper_from_size sz in
        ( alu_name
        , IS.new_dsingleinstr alu_name outvar_mid (IS.new_ebinop Cast src width)
        )
      | EOp (_, _) ->
        let dstr = Printing.statement_to_string opstmt in
        error
          ("operations must all be binary for translation to tofino. \
            statement: "
          ^ dstr)
      | ECall (fcn_id, args) ->
        ((* a call could either be a call, or an event declaration. *)
        match raw_ty_of_exp val_exp with
        | TEvent _ ->
          let call_result =
            ctx_call_codegen
              LLConstants.event_generate_cid
              { hdl_id = Some hdl_id
              ; basename = Some base_name
              ; retname = Some outvar_mid
              ; args = [val_exp]
              }
          in
          CL.hd call_result.names, CL.hd call_result.objs
        | _ ->
          t_info "[from_assign: ECall]";
          let call_result =
            ctx_call_codegen
              fcn_id
              { hdl_id = Some hdl_id
              ; basename = Some base_name
              ; retname = Some outvar_mid
              ; args
              }
          in
          CL.hd call_result.names, CL.hd call_result.objs)
      | EHash (size, exps) ->
        (* let width = SyntaxUtils.extract_size size in *)
        (* hack for temporary cast *)
        let width = SyntaxUtils.extract_size_default size 32 in
        let epoly = CL.hd exps in
        let poly = int_from_const_exp epoly in
        (* poly can be an integer or a const *)
        (* let poly = Integer.to_int (zint_from_evalue (CL.hd exps)) in  *)
        let args = CL.map (oper_from_immediate hdl_id) (CL.tl exps) in
        alu_name, IS.new_hasher alu_name width poly outvar_mid args
      | EProj _
      | ERecord _
      | EWith _
      | EComp _
      | EIndex _
      | EVector _
      | ETuple _
      | ESizeCast _
      | EStmt _ -> error "Should be eliminated long before this point."
    in
    !dprint_endline
      (sprintf "[from_assign] created alu: " ^ Printing.cid_to_string alu_name);
    alu_name, alu_obj
  ;;

  let from_unit hdl_id val_exp opstmt =
    t_info "[from_unit]";
    let base_name = uname_of_stmt opstmt in
    let alu_name, alu_obj =
      match val_exp.e with
      | ECall (name, args) ->
        let call_result =
          ctx_call_codegen
            name
            { hdl_id = Some hdl_id
            ; basename = Some base_name
            ; retname = None
            ; args
            }
        in
        CL.hd call_result.names, CL.hd call_result.objs
      | _ ->
        let dstr = Printing.exp_to_string val_exp in
        error
          ("unsupported expression in unit statement. Unit statements should \
            only have calls, but this expression is: "
          ^ dstr)
    in
    alu_name, alu_obj
  ;;

  (* TODO: add delay and location meta fields, put at the beginning, set to 0 initially *)
  let event_init_meta_instrs ev_id =
    let struct_id = ctx_find_event_struct_cid ev_id in 
    (* set the event id field in the packet header *)
    let init_event_id_instr =
      IS.new_iassign_int
        (Cid.concat struct_id event_id_field)
        (ctx_find_event_iid ev_id)
    in
    let init_event_mc_instr =
      IS.new_iassign_int (Cid.concat struct_id event_mc_field) 0
    in
    let init_event_loc_instr =
      IS.new_iassign_int (Cid.concat struct_id event_loc_field) 0
    in
    let init_event_delay_instr =
      IS.new_iassign_int (Cid.concat struct_id event_delay_field) 0
    in
    (* let event_delay_field = Cid.concat struct_id event_ *)
    [ init_event_id_instr
    ; init_event_mc_instr
    ; init_event_loc_instr
    ; init_event_delay_instr ]
  ;;

  let set_next_event_instr ev_id =
    print_endline ("[set_next_event_instr] event id: " ^ Cid.to_string ev_id);
    let event_iid = ctx_find_event_iid ev_id in
    (* set the event id field in lucid metadata *)
    let nextevent_id_field =
      Cid.create [md_instance_prefix; dpt_meta_str; next_event_str]
    in
    IS.IAssign (nextevent_id_field, IS.new_expr_of_int event_iid)
  ;;

  let set_exit_event_instr ev_id =
    let event_iid = ctx_find_event_iid ev_id in
    (* set the event id field in lucid metadata *)
    let exitevent_id_field =
      Cid.create [md_instance_prefix; dpt_meta_str; exit_event_str]
    in
    IS.IAssign (exitevent_id_field, IS.new_expr_of_int event_iid)
  ;;

  (* increment a variable that counts 
     the number of background events generated. *)
  let increment_event_counter () = 
    let ev_ct_cid = Cid.create [md_instance_prefix; dpt_meta_str; events_count_str] in 
    IS.new_iassign 
      ev_ct_cid 
      (IS.new_ebinop 
        IS.Add 
        (IS.new_oper_of_meta ev_ct_cid)
        (IS.new_oper_of_int 1)
      )
  ;;
  (* generate an alu instruction from the instantiation of an event *)
  let from_event_instantiation hdl_id alu_basename ev_id ev_args =
    !dprint_endline ("[generate_event] event id: " ^ Cid.to_string ev_id);
    !dprint_endline "[generate_event] event args: ";
    let iter_f ev_arg = !dprint_endline (Printing.exp_to_string ev_arg) in
    CL.iter iter_f ev_args;
    (* get a list of qualified out struct field parameters *)
    (* generate alu instructions of the form: field_param := ev_arg *)
    (* since the out fields are written, the variable references must be
       lmids, else dataflow analysis will fail. *)
    let to_lmid (mid : Cid.t) : IS.lmid = mid in
    let out_struct_fields =
      TofinoStructs.qual_out_fieldnames_of_event (Cid.to_id ev_id)
      |> CL.map to_lmid
    in
    let alu_rhs_exps = CL.map (eoper_from_immediate hdl_id) ev_args in
    let to_ass_f (lhs, rhs) = IS.IAssign (lhs, rhs) in
    let (ivec : IS.instrVec) =
      CL.map to_ass_f (CL.combine out_struct_fields alu_rhs_exps)
    in
    let struct_id = ctx_find_event_struct_cid ev_id in
    (* add instructions to set metadata fields, e.g., event name *)
    let event_meta_instrs = event_init_meta_instrs ev_id in
    (* if the event is a background event: set the header field to valid, 
        set the next event instruction, 
        and increment the events generated count. *)
    let final_instruction = 
      match (ctx_find_eventrec ev_id).event_sort with
      | EBackground ->
        [ IS.IValidate struct_id
        ; set_next_event_instr ev_id 
        ; increment_event_counter ()

        ]
      | _ -> [set_exit_event_instr ev_id]
    in
    let ivec = event_meta_instrs @ ivec @ final_instruction in
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
    !dprint_endline "[wrap_alu_in_call_table] successor tables: ";
    CL.iter
      (fun t -> !dprint_endline ("[wrap_alu_in_call_table] " ^ Cid.to_string t))
      succ_tbls;
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
  let from_assign hdl_id opgraph opstmt =
    let outvar_id, val_exp = unpack_assign opstmt in
    (* make the ALU that does the computation. *)
    let alu_name, alu_obj =
      TofinoAlu.from_assign hdl_id outvar_id val_exp opstmt
    in
    (* wrap it in a table that calls the ALU and points to the next table. *)
    wrap_alu_in_call_table opgraph opstmt alu_name alu_obj
  ;;

  (* almost the same as assign, but there's also an extra declaration before control objects. *)
  let from_local hdl_id opgraph opstmt =
    let outvar_id, var_ty, val_exp = unpack_local opstmt in
    (* caution: must use mid_from_id in from_assign too. *)
    let outvar_mid = mid_from_id hdl_id outvar_id in
    print_endline ("FROM_LOCAL: " ^ Printing.stmt_to_string opstmt);
    (* make the ALU that does the computation. *)
    let alu_name, alu_obj =
      TofinoAlu.from_assign hdl_id outvar_id val_exp opstmt
    in
    (* wrap it in a table that calls the ALU and points to the next table. *)
    let assign_objs = wrap_alu_in_call_table opgraph opstmt alu_name alu_obj in
    (* add an object to declare the new local *)
    print_endline
      ("trying to find width of type: " ^ Printing.ty_to_string var_ty);
    (* let width = width_from_ty var_ty in  *)
    (* HACK (7/6/21) -- should figure out what is wrong here. Probably related to consts. *)
    let width =
      match width_from_ty_opt var_ty with
      | Some width -> width
      | None -> 32
    in
    let meta_obj = IS.new_globalmeta outvar_mid width in
    meta_obj :: assign_objs
  ;;

  (* unit statements can only be calls, so handle them separately. *)
  let from_unit hdl_id opgraph opstmt =
    let val_exp = unpack_unit opstmt in
    (* generate alu *)
    let alu_name, alu_obj = TofinoAlu.from_unit hdl_id val_exp opstmt in
    (* wrap in a table *)
    wrap_alu_in_call_table opgraph opstmt alu_name alu_obj
  ;;

  let from_gen hdl_id opgraph opstmt =
    !dprint_endline "generate handler.";
    let base_name = uname_of_stmt opstmt in
    (* generate the ALU *)
    let args = unpack_generate opstmt in
    let fcn_name = LLConstants.event_generate_cid in
    let generated_alus =
      ctx_call_codegen
        fcn_name
        { hdl_id = Some hdl_id
        ; basename = Some base_name
        ; retname = None
        ; (* generate doesn't return *)
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

  (* if to table conversion (new 8/1/21) *)

  (* a rule in a binary table evaluates to either true or false. *)
  type binary_rule =
    | BTrue of IS.pattern
    | BFalse of IS.pattern

  type binary_rules = binary_rule list

  type table_actions =
    { ta_true : IS.decl
    ; ta_false : IS.decl
    }

  let str_of_binary_rule br =
    match br with
    | BTrue pat -> P4tPrint.PrimitiveString.str_of_pat pat ^ "-->TRUE"
    | BFalse pat -> P4tPrint.PrimitiveString.str_of_pat pat ^ "-->FALSE"
  ;;

  let str_of_binary_rules brs =
    String.concat "\n" (CL.map str_of_binary_rule brs)
  ;;

  (* a binary table is a list of binary rules and true / false actions *)
  (*   type binary_table = binary_rules * table_actions
 *)
  (* get the mapping from key (which appears in the atom) to value *)
  let field_pat_of_atom_exp hdl_id exp =
    match exp.e with
    | EOp (Eq, [evar; eval]) | EOp (Neq, [evar; eval]) ->
      ( name_from_exp evar |> mid_from_cid hdl_id
      , IS.Exact (Integer.of_int (int_from_const_exp eval)) )
    | _ -> error "unexpected form of expression to convert into a pattern. "
  ;;

  (* get a pattern that encodes exact matches for the keys in the atoms,
  and wildcard for the other keys. *)
  let pattern_of_atoms hdl_id keys atoms : IS.pattern =
    let pattern = CL.map (field_pat_of_atom_exp hdl_id) atoms in
    let condition_of_key key =
      match CL.assoc_opt key pattern with
      | Some cond -> cond
      | None -> Any
    in
    let key_conditions = CL.map condition_of_key keys in
    CL.combine keys key_conditions
  ;;

  let dprint_entry_rule (r, _) =
    !dprint_endline (P4tPrint.PrimitiveString.str_of_rule r)
  ;;

  (* for booleans, this data structure should have true and false as types of rules. *)

  (* generate the actions that point to true and false branches *)
  let table_actions_of_ifnode opgraph opstmt : table_actions =
    (* get the true and false opstatements. *)
    let _, true_stmt, false_stmt, _ = unpack_if opstmt in
    let true_stmt, false_stmt =
      fst_op_of_branch true_stmt, fst_op_of_branch false_stmt
    in
    (* convert each statement into an action *)
    let gen_case_action opgraph acn_name stmt =
      match stmt.s with
      | SNoop ->
        let succ_tbls = successortbls_of_stmt opgraph stmt in
        (match succ_tbls with
        | [] ->
          print_endline ("NO SUCCESSOR TABLE FOR NOOP: " ^ stmt.sspan.fname)
        | _ -> ());
        (* let succ_tbl = tblname_of_stmt stmt in *)
        IS.new_action acn_name [] succ_tbls
      | _ ->
        let succ_tbl = tblname_of_stmt stmt in
        IS.new_action acn_name [] [succ_tbl]
    in
    let true_name = Cid.fresh ["true"] in
    let false_name = Cid.fresh ["false"] in
    let true_acn = gen_case_action opgraph true_name true_stmt in
    let false_acn = gen_case_action opgraph false_name false_stmt in
    { ta_true = true_acn; ta_false = false_acn }
  ;;

  (* extract the toplevel expressions *)
  let get_toplevel_exps = flatten_disjunction
  let get_atom_exps = flatten_conjunction

  let get_keys hdl_id exp =
    let keys = dynamic_vars_in_exp exp |> CL.map (mid_from_cid hdl_id) in
    print_endline "KEYS: ";
    CL.iter (fun k -> print_endline (Cid.to_string k)) keys;
    print_endline "-----";
    keys
  ;;

  let binary_rules_from_toplevel_exp hdl_id keys toplevel_exp : binary_rules =
    (* Given a toplevel expression, which is a conjunction of the form:
        T1 && T2 && ... && TN,
        where T = [T1, ..., TN] are binary equality or inequality tests,
        this function generates a list of binary rules that
        evaluate to true iff (T1 && T2 && ... && TN) is true.
        Assume T is split into two non-overlapping subsets:
          - Te -- terms that are equal tests
          - Tn -- terms that are not equal tests
        The rules generated are (in order):
          - one rule that evaluates to false for each term in Te.
          - one rule that evaluates to true for _all_ the terms in Tn.
          - a final rule that evaluates to false.

      (x != 0) ->
      x : 0 -> false
      x : _ -> true

      (y == 0 || x != 0) ->
      y : 0, x : _ --> true
      y : _, x : _ --> false

      y : _, x : 0 --> false
      y : _, x : _ --> true

      y : 0, x : _ -->

      y : _, x : 0 -> false
      y : 0, x : _ -> true
      y : _, x : _ -> false


    *)
    let atom_exps = get_atom_exps toplevel_exp in
    let eqs = CL.filter (filter_eop_kind Eq) atom_exps in
    let neqs = CL.filter (filter_eop_kind Neq) atom_exps in
    (* neq_rules -- one rule that evaluates to false for
       every atom that does a not equals test *)
    let neq_rules =
      CL.map
        (fun neq -> BFalse (pattern_of_atoms hdl_id keys (neq :: eqs)))
        neqs
    in
    (* eq_rules -- one rule for all the conditions that must hold for
       the term to evaluate to true, after all the inequalities
       have been tested *)
    let eq_rules = [BTrue (pattern_of_atoms hdl_id keys eqs)] in
    neq_rules @ eq_rules
  ;;

  let remove_shadow true_rule pred =
    (* Poke a hole in pred so that true_rule is not shadowed. *)
    match true_rule, pred with
    | BTrue _, BTrue _ -> [pred]
    | BTrue tpat, BFalse ppat ->
      (match MergeUtils.intersect_patterns tpat ppat with
      | None -> [pred] (* no shadow *)
      | Some ipat ->
        (* ipat is the shadow *)
        [BTrue ipat; pred])
    | BFalse _, _ -> error "[remove_shadow] it is okay to shadow a false rule."
  ;;

  let rec remove_shadows rule pred_rules =
    (* If rule is true, poke holes in any false rules in pred_rules, so that
       rule matches. *)
    match rule with
    | BTrue _ ->
      (match pred_rules with
      | [] -> []
      | [pred_rule] -> remove_shadow rule pred_rule
      | pred_rule :: pred_rules ->
        remove_shadow rule pred_rule @ remove_shadows rule pred_rules)
    | BFalse _ -> pred_rules
  ;;

  let remove_shadow_outer rule_lists =
    let remove_shadow_inner pred_lists rule_list =
      (* find the positive rule in rule list. *)
      let pos_rules =
        CL.filter
          (fun r ->
            match r with
            | BTrue _ -> true
            | _ -> false)
          rule_list
      in
      let pos_rule = CL.hd pos_rules in
      (* poke holes in all the predecessor lists *)
      let deshadowed_pred_lists = CL.map (remove_shadows pos_rule) pred_lists in
      (* return the list up to here. *)
      deshadowed_pred_lists @ [rule_list]
    in
    CL.fold_left remove_shadow_inner [] rule_lists
  ;;

  let is_rule_matchable pred_rules rule =
    let pat_of_rule r =
      match r with
      | BTrue pat -> pat
      | BFalse pat -> pat
    in
    let pred_pats = CL.map pat_of_rule pred_rules in
    let pat = pat_of_rule rule in
    RuleSolve.is_pat_still_feasible pat pred_pats
  ;;

  (* delete any rules that cannot ever be matched due to
     shadows from combined predecessors. *)
  let delete_unmatchable_rules rules =
    let fold_if_reachable preds rule =
      match is_rule_matchable preds rule with
      | true -> preds @ [rule]
      | false ->
        !dprint_endline ("unreachable rule! " ^ str_of_binary_rule rule);
        !dprint_endline ("preds: " ^ str_of_binary_rules preds);
        preds
    in
    CL.fold_left fold_if_reachable [] rules
  ;;

  let new_merge hdl_id keys toplevel_rule_lists =
    (* for each true rule in a rule set, make sure that it is not shadowed by
       and rules in previous rule lists. *)
    let deshadowed_rule_lists = remove_shadow_outer toplevel_rule_lists in
    (* flatten the list of rules *)
    let rules = CL.flatten deshadowed_rule_lists in
    (* add the default rule *)
    let default_false = BFalse (pattern_of_atoms hdl_id keys []) in
    let full_rules = rules @ [default_false] in
    (* remove any unreachable rules. *)
    delete_unmatchable_rules full_rules
  ;;

  (* generate a real rule from a binary rule *)
  let generate_rule table_actions binary_rule =
    match binary_rule with
    | BTrue pat ->
      IS.new_rule
        (Cid.fresh ["btrue"])
        pat
        (IS.id_of_decl table_actions.ta_true)
    | BFalse pat ->
      IS.new_rule
        (Cid.fresh ["bfalse"])
        pat
        (IS.id_of_decl table_actions.ta_false)
  ;;

  let from_if hdl_id opgraph opstmt =
    print_endline "FROM IF STARTING.";
    !dprint_endline "-----[from_if]-----";
    (* extract the details of the statement *)
    let exp, _, _, _ = unpack_if opstmt in
    (* get the toplevel expressions *)
    let toplevel_exps = get_toplevel_exps exp in
    (* get the keys *)
    let key_vars = get_keys hdl_id exp in
    (* generate a list of binary rules for each toplevel expression *)
    let toplevel_rule_lists =
      CL.map (binary_rules_from_toplevel_exp hdl_id key_vars) toplevel_exps
    in
    (* merge the rule lists together, being careful about shadows and reachability *)
    let rule_list = new_merge hdl_id key_vars toplevel_rule_lists in
    (* generate the true and false actions *)
    let table_actions = table_actions_of_ifnode opgraph opstmt in
    (* generate real rules from binary rules *)
    let rules = CL.map (generate_rule table_actions) rule_list in
    (* wrap everything in a table *)
    (* wrap it in a table *)
    let tblname = tblname_of_stmt opstmt in
    let table = IS.new_table tblname rules in
    (* objects to return *)
    let new_objs = [table_actions.ta_true; table_actions.ta_false; table] in
    (* debug printing *)
    !dprint_endline "-----[from_if SUMMARY]-----";
    !dprint_endline "input statement:";
    !dprint_endline (Pr.stmt_to_string opstmt);
    !dprint_endline "new objects:";
    log_objs opstmt new_objs;
    print_endline "[from_if] finished.";
    new_objs
  ;;

  (**** match statements ****)
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

  let tblentry_from_branch keylist opgraph (patlist, stmt) =
    (* tricky: the statement here might not be an opstatement. *)
    let stmt = fst_op_of_branch stmt in
    match stmt.s with
    | SNoop ->
      (* if the action in this branch is a noop, its successor is the
      successor of the match statement *)
      let acn_name = Cid.fresh ["match_stmt_action"] in
      let succ_tbls = successortbls_of_stmt opgraph stmt in
      let acn = IS.new_action acn_name [] succ_tbls in
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

  let from_match hdl_id opgraph opstmt =
    let _ = opgraph in
    let keys, branches = unpack_match opstmt in
    let key_mids = CL.map name_from_exp keys |> CL.map (mid_from_cid hdl_id) in
    (* make a rule and action for each entry. *)
    let tbl_entries = CL.map (tblentry_from_branch key_mids opgraph) branches in
    (* may need to make a default rule too, if the last one is not PWild,
    and link it to the first statement after the match block ends.
    (I think this is why we passed in opgraph) *)
    let rules, acns = CL.split tbl_entries in
    let tblname = tblname_of_stmt opstmt in
    (* make the table with the rules *)
    let tbl = IS.new_table tblname rules in
    (* return table and actions *)
    !dprint_endline "[from_match] handling match:";
    !dprint_endline (Pr.stmt_to_string opstmt);
    !dprint_endline "[from_match] generated table: ";
    !dprint_endline (DebugPrint.str_of_decls [tbl]);
    !dprint_endline "[from_match] generated actions: ";
    !dprint_endline (DebugPrint.str_of_decls acns);
    let new_objs = acns @ [tbl] in
    log_objs opstmt new_objs;
    new_objs
  ;;

  let from_opstmt hdl_id opgraph opstmt objs =
    !dprint_endline "-----[from_opstmt] ------";
    !dprint_endline ("creating table: " ^ Cid.to_string (tblname_of_stmt opstmt));
    !dprint_endline ("statement: " ^ Printing.stmt_to_string opstmt);
    print_endline (Printing.stmt_to_string opstmt);
    !dprint_endline ("op statement: " ^ OGSyntax.print_op_stmt opstmt);
    !dprint_endline "-----------";
    match opstmt.s with
    | SAssign _ -> objs @ from_assign hdl_id opgraph opstmt
    | SUnit _ ->
      objs @ from_unit hdl_id opgraph opstmt
      (* a local is a variable declaration followed by an assign. *)
    | SLocal _ -> objs @ from_local hdl_id opgraph opstmt
    | SGen _ -> objs @ from_gen hdl_id opgraph opstmt
    | SIf _ -> objs @ from_if hdl_id opgraph opstmt
    | SMatch _ -> objs @ from_match hdl_id opgraph opstmt
    | SSeq _ -> error "Seq is not an opstatement."
    | _ -> objs
  ;;

  (* main function. make a tofino control graph from an op statement graph. *)
  let from_opstmt_graph hdl_id opgraph =
    StGraph.fold_vertex (from_opstmt hdl_id opgraph) opgraph []
  ;;
end

(* convert a handler's statement graph into a tofino control graph *)
type tofino_control_g =
  { hid : id
  ; cid_decls : (Cid.t * IS.decl) list
  ; root_tid : Cid.t
  }

let dpahandler_from_handler hog_rec : tofino_control_g =
  (* 1: map each operation statement to an object *)
  let tofino_objs =
    TofinoControl.from_opstmt_graph hog_rec.h_name hog_rec.h_opgraph
  in
  (* 2. convert to an assoc list of object declarations *)
  let cid_decls = IS.dict_of_decls tofino_objs in
  LLValidate.validate_cid_decls cid_decls "[dpahandler_from_handler]";
  { hid = hog_rec.h_name; cid_decls; root_tid = tblname_of_stmt hog_rec.h_root }
;;

(* merge the handler definitions together into a complete
program for the backend syntax. *)
let merge_handler_defs (hdl_defs : tofino_control_g list) : IS.llProg =
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
  { root_tid = root_tblname; instr_dict = cid_decls }
;;

let start_logging () = DBG.start_mlog __FILE__ outc dprint_endline
