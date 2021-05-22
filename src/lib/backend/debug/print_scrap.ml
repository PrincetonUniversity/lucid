
let span_to_string_custom (span : Span.t) =
  if span = Span.default
  then "default_span"
  else Printf.sprintf "%d-%d" span.start span.finish
;;

let stmt_to_string_custom statement = 
  match statement.s with 
  | SIf _ -> Printf.sprintf "\"If-%s\"" (span_to_string_custom statement.sspan)
  | SAssign _ -> Printf.sprintf "\"Assign-%s\"" (span_to_string_custom statement.sspan)
  | SLocal _ -> Printf.sprintf "\"Local-%s\"" (span_to_string_custom statement.sspan)
  | SUnit _ -> Printf.sprintf "\"Unit-%s\"" (span_to_string_custom statement.sspan)
  | SGen _ -> Printf.sprintf "\"Gen-%s\"" (span_to_string_custom statement.sspan)
  | SNoop -> Printf.sprintf "\"Noop-%s\"" (span_to_string_custom statement.sspan)
  | SSeq _ -> Printf.sprintf "\"Seq-%s\"" (span_to_string_custom statement.sspan)
  | _ ->  Printf.sprintf "\"Other-%s\"" (span_to_string_custom statement.sspan)
;;
(* get a table id from a DPT statement *)
let tid_of_stmt (st:statement) : tid = 
  Cid.create [stmt_to_string_custom st]
;;

(* metadata id from DPT expression *)
let mid_of_varexp (ex : exp) : mid = 
  match ex.e with 
    | EVar cid -> cid
    | _ -> error "tried to get metadata id from non-var expression"
;;

let width_of_ty (rt:raw_ty) : int = 
  match rt with 
    | TInt (IConst w) -> w
    | TBool -> 8 (* TODO: change to 1 bit. *)
    | rty -> error ("cannot interpret type"^(show_raw_ty rty))
;;      

let width_of_varexp (ex : exp) : int = 
  match ex.ety with 
    | None -> error "tried to get width of variable expression with no type."
    | Some t -> width_of_ty t
;;

(* id, (params, param widths), object declarations, object call graph *)
type dpa_handler = id * mid list * int list * InstrSyntax.decl list * DagSyntax.G.t

(* 
(* convert dpt params into dpa params *)
let dpa_params_of_dpt_params (ps:params) = 
  let mids, tys = CL.split ps in 
  let mids = CL.map (Cid.id) mids in 
  let wids = CL.map (fun t -> width_of_ty t.raw_ty) tys in 
  mids, wids
;;
 *)


(* dead code 2/9/21 *)


(* print a table with a single routine that calls a single object. *)
let print_singleprogram_table fmt tbl_id obj_id stage_num_opt : unit =
  (* Print the stage number pragma... *)
  let acn_id = Cid.str_cons "acn" obj_id in
  (match stage_num_opt with
  | Some stage_num -> fprintf fmt "@pragma stage %i@," stage_num
  | None -> fprintf fmt "//@pragma stage ANY@,");
  tab ();
  fprintf fmt "table %s {@," (mid_to_str_suffix tbl_id);
  tab ();
  fprintf fmt "actions = {@,%s;" (mid_to_str_suffix acn_id);
  untab ();
  fprintf fmt "@,}@,";
  fprintf fmt "const default_action = %s();" (mid_to_str_suffix acn_id);
  untab ();
  fprintf fmt "@,}"
;;

(* datapath statement printers. *)
let dpPredToStr (pred : dpPredExpr) : string =
  match pred with
  | SimpleExpr (co, i1, i2) ->
    ((* Slice metadata. 
        TODO: this should be solved by the metadata type system. *)
    match i1 with
    | Meta _ ->
      (match i2 with
      | Meta _ -> error "one operand of a dpPred must be a constant."
      | _ -> str_of_oper i1 ^ "[12:0]" ^ " " ^ str_of_cmpop co ^ " " ^ str_of_oper i2)
    | _ ->
      (match i2 with
      | Meta _ ->
        str_of_oper i1 ^ " " ^ str_of_cmpop co ^ " " ^ str_of_oper i2 ^ "[12:0]"
      | _ -> str_of_oper i1 ^ " " ^ str_of_cmpop co ^ " " ^ str_of_oper i2))
;;

let rec dpStatementToStr (st : dpStatement) =
  match st with
  | Noop -> fprintf str_formatter ""
  | BranchTable (table_id, branch_list) ->
    print_branch_stmt table_id branch_list
  | CallTable table_id ->
    fprintf str_formatter "%s.apply();" (mid_to_str_suffix table_id)
  | IfElse (predExp, st1, st2) ->
    pp_open_vbox str_formatter 4;
    fprintf str_formatter "if (%s){@," (dpPredToStr predExp);
    dpStatementToStr st1;
    pp_close_box str_formatter ();
    fprintf str_formatter "@,} ";
    pp_open_vbox str_formatter 4;
    fprintf str_formatter "else {@,";
    dpStatementToStr st2;
    pp_close_box str_formatter ();
    fprintf str_formatter "@,}"
  | Seq (st1, st2) ->
    dpStatementToStr st1;
    fprintf str_formatter "@,";
    dpStatementToStr st2



let rec print_decl stage_tbl_map fmt (decl : decl) =
  match decl with
  | SchedBlock (tbl_id, printer, args, _, stage_opt) ->
    print_native_table fmt tbl_id printer args stage_opt
  | RegVec (rid, wid, len, def, _) -> print_reg_decl fmt rid wid len def
  | InstrVec (iid, iVec) -> print_ivec_decl fmt iid iVec
  | SInstrVec (sid, rid, rid_width, siv, out_var_opt, idx_oper) ->
    print_sivec_decl fmt sid rid rid_width siv out_var_opt idx_oper
  | Hasher (hasher_id, out_width, poly, out_var, in_vars) ->
    print_hasher_decl fmt hasher_id out_width poly out_var in_vars
  | GlobalMeta (newMid, midWidth) ->
    (* print_pragmas fmt newMid; *) (* allow P4 to overlay variables by commenting out this line *)
    fprintf fmt "bit<%i> %s;@," midWidth (mid_to_str newMid)
  (* ASSUMPTION: by this point, there is only one datapath in the decls list. *)
  | Routine (dpid, _, _, decls, st) ->
    fprintf fmt "@,//Thread-local decls @,";
    print_decls decls;
    print_dp_apply st
  | Action (actn_id, obj_ids, next_tids) ->
(*     fprintf fmt "@,//action %s@," (mid_to_str_suffix actn_id); *)
    print_action fmt actn_id obj_ids next_tids;
(*     fprintf fmt "@,//end action %s@," (mid_to_str_suffix actn_id) *)
  | Table (tbl_id, branches, stage_opt) ->
    let match_vars = match_vars_of_rules branches in 
    print_tbl fmt tbl_id match_vars branches stage_opt stage_tbl_map 

(* | _ -> error "unimplemented decl." *)
and print_decls decls =
  (* get a mapping from stage number to table, for 
     printing the ignore table dependency pragmas *)
  let stage_tables_map = stageMap_of_tbls decls in 
  let ppSep fmt () = fprintf fmt "@," in
  pp_print_list ~pp_sep:ppSep (print_decl stage_tables_map) str_formatter decls
;;


let printP4Block (blk : dataPath) =
  (* let (ccid, gDefs, gDecls, tProgs) = blk in  *)
  let ccid, gDefs, gWidths, gDecls = blk in
  let _ = gDefs in
  let _ = gWidths in
  pp_open_vbox str_formatter 4;
  fprintf
    str_formatter
    "control %s(inout %s %s, inout %s %s){@,"
    (mid_to_str ccid)
    dpa_global_t
    dpa_global_name
    dpa_local_t
    dpa_local_name;
  fprintf str_formatter "//Global decls @,";
  print_decls gDecls;
  pp_close_box str_formatter ();
  fprintf str_formatter "@,}"
;;

let print_p4_control blk out_f =
  pp_open_vbox str_formatter 0;
  printP4Block blk;
  let ctl_block = flush_str_formatter () in
  pp_close_box str_formatter ();
  Caml.Printf.fprintf out_f "%s" ctl_block
;;


(* print a table with multiple branches *)
let print_multiprogram_table fmt table_id key_vars action_ids def_action_id =
  tab ();
  fprintf fmt "table %s {@," (mid_to_str_suffix table_id);
  (match key_vars with
  | [] -> ()
  | _ ->
    tab ();
    fprintf fmt "key = {@,";
    pp_print_list
      ~pp_sep:lineSep
      (fun fmt key -> fprintf fmt "%s : ternary;" (mid_to_str key))
      fmt
      key_vars;
    untab ();
    fprintf fmt "@,}@,");
  fprintf fmt "actions = {";
  let actionNameToStr fmt actn_name =
    fprintf fmt "%s" (mid_to_str_suffix actn_name)
  in
  pp_print_list ~pp_sep:semiSep actionNameToStr fmt action_ids;
  fprintf fmt ";}@,";
  (match def_action_id with
  | Some def_action_id ->
    fprintf
      fmt
      "const default_action = %s();@,"
      (mid_to_str_suffix def_action_id)
  | _ -> ());
  untab ();
  fprintf fmt "@,}@,"
;;




let actionToStr fmt acn_name =
  fprintf fmt "action %s() {}@," (mid_to_str_suffix acn_name)
;;

let printEmptyActions fmt action_ids =
  pp_print_list ~pp_sep:lineSep actionToStr fmt action_ids
;;

