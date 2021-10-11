(* passes over the dag program.
  11/16/20 -- this pass sets up input and output variables for salus that have a limited number of i/o vars. 
  It basically transforms sInstrs from salus to salu _calls_
  There are many optimizations to do here, also the DPA syntax makes this really messy, and should be changed.
 *)
open Format
open LLSyntax
open MiscUtils
open DFSyntax
open DebugPrint
open Liveliness
open MiscUtils
module CL = Caml.List

(* logging *)
module DBG = BackendLogging

let outc = ref None
let dprint_endline = ref DBG.no_printf

(* printing hints to rewrite program for better layout... *)
let out_hintc = ref None
let hintprint_endline = ref DBG.no_printf

(* a map from variable id to all the variable ids that it replaces *)
let var_contains_map = ref []
let unmergable_var_sets = ref []

(* concat b to assoc_list[a] *)
let emplace_concat assoc_list (a, b) =
  match Cid.lookup_opt assoc_list a with
  | Some bs -> emplace assoc_list (a, bs @ b)
  | None -> emplace assoc_list (a, b)
;;

let print_salu_input_conflict_hint rid vars conflict_pairs =
  unmergable_var_sets := !unmergable_var_sets @ [conflict_pairs];
  let hint_str =
    sprintf
      "[salu input merging] there are conflicts between the variables that the \
       salus for register array %s access\n"
      (P4tPrint.str_of_private_oid rid)
  in
  let hint_str =
    hint_str ^ sprintf "[salu input merging] (%s) \n" (str_of_cids vars)
  in
  let map_f v =
    match Cid.lookup_opt !var_contains_map v with
    | None -> None
    | Some vs ->
      Some
        (sprintf
           "\t%s represents (%s)"
           (P4tPrint.str_of_private_oid v)
           (str_of_cids vs))
  in
  let represented_vs_str =
    (CL.filter_map map_f vars |> String.concat "\n") ^ "\n"
  in
  let hint_str = hint_str ^ represented_vs_str in
  let hint_str =
    hint_str
    ^ sprintf "conflict pairs: %s\n" (dbgstr_of_cidpairs conflict_pairs)
  in
  let hint_str =
    hint_str
    ^ sprintf
        "[salu input merging] removing these conflicts may allow a more \
         compact layout. \n"
  in
  !hintprint_endline hint_str
;;

let print_salu_input_merge_hint master_v (vs : mid list) =
  (* loop through the contains map, update the lists for any var that represents master_v*)
  var_contains_map := emplace_concat !var_contains_map (master_v, vs);
  let hint_str =
    sprintf
      "[salu input merging] %s <-- (%s)\n"
      (P4tPrint.str_of_private_oid master_v)
      (str_of_cids vs)
  in
  !hintprint_endline hint_str
;;

let print_salu_merge_summary_hint () =
  match !unmergable_var_sets with
  | [] ->
    !hintprint_endline
      "note: all salu input / output variable were merged successfully! No \
       call overhead."
  | var_sets ->
    !hintprint_endline
      (sprintf
         "note: %i var sets could not be merged. There is call overhead."
         (CL.length var_sets))
;;

  (* generate code to pass outer_var in the caller to inner_var in the salu_call *)  
let prepare_call_var_for_salu inner_var cid_decls (salu_call, outer_var) =
  (* 
    replace outer_var in salu_call with inner_var. 
    Add a command, before the call, that sets inner_var := outer_var.
  example input: 
    salu_call {
      outer_var = foo; 
    }
  example output:
    inner_var = outer_var;
    salu_call {
      inner_var = foo;
    }
  *)

  let tbl_id = Cid.concat inner_var (oid_of_sInstr salu_call) in

  (* inner_var := outer_var *)
  let new_decls = tbl_of_assign tbl_id inner_var (Meta outer_var) in

  let cid_decls = batch_emplace_decl cid_decls new_decls in
  let salu_call_tid = tid_of_oid cid_decls (id_of_decl salu_call) in
  let cid_decls = insert_before_tid cid_decls tbl_id salu_call_tid in

  (* replace outer_var with inner_var in the salu call *)
  let new_salu_call = replace_mid_in_decl salu_call outer_var inner_var in
  let cid_decls = emplace_decl cid_decls new_salu_call in
  cid_decls
;;

(* set up to fill arg_var after running salu_call, assuming that 
   salu_call returns ret_var 
  salu_call() {
    ...
    return ret_var;
  }
  arg_var = salu_call(...);
 *)
let prepare_ret_var_for_salu inner_var cid_decls (salu_call, outer_var) =
  (* set outer_var = inner_var;
     place instr after salu_call
     change outer_var to inner_var in salu_call *)
  let tbl_id = Cid.concat inner_var (oid_of_sInstr salu_call) in
  let new_decls = tbl_of_assign tbl_id outer_var (Meta inner_var) in
  let cid_decls = batch_emplace_decl cid_decls new_decls in
  let salu_call_tid = tid_of_oid cid_decls (id_of_decl salu_call) in
  let cid_decls = insert_after_tid cid_decls salu_call_tid tbl_id in
  let new_salu_call = replace_mid_in_decl salu_call outer_var inner_var in
  let cid_decls = emplace_decl cid_decls new_salu_call in
  cid_decls
;;

(* Find all pairs of variables that conflict, given a list of variables (vars). *)
let conflict_pairs_of arg_vars cid_decls (vars : mid list) : (mid * mid) list =
  let var_pairs = get_all_pairs vars vars in
  let interf_g = interf_g_of_dmap arg_vars cid_decls in
  let fold_f conflict_pairs (var1, var2) =
    match UG.mem_edge interf_g var1 var2 with
    | true -> (var1, var2) :: conflict_pairs
    | false -> conflict_pairs
  in
  CL.fold_left fold_f [] var_pairs
;;

(* Replace v with new_v everywhere it appears in cid_decls. 
   Delete the declaration for v.
   Assume that new_v is already declared. *)
let replace_var cid_decls v new_v =
  let renamer =
    object
      inherit [_] dataPathMap as super

      val mutable cur_oid : oid option = None

      method cur_oid = cur_oid

      method! visit_mid _ mid =
        match Cid.equals mid v with
        | true ->
          DBG.printf
            outc
            "[replace_var:renamer] in declaration: %s\n"
            (P4tPrint.str_of_private_oid (Option.get cur_oid));
          DBG.printf
            outc
            "[replace_var:renamer] replacing %s with %s\n"
            (P4tPrint.str_of_private_oid mid)
            (P4tPrint.str_of_private_oid new_v);
          new_v
        | false -> mid

      method! visit_decl env dec =
        cur_oid <- Some (id_of_decl dec);
        super#visit_decl env dec
    end
  in
  let map_f (id, dec) =
    match Cid.equals id v with
    | true ->
      (* delete declaration for v *)
      None
    | false ->
      (* update declaration, replacing all uses of v*)
      let new_dec = renamer#visit_decl () dec in
      Some (id, new_dec)
  in
  CL.filter_map map_f cid_decls
;;

(* left off: arg_vars, which come from the declaration, don't have header prefixes in their ids... figure out a solution in MergeHandlers.ml *)

(* collapse all non-conflicting vars into a single var *)
(* if one of the vars is an arg, rename all vars to that. *)
let merge_vars_in arg_vars cid_decls vars =
  let filter_f v = CL.mem v arg_vars in
  let arg_vars_in_vars = CL.filter filter_f vars in
  DBG.printf outc "[merge_vars_in] arg_vars: (%s)\n" (str_of_cids arg_vars);
  let master_v =
    match arg_vars_in_vars with
    | [] ->
      DBG.printf outc "[merge_vars_in] no arg vars found.\n";
      CL.hd vars
    | [arg_var] ->
      DBG.printf outc "[merge_vars_in] 1 arg var found.\n";
      arg_var
    | _ ->
      error
        "[merge_vars_in] error: cannot merge vars when multiple vars are args."
  in
  let vars_to_remove = CL.filter (fun v -> not (Cid.equals v master_v)) vars in
  DBG.printf
    outc
    "[merge_vars_in] merging (%s) --> %s\n"
    (str_of_cids vars_to_remove)
    (P4tPrint.str_of_private_oid master_v);
  let fold_f cid_decls v = replace_var cid_decls v master_v in
  print_salu_input_merge_hint master_v vars_to_remove;
  CL.fold_left fold_f cid_decls vars_to_remove
;;

type shared_arg_var =
  | SharedInput of mid
  | SharedIndex of mid

let cid_of_shared_arg_var sav =
  match sav with
  | SharedIndex cid | SharedInput cid -> cid
;;

(* This function does not attempt to overlay variables, it always makes a temporary variable. *)
let prepare_frame_var_for_rid_no_merging
    max_len
    _
    salu_param_finder
    prog_update_fcn
    (shared_salu_arg_var : shared_arg_var)
    cid_decls
    rid
  =
  let salu_arg_id = cid_of_shared_arg_var shared_salu_arg_var in
  (* get the variable in all salus that use this register. *)
  let salu_vars_map = salu_param_finder cid_decls rid in
  let _, vars = CL.split salu_vars_map in
  let vars = unique_list_of vars in
  match CL.length vars <= max_len with
  | true -> cid_decls (* nothing needs to change. *)
  (* there are conflicts. We have to add the per-register input variable and 
       instructions to load arguments to the input variable before the salu call. *)
  | false ->
    DBG.printf outc "-------------\n";
    DBG.printf
      outc
      "[prepare_frame_var_for_rid] memory cell: %s\n"
      (P4tPrint.str_of_private_oid rid);
    DBG.printf
      outc
      "[prepare_frame_var_for_rid] parameter: %s\n"
      (P4tPrint.str_of_private_oid salu_arg_id);
    DBG.printf outc "[prepare_frame_var_for_rid] vars: %s\n" (str_of_cids vars);
    DBG.printf outc "-------------\n";
    (* the width of the intermediate is equal to the width of the register's cell *)
    (* except for index variables, which are always 32-bits (for now) *)
    let var_size = find_width_of_var cid_decls (CL.hd vars) in 
    (* declare the per-register input variable. *)
    let salu_var_decl = to_globalmeta salu_arg_id var_size in
    let cid_decls = emplace_decl cid_decls salu_var_decl in
    let cid_decls =
      CL.fold_left (prog_update_fcn salu_arg_id) cid_decls salu_vars_map
    in
    cid_decls
;;

(* create and set an input variable for all salus that use a register *)
let prepare_frame_var_for_rid
    max_len
    unmergable_vars
    salu_param_finder
    prog_update_fcn
    (shared_salu_arg_var : shared_arg_var)
    cid_decls
    rid
  =
  let salu_arg_id = cid_of_shared_arg_var shared_salu_arg_var in
  (* get the set of variables that need to be overlaid. *) 
  let salu_vars_map = salu_param_finder cid_decls rid in
  let vars = CL.split salu_vars_map |> snd |> unique_list_of in
  match CL.length vars <= max_len with
  | true -> cid_decls (* nothing needs to change. *)
  | false ->
    let conflict_pairs = conflict_pairs_of unmergable_vars cid_decls vars in
    (match CL.length conflict_pairs with
    (* more than 1 variable, but there are no conflicts, so we can merge all variables. *)
    | 0 ->
      DBG.printf outc "-------------\n";
      DBG.printf
        outc
        "[prepare_frame_var_for_rid] memory cell: %s\n"
        (P4tPrint.str_of_private_oid rid);
      DBG.printf
        outc
        "[prepare_frame_var_for_rid] parameter: %s\n"
        (P4tPrint.str_of_private_oid salu_arg_id);
      DBG.printf
        outc
        "[prepare_frame_var_for_rid] vars: %s\n"
        (str_of_cids vars);
      DBG.printf outc "[prepare_frame_var_for_rid] no conflicts!\n";
      DBG.printf outc "-------------\n";
      merge_vars_in unmergable_vars cid_decls vars
    (* there are conflicts. We have to add the per-register input variable and 
           instructions to load arguments to the input variable before the salu call. *)
    | _ ->
      print_salu_input_conflict_hint rid vars conflict_pairs;
      DBG.printf outc "-------------\n";
      DBG.printf
        outc
        "[prepare_frame_var_for_rid] memory cell: %s\n"
        (P4tPrint.str_of_private_oid rid);
      DBG.printf
        outc
        "[prepare_frame_var_for_rid] parameter: %s\n"
        (P4tPrint.str_of_private_oid salu_arg_id);
      DBG.printf
        outc
        "[prepare_frame_var_for_rid] vars: %s\n"
        (str_of_cids vars);
      DBG.printf
        outc
        "[prepare_frame_var_for_rid] conflict pairs: %s\n"
        (dbgstr_of_cidpairs conflict_pairs);
      DBG.printf outc "-------------\n";
      (* the width of the intermediate is equal to the width 
         of the first variable that must be merged. This assumes 
         that all variables passed to the same array.update parameter 
         for a given array are the same type. That actually may 
         not be true for the index variable, in which case 
         incorrect code will be generated. 
       *)
      let var_size = find_width_of_var cid_decls (CL.hd vars) in 
  (*     let var_size =
        match shared_salu_arg_var with
        | SharedIndex _ -> find_width_of_var cid_decls (CL.hd vars)
        | SharedInput _ -> width_of_regvec (Cid.lookup cid_decls rid)
      in *)

      (* declare the per-register input variable. *)
      (* the declaration needs to be sized based on the variables that will be assigned to it. *)
      let salu_var_decl = to_globalmeta salu_arg_id var_size in
      let cid_decls = emplace_decl cid_decls salu_var_decl in
      let cid_decls =
        CL.fold_left (prog_update_fcn salu_arg_id) cid_decls salu_vars_map
      in
      cid_decls)
;;

(* get (salu_call, index var id) pairs for all 
   salu_calls to rid that use a variable 
   as an index argument 
 *)
let salu_idx_vars_by_rid cid_decls rid =
  let filter_map_f sInstr_id =
    let salu_call = Cid.lookup cid_decls sInstr_id in
    let idx_arg = idx_of_sInstr salu_call in
    match idx_arg with
    | Const _ -> None
    | Meta m -> Some (salu_call, m)
    | _ -> error "[salu_idx_vars_by_rid] index variable to an sALU must be a const or meta variable"
  in
  let var_idx_args = CL.filter_map filter_map_f (sInstrs_of_rid cid_decls rid) in
  var_idx_args
;;

(* infer the first positional argument of every salu_call that accesses rid *)
let fst_salu_read_vars_by_rid cid_decls rid =
  let filter_map_f sInstr_id =
    let salu_call = Cid.lookup cid_decls sInstr_id in
    let read_args = readvars_of_sInstr salu_call in 
    Printf.printf "[fst_salu_read_vars_by_rid] rid: %s salu_id: %s read_args: %s\n" (P4tPrint.str_of_varid rid) (Cid.to_string sInstr_id) (P4tPrint.str_of_varids read_args); 
    match read_args with
    | fst :: _ ->
      Some (salu_call, fst)
    | _ -> None
  in
  let pos_args = CL.filter_map filter_map_f (sInstrs_of_rid cid_decls rid) in
  pos_args
;;

let snd_salu_read_vars_by_rid cid_decls rid =
  let filter_map_f sInstr_id =
    let salu_call = Cid.lookup cid_decls sInstr_id in
    let read_args = readvars_of_sInstr salu_call in
    match read_args with
    | _ :: snd :: _ -> Some (salu_call, snd)
    | _ -> None
  in
  let pos_args = CL.filter_map filter_map_f (sInstrs_of_rid cid_decls rid) in
  pos_args
;;

let salu_ret_vars_by_rid cid_decls rid =
  let filter_map_f sInstr_id =
    let salu_call = Cid.lookup cid_decls sInstr_id in
    let ret_arg = outarg_of_sInstr salu_call in
    match ret_arg with
    | Some ret_mid -> Some (salu_call, ret_mid)
    | None -> None
  in
  let out_args = CL.filter_map filter_map_f (sInstrs_of_rid cid_decls rid) in
  out_args
;;

let shared_reg_argid prefix rid =
  match rid with
  | Cid.Id id -> Cid.id (Id.prepend_string prefix id)
  | _ -> error "[shared_reg_argid] unexpected: register id is a compound id."
;;

(* add the frame setup instructions for every salu that uses a register
   we do it by register because there are per-register limits on arguments. *)
let prepare_salu_frames_for_rid
    prepare_frame_var_for_rid
    unmergable_vars
    cid_decls
    rid
  =
  (* merge index vars *)
  let cid_decls =
    prepare_frame_var_for_rid
      1
      unmergable_vars
      salu_idx_vars_by_rid (* how to find index vars *)
      prepare_call_var_for_salu
      (* what code to add to the program to prepare the call *)
      (SharedIndex (shared_reg_argid "INDEX_VAR_OF_" rid))
      cid_decls
      rid
  in
  (* merge input vars by position *)
  let cid_decls =
    prepare_frame_var_for_rid
      1 (* the maximum number of unique variables that the register can read. *)
      unmergable_vars
      fst_salu_read_vars_by_rid (* how to find first pos vars *)
      prepare_call_var_for_salu
      (* what code to add to the program to prepare the call *)
      (SharedInput (shared_reg_argid "ARG1_VAR_OF_" rid))
      (* name of the first positional var used by this register's salu *)
      cid_decls
      rid
  in
  let cid_decls =
    prepare_frame_var_for_rid
      1
      unmergable_vars
      snd_salu_read_vars_by_rid (* how to find second pos vars *)
      prepare_call_var_for_salu
      (* what code to add to the program to prepare the call *)
      (SharedInput (shared_reg_argid "ARG2_VAR_OF_" rid))
      (* name of the second positional var used by this register's salu *)
      cid_decls
      rid
  in
  (* merge output vars  *)
  (* 11/19/20 -- this is not needed *)
  (*   let cid_decls = prepare_frame_var_for_rid 
    unmergable_vars
    salu_ret_vars_by_rid
    prepare_ret_var_for_salu
    (Cid.concat (Cid.create ["RET_VAR_OF"]) rid)
    cid_decls 
    rid 
  in 
 *)
  cid_decls
;;

let str_of_new_decls orig_cid_decls updated_cid_decls =
  let _, orig_decls = CL.split orig_cid_decls in
  let _, updated_decls = CL.split updated_cid_decls in
  let filter_f ud = not (CL.mem ud orig_decls) in
  let _, _ = filter_f, updated_decls in
  (* let changed_decls = CL.filter filter_f updated_decls in  *)
  (* pp_open_vbox str_formatter 0; *)
  (* fprintf str_formatter "number of changed decls: %i\n" (CL.length changed_decls); *)
  (* DagToP4.print_decls_from_list str_formatter changed_decls; *)
  (* pp_close_box str_formatter (); *)
  (* let str = flush_str_formatter () in  *)
  let str = "" in
  str
;;

(* new analysis function -- find the variables that 
declared outside the scope of a set of declarations, 
i.e., they are used in a compute object but never
declared in a MetaVar *)
let find_unbound_vars cid_decls =
  let ds = snd (CL.split cid_decls) in
  (* get all the mids and lmids *)
  let mids_of_compute_objs dec =
    let v =
      object
        inherit [_] dataPathIter as super

        val mutable mids = []

        method mids = mids

        method! visit_mid _ m = mids <- m :: mids
      end
    in
    v#visit_decl () dec;
    v#mids
  in
  let is_compute_object d =
    match d with
    | MetaVar _
    | StructDef _
    | StructVar _
    | ConstVar _
    | ParseTree _
    | SchedBlock _ -> false
    | _ -> true
  in
  let compute_ds = CL.filter is_compute_object ds in
  let all_mids = CL.map mids_of_compute_objs compute_ds |> CL.flatten in
  let mid_of_metadec d =
    match d with
    | MetaVar (mid, _) -> Some mid
    | _ -> None
  in
  let declared_mids = CL.filter_map mid_of_metadec ds in
  let struct_mids = list_sub all_mids declared_mids in
  !dprint_endline
    ("undeclared mids that are used in compute objects: "
    ^ P4tPrint.str_of_varids struct_mids);
  (* struct_mids should consist of struct instances and their fields *)
  struct_mids
;;

let prepare_salu_frames dag prepare_frame_var_for_rid =
  let cid_decls = dmap_of_dprog dag in
  (* we do not merge mids declared inside of structures, 
     because some structures are bound outside of the program. *)
  let struct_mids = find_unbound_vars cid_decls in 
  let rids = ids_of_type is_reg cid_decls in 
  let updated_cid_decls = CL.fold_left (prepare_salu_frames_for_rid prepare_frame_var_for_rid struct_mids) cid_decls rids in 
  let updated_g = graph_of_declsMap updated_cid_decls in 

  let _, root_tbl_id, _ = dag in   
  let framed_dag = (updated_cid_decls, root_tbl_id, updated_g) in 
  (* DagToP4.print_control dag (!BackendLogging.irLogDir^"/pre_salu_frames.p4"); *)
  (* DagToP4.print_control framed_dag (!BackendLogging.irLogDir^"/post_salu_frames.p4"); *)
  print_salu_merge_summary_hint ();
  framed_dag
;;

(* try to merge alu input variables before creating temp variables. *)
let merge_and_temp dag =
  DBG.start_mlog __FILE__ outc dprint_endline;
  DBG.start_mlog "dagPass_optimization_hints" out_hintc hintprint_endline;
  let framed_dag = prepare_salu_frames dag prepare_frame_var_for_rid in
  framed_dag
;;

(* just create alu input variables, don't do merging. *)
let temp_only dag =
  DBG.start_mlog __FILE__ outc dprint_endline;
  DBG.start_mlog "dagPass_optimization_hints" out_hintc hintprint_endline;
  let framed_dag =
    prepare_salu_frames dag prepare_frame_var_for_rid_no_merging
  in
  framed_dag
;;

(* let doPasses dag =
  DBG.start_mlog __FILE__ outc dprint_endline;
  DBG.start_mlog "dagPass_optimization_hints" out_hintc hintprint_endline;

  let framed_dag = prepare_salu_frames dag prepare_frame_var_for_rid in 
  framed_dag
  (* dag *)
;;
 *)
