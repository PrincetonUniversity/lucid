(* Extension of LLSyntax where objects 
  are laid out in a pipeline of stages. 
  Built from the DFSyntax. *)
open Consts
open LLSyntax
open DFSyntax
open SLSyntax
module MU = MergeUtils
module CL = Caml.List
open Printf
open Format
open DebugPrint
open MiscUtils

exception Error of string

let error s = raise (Error s)

let silent = ref false;;

let layout_report str =
  if (not !silent) then (
    Console.show_message str ANSITerminal.Green "Pipeline layout"
  )
;;


(* logging *)
module DBG = BackendLogging
let outc = ref None
let dprint_endline = ref DBG.no_printf
let start_logging () = DBG.start_mlog __FILE__ outc dprint_endline

let str_of_tid = P4tPrint.str_of_private_oid
let str_of_tids tids = CL.map str_of_tid tids |> String.concat ", "

(*** pipe syntax ***)

(* a table group is a set of tables that will be merged into one table. 
   each field corresponds to a different constraint. *)
type table_group =
  { g_tbl : decl
  ; g_acns : decl list
  ; g_alus : decl list
  ; (* the objects merged into this table group, these are just here for tracking. *)
    g_src : decl list
  ; g_solitary : bool
  }

(* a stage is a vector of tables applied in parallel. *)
type stage =
  { s_num : int
  ; (* stage number *)
    s_tgs : table_group list
  }

(* a pipe is a sequence of stages *)
type pipe =
  { p_globals : decl list
  ; (* global declarations: variables, register arrays, etc. *)
    p_stages : stage list
  }

(*** constructors ***)
let tg_ct = ref 0

let tg_next () =
  tg_ct := !tg_ct + 1;
  !tg_ct
;;

let empty_tg stage_num =
  let merged_id = tg_next () in
  let tbl_id = Cid.create_ids [Id.to_id ("merged_tbl", merged_id)] in
  let acn_id = Cid.create_ids [Id.to_id ("merged_acn", merged_id)] in
  let acn = Action (acn_id, [], []) in
  (* a rule to do nothing for any packet.*)
  let def_rule = Match (Cid.fresh ["r"], [], acn_id) in
  let tbl = Table (tbl_id, [def_rule], Some stage_num) in
  { g_tbl = tbl; g_acns = [acn]; g_alus = []; g_src = []; g_solitary = false}
;;

let empty_stage num = { s_num = num; s_tgs = [] }
let empty_pipe = { p_globals = []; p_stages = [] }


(*** accessors ***)
let tid_of_tg tg = tid_of_tbl tg.g_tbl
let objs_of_tg tg = (tg.g_tbl :: tg.g_acns) @ tg.g_alus |> CL.rev

(* reverse order so that everything is declared before use. *)

let regs_of_tg tg =
  let reg_of_alu dec =
    match dec with
    | SInstrVec (_, { sRid = rid; _ }) -> Some rid
    | _ -> None
  in
  objs_of_tg tg |> CL.filter_map reg_of_alu
;;

let hashers_of_tg tg = 
  (objs_of_tg tg |> CL.filter is_hash)
  @ (objs_of_tg tg |> CL.filter is_random) (* randoms use hash alu. *)
let src_of_tg tg = tg.g_src

let src_tids_of_tg tg =
  tg.g_src |> dict_of_decls |> tbls_of_dmap |> CL.map id_of_decl
;;

let src_aids_of_tg tg =
  tg.g_src |> dict_of_decls |> acns_of_dmap |> CL.map id_of_decl
;;

let src_of_stage stage = CL.map src_of_tg stage.s_tgs |> CL.flatten
let src_tids_of_stage stage = CL.map src_tids_of_tg stage.s_tgs |> CL.flatten
let src_aids_of_stage stage = CL.map src_aids_of_tg stage.s_tgs |> CL.flatten
let objs_of_stage stage = CL.map objs_of_tg stage.s_tgs |> CL.flatten
let tbls_of_stage stage = CL.map (fun tg -> tg.g_tbl) stage.s_tgs

(* all the source object placed in the pipe *)
let src_of_pipe pipe = CL.map src_of_stage pipe.p_stages |> CL.flatten

let objs_of_pipe pipe =
  pipe.p_globals @ (CL.map objs_of_stage pipe.p_stages |> CL.flatten)
;;

let tbls_of_pipe pipe =
  (* staged tables *)
  CL.map tbls_of_stage pipe.p_stages |> CL.flatten
;;

(*** updates ***)


(* merge the table cid_decls[tid], and all of the actions / objects that it calls, 
into tg. return the updated table group and cid decls. This function is 
NOT AWARE OF CONSTRAINTS. *)
let merge_into_group dagProg group tid : table_group =
  (* get the objects for tbl group and new tid *)
  let group_tid = tid_of_tg group in
  let group_decls = objs_of_tg group in
  let new_decls = objs_of_tid dagProg.dp_instr_dict tid in
  !dprint_endline "[merge_into_group] --- single-operation tables --- ";
  !dprint_endline (DebugPrint.str_of_decls new_decls);
  !dprint_endline "[merge_into_group] --- single-operation tables --- ";
  let relevant_cid_decls = group_decls @ new_decls |> dict_of_decls in
  (* do the merge *)
  let merged_cid_decls = MU.parallel_merge relevant_cid_decls group_tid tid in
  !dprint_endline "[merge_into_group] --- updated group table --- ";
  !dprint_endline
    (Cid.lookup merged_cid_decls group_tid |> DebugPrint.str_of_decl);
  !dprint_endline "[merge_into_group] --- updated group table --- ";
  (* build the merged group *)
  { (* update the table, whose rules and fields may have changed. *)
    g_tbl = Cid.lookup merged_cid_decls group_tid
  ; (* update the merged actions and objects. *)
    g_acns = acns_of_tid merged_cid_decls group_tid |> unique_list_of
  ; g_alus = alus_of_tid merged_cid_decls group_tid |> unique_list_of
  ; (* add the new source objects to the source fields. *)
    g_src = group.g_src @ new_decls
  ; g_solitary = false
  }
;;

let merge_set_into_group dagProg group tids : table_group =
  CL.fold_left (merge_into_group dagProg) group tids
;;

(* merge cid_decls[tid] 
  **and all of the tables that it shares registers with** 
  into tg *)
let merge_into_group_with_shared_reg_tbls dagProg group tid =
  let regmates_of_tid = regmates_of_tid dagProg.dp_instr_dict tid in
  sprintf
    "[merge_into_group_with_shared_reg_tbls] merging %s and %i other tables \
     that share the same registers"
    (str_of_tid tid)
    (CL.length regmates_of_tid)
  |> !dprint_endline;
  CL.fold_left (merge_into_group dagProg) group (tid :: regmates_of_tid)
;;

let add_tg s tg = { s with s_tgs = s.s_tgs @ [tg] }
let add_stage p s = { p with p_stages = p.p_stages @ [s] }

let replace_last_stage p s =
  let new_stages = s :: (CL.rev p.p_stages |> CL.tl) |> CL.rev in
  { p with p_stages = new_stages }
;;

(*** to string ***)
let summary_of_group g =
  CL.map id_of_decl g.g_src
  |> str_of_tids
  |> sprintf "<%s:[%s]>" (str_of_tids [id_of_decl g.g_tbl])
;;

let summary_of_stage s =
  sprintf "[%s]" (String.concat ", " (CL.map summary_of_group s.s_tgs))
;;

let summary_of_pipe p = String.concat "\n" (CL.map summary_of_stage p.p_stages)
let dump_pipe p = DBG.printf outc "layout:\n%s\n" (summary_of_pipe p)

let dbgstr_of_tg tg =
  let tg_str = str_of_decls ((tg.g_tbl :: tg.g_acns) @ tg.g_alus) in
  let individual_objs_str = str_of_decls (tg.g_src @ tg.g_src) in
  "----table group objects --------\n"
  ^ tg_str
  ^ "---- original objects ----\n"
  ^ individual_objs_str
;;

let dbgstr_of_stage s =
  let tgs_str = CL.map dbgstr_of_tg s.s_tgs |> String.concat "\n" in
  "---- stage ----\n"
  ^ PrintUtils.indent_block tgs_str
  ^ "---- end stage ----\n"
;;

let dbgstr_of_pipe p =
  let globals_str = str_of_decls p.p_globals in
  let pipe_str = CL.map dbgstr_of_stage p.p_stages |> String.concat "\n" in
  "---- GLOBAL OBJECTS ----\n"
  ^ globals_str
  ^ "---- STAGE OBJECTS ----\n"
  ^ pipe_str
;;

(*** constraint checking  ***)
module Constraints = struct
  (* check if tg meets table group constraints *)
  type group =
    { gmax_tbls : int
    ; gmax_regs : int
    ; gmax_hashers : int
    ; gmax_matchbits : int
    ; gmax_actions : int
    }

  let group_def =
    { gmax_tbls = 100
    ; (* there really is no limit to how many tables you can merge together. This is just here for debugging. *)
      gmax_regs = 1
    ; gmax_hashers = 1
    ; gmax_matchbits = 512
    ; gmax_actions = 25 (* this should probably count alus. *)
    }
  ;;

  (* check if stage meet stage constraints *)
  type stage =
    { smax_tbls : int
    ; smax_regs : int
    ; smax_hashers : int
    }

  let stage_def = { smax_tbls = 16; smax_regs = 4; smax_hashers = 6 }

  let group_check_debug cid_decls =
    !dprint_endline "--- all the decls in cid_decls before max_matchbits --- ";
    !dprint_endline (DebugPrint.str_of_decls (CL.split cid_decls |> snd));
    !dprint_endline "--- all the decls in cid_decls before max_matchbits --- ";
    !dprint_endline (show_declsMap cid_decls);
    !dprint_endline "--- all the decls in cid_decls before max_matchbits --- ";
    !dprint_endline "--- all the struct related decls --- ";
    CL.split cid_decls
    |> snd
    |> CL.filter_map (fun dec ->
           match dec with
           | StructVar _ | StructDef _ -> Some (show_decl dec)
           | _ -> None)
    |> String.concat "\n--\n"
    |> !dprint_endline
  ;;

  (* this is a check that applies _after_ we merge a table into a group. *)
  let group_check dagProg tg =
    let max_tbls tg =
      let res =
        CL.length (src_tids_of_tg tg |> unique_list_of) <= group_def.gmax_tbls
      in
      if res <> true then !dprint_endline "[group_check] fail: max_tbls";
      res
    in
    let max_regs tg =
      let res =
        CL.length (regs_of_tg tg |> unique_list_of) <= group_def.gmax_regs
      in
      if res <> true then !dprint_endline "[group_check] fail: max_regs";
      res
    in
    let max_hash tg =
      let res =
        CL.length (hashers_of_tg tg |> unique_list_of) <= group_def.gmax_hashers
      in
      if res <> true then !dprint_endline "[group_check] fail: max_hash";
      res
    in
    let max_matchbits dagProg tg =
      let keys = keys_of_table tg.g_tbl in
      (* bug when the variable is a parameter. *) 
      let widths = CL.map (find_width_of_var dagProg) keys in
      !dprint_endline
        ("[max_matchbits] checking table group: " ^ summary_of_group tg);
      CL.combine keys widths
      |> CL.map (fun (v, w) -> sprintf "%s: %i" (str_of_tid v) w)
      |> CL.iter (fun st -> !dprint_endline ("[max_matchbits] " ^ st));
      let total_width = CL.fold_left ( + ) 0 widths in
      !dprint_endline
        ("[max_matchbits] tg keys total_width: " ^ string_of_int total_width);
      let res = total_width <= group_def.gmax_matchbits in
      if res <> true then !dprint_endline "[group_check] fail: max_matchbits";
      res
    in
    let max_actions tg = 
      let n_acns = CL.length (tg.g_acns) in 
      !dprint_endline
        ("[max_actions] tg number of actions: " ^ string_of_int n_acns);
      n_acns <= group_def.gmax_actions      
    in
    max_tbls tg && max_regs tg && max_hash tg && max_matchbits dagProg tg && max_actions tg
  ;;

  (* When we place table t into a stage s, we have to make 
    sure that the dataflow constraints are still satisfied:
      1. all of tid's predecessors are already placed. 
        * this is always true because we traverse  
          the data flow graph topologically. 
      2. none of tid's predecessors appear at or after stage s.
        * we are only every attempting to place in the last stage. 
          so all we have to check is that the predecessors don't 
          appear in stage s itself. 
    *)
  let dataflow_check dfg tid prior_stages =
    (* the tables that preceed this table, according to the dfg *)
    let pred_tids = G.pred dfg tid |> unique_list_of in
    (* the tables placed in prior stages *)
    let placed_tids =
      CL.map src_tids_of_stage prior_stages |> CL.flatten |> unique_list_of
    in
    let unplaced_preds = list_sub pred_tids placed_tids in
    (* if some pred actions were not placed, fail. *)
    !dprint_endline ("[dataflow_check] table id: " ^ str_of_tid tid);
    !dprint_endline ("[dataflow_check] pred_tids: " ^ str_of_tids pred_tids);
    !dprint_endline ("[dataflow_check] placed_tids: " ^ str_of_tids placed_tids);
    !dprint_endline
      ("[dataflow_check] unplaced_preds: " ^ str_of_tids unplaced_preds);
    match unplaced_preds with
    | [] -> true
    | _ ->
      !dprint_endline "[dataflow_check] fail";
      false
  ;;

  (* are all the predecessors of all the tids placed into previous stages? *)
  let dataflow_multi_check dfg tids prior_stages =
    let single_checks =
      CL.map (fun tid -> dataflow_check dfg tid prior_stages) tids
    in
    CL.fold_left ( && ) true single_checks
  ;;

  let stage_check dfg tids stage prior_stages =
    let max_tbls stage =
      let res = CL.length (src_tids_of_stage stage) <= stage_def.smax_tbls in
      if res <> true then !dprint_endline "[stage_check] fail: max_tbls";
      res
    in
    let max_regs stage =
      let res =
        CL.length (CL.map regs_of_tg stage.s_tgs |> unique_list_of)
        <= stage_def.smax_regs
      in
      if res <> true then !dprint_endline "[stage_check] fail: max_regs";
      res
    in
    let max_hashers stage =
      let res =
        CL.length (CL.map hashers_of_tg stage.s_tgs |> unique_list_of)
        <= stage_def.smax_hashers
      in
      if res <> true then !dprint_endline "[stage_check] fail: max_hashers";
      res
    in
    max_tbls stage
    && max_regs stage
    && max_hashers stage
    (* When choosing the stage, we have to 
      make sure that dataflow constraints 
      are also satisfied. *)
    && dataflow_multi_check dfg tids prior_stages
  ;;

  (* check if the entire pipe meets constraints... 
  this is just here for symmetry -- there currently are no 
  pipeline-wide constraints *)
  let pipe_check pipe =
    let _ = pipe in
    true
  ;;
end

module Placement = struct
  (* This module traverses a dataflow graph of the program in 
    topological order, placing each table node and all of its 
    action and ALU objects into the first feasible stage and table. *)
  (* Basic algorithm: 
      The basic algorithm is to try and place each atomic table (in a toplogical ordering) into the first 
      multi-operation table such that no table, stage, or dataflow constraints are violated. 
      Sometimes, it may not be possible to place a table until tables that appear 
      after it in topological ordering. For those cases, the agorithm iterates until 
      all tables are placed. 
    Optimizations: 
      The general optimization is to check as many constraints as possible before attempting to 
      place a table into a stage or table group. Most importantly, check the data flow constraints 
      before attempting to place a table into a stage. This is because placing a table into a 
      stage, then checking to see if the placement is valid, is expensive. Checking the 
      dataflow constraints is cheap. Further, for complex programs, it is much 
      more common for placement to fail because of dataflow constraints than other constraints. 
  *)

  type ctx =
    { iteration : int
    ; prog : dagProg
    ; unplaced : Cid.t list (* from dp_instr_dict *)
    ; placed : (Cid.t * int) list (* table / reg id : stage # *)
    }

  let ctx_new dagProg =
    { iteration = 0; prog = dagProg; unplaced = ids_of_type is_tbl dagProg.dp_instr_dict; placed = [] }
  ;;

  let ctx_summary ctx =
    sprintf
      "[ctx_summary] iteration: %i placed: %i unplaced: %i"
      ctx.iteration
      (CL.length ctx.placed)
      (CL.length ctx.unplaced)
  ;;

  let ctx_note_placement stagenum ctx cid =
    { ctx with
      placed = (cid, stagenum) :: ctx.placed
    ; unplaced = list_remove ctx.unplaced cid
    }
  ;;

  let ctx_note_placements stagenum ctx cids =
    CL.fold_left (ctx_note_placement stagenum) ctx cids
  ;;

  (* try to place tid into tg of stage. *)
  let place_in_group (dagProg, rebuilt_stage, tids) tg =
    match tg.g_solitary with
      (* we can't add anything to a solitary table group *)
      true -> 
      (
        !dprint_endline ("[place_in_group] skipping solitary group "^(str_of_tid (tid_of_tg tg)));
        dagProg, add_tg rebuilt_stage tg, tids
      )
      | false -> 
      (
        match tids with
        (* there's nothing to place. return this stage with the current tablegroup. *)
        | [] -> 
        (
          dagProg, add_tg rebuilt_stage tg, tids
        )
        | _ -> 
        (
          (* 1. merge all tids into table group *)
          let new_tg = merge_set_into_group dagProg tg tids in
          !dprint_endline
            ("[place_in_group] proposed table group: " ^ summary_of_group new_tg);
          !dprint_endline "[place_in_group] checking table group constraints.";
          match Constraints.group_check dagProg new_tg with
          (* constraint check passes -- remove all the now placed source objects, 
              add updated tg to stage, return updated cid_decls. *)
          | true ->
          (
            !dprint_endline "[place_in_group] constraint check passed";
            let new_cid_decls = remove_decls dagProg.dp_instr_dict new_tg.g_src in
            {dagProg with dp_instr_dict = new_cid_decls}, add_tg rebuilt_stage new_tg, []
          )
          (* constraint check fails -- return original versions *)
          | false ->
          (
            !dprint_endline "[place_in_group] constraint check FAILED";
            dagProg, add_tg rebuilt_stage tg, tids
          )
        )
      )

  ;;

  let create_new_tg stage_num rules acns alus src_objs solitary = 
    let tbl_id = Cid.create_ids [Id.to_id ("merged_tbl", tg_next ())] in
    let tbl = Table (tbl_id, rules, Some stage_num) in
    { g_tbl = tbl; g_acns = acns; g_alus = alus; g_src = src_objs; g_solitary = solitary }
  ;;

  (* place a table that is too complex to be merged with any other 
     tables into its own "solitary" group. *)
  let place_in_solitary_group dagProg stage tids = 
    (* get the rules, actions, and ALUs of the solitary table. *)
    let tbl_rules, tbl_acns, tbl_alus, all_tbl_objs =
      match (Cid.lookup dagProg.dp_instr_dict (CL.hd tids)) with
      | Table(tid, rules, _) -> (
        rules, 
        acns_of_tid dagProg.dp_instr_dict tid, 
        alus_of_tid dagProg.dp_instr_dict tid,
        objs_of_tid dagProg.dp_instr_dict tid
      )
      | _ -> error "[placement_in_solitary_group] unexpected input (tids). Expected a single table."
    in 
    (* create the new table group *)
    let new_tg = create_new_tg 
      stage.s_num 
      tbl_rules 
      tbl_acns 
      tbl_alus 
      all_tbl_objs
      true
    in 
    !dprint_endline (
      "[place_in_solitary_group] placing table "
      ^(str_of_tid (CL.hd tids))
      ^" into solitary group "
      ^(str_of_tid (tid_of_tg new_tg))
    );
    (* remove the placed declarations from the dagProg, return the stage 
       with the new table group added to it. *)
    { dagProg with dp_instr_dict = 
        remove_decls dagProg.dp_instr_dict new_tg.g_src
    }, add_tg stage new_tg
  ;;

  let num_actions dagProg tid = 
    acns_of_tid dagProg.dp_instr_dict tid |> CL.length
  ;;

  let place_in_new_group dagProg stage tids =
    let new_tg = CL.fold_left 
      (fun cur_group tid -> merge_into_group dagProg cur_group tid)
      (empty_tg stage.s_num)
      tids
    in 
    let new_cid_decls = remove_decls dagProg.dp_instr_dict new_tg.g_src in
    {dagProg with dp_instr_dict = new_cid_decls}, add_tg stage new_tg
  ;;

  (* Try to place tids into stage. 
    Return: 
            updated context
            Some update cid_decls and stage, 
            or None if it fails.  *)

  let impossible_stage_placements_skipped = ref 0
  let failed_stage_placements = ref 0
  (* try placing into one of the existing groups in a 
     stage. return rebuilt stage and updated dagProg. *)
  let place_in_one_group dagProg stage tids = 
    let dagProg, stage, unplaced_tids =
      CL.fold_left
        place_in_group
        (dagProg, empty_stage stage.s_num, tids)
        stage.s_tgs
    in
    dagProg, stage, unplaced_tids
  ;;

  let place_in_stage
      (ctx : ctx)
      dfg
      dagProg
      stage
      prior_stages
      (tids : oid list)
      : ctx * (dagProg * stage) option
    =
    if (CL.length tids = 0)
    (* no tids to place! *)
    then (ctx, Some(dagProg, stage))
    else (
      (* optimization: before actually trying to place the table into any of the groups in this 
         stage, make sure that placement into this stage would not violate any dataflow constraints. *)
      match Constraints.dataflow_multi_check dfg tids prior_stages with
      | false ->
        !dprint_endline
          (sprintf
             "[place_in_stage] dataflow constraints VIOLATED for {%s} in stage \
              %i (skipping)"
             (str_of_tids tids)
             stage.s_num);
        impossible_stage_placements_skipped
          := !impossible_stage_placements_skipped + 1;
        failed_stage_placements := !failed_stage_placements + 1;
        ctx, None
      (* dataflow says we can place in this stage -- so go ahead and try it. *)
      | true ->
        (* for now, very large tables cannot be placed into a group 
          with other tables because the compiler takes too long to do 
          the merge. We detect large tables and put them into 
          their own group. We detect large merged tables and 
          don't try to add anything else into them. *) 
        !dprint_endline
          (sprintf
             "[place_in_stage] dataflow constraints SATISFIED for {%s} in stage \
              %i (placing)"
             (str_of_tids tids)
             stage.s_num);
        (* try placing the table into each group in this stage, one at a time. 
           or, if the table is large, place it in a solitary group. *)
        (* if this is a large table from a match statement, place it in a 
           solitary table group. Else, try placing it into an existing table. *)
        if ((num_actions dagProg (CL.hd tids)) > 50)
        then (
          let new_dagprog, updated_stage = place_in_solitary_group dagProg stage tids in 
          let ctx = ctx_note_placements stage.s_num ctx tids in 
          ctx, Some (new_dagprog, updated_stage)
        )
        else (
          let new_dagProg, new_stage, unplaced_tids = 
            place_in_one_group dagProg stage tids
          in 
          (* if that failed, place the table into a new stage. *)      
          let new_dagProg, new_stage =
            match unplaced_tids with 
            (* we have placed all the tids that we wanted to -- success. *)
            | [] -> new_dagProg, new_stage
            (* could not place into any current group -- try a new group. *)
            (* We assume this always succeeds, but that's actually not true, if the 
             set of tables we're trying to place into a group are too complex for a group. That is currently an 
             un-compilable program. *)
            | _ ->
              let dagProg, new_stage = place_in_new_group dagProg stage tids in
              dagProg, new_stage
          in
          (* check the stage-level constraints. *)
          (match Constraints.stage_check dfg tids new_stage prior_stages with
          | true ->
            !dprint_endline "[place_in_stage] stage constraints satisfied";
            (* if placement succeeded, note it in the context *)
            let new_ctx = ctx_note_placements stage.s_num ctx tids in
            !dprint_endline
              ("[place_in_stage] placement of { "
              ^ str_of_tids tids
              ^ " } successful. "
              ^ ctx_summary new_ctx);
            new_ctx, Some (new_dagProg, new_stage)
          | false ->
            failed_stage_placements := !failed_stage_placements + 1;
            !dprint_endline "[place_in_stage] stage constraints violated";
            ctx, None)
        )
    )


  ;;

  (* try to put tids first stage. Recurse on tail stages if fail. *)
  let rec place_in_stages (ctx : ctx) dfg dagProg stages prior_stages tids
      : ctx * (dagProg * stage list) option
    =
    match stages with
    | [] -> ctx, None (* ran out of stages. *)
    | stage :: remaining_stages ->
      (* try placing in first stage. *)
      let placement_result =
        place_in_stage ctx dfg dagProg stage prior_stages tids
      in
      (match placement_result with
      (* fail -- move stage to attempted and try next. *)
      | ctx, None ->
        place_in_stages
          ctx
          dfg
          dagProg
          remaining_stages
          (prior_stages @ [stage])
          tids
      (* success -- replace the stage with the new stage and prepend prior stages *)
      | updated_ctx, Some (new_dagProg, new_stage) ->
        let num_stages =
          CL.length (prior_stages @ [new_stage] @ remaining_stages)
        in
        !dprint_endline
          (sprintf
             "[place_in_stages] placed table %s in stage %i/%i"
             (str_of_tids tids)
             (CL.length prior_stages)
             num_stages);
        ( updated_ctx
        , Some (new_dagProg, prior_stages @ [new_stage] @ remaining_stages) ))
  ;;

  (* extend the pipeline with a new stage and place into that stage *)
  let place_in_new_stage ctx dfg dagProg pipe tids
      : ctx * (dagProg * stage) option
    =
    let new_stage_num = CL.length pipe.p_stages in
    !dprint_endline
      (sprintf
         "[place_in_new_stage] placed table %s in NEW stage %i"
         (str_of_tids tids)
         new_stage_num);
    place_in_stage
      ctx
      dfg
      dagProg
      (empty_stage new_stage_num)
      pipe.p_stages
      tids
  ;;

  (* place tids into a single group in the pipe, extending the pipe if needed. 
     tids is either a single table, or a set of tables that all use a common 
     register, plus that register. *)
  let place_in_pipe
      dfg
      (tids : Cid.t list)
      ((ctx : ctx), (dagProg : dagProg), (p : pipe))
      : ctx * dagProg * pipe
    =

    !dprint_endline
      ("[place_in_pipe] starting placement for: " ^ str_of_tids tids);
    !dprint_endline
      ("[place_in_pipe] stages in current pipe: "
      ^ (CL.length p.p_stages |> string_of_int));
    match p.p_stages with
    (* pipe is empty, add first stage. *)
    | [] -> (
      (* edge case can happen here: we try to place an instruction in the 
         first stage, but that instruction can't be placed yet. *)
      let updated_ctx, opt_res = place_in_new_stage ctx dfg dagProg p tids in
      match opt_res with 
      | Some (new_dagProg, fst_stage) -> 
        updated_ctx, new_dagProg, add_stage p fst_stage
      | None -> ctx, dagProg, p
    )
    (* pipe has stages. try to place into one of them. If that fails, append a new stage. *)
    | stages ->
      (match place_in_stages ctx dfg dagProg stages [] tids with
      (* placement in some existing stage succeeded. Replace the stages in the pipe. *)
      | updated_ctx, Some (new_dagProg, new_stages) ->
        let new_p = { p with p_stages = new_stages } in
        !dprint_endline
          ("[place_in_pipe] finished placement for: " ^ str_of_tids tids);
        !dprint_endline
          ("[place_in_pipe] stages in updated pipe: "
          ^ (CL.length new_p.p_stages |> string_of_int));
        updated_ctx, new_dagProg, new_p
      (* placement in an existing stage failed. 
              - try placing in a new last stage. 
              - if this fails, we cannot place the table in this round due to dataflow dependencies. *)
      | _, None ->
        !dprint_endline
          ("[place_in_pipe] placement in existing stages failed. trying a new \
            stage for: "
          ^ str_of_tids tids);
        (match place_in_new_stage ctx dfg dagProg p tids with
        | updated_ctx, Some (new_dagProg, new_last_stage) ->
          updated_ctx, new_dagProg, add_stage p new_last_stage
        | _, None ->
          !dprint_endline
            ("[place_in_pipe] placement in existing or new stages failed. \
              leaving table for next round: "
            ^ str_of_tids tids);
          ctx, dagProg, p))
  ;;

  (* place a table or register node into a single table group into the earliest 
    feasible stage of the pipeline. If no stage is feasible, try adding a new stage. 
    If that is not feasible either, return the unmodified pipeline.
      - if the node is a table that doesn't use a register, place it immediately. 
      - if the node is a table that uses a register, skip placing it. 
      - if the node is a register, place all the tables that use it. *)
  let place_node_in_pipe
      dfg
      (oid : Cid.t)
      ((ctx : ctx), (dagProg:dagProg), (p : pipe))
      : ctx * dagProg * pipe
    =
    match Cid.lookup_opt dagProg.dp_instr_dict oid with
    (* the object does not exist -- this can happen if a table was already placed 
       in this round (e.g., because it shares a register with another table) *)
    | None -> ctx, dagProg, p
    (* case: place table *)
    | Some (Table _) ->
      (match rids_of_tid dagProg.dp_instr_dict oid with
      (* the table does not use any registers, place it *)
      | [] -> place_in_pipe dfg [oid] (ctx, dagProg, p)
      (* the table uses a register, skip placing it for now *)
      | _ -> ctx, dagProg, p)
    (* case: place register + associated tables *)
    | Some (RegVec _) ->
      place_in_pipe dfg (tids_of_rid dagProg.dp_instr_dict oid) (ctx, dagProg, p)
    | Some _ ->
      error
        ("[place_node_in_pipe] attempting to place object that is not a table \
          or register..."
        ^ str_of_tid oid)
  ;;

  let all_placed_so_far cid_decls oid all_previous_placed =
    (not (Cid.exists cid_decls oid)) && all_previous_placed
  ;;

  let placement_complete dfg cid_decls =
    G.fold_vertex (all_placed_so_far cid_decls) dfg true
  ;;

  let unplaced_nodes dfg cid_decls pipe =
    let all_nodes = G.fold_vertex (fun v all_nodes -> v :: all_nodes) dfg [] in
    let placed_tables =
      src_of_pipe pipe |> CL.filter is_table |> CL.map id_of_decl
    in
    let unplaced_registers =
      decls_of_type is_reg cid_decls |> CL.map id_of_decl
    in
    let unplaced_nodes = list_sub all_nodes placed_tables in
    let unplaced_tables = list_sub unplaced_nodes unplaced_registers in
    !dprint_endline ("[unplaced_nodes] all_nodes: " ^ str_of_tids all_nodes);
    !dprint_endline
      ("[unplaced_nodes] placed_tables: " ^ str_of_tids placed_tables);
    !dprint_endline
      ("[unplaced_nodes] unplaced_registers: " ^ str_of_tids unplaced_registers);
    !dprint_endline
      ("[unplaced_nodes] unplaced_nodes: " ^ str_of_tids unplaced_nodes);
    !dprint_endline
      ("[unplaced_nodes] unplaced_tables: " ^ str_of_tids unplaced_tables);
    unplaced_tables
  ;;

  (* The core layout function. In each pass, the layout function 
    tries to place tables and registers from the dfg, one at a time, 
    by traversing dfg topologically.  
    There are multiple passes because some programs cannot be 
    laid out in a single traversal of the DFG. This happens 
    because tables that use the same register must be placed 
    in the same stage. For example, consider this example: 
            (a, b)
            /    \
           /      \
          c       e
          |      /|
          |  FOO  |
          | /     |
          d ----  f 

    Suppose the first pass visits e then c. We cannot place e 
    because we must place FOO, and so also d. But we cannot 
    place d because c has not been placed. So, the algorithm 
    will skip e for now, moving on to c, and it will place 
    e in the next iteration when it is safe to place e, FOO, and d. 
  *)
  let rec layout_rec (ctx : ctx) (dagProg:dagProg) dfg pipe previously_unplaced_nodes
      : pipe
    =
    let ctx = { ctx with iteration = ctx.iteration + 1 } in
    !dprint_endline ("[layout_rec] (start) " ^ ctx_summary ctx);
    !dprint_endline
      ("[layout_rec] (start) failed_stage_placements: "
      ^ string_of_int !failed_stage_placements);
    !dprint_endline
      ("[layout_rec] (start) impossible_stage_placements_skipped: "
      ^ string_of_int !impossible_stage_placements_skipped);
    let ctx, unplaced_dag_prog, updated_pipe =
      Topo.fold (place_node_in_pipe dfg) dfg (ctx, dagProg, pipe)
    in
    let res =
      match unplaced_nodes dfg unplaced_dag_prog.dp_instr_dict updated_pipe with
      (* placement of all nodes in dfg is complete. The remaining
    objects in cid_decls are global -- build the final pipe. *)
      | [] ->
        !dprint_endline "[layout_rec] finished placing nodes in this pass.";
        { updated_pipe with p_globals = CL.split unplaced_dag_prog.dp_instr_dict |> snd }
      (* placement is not complete, we need to recurse on the 
    pipe that we have built and the remaining cid_decls *)
      | nodes ->
        !dprint_endline
          ("[layout_rec] did not place all nodes in this pass. Remaining nodes:"
          ^ str_of_tids nodes);
        (match list_eq previously_unplaced_nodes nodes with
        | true ->
          error
            "[layout_rec] the nodes that could not be placed in the previous \
             pass are exactly the same as the nodes that code not be placed in \
             the current pass."
        | false -> layout_rec ctx {dagProg with dp_instr_dict = unplaced_dag_prog.dp_instr_dict} dfg updated_pipe nodes)
    in
    !dprint_endline ("[layout_rec] (end) " ^ ctx_summary ctx);
    !dprint_endline
      ("[layout_rec] (end) failed_stage_placements: "
      ^ string_of_int !failed_stage_placements);
    !dprint_endline
      ("[layout_rec] (end) impossible_stage_placements_skipped: "
      ^ string_of_int !impossible_stage_placements_skipped);
    res
  ;;

  (* convenience wrapper for recursive layout function. *)
  (* using the definitions in cid_decls, 
      generate a pipe where: 
      - every table and register node in dfg is 
      placed in a stage, along with all of the actions and alus that 
      the tables use. 
      - the other objects in cid_decls are placed as "globals" in the 
      pipe. *)
  let layout (dfprog:DFSyntax.dagProg) dfg : pipe =
    layout_rec (ctx_new dfprog) dfprog dfg empty_pipe []
  ;;
end

(**** Convert to table sequence program ****)
(* This can probably be removed. We can print the pipeline directly 
  to P4, all we really need is the call sequence. *)
let rec callseq_of_tids tids : tblStmt =
  match tids with
  | [] -> Noop
  | [tid] -> CallTable tid
  | tid :: tids -> Seq (CallTable tid, callseq_of_tids tids)
;;

let tblseq_of_pipe pipe dagProg : tblSeq =
  { tsname = Cid.Id dagProg.dp_name
  ; tsstmt = tbls_of_pipe pipe |> CL.map id_of_decl |> callseq_of_tids
  }
;;

let to_tblseqprog pipe dagProg : tblSeqProg =
  { tspname = Consts.progId
  ; tspinputs = dagProg.dp_inputs
  ; tspglobals = Consts.globalArgs
  ; tspglobal_widths = Consts.globalArgWidths
  ; tspdecls = objs_of_pipe pipe
  ; tsptblseq = tblseq_of_pipe pipe dagProg
  }
;;

(* temporarily copy debugging stuff from LogIr *)
module DotConfig = struct
  include DFSyntax.G (* use the graph module from above *)

  let graph_attributes _ = []
  let edge_attributes (_, _, _) = []
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes _ = [`Shape `Box]
  let vertex_name v = P4tPrint.str_of_private_oid v
  let default_vertex_attributes _ = []
end

module Dot = Graph.Graphviz.Dot (DotConfig)

let log_lir_dag fn dag =
  let full_fn = !BackendLogging.irLogDir ^ "/" ^ fn ^ ".dot" in
  Dot.output_graph (open_out_bin full_fn) dag
;;

(* transform a dataflow graph, where each node represents a single-instruction 
table, into a dataflow graph that contains nodes for both tables and register arrays. 
a table has an edge to a register array iff it uses that register array *)
let to_tbl_reg_dfg cid_decls dfg =
  let add_edge src_id g dst_id = G.add_edge g src_id dst_id in
  (* for each table, get all registers. Add edges from tbl to register *)
  let add_tbl_regs (tbl_id : oid) (g : G.t) : G.t =
    (*    G.add_edge g tbl_id tbl_id *)
    let reg_ids = rids_of_tid cid_decls tbl_id in
    CL.fold_left (add_edge tbl_id) g reg_ids
  in
  let tbl_reg_dfg = G.fold_vertex add_tbl_regs dfg dfg in
  (* print out the table dataflow graph *)
  log_lir_dag "pipeline_full_dataflow" dfg;
  log_lir_dag "pipeline_tbl_reg_dataflow" tbl_reg_dfg;
  tbl_reg_dfg
;;

(* left off here. If two salus in the same table are identical, replace them with one salu. 
  0. define "identical"
  1. create a new salu, which is just a copy of one of them. 
  2. replace the calls to the original salus with the calls to the new salu.
 *)

module OperationDedup = struct
  (**** New 10/11/21 -- deduplicate structurally equivalent 
    operation decls (currently only salu and hash ops) ****)

  (** Structural equality tests of operation objects (LLSyntax) **)
  let opt_eq eq opt1 opt2 =
    match opt1, opt2 with
    | Some v1, Some v2 -> eq v1 v2
    | None, None -> true
    | _ -> false
  ;;

  let list_eq eq l1 l2 =
    match CL.length l1 = CL.length l2 with
    | false -> false
    | true ->
      CL.combine l1 l2
      |> CL.map (fun (a, b) -> eq a b)
      |> CL.for_all (fun x -> x)
  ;;

  let int_eq i1 i2 = i1 = i2
  let id_eq = Cid.equal

  let ids_eq xs ys = 
    if (CL.length xs = CL.length ys)
    then (CL.fold_left2 (fun prev x y -> prev & (id_eq x y)) true xs ys)
    else (false)
  ;;
  let const_eq = Integer.equal
  let memCell_eq _ _ = true

  let oper_eq o1 o2 =
    match o1, o2 with
    | Const c1, Const c2 -> const_eq c1 c2
    | Const _, _ -> false
    | Meta m1, Meta m2 -> id_eq m1 m2
    | Meta _, _ -> false
    | MetaSlice(l1, h1, m1), MetaSlice(l2, h2, m2) -> 
      (id_eq m1 m2) & (l1 = l2) & (h1 = h2)
    | MetaSlice(_), _ -> false
    | RegVar r1, RegVar r2 -> memCell_eq r1 r2
    | RegVar _, _ -> false
    | NoOper, NoOper -> true
    | NoOper, _ -> false
  ;;

  let opers_eq o1s o2s = 
    if (CL.length o1s = CL.length o2s)
    then (CL.fold_left2 (fun prev x y -> prev & (oper_eq x y)) true o1s o2s)
    else (false)
  ;;

  let binOp_eq b1 b2 = b1 = b2
  let cmpOp_eq c1 c2 = c1 = c2

  let sArithExp_eq e1 e2 =
    match e1, e2 with
    | SVar o1, SVar o2 -> oper_eq o1 o2
    | SVar _, _ -> false
    | SBinOp (op1, a1, b1), SBinOp (op2, a2, b2) ->
      binOp_eq op1 op2 && oper_eq a1 a2 && oper_eq b1 b2
    | SBinOp _, _ -> false
  ;;

  let sCompExpr_eq e1 e2 =
    match e1, e2 with
    | (a1, op1, b1, cmp1, c1), (a2, op2, b2, cmp2, c2) ->
      oper_eq a1 a2
      && binOp_eq op1 op2
      && oper_eq b1 b2
      && cmpOp_eq cmp1 cmp2
      && oper_eq c1 c2
  ;;

  let sPredExpr_eq e1 e2 =
    match e1, e2 with
    | Comp e1, Comp e2 -> sCompExpr_eq e1 e2
    | Comp _, _ -> false
    | Neg e1, Neg e2 -> sCompExpr_eq e1 e2
    | Neg _, _ -> false
  ;;

  let sExpr_eq e1 e2 =
    match e1, e2 with
    | MemExpr (sp1, se1), MemExpr (sp2, se2) ->
      opt_eq sPredExpr_eq sp1 sp2 && sArithExp_eq se1 se2
    | MemExpr _, _ -> false
    | RetExpr (sp1, se1), RetExpr (sp2, se2) ->
      opt_eq sPredExpr_eq sp1 sp2 && sArithExp_eq se1 se2
    | RetExpr _, _ -> false
  ;;

  let sInstr_eq (iv1 : sInstr) (iv2 : sInstr) =
    id_eq iv1.sRid iv2.sRid
    && int_eq iv1.sWid iv2.sWid
    && list_eq sExpr_eq iv1.sExprs iv2.sExprs
    && opt_eq id_eq iv1.sOut iv2.sOut
    && oper_eq iv1.sIdx iv2.sIdx
  ;;

  let expr_eq e1 e2 = 
    match (e1, e2) with 
      | Oper o1, Oper o2 -> oper_eq o1 o2
      | BinOp(b1, o1s), BinOp(b2, o2s) -> 
        (binOp_eq b1 b2) & (opers_eq o1s o2s)
      | HashOp(m1s), HashOp (m2s) -> (ids_eq m1s m2s)
      | _, _ -> false
  ;;


  let instr_eq i1 i2 = 
    match i1, i2 with 
    | (IAssign(l1, e1), IAssign(l2, e2)) ->
      (id_eq l1 l2) & (expr_eq e1 e2)
    | IValidate l1, IValidate l2 -> 
      id_eq l1 l2
    | IInvalidate l1, IInvalidate l2 ->
      id_eq l1 l2
    | _, _ -> false
  ;;

  let instrVec_eq (iv1 : instrVec) (iv2 : instrVec) = 
    if ((CL.length iv1) = (CL.length iv2))
    then (
      let folder prev i1 i2 = 
        prev & (instr_eq i1 i2) 
      in 
      CL.fold_left2 folder true iv1 iv2
    )
    else (false)
  ;;  

  (* We only consider equality of instructions and actions for now. 
     Note that action comparison is shallow based on ids *)
  let decl_eq d1 d2 =
    match d1, d2 with
    | InstrVec (_, iv1), InstrVec (_, iv2) -> instrVec_eq iv1 iv2
    | SInstrVec (_, iv1), SInstrVec (_, iv2) -> sInstr_eq iv1 iv2
    | Hasher (_, ai1, bi1, out1, opers1), Hasher (_, ai2, bi2, out2, opers2) ->
      int_eq ai1 ai2
      && int_eq bi1 bi2
      && id_eq out1 out2
      && list_eq oper_eq opers1 opers2
    | Action (_, instr_ids1, succ_ids1), Action (_, instr_ids2, succ_ids2) -> 
      (* note: this is a shallow comparison, so you need to *)
      (ids_eq instr_ids1 instr_ids2) & (ids_eq succ_ids1 succ_ids2) 
    | Hasher _, _ -> false
    | _ -> false
  ;;

  let decl_cmp d1 d2 =
    match decl_eq d1 d2 with
    | true -> 0
    | false -> 1
  ;;

  (* is d2 a duplicate of d1? *)
  let decl_is_dup d1 d2 =
    let d1_id = id_of_decl d1 in 
    let d2_id = id_of_decl d2 in
    match decl_eq d1 d2 with
    | true -> (
      !dprint_endline (sprintf "%s EQUALS %s" (str_of_tid d1_id) (str_of_tid d2_id));
      let result = not (id_eq (id_of_decl d1) (id_of_decl d2)) in 
      (* !dprint_endline ("result: "^(string_of_bool result)); *)
      result
    )
    | false -> (
      (* !dprint_endline (sprintf "%s NEQ %s" (str_of_tid d1_id) (str_of_tid d2_id)); *)
      false
    )
  ;;

  (* Main function: deduplicate a list of decls. 
     eliminate any structurally equivalent decls, 
     replace references to deleted decls with 
     references to the structurally equivalent
     decl that remains. 
     This is meant to be used on the decls that 
     are mapped to the same table in a pipeline. *)
  let rec dedup_decls ds =
    match ds with
    | [] -> []
    | d :: ds ->
      let id_of_d = id_of_decl d in
      !dprint_endline ("removing duplicates of " ^ (str_of_tid id_of_d));
      (* find ids of duplicates of d in ds *)
      let dup_ds = CL.filter (decl_is_dup d) ds in
      let aliases_of_d = CL.map id_of_decl dup_ds in
      !dprint_endline
        ("decl id: "^(str_of_tid id_of_d)^" number of aliases: " ^ (CL.length aliases_of_d |> string_of_int));
      (* get ds without any duplicates of the current element *)
      let new_ds = CL.filter (fun d_ds -> not (decl_is_dup d d_ds)) ds in
      !dprint_endline ("original ds len: "^(string_of_int (CL.length ds))^" new ds len:"^(string_of_int (CL.length new_ds)));
      (* replace alias with d's id *)
      let replace_alias ds alias =
        !dprint_endline
          ("replacing alias: "
          ^ str_of_tid alias
          ^ " with "
          ^ str_of_tid id_of_d);
        CL.map (fun d -> replace_oid_in_decl d alias id_of_d) ds
      in
      let new_ds = CL.fold_left replace_alias new_ds aliases_of_d in
      (* keep d, dedup the new tail of other elements. *)
      d :: dedup_decls new_ds
  ;;
end

let dedup_slprog tsprog =
  !dprint_endline "---- deduplication started -----";
  let r =
    { tsprog with tspdecls = 
      tsprog.tspdecls 
      |> OperationDedup.dedup_decls
      |> OperationDedup.dedup_decls
    } (* two passes because we must 
    dedup instructions before 
    we can dedup actions *)
  in
  let n_before = CL.length tsprog.tspdecls in
  let n_after = CL.length r.tspdecls in
  layout_report
    ("deduplication eliminated "
    ^ (n_before - n_after |> string_of_int)
    ^ " operations");
  !dprint_endline
    ("deduplication eliminated "
    ^ (n_before - n_after |> string_of_int)
    ^ " operations");
  !dprint_endline "---- deduplication finished -----";
  r
;;

let do_passes df_prog =
  (* print_endline "---- starting pipesyntax passes ----"; *)
  let cid_decls, _, dfg = DFSyntax.to_tuple df_prog in
  let dfg_with_regs = to_tbl_reg_dfg cid_decls dfg in
  let pipe = Placement.layout df_prog dfg_with_regs in
  (* todo:  
      - move translation to SLSyntax and passes over it 
        to the SLSyntax file. Messy because of cyclic 
        dependency rules. *)
  let straightline_prog = to_tblseqprog pipe df_prog in
  let deduped_prog = dedup_slprog straightline_prog in
  pipe, deduped_prog
;;
