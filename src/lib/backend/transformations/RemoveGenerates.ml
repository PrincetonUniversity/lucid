(* 

  After this pass, generate statements only need to set multicast ID and egress port. 


    - All event parameters are set in this pass. 
    - event count is set (really: mc_group_a)
    - After this pass, to translate a generate statement, you must: 
      1. recirc generate: nothing
      2. port generate: set egress_port and port_evid
      3. ports generate: create multicast group and set ports_mcid

  1. walk the program and collect a list of all possible sequences of events generated. Will be used later. 
  2. eliminate generate statements. 
    1. at each generate, set all parameter variables. for each parameter variable foo set, check if it is later read. If so, set orig_foo = foo before the generate and replace foo with orig_foo everywhere after the generate. 
    2. at each generate(_, x()), set flag ev_x_generated.
    3. if generate type is: 
      - generate self: increment self_generate_ct
      - generate port: set egress port variable
      - generate ports: 
        - if flood: set ports_mcid = 512 + flood port
        - if group value: create a multicast group and set ports_mcid
  *)

open CoreSyntax
open TofinoCore
open CoreCfg
exception Error of string
let error s = raise (Error s)

(* find all the possible sequences of events that get generated *)
let rec find_ev_gen_seqs statement =
  match statement.s with 
  | SGen(_, ev_exp) -> (
    match ev_exp.e with 
    | ECall(ev_cid, _) -> (
      [[Cid.to_id ev_cid]]
    )
    | _ -> error "[find_ev_gen_seqs] event should be a call by this point"
  )
  | SIf(_, s1, s2) ->
    (find_ev_gen_seqs s1)@(find_ev_gen_seqs s2)
  | SMatch(_, branches) -> 
    List.fold_left (fun seqs (_, stmt) -> seqs@(find_ev_gen_seqs stmt)) [] branches 
  | SSeq(s1, s2) -> (
    let seqs_s1 = find_ev_gen_seqs s1 in
    let seqs_s2 = find_ev_gen_seqs s2 in
    let update_seqs seqs seq = 
      List.map (fun s -> s@seq) seqs
    in 

    List.fold_left 
      (fun new_seqs_s1 seq -> 
        new_seqs_s1@(update_seqs seqs_s1 seq)
      )
      []
      seqs_s2
    )
  (* no events in rest *)
  | _ -> [[]]
;;


let set_ev_gen_seqs tds =
  let main_handler = (main tds) in 
  let main_stmt = main_handler.main_body |> List.hd in
  let ev_gen_seqs = find_ev_gen_seqs main_stmt in
  let tds = update_main tds {main_handler with event_output = {main_handler.event_output with ev_gen_seqs}} in 
  tds
;;



(* make multicast groups for: 
   1) flood expressions
   2) self-generate recirculation cloning *)
let create_fixed_mcgroups tds config =
  let all_portnums = List.map (fun p -> p.num) (config.internal_ports@config.external_ports) in 
  let flood_groups = 
    let create_flood_group portnum =
      let mcid = config.mcid_port_flood_start + portnum in 
      let out_ports = MiscUtils.remove portnum all_portnums in
      dmcgroup GFlood mcid (List.map (fun p -> (p, 0)) out_ports)
    in 
    List.map create_flood_group all_portnums
  in
  let recirc_groups = 
    let create_recirc_group n_copies =
      (* (192, i) for i in range (1, n) *)
      let rids = MiscUtils.range 1 (1+n_copies) in
      let replicas = List.map (fun rid -> (config.recirc_port.num, rid)) rids in
      dmcgroup GRecirc n_copies replicas
    in
    List.map create_recirc_group (MiscUtils.range 1 (1 + CL.length (main tds).hdl_enum))
  in
  recirc_groups@flood_groups
;;


(*** generate elimination ***)


let rec writes_in tds stmt = 
  match stmt.s with 
  | SSeq(s1, s2) 
  | SIf(_, s1, s2) -> 
    (writes_in tds s1)@(writes_in tds s2)
  | SMatch(_, bs) -> List.map
    (fun (_, stmt) -> writes_in tds stmt)
    bs |> List.flatten
  | SGen(_, ev_exp) -> (
    match ev_exp.e with 
    | ECall(ev_cid, _) -> (
      let ev_param_ids = 
        List.assoc (Cid.to_id ev_cid) ((main tds).hdl_params)
        |> List.split |> fst
      in
      ev_param_ids
    )
    | _ -> error "[writes_in] event values not supported"
  )
  | _ -> []
;;
(* which parameters are read in stmt? *)
let reads_in param_ids stmt =
  let v = 
    object 
      inherit [_] s_iter as super
      val mutable read_param_ids = []
      method! visit_EVar _ cid =
        let var_id = Cid.to_id cid in 
        if (MiscUtils.contains param_ids var_id)
        then (read_param_ids <- (var_id)::read_param_ids)
      method get_read_params () = 
        read_param_ids
    end
  in
  v#visit_statement () stmt;
  (v#get_read_params ())
;;

let size_of_tint ty = 
  match ty.raw_ty with 
  | TInt(sz) -> sz
  | _ -> error "[size_of_tint] not a tint"
;;

(* generate statements that set the parameter 
   variables of evid to eargs *)
let set_event_params tds ev_cid arg_exps =
  let param_ids = List.assoc (Cid.to_id ev_cid) ((main tds).hdl_params) |> List.split |> fst in 
  let set_event_param (param_id, arg_exp) =
    (sassign param_id arg_exp)
  in
  List.map set_event_param (List.combine param_ids arg_exps)
;;

let set_event_generated_flag tds ev_cid = 
  let flag_var, flag_ty = List.assoc 
    (Cid.to_id ev_cid) 
    (main tds).event_output.ev_generated_flags
  in  
  sassign flag_var (vint 1 (size_of_tint flag_ty) |> value_to_exp)
;;

(* id = e + i; *)
let sassign_incr id ty e i =
  sassign id
  (op_sp
    Plus
    [
      e;
      ((vint (i) (size_of_tint ty)) |> value_to_exp)
    ]
    ty
    Span.default
  )
;;

let incr_recirc_mcid tds =
  let ct_var, ct_ty = (main tds).event_output.recirc_mcid_var in 
  sassign_incr ct_var ct_ty (var_sp (Cid.id ct_var) ct_ty Span.default) 1
;;

let set_port_evid m ev_cid = 
  let port_evid_var, port_evid_ty = m.event_output.port_evid_var in
  let evnum = List.assoc (Cid.to_id ev_cid) (m.hdl_enum) in 
  let e_evnum = (vint evnum (size_of_tint port_evid_ty)) |> value_to_exp in 
  let set_port_evid = (sassign port_evid_var e_evnum) in 
  set_port_evid
;;

let set_port_fields tds ev_cid eport =
  let m = (main tds) in 
  let port_var, _ = m.event_output.egress_port_var in
  [(sassign port_var eport); set_port_evid m ev_cid]
;;

(* set fields for generate_port, and possibly create user mc group *)
let set_ports_fields tds config prev_mcgroup_decls ev_cid eports =
  let m = (main tds) in 
  let mcid_var, mcid_ty = m.event_output.ports_mcid_var in 
  let port_evid_set = set_port_evid m ev_cid in
  match eports.e with 
    | EFlood(e_igr_port) -> 
      let mcid_set =  
        sassign_incr
          mcid_var mcid_ty
          e_igr_port
          config.mcid_port_flood_start
      in
      [], [port_evid_set; mcid_set]
    | EVal({v=VGroup(ports);}) -> (
      let mcid = config.mcid_user_groups_start + List.length (prev_mcgroup_decls) in
      let mcdecl = dmcgroup GUser mcid (List.map (fun p -> (p, 0)) ports) in 
      (* now, assign the mcid*)
      let mcid_set = 
        sassign_sp 
          mcid_var
          ((vint (mcid) (size_of_tint mcid_ty)) |> value_to_exp)
          Span.default
      in
      [mcdecl], [port_evid_set; mcid_set]
    )
    | _ -> error "[eliminate_generates.set_ports_fields] first arg of generate_ports must be a flood expression or group value. Group variables should be inlined by now."
;;




let orig param_id =
  Id.fresh ("orig_"^(fst param_id))
;;


(* replace any reads of param_ids
   with reads of the orig var *)
let rec replace_reads param_ids exp = 
  match exp.e with 
  | EVar(cid) -> 
    if (MiscUtils.contains param_ids (Cid.to_id cid))
    then {exp with e=EVar(Cid.id (orig (Cid.to_id cid)))}
    else exp
  | EOp(o, exps) -> 
    let exps = List.map (replace_reads param_ids) exps in
    {exp with e=EOp(o, exps)}
  | ECall(cid, exps) -> 
    let exps = List.map (replace_reads param_ids) exps in
    {exp with e=ECall(cid, exps)}
  | EHash(s, exps) -> 
    let exps = List.map (replace_reads param_ids) exps in
    {exp with e=EHash(s, exps)}
  | EFlood(exp) -> {exp with e=EFlood(replace_reads param_ids exp)}
  | EVal _ -> exp
;;

type elim_ctx = {
  writes_before : Id.t list; (* param vars changed before stmt *)
  reads_after : Id.t list; (* param vars read after stmt *)  
}

(* don't eliminate generates completely, but simplify them.

  1. for all generates, set parameters and make copies if necessary
  2. for recirc generates, increment counter
  after this:
  - all generates need to set the event active flag
  - port generates just need out port and port evid set
  - ports generates need groups created, port evid set, and port_mcid set
 *)
let reduce_generates tds = 
  let root_stmt = match (main tds).main_body with 
    | [root] -> root 
    | _ -> error "[generate_recirc_mc_groups] must be run before main is split into multiple stage statements"
  in
  let all_params = (main tds).hdl_params 
    |> List.split |> snd 
    |> List.flatten 
    |> List.split |> fst 
  in 
  let user_mcgroup_decls = ref [] in 

  let rec trav_stmts ctx stmt =
    match stmt.s with 
    | SSeq(s1, s2) -> (
      let before_s1 = ctx.writes_before in
      let after_s1  = ctx.reads_after @ (reads_in all_params s2) in 
      let before_s2 = ctx.writes_before @ (writes_in tds s1) in 
      let after_s2 = ctx.reads_after in

      let new_s1 = trav_stmts {
          writes_before=before_s1;
          reads_after = after_s1;
        }
        s1
      in
      let new_s2 = trav_stmts {
          writes_before = before_s2;
          reads_after = after_s2;
        }
        s2
      in
      {stmt with s=SSeq(new_s1, new_s2)}
    )
    | SIf(e, s1, s2) -> 
      {stmt with s=SIf(e, trav_stmts ctx s1, trav_stmts ctx s2)}
    | SMatch(es, branches) -> 
      let new_branches = List.map
        (fun (ps, stmt) -> (ps, trav_stmts ctx stmt))
        branches
      in
      {stmt with s=SMatch(es, new_branches)}
    | SGen(gty, ev_exp) -> (
      match ev_exp.e with 
      | ECall(ev_cid, args) -> (
        let args = List.map (replace_reads ctx.writes_before) args in 
        (* 1. for each parameter of ev_cid: 
            if the parameter is in reads_after, create a statement
            initializing (orig_var param_id)
           2. create a statement setting param[i] = arg[i] *)
        let orig_var_init_stmts = List.filter_map 
          (fun (param_id, param_ty) -> 
            if (MiscUtils.contains ctx.reads_after param_id)
            then (Some(slocal (orig param_id) param_ty (var_sp (Cid.id param_id) param_ty Span.default)))
            else (None)
          )
          (List.assoc (Cid.to_id ev_cid) (main tds).hdl_params)
        in
        let param_set_stmts = set_event_params tds ev_cid args in
        let incr_self_event_ctr = match gty with 
          | GSingle(None) -> [incr_recirc_mcid tds]
          | _ -> []
        in
        InterpHelpers.fold_stmts (orig_var_init_stmts@param_set_stmts@incr_self_event_ctr)

        (* let flag_stmts = [set_event_generated_flag tds ev_cid] in  *)
        (* there's also some special processing for each kind of generate *)
(*         let gty_stmts = match gty with 
          | GSingle(None) -> [incr_recirc_mcid tds]
          | GPort(eport) -> set_port_fields tds ev_cid eport
          | GMulti(eports) -> 
            let new_mcdecls, stmts = set_ports_fields tds config (!user_mcgroup_decls) ev_cid eports in
            user_mcgroup_decls := (!user_mcgroup_decls)@new_mcdecls;
            stmts
          | _ -> error "[eliminate_generates] unsupported generate type"
        in
        InterpHelpers.fold_stmts (orig_var_init_stmts@param_set_stmts@flag_stmts@gty_stmts) *)
      )
      | _ -> error "[eliminate_generates] event variables not supported yet."
    )
    | SNoop -> stmt
    | SUnit(e) -> {stmt with s=SUnit(replace_reads ctx.writes_before e)}
    | SLocal(id, ty, exp) -> {stmt with s=SLocal(id, ty, replace_reads ctx.writes_before exp)}
    | SAssign(id, exp) -> {stmt with s=SAssign(id, replace_reads ctx.writes_before exp)}
    | SPrintf(s, exps) -> {stmt with s=SPrintf(s, List.map (replace_reads ctx.writes_before) exps)}
    | SRet(Some(exp)) -> {stmt with s=SRet(Some(replace_reads ctx.writes_before exp))}
    | SRet(None) -> stmt
  in
  let new_root_stmt = trav_stmts {writes_before = []; reads_after = [];} root_stmt in 
  update_main tds {(main tds) with main_body=[new_root_stmt];}
;;

let eliminate_generates tds config =
  let root_stmt = match (main tds).main_body with 
    | [root] -> root 
    | _ -> error "[generate_recirc_mc_groups] must be run before main is split into multiple stage statements"
  in
  let all_params = (main tds).hdl_params 
    |> List.split |> snd 
    |> List.flatten 
    |> List.split |> fst 
  in 
  let user_mcgroup_decls = ref [] in 

  let rec trav_stmts ctx stmt =
    match stmt.s with 
    | SSeq(s1, s2) -> (
      let before_s1 = ctx.writes_before in
      let after_s1  = ctx.reads_after @ (reads_in all_params s2) in 
      let before_s2 = ctx.writes_before @ (writes_in tds s1) in 
      let after_s2 = ctx.reads_after in

      let new_s1 = trav_stmts {
          writes_before=before_s1;
          reads_after = after_s1;
        }
        s1
      in
      let new_s2 = trav_stmts {
          writes_before = before_s2;
          reads_after = after_s2;
        }
        s2
      in
      {stmt with s=SSeq(new_s1, new_s2)}
    )
    | SIf(e, s1, s2) -> 
      {stmt with s=SIf(e, trav_stmts ctx s1, trav_stmts ctx s2)}
    | SMatch(es, branches) -> 
      let new_branches = List.map
        (fun (ps, stmt) -> (ps, trav_stmts ctx stmt))
        branches
      in
      {stmt with s=SMatch(es, new_branches)}
    | SGen(gty, ev_exp) -> (
      match ev_exp.e with 
      | ECall(ev_cid, args) -> (
        let args = List.map (replace_reads ctx.writes_before) args in 
        (* 1. for each parameter of ev_cid: 
            if the parameter is in reads_after, create a statement
            initializing (orig_var param_id)
           2. create a statement setting param[i] = arg[i] *)
        let orig_var_init_stmts = List.filter_map 
          (fun (param_id, param_ty) -> 
            if (MiscUtils.contains ctx.reads_after param_id)
            then (Some(slocal (orig param_id) param_ty (var_sp (Cid.id param_id) param_ty Span.default)))
            else (None)
          )
          (List.assoc (Cid.to_id ev_cid) (main tds).hdl_params)
        in
        let param_set_stmts = set_event_params tds ev_cid args in
        let flag_stmts = [set_event_generated_flag tds ev_cid] in 
        (* there's also some special processing for each kind of generate *)
        let gty_stmts = match gty with 
          | GSingle(None) -> [incr_recirc_mcid tds]
          | GPort(eport) -> set_port_fields tds ev_cid eport
          | GMulti(eports) -> 
            let new_mcdecls, stmts = set_ports_fields tds config (!user_mcgroup_decls) ev_cid eports in
            user_mcgroup_decls := (!user_mcgroup_decls)@new_mcdecls;
            stmts
          | _ -> error "[eliminate_generates] unsupported generate type"
        in
        InterpHelpers.fold_stmts (orig_var_init_stmts@param_set_stmts@flag_stmts@gty_stmts)
      )
      | _ -> error "[eliminate_generates] event variables not supported yet."
    )
    | SNoop -> stmt
    | SUnit(e) -> {stmt with s=SUnit(replace_reads ctx.writes_before e)}
    | SLocal(id, ty, exp) -> {stmt with s=SLocal(id, ty, replace_reads ctx.writes_before exp)}
    | SAssign(id, exp) -> {stmt with s=SAssign(id, replace_reads ctx.writes_before exp)}
    | SPrintf(s, exps) -> {stmt with s=SPrintf(s, List.map (replace_reads ctx.writes_before) exps)}
    | SRet(Some(exp)) -> {stmt with s=SRet(Some(replace_reads ctx.writes_before exp))}
    | SRet(None) -> stmt
  in
  let new_root_stmt = trav_stmts {writes_before = []; reads_after = [];} root_stmt in 
  update_main tds {(main tds) with main_body=[new_root_stmt];}

;;


let eliminate tds config = 
  (* 1. find all the possible sequences of event generates *)
  let tds = set_ev_gen_seqs tds in 
  (* 2. create multicast groups for flood ports and recirc events *)
  let tds = tds@(create_fixed_mcgroups tds config) in 
  (* 3. eliminate generates *)
  let tds = eliminate_generates tds config in 
  tds ;;
