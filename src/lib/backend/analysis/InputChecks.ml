(* check that all event parameters are byte-aligned *)

open CoreSyntax

let fail_report str = Console.show_message str ANSITerminal.Red "Tofino Checker"

let report_err span msg : unit =
  Console.show_message_position span msg ANSITerminal.Red "Tofino Checker"
;;

let params_wid params =
  List.fold_left
    (fun tot_wid (_, pty) ->
      match pty.raw_ty with
      | TInt w -> tot_wid + w
      | TBool -> tot_wid + 1
      | _ -> error "[param_wids] event parameters must be ints or bools")
    0
    params
;;

let event_param_alignment ds =
  let pass = ref true in
  let v =
    object
      inherit [_] s_iter as super

      method! visit_decl ctx decl =
        super#visit_decl ctx decl;
        match decl.d with
        | DEvent (id, _, params) ->
          if params_wid params mod 8 <> 0
          then (
            report_err
              decl.dspan
              ("[Event Alignment Check] every event parameter must either \
                start or end on a byte boundary. The event "
              ^ fst id
              ^ "'s parameters are not byte-aligned.");
            pass := false)
        | _ -> ()
    end
  in
  v#visit_decls () ds;
  !pass
;;

let array_sizes ds : bool =
  let max_sblocks = 36 in
  let pass =
    List.fold_left
      (fun prev_pass dec ->
        match dec.d with
        | DGlobal
            ( id
            , { raw_ty = TName (ty_cid, sizes, true); _ }
            , { e = ECall (_, num_slots :: _) } ) ->
          (match Cid.names ty_cid |> List.hd with
           | "Array" ->
             let slot_sz = List.hd sizes in
             let num_slots = InterpHelpers.int_from_exp num_slots in
             let sblocks = CoreResources.sblocks_of_arr slot_sz num_slots in
             let pass = sblocks <= max_sblocks in
             if not pass
             then
               report_err
                 dec.dspan
                 ("[array sizes check] An Array created by the above statement \
                   ( internal Array name: "
                 ^ Id.to_string id
                 ^ ") is too large. An array must fit into 35 16KB blocks. \
                    This array requires "
                 ^ string_of_int (sblocks - 1));
             prev_pass && pass
           | "PairArray" ->
             let slot_sz = 2 * List.hd sizes in
             let num_slots = InterpHelpers.int_from_exp num_slots in
             let sblocks = CoreResources.sblocks_of_arr slot_sz num_slots in
             let pass = sblocks <= max_sblocks in
             if not pass
             then
               report_err
                 dec.dspan
                 ("[array sizes check] An Array created by the above statement \
                   ( internal Array name: "
                 ^ Id.to_string id
                 ^ ") is too large. An array must fit into 35 16KB blocks. \
                    This array requires "
                 ^ string_of_int (sblocks - 1));
             prev_pass && pass
           | _ -> prev_pass)
        | _ -> prev_pass)
      true
      ds
  in
  pass
;;

let port_tys ds =
  (* in the tofino, ingress and egress port variables are 9 bits wide.
     This check makes sure that:
     1. the ingress_port builtin is 9 bits;
     2. the first parameter of generate_ports is 9 bits *)
  let tofino_port_var_wid = 9 in
  let pass = ref true in
  let v =
    object
      inherit [_] s_iter as super

      method! visit_statement ctx stmt =
        super#visit_statement ctx stmt;
        match stmt.s with
        | SGen (GPort port_exp, _) ->
          let wid = InterpHelpers.width_from_ty port_exp.ety in
          if wid <> tofino_port_var_wid
          then (
            pass := false;
            report_err
              port_exp.espan
              "For Tofino, the first argument of generate must have type \
               int<<9>>")
        | _ -> ()

      method! visit_exp ctx exp =
        super#visit_exp ctx exp;
        match exp.e with
        | EVar var_cid ->
          if Cid.to_id var_cid = Builtins.ingr_port_id
          then (
            let wid = InterpHelpers.width_from_ty exp.ety in
            if wid <> tofino_port_var_wid
            then (
              pass := false;
              report_err
                exp.espan
                ("For Tofino, ingress_port must have type int<<9>> (9-bit \
                  int). This expression requires it to be int<<"
                ^ string_of_int wid
                ^ ">>.")))
        | _ -> ()
    end
  in
  v#visit_decls () ds;
  !pass
;;

(* All tables must be used in a program to compile to P4,
   because if a table is not used, we have no way of
   creating the actions, whose bodies must be inlined with
   the parameters passed to the table match statement. *)
(* this can be a warning that a table was declared but not used,
   then we can delete the table, as the P4 compiler would.  *)
(* technically, we could generate a P4 table from a lucid table
   that is never used, but it requires a bit different approach
   in the table inlining. (and the P4 compiler would likely
   delete it anyway) *)
let all_tables_used ds =
  let pass = ref true in
  let rec tables_in_prog ds =
    match ds with
    | [] -> []
    | { d = DGlobal (_, _, { e = ETableCreate tbl_def; espan }); dspan } :: ds'
      -> (tbl_def, dspan, espan) :: tables_in_prog ds'
    | _ :: ds' -> tables_in_prog ds'
  in
  let tables_matched_in_prog ds =
    let tbl_ids = ref [] in
    let v =
      object
        inherit [_] CoreSyntax.s_iter as super

        method! visit_tbl_match _ tm =
          tbl_ids := (tm.tbl |> CoreSyntax.exp_to_id) :: !tbl_ids
      end
    in
    v#visit_decls () ds;
    !tbl_ids |> MiscUtils.unique_list_of
  in
  let defined_tbls = tables_in_prog ds in
  let used_ids = tables_matched_in_prog ds in
  List.iter
    (fun (tdef, dspan, espan) ->
      let is_used = List.exists (fun id -> id = tdef.tid) used_ids in
      if not is_used
      then (
        pass := false;
        (* use source name if available *)
        let tbl_id =
          match espan.global_created_in_src with
          | Some tcid ->
            List.map TaggedCid.to_string (TaggedCid.tcid_ancestors tcid @ [tcid])
            |> String.concat "."
          | None -> Cid.id tdef.tid |> CorePrinting.cid_to_string
        in
        let msg = "The table " ^ tbl_id ^ " was declared but never used." in
        report_err dspan msg))
    defined_tbls;
  if not !pass
  then
    fail_report
      "Some tables were declared but never used (see above). The compiler \
       cannot translate a lucid table into a p4 table if it is not used.";
  !pass
;;

(* TODO: Add a check that each action can be evaluated in a single stage (so no
   large expressions, etc)
   FIXME: The port-type check should be a paramter to the type system, not a
   separate check in the midend/backend
*)
let all_checks ds =
  let checks =
    [event_param_alignment; array_sizes; port_tys; all_tables_used]
  in
  let pass =
    List.fold_left
      (fun pass check ->
        let check_pass = check ds in
        pass && check_pass)
      true
      checks
  in
  if pass <> true
  then (
    fail_report "some Tofino-specific syntax checks failed. See above.";
    exit 1)
;;

(*** old code to align parameters automatically. This is not a good idea currently,
     because events represent packet headers in their wire format. We don't want
     to pad between tcp flag fields, for example. So if we want to do something
     automatic, it has to be more sophisticated than this. ***)
(* after adding each field, check if it is byte-aligned. If not, add padding. *)
let rec align_params ps =
  match ps with
  | [] -> []
  | (id, pty) :: ps ->
    (match pty.raw_ty with
     | TInt w ->
       if w mod 8 = 0
       then (id, pty) :: align_params ps
       else (
         let pad_field = Id.fresh_name "pad", ty (TInt (8 - (w mod 8))) in
         (id, pty) :: pad_field :: align_params ps)
     | _ -> (id, pty) :: align_params ps)
;;

(* align arguments of an event creation *)
let rec align_args exps =
  match exps with
  | [] -> []
  | exp :: exps ->
    (match exp.ety.raw_ty with
     | TInt w ->
       if w mod 8 = 0
       then exp :: align_args exps
       else (
         let pad_w = 8 - (w mod 8) in
         let pad_arg = vint 0 pad_w |> value_to_exp in
         exp :: pad_arg :: align_args exps)
     | _ -> exp :: align_args exps)
;;

let align_decls ds =
  let v =
    object
      inherit [_] s_map as super

      method! visit_decl _ decl =
        match decl.d with
        | DEvent (id, esort, params) ->
          let params = align_params params in
          { decl with d = DEvent (id, esort, params) }
        | DHandler (id, hsort, (params, body)) ->
          let params = align_params params in
          { decl with d = DHandler (id, hsort, (params, body)) }
        | _ -> decl
    end
  in
  v#visit_decls () ds
;;

let align_ecalls ds =
  let v =
    object
      inherit [_] s_map as super

      method! visit_exp ctx exp =
        let exp = super#visit_exp ctx exp in
        match exp.ety.raw_ty, exp.e with
        | TEvent, ECall (ev_cid, args) ->
          { exp with e = ECall (ev_cid, align_args args) }
        | _ -> exp
    end
  in
  v#visit_decls () ds
;;

let process ds = ds |> align_decls |> align_ecalls
