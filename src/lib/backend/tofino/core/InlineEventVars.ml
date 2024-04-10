open CoreSyntax
(* open TofinoCore *)
exception Error of string
let error s = raise (Error s)

(* bug: this is broken for 
        conditional event assignments. 

  example: 
  event foo = ev1();
  if (bar) {
    foo = ev2();
  }
  generate(foo);

  bug: 
  - generate(foo) will generate ev1
*)

(* replace location variables with their values *)
let inline_locations ds =
  (* cid : exp *)
  let loc_vars = ref [] in 

  let v = object
      inherit [_] s_map as super

      (* binding *)
      method! visit_statement ctx stmt =
        match stmt.s with 
        | SLocal(id, _, exp) -> (
          let exp = super#visit_exp ctx exp in
          match exp.ety.raw_ty with 
          | TGroup -> (
            match exp.e with 
            | EVal _ ->
              (* bind id *)
              loc_vars := (Cid.id id, exp)::(!loc_vars);
              snoop (* delete the statement *)
            | _ -> error "[inline_locations] a group exp that is not a value."          
          )
          | _ -> stmt
        )
        | SAssign(cid, exp) -> (
          (* get the value of the expression, if not already a value *)
          let exp = super#visit_exp ctx exp in
          match exp.ety.raw_ty with 
          | TGroup -> (
            match exp.e with 
            | EVal _ ->
              (* bind id *)
              loc_vars := (cid, exp)::(!loc_vars);
              snoop (* delete the statement *)
            | _ -> error "[inline_locations] a group exp that is not a value."
          )
          (* case: not a group *)
          | _ -> stmt
        )
        | _ ->  super#visit_statement ctx stmt 

      (* replacing *)
      method! visit_exp ctx exp =
      let exp = super#visit_exp ctx exp in
      match exp.ety.raw_ty with 
        | TGroup -> (
          match exp.e with 
            | EVar(var_cid) -> (
              match List.assoc_opt (var_cid) (!loc_vars) with 
              | Some(eval) -> eval
              | None -> error "[inline_locations] location variable not found"
            )
            | _ -> exp
        )
        | _ -> exp
    end
  in
  v#visit_decls () ds 
(*   let main_stmt = (main tds).main_body |> List.hd in
  let main_stmt = v#visit_statement () main_stmt in 
  update_main tds {(main tds) with main_body = [main_stmt]}   *)
;;

(* replace "this" keyword with an event variable that is a copy of the current event at input. *)
let eliminate_this ds =
  let scopy_event var_id evid params =
    let param_vars = List.map 
        (fun (id, ty) -> (var_sp (Cid.id id) ty Span.default)) 
        params 
    in       
    slocal
      var_id
      tevent
      (
        call_sp 
          (Cid.id evid) 
          param_vars 
          tevent 
          Span.default
      )
  in
  let v = object
      inherit [_] s_map as super
      val mutable has_this = false
      val mutable eventv_var = None
      method! visit_DHandler ctx hid s (params, stmt) =
        let ev_var_id = Id.create (fst hid^"_input") in
        has_this <- false;
        eventv_var <- Some(EVar((Cid.id ev_var_id)));
        (* descend into the body *)
        let stmt = super#visit_statement ctx stmt in
        let stmt = match (has_this) with 
          | true -> 
            sseq (scopy_event ev_var_id hid params) stmt
          | false -> stmt 
        in
        DHandler(hid, s, (params, stmt))

      method! visit_EVar _ cid =
        if ((Cid.names cid |> List.hd) = "this")
        then (
          has_this <- true;
          match (eventv_var) with 
            | Some ev -> ev
            | None -> error "[eliminate_this] no event var?"
          )
        else (EVar(cid))
      end
  in
  v#visit_decls () ds
;;


(* replace event variables with their call expressions *)
(* future optimization: 
    - only create a copy of an argument if it contains a variable that 
      is changed before all uses of the event *)

let inline_event_vars (ds:decls) =
  let ev_calls = ref [] in 

  let slocal_exp id exp =
    slocal id (exp.ety) exp
  in

  let v = object
      inherit [_] s_map as super

      method! visit_statement ctx stmt =
        (* var_id = ecall --> add binding (var_id:ecall) *)
        (* var_id = evar (vid)--> add binding (var_id:ev_calls[vid]) *)
        (* gen(var_id) --> replace var_id *)
        match stmt.s with 
        | SAssign(cid, exp) -> (
          match exp.ety.raw_ty with 
          | TEvent-> (
            match exp.e with 
            (* set event var to value --  bind *)
            | ECall(ev_cid, ev_args, u) -> (
              (* create new variables that are copies of args 
                 at event var decl time. Create var exps and assign stmts. *)
              let ev_args, arg_assignments = List.mapi
                (fun i exp -> 
                  let var_id = Id.create (fst (Cid.to_id cid)^"_arg_"^(string_of_int i)) in 
                  let var_exp = {exp with e=EVar(Cid.id var_id)} in 
                  let var_stmt = slocal_exp var_id exp in
                  var_exp, var_stmt                  
                )
                ev_args
              |> List.split
              in
              (* use copy variables as event arguments *)
              let exp = {exp with e=ECall(ev_cid, ev_args, u)} in 
              (* bind the updated event exp *)
              ev_calls := (cid, exp)::(!ev_calls);
              (* return statements to set copy variables instead of setting event*)
              let stmt = InterpHelpers.fold_stmts arg_assignments in 
              stmt
            )
            (* case: event foo = bar; bar should already be declared. Just bind foo to var's exp *)
            | EVar(ev_cid) -> (
              match (List.assoc_opt ( ev_cid) (!ev_calls)) with 
              | Some exp -> 
                ev_calls := (cid, exp)::(!ev_calls);
                snoop
              | _ -> error "[inline_event_vars] the event variable on the rhs is not bound."
            )
            | _ -> error "[inline_event_vars] reached an event assign or declaration that has a rhs which is not a call or a variable."
          )
          | _ -> stmt
        )
        | SLocal(id, _, exp) -> (
          let cid = Cid.id id in 
          match exp.ety.raw_ty with 
          | TEvent -> (
            match exp.e with 
            (* set event var to value --  bind *)
            | ECall(ev_cid, ev_args, u) -> (
              (* create new variables that are copies of args 
                 at event var decl time. Create var exps and assign stmts. *)
              let ev_args, arg_assignments = List.mapi
                (fun i exp -> 
                  let var_id = Id.create (fst id^"_arg_"^(string_of_int i)) in 
                  let var_exp = {exp with e=EVar(Cid.id var_id)} in 
                  let var_stmt = slocal_exp var_id exp in
                  var_exp, var_stmt                  
                )
                ev_args
              |> List.split
              in
              (* use copy variables as event arguments *)
              let exp = {exp with e=ECall(ev_cid, ev_args, u)} in 
              (* bind the updated event exp *)
              ev_calls := (cid, exp)::(!ev_calls);
              (* return statements to set copy variables instead of setting event*)
              let stmt = InterpHelpers.fold_stmts arg_assignments in 
              stmt
            )
            (* case: event foo = bar; bar should already be declared. Just bind foo to var's exp *)
            | EVar(ev_cid) -> (
              match (List.assoc_opt (ev_cid) (!ev_calls)) with 
              | Some exp -> 
                ev_calls := (cid, exp)::(!ev_calls);
                snoop
              | _ -> error "[inline_event_vars] the event variable on the rhs is not bound."
            )
            | _ -> error "[inline_event_vars] reached an event assign or declaration that has a rhs which is not a call or a variable."
          )
          | _ -> stmt
        )
        (* replace expressions in generates if they are evars *)
        | SGen(gty, exp) -> (
          let exp = match exp.ety.raw_ty with 
            | TEvent -> (
              match exp.e with 
              | EVar(cid) -> (
                match (List.assoc_opt (cid) (!ev_calls)) with
                | Some ev_call -> ev_call
                | None -> error "[inline_event_vars] event variable not found in context"
              )
              | _ -> exp
            )
            | _ -> exp
          in
          {stmt with s=SGen(gty, exp)}
        )
        (* other statements -- just recurse *)
        | _ -> super#visit_statement ctx stmt
    end
  in

  v#visit_decls () ds 
;;


(* assign a number to each event *)
let set_event_nums decls =
  let event_nums = List.filter_map 
    (fun decl -> match decl.d with
      | DEvent(_, nopt, _, _) -> nopt
      | _ -> None)
    decls
  in
  let rec set_event_nums' num decls = 
    if (List.exists (fun v -> v = num) event_nums)
    then set_event_nums' (num+1) decls
    else 
      match decls with
      | [] -> []
      | decl::decls -> (
        match decl.d with
        | DEvent(a, None, b, c) -> 
          {decl with d = DEvent(a, Some(num), b, c)}::(set_event_nums' (num+1) decls)
        | _ -> decl::(set_event_nums' num decls)
      )
  in
  set_event_nums' 1 decls
;;



(*   let main_stmt = (main tds).main_body |> List.hd in
  let main_stmt = v#visit_statement () main_stmt in 
  update_main tds {(main tds) with main_body = [main_stmt]}   *)
;;

(*** 
  event variable elimination. 
  - walk through the program, collecting information about the possible event constructors 
    that each event variable could be made with.
  - when an event variable is declared or assigned, create a copy of each of its arguments 
    and assign them to new variables.
  - this actually misses copies. Sigh.
  - when an event variable is used in a generate statement, create a match statement that 
    branches on the tag of the event variable, and branches to a call to the appropriate 
    event constructor.    


***)

type event_constr = {evcid : cid; evnum : int; evsort : event_sort; evparams : params}

type ctx = {
  event_constrs : (cid * event_constr) list; (*all the event constructors in the program. *)
  event_vars : (cid * event_constr list) list; (*event variable and the constructors that it might use. *)
}
let empty_ctx = {event_constrs = []; event_vars = []};;


let evconstr_of_exp ctx exp = 
  match exp.e with 
  | ECall(cid, args, _) -> (
    match List.assoc_opt cid ctx.event_constrs with
    | Some(constr) -> Some(constr, args)
    | None -> None
  )
  | _ -> None
;;

let rec merge_contexts ctx1 ctx2 = 
  let event_constrs = ctx1.event_constrs @ ctx2.event_constrs in
  let event_vars = List.fold_left
    (fun event_vars (cid, constrs) -> 
      match List.assoc_opt cid event_vars with 
      | Some(constrs') -> 
        let constrs = List.sort_uniq compare (constrs' @ constrs) in
        (cid, constrs)::(List.remove_assoc cid event_vars)
      | None -> (cid, constrs)::event_vars)
    ctx1.event_vars
    ctx2.event_vars
  in
  {event_constrs; event_vars} 
;;

let evar_param_cid evar_cid constr n = 
  let param_cid = Cid.id (List.nth constr.evparams n |> fst) in
  let full_name = (Cid.names evar_cid) @ (Cid.names constr.evcid) @ (Cid.names param_cid) |> 
    String.concat "_"
  in
  let full_cid = Cid.create_ids [Id.create full_name] in
  full_cid
;;

let evar_param_cids evar_cid constr = 
  List.mapi
    (fun i _ -> evar_param_cid evar_cid constr i)
    constr.evparams
;;

(* the cid of an argument of an event variable *)
(* result is evar_cid^constr_cid^arg_cid *)
let ctx_evar_param_cid ctx evar_cid constr_cid arg_pos = 
  let constr = List.assoc constr_cid ctx.event_constrs in  
  evar_param_cid evar_cid constr arg_pos
;;

let evar_tag_cid evar_cid = 
  let full_name = (Cid.names evar_cid)@["tag"] |> String.concat "_" in
  let full_cid = Cid.create_ids [Id.create full_name] in
  full_cid
;;
let evar_tag_size = 16;;



(* set parameters of event variable from constructor call rhs *)
let param_set_stmts ctx evcid evargs constr_cid = 
  let param_set_stmts = List.mapi
    (fun i arg ->
      let param_cid = ctx_evar_param_cid ctx evcid constr_cid i in
      sassign param_cid arg)
    evargs
  in
  sequence_stmts param_set_stmts
;;
(* set parameters of event variable from event ref rhs *)
let param_copy_stmts ctx lhs_evcid rhs_evcid = 
  (* at this point, rhs_evcid should have a subset of lhs_evcid's constructors. 
     And we want to generate statements to copy all the parameters of all of lhs_evcid's 
     constructors to rhs_evcid. *)
  let constrs = List.assoc lhs_evcid ctx.event_vars in
  (* for each constructor of lhs_evcid, find the corresponding constructor of rhs_evcid. *)
  let param_stmts = List.fold_left
    (fun stmts constr ->
      let param_stmts = List.mapi
        (fun i (_, param_ty) -> 
          let lhs_param_cid = ctx_evar_param_cid ctx lhs_evcid constr.evcid i in
          let rhs_param_cid = ctx_evar_param_cid ctx rhs_evcid constr.evcid i in
          sassign lhs_param_cid (var rhs_param_cid (param_ty)))
          constr.evparams
      in
      stmts @ param_stmts)
    []
    constrs
  in
  sequence_stmts param_stmts
;;


let add_constr ctx cid constr_cid = 
  let constr = List.assoc constr_cid ctx.event_constrs in
  let constrs = match List.assoc_opt cid ctx.event_vars with 
    | None -> [] | Some(constrs) -> constrs 
  in
  let constrs = List.sort_uniq compare (constr::constrs) in
  let event_vars = (cid, constrs)::(List.remove_assoc cid ctx.event_vars) in
  {ctx with event_vars}
;;

let add_constrs_from_var ctx cid rhs_cid = 
  let constrs = match List.assoc_opt cid ctx.event_vars with 
    | None -> [] | Some(constrs) -> constrs 
  in
  let constrs = List.sort_uniq compare (constrs @ (List.assoc rhs_cid ctx.event_vars)) in
  let event_vars = (cid, constrs)::(List.remove_assoc cid ctx.event_vars) in
  {ctx with event_vars}
;;

let tag_set_stmt cid tag_num = sassign (evar_tag_cid cid) (vint_exp tag_num evar_tag_size);;

let tag_cpy_stmt cid rhs_cid = 
  let rhs_tag_cid = evar_tag_cid rhs_cid in
  let lhs_tag_cid = evar_tag_cid cid in
  sassign lhs_tag_cid (var rhs_tag_cid (tint@@Sz evar_tag_size))
;;

let evconstr_num ctx constr_cid = 
  let constr = List.assoc constr_cid ctx.event_constrs in
  constr.evnum
;;

(* inline an event variable declaration or assignment *)
let rec inline_stmt ctx stmt = 
  let elim_ev_var_update cid exp = 
    match exp.ety.raw_ty, exp.e  with
    | TEvent, ECall(constr_cid, args, _) -> 
      (* update the context, adding the constructor to the event *)
      let ctx = add_constr ctx cid constr_cid in
      (* set tag and params from constructor  *)
      let param_stmt = param_set_stmts ctx cid args constr_cid in
      let stmt = sequence_stmts
        ((tag_set_stmt cid (evconstr_num ctx constr_cid))::[param_stmt])
      in
      (* return the updated context and statement *)
      ctx, stmt
    | TEvent, EVar(rhs_cid) -> 
      let ctx = add_constrs_from_var ctx cid rhs_cid in
      let param_stmt = param_copy_stmts ctx cid rhs_cid in
      let stmt = sequence_stmts
        ((tag_cpy_stmt cid rhs_cid)::[param_stmt])
      in
      ctx, stmt
    | _ -> ctx, stmt
  in
  match stmt.s with
  | SLocal(id, _, exp) ->
    elim_ev_var_update (Cid.id id) exp
  | SAssign(cid, exp) -> 
    elim_ev_var_update cid exp 
  | SGen(gty, exp) -> (
    match exp.e with EVar(evar_cid) -> ( 
      (* get a list of the possible event constructors that evar_cid could be made with *)
      let constrs = List.assoc evar_cid ctx.event_vars in
      (* create branches of a match statement for each possible event constructor. *)
      let gen_branches = List.map
        (fun constr -> 
          (* the arguments to the event constructor call are the variables of evar_cid for this constructor *)
          let param_exps = List.mapi
            (fun i param -> 
              let param_cid = ctx_evar_param_cid ctx evar_cid constr.evcid i in
              var param_cid (param |> snd))
            constr.evparams
          in
          let call_exp = call_sp constr.evcid param_exps tevent Span.default in
          let gen_stmt = {stmt with s=SGen(gty, call_exp)} in
          let pat = [PNum(Z.of_int constr.evnum)] in
          (* now the branch is an exact match for this constructors num and the gen statement *)
          pat, gen_stmt)
        constrs
      in
      (* create a match statement that branches on the tag of evar_cid *)
      let tag_cid = evar_tag_cid evar_cid in
      let tag_exp = var tag_cid (tint@@Sz evar_tag_size) in
      let gen_stmt = smatch [tag_exp] gen_branches in
      ctx, gen_stmt
    )
    (* generate statements that don't have variable args don't change *)
    | _ -> ctx, stmt
  )
  | SSeq(s1, s2) -> (
    let ctx, s1 = inline_stmt ctx s1 in
    let ctx, s2 = inline_stmt ctx s2 in
    ctx, sseq s1 s2
  )
  | SIf(e, s1, s2) -> (
    (* take the contexts of each branch, then fold them together. *)
    let ctx1, s1 = inline_stmt ctx s1 in
    let ctx2, s2 = inline_stmt ctx s2 in
    let ctx = merge_contexts ctx1 ctx2 in
    ctx, sifte e s1 s2
  )
  | SMatch(e, cases) -> (
    let ctxs, cases = 
      List.split (List.map 
        (fun (p, s) -> 
          let ctx, s' = inline_stmt ctx s in ctx, (p, s')) cases) 
    in
    let ctx = List.fold_left merge_contexts ctx ctxs in
    (* let ctx = {ctx with event_vars = List.sort_uniq (fun (cid1, _) (cid2, _) -> Cid.compare cid1 cid2) ctx.event_vars} in *)
    ctx, smatch e cases
  )
  | _ -> ctx, stmt
;;

let event_var_param_slocals evar_cid constrs = 
  (* declare the tag *)
  let tag_decl = slocal (evar_tag_cid evar_cid |> Cid.to_id) (ty (TInt(Sz evar_tag_size))) (vint_exp 0 evar_tag_size) in
  (* for each constructor that the event variable may take, declare parameter variables.  *)
  let param_decls = List.map
    (fun constr -> 
      let param_decls = List.mapi
        (fun i (_, ty) -> 
          let evar_param_cid = evar_param_cid evar_cid constr i in
          slocal (Cid.to_id evar_param_cid) ty (vint_exp_ty 0 ty))
        constr.evparams
      in
      sequence_stmts param_decls)
    constrs
  in
  (* return tag declaration + all param declarations *)
  sequence_stmts (tag_decl::param_decls)
;;

let inline_decl ctx decl = 
  match decl.d with 
  | DEvent(evid, evnum, evsort, params) -> (
    let ev_constr = {
      evcid = Cid.id evid;
      evnum = Option.get evnum;
      evsort = evsort;
      evparams = params;
    } in
    let ctx = {ctx with event_constrs = (Cid.id evid, ev_constr)::(ctx.event_constrs)} in    
    ctx, decl
  )
  | DHandler(hid, s, (params, stmt)) -> (
    (* inline all event constructors into generate statements *)
    let ctx', stmt = inline_stmt ctx stmt in
    (* construct new statements, declaring all the event variable parameters created in the process. *)
    let ev_var_slocals = List.map
      (fun (cid, constrs) -> 
        let decl = event_var_param_slocals cid constrs in
        decl)
      ctx'.event_vars
    in
    (* update the statement, but return original context because locals are ... local *)
    let stmt = sequence_stmts (ev_var_slocals @ [stmt]) in
    let decl = {decl with d = DHandler(hid, s, (params, stmt))} in
    ctx, decl
  )
  (* nothing else changes, because event variables don't appear anywhere else.  *)
  | _ -> ctx, decl
;;

let rec inline_decls ctx decls = 
  match decls with 
  | [] -> []
  | d::ds -> 
    let ctx, d = inline_decl ctx d in 
    d::(inline_decls ctx ds)
;;

let inline tds = 
  (* inline_locations tds |> eliminate_this |> inline_event_vars ;; *)
  let res = inline_locations tds |> eliminate_this |> (inline_decls empty_ctx) in
  (* print_endline ("after inline event variables");
  print_endline (CorePrinting.decls_to_string res);
  exit 1; *)
  res
