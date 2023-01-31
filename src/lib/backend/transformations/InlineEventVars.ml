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
        | SLocal(id, _, exp)
        | SAssign(id, exp) -> (
          (* get the value of the expression, if not already a value *)
          let exp = super#visit_exp ctx exp in
          match exp.ety.raw_ty with 
          | TGroup -> (
            match exp.e with 
            | EVal _ ->
              (* bind id *)
              loc_vars := (id, exp)::(!loc_vars);
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
              match List.assoc_opt (Cid.to_id var_cid) (!loc_vars) with 
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
      (ty TEvent)
      (
        call_sp 
          (Cid.id evid) 
          param_vars 
          (ty TEvent) 
          Span.default
      )
  in
  let v = object
      inherit [_] s_map as super
      val mutable has_this = false
      val mutable eventv_var = None
      method! visit_DHandler ctx hid (params, stmt) =
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
        DHandler(hid, (params, stmt))

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
        | SAssign(id, exp)
        | SLocal(id, _, exp) -> (
          match exp.ety.raw_ty with 
          | TEvent -> (
            match exp.e with 
            (* set event var to value --  bind *)
            | ECall(ev_cid, ev_args) -> (
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
              let exp = {exp with e=ECall(ev_cid, ev_args)} in 
              (* bind the updated event exp *)
              ev_calls := (id, exp)::(!ev_calls);
              (* return statements to set copy variables instead of setting event*)
              let stmt = InterpHelpers.fold_stmts arg_assignments in 
              stmt
            )
            (* case: event foo = bar; bar should already be declared. Just bind foo to var's exp *)
            | EVar(ev_cid) -> (
              match (List.assoc_opt (Cid.to_id ev_cid) (!ev_calls)) with 
              | Some exp -> 
                ev_calls := (id, exp)::(!ev_calls);
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
                match (List.assoc_opt (Cid.to_id cid) (!ev_calls)) with
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

(*   let main_stmt = (main tds).main_body |> List.hd in
  let main_stmt = v#visit_statement () main_stmt in 
  update_main tds {(main tds) with main_body = [main_stmt]}   *)
;;

let inline tds = 
  (* tds *)
(* inline_event_vars tds  *)

  inline_locations tds |> eliminate_this |> inline_event_vars ;;
