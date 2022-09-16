(* check that all event parameters are byte-aligned *) 

open CoreSyntax

let fail_report str = 
  Console.show_message str ANSITerminal.Red "Tofino Checker"
;;

let params_wid params = 
  List.fold_left (fun tot_wid (_, pty) -> 
    match pty.raw_ty with 
      | TInt(w) -> tot_wid + w
      | TBool -> tot_wid + 1
      | _ -> error "[param_wids] event parameters must be ints or bools"
    )
    0
    params
;;

let check ds = 
  let pass = ref true in
  let v = object
    inherit [_] s_iter as super

    method! visit_DEvent _ id _ params =
      if (((params_wid params) mod 8) <> 0)
      then (
        fail_report 
          ("[Event Alignment Check] all event parameters must be byte-aligned,\
          i.e., the total width is a multiple of 8. \
          The event "^(fst id)^"'s parameters are not byte-aligned.");
        pass := false
      )
    end
  in
  v#visit_decls () ds;
  !pass
;;

(*** old code to align parameters automatically. This is not a good idea currently, 
     because events represent packet headers in their wire format. We don't want 
     to pad between tcp flag fields, for example. So if we want to do something 
     automatic, it has to be more sophisticated than this. ***)
(* after adding each field, check if it is byte-aligned. If not, add padding. *)
let rec align_params ps =
  match ps with 
  | [] -> []
  | (id, pty)::ps -> (
    match pty.raw_ty with 
    | TInt(w) -> (
      if ((w mod 8) = 0)
      then ((id, pty)::(align_params ps))
      else (
        let pad_field = 
          (Id.fresh_name "pad", ty (TInt(8 - (w mod 8))))
        in
        (id, pty)::pad_field::(align_params ps)        
      )
    )
    | _ -> (id, pty)::(align_params ps)
  )
;;

(* align arguments of an event creation *)
let rec align_args exps =
  match exps with
  | [] -> []
  | exp::exps -> (
    match exp.ety.raw_ty with 
    | TInt (w) -> (
      if ((w mod 8) = 0)
      then (exp::(align_args exps))
      else (
        let pad_w = 8 - (w mod 8) in
        let pad_arg = vint 0 pad_w |> value_to_exp in 
        exp::pad_arg::(align_args exps)
      )
    )
    | _ -> exp::(align_args exps)
  )
;;


let align_decls ds = 
  let v = object
    inherit [_] s_map as super

    method! visit_decl _ decl =
      match decl.d with 
      | DEvent(id, esort, params) ->
        let params = align_params params in
        {decl with d=DEvent(id, esort, params)}
      | DHandler(id, (params, body)) -> 
        let params = align_params params in
        {decl with d=DHandler(id, (params, body))}
      | _ -> decl
    end
  in
  v#visit_decls () ds
;;
let align_ecalls ds =
  let v = object
    inherit [_] s_map as super

    method! visit_exp ctx exp =
      let exp = super#visit_exp ctx exp in 
      match (exp.ety.raw_ty, exp.e) with 
      | TEvent, (ECall(ev_cid, args)) -> (
        {exp with e=ECall(ev_cid, align_args args)}
      )    
      | _ -> exp
    end
  in
  v#visit_decls () ds
;;


let process ds = 
  ds |> align_decls |> align_ecalls
;;