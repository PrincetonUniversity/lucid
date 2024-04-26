(* transforming events into tagged unions *)

(*  
  There is a single type "event": 

  // source language

  event foo(int x, int y);
  event bar(int z);

  // --> c-like language 

    int foo_tag = 1;
    type foo_t {
      int x;
      int y;
    }
    int bar_tag = 2;
    type bar_t {
      int z;
    }
    type event_data = foo_t; // largest event
    type event {
      int tag;
      event_data data;
    }

  ==========

  - declarations: event declarations should be changed to function declarations that 
    return an instance of the global event type. 
  - types: event types should all be changed to the global event record type. 
  - expressions: ECall(CEvent, ...) -> ECall(CFunction)
  - statements: match statements: 
    // translate a match on an event into a match on 
    // the event's tag, and make the first statements 
    // of the branch bind the data parameters to 
    // the specified identifiers.

    match e with 
      | foo(int x, int y) -> ...
      | bar(int z) -> ...
    
    // --> 
    match e.tag with 
      | foo_tag {
        int x = e.data.foo.x;
        int y = e.data.foo.y;
        ...
      }
      | bar_tag {
        int z = e.data.bar.z;
        ...
      }
*)
open CCoreSyntax
open CCoreUtils

(* let print_endline _ = () *)
(*** rewrite ***)
let event_tag_size = 16
let event_tag_ty = tint event_tag_size
let event_tag evid = cid (Printf.sprintf "%s_tag" (Cid.name evid));;
let event_untag evid = cid (Cid.name evid |> fun name -> String.sub name 0 ((String.length name) - 4));;

let decl_event_tag event_def = 
  (* const uint16_t e_tag = {$num}; *)
  dvar_const 
    (event_tag event_def.evconstrid) 
    event_tag_ty 
    (eval (vint (Option.get event_def.evconstrnum) event_tag_size))
;;
let event_tag_val event_def = 
  vint (Option.get event_def.evconstrnum) event_tag_size
;;
let event_tag_var event_def = 
  evar (event_tag event_def.evconstrid) event_tag_ty
;;

(* event foo(int a, int b) -> type foo = {int a; int b} *)
let event_param_tyid event_def = 
  (cid((Cid.name event_def.evconstrid)^"_t")) 
;;
let event_param_ty event_def =
  let evparams = List.map (fun (id, ty) -> (id, ty)) event_def.evparams in
  tabstract_cid
  (event_param_tyid event_def)
  (trecord evparams)
;;

(* event foo(int a, int b); event bar(int c); -> type event_data_t = union {foo foo; bar bar;} *)
let event_len event_def = 
  let bit_len = List.fold_left  
  (fun sz (_, ty) -> sz + bitsizeof_ty_exn ty)
  0
  event_def.evparams
  in
  let byte_len = (bit_len+7) / 8 in
  byte_len
;;
let event_len_ty = tint 16
let event_len_val event_def = 
  let res = vint (event_len event_def) (bitsizeof_ty_exn event_len_ty) in
  res
;;
let event_data_ty_id = cid"event_data_t";;
let event_data_ty (event_defs :event_def list) = 
  let biggest_ev_opt, _ =  List.fold_left 
    (fun (ev, sz) evdef -> 
      if (event_len evdef) > sz then (Some(evdef), event_len evdef) else (ev, sz))
    (None, -1)
    event_defs
  in
  tabstract_cid (event_data_ty_id) (tname@@event_param_tyid (Option.get biggest_ev_opt))
;;

(* the toplevel event type *)
let event_ty_id = cid"event_t";;
let event_ty event_defs = 
  tabstract_cid event_ty_id (trecord [
    (cid"data",event_data_ty event_defs); (* IMPORTANT: the data has to come first because of how we cast *)
    (cid"tag", event_tag_ty);
    (cid"len", event_len_ty);
    (cid"is_packet", tint 8)
  ])

let mk_toplevel_event_decls event_defs = 
  [decl_tabstract (event_data_ty event_defs); decl_tabstract (event_ty event_defs)]
;;

(**** code generation and transformations ****)

(* 
  make a function that constructs an event record from its parameters.
  Lets the program keep the same syntax for event value construction.

  event foo(int a, int b); ==>   
  event foo(int a, int b) {
    return 
      {
        tag=foo_tag;
        data={
          foo={a=a; b=b;}
        };
      }
    ;
*)
let event_constr_ty event_ty event_def = 
  tfun (List.map snd event_def.evparams) event_ty
;;
let event_constr event_ty event_def = 
  (* construct an event *)
  (* 
    event_t mk_ev_a(uint16_t x, u_int16_t y, u_int32_t z) {
        event_t rv;
        ((ev_a * )(&rv))->x = x;
        ((ev_a * )(&rv))->y = y;
        ((ev_a * )(&rv))->z = z;
        rv.tag = ev_a_num;
        rv.len = sizeof(ev_a);
        return rv;
    }     
  *)
  let rv_cid = cid"ev" in
  let set_data_field (event : exp) (event_params_ty : ty) (field : cid) (newval : exp) : statement = 
    (* cast the event to the type of the specific event and then 
       set field to newval *)
    (* event.field := newval; // where field is a member of event_data_ty *)
    let event_var, event_var_ty = extract_evar event in 
    sassign_exp
      (* >>> ((event_data_ty * )(&event_var)) -> field_cid = newval <<< *)
      (( ecast (tptr event_params_ty) (eaddr event_var event_var_ty))/->(field))
      newval
  in
  let event_var = evar rv_cid event_ty in
  let event_param_ty = event_param_ty event_def in
  let constr_args = event_def.evparams in
  let constr_param_vars = List.map param_evar constr_args in
  let event_fields = List.split event_def.evparams |> fst in
  let init_rv = slocal rv_cid event_ty (eval@@memzero event_ty) in 
  let set_data = stmts@@List.map2 (set_data_field event_var event_param_ty) event_fields constr_param_vars in
  let set_tag = sassign_exp (event_var/.cid"tag") (event_tag_var event_def) in
  let set_len = sassign_exp (event_var/.cid"len") (eval (event_len_val event_def)) in
  let set_packet = sassign_exp (event_var/.cid"is_packet") (eval (if (event_def.is_packet) then vint 1 8 else vint 0 8)) in
  let ret_rv = sret event_var in
  dfun (event_def.evconstrid) event_ty event_def.evparams @@
    stmts [
      init_rv;
      set_data;
      set_tag;
      set_len;
      set_packet;
      ret_rv
    ]
;;
let transformer = 
  (* do all the transformations on event-related types, expressions, and statements *)
  (* first, some helpers *)
  let extract_fields ev evdef params = 
    (* ev should be a global event union type expression *)
    let evref = (* we want a reference to the event *)
      if (is_ederef ev) then (extract_ederef ev) (* if its a deref, just get the event ref out*)
      else (* if its not a deref, it must be a var, so we take its address *)
        let event_var, event_var_ty = extract_evar ev in
        (eaddr event_var event_var_ty)
    in
    let extract_field (field : cid) (ty : ty) = 
      (* (( ecast (tptr event_params_ty) (eaddr event_var event_var_ty))/->(field)) *)
      let rhs = (( ecast (tptr (event_param_ty evdef)) evref)/->(field)) in
      slocal (field) ty rhs
    in
    stmts
    @@List.map2 
      extract_field 
      (List.split params |> fst) 
      (List.split params |> snd)
  in
  let event_tag_val event_def_assoc evid = 
    event_tag_val (List.assoc evid event_def_assoc)
  in
  let rec inline_eventpat_into_case event_def_assoc exps n cases = 
    match cases with 
    | [] -> cases 
    | (pats, bstmt)::cases -> (
      let exp = List.nth exps n in
      let pat = List.nth pats n in
      match pat with 
        | PEvent{event_id; params} -> 
          let field_var_init = extract_fields exp (List.assoc event_id event_def_assoc) params in 
          let pat = event_tag_val event_def_assoc (event_id) in
          let pats = replace n (patval pat) pats in
          let bstmt = sseq field_var_init bstmt in
          let res = 
            (pats, bstmt)::(inline_eventpat_into_case event_def_assoc exps n cases)
          in
          res
  
        | PWild(ty) when (ty = (event_ty (List.split event_def_assoc |> snd))) -> 
          (* wildcard on event translates to wildcard on event tag, with nothing bound *)
          let pat = PWild(event_tag_ty) in
          let pats = replace n (pat) pats in 
          (pats, bstmt)::(inline_eventpat_into_case event_def_assoc exps n cases)
        | _ ->  
          (pats, bstmt)::(inline_eventpat_into_case event_def_assoc exps n cases)
    )
  in

  object (_) inherit [_] s_map as super 
  (* event types change to the union event type  *)
  method! visit_ty event_def_assoc ty = 
    let ty = super#visit_ty event_def_assoc ty in
    match ty.raw_ty with 
      | TEvent -> event_ty (List.map snd event_def_assoc)
      | _ -> ty

  (* event expressions and values change to constructor function calls *)
  method! visit_exp event_def_assoc exp = 
    let exp = super#visit_exp event_def_assoc exp in
    let exp = 
      match exp.e with 
        | ECall{f; args; call_kind=CEvent} -> 
          let union_event_ty = f.ety in (* because of visit_ty *)
          let f_ety = tfun (List.map (fun arg -> arg.ety) args) union_event_ty in
          let f = {f with ety=f_ety} in
          {exp with e=ECall{f; args; call_kind=CFun}}
        | EVal({v=VEvent(vevent); vty=union_event_ty}) -> 
          let args = List.map eval vevent.evdata in
          let f_cid = vevent.evid in
          let f_ety = tfun (List.map (fun arg -> arg.ety) args) union_event_ty in
          let f = efunref f_cid f_ety in
          {exp with e=ECall{f; args; call_kind=CFun}}
        | _ -> exp
    in
    exp

  (* match statements:
      for each pattern in a case: 
        if the pattern is on an event:
          replace the pattern with the event's tag
          prepend statements to the case's branch to initialize 
          the event's parameters as variables. *)
  method! visit_statement event_def_assoc stmt = 
    let stmt = super#visit_statement event_def_assoc stmt in
    match stmt.s with 
    | SMatch(exps, branches) -> 
      (* match on event tags, not the event itself *)
      let tag_of_union_event_exp exp = match exp.ety with 
        | ty when ty = (event_ty (List.split event_def_assoc |> snd)) -> 
          exp/.cid"tag"
        | ty when ty = (tref@@event_ty (List.split event_def_assoc |> snd)) -> 
          exp/->cid"tag"
        | _ -> exp
      in
      let branches' = 
        List.fold_left 
          (fun branches n -> inline_eventpat_into_case event_def_assoc exps n branches)
          branches
          (List.init (List.length exps) (fun i -> i))
      in
      (* we transform exps _after_ the branches, because to transform the branches 
         we need to know the data in the tagged union, which we don't get if 
          we transform the expressions to tags *)
      let exps = List.map tag_of_union_event_exp exps in
      {stmt with s=SMatch(exps, branches')}
    | _ -> stmt
  end
;;

(* declarations: 
    1. each event translates into: 1) an event parameter struct; 2) a constructor that returns a struct.
    2. there are two toplevel types: 
        1. an event parameters union, which is just an alias of the largest event parameter struct
        2. an event tagged-union record, which holds a tag of type event_enum and a data of type parameter_union
*)
let transform_decl last_event_id event_defs decls decl : decls = 
  (* The transformation is a little convoluted because the toplevel types 
     and the event constructors need to be declared after all the event types. *)
  match extract_devent_opt decl with
  | None -> decls @ [decl]
  | Some(event_def) -> 
    let event_ty_decl = decl_tabstract@@event_param_ty event_def in
    let event_num_decl = decl_event_tag event_def in
    if (Cid.equal event_def.evconstrid last_event_id) then (
      (* this is the last event. So we also need to generate 
         the toplevel / global event data structures *)
      let toplevel_event_decls = mk_toplevel_event_decls event_defs in
      (* event functions have to go after events *)
      let event_fun_decls = List.map (event_constr (event_ty event_defs)) event_defs in
      decls @ [event_num_decl;event_ty_decl] @ toplevel_event_decls @ event_fun_decls
    )
    else
      (* this is not the last event. So just return the event-specific decls *)
      decls@[event_num_decl;event_ty_decl]
;;

let process decls = 
  let event_decls = List.filter is_devent decls in 
  let event_defs = List.filter_map extract_devent_opt decls in
  let last_event_id = (List.rev event_defs |> List.hd).evconstrid in
  let decls = List.fold_left (transform_decl last_event_id event_defs) [] decls in
  let event_defs_assoc = List.map (fun event_def -> (event_def.evconstrid, event_def)) event_defs in
  (* the event declarations are convenient for deparse generation, so keep them around until that *)
  let decls = 
    (event_decls@(transformer#visit_decls event_defs_assoc decls)) 
  in
  decls
;;

