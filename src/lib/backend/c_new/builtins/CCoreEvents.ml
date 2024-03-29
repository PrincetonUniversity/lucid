(* transforming events into tagged unions *)

(*  
  There is a single type "event": 

  // source language

  event foo(int x, int y);
  event bar(int z);

  // --> c-like language 

    type foo_t {
      int x;
      int y;
    }
    type bar_t {
      int z;
    }
    enum event_tag = {
      foo_tag;
      bar_tag;
    }
    union event_data = {
      foo_t foo; 
      bar_t bar;
    }
    type event {
      event_tag tag;
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

(* let print_endline _ = () *)

let replace n x xs = 
  let rec replace' n x xs acc = 
    match xs with 
    | [] -> List.rev acc
    | y::ys -> 
      if n = 0 then 
        List.rev_append (x::acc) ys
      else 
        replace' (n-1) x ys (y::acc)
  in
  replace' n x xs []
;;


let id = Id.create

let event_rec_ty_id event_id = id (Printf.sprintf "%s_t" (Id.name event_id));;

let event_to_trecord event_def = tabstract_id 
  event_def.evconstrid 
  (trecord_pairs event_def.evparams)
;;

let event_tag evid = id (Printf.sprintf "%s_tag" (Id.name evid));;
(* remove the "_tag" suffix from evid's name component *)
let event_untag evid = id (Id.name evid |> fun name -> String.sub name 0 ((String.length name) - 4));;
let event_enum_tyid = id"event_tag_t";;
let events_to_tenum (event_defs :event_def list) = 
  let event_tags =     (List.mapi (fun i event_def -> 
    (event_tag event_def.evconstrid |> Cid.id, 
      (Option.value ~default:(i+1) event_def.evconstrnum)
    )
    ) event_defs)
  in
  let event_tags = event_tags in
  tabstract_id event_enum_tyid (tenum_pairs event_tags)
;;

let event_union_tyid = id"event_data_t";;
let events_to_tunion (event_defs :event_def list) = 
  let event_unions = List.map (fun event_def -> 
    (event_def.evconstrid, event_to_trecord event_def)
  ) event_defs
  in
  tabstract_id event_union_tyid (tunion_pairs event_unions)
;;

let event_tyid = id"event";;
let events_to_t_taggedunion event_defs = 
  let tag_ty = events_to_tenum event_defs in
  let data_ty = events_to_tunion event_defs in
  tabstract_id event_tyid (trecord_pairs [
    (id"tag", tag_ty);
    (id"data", data_ty);
  ])  
;;

(* 
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
let event_to_constr_fun event_defs event_def = 
  let event_ty = events_to_t_taggedunion event_defs in
  let event_enum_ty = events_to_tenum event_defs in
  let event_data_ty = events_to_tunion event_defs in
  dfun (Cid.id event_def.evconstrid) event_ty event_def.evparams
    (
      sret
      @@erecord_pair 
        [
          id"tag", eval@@venum (Cid.id@@event_tag event_def.evconstrid) event_enum_ty; 
          id"data", (eunion
                        event_def.evconstrid 
                        (erecord_pair 
                          (List.map 
                            (fun (id, ty) -> 
                              id, evar (Cid.id id) ty)
                            event_def.evparams))
                        event_data_ty)
        ]
    )
;;



let global_event_ty_decls event_defs = 
  let tag_ty = events_to_tenum event_defs in
  let data_ty = events_to_tunion event_defs in
  let event_ty = events_to_t_taggedunion event_defs in
  [decl_tabstract tag_ty; decl_tabstract data_ty; decl_tabstract event_ty]
;;


let transformer = 
  (* given an event expression, the events expected id and params, 
     generate statements to initialize variables with the same ids 
     as the parameters of the event to the fields of the 
     event-record's tagged-union record. *)
  let extract_fields ev evid params = 
    (* ev should be an event expression AFTER TRANSFORMATION *)
    let extract_field (ev:exp) (evid : id) (field : id) (ty : ty) = 
      let data = ev/.id"data" in
      let event = data/.evid in 
      let param = event/.field in
      slocal (Cid.id field) ty param
    in
    stmts
    @@List.map2 
      (extract_field ev evid) 
      (List.split params |> fst) 
      (List.split params |> snd)
  in
  (* the event's tag as an enum value *)
  let event_tag_symbol_val event_def_assoc evid = 
    let ty = events_to_tenum (List.split event_def_assoc |> snd) in
    vsymbol (Cid.id@@event_tag evid) ty
  in
  let rec inline_eventpat_into_case event_def_assoc exps n cases = 
    match cases with 
    | [] -> cases 
    | (pats, bstmt)::cases -> (
      let exp = List.nth exps n in
      let pat = List.nth pats n in
      match pat with 
        | PEvent{event_id; params} -> 
          let field_var_init = extract_fields exp (Cid.to_id event_id) params in 
          let pat = event_tag_symbol_val event_def_assoc (Cid.to_id event_id) in
          let pats = replace n (patval pat) pats in
          let bstmt = sseq field_var_init bstmt in
          (pats, bstmt)::(inline_eventpat_into_case event_def_assoc exps n cases)
        | PWild(ty) when (ty = (events_to_t_taggedunion (List.split event_def_assoc |> snd))) -> 
          (* wildcard on event translates to wildcard on event tag *)
          let ty = events_to_tenum (List.split event_def_assoc |> snd) in
          let pat = PWild(ty) in
          let pats = replace n (pat) pats in 
          (pats, bstmt)::(inline_eventpat_into_case event_def_assoc exps n cases)
        | _ ->  
          (pats, bstmt)::(inline_eventpat_into_case event_def_assoc exps n cases)
    )
  in

  object (_)
  inherit [_] s_map as super 

  (* event types change to the global event tagged union type  *)
  method! visit_ty event_def_assoc ty = 
    let ty = super#visit_ty event_def_assoc ty in
    match ty.raw_ty with 
      | TEvent -> events_to_t_taggedunion (List.map snd event_def_assoc)
      | _ -> ty

  (* event values change to global event tagged union values *)
  method! visit_value event_def_assoc value = 
    let value = super#visit_value event_def_assoc value in
    match value.v with 
      | VEvent(vevent) -> 
        let event_enum_ty = events_to_t_taggedunion (List.map snd event_def_assoc) in
        let event_data_ty = events_to_tunion (List.map snd event_def_assoc) in
        let evconstrid = vevent.evid |> Cid.to_id in
        let ev_params  = (List.assoc evconstrid event_def_assoc).evparams in
        let evdata = vevent.evdata in
        vrecord_pairs 
          [
            id"tag",   venum (Cid.id@@event_tag evconstrid) event_enum_ty;
            id"data", (vunion
                        evconstrid
                        (vrecord_pairs
                          (List.map2
                            (fun (id, _) param_value -> 
                              id, param_value)
                            ev_params
                            evdata))
                        event_data_ty)
          ]
      | _ -> value    

  (* event constructors change to function calls *)
  method! visit_exp event_def_assoc exp = 
    let exp = super#visit_exp event_def_assoc exp in
    let exp = 
      match exp.e with 
        | ECall{f; args; call_kind=CEvent} -> 
          (* because of visit_ty, f's type is the event tagged union.
             but inside of a call, that's wrong because now we're calling the 
             event constructor, which is a function from params -> event struct. 
             So we update f's type. *)
          let f_ety = tfun (List.map (fun arg -> arg.ety) args) f.ety in
          let f = {f with ety=f_ety} in
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
      let tag_of_union_exp exp = 
        match exp.ety with 
        | ty when ty = (events_to_t_taggedunion (List.split event_def_assoc |> snd)) -> 
          exp/.id"tag"
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
      let exps = List.map tag_of_union_exp exps in
      {stmt with s=SMatch(exps, branches')}
    | _ -> stmt
  end
;;

(* declarations: 
    1. each event translates into: 1) a struct; 2) a function that returns the struct.
    2. there are three toplevel types: 
        1. an event enum, with a tag for each event's id
        2. an event parameters union, with a component named after each event that holds its parameters
        3. an event tagged-union record, which holds a tag of type event_enum and a data of type parameter_union
*)
let transform_decl last_event_id event_defs decls decl : decls = 
  (* The toplevel types and the event functions need to be declared after all the 
     event types.*)
  match extract_devent_opt decl with
  | None -> decls @ [decl]
  | Some(event_def) -> 
    let event_ty_decl = decl_tabstract@@event_to_trecord event_def in
    if (Id.equal event_def.evconstrid last_event_id) then (
      (* this is the last event. So we also need to generate 
         the toplevel / global event data structures *)
      let toplevel_event_decls = global_event_ty_decls event_defs in
      let event_fun_decls = List.map (event_to_constr_fun event_defs) event_defs in
      decls @ [event_ty_decl] @ toplevel_event_decls @ event_fun_decls
    )
    else
      (* this is not the last event. So just return the event-specific decls *)
      decls@[event_ty_decl]
;;
  
let process decls = 
  let event_defs = List.filter_map extract_devent_opt decls in
  let last_event_id = (List.rev event_defs |> List.hd).evconstrid in
  let decls = List.fold_left (transform_decl last_event_id event_defs) [] decls in
  let event_defs_assoc = List.map (fun event_def -> (event_def.evconstrid, event_def)) event_defs in
  let decls = transformer#visit_decls event_defs_assoc decls in
  decls
;;

