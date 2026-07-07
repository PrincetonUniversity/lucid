(* Variant to tagged union lowering

     type event_variant = variant {            type event_variant = {
       foo(1): {x: int; y: int}        -->        tag: int16;
       bar(2): {z: int}                            args: union {
     };                                              foo: {x: int; y: int};
                                                     bar: {z: int};
                                                   };
                                                 };
   Also lower constructors and matches on variants. *)
open CCoreSyntax
open CCoreUtils

let tag_size = 16
let tag_ty = tint tag_size
let arm_tag_cid evid = cid (Printf.sprintf "%s_tag" (Cid.name evid));;

let decl_arm_tag arm =
  (* const uint16_t foo_tag = {$num}; *)
  dvar_const (arm_tag_cid arm.evconstrid) tag_ty
    (eval (vint (Option.get arm.evconstrnum) tag_size))
;;
let arm_tag_val arm = vint (Option.get arm.evconstrnum) tag_size
let arm_tag_var arm = evar (arm_tag_cid arm.evconstrid) tag_ty

(* the lowered variant body: { tag; args: union { arm: {fields}; ... } }, with
   per-arm payloads inline. *)
let payload_union_ty arms =
  tunion_pairs (List.map (fun arm -> (arm.evconstrid, trecord arm.evparams)) arms)
;;
let variant_struct_ty arms =
  trecord [ (cid"tag", tag_ty); (cid"args", payload_union_ty arms) ]
;;

(*
  a constructor that builds the tagged variant from its arm's parameters:

  // for arm foo(int a, int b):
  event_variant foo(int a, int b) {
    event_variant rv = {0};
    rv.args.foo.a = a;
    rv.args.foo.b = b;
    rv.tag = foo_tag;
    return rv;
  }
*)
let variant_constr variant_ty arm =
  let rv_cid = cid"ev" in
  let set_data_field (v : exp) (member : cid) (field : cid) (newval : exp) : statement =
    (* write through the named union member: v.args.<member>.<field> *)
    sassign_exp (((v /. cid"args") /. member) /. field) newval
  in
  let variant_var = evar rv_cid variant_ty in
  let constr_param_vars = List.map param_evar arm.evparams in
  let arm_fields = List.split arm.evparams |> fst in
  let init_rv = slocal rv_cid variant_ty (eval@@memzero variant_ty) in
  let set_data = stmts@@List.map2 (set_data_field variant_var arm.evconstrid) arm_fields constr_param_vars in
  let set_tag = sassign_exp (variant_var/.cid"tag") (arm_tag_var arm) in
  let ret_rv = sret variant_var in
  dfun (arm.evconstrid) variant_ty arm.evparams @@
    stmts [ init_rv; set_data; set_tag; ret_rv ]
;;

let transformer =
  let extract_fields ev arm params =
    (* ev is the matched variant; read each arm field through its union member:
       ev.args.<evconstrid>.<field>. *)
    let v = if (is_tref ev.ety) then (ederef ev) else ev in
    let data = (v /. cid"args") /. (arm.evconstrid) in
    let extract_field (pat_id, pat_ty) (decl_id, _) =
      slocal pat_id pat_ty (data /. decl_id)
    in
    stmts @@ List.map2 extract_field params arm.evparams
  in
  let arm_tag_val_of arm_assoc evid =
    arm_tag_val (List.assoc evid arm_assoc)
  in
  let rec inline_variant_pat arm_assoc exps n cases =
    match cases with
    | [] -> cases
    | (pats, bstmt)::cases -> (
      let exp = List.nth exps n in
      let pat = List.nth pats n in
      match pat with
        | PVariant{event_id; params} ->
          let field_var_init = extract_fields exp (List.assoc event_id arm_assoc) params in
          let pat = arm_tag_val_of arm_assoc (event_id) in
          let pats = replace n (patval pat) pats in
          let bstmt = sseq field_var_init bstmt in
          (pats, bstmt)::(inline_variant_pat arm_assoc exps n cases)
        | PWild ty when is_tevent_variant ty ->
          (* wildcard on the variant -> wildcard on the tag, nothing bound *)
          let pats = replace n (PWild tag_ty) pats in
          (pats, bstmt)::(inline_variant_pat arm_assoc exps n cases)
        | _ ->
          (pats, bstmt)::(inline_variant_pat arm_assoc exps n cases)
    )
  in

  object (_) inherit [_] s_map as super

  (* variant construction -> constructor call *)
  method! visit_exp arm_assoc exp =
    let exp = super#visit_exp arm_assoc exp in
    match exp.e with
      | ECall{f; args; call_kind=CVariant} ->
        let variant_ty = f.ety in
        let f_ety = tfun (List.map (fun arg -> arg.ety) args) variant_ty in
        {exp with e=ECall{f={f with ety=f_ety}; args; call_kind=CFun}}
      | _ -> exp

  method! visit_statement arm_assoc stmt =
    let stmt = super#visit_statement arm_assoc stmt in
    match stmt.s with
    | SMatch(exps, branches) ->
      let tag_of_variant_exp exp =
        if is_tevent_variant exp.ety then exp/.cid"tag"
        else if is_tref exp.ety && is_tevent_variant (extract_tref exp.ety) then exp/->cid"tag"
        else exp
      in
      let branches' =
        List.fold_left
          (fun branches n -> inline_variant_pat arm_assoc exps n branches)
          branches
          (List.init (List.length exps) (fun i -> i))
      in
      let exps = List.map tag_of_variant_exp exps in
      {stmt with s=SMatch(exps, branches')}
    | _ -> stmt
  end
;;

(* find the variant's arms from its DTy (a (constructor, tag, payload) list) *)
let find_variant_sigs decls =
  List.find_map
    (fun d -> match d.d with
      | DTy(cid, Some ty) when Cid.equal cid event_variant_cid ->
        (match (base_type ty).raw_ty with TVariant sigs -> Some sigs | _ -> None)
      | _ -> None)
    decls
;;
let sig_to_event_def (ctor, tag, payload_ty) =
  let evparams = match payload_ty.raw_ty with
    | TRecord(cids, tys) -> List.combine cids tys
    | _ -> failwith "[CCoreVariants] variant arm payload is not a record"
  in
  { evconstrid = ctor; evconstrnum = Some tag; evparams; is_packet = false; has_payload = false }
;;

let lower decls =
  let sigs = match find_variant_sigs decls with
    | Some sigs -> sigs
    | None -> failwith "[CCoreVariants] no variant type definition found"
  in
  let arms = List.map sig_to_event_def sigs in
  let struct_body = variant_struct_ty arms in
  (* the variant's name now carries its lowered struct body: substitute it into
     every reference (including inside other carried definitions, e.g. the
     envelope's `data` field), so the constructors' and matches' field
     projections resolve. *)
  let tvariant = tabstract_cid event_variant_cid struct_body in
  let set_variant_body ty = match ty.raw_ty with
    | TName(cid, _) when Cid.equal cid event_variant_cid -> tvariant
    | _ -> ty
  in
  let lowered_dty = dty event_variant_cid struct_body in
  let tags = List.map decl_arm_tag arms in
  let ctors = List.map (variant_constr tvariant) arms in
  let decls = List.concat_map
    (fun d -> match d.d with
      | DTy(cid, _) when Cid.equal cid event_variant_cid -> tags @ (lowered_dty :: ctors)
      | _ -> [d])
    decls
  in
  let decls = List.map (CCoreTransformers.subst_ty#visit_decl set_variant_body) decls in
  let arm_assoc = List.map (fun ed -> (ed.evconstrid, ed)) arms in
  transformer#visit_decls arm_assoc decls
;;
