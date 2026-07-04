(* Lower a tagged-variant ADT to a C tagged union, in place.

   The variant is a DTy whose body is a TVariant -- a list of (constructor, tag,
   payload) arms. This pass rewrites that DTy's *body* into the C tagged-union form
   and leaves the type's name (and every reference to it) untouched:

     type event_variant = variant {            type event_variant = {
       foo(1): {x: int; y: int}        -->        tag: int16;
       bar(2): {z: int}                            payload: union {
     };                                              foo: {x: int; y: int};
                                                     bar: {z: int};
                                                   };
                                                 };

   Per-arm payloads are inline (anonymous) union members -- they are only ever
   reached via the member (`v.payload.foo.x`), never as standalone types crossing a
   function boundary, so they need no typedef of their own. The pass also emits a
   tag constant per arm and a constructor function per arm, and rewrites uses:
     - ECall(CVariant, ..)  -> a call to the arm's constructor function
     - SMatch on PVariant   -> a switch on `.tag`, with the branch prefixed by reads
                               of the arm's union member into the bound variables.

   It knows nothing about events: any record/envelope *wrapping* the variant (e.g.
   the event `{meta; data: event_variant}`) is an ordinary record that this pass
   does not touch. (It still finds the one variant by name -- event_variant_cid --
   since that is the only variant in the program today.) *)
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

(* the lowered variant body: { tag; payload: union { arm: {fields}; ... } }, with
   per-arm payloads inline. *)
let payload_union_ty arms =
  tunion_pairs (List.map (fun arm -> (arm.evconstrid, trecord arm.evparams)) arms)
;;
let variant_struct_ty arms =
  trecord [ (cid"tag", tag_ty); (cid"payload", payload_union_ty arms) ]
;;

(*
  a constructor that builds the tagged variant from its arm's parameters:

  // for arm foo(int a, int b):
  event_variant foo(int a, int b) {
    event_variant rv = {0};
    rv.payload.foo.a = a;
    rv.payload.foo.b = b;
    rv.tag = foo_tag;
    return rv;
  }
*)
let variant_constr variant_ty arm =
  let rv_cid = cid"ev" in
  let set_data_field (v : exp) (member : cid) (field : cid) (newval : exp) : statement =
    (* write through the named union member: v.payload.<member>.<field> *)
    sassign_exp (((v /. cid"payload") /. member) /. field) newval
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

(* rewrite construction (CVariant -> constructor call) and matches (PVariant -> tag
   switch + union-member reads). No type rewriting: the variant keeps its name, so
   every reference already resolves. *)
let transformer =
  let extract_fields ev arm params =
    (* ev is the matched variant; read each arm field through its union member:
       ev.payload.<evconstrid>.<field>. *)
    let v = if (is_tref ev.ety) then (ederef ev) else ev in
    let data = (v /. cid"payload") /. (arm.evconstrid) in
    (* bind by position: the pattern may name params differently from the arm
       declaration, so the binding name comes from the pattern, the field read from
       the declaration. *)
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
      | EVal({v=VVariant(vvariant); vty=variant_ty}) ->
        let args = List.map eval vvariant.evdata in
        let f = efunref vvariant.evid (tfun (List.map (fun arg -> arg.ety) args) variant_ty) in
        {exp with e=ECall{f; args; call_kind=CFun}}
      | _ -> exp

  (* match on a variant -> match on its tag, with each branch prefixed by reads of
     the matched arm's fields into the pattern's bound variables. *)
  method! visit_statement arm_assoc stmt =
    let stmt = super#visit_statement arm_assoc stmt in
    match stmt.s with
    | SMatch(exps, branches) ->
      let tag_of_variant_exp exp =
        if is_tevent_variant exp.ety then exp/.cid"tag"
        else if is_tref exp.ety && is_tevent_variant (extract_tref exp.ety) then exp/->cid"tag"
        else exp
      in
      (* transform branches first: we need the variant type of the exps to know the
         union members, which we lose once the exps become tags. *)
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
        (* the DTy body is a bare TVariant (definitions stay structural); base_type
           would also resolve a TName alias via tydefs *)
        (match (base_type ty).raw_ty with TVariant sigs -> Some sigs | _ -> None)
      | _ -> None)
    decls
;;
let sig_to_event_def (ctor, tag, payload_ty) =
  let evparams = match payload_ty.raw_ty with
    | TRecord(cids, tys) -> List.combine cids tys
    | _ -> failwith "[CCoreVariants] variant arm payload is not a record"
  in
  (* has_payload is irrelevant here: this reconstruction feeds the variant lowering +
     deparser synthesis (which key on the fields/tag), not mk_event's meta stamping. *)
  { evconstrid = ctor; evconstrnum = Some tag; evparams; is_packet = false; has_payload = false }
;;

let lower decls =
  let sigs = match find_variant_sigs decls with
    | Some sigs -> sigs
    | None -> failwith "[CCoreVariants] no variant type definition found"
  in
  let arms = List.map sig_to_event_def sigs in
  let struct_body = variant_struct_ty arms in
  (* the variant DTy now resolves to its lowered struct body, so the constructors'
     and matches' field projections type-check. *)
  register_tydef event_variant_cid struct_body;
  let lowered_dty = dty event_variant_cid struct_body in
  let tags = List.map decl_arm_tag arms in
  let ctors = List.map (variant_constr tevent_variant) arms in
  (* lower the variant DTy in place: tag constants, the lowered struct, then the
     constructors. Every other decl -- including the event envelope/meta records
     that wrap the variant -- is left untouched. *)
  let decls = List.concat_map
    (fun d -> match d.d with
      | DTy(cid, _) when Cid.equal cid event_variant_cid -> tags @ (lowered_dty :: ctors)
      | _ -> [d])
    decls
  in
  let arm_assoc = List.map (fun ed -> (ed.evconstrid, ed)) arms in
  transformer#visit_decls arm_assoc decls
;;
