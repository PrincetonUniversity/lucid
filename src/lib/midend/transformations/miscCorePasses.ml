open CoreSyntax
(* delete noops *)
let noop_deleter = 
  let rec unfold_stmts (st : statement) =
    match st.s with
    | SNoop -> []
    | SSeq (s1, s2) -> unfold_stmts s1 @ unfold_stmts s2
    | _ -> [st]
  in
  let rec sequence_stmts lst =
    match lst with
    | [] -> snoop
    | { s = SNoop } :: tl -> sequence_stmts tl
    | [hd] -> hd
    | hd :: tl -> sseq hd (sequence_stmts tl)
  in
  
  let rec fold_stmts (sts : statement list) : statement = sequence_stmts sts
  in
  
  (* remove noops *)
  let rec strip_noops statement : statement = 
    (* turn into a list *)
    let statements = unfold_stmts statement in
    (* recurse on if and match branches *)
    let statements = List.map 
      (fun s -> match s.s with 
        | SIf (e, s1, s2) -> {s with s=SIf(e,(strip_noops s1),(strip_noops s2))}
        | SMatch (e, branches) -> {s with s=SMatch(e,(List.map (fun (p, s) -> (p, strip_noops s)) branches))}
        | _ -> s) statements in
    (* turn back into a sequence *)
    fold_stmts statements
  in
    object inherit [_] s_map
    method! visit_statement () stmt = 
      strip_noops stmt
    end
;;

let this_eliminator = 
  object (self) inherit [_] s_map
    val mutable contains_this = false
    method! visit_EVar () cid = 
      if (Cid.equal cid (Cid.id Builtins.this_id)) then 
        contains_this <- true;
      EVar(cid)
    method! visit_decl () decl = 
      match decl.d with 
      | DHandler(id, sort, body) -> 
        contains_this <- false;
        let (params, stmt) = self#visit_body () body in
        let param_vars = List.map 
          (fun (id, ty) -> var (Cid.id id) ty)
          params
        in
        let declare_this = slocal 
          Builtins.this_id 
          tevent 
          (call (Cid.id id) param_vars tevent) 
        in
        let stmt = sseq declare_this stmt in
        {decl with d = DHandler(id, sort, (params, stmt))}
      | _ -> decl    
  end     
;;

let pack_hash_args = 
  object (_) inherit [_] s_map as super
    method! visit_exp () exp = 
      let exp = super#visit_exp () exp in
      match exp.e with 
      | EHash(size, seed::args) -> (
        match args with 
          | [] -> error "wrong number of hash args"
          | [_] -> exp
          | args ->  
            let tuple_ty = ttuple@@List.map (fun arg -> arg.ety.raw_ty) args in
            let tuple_exp = CoreSyntax.exp (ETuple(args)) tuple_ty in
            {exp with e=EHash(size, [seed; tuple_exp]) }
      )
      | _ -> exp
  end
;;
