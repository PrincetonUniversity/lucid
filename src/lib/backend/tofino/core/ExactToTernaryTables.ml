(* Convert exact tables (Table) into ternary tables (TernaryTable).
   The tofino backend only implements ternary tables. An exact table
   is a ternary table whose entries all have a full mask and the
   same priority. After this pass, a program has no Table types
   and no calls to Table functions. *)
open CoreSyntax

(* the TernaryTable function named fname *)
let ternary_cid fname =
  List.find
    (fun cid -> List.nth (Cid.names cid) 1 = fname)
    TernaryTables.function_cids
;;

(* a mask that covers every bit of a key *)
let rec full_mask (key_rty : raw_ty) : exp =
  match key_rty with
  | TInt(Sz sz) -> value_to_exp (vinteger (Integer.max_int sz))
  | TBool -> value_to_exp (vbool true)
  | TTuple(key_rtys) -> tup_sp (List.map full_mask key_rtys) Span.default
  | _ -> error "[ExactToTernaryTables] a table key is not an int, bool, or tuple"
;;

let process ds =
  let v =
    object (self)
      inherit [_] s_map as super

      method! visit_TBuiltin ctx cid raw_tys =
        let cid = if (Cid.equals cid Tables.t_id) then TernaryTables.t_id else cid in
        super#visit_TBuiltin ctx cid raw_tys

      method! visit_ECall ctx fcid args unordered =
        let args = List.map (self#visit_exp ctx) args in
        let mask (key : exp) = full_mask key.ety.raw_ty in
        let prio = vint_exp Tables.exact_priority 32 in
        match Cid.names fcid, args with
        | ["Table"; ("create" | "lookup" as fname)], _ ->
          ECall(ternary_cid fname, args, unordered)
        | ["Table"; ("install" | "update" as fname)], [tbl; key; acn; acn_arg] ->
          ECall(ternary_cid fname, [tbl; key; mask key; prio; acn; acn_arg], unordered)
        | ["Table"; "install_ternary"], [tbl; key; mask; acn; acn_arg] ->
          ECall(ternary_cid "install", [tbl; key; mask; prio; acn; acn_arg], unordered)
        | ["Table"; "remove"], [tbl; key] ->
          ECall(ternary_cid "remove", [tbl; key; mask key; prio], unordered)
        | "Table"::_, _ ->
          error ("[ExactToTernaryTables] unexpected call to "^(CorePrinting.cid_to_string fcid))
        | _ -> ECall(fcid, args, unordered)
    end
  in
  v#visit_decls () ds
;;
