open Batteries
open Syntax
open SyntaxUtils
open Collections

(* Remove all vectors from the program by transforming them into tuples.
   Assumes functions have already been eliminated, so we don't have
   any more polymorphic vector sizes. *)

(* WARNING: It's convenient to run this before record elimination, but we
   might have problems if a record contains a vector of polymorphic length. All
   record _uses_ should be concretized, but record type declarations won't be.
   We take the lazy approach to get around this and simply remove all DUserTys,
   but this technically leaves the program in an ill-formed state until we run
   record elimination, because we are now using record types without a corresponding
   declaration. As long as we don't run the typechecker in between, it should be
   fine. The "right" way to do this is to duplicate the DUserTy once for each
   set of polymorphic values, but that's a loooooot of work. *)

let extract_size sz =
  match normalize_size sz with
  | IConst n -> n (* Should be the only possible outcome *)
  | IUser cid ->
    failwith
    @@ "Illegal vector length (user variable): "
    ^ Printing.cid_to_string cid
  | ISum _ -> failwith "Got sum after normalize_size?"
  | IVar _ -> failwith "Got polymorphic vector length during elimination"
;;

let subst_index =
  object
    inherit [_] s_map

    method! visit_IUser (target, sz) cid =
      if Cid.equal target cid then sz else IUser cid
  end
;;

let replacer =
  object (self)
    inherit [_] s_map

    method! visit_TVector env rty sz =
      let length = extract_size sz in
      let rty = self#visit_raw_ty env rty in
      TTuple (List.init length (fun _ -> rty))

    method! visit_EVector env es = ETuple (List.map (self#visit_exp env) es)

    method! visit_EIndex env e sz =
      let idx = extract_size sz in
      let e = self#visit_exp env e in
      let length =
        match (Option.get e.ety).raw_ty with
        | TTuple lst -> List.length lst
        | _ -> failwith "Impossible"
      in
      EOp (TGet (length, idx), [e])

    method! visit_SLoop env body i sz =
      let body = body in
      let length = extract_size sz in
      let bodies =
        List.init length (fun n ->
            let subst_body =
              subst_index#visit_statement (Id i, IConst n) body
            in
            self#visit_statement env subst_body)
      in
      let seq = List.fold_left sseq snoop bodies in
      seq.s

    method! visit_EComp env body i sz =
      let length = extract_size sz in
      let bodies =
        List.init length (fun n ->
            let subst_body = subst_index#visit_exp (Id i, IConst n) body in
            self#visit_exp env subst_body)
      in
      ETuple bodies

    (* Once vectors are unrolled, these should all be immediate operations *)
    method! visit_ESizeCast _ sz1 sz2 =
      EInt (Z.of_int (extract_size sz2), Some sz1)
  end
;;

let eliminate_prog ds =
  ds
  |> List.filter (function
         | { d = DUserTy _ } -> false
         | _ -> true)
  |> replacer#visit_decls ()
;;
