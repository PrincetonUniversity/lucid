open Batteries
open Syntax
open SyntaxUtils
open Collections

(* Remove all vectors from the program by transforming them into tuples.
   Assumes functions have already been eliminated, so we don't have
   any more polymorphic vector sizes. 
   Also assumes that all user type declarations have been made concrete. *)

let extract_size sz =
  match normalize_size sz with
  | IConst n -> n (* Should be the only possible outcome *)
  | IUser cid ->
    failwith
    @@ "Illegal vector length (user variable): "
    ^ Printing.cid_to_string cid
  | ISum _ -> failwith "Got sum after normalize_size?"
  | IVar _ -> failwith "Got polymorphic vector length during elimination"
  | ITup _ -> failwith "Got size tuple for vector length during elimination"
;;

let subst_index =
  object
    inherit [_] s_map

    method! visit_IUser (target, sz) cid =
      if Cid.equal target cid then sz else IUser cid
  end
;;

(* refresh the effects on each element of a tuple. 
   Use on the tuple generated from comprehension unrolling. 
   Necessary because a tuple element generated from a 
   comprehension unrolling might have the comp's 
   index effect bound to it. *)
   let refresh_tup_effects exp =    
    let refresh_effect =
      object (_) 
      inherit [_] s_map as super  
    
      method! visit_effect tgt_effect effect = 
        if (equiv_effect tgt_effect effect)
        then TyperUtil.fresh_effect ~name:"comp_unroll" ()
        else effect
      end  
    in  
    match exp.e with 
    | ETuple(exps) -> 
      let exps' = List.map
        (fun exp -> 
          let eff = (Option.get exp.ety).teffect in
          refresh_effect#visit_exp eff exp)
        exps
      in
        {exp with e = ETuple(exps')}
    | _ -> exp
  ;;

let replacer =
  object (self)
    inherit [_] s_map as super

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

    method! visit_exp env exp = 
      (* re-annotate comprehensions after expansion to set proper index *)
      match exp.e with
      | EComp(_) -> (
        super#visit_exp env exp |>
        GlobalConstructorTagging.reannotate_inlined_exp exp |> 
        refresh_tup_effects
      )
      | _ -> super#visit_exp env exp

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

let eliminate_prog = (replacer#visit_decls ())
(* note: before concreteUserTypes pass, we just deleted 
   all user type definitions at this point, to get around 
   polymorphic user type definitions. This meant that the 
   program did not type check from this point 
   until after tuples were eliminated, though.  *)
  (* |> List.filter (function
         | { d = DUserTy _ } -> false
         | _ -> true) *)

;;
