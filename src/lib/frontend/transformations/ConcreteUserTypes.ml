(* make user type declarations concrete. Assumes that 
   all _uses_ of user types are concrete, but the 
   declarations are not. This should be true 
   _immediately before_ vector elimination, and this 
   pass should be run immediately before it. *)

open Batteries
open Syntax
open SyntaxUtils
open Collections
   
module TypeHash = struct
  type t = raw_ty
  let equal = SyntaxUtils.equiv_raw_ty ~ignore_effects:false ~qvars_wild:false
  let hash = (fun _ -> 1)
end
module TypeHashTbl = Hashtbl.Make(TypeHash)

let print_hash_tbl tbl =
  print_endline "----current contents of hash table----";
  TypeHashTbl.iter (fun k (v1, _, _) -> 
    print_endline ("key: "^(Printing.raw_ty_to_string k)^" value: "^(Printing.raw_ty_to_string v1))) tbl;
  print_endline "-------------------------";
;;
let num = ref (-1);;
let refresh_string s = 
  num := (!num) + 1;
  s^"_"^(string_of_int !num)
;;
let relabel label_rtys = 
  List.map (fun (s, raw_ty) -> (refresh_string s, raw_ty)) label_rtys
;;

let refresh_effect =
  object (_) 
  inherit [_] s_map as super  

  method! visit_effect tgt_effect effect = 
    if (equiv_effect tgt_effect effect)
    then TyperUtil.fresh_effect ~name:"concrete_user_ty" ()
    else effect
  end  
;;

let concretize_user_ty_decls ds =   
  (*  
    create DUserTys for all the record labels used in the program, which should be concrete. 
    This _should_ let the program type check with vectors, but not records, eliminated. 

    The main task is re-labeling each record type so that the declarations don't shadow each other. 
    Algorithm: 
      - Maintain a map from record type -> (relabeled record type, label renaming map)
      - At each record type: 
        - recurse on each inner type, then:
        check the map for the relabeled type (maybe created by a prior visit)
        if none exists, construct one and add it to the map
        - return updated record type
      - at each expression: 
        - if its a ERecord, EWith, or EProj:
          - recurse on each inner expression
          - recurse on ety
          - get label renaming map from type renaming map
          - replace labels
      - Finally, add all the concrete type declarations to the program, 
        replacing the original ones.
  *)

  let concrete_rec_tys = TypeHashTbl.create 16 in
  let label_to_userty = Hashtbl.create 16 in

  let find_concrete_rec_ty raw_ty = 
    (* print_endline@@
    "looking for raw_ty = "^(Printing.raw_ty_to_string raw_ty);
    print_hash_tbl concrete_rec_tys; *)
    match TypeHashTbl.find_option concrete_rec_tys raw_ty with 
    | Some(raw_ty', _, _) ->  
      (* print_endline "found!";  *)
      Some(raw_ty')
    | None -> 
      None
  in
  let add_new_rec_ty raw_ty (raw_ty', label_rename, orig_ty_opt) = 
    (* print_hash_tbl concrete_rec_tys; *)
    (* print_endline ("adding new_rec_tys entry for raw type: "^(Printing.raw_ty_to_string raw_ty)); *)
    TypeHashTbl.add concrete_rec_tys raw_ty (raw_ty', label_rename, orig_ty_opt);
  in         
  let v = object (self) 
    inherit [_] s_map as skip

      (* build a mapping from field label -> type id
         also, skip named type declarations *)
      method! visit_DUserTy () id sizes ty = 
        match ty.raw_ty with 
        | TRecord(fields) ->           
          List.iter 
            (fun fieldname -> Hashtbl.add label_to_userty fieldname (id, sizes, ty, ty.tspan))
            (fst (List.split fields))
          ;
          DUserTy(id, sizes, ty)
        | TName(_, _, _) -> 
          DUserTy(id, sizes, ty)
          (* a user _could_ rename another named type, but that named type 
             should be replaced by whatever it referenced by now *)
        | _ -> DUserTy(id, sizes, ty)
      

      method! visit_raw_ty () raw_ty = 
        match raw_ty with
        | TRecord(label_rtys) -> (
          (* recurse on label types *)
          let label_rtys_orig = label_rtys in 
          let label_rtys = List.map (fun (label, lty) -> label, self#visit_raw_ty () lty) label_rtys in
          (* check in tbl *)
          let raw_ty_opt = find_concrete_rec_ty (TRecord(label_rtys_orig)) in
          match raw_ty_opt with 
            (* the concrete raw type was found *)
            | Some(raw_ty') -> raw_ty'
            | _ -> 
              (* relabel *)
              let label_rtys' = relabel label_rtys in
              let raw_ty' = TRecord(label_rtys') in
              let label_rename = 
                List.combine
                (fst (List.split label_rtys))  
                (fst (List.split label_rtys'))  
              in  
              (* update table *)
              (* try to infer the name of the type *)
              let fst_label = List.split label_rtys |> fst |> List.hd in
              let orig_ty_opt = Hashtbl.find_option label_to_userty fst_label in
              
              add_new_rec_ty raw_ty (raw_ty', label_rename, orig_ty_opt);   
              (* return new type *)
              raw_ty'
        )
        | _ -> skip#visit_raw_ty () raw_ty
      method! visit_DGlobal () id ty exp = 
        let new_ty = self#visit_ty () ty in
        let new_exp = self#visit_exp () exp in
        DGlobal(id, new_ty, new_exp)

      method! visit_exp () exp = 
        match exp.e with           
        | ERecord(label_exps) -> 
          (* recurse on the type *)
          let rec_ty = match exp.ety with | None -> error "untyped expression" | Some(r) -> r in
          (* recurse on the record type *)
          let rec_ty' = self#visit_ty () rec_ty in
          (* now use the _original_ record type to get the relabelings *)
          let _, label_renames, _ = TypeHashTbl.find concrete_rec_tys rec_ty.raw_ty in
          (* relabel and also visit the inner exp *)
          let label_exps = List.map 
            (fun (label, exp) -> (List.assoc label label_renames, self#visit_exp () exp))
            label_exps
          in
          (* return updated expression and type *)
          {exp with e=ERecord(label_exps); ety=Some(rec_ty')}
        | EComp(inner_exp, id, size) -> (
          (* for comprehensions, we might change the effect of the inner type, 
             so we refresh it and let the type checker straighten it out. *)
          let inner_ety = Option.get inner_exp.ety in
          let inner_ety_eff = inner_ety.teffect in
          let inner_exp = self#visit_exp () inner_exp in
          let exp = {exp with e=EComp(inner_exp, id, size)} in
          refresh_effect#visit_exp inner_ety_eff exp
        )
        | EWith(rec_exp, label_exps) -> 
          let orig_rec_ety = rec_exp.ety in
          let rec_exp = self#visit_exp () rec_exp in
          let ety = rec_exp.ety in (* use the records type, which should be updated *)
          let _, label_renames, _ = TypeHashTbl.find concrete_rec_tys (Option.get orig_rec_ety).raw_ty in          
          let label_exps = List.map 
            (fun (label, exp) -> 
              (* relabel and also visit the inner exp *)
              (List.assoc label label_renames, self#visit_exp () exp))
            label_exps
          in
          {exp with e=EWith(rec_exp, label_exps); ety}
        | EProj(rec_exp, label) ->
          let orig_rec_ety = rec_exp.ety in
          let rec_exp = self#visit_exp () rec_exp in
          let _, label_renames, _ = TypeHashTbl.find concrete_rec_tys (Option.get orig_rec_ety).raw_ty in
          let new_label = List.assoc label label_renames in
          (* update the label type *)
          let new_ety = 
            let new_rawty = match (Option.get rec_exp.ety).raw_ty with 
              | TRecord(label_rtys) -> List.assoc new_label label_rtys
              | _ -> error "[EProj] expected record type for record operand of projection expression"
            in
            Some({(Option.get(exp.ety)) with raw_ty= new_rawty})
          in
          {exp with e=EProj(rec_exp, new_label); ety =new_ety}
        | _ -> skip#visit_exp () exp
    end
  in
  let ds = v#visit_decls () ds in
  (* declare the new named types and put them in a map from original type id -> new ones *)
  let userty_to_new_decls = TypeHashTbl.fold 
    (fun _ (new_rec_ty, _, orig_ty_opt) userty_to_new_decls -> 
      match orig_ty_opt with 
      | Some(id, _, ty, tspan) -> (
        (* ignore sizes *)
        let concrete_id = Id.create (refresh_string (Id.name id)) in
        let ty = {ty with raw_ty = new_rec_ty} in
        let concrete_duserty = duty_sp concrete_id [] ty tspan in
        (* put the new duserty in the map for the original id *)
        match IdMap.find_opt id userty_to_new_decls with 
        | None -> IdMap.add id [concrete_duserty] userty_to_new_decls
        | Some(ds) -> IdMap.add id (ds@[concrete_duserty]) (IdMap.remove id  userty_to_new_decls)
        (* new_decls@[duty_sp id sizes ty tspan] *)
      )
      | _ -> error "could not find the type that a record label belongs to")
    concrete_rec_tys
    (IdMap.empty)
  in
  (* finally, delete all the original user type declarations and replace 
     them with the concrete ones *)
  List.fold_left 
    (fun decls decl -> 
      match decl.d with 
        | DUserTy(id, _, _) -> (
          let concrete_tydecl_opts = IdMap.find_opt id userty_to_new_decls in
          match concrete_tydecl_opts with 
          | None -> decls (*if there's no concrete declarations... then we still don't need it! *)
          | Some(concrete_tydecls) -> 
            decls@concrete_tydecls
        )
        | _ -> decls@[decl]
      )
    []
    ds
;;

let replace_prog ds = concretize_user_ty_decls ds
;;