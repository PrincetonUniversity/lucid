(* make user type declarations concrete. Assumes that 
   all _uses_ of user types are concrete, but the 
   declarations are not. This is true immediately before vector elimination, 
   which is when this pass should be run. *)

open Batteries
open Syntax
open SyntaxUtils
open Collections
   
let print_endline _ = ();;

let get_fieldnames fields = 
  (* order the field strings in some consistent way *)
  List.split fields |> fst |> List.sort compare
;;

let is_duserty_concrete id sizes ty = 
  let result = ref true in
  let checker = object (_)
    inherit [_] s_iter as super 
    method !visit_raw_ty () rty = 
      (* print_endline "visiting raw ty";
      print_endline (Printing.raw_ty_to_string rty);
      print_endline ("result = "^(string_of_bool result)); *)
      super#visit_raw_ty () rty
      (* ; *)
      (* print_endline ("result after = "^(string_of_bool result));
      () *)  
    method! visit_size () sz = 
      match (STQVar.strip_links sz) with 
      | IVar _ -> result := false
      | _ -> ()
    end
  in
  checker#visit_d () (DUserTy(id, sizes, ty));
  !result
;;

let is_tydecl_concrete (id, sizes, ty, _) = 
  (* print_endline ("checking if type declaration for "^(Printing.id_to_string id)^" is concrete."); *)
  is_duserty_concrete id sizes ty
;;

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

  (* a map from the field names of a declared user type (record type) to the type declaration *)
  let fieldnames_to_tydecl = Hashtbl.create 16 in
  let add_tydecl fieldnames tydecl = Hashtbl.add fieldnames_to_tydecl fieldnames tydecl in
  let get_tydecl fieldnames = Hashtbl.find fieldnames_to_tydecl fieldnames in

  (* a map from record types that appear in the program to: 
      1. record types with their labels renamed (so future occurrences can be replaced)
      2. a label rename map (for renaming labels that appear in expressions)
      3. a copy of the original type declaration (because once you rename the labels, its hard to infer original decl) *)
  let used_ty_renames = TypeHashTbl.create 16 in

  let add_renamed_concrete_ty raw_ty (raw_ty', label_rename, tydecl) = 
    print_hash_tbl used_ty_renames;
    print_endline ("adding used_ty_renames entry for raw type: "^(Printing.raw_ty_to_string raw_ty));
    TypeHashTbl.add used_ty_renames raw_ty (raw_ty', label_rename, tydecl);
  in  

  let find_renamed_concrete_ty raw_ty = 
    (* print_endline@@
    "looking for raw_ty = "^(Printing.raw_ty_to_string raw_ty);
    print_hash_tbl concrete_rec_tys; *)
    match TypeHashTbl.find_option used_ty_renames raw_ty with 
    | Some(raw_ty', _, _) ->  
      (* print_endline "found!";  *)
      Some(raw_ty')
    | None -> 
      None
  in

  let v = object (self) 
    inherit [_] s_map as skip
      method! visit_decl () decl = 
        print_endline ("DECL: ");
        print_endline (Printing.decl_to_string decl);
        skip#visit_decl () decl
      (* record the type declarations in a map from fieldnames -> type decls *)
      method! visit_DUserTy () id sizes ty = 
        match ty.raw_ty with 
        | TRecord(fields) ->           
          print_endline ("visiting declaration of user type "^(Printing.id_to_string id));
          print_endline ("ty: "^(Printing.ty_to_string ty));
          let is_conc = is_duserty_concrete id sizes ty in
          print_endline ("is it concrete? "^(string_of_bool is_conc));
          let fieldnames = get_fieldnames fields in
          (* if this is _not_ a concrete type, then we just add an entry for 
             the type declaration and we're done here *)
          if (not is_conc) then  (
            add_tydecl fieldnames (id, sizes, ty, ty.tspan);
            DUserTy(id, sizes, ty)
          )
          else
            (* if this _is_ a concrete type, its fields could still reference polymorphic types. 
              So, we (may) have to add concrete renames of types for some of the subfields *)
          (            
            print_endline ("here");            
            let fields = List.map 
              (fun (fieldname, fieldty) -> fieldname, self#visit_raw_ty () fieldty)
              fields
            in
            (* the declared types inner types might change  *)
            let ty = {ty with raw_ty = TRecord(fields)} in
            add_tydecl fieldnames (id, sizes, ty, ty.tspan);
            print_endline "done. Final:";
            print_endline (Printing.ty_to_string ty);
            DUserTy(id, sizes, ty)
          )
        | TName(_, _, _, _) -> 
          DUserTy(id, sizes, ty)
          (* a user _could_ rename another named type, but that named type 
             should be replaced by whatever it referenced by now *)
        | _ -> DUserTy(id, sizes, ty)
      
      (* rename labels in record types. 
          Each unique record type (that is, each record type with different concrete sizes),
          gets an entry with unique labels. *)
      method! visit_raw_ty () raw_ty = 
        print_endline ("visiting raw type: "^(Printing.raw_ty_to_string raw_ty));
        match raw_ty with
        | TRecord(label_rtys) -> (
          let fieldnames = get_fieldnames label_rtys in
          (* the declared record type is concrete -- that means we don't have to recurse
              because everything inside the type must be concrete as well *)
          let tydecl = get_tydecl fieldnames in
          if (is_tydecl_concrete (tydecl)) then (
            (* this be a concrete type. But... its fields could be 
               polymorphic types that have been inlined, so we recurse *)            
            print_endline ("raw_ty references a CONCRETE user type");
            skip#visit_raw_ty () raw_ty
            (* raw_ty *)
          )
          else (
            print_endline ("raw_ty references a POLYMORPHIC user type");
            let (id, sizes, ty, _) = tydecl in
            print_endline (Printing.d_to_string (DUserTy(id, sizes, ty)));

            (* check to see if there's a renamed instance of this type available already *)
            match (find_renamed_concrete_ty raw_ty) with 
              | Some(raw_ty') -> raw_ty'
              (* if not, we need to create the concrete type with renamed labels *)
              | None -> (
                (* find the declaration for this type based on its field names *)
                let tydecl = Hashtbl.find fieldnames_to_tydecl fieldnames in  
                (* update / refresh the labels and visit inner *)
                let label_rtys = List.map (fun (label, lty) -> label, self#visit_raw_ty () lty) label_rtys in
                let label_rtys' = relabel label_rtys in
                
                (* the new raw type *)
                let raw_ty' = TRecord(label_rtys') in
                (* the rename map, for transforming labels that appear in expression *)
                let label_rename = 
                  List.combine
                  (fst (List.split label_rtys))  
                  (fst (List.split label_rtys'))  
                in
                print_endline "RELABELED CONCRETE TYPE THAT REFS POLY TYPE";
                (* List.iter 
                  (fun (o, n) -> 
                    Printf.printf "%s -> %s\n" o n)
                  label_rename; *)
                add_renamed_concrete_ty raw_ty (raw_ty', label_rename, tydecl);   
                raw_ty'
            )
          )
        )
        | _ -> (* this is not a record type, but it could have a poly type in it *) 
          skip#visit_raw_ty () raw_ty
      (* method! visit_DGlobal () id ty exp = 
        let d = DGlobal(id, ty, exp) in 
        print_endline ("visiting global "^(Printing.d_to_string d));
        let new_ty = self#visit_ty () ty in
        let new_exp = self#visit_exp () exp in
        (* exit 1; *)
        DGlobal(id, new_ty, new_exp) *)

      method! visit_exp () exp = 
        print_endline@@"visiting EXP: "^(Printing.exp_to_string exp);
        print_endline@@"exp form: "^SyntaxUtils.e_to_constr_str exp.e;
        print_endline@@"exp type: "^(Printing.ty_to_string (Option.get exp.ety));
        let res = match exp.e with           
          | ERecord(label_exps) -> 
            (* only relabel if this is a polymorphic declared type *)
            let fieldnames = get_fieldnames label_exps in
            if (not (is_tydecl_concrete (get_tydecl fieldnames))) then (
              let ty_id, _, _, _ = get_tydecl fieldnames in
              print_endline ("POLYMORPHIC RECORD EXPRESSION");
              print_endline ("inner expression is polymorphic: "^(Printing.id_to_string ty_id));
              (* recurse on the outer type *)
              let rec_ty = match exp.ety with | None -> error "untyped expression" | Some(r) -> r in
              let rec_ty' = self#visit_ty () rec_ty in
              (* now use the _original_ record type to get the relabelings *)
              let _, label_renames, _ = TypeHashTbl.find used_ty_renames rec_ty.raw_ty in
              (* relabel and also visit the inner exp *)
              let label_exps = List.map 
                (fun (label, exp) -> (List.assoc label label_renames, self#visit_exp () exp))
                label_exps
              in
              print_endline "RELABELED EXPRESSION FIELDS";
              (* List.iter 
                (fun (o, n) -> 
                  Printf.printf "%s -> %s\n" o n)
                label_renames; *)

              (* return updated expression and type *)
              {exp with e=ERecord(label_exps); ety=Some(rec_ty')}
            )
            else(
              (* this is a concrete type, but its members could be a polymorphic type *)
              print_endline ("CONCRETE RECORD EXPRESSION");              
              skip#visit_exp () exp
            )
          | EComp(inner_exp, id, size) -> (
            (* for comprehensions, we might change the effect of the inner type, 
              so we refresh it and let the type checker straighten it out. *)
            let inner_ety_eff = (Option.get inner_exp.ety).teffect in
            let inner_exp = self#visit_exp () inner_exp in
            print_endline@@"ECOMP OLD: "^(Printing.exp_to_string exp);
            let exp = {exp with e=EComp(inner_exp, id, size)} in
            print_endline@@"ECOMP NEW: "^(Printing.exp_to_string exp);
            refresh_effect#visit_exp inner_ety_eff exp
          )
          | EWith(rec_exp, label_exps) -> (
            (* first of all, recurse on the record *)
            print_endline ("recursing on: "^(Printing.exp_to_string rec_exp));
            let old_rec_exp_raw_ty = (Option.get rec_exp.ety).raw_ty in 
            let rec_exp = self#visit_exp () rec_exp in
            (* get the _original_ fieldnames to look up the type declaration. *)
            (* let fieldnames = match ((Option.get rec_exp.ety).raw_ty) with  *)
            let fieldnames = match old_rec_exp_raw_ty with 
              | TRecord(fields) -> get_fieldnames fields
              | _ -> error "err"
            in
            (* if the field names belong to a concrete type declaration *)
            print_endline (String.concat ", " fieldnames);
            if (is_tydecl_concrete (get_tydecl fieldnames)) then (
              (* a concrete type declaration means that the labels don't change, but 
                 their attached expressions may change, so recurse. *)
              let label_exps = List.map (fun (lbl, exp) -> (lbl, self#visit_exp () exp)) label_exps in
              {exp with e=EWith(rec_exp, label_exps)}
            )
            else (
              (* if the field names belong to a polymorphic type declaration, then 
                 the labels change, and also the inner expressions may change *)
              let _, label_renames, _ = TypeHashTbl.find used_ty_renames old_rec_exp_raw_ty in          
              let label_exps = List.map 
              (fun (label, exp) -> 
                (* relabel and also visit the inner exp *)
                (List.assoc label label_renames, self#visit_exp () exp))
              label_exps
            in
            {exp with e=EWith(rec_exp, label_exps)}
            )
          )
          | EProj(rec_exp, label) -> (
            let original_rec_rty = (Option.get rec_exp.ety).raw_ty in
            (* only relabel if the inner record exp is a polymorphic declared type *)       
            match original_rec_rty with 
              | TRecord(fields) -> (
                let fieldnames = get_fieldnames fields in
                (*recurse on outer type *)
                let new_ety = match exp.ety with 
                  | None -> None
                  | Some(ety) -> Some(self#visit_ty () ety)
                in
                (* recurse on inner record *)
                let rec_exp = self#visit_exp () rec_exp in
                  
                (* rename the label argument if its polymorphic *)
                let new_label = if (is_tydecl_concrete (get_tydecl fieldnames)) 
                  then (label)
                  else (
                    print_endline ("[EProj] the type "^(Printing.raw_ty_to_string original_rec_rty)^" is polymorphic");

                    let _, label_renames, _ = TypeHashTbl.find used_ty_renames original_rec_rty in
                    let new_label = List.assoc label label_renames in 
                    new_label)
                in
                {exp with e=EProj(rec_exp, new_label); ety=new_ety}
                ) 
              | _ -> error "internal compiler error: projection from non-record type"
          )
          | _ -> skip#visit_exp () exp
        in
        print_endline@@"DONE visiting EXP: "^(Printing.exp_to_string exp);
        print_endline@@"exp form: "^SyntaxUtils.e_to_constr_str exp.e;
        print_endline@@"exp type: "^(Printing.ty_to_string (Option.get exp.ety));
        print_endline "---------------";
        res
    end
  in
  (* relabel record type fields in the program *)
  let ds = v#visit_decls () ds in
  (* now we make declarations for all the concrete user types we created by doing the relabeling. 
     Put the type declarations in a map from original polymorphic type to concrete types. *)
  let poly_userty_to_concrete_decls = TypeHashTbl.fold 
    (fun _ (new_rec_ty, _, (id, sizes, ty, tspan)) poly_userty_to_concrete_decls -> 
        (* if the declared user type is concrete, we don't add anything *)
        if (is_tydecl_concrete (id, sizes, ty, tspan)) then 
          poly_userty_to_concrete_decls
        else
        (* ignore sizes in original declaration *)
        let concrete_id = Id.create (refresh_string (Id.name id)) in
        (* keep all the annotations on the raw type from the original *)
        let ty = {ty with raw_ty = new_rec_ty} in
        (* construct the declaration *)
        let concrete_duserty = duty_sp concrete_id [] ty tspan in
        (* put the new duserty in the map for the original id *)
        match IdMap.find_opt id poly_userty_to_concrete_decls with 
          | None -> IdMap.add id [concrete_duserty] poly_userty_to_concrete_decls
          | Some(ds) -> IdMap.add id (ds@[concrete_duserty]) (IdMap.remove id  poly_userty_to_concrete_decls)
        (* new_decls@[duty_sp id sizes ty tspan] *)
    )
    used_ty_renames 
    (IdMap.empty)
  in
  (* finally, replace all the polymorphic user types with declarations of all the concrete 
     instances that we created. *)
  List.fold_left 
    (fun decls decl -> 
      match decl.d with 
        | DUserTy(id, sizes, ty) -> (
          (* if the user type declaration is concrete, we keep it *)
          if (is_tydecl_concrete (id, sizes, ty, decl.dspan)) then (
            print_endline@@"DECISION FOR: "^(Printing.decl_to_string decl)^" -> KEEP CONCRETE";
            decls@[decl])
          else            
            let concrete_tydecl_opts = IdMap.find_opt id poly_userty_to_concrete_decls in
            match concrete_tydecl_opts with 
            | None -> 
              print_endline@@"DECISION FOR: "^(Printing.decl_to_string decl)^" -> DELETE POLYMORPHIC";
              (* this is a polymorphic user type declaration that is never used. 
                   So its safe to delete without adding and concrete replacements. *) 
              decls
            | Some(concrete_tydecls) -> 
              print_endline@@"DECISION FOR: "^(Printing.decl_to_string decl)^" -> REPLACE POLYMORPHIC";
              List.iter 
                (fun decl -> print_endline (Printing.decl_to_string decl))
                concrete_tydecls;
              print_endline ("--------");
              decls@concrete_tydecls
        )
        (* not a user type declaration *)
        | _ -> decls@[decl]
      )
    []
    ds
;;

let replace_prog ds = concretize_user_ty_decls ds
;;