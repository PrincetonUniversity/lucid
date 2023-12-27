open Yojson.Basic.Util
open Syntax
open TaggedCid

(* 1. annotate expressions that construct globals 
   with tagged cids that identify: 
    - the name of the global in the original source
    - the parent of the global in the source
    - the "type" of the global, via a string tag
   2. given a program with annotated expressions, 
      construct a directory that maps original global 
      names to current global names in the IR.

  stage 1 should be run after the first type checking, 
  stage 2 can be run at any point after *)

(* tags for taggedcids identifying different 
   types of globals in the directory *)
let arraytag = "array"
let tabletag = "table"
let actiontag = "action"
let tupletag = "tuple"
let recordtag = "record"
let unknowntag = "unknown"
let gty_to_tag ty = match (TyTQVar.strip_links ty.raw_ty) with
  | TName(cid, _, true) -> (
    match (Cid.names cid) with 
    | "Array"::_ -> (arraytag)
    | "PairArray"::_ -> (arraytag)
    | _ -> unknowntag

    (* error ("[gty_to_tag] unknown global constructor type: "^(Printing.ty_to_string ty)) *)
  )
  | TTable _ -> (tabletag)
  | TActionConstr _ -> (actiontag)
  | TRecord _ -> (recordtag)
  | TVector _ -> (tupletag)
  | TTuple _ -> (tupletag)
  (* definitely not a global *)
  | _ -> unknowntag
  (* error ("[gty_to_tag] unknown global constructor exp: "^(Printing.ty_to_string ty)) *)
;;

type user_constrs = (cid * exp) list

let annotate_espan exp (path:tcid option) tcid =
  let tcid' = {tcid with tparent=path;} in
  {exp with espan = {exp.espan with global_created_in_src = Some(tcid')}}
;;

let annotation_to_string exp = 
  match exp.espan.global_created_in_src with 
    | None -> "NO ANNOTATION"
    | Some(tcid) -> TaggedCid.to_string tcid
;;

let rec annotated_exp_to_string exp = 
  match exp.espan.global_created_in_src with 
    | None -> Printing.exp_to_string exp
    | Some(tcid) -> (
      let ancestors = TaggedCid.tcid_ancestors tcid in
      let path_str = "ROOT"::(List.map TaggedCid.to_string ancestors) in
      Printf.sprintf "%s.%s = %s"
      (String.concat "." path_str)
      (TaggedCid.to_string tcid)
      (Printing.exp_to_string exp)
    )
;;

let tcid_ancestors_str tcid = 
  TaggedCid.tcid_ancestors tcid |> List.map TaggedCid.to_string |> (String.concat ".")
;;


(* annotate the constructor expression constr_exp with the id of the
   global it creates (var_cid) and the path / scope of that global 
   in the program (var_path). user_constrs is an assoc of user-defined 
   constructors that constr_exp may call. *)
let rec globals_of_econstr user_constrs parent_tcid var_tcid constr_exp : (exp) =
  (* put the annotation on this constructor, then recurse through inner components *)
  let tag = gty_to_tag (Option.get constr_exp.ety) in
  let var_tcid = tytag var_tcid tag in
  let var_tcid = {var_tcid with tparent=parent_tcid;} in
  (* let annotated_constr_exp = annotate_espan constr_exp parent_tcid var_tcid in *)
  match constr_exp.e with
  | ECall(constr_cid, _, _) -> (
(*     print_endline ("[globals_of_econstr.ECall]");
    print_endline ("[globals_of_econstr.INPUT] "^(annotated_exp_to_string annotated_constr_exp)); *)
    match (Cid.names constr_cid) with
    | ["Array"; "create"] -> 
      annotate_espan constr_exp parent_tcid var_tcid
    | _ -> (
      match (List.assoc_opt constr_cid user_constrs) with
      | None -> 
        (* this is like c = mk_arr(), when mk_array is not in context. 
           What should we annotate that expresison with?  *)
        (* well.. we know the parent and the id... and the type has already been set...*)
        (* so maybe?  *)
        let res = annotate_espan constr_exp parent_tcid var_tcid in
        (* print_endline ("[globals_of_econstr.ECall.OUTPUT] "^(annotated_exp_to_string res)); *)
        res
        (* there's no user-defined constructor *)
      (* constr_exp  *)
      (* there's no user-defined constructor -- returns nothing *)
      | Some(constr_exp') -> (* there is a user-defined constructor, annotate that instead *)
        let annotated_inner_econstr = globals_of_econstr user_constrs parent_tcid var_tcid constr_exp' in
        (* note that we give back the original constructor with the new annotations *)
        {constr_exp with espan=annotated_inner_econstr.espan;}
        ))
  | ETableCreate(tbl) -> (
    (* print_endline ("[globals_of_econstr.ETableCreate]"); *)
    (* annotate the table constructor just like an array *)
    (* let fully_qualified_cid = cid_concats (var_path@[var_cid]) in *)
    (* but also, annotate the action references with their source names. 
       Note that action names are not type fields, but declared in 
       modules, like functions. (So var_path is not relevant) *)
    let tactions' = List.map
      (fun action -> 
        annotate_espan action None (SyntaxUtils.cid_of_exp action |> cid))
      tbl.tactions
    in 
    let e' = ETableCreate({tbl with tactions = tactions';}) in
    annotate_espan {constr_exp with e=e'} parent_tcid var_tcid
  )
  | ERecord(fields) -> (
(*     print_endline ("[globals_of_econstr.ERecord]");
    print_endline ("[globals_of_econstr.INPUT] "^(annotated_exp_to_string annotated_constr_exp)); *)
    (* the parent of every field is the outer var *)
    let parent = Some(var_tcid) in 
    (* recurse on each field *)
    let globals_of_field user_constrs field_name field_econstr =
      let field_cid = TaggedCid.create [field_name] in
      let constr_exp' = globals_of_econstr user_constrs parent field_cid field_econstr in
      (* globals is a list of globals created from this field... we need to prepend 
         the name of the record to it. *)
      constr_exp'
    in 
    let fields' = List.fold_left
      (fun (fields) (fieldname, fieldexp) -> 
        let fieldexp' = globals_of_field user_constrs fieldname fieldexp in
        let fields = fields@[(fieldname, fieldexp')] in
        fields) 
      ([])
      fields
    in 
    let res = annotate_espan {constr_exp with e=ERecord(fields')} parent_tcid var_tcid in
    (* print_endline ("[globals_of_econstr.ERecord] inner constructors:"); *)
    (* List.iter (fun (_, fe) -> print_endline (annotated_exp_to_string fe)) fields'; *)
    (* print_endline ("[globals_of_econstr.ERecord.OUTPUT] "^(annotated_exp_to_string res)); *)
    res)
  | EComp(constr, a, b) -> 
    (* print_endline ("[globals_of_econstr.EComp]"); *)
    (* print_endline ("[globals_of_econstr.INPUT] "^(annotated_exp_to_string annotated_constr_exp)); *)
    (* a vector comprehension. Pretend it is a vector with a single field named 
       index. This should get adjusted when the comprehension is unrolled. *)
    (* let fieldcid = (TaggedCid.create_ids [("INDEX", 0); ("[??]", 0)]) in *)
    let fieldcid = TaggedCid.create_ids [("", 0)] in
    (* sanity check: is the outer annotated with its parent? *)
    (* print_endline ("ancestors of outer EComp: "^(tcid_ancestors_str var_tcid)); *)
    let parent = Some(var_tcid) in (* parent is the outer variable... *)
    (* let inner_path = (var_path@[var_cid]) in  *)
    let constr' = globals_of_econstr user_constrs parent fieldcid constr in
    let res = annotate_espan {constr_exp with e=EComp(constr', a, b)} parent_tcid var_tcid in
    (* print_endline ("[globals_of_econstr.EComp] inner constructor: "^(annotated_exp_to_string constr')); *)
    (* print_endline ("[globals_of_econstr.EComp.OUTPUT] "^(annotated_exp_to_string res)); *)
    res
  | EVector(exps) -> 
    (* print_endline ("[globals_of_econstr.EVector]"); *)
    (* print_endline ("[globals_of_econstr.INPUT] "^(annotated_exp_to_string annotated_constr_exp)); *)
    (* vectors and tuples get annotated the same way *)
    (* a vector. Pretend each element in the vector is a field with its index as the name. *)
    let parent = Some(var_tcid) in 
    (* let inner_path = (var_path@[var_cid]) in  *)
    let fields = List.mapi (fun i exp -> (i, exp)) exps in
    let fieldcid = TaggedCid.create_ids [("", 0)] in
    let exps' = List.fold_left
      (fun (exps') (i, exp) -> 
        (* add index tag *)
        let fieldcid = TaggedCid.idxtag fieldcid i in
        (* let fieldcid = TaggedCid.create_ids [("["^(string_of_int i)^"]", 0)] in *)
        let exp' =globals_of_econstr user_constrs parent fieldcid exp in
        exps'@[exp'])
      ([])
      fields
    in
    let res = annotate_espan {constr_exp with e=EVector(exps')} parent_tcid var_tcid in
    (* print_endline ("[globals_of_econstr.EComp.OUTPUT] "^(annotated_exp_to_string res)); *)
    res
  | ETuple(exps) -> 
    (* print_endline ("[globals_of_econstr.ETuple]"); *)
    (* print_endline ("[globals_of_econstr.INPUT] "^(annotated_exp_to_string annotated_constr_exp)); *)
    (* vectors and tuples get annotated the same way *)
    (* a vector. Pretend each element in the vector is a field with its index as the name. *)
    let parent = Some(var_tcid) in 
    (* let inner_path = (var_path@[var_cid]) in  *)
    let fields = List.mapi (fun i exp -> (i, exp)) exps in
    let fieldcid = TaggedCid.create_ids [("", 0)] in
    let exps' = List.fold_left
      (fun (exps') (i, exp) -> 
        (* add index tag *)
        let fieldcid = TaggedCid.idxtag fieldcid i in
        (* let fieldcid = TaggedCid.create_ids [("["^(string_of_int i)^"]", 0)] in *)
        let exp' =globals_of_econstr user_constrs parent fieldcid exp in
        exps'@[exp'])
      ([])
      fields
    in
    let res = annotate_espan {constr_exp with e=ETuple(exps')} parent_tcid var_tcid in
    (* print_endline ("[globals_of_econstr.EComp.OUTPUT] "^(annotated_exp_to_string res)); *)
    res
  | _ -> 
    (* the thing we are constructing is not a global, so nothing to do *)
    constr_exp
  (* error "[globals_of_econstr] error: unexpected constructor expression form" *)
;;


(* annotate constructor expressions with the ids 
   of the globals that they construct in the source syntax *)
let annotate decls =
  let user_constrs :user_constrs ref = ref [] in
  let v = object
    inherit [_] s_map as super
    (* 
       
       after leaving a module M, we want to remember all the 
       constructors declared in M's interface with the name M.<constr_name> *)
(*     method! visit_DModule module_path id interface decls = 
      let original_user_constrs = !user_constrs in
      user_constrs := [];
      (* collect constructors from the module... *)
      let m = super#visit_DModule module_path id interface decls in
      (* for each constructor in the module's interface, 
         add the module's id to its name in the constructors map. 
       *)
      user_constrs := original_user_constrs;
      (* module should be unchanged -- no globals inside a module *)
      m
 *)
    (* remember the constructor *)
    method! visit_decl module_path decl =
      super#visit_decl module_path decl

    (* I don't think this is necessary -- somehow the 
       methods defined by the module never get found 
       anyway. They are not bound to the same cid as 
       the outer uses, probably outer uses a cid with 
       different number *)
    method! visit_DModule module_path id interface decls =
      let constrs = !user_constrs in
      (* update module path *)
      let module_path' = Some(match module_path with 
        | Some(module_path) -> Cid.concat module_path (Cid.id id)
        | None -> (Cid.id id))
      in
      (* recurse on inner *)
      let decls' = super#visit_decls module_path' decls in
      (* find the constructers declared in inner that 
         were also in the interface *)
      let constrs_in_module = !user_constrs in
      let constrs_in_interface = List.filter_map
        (fun intf_spec -> match intf_spec.ispec with
          | InConstr(id, _, _) -> (
            let public_cid = Cid.concat (Option.get module_path') (Cid.id id) in
            let res = 
              List.assoc_opt (Cid.id id) constrs_in_module
            in
            (* return the entry from the module bound to the 
               public name of the constructor *)
            match res with
            | Some(constr_exp) -> 
            Some(public_cid, constr_exp)
            | None -> 
                None
          )
          | _ -> None)
        interface
      in 
      (* return original constructors, plus those in the interface *)
      let constrs' = constrs@constrs_in_interface in
      user_constrs := constrs';
      DModule(id, interface, decls')

    method! visit_DConstr module_path id a b exp =
      user_constrs := (!user_constrs)@[(Cid.id id, exp)];
      super# visit_DConstr module_path id a b exp
    (* DGlobals -- fetch ids of materialized globals from this declaration.  *)
    method! visit_DGlobal module_path id ty exp =
      let exp' = globals_of_econstr 
        (!user_constrs) 
        None
        (TaggedCid.id id) 
        exp
      in
      super# visit_DGlobal module_path id ty exp';
    end
  in
  let decls' = v#visit_decls None decls in
  decls'
;;

(* update the annotation of a constructor body and all of its sub-expressions, 
   based on the annotation of its call. Assumes that the body does not 
   call any other user-defined constructors. *)
let reannotate_inlined_exp ecall einlined = 
  (* print_endline (Printf.sprintf "[reannotate_inlined_exp] call: (%s) inlined: (%s)\n" *)
    (* (Printing.exp_to_string ecall) (Printing.exp_to_string einlined) ); *)
  match (ecall.espan.global_created_in_src) with
  (* if call was not annotated, then it is not a global *)
  | None -> einlined
  (* | None -> error@@"[annotate_inlined_exp] cannot reannotate constructor expression ("^(Printing.exp_to_string ecall)^") -- it is not annotated to begin with." *)
  | Some(src_id) ->     
    globals_of_econstr [] src_id.tparent src_id einlined
;;



(* blob of debugging code *)
let debug_tagged_global_names decls =
  let cid_concats cids =
    match cids with
    | [] -> error "[cid_concats] trying to concatentate an empty list of cids"
    | cid::[] -> cid
    | cid::cids -> 
      List.fold_left Cid.concat
      cid
      cids
  in
  let inline_tcid_index tcid = 
    match (List.assoc_opt "index" tcid.tags) with
      | None -> tcid
      | Some(tagval) -> 
        {tcid with 
        tcid = match tcid.tcid with 
          | Cid.Id(n, idx) -> Cid.Id((n^"["^TaggedCid.tagval_to_string tagval^"]"), idx)
          | _ -> Cid.concat 
            tcid.tcid 
            (Cid.create ["["^TaggedCid.tagval_to_string tagval^"]"])}            
  in
  let rec fully_qualified_tcid tcid_path tcid = 
    let to_cids tcids = List.map to_cid tcids in
    let fq_cid = cid_concats (tcid_path@[tcid] |> to_cids) in
    {tcid with tcid=fq_cid}
  in
  let tagged_path_to_string tcid_path = 
    Printing.comma_sep TaggedCid.to_string tcid_path
  in
  (* map from global in current program -> qualified / tagged global in source *)
  let name_map = ref [] in
  let v = object
    inherit [_] s_iter as super
    method! visit_DGlobal () id ty exp =
      (* traverse interior for actions *)
      super#visit_DGlobal () id ty exp;
      match exp.espan.global_created_in_src with
      | None -> error "[debug_tagged_global_names] found a global without a source id tag!"
      | Some(src_tcid) -> 
        let src_path = List.map inline_tcid_index (tcid_ancestors src_tcid) in
        let fq_tcid = fully_qualified_tcid src_path src_tcid in
        print_endline ("source variable: ");
        print_endline (TaggedCid.to_string fq_tcid);
        print_endline ("path: ");
        print_endline (tagged_path_to_string src_path);
        name_map := (Cid.Id id, fq_tcid)::(!name_map);
    method! visit_ETableCreate () _ tactions _ _ = 
      let action_names = List.map 
        (fun eaction -> 
          SyntaxUtils.cid_of_exp eaction, Option.get eaction.espan.global_created_in_src)
        tactions
      in
      name_map := action_names@(!name_map);
    end
  in
  v#visit_decls () decls;
  !name_map
;;  





