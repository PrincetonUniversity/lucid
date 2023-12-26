(* build the global directory from CoreSyntax. 
   Assumes that all constructors have been annotated by 
   GlobalConstructorTagging.annotate in the frontend. *)
open Syntax
open TaggedCid
open SyntaxGlobalDirectory
module C = CoreSyntax


(*** coresyntax -> mat_global constructors ***)
let core_exp_to_arrmeta id (exp:C.exp) = 
  let name = match (exp.espan.global_created_in_src) with
    | None -> error "[exp_to_arrmeta] array constructor is not annotated"
    | Some(tcid) -> tcid.tcid 
  in
  let compiled_cid = (Cid.id id) in
  let length = (match exp.e with
    | ECall(_, len_exp::_, _) -> C.exp_to_int len_exp
    | _ -> error "[core_exp_to_arrmeta] array constructor has wrong form")
  in
  let cell_size = match exp.ety.raw_ty with
    | TName(_, sizes, true) -> sizes
    | _ -> error "[core_exp_to_arrmeta] array constructor has unexpected type"
  in
  let arr = {name; compiled_cid; length; cell_size} in
  Arr(arr)
;;

let core_exp_to_tblmeta id (exp : C.exp) = 
  let user_key sz = {kid = None; kty = "ternary"; ksize = sz;} in
  let priority_key = 
    {kid = Some(Cid.create ["$MATCH_PRIORITY"]); kty = "exact"; ksize = 32}
  in
  let evar_to_action (exp : C.exp) = 
    let aid = match exp.espan.global_created_in_src with
      | None -> error "[evar_to_action] action expression not annotated with source id"
      | Some(src_tcid) -> src_tcid.tcid
    in
    let acompiled_id = match exp.e with
      | EVar(compiled_id) -> compiled_id
      | _ -> error "[evar_to_action] expected an action variable"
    in
    let arg_sizes = match exp.ety.raw_ty with
      | TActionConstr(aty) -> List.map C.ty_to_size aty.aconst_param_tys
      | _ -> error "[action_to_argsizes] action exp is not an action type"
    in
    {aid; acompiled_id; arg_sizes}
  in  
  let keys = match exp.ety.raw_ty with
    | TTable(tty) -> (List.map user_key tty.tkey_sizes)@[priority_key] 
    | _ -> error "[exp_to_tblmeta] expression is not a table type"
  in
  let actions, length = match exp.e with
    | ETableCreate(tbl) -> (
      List.map evar_to_action tbl.tactions,
      match tbl.tsize.e with 
        | EVal({v=VInt(z); _}) ->
          Integer.to_int z
        | _ -> error "[exp_to_tblmeta] table size expression is not an EVal(EInt(...))")
    | _ -> error "[exp_to_tblmeta] expression is not a table create"
  in
  let compiled_cid = (Cid.id id) in 
  let name = match exp.espan.global_created_in_src with
    | None -> error "[exp_to_tblmeta] table constructor not annotated with source id"
    | Some(src_tcid) -> src_tcid.tcid
  in
  Tbl{name; compiled_cid; length; keys; actions;}
;;

let build_coredirectory (decls:C.decls) = 
  let subtrees = ref [] in
  let v = object
    inherit [_] C.s_iter as super
      method! visit_DGlobal () id ty exp =
        (* only considering arrays and tables for now *)
        let proceed = match ty.raw_ty with
          | TName(cid, _, true) -> (
            match (Cid.names cid) with
            | "Array"::_ -> true
            | _ -> false
          )
          | TTable(_) -> true
          | _ -> false
        in
        if (proceed) then
          match exp.espan.global_created_in_src with
          | None -> 
            print_endline ("DGlobal: "^(CorePrinting.id_to_string id)^" = "^(CorePrinting.exp_to_string exp));
            error "[dir_of_decls] encountered a global without a src_id"
          | Some(tcid) -> (
          match (TaggedCid.ty tcid) with
          | "array" ->
            let dir_tree = path_to_dir
              ((tcid_ancestors tcid)@[tcid])
              (core_exp_to_arrmeta id exp)
            in
            subtrees := dir_tree::(!subtrees);
          | "table" ->
            let dir_tree = path_to_dir
              ((tcid_ancestors tcid)@[tcid])
              (core_exp_to_tblmeta id exp)
            in
            subtrees := dir_tree::(!subtrees);        
          | tag -> 
            print_endline (CorePrinting.exp_to_string exp);
            error@@"[dir_of_decls] unsupported global type (expected array or table, got "^tag^")"
      )          
    end
  in
  v#visit_decls () decls;
  let dir = List.fold_left (merge_roots) empty_dir (!subtrees) in
  dir
;;

(*** name resolution for the interpreter ***)
let dir_to_name_assoc dir = 
  let cid_to_name n = Cid.to_id n |> fst in
  match dir with
  | Root(_) -> ("root", dir)
  | Tuple{name=name; _} -> (cid_to_name name, dir)
  | Record{name=name; _} -> (cid_to_name name, dir)
  | MatGlobal(Tbl{name=name; _}) -> (cid_to_name name, dir)
  | MatGlobal(Arr{name=name; _}) -> (cid_to_name name, dir)
;;

let dir_to_string dir = 
  ( dir_to_json dir |> Yojson.Basic.pretty_to_string )
;;

(* resolve the name of a materialized global 
   in the source program to its name in the interpreted 
   program.
   - to reference the field bar of a record foo, 
     the input name string should be: "foo.bar"
   - to reference the nth item in a tuple foo, 
     the input name string should be "foo.[n]" *)
let rec names_to_compiled_cid dir names =
  match names with
  (* nothing left to resolve -- you should be on the node *)
  | [] -> (
    match dir with 
    | MatGlobal(Arr(a)) -> a.compiled_cid
    | MatGlobal(Tbl(t)) -> t.compiled_cid
    | _ -> error "did not reach leaf by end of names"
  )
  (* find the node named name and recurse on tail *)
  | name::names -> (
    match dir with 
    | Tuple{elements;} (* resolve tuples the same way as everything else, because tuples are just records with fields named "[0]", "[1]", etc. *)
    | Root{elements;}
    | Record{elements;} -> (
      match List.assoc_opt name (List.map dir_to_name_assoc elements) with
      | Some(subdir) -> names_to_compiled_cid subdir names
      | None -> error (Printf.sprintf "problem resolving a global name to a \
        compiled object. Looked for %s in the globals name directory\n%s"
          (String.concat "." (name::names))
          (dir_to_string dir)))
    (* case: resolving action name *)
    | MatGlobal(Tbl(t)) -> (
      let acn_assoc = List.map (fun acn -> (Cid.to_id (acn.aid) |> fst, acn)) t.actions in 
      (List.assoc name acn_assoc).acompiled_id
    )
    | MatGlobal(_) -> error "directory is a leaf, but a node was expected")
;;

let name_to_compiled_cid dir name =
  names_to_compiled_cid dir (String.split_on_char '.' name)
;;

let do_test ds = 
  let dir = build_coredirectory ds in
  (* let json = dir_to_json dir in *)
  (* print_endline ( json |> Yojson.Basic.pretty_to_string ); *)

  let arr_cid = name_to_compiled_cid dir "myarr.[0]" in
  print_endline ("got compiled cid for arr: "^(Cid.to_string arr_cid));

  let tbl_cid = name_to_compiled_cid dir "ftbl" in
  print_endline ("got compiled cid for tbl: "^(Cid.to_string tbl_cid));

  let acn_cid = name_to_compiled_cid dir "ftbl.hit_acn" in
  print_endline ("got compiled cid for acn: "^(Cid.to_string acn_cid));
  ()  
;;

(* this isn't useful for the interpreter, 
   but keep it for debugging *)
let coresyntax_to_globaldir ds =
  let dir = build_coredirectory ds in
  let json = dir_to_json dir in
  print_endline ( json |> Yojson.Basic.pretty_to_string );
  json
;;

