(* build the global directory from a tagged syntax / coresyntax tree
  This module should be run after GlobalConstructorTagging.annotate *)
open Syntax
open TaggedCid
module C = CoreSyntax
module P4 = P4TofinoSyntax


type key = {kid : cid option; kty : string; ksize : int;}
type action = {aid : cid; acompiled_id : cid; arg_sizes : int list;}
type tbl = {name:cid; compiled_cid:cid; length : int; keys : key list; actions : action list;}
type arr = {name:cid; compiled_cid:cid; length : int; cell_size : int list;}
type mat_global = 
  | Tbl of tbl 
  | Arr of arr

type dir = 
  | Root of {elements:dir list;}
  | Tuple of {name:cid; elements:dir list}
  | Record of {name:cid; elements:dir list}
  | MatGlobal of mat_global

let empty_dir = Root({elements=[];})

(*** syntax -> mat_global constructors ***)
let exp_to_arrmeta id exp = 
  let exp_to_int exp = 
    match exp.e with
    | EVal({v=VInt(z); _}) ->
      Integer.to_int z
    | EInt(z, _) -> Z.to_int z
    | _ -> error "[exp_to_int] exp is not an EVal(EInt(...))"
  in
  let arrconstr_to_sz exp =
    let rty = TyTQVar.strip_links (Option.get(exp.ety)).raw_ty in 
    match rty with
    | TName(_, [sz], true) ->
      SyntaxUtils.extract_size sz
    | TName(_, _, _) -> error "[arrconstr_to_sz] exp type is tname, but wrong form"
    | _ -> error "[arrconstr_to_sz] exp type is not in expected form"
  in
  let name = match (exp.espan.global_created_in_src) with
    | None -> error "[exp_to_arrmeta] array constructor is not annotated"
    | Some(tcid) -> tcid.tcid 
  in
  let arr = {
    name;
    compiled_cid = (Cid.id id);
    length = (match exp.e with
      | ECall(_, len_exp::_, _) -> exp_to_int len_exp
      | _ -> error "[dir_of_decls] wrong form");
    cell_size = [arrconstr_to_sz exp];}
  in
  Arr(arr)
;;

let exp_to_tblmeta id exp = 
  let ty_to_size ty = 
    match ty.raw_ty with 
    | TInt(sz) -> sz
    | TBool -> IConst(1)
    | _ -> error "[rty_to_size] expected an integer, but got something else"
  in
  (* a user-defined key of size sz *)
  let user_key ty = 
    {kid = None; kty = "ternary"; ksize = SyntaxUtils.extract_size (ty_to_size ty);}
  in
  (* builtin *)
  let priority_key = 
    {kid = Some(Cid.create ["$MATCH_PRIORITY"]); kty = "exact"; ksize = 32}
  in
  let evar_to_action exp = 
    let aid = match exp.espan.global_created_in_src with
      | None -> error "[evar_to_action] action expression not annotated with source id"
      | Some(src_tcid) -> src_tcid.tcid
    in
    let acompiled_id = match exp.e with
      | EVar(compiled_id) -> compiled_id
      | _ -> error "[evar_to_action] expected an action variable"
    in
    let arg_sizes = match (Option.get exp.ety).raw_ty with
      | TActionConstr(aty) -> 
        List.map ty_to_size aty.aconst_param_tys
        |> List.map SyntaxUtils.extract_size
      | _ -> error "[action_to_argsizes] action exp is not an action type"
    in
    {aid; acompiled_id; arg_sizes}
  in
  let keys = match TyTQVar.strip_links ((Option.get exp.ety).raw_ty) with
    | TTable(tty) -> (List.map user_key tty.tkey_sizes)@[priority_key] 
    | TName(_, sizes, _) ->
      let key_sz = List.nth sizes 0 in
      let key_sizes = SyntaxUtils.flatten_size key_sz in
      let key_tys = List.map (fun sz -> Syntax.ty@@TInt(sz)) key_sizes in
      (List.map user_key key_tys)@[priority_key] 
    | TQVar _ -> error "[exp_to_tblmeta] expression is a type variable"
    | raw_ty -> error@@"[exp_to_tblmeta] expression is not a table type ("^(Printing.raw_ty_to_string raw_ty)^")"
  in
  let actions, length = match exp.e with
    | ETableCreate(tbl) -> (
      List.map evar_to_action tbl.tactions,
      match tbl.tsize.e with 
        | EVal({v=VInt(z); _}) ->
          Integer.to_int z
        | EInt(z, _) -> Z.to_int z
        | _ -> error "[exp_to_tblmeta] table size expression is not an EVal(EInt(...))")
    | ECall(_, [len_exp; acns_exp; _], _) -> (
      List.map evar_to_action (SyntaxUtils.flatten_exp acns_exp),
      match len_exp.e with 
        | EVal({v=VInt(z); _}) ->
          Integer.to_int z
        | EInt(z, _) -> Z.to_int z
        | _ -> error "[exp_to_tblmeta] table size expression is not an EVal(EInt(...))"
    )
    | _ -> error@@Printf.sprintf "[exp_to_tblmeta] expression %s is not a table create" (Printing.exp_to_string exp)
  in
  let compiled_cid = (Cid.id id) in 
  let name = match exp.espan.global_created_in_src with
    | None -> error "[exp_to_tblmeta] table constructor not annotated with source id"
    | Some(src_tcid) -> src_tcid.tcid
  in
  Tbl{name; compiled_cid; length; keys; actions;}
;;

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
    | TName(_, sizes) -> List.map (fun sz -> match sz with | C.Sz sz -> sz | _ -> error "need singleton size") sizes
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
    | TName(_, sizes) -> 
      let key_sizes = CoreSyntax.size_to_ints (List.hd sizes) in
      (List.map user_key key_sizes)@[priority_key]
    (* | TTable(tty) -> 
      let key_sizes = List.map (fun sz -> match sz with | C.Sz sz -> sz | _ -> error "need singleton size") tty.tkey_sizes in
      (List.map user_key key_sizes)@[priority_key]  *)
    | _ -> error "[exp_to_tblmeta] expression is not a table type"
  in
  let actions, length = match exp.e with
    | ECall(_, [len_exp; acns_exp; _], _) -> (
      let acn_exps = match acns_exp.e with 
        | ETuple(exps) -> exps
        | _ -> [acns_exp]
      in
      List.map evar_to_action acn_exps,
      match len_exp.e with 
        | EVal({v=VInt(z); _}) -> Integer.to_int z
        | _ -> error "[exp_to_tblmeta] table size expression is not an EVal(EInt(...))"
    )
    (* | ETableCreate(tbl) -> (
      List.map evar_to_action tbl.tactions,
      match tbl.tsize.e with 
        | EVal({v=VInt(z); _}) ->
          Integer.to_int z
        | _ -> error "[exp_to_tblmeta] table size expression is not an EVal(EInt(...))") *)
    | _ -> error "[exp_to_tblmeta] expression is not a table create"
  in
  let compiled_cid = (Cid.id id) in 
  let name = match exp.espan.global_created_in_src with
    | None -> error "[exp_to_tblmeta] table constructor not annotated with source id"
    | Some(src_tcid) -> src_tcid.tcid
  in
  Tbl{name; compiled_cid; length; keys; actions;}
;;

(*** directory constructors 
     (uses syntax or coresyntax materialized global constructors) ***)
let inline_idx tcid = 
  match (List.assoc_opt "index" tcid.tags) with
    | None -> tcid
    | Some(tagval) -> 
      {tcid with 
      tcid = 
        (Cid.create ["["^TaggedCid.tagval_to_string tagval^"]"])}
;;
(* path: ancestor path of a global 
   mat_global: metadata about the global
   return: a directory for just the global and its ancestors *)
let path_to_dir path mat_global =
  let rec _path_to_dir path mat_global = 
    match path with
    | [] -> Root{elements=[];}
    | node::path -> (
      match (TaggedCid.ty node) with 
      | "tuple" -> 
        (* tuple elements have names based on index *)
        let path = match path with
          | hd::tl -> (inline_idx hd)::tl
          | [] -> path
        in 
        Tuple{name=node.tcid; elements=[_path_to_dir path mat_global];}
      | "record" -> 
        let field = _path_to_dir path mat_global in
        Record{name=node.tcid; elements=[field]}
      | "array" -> (
        let mat_global = match mat_global with
          | Arr(t) -> Arr({t with name=node.tcid;})
          | _ -> error "[path_to_dir] expected array, got another type of global"
        in
        MatGlobal(mat_global))
      | "table" -> (
        let mat_global = match mat_global with
          | Tbl(t) -> Tbl({t with name=node.tcid;})
          | _ -> error "[path_to_dir] expected table, got another type of global"
        in
        MatGlobal(mat_global))
      | nodename -> error ("unknown node type while building directory: "^(nodename))
    )  
  in
  Root{elements=[_path_to_dir path mat_global];}
;;

(* merge 2 nodes. only succeeds when nodes have same type and name *)
let rec merge_nodes n1 n2 = 
  match (n1, n2) with
  | Root({elements=es1;}), Root({elements=es2;}) -> 
    let es' = merge_elements es1 es2 in
    Some(Root({elements = es';}))

  | Tuple({name=n1; elements=es1;}), Tuple({name=n2; elements=es2;}) -> 
    if (Cid.equals n1 n2) then (
      let es' = merge_elements es2 es1 in
      Some(Tuple({name=n1; elements=es';})))
    else (None)
  | Record({name=n1; elements=es1;}), Record({name=n2; elements=es2;}) -> 
    if (Cid.equals n1 n2) then (
      let es' = merge_elements es2 es1 in
      Some(Record({name=n1; elements=es';})))
    else (None)
  | _ -> 
    None

(* merge two element lists *)
and merge_elements es1 es2 = 
  List.fold_left merge_single_element es1 es2 
(* merge e into es*)
and merge_single_element (es:dir list) (e:dir) = 
  let es', merged = List.fold_left 
    (* fold over elements in es, attempting to merge 
       until you find the same element.  *)
    (fun (es', merged) es_e -> 
      if (merged) then (es'@[es_e], merged)
      else (
        match (merge_nodes es_e e) with
        | None -> (es'@[es_e], merged)
        | Some(es_e') -> (es'@[es_e'], true)))
    ([], false)
    es
  in
  (* if there was nothing to merge into, add the element *)
  if (merged) then (es')
  else (es@[e])
;;
let merge_roots n1 n2 = 
  match merge_nodes n1 n2 with
  | None -> error "[merge_roots] merge failed"
  | Some(n) -> n
;;

let build_directory decls = 
  let subtrees = ref [] in
  let v = object
    inherit [_] s_iter as super
    method! visit_DGlobal () id _ exp =
      match exp.espan.global_created_in_src with
      | None -> 
        print_endline ("DGlobal: "^(Printing.id_to_string id)^" = "^(Printing.exp_to_string exp));
        error "[dir_of_decls] encountered a global without a src_id"
      | Some(tcid) -> (
        match (TaggedCid.ty tcid) with
        | "array" ->
          let dir_tree = path_to_dir
            ((tcid_ancestors tcid)@[tcid])
            (exp_to_arrmeta id exp)
          in
          subtrees := dir_tree::(!subtrees);
        | "table" ->
          let dir_tree = path_to_dir
            ((tcid_ancestors tcid)@[tcid])
            (exp_to_tblmeta id exp)
          in
          subtrees := dir_tree::(!subtrees);
        | tag -> error@@"[dir_of_decls] unsupported global type (expected array or table, got "^tag^")"
      )
      end
  in
  v#visit_decls () decls;
  let dir = List.fold_left (merge_roots) empty_dir (!subtrees) in
  dir
;;

let build_coredirectory (decls:C.decls) = 
  let subtrees = ref [] in
  let v = object
    inherit [_] C.s_iter as super
      method! visit_DGlobal () id _ exp =
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
        | tag -> error@@"[dir_of_decls] unsupported global type (expected array or table, got "^tag^")"
      )          
    end
  in
  v#visit_decls () decls;
  let dir = List.fold_left (merge_roots) empty_dir (!subtrees) in
  dir
;;


(*** directory printers ***)
let sp n = String.make n ' '
let indent n str = 
  String.split_on_char '\n' str 
  |> List.map (fun s -> (sp n)^s)
  |> String.concat "\n"
;;
let ln_sep f xs =
  String.concat ",\n" (List.map f xs)
;;

let cid_to_string = Printing.cid_to_string ;;

let _key_to_json key = 
  match key.kid with
  | Some(name) -> 
    `Assoc [
      "compiled_name", `String (cid_to_string name);
      "size", `Int key.ksize;
      "matchtype", `String key.kty;
    ]
  | None -> 
    `Assoc [
      "size", `Int key.ksize;
      "matchtype", `String key.kty;
    ]
;;
let _action_to_json action =
  `Assoc [
    "name", `String (cid_to_string action.aid);
    "compiled_name", `String (cid_to_string action.acompiled_id);
    "install_arg_sizes", `List (List.map (fun i -> `Int i) action.arg_sizes)
  ]
;;
let tbl_to_json (tbl:tbl) = `Assoc [
  "type", `String "table";
  (* "name", `String (cid_to_string tbl.name); *)
  "compiled_name", `String (cid_to_string tbl.compiled_cid);
  "length", `Int tbl.length;
  "keys", `List (List.map _key_to_json tbl.keys);
  "actions", `List (List.map _action_to_json tbl.actions); ]
;;

(* let json_output = `Assoc [("dude", `String "whatever")] in
let s = Yojson.Basic.pretty_to_string json_output in *)
let arr_to_json (arr:arr) = `Assoc [
  "type", `String "array";
  (* "name", `String (cid_to_string arr.name); *)
  "compiled_name", `String (cid_to_string arr.compiled_cid);
  "length", `Int arr.length;
  "cell_size", (`List (List.map (fun i -> `Int i) arr.cell_size));]
;;

let mat_global_to_json mat_global =
  match mat_global with
    | Arr(a) -> cid_to_string a.name, arr_to_json a 
    | Tbl(t) -> cid_to_string t.name, tbl_to_json t

let rec dir_to_json_assoc_element dir = 
  match dir with
    | Root({elements=elements;}) ->
      "ROOT", (dirs_to_json_assoc elements)
    | Record({name=name; elements=elements;}) -> 
      cid_to_string name, (dirs_to_json_assoc elements)
    | Tuple({name=name; elements=elements;}) -> 
      cid_to_string name, (dirs_to_json_list elements)
    | MatGlobal(m) -> mat_global_to_json m
and dir_to_json_list_element dir = 
  (* the dir is an element in a list, so no name *)
  match dir with
    | Root({elements=elements;}) ->
      (dirs_to_json_assoc elements)
    | Record({elements=elements;}) -> 
      (dirs_to_json_assoc elements)
    | Tuple({elements=elements;}) -> 
      (dirs_to_json_list elements)
    | MatGlobal(m) -> mat_global_to_json m |> snd
and dirs_to_json_assoc dirs = 
  `Assoc (List.map dir_to_json_assoc_element dirs)
and dirs_to_json_list dirs = 
  `List (List.map dir_to_json_list_element dirs)
;;

let dir_to_json root_dir = 
  dir_to_json_list_element root_dir 
;;

(* main function from syntax *)
let syntax_to_globaldir ds = 
  let dir = build_directory ds in
  print_endline (dir_to_json dir |> Yojson.Basic.pretty_to_string );
  dir_to_json dir
;;
let coresyntax_to_globaldir ds =
  let dir = build_coredirectory ds in
  let json = dir_to_json dir in
  print_endline ( json |> Yojson.Basic.pretty_to_string );
  json
;;

