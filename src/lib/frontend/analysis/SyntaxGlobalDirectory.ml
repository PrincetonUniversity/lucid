(* The global directory: a map from the name of each global in the 
   source program (arrays, tables, and the actions of tables) to the 
   object it compiles to. 
   This module has the directory's types, tree construction, and json 
   printing. The directory itself is built from: 
    - CoreSyntax, for the interpreter (CoreSyntaxGlobalDirectory.ml)
    - the final P4, for the tofino (P4TofinoGlobalDirectory.ml)
   Both use the source names that GlobalConstructorTagging.annotate 
   attaches to global constructors in the frontend. *)
open Syntax
open TaggedCid


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

(* the key of a ternary table that holds an entry's priority *)
let priority_key = 
  {kid = Some(Cid.create ["$MATCH_PRIORITY"]); kty = "exact"; ksize = 32}
;;

(*** directory constructors ***)
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
