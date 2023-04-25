(* compound identifiers, with tags / annotations *)

type cid = Cid.t
type tagval = 
  | TagStr of string
  | TagInt of int

type t = {
  tcid : cid;
  tparent: t option;
  tag  : string option;
  tags : (string * tagval) list;
}

let cid cid = {tparent = None; tcid=cid; tag=None; tags = [];}
let id id = cid (Cid.id id)
let tagged_cid cid s = {tparent = None; tcid=cid; tag=Some(s); tags = [("tag", TagStr(s))];}
let tagged_id id s = tagged_cid (Cid.id id) s

let create ss = cid (Cid.create ss)
let create_ids ids = cid (Cid.create_ids ids)
let create_tagged ss s = {(create ss) with tag=Some(s); tags = [("tag", TagStr(s))];}
let create_ids_tagged ids s = {(create_ids ids) with tag=Some(s); tags = [("tag", TagStr(s))];}

let to_cid t = t.tcid
let to_id t = Cid.to_id (to_cid t)


let tagval_to_string tv = match tv with
  | TagStr s -> s
  | TagInt i -> string_of_int i
;;
let tag_to_string (s,v) = 
  Printf.sprintf "%s = %s" s (tagval_to_string v)
;;
let to_string tcid = 
  match tcid.tags with
  | [] ->
    Cid.to_string tcid.tcid
  | _ -> 
    let tags_str = (List.map tag_to_string tcid.tags) |> String.concat ";"  in    
    let idx_str = match List.assoc_opt "index" tcid.tags with 
      | Some(tagval) -> "["^tagval_to_string tagval^"]"
      | None -> ""
    in
    Printf.sprintf "%s%s <<%s>>" (Cid.to_string tcid.tcid) idx_str tags_str
;;

(* update type tag *)
let tytag t s = 
  match (List.assoc_opt "type" t.tags) with
  | None -> {t with tags = ("type", TagStr(s))::t.tags}
  | Some(_) -> {t with tags = ("type", TagStr(s))::(List.remove_assoc "type" t.tags);}
;;

let idxtag t i = 
  match (List.assoc_opt "index" t.tags) with
  | None -> {t with tags = ("index", TagInt(i))::t.tags}
  | Some(_) -> {t with tags = ("index", TagInt(i))::(List.remove_assoc "index" t.tags);}
;;

let equals t1 t2 = ( (Cid.equals t1.tcid t2.tcid)
    && (MiscUtils.list_eq t1.tags t2.tags))
;;

let ty t = 
  match (List.assoc_opt "type" t.tags) with
  | None -> "none"
  | Some(tystr) -> tagval_to_string tystr  
;;



(* ancestors of tcid *)
let tcid_ancestors tcid = 
  (* ancestors of tcid, including tcid *)
  let rec _tcid_ancestors tcid = 
    match tcid.tparent with 
    | None -> [tcid] (* im the root *)
    | Some(tparent) -> (_tcid_ancestors tparent)@[tcid] (* not root *)
  in
  match tcid.tparent with 
  | None -> []
  | Some(tparent) -> (_tcid_ancestors tparent)
;;

