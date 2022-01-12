(* Compound Identifiers *)
type id = Id.t

type t =
  | Id of id
  | Compound of id * t

(* We represent "Array.create" as Compound("Array", Id "create")) *)

(* Constructors *)

let rec create ss =
  match ss with
  | [] -> failwith "Cannot create an empty cid!" (* Id (Id.create "") *)
  | [s] -> Id (Id.create s)
  | s :: ss -> Compound (Id.create s, create ss)
;;

let rec create_ids ids =
  match ids with
  | [] -> failwith "Cannot create an empty cid!"
  (* Id (Id.create "") *)
  | [id] -> Id id
  | id :: ids -> Compound (id, create_ids ids)
;;

let create_ids_rev ids = create_ids (List.rev ids)

let rec fresh ss =
  match ss with
  | [] -> Id (Id.fresh "")
  | [s] -> Id (Id.fresh s)
  | s :: ss -> Compound (Id.create s, fresh ss)
;;

let str_cons str cid : t =
  match cid with
  | Id id -> Compound (Id.fresh str, Id id)
  | Compound (id, cid) -> Compound (Id.fresh str, Compound (id, cid))
;;

let id id = Id id
let compound id cid = Compound (id, cid)

let rec concat cid1 cid2 =
  match cid1 with
  | Id id -> compound id cid2
  | Compound (id, cid1) -> compound id (concat cid1 cid2)
;;

let from_string str = BatString.split_on_char '.' str |> create

(* Destructors *)

let rec to_string cid =
  match cid with
  | Id id -> Id.to_string id
  | Compound (id, cid) -> Id.to_string id ^ "." ^ to_string cid
;;

let rec to_string_delim d cid =
  match cid with
  | Id id -> Id.to_string_delim d id
  | Compound (id, cid) -> Id.to_string_delim d id ^ "." ^ to_string_delim d cid
;;

let to_id d =
  match d with
  | Id evid -> evid
  | _ -> failwith "attempted to convert cid with multiple parts into id"
;;

let rec to_ids d =
  match d with
  | Id id -> [id]
  | Compound (id, cid) -> id :: to_ids cid
;;

let rec to_ids_prefix d =
  match d with
  | Id id -> id, []
  | Compound (id, cid) ->
    let base, rest = to_ids_prefix cid in
    base, id :: rest
;;

let rec first_id d =
  match d with
  | Id id -> id
  | Compound (id, _) -> id
;;

let rec last_id d =
  match d with
  | Id id -> id
  | Compound (_, cid) -> last_id cid
;;

let rec names cid =
  match cid with
  | Id id -> [Id.name id]
  | Compound (id, cid) -> Id.name id :: names cid
;;

(* Operations *)

let rec compare cid1 cid2 =
  match cid1, cid2 with
  | Id i1, Id i2 -> Id.compare i1 i2
  | Compound (id1, cid1), Compound (id2, cid2) ->
    let i = Id.compare id1 id2 in
    if i = 0 then compare cid1 cid2 else i
  | Id _, Compound _ -> -1
  | Compound _, Id _ -> 1
;;

let equal cid1 cid2 = compare cid1 cid2 = 0
let equals = equal

let rec equal_names cid1 cid2 =
  match cid1, cid2 with
  | Id i1, Id i2 -> Id.equal_names i1 i2
  | Compound (id1, cid1), Compound (id2, cid2) ->
    Id.equal_names id1 id2 && equal_names cid1 cid2
  | _ -> false
;;

let lookup_opt tuples key = Core.List.Assoc.find ~equal:equals tuples key

let lookup tuples key =
  match lookup_opt tuples key with
  | Some v -> v
  | None ->
    let err_str = "error looking up " ^ to_string key in
    failwith err_str
;;

let exists tuples key =
  match lookup_opt tuples key with
  | Some _ -> true
  | None -> false
;;

let rec replace tuples key new_val =
  match tuples with
  | [] -> []
  | (k, v) :: tuples ->
    if equals k key
    then (k, new_val) :: tuples
    else (k, v) :: replace tuples key new_val
;;

let rec remove tuples key =
  match tuples with
  | [] -> []
  | (k, v) :: tl ->
    if equals k key then remove tl key else (k, v) :: remove tl key
;;

let rec modify_tail f t =
  match t with
  | Id id -> Id (f id)
  | Compound (id, cid) -> Compound (id, modify_tail f cid)
;;
