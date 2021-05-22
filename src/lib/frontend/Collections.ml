module IdMap = Map.Make (Id)

let idmap_to_string f map =
  "{ "
  ^ IdMap.fold
      (fun k v acc -> Id.to_string k ^ " -> " ^ f v ^ "; " ^ acc)
      map
      ""
  ^ " }"
;;

module CidMap = Map.Make (Cid)
module StringMap = Map.Make (String)
module IdSet = Set.Make (Id)
module CidSet = Set.Make (Cid)
module StringSet = Set.Make (String)
