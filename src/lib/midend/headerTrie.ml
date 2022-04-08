(* For implementing longest-prefix matching on headers in the interpreter *)
open Batteries

module Trie (M : Map.OrderedType) : sig
  type 'a t

  val empty : 'a t
  val add : M.t list -> 'a -> 'a t -> 'a t

  (* Longest-prefix matching *)
  val lpm : M.t list -> 'a t -> 'a option
end = struct
  module KeyMap = Map.Make (M)

  type 'a t =
    { value : 'a option
    ; children : 'a t KeyMap.t
    }

  let empty = { value = None; children = KeyMap.empty }

  let rec add (keys : M.t list) (value : 'a) (t : 'a t) =
    match keys with
    | [] -> { t with value = Some value }
    | hd :: tl ->
      let subtrie = KeyMap.find_default empty hd t.children in
      let subtrie' = add tl value subtrie in
      { t with children = KeyMap.add hd subtrie' t.children }
  ;;

  let rec lpm keys t =
    match keys with
    | [] -> t.value
    | hd :: tl ->
      (match KeyMap.find_opt hd t.children with
      | None -> t.value
      | Some t ->
        (match lpm tl t with
        | None -> t.value
        | Some ret -> Some ret))
  ;;
end

(* Not CoreSyntax since the packet type declarations aren't really part of the
   main program anymore *)
open Syntax
module HTrie = Trie (String)

let mk_trie header_defs =
  let packet_tys =
    List.filter_map
      (fun d ->
        match d.d with
        | DPacketTy (pkt_ty_id, ty) ->
          let header_ty_ids =
            match TyTQVar.strip_links ty.raw_ty with
            | TRecord lst ->
              List.map
                (function
                  | _, TName (cid, _, _) -> Id.name @@ Cid.to_id cid
                  | _ ->
                    Console.error_position
                      ty.tspan
                      "Expected the name of a header type")
                lst
            | _ ->
              failwith
                "Internal error: Expected TRecord for packet_ty definition"
          in
          Some ((pkt_ty_id, ty), header_ty_ids)
        | _ -> None)
      header_defs
  in
  List.fold_left
    (fun acc (id, strs) -> HTrie.add strs id acc)
    HTrie.empty
    packet_tys
;;
