(* For implementing longest-prefix matching on headers in the interpreter *)
open Batteries
open Collections

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

let get_header_ids ty =
  match TyTQVar.strip_links ty.raw_ty with
  | TRecord lst ->
    (* Drop last entry, which should always be a Payload.t *)
    let lst, payload = List.takedrop (List.length lst - 1) lst in
    ( List.map
        (function
          | label, TName (cid, _, _) -> label, Cid.to_id cid
          | _ ->
            Console.error_position ty.tspan "Expected the name of a header type")
        lst
    , List.hd payload )
  | _ -> failwith "Internal error: Expected TRecord for packet_ty definition"
;;

(* For each packet type, include the list of header names (in order), the
   number of variables it unrolls into, and the actual type of the packet
   (as a nested record with no TNames) *)
type pkt_entry =
  { header_names : string list
  ; size : int
  ; full_ty : ty
  }

let make_packet_map header_decs : pkt_entry IdMap.t =
  let header_decs, packet_decs =
    List.partition_map
      (function
        | DHeaderTy (id, ty) -> Left (id, ty)
        | DPacketTy (id, ty) -> Right (id, ty)
        | _ ->
          failwith
            "Internal error: Header/Packet declaration list had another kind \
             of declaration in it")
      header_decs
  in
  let header_map =
    List.fold_left
      (fun acc (id, ty) -> IdMap.add id ty.raw_ty acc)
      IdMap.empty
      header_decs
  in
  let packet_map =
    List.fold_left
      (fun acc (id, pty) ->
        let header_ids, payload = get_header_ids pty in
        let full_ty_entries =
          List.map (fun (s, id) -> s, IdMap.find id header_map) header_ids
        in
        (* unrolled_size will fail if we give it the Payload because it's a TName.
           TODO: We could get around this by having it check if the TName is builtin *)
        let size =
          SyntaxUtils.unrolled_size (ty (TRecord full_ty_entries)) + 1
        in
        let full_ty =
          { pty with raw_ty = TRecord (full_ty_entries @ [payload]) }
        in
        let entry =
          { header_names = List.map (Id.name % snd) header_ids; size; full_ty }
        in
        IdMap.add id entry acc)
      IdMap.empty
      packet_decs
  in
  packet_map
;;

let mk_trie pkt_map =
  IdMap.fold
    (fun pkt_id { header_names; full_ty } acc ->
      HTrie.add header_names (pkt_id, full_ty) acc)
    pkt_map
    HTrie.empty
;;

let preprocess_headers header_decs =
  let pkt_map = make_packet_map header_decs in
  pkt_map, mk_trie pkt_map
;;
