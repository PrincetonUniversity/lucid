open Batteries
open Syntax
open SyntaxUtils
open Collections

(* Make sure each event that takes a header as its first argument is annotated
   as such *)
let annotate_events pkt_decs ds =
  let pkt_ids =
    List.fold_left
      (fun acc d ->
        match d.d with
        | DPacketTy (id, _) -> IdSet.add id acc
        | _ -> acc)
      IdSet.empty
      pkt_decs
  in
  let ds =
    List.map
      (fun d ->
        match d.d with
        | DEvent
            (id, _, cs, ((_, { raw_ty = TName (Id id2, _, _) }) :: _ as params))
          when IdSet.mem id2 pkt_ids ->
          (* The first argument's type is a packet type *)
          { d with d = DEvent (id, Some id2, cs, params) }
        | _ -> d)
      ds
  in
  ds
;;

let eliminate_prog ds =
  let pkt_decs, ds =
    let duplicate_d d =
      let mk_ty = ty in
      match d.d with
      | DHeaderTy (id, ty) -> [d; { d with d = DUserTy (id, [], ty) }]
      | DPacketTy (id, ty) ->
        let pkt_ty = mk_ty (TName (Id id, [], false)) in
        let params = [Id.fresh "pkt_arg", pkt_ty] in
        [ d
        ; { d with d = DUserTy (id, [], ty) }
        ; { d with d = DEvent (id, Some id, [], params) } ]
      | _ -> [d]
    in
    ds
    |> List.map duplicate_d
    |> List.flatten
    |> List.partition (fun d ->
           match d.d with
           | DHeaderTy _ | DPacketTy _ -> true
           | _ -> false)
  in
  let ds = annotate_events pkt_decs ds in
  let pkt_decs = List.map (fun d -> d.d) pkt_decs in
  pkt_decs, ds
;;
