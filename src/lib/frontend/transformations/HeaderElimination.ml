open Batteries
open Syntax
open SyntaxUtils

let eliminate_prog ds =
  let duplicate_d d =
    let mk_ty = ty in
    match d.d with
    | DHeaderTy (id, ty) -> [d; { d with d = DUserTy (id, [], ty) }]
    | DPacketTy (id, ty) ->
      let params = [Id.fresh "pkt_arg", mk_ty (TName (Id id, [], false))] in
      [ d
      ; { d with d = DUserTy (id, [], ty) }
        (* TODO: The event sort is a dummy arg since it'll be removed soon *)
      ; { d with d = DEvent (id, EEntry true, [], params) } ]
    | _ -> [d]
  in
  ds
  |> List.map duplicate_d
  |> List.flatten
  |> List.partition (fun d ->
         match d.d with
         | DHeaderTy _ | DPacketTy _ -> true
         | _ -> false)
;;
