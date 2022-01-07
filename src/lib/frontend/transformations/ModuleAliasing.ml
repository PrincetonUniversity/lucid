open Batteries
open Syntax
open SyntaxUtils
open Collections

let aliaser =
  object
    inherit [_] s_map

    method! visit_cid (target, new_cid) cid =
      match cid with
      | Compound (id, cid') when Id.equal id target -> Cid.concat new_cid cid'
      | _ -> cid
  end
;;

let rec alias_helper acc ds =
  match ds with
  | [] -> List.rev acc
  | hd :: tl ->
    (match hd.d with
    | DModuleAlias (id, e, cid1, cid2) ->
      let cid =
        match e.e with
        | EVal { v = VBool b } -> if b then cid1 else cid2
        | _ ->
          Console.error_position hd.dspan
          @@ "Module aliasing booleans must be either symbolic or constant"
      in
      let tl = aliaser#visit_decls (id, cid) tl in
      alias_helper acc tl
    | DModule (id, intf, ds') ->
      let ds' = alias_helper [] ds' in
      let new_d = { hd with d = DModule (id, intf, ds') } in
      alias_helper (new_d :: acc) tl
    | _ -> alias_helper (hd :: acc) tl)
;;

let alias_prog = alias_helper []
