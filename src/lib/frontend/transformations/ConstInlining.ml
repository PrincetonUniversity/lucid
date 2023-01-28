open Batteries
open Syntax
open Collections

(* Inline all const declarations. Expects modules to be eliminated first as
   well as alpha-renaming. *)
type env = e IdMap.t

let inliner =
  object
    inherit [_] s_map

    method! visit_EVar env cid =
      match IdMap.find_opt (Cid.to_id cid) env with
      | None -> EVar cid
      | Some e -> e

    method! visit_PVar env cid span =
      match IdMap.find_opt (Cid.to_id cid) env with
      | Some (EVal { v = VInt n }) -> PNum (Integer.value n)
      | Some (EInt (n, _)) -> PNum n
      | Some (EVar (_)) -> Console.error_position span
                            @@ Printing.cid_to_string cid
                            ^ " ITS A VAR"
      | Some (ECall (_, _)) -> Console.error_position span
      @@ Printing.cid_to_string cid
      ^ " ITS A CALL!!!"
      | None ->
        Console.error_position span
        @@ Printing.cid_to_string cid
        ^ " is either unbound or not a constant/symbolic."
      | _ -> failwith "Non-int PVar?"
  end
;;

let inline_decl env d =
  match d.d with
  | DConst (id, _, exp) ->
    let exp = inliner#visit_exp env exp in
    IdMap.add id exp.e env, []
  | _ -> env, [inliner#visit_decl env d]
;;

let inline_prog ds =
  let _, ds =
    List.fold_left
      (fun (env, ds) d ->
        let env, d = inline_decl env d in
        env, d @ ds)
      (IdMap.empty, [])
      ds
  in
  List.rev ds
;;
