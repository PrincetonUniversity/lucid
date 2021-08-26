open Batteries
open Syntax
open SyntaxUtils
open Collections

(* Transform all records in the program into tuples, and remove all user type
   declarations because we don't need them anymore. This has the unfortunate
   effect of removing the label names from the syntax, which can make it harder
   to debug problems during later transformations. *)

let sort_by_label lst =
  List.sort (fun (x, _) (y, _) -> Pervasives.compare x y) lst
;;

let replacer =
  object (self)
    inherit [_] s_map

    method! visit_TRecord env lst =
      let lst =
        lst
        |> sort_by_label
        |> List.map (fun (_, raw_ty) -> self#visit_raw_ty env raw_ty)
      in
      TTuple lst

    method! visit_ERecord env lst =
      let lst =
        lst |> sort_by_label |> List.map (fun (_, e) -> self#visit_exp env e)
      in
      ETuple lst

    method! visit_EWith env exp lst =
      let labels =
        match (Option.get exp.ety).raw_ty with
        | TRecord lst -> lst |> sort_by_label
        | _ -> failwith "Impossible"
      in
      let entries =
        List.map
          (fun (l, rty) ->
            match List.assoc_opt l lst with
            | Some e -> e
            | None -> aexp (EProj (exp, l)) (Some (ty rty)) exp.espan)
          labels
      in
      ETuple (List.map (self#visit_exp env) entries)

    method! visit_EProj env exp label =
      let labels =
        match (Option.get exp.ety).raw_ty with
        | TRecord lst -> lst |> sort_by_label |> List.map fst
        | _ -> failwith "Impossible"
      in
      let idx = List.index_of label labels |> Option.get in
      EOp (TGet (List.length labels, idx), [self#visit_exp env exp])
  end
;;

let eliminate_prog ds =
  ds
  |> List.filter (function
         | { d = DUserTy _ } -> false
         | _ -> true)
  |> replacer#visit_decls ()
;;
