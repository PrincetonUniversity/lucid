open Batteries
open Syntax
open SyntaxUtils
open Collections

(* Transform all records in the program into tuples, and remove all user type
   declarations because we don't need them anymore. This has the unfortunate
   effect of removing the label names from the syntax, which can make it harder
   to debug problems during later transformations. *)

(* Preprocessing pass: we need to sort our labels not alphabetically,
   but by declaration order. We do this by collecting the index for
   each label, and looking those up every time we do a sort. *)
let create_label_map ds : int StringMap.t =
  let label_indexer =
    object
      inherit [_] s_iter

      method! visit_DUserTy env _ _ ty =
        match ty.raw_ty with
        | TRecord lst ->
          List.iteri (fun n (str, _) -> env := StringMap.add str n !env) lst
        | _ -> ()
    end
  in
  let label_map = ref StringMap.empty in
  label_indexer#visit_decls label_map ds;
  !label_map
;;

let sort_by_label label_map lst =
  let lst =
    List.map (fun (str, x) -> StringMap.find str label_map, (str, x)) lst
  in
  let sorted = List.sort (fun (x, _) (y, _) -> Int.compare x y) lst in
  List.map snd sorted
;;

let record_replacer label_map =
  let sort_by_label lst = sort_by_label label_map lst in
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
  let replacer = record_replacer (create_label_map ds) in
  ds
  |> List.filter (function
         | { d = DUserTy _ } -> false
         | _ -> true)
  |> replacer#visit_decls ()
;;
