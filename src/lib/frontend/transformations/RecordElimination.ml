open Batteries
open Syntax
open SyntaxUtils
open Collections

(* Transform all records in the program into tuples, and remove all user type
   declarations because we don't need them anymore. This has the unfortunate
   effect of removing the label names from the syntax, which can make it harder
   to debug problems during later transformations. *)

(* Selection sort, basically *)
let sort_by_label sorted_lst lst =
  List.map (fun (str, _) -> str, List.assoc str lst) sorted_lst
;;

let replacer =
  object (self)
    inherit [_] s_map as super

    method! visit_TRecord env lst =
      let lst =
        lst
        (* |> sort_by_label *)
        |> List.map (fun (_, raw_ty) -> self#visit_raw_ty env raw_ty)
      in
      TTuple lst

    method! visit_exp env exp =
      let extract_tys e =
        match e.ety with
        | Some { raw_ty = TRecord lst } -> lst
        | _ -> failwith "Internal error: record elimination"
      in
      let e =
        match exp.e with
        | ERecord lst ->
          let lst =
            lst
            |> sort_by_label (extract_tys exp)
            |> List.map (fun (_, e) -> self#visit_exp env e)
          in
          ETuple lst
        | EWith (exp, lst) ->
          let labels =
            match (Option.get exp.ety).raw_ty with
            | TRecord lst -> lst |> sort_by_label (extract_tys exp)
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
        | EProj (exp, label) ->
          let labels =
            match (Option.get exp.ety).raw_ty with
            | TRecord lst ->
              lst |> sort_by_label (extract_tys exp) |> List.map fst
            | _ -> failwith "Impossible"
          in
          let idx = List.index_of label labels |> Option.get in
          EOp (TGet (List.length labels, idx), [self#visit_exp env exp])
        | _ -> super#visit_e env exp.e
      in
      { exp with e; ety = Option.map (self#visit_ty env) exp.ety }
  end
;;


let eliminate_prog ds = replacer#visit_decls () ds
