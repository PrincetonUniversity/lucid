open Batteries
open Yojson.Basic

let error s = raise (CoreSyntax.Error s)

type record_autofill =
  | Never
  | Random of int (* seed *)
  | Default

let parse_int err_str (j : json) =
  match j with
  | `Int n -> n
  | _ -> error @@ "Non-integer value for " ^ err_str
;;

let parse_int_entry lst str default =
  match List.assoc_opt str lst with
  | Some j -> parse_int str j
  | None -> default
;;

let parse_port str =
  match String.split_on_char ':' str with
  | [id; port] ->
    (try int_of_string id, int_of_string port with
    | _ -> error "Incorrect format for link entry!")
  | _ -> error "Incorrect format for link entry!"
;;

(* Getting weird errors if I put the let open after the function arguments *)
let parse_simple_value =
  let open CoreSyntax in
  fun err_str ty j ->
    match j, ty.raw_ty with
    | `Int n, TInt size -> vint n size
    | `Bool b, TBool -> vbool b
    | `List lst, TGroup ->
      vgroup (List.map (fun n -> parse_int "group value definition" n) lst)
    | _ ->
      error
      @@ err_str
      ^ " specification had wrong or unexpected argument "
      ^ to_string j
;;

let rec parse_header_value autofill (rty : Syntax.raw_ty) json =
  let open SyntaxToCore in
  let recurse = parse_header_value autofill in
  match Syntax.TyTQVar.strip_links rty, json with
  | TInt size, `Int n -> [CoreSyntax.vint n (translate_size size)]
  | TBool, `Bool b -> [CoreSyntax.vbool b]
  | TGroup, `List lst ->
    [ CoreSyntax.vgroup
        (List.map (fun n -> parse_int "group value definition" n) lst) ]
  | TTuple tlst, `List vlst ->
    if List.length tlst <> List.length vlst then error "TODO: Error";
    List.map2 recurse tlst vlst |> List.flatten
  | TVector (rty, size), `List lst ->
    if List.length lst <> translate_size size then error "TODO: Error";
    List.map (recurse rty) lst |> List.flatten
  | TRecord rlst, `Assoc jlst ->
    List.map
      (fun (label, rty) ->
        match List.assoc_opt label jlst with
        | Some json -> recurse rty json
        | None ->
          (match autofill with
          | Default -> failwith "TODO: Default values"
          | Random _ -> failwith "TODO: Random values"
          | Never -> error "TODO: Error"))
      rlst
    |> List.flatten
  | ( ( TQVar _
      | TVoid
      | TMemop _
      | TEvent
      | TInt _
      | TBool
      | TGroup
      | TTuple _
      | TVector _
      | TRecord _
      | TFun _
      | TName _
      | TAbstract _ )
    , _ ) -> failwith ""
;;
