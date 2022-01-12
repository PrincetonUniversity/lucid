open Batteries
open Syntax
open Collections
open Yojson.Basic

let parse_size name j =
  match j with
  | `Int n -> n
  | _ ->
    Console.error
    @@ "Size "
    ^ name
    ^ " incorrect type in specification file (should be an integer)"
;;

(* Mostly copied from InterpCore *)
let rec compute_size err env size =
  match STQVar.strip_links size with
  | IConst n -> n
  | IUser (Id id) ->
    (match StringMap.find_opt (Id.name id) env with
    | Some n -> n
    | None -> failwith "Should be checked in Wellformed")
  | ISum (sizes, n) ->
    n + List.fold_left (fun acc s -> acc + compute_size err env s) 0 sizes
  | IVar _ | IUser _ -> err ()
;;

let parse_symbolic sizes id ty (j : json) =
  let error () =
    Console.error
    @@ Printf.sprintf
         "Symbolic %s has incorrect type in specification file (should be %s)"
         (Printing.id_to_string id)
         (Printing.ty_to_string ty)
  in
  let rec aux rty (j : json) =
    match j, rty with
    | `Int n, TInt size -> eint (Z.of_int n) (Some size)
    | `Bool b, TBool -> value_to_exp (vbool b)
    | `List lst, TVector (rty, size) ->
      let size = compute_size error sizes size in
      if List.length lst <> size
      then
        Console.error
        @@ Printf.sprintf
             "List inside symbolic %s has length %d, should be %d"
             (Printing.id_to_string id)
             (List.length lst)
             size;
      vector_sp (List.map (aux rty) lst) Span.default
    | `Assoc js, TRecord tys ->
      if List.length js <> List.length tys
      then
        Console.error
        @@ Printf.sprintf
             "Record inside symbolic %s has %d entries, should be %d"
             (Printing.id_to_string id)
             (List.length js)
             (List.length tys);
      let exps =
        List.map
          (fun (label, rty) ->
            let j =
              match List.assoc_opt label js with
              | Some j -> j
              | None ->
                Console.error
                @@ Printf.sprintf
                     "Record inside symbolic %s is missing entry %s"
                     (Printing.id_to_string id)
                     label
            in
            label, aux rty j)
          tys
      in
      record_sp exps Span.default
    | _ -> error ()
  in
  aux ty.raw_ty j
;;

let parse_symbolics (filename : string) =
  let j = from_file filename in
  match j with
  | `Assoc lst ->
    let sizes =
      match List.assoc_opt "sizes" lst with
      | Some (`Assoc lst) ->
        List.fold_left
          (fun acc (name, j) ->
            let size = parse_size name j in
            StringMap.add name size acc)
          StringMap.empty
          lst
      | Some _ -> failwith "Unexpected symbolic sizes format"
      | None -> StringMap.empty
    in
    let symbolics =
      match List.assoc_opt "symbolics" lst with
      | Some (`Assoc lst) ->
        (* We have to delay parsing until we know what type to expect *)
        List.fold_left
          (fun acc (name, j) -> StringMap.add name j acc)
          StringMap.empty
          lst
      | Some _ -> failwith "Unexpected symbolic sizes format"
      | None -> StringMap.empty
    in
    sizes, symbolics
  | _ -> failwith "Unexpected symbolic file format"
;;

let find_symb_file dpt_file filename =
  if not (String.is_empty filename)
  then filename
  else if not (String.ends_with dpt_file ".dpt")
  then ""
  else (
    let symb_name = String.rchop ~n:4 dpt_file ^ ".symb" in
    if Sys.file_exists symb_name
    then (
      Console.report @@ "Auto-detected symbolic file " ^ symb_name;
      symb_name)
    else "")
;;

let eliminate_prog ds =
  let filename = find_symb_file Cmdline.cfg.dpt_file Cmdline.cfg.symb_file in
  let sizes, symbolics =
    if String.is_empty filename
    then StringMap.empty, StringMap.empty
    else parse_symbolics filename
  in
  List.map
    (fun d ->
      match d.d with
      | DSize (id, None) ->
        let sz =
          match StringMap.find_opt (Id.name id) sizes with
          | Some n -> IConst n
          | None ->
            Console.error
            @@ "Specification file contains no definition for size "
            ^ Printing.id_to_string id
        in
        { d with d = DSize (id, Some sz) }
      | DSymbolic (id, ty) ->
        let e =
          match StringMap.find_opt (Id.name id) symbolics with
          | Some j -> parse_symbolic sizes id ty j
          | None ->
            Console.error
            @@ "Specification file contains no definition for symbolic "
            ^ Printing.id_to_string id
        in
        { d with d = DConst (id, ty, e) }
      | _ -> d)
    ds
;;
