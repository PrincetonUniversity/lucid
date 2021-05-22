open Batteries
open Syntax
open SyntaxUtils

let cfg = Cmdline.cfg
let concat_map sep f lst = lst |> List.map f |> String.concat sep
let list_to_string f lst = Printf.sprintf "[%s]" (concat_map "; " f lst)
let comma_sep f xs = concat_map "," f xs

let integer_to_string n =
  if cfg.verbose_types then Integer.to_string n else Integer.value_string n
;;

let location_to_string l = integer_to_string l
let id_to_string id = if cfg.verbose_types then Id.to_string id else Id.name id

let cid_to_string cid =
  if cfg.verbose_types
  then Cid.to_string cid
  else String.concat "." @@ Cid.names cid
;;

let tqvar_to_string_friendly f t =
  match t with
  | QVar id -> "'" ^ Id.to_string id
  | TVar { contents = Unbound (id, _) } -> "?" ^ Id.to_string id
  | TVar { contents = Link x } -> f x
;;

let tqvar_to_string_full f t =
  match t with
  | QVar id -> "Q{" ^ Id.to_string id ^ "}"
  | TVar { contents = Unbound (id, l) } ->
    "T{" ^ Id.to_string id ^ ", " ^ string_of_int l ^ "}"
  | TVar { contents = Link x } ->
    let xstr = f x in
    if cfg.show_tvar_links then "T{" ^ xstr ^ "}" else xstr
;;

let tqvar_to_string tqv =
  if cfg.verbose_types
  then tqvar_to_string_full tqv
  else tqvar_to_string_friendly tqv
;;

let rec size_to_string s =
  match s with
  | IVar tqv -> tqvar_to_string size_to_string tqv
  | IConst i -> string_of_int i
  | IUser cid -> cid_to_string cid
  | ISum (tqvs, n) ->
    concat_map " + " size_to_string tqvs ^ " + " ^ string_of_int n
;;

let sizes_to_string sizes = comma_sep size_to_string sizes
let wrap l r str = if String.equal str "" then "" else l ^ str ^ r

let rec effect_to_string e =
  let wrap_hd hd str =
    if hd <= 0 then str else "(" ^ str ^ "+" ^ string_of_int hd ^ ")"
  in
  let base_str, tl =
    match unwrap_effect e with
    | FZero, hd :: tl -> string_of_int hd, tl
    | FVar tqv, hd :: tl ->
      tqvar_to_string effect_to_string tqv |> wrap_hd hd, tl
    | _ -> failwith "impossible"
  in
  List.fold_left (fun acc n -> acc ^ "." ^ string_of_int n) base_str tl
;;

let constraint_to_string c =
  match c with
  | CLeq (e1, e2) -> effect_to_string e1 ^ " <= " ^ effect_to_string e2
;;

let cspecs_to_string constraints =
  let cmp_to_string cmp =
    match cmp with
    | SpecLess -> " < "
    | SpecLeq -> " <= "
  in
  if List.length constraints = 0
  then ""
  else
    " "
    ^ list_to_string
        (function
          | CSpec lst ->
            fst
            @@ List.fold_left
                 (fun (acc, prev) curr ->
                   ( acc
                     ^ cid_to_string (fst prev)
                     ^ cmp_to_string (snd prev)
                     ^ cid_to_string (fst curr)
                   , curr ))
                 ("", List.hd lst)
                 (List.tl lst)
          | CEnd cid1 -> "end " ^ cid_to_string cid1)
        constraints
;;

let gty_to_string (cid, sizes) =
  cid_to_string cid ^ "<<" ^ comma_sep size_to_string sizes ^ ">>"
;;

let rec raw_ty_to_string t =
  match t with
  | TQVar tqv -> tqvar_to_string raw_ty_to_string tqv
  | TBool -> "bool"
  | TInt i -> "int<<" ^ size_to_string i ^ ">>"
  | TGlobal (gty, effect) ->
    let eff =
      if cfg.show_effects then "(" ^ effect_to_string effect ^ ")" else ""
    in
    gty_to_string gty ^ eff
  | TEvent b -> if b then "mevent" else "event"
  | TFun func -> func_to_string func
  | TMemop (size, ty) ->
    Printf.sprintf
      "memop[int<<%s>>, %s]"
      (size_to_string size)
      (raw_ty_to_string ty)
  | TVoid -> "void"
  | TGroup -> "group"

and func_to_string func =
  let arg_tys = concat_map ", " raw_ty_to_string func.arg_tys in
  let ret_ty = raw_ty_to_string func.ret_ty in
  if cfg.show_effects
  then
    Printf.sprintf
      "[%s] => (%s, %s) -> (%s, %s)"
      (comma_sep constraint_to_string !(func.constraints))
      arg_tys
      (effect_to_string func.start_eff)
      ret_ty
      (effect_to_string func.end_eff)
  else Printf.sprintf "(%s) -> %s" arg_tys ret_ty

and ty_to_string t = raw_ty_to_string t.raw_ty

let pat_to_string p =
  match p with
  | PWild -> "_"
  | PNum n -> Z.to_string n
  | PBit bs ->
    "0b"
    ^ (bs
      |> List.map (function
             | 0 -> '0'
             | 1 -> '1'
             | _ -> '*')
      |> String.of_list)
;;

let op_to_string op =
  match op with
  | And -> "&&"
  | Or -> "||"
  | Not -> "!"
  | Eq -> "=="
  | Neq -> "!="
  | Less -> "<"
  | More -> ">"
  | Leq -> "<="
  | Geq -> ">="
  | Plus -> "+"
  | Sub -> "-"
  | SatSub -> "-"
  | Cast size -> "(int<<" ^ size_to_string size ^ ">>)"
  | Conc -> "^"
  | BitAnd -> "&"
  | BitOr -> "|"
  | LShift -> "<<"
  | RShift -> ">>"
  | Slice (n, m) -> Printf.sprintf "[%d : %d]" n m
;;

let rec v_to_string v =
  match v with
  | VBool true -> "true"
  | VBool false -> "false"
  | VInt i -> integer_to_string i
  | VEvent event -> event_to_string event
  | VGlobal i -> "global_" ^ string_of_int i
  | VGroup vs -> Printf.sprintf "{%s}" (comma_sep location_to_string vs)

and value_to_string v = v_to_string v.v

and event_to_string { eid; data; edelay; elocations } =
  let locstr =
    match elocations with
    | [] -> "self"
    | _ -> list_to_string location_to_string elocations
  in
  let locstr = if cfg.verbose then "@" ^ locstr else "" in
  let delaystr =
    if edelay <> 0 && cfg.verbose_types
    then "@" ^ string_of_int edelay ^ "ns"
    else ""
  in
  Printf.sprintf
    "%s(%s)%s%s"
    (cid_to_string eid)
    (comma_sep value_to_string data)
    delaystr
    locstr
;;

let rec e_to_string e =
  match e with
  | EVal v -> v_to_string v.v
  | EVar cid -> cid_to_string cid
  | EInt (z, size) ->
    Z.to_string z
    ^
    (match Option.map STQVar.strip_links size with
    | None -> ""
    | Some (IConst 32) -> ""
    | Some size -> "<<" ^ size_to_string size ^ ">>")
  | EOp (op, [e]) -> op_to_string op ^ exp_to_string e
  | EOp (op, [e1; e2]) -> exp_to_string e1 ^ op_to_string op ^ exp_to_string e2
  | EOp (op, es) ->
    error
      ("wrong number of arguments ("
      ^ string_of_int (List.length es)
      ^ ") to "
      ^ op_to_string op)
  | ECall (cid, es) ->
    Printf.sprintf "%s(%s)" (cid_to_string cid) (es_to_string es)
  | EHash (size, es) ->
    Printf.sprintf "hash<<%s>>(%s)" (size_to_string size) (es_to_string es)
  | EProj (e, l) -> exp_to_string e ^ "#" ^ l
  | ERecord lst ->
    Printf.sprintf
      "{%s}"
      (concat_map "; " (fun (str, exp) -> str ^ " = " ^ exp_to_string exp) lst)

and exp_to_string e = e_to_string e.e
and es_to_string es = comma_sep exp_to_string es

let params_to_string ps =
  comma_sep (fun (i, t) -> ty_to_string t ^ " " ^ id_to_string i) ps
;;

let rec branch_to_string (ps, s) =
  Printf.sprintf
    "| %s -> {\n%s\n}"
    (comma_sep pat_to_string ps)
    (stmt_to_string s)

and stmt_to_string s =
  match s.s with
  | SAssign (i, e) -> id_to_string i ^ " = " ^ exp_to_string e ^ ";"
  | SNoop -> ""
  | SGen (b, e) ->
    Printf.sprintf "%sgenerate %s;" (if b then "m" else "") (exp_to_string e)
  | SLocal (i, t, e) ->
    Printf.sprintf
      "%s %s = %s;"
      (raw_ty_to_string t.raw_ty)
      (id_to_string i)
      (exp_to_string e)
  | SPrintf (s, es) ->
    Printf.sprintf "printf \"%s\" %s;" s (comma_sep exp_to_string es)
  | SUnit e -> exp_to_string e ^ ";"
  | SIf (e, s1, s2) ->
    let s2_str =
      match s2.s with
      | SNoop -> ""
      | _ -> "else {\n" ^ stmt_to_string s2 ^ "\n}"
    in
    Printf.sprintf
      "if (%s) {\n%s\n} %s"
      (exp_to_string e)
      (stmt_to_string s1)
      s2_str
  | SRet eopt ->
    let estr =
      match eopt with
      | Some e -> " " ^ exp_to_string e
      | None -> ""
    in
    Printf.sprintf "return%s;" estr
  | SSeq (s1, s2) -> stmt_to_string s1 ^ "\n" ^ stmt_to_string s2
  | SMatch (es, branches) ->
    let estr =
      let s = comma_sep exp_to_string es in
      if List.length es = 1 then s else "(" ^ s ^ ")"
    in
    "match " ^ estr ^ " with \n" ^ concat_map "\n" branch_to_string branches
;;

let event_sort_to_string sort =
  match sort with
  | EEntry true -> "entry control event"
  | EEntry false -> "entry event"
  | EExit -> "exit event"
  | EBackground -> "event"
;;

let rec interface_spec_to_string spec =
  match spec.ispec with
  | InSize id -> Printf.sprintf "size %s;" (id_to_string id)
  | InVar (id, ty) ->
    Printf.sprintf "%s %s;" (ty_to_string ty) (id_to_string id)
  | InGlobalTy (id, size_names, body) ->
    if not (List.is_empty body)
    then d_to_string (DGlobalTy (id, size_names, body))
    else
      Printf.sprintf
        "global type %s%s;"
        (id_to_string id)
        (comma_sep id_to_string size_names |> wrap "<<" ">>")
  | InConstr (constr_id, ty_id, size_names, params) ->
    Printf.sprintf
      "constr %s%s %s(%s);"
      (cid_to_string ty_id)
      (comma_sep id_to_string size_names |> wrap "<<" ">>")
      (id_to_string constr_id)
      (params_to_string params)
  | InFun (id, rty, cspecs, params) ->
    Printf.sprintf
      "fun %s %s(%s)%s;"
      (ty_to_string rty)
      (id_to_string id)
      (params_to_string params)
      (cspecs_to_string cspecs)
  | InEvent (id, cspecs, params) ->
    Printf.sprintf
      "event %s(%s)%s;"
      (id_to_string id)
      (params_to_string params)
      (cspecs_to_string cspecs)
  | InModule (id, intf) ->
    Printf.sprintf
      "module %s : {\n%s\n}"
      (id_to_string id)
      (interface_to_string intf)

and interface_to_string specs =
  "{" ^ concat_map "\n" interface_spec_to_string specs ^ "}"

and d_to_string d =
  match d with
  | DGlobal (id, gty, cid, args) ->
    Printf.sprintf
      "global %s %s = %s(%s);\n"
      (gty_to_string gty)
      (id_to_string id)
      (cid_to_string cid)
      (comma_sep exp_to_string args)
  | DHandler (id, (params, s)) ->
    Printf.sprintf
      "handle %s(%s) {\n%s\n}\n"
      (id_to_string id)
      (params_to_string params)
      (stmt_to_string s)
  | DEvent (id, sort, cspecs, params) ->
    Printf.sprintf
      "%s %s(%s) %s;\n"
      (event_sort_to_string sort)
      (id_to_string id)
      (params_to_string params)
      (cspecs_to_string cspecs)
  | DFun (id, rty, cspecs, (params, s)) ->
    Printf.sprintf
      "fun %s %s(%s)%s {\n%s\n}\n"
      (ty_to_string rty)
      (id_to_string id)
      (params_to_string params)
      (cspecs_to_string cspecs)
      (stmt_to_string s)
  | DMemop (id, (params, s)) ->
    Printf.sprintf
      "memop %s(%s)\n {%s}\n"
      (id_to_string id)
      (params_to_string params)
      (stmt_to_string s)
  | DSize (id, size) ->
    Printf.sprintf "size %s = %s;" (id_to_string id) (size_to_string size)
  | DConst (id, ty, e) ->
    Printf.sprintf
      "const %s %s = %s;"
      (id_to_string id)
      (ty_to_string ty)
      (exp_to_string e)
  | DExtern (id, ty) ->
    Printf.sprintf "extern %s %s;" (id_to_string id) (ty_to_string ty)
  | DGroup (id, es) ->
    Printf.sprintf
      "group %s = {%s};"
      (id_to_string id)
      (comma_sep exp_to_string es)
  | DGlobalTy (id, ids, params) ->
    Printf.sprintf
      "global type %s%s = {\n%s}"
      (id_to_string id)
      (comma_sep id_to_string ids |> wrap "<<" ">>")
      (List.fold_right
         (fun (id, ty) acc ->
           ty_to_string ty ^ "  " ^ id_to_string id ^ ";\n" ^ acc)
         params
         "")
  | DConstr { constr_id; ty_id; size_args; params; body } ->
    Printf.sprintf
      "constr %s%s %s(%s) {\n%s\n}"
      (cid_to_string ty_id)
      (comma_sep id_to_string size_args |> wrap "<<" ">>")
      (id_to_string constr_id)
      (params_to_string params)
      (decls_to_string body)
  | DModule (id, intf, ds) ->
    let intf_str =
      if List.is_empty intf then "" else " : " ^ interface_to_string intf
    in
    Printf.sprintf
      "module %s%s {\n%s\n}\n"
      (id_to_string id)
      intf_str
      (decls_to_string ds)

and decl_to_string d = d_to_string d.d
and decls_to_string ds = concat_map "\n" decl_to_string ds
