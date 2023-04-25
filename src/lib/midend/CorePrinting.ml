open Batteries
open CoreSyntax

let cfg = Cmdline.cfg
let concat_map sep f lst = lst |> List.map f |> String.concat sep
let list_to_string f lst = Printf.sprintf "[%s]" (concat_map "; " f lst)
let comma_sep f xs = concat_map "," f xs

let integer_to_string n =
  if cfg.verbose_types then Integer.to_string n else Integer.value_string n
;;

let location_to_string l = string_of_int l
let id_to_string id = if cfg.verbose_types then Id.to_string id else Id.name id

let cid_to_string cid =
  if cfg.verbose_types
  then Cid.to_string cid
  else String.concat "." @@ Cid.names cid
;;

let rec size_to_string = string_of_int
let wrap l r str = if String.equal str "" then "" else l ^ str ^ r
let sizes_to_string sizes = comma_sep size_to_string sizes |> wrap "<<" ">>"

let rec raw_ty_to_string t =
  match t with
  | TBool -> "bool"
  | TInt i -> "int<<" ^ size_to_string i ^ ">>"
  | TName (cid, sizes, b) ->
    cid_to_string cid
    ^ sizes_to_string sizes
    ^ if cfg.verbose_types then "{" ^ string_of_bool b ^ "}" else ""
  | TEvent -> "event"
  | TFun func -> func_to_string func
  | TMemop (n, size) -> Printf.sprintf "memop%d<<%s>>" n (size_to_string size)
  | TGroup -> "group"
  | TTable t ->
    " table_type {"
    ^ "\n\tkey_size: "
    ^ comma_sep size_to_string t.tkey_sizes
    ^ "\n\targ_ty: "
    ^ comma_sep ty_to_string t.tparam_tys
    ^ "\n\tret_ty: "
    ^ comma_sep ty_to_string t.tret_tys
    ^ "}\n"
  | TAction a ->
    Printf.sprintf
      "%s -> %s -> %s"
      (concat_map " * " ty_to_string a.aconst_param_tys)
      (concat_map " * " ty_to_string a.aparam_tys)
      (comma_sep ty_to_string a.aret_tys)
  | TPat s -> "pat<<" ^ size_to_string s ^ ">>"

and func_to_string func =
  let arg_tys = concat_map ", " ty_to_string func.arg_tys in
  let ret_ty = ty_to_string func.ret_ty in
  Printf.sprintf "(%s) -> %s" arg_tys ret_ty

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
  (* Unary operators *)
  | Not -> "!"
  | Neg -> "-"
  | BitNot -> "~"
  | Cast size -> "(int<<" ^ size_to_string size ^ ">>)"
  (* Binary operators *)
  | And -> "&&"
  | Or -> "||"
  | Eq -> "=="
  | Neq -> "!="
  | Less -> "<"
  | More -> ">"
  | Leq -> "<="
  | Geq -> ">="
  | Plus -> "+"
  | Sub -> "-"
  | SatPlus -> "|+|"
  | SatSub -> "|-|"
  | Conc -> "^"
  | BitAnd -> "&"
  | BitOr -> "|"
  | BitXor -> "^^"
  | LShift -> "<<"
  | RShift -> ">>"
  | Slice (n, m) -> Printf.sprintf "[%d : %d]" n m
  | PatExact -> ""
  | PatMask -> "&"
;;

let bs_to_string bs =
  "0b"
  ^ (bs
    |> List.map (function
         | 0 -> '0'
         | 1 -> '1'
         | _ -> '*')
    |> String.of_list)
;;

let rec v_to_string v =
  match v with
  | VBool true -> "true"
  | VBool false -> "false"
  | VInt i -> integer_to_string i
  | VEvent event -> event_to_string event
  | VGlobal i -> "global_" ^ string_of_int i
  | VGroup vs -> Printf.sprintf "{%s}" (comma_sep location_to_string vs)
  | VTuple vs -> Printf.sprintf "(%s)" (comma_sep v_to_string vs)
  | VPat bs -> bs_to_string bs

and value_to_string v = v_to_string v.v

and event_to_string { eid; data; edelay } =
  let delaystr =
    if edelay <> 0 && cfg.verbose_types
    then "@" ^ string_of_int edelay ^ "ns"
    else ""
  in
  Printf.sprintf
    "%s(%s)%s"
    (cid_to_string eid)
    (comma_sep value_to_string data)
    delaystr
;;

let rec e_to_string e =
  match e with
  | EVal v -> v_to_string v.v
  | EVar cid -> cid_to_string cid
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
  | EFlood e -> Printf.sprintf "flood %s" (exp_to_string e)
  | ETableCreate t ->
    Printf.sprintf
      "table_create<%s>((%s),%s, %s(%s))"
      (ty_to_string t.tty)
      (concat_map "," exp_to_string t.tactions)
      (exp_to_string t.tsize)
      (cid_to_string (fst t.tdefault))
      (comma_sep exp_to_string (snd t.tdefault))

and exp_to_string e = e_to_string e.e
and es_to_string es = comma_sep exp_to_string es

and params_to_string ps =
  comma_sep (fun (i, t) -> ty_to_string t ^ " " ^ id_to_string i) ps

and branch_to_string (ps, s) =
  Printf.sprintf
    "| %s -> {\n%s\n}"
    (comma_sep pat_to_string ps)
    (stmt_to_string s)

and action_to_string (name, (ps, stmt)) =
  Printf.sprintf
    "%s(%s) =\n\t{%s}"
    name
    (params_to_string ps)
    (stmt_to_string stmt)

and entry_to_string entry =
  Printf.sprintf
    "[%s](%s) -> %s(%s);"
    (string_of_int entry.eprio)
    (comma_sep exp_to_string entry.ematch)
    (id_to_string entry.eaction)
    (comma_sep exp_to_string entry.eargs)

and stmt_to_string s =
  match s.s with
  | SAssign (i, e) -> id_to_string i ^ " = " ^ exp_to_string e ^ ";"
  | SNoop -> ""
  | SGen (g, e) ->
    (match g with
     | GSingle None -> Printf.sprintf "generate %s;" (exp_to_string e)
     | _ ->
       let gen_str, loc =
         match g with
         | GSingle eo -> "generate_switch", Option.get eo
         | GMulti loc -> "generate_ports", loc
         | GPort loc -> "generate_port", loc
       in
       Printf.sprintf
         "%s (%s, %s);"
         gen_str
         (exp_to_string loc)
         (exp_to_string e))
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
  | SSeq (s1, s2) ->
    let str1, str2 = stmt_to_string s1, stmt_to_string s2 in
    if str1 = "" then str2 else if str2 = "" then str1 else str1 ^ "\n" ^ str2
  | SMatch (es, branches) ->
    let estr =
      let s = comma_sep exp_to_string es in
      if List.length es = 1 then s else "(" ^ s ^ ")"
    in
    "match " ^ estr ^ " with \n" ^ concat_map "\n" branch_to_string branches
  | SRet eopt ->
    let estr =
      match eopt with
      | Some e -> " " ^ exp_to_string e
      | None -> ""
    in
    Printf.sprintf "return%s;" estr
  | STableMatch tbl_rec ->
    if tbl_rec.out_tys <> None
    then
      Printf.sprintf
        "%s %s = table_match(%s, (%s), (%s));"
        (comma_sep ty_to_string (Option.get tbl_rec.out_tys))
        (comma_sep id_to_string tbl_rec.outs)
        (exp_to_string tbl_rec.tbl)
        (comma_sep exp_to_string tbl_rec.keys)
        (comma_sep exp_to_string tbl_rec.args)
    else
      Printf.sprintf
        "%s = table_match(%s);"
        (comma_sep id_to_string tbl_rec.outs)
        (comma_sep exp_to_string ((tbl_rec.tbl :: tbl_rec.keys) @ tbl_rec.args))
  | STableInstall (tbl_exp, entries) ->
    Printf.sprintf
      "table_install(%s, {\n\t%s\n\t}\n);"
      (exp_to_string tbl_exp)
      (List.map entry_to_string entries |> String.concat "\n")
;;

let statement_to_string = stmt_to_string

let event_sort_to_string sort =
  match sort with
  | EEntry -> "entry event"
  | EBackground -> "event"
;;

let memop_to_string body =
  match body with
  | MBReturn e -> "return " ^ exp_to_string e ^ ";\n"
  | MBIf (e1, e2, e3) ->
    Printf.sprintf
      "if %s then %s else %s"
      (exp_to_string e1)
      (exp_to_string e2)
      (exp_to_string e3)
  | MBComplex body ->
    let print_b b =
      match b with
      | None -> "None"
      | Some (id, e) -> Id.name id ^ "," ^ exp_to_string e |> wrap "(" ")"
    in
    let print_cr cro =
      match cro with
      | None -> "None"
      | Some (e1, e2) ->
        exp_to_string e1 ^ " -> " ^ exp_to_string e2 |> wrap "(" ")"
    in
    let print_calls calls =
      concat_map
        "\n"
        (fun (cid, es) ->
          Printf.sprintf
            "%s(%s);"
            (cid_to_string cid)
            (comma_sep exp_to_string es))
        calls
    in
    Printf.sprintf
      "{\nb1=%s;\nb2=%s\ncell1=%s, %s\ncell2=%s, %s\n%s\nret=%s\n}"
      (print_b body.b1)
      (print_b body.b2)
      (print_cr @@ fst body.cell1)
      (print_cr @@ snd body.cell1)
      (print_cr @@ fst body.cell2)
      (print_cr @@ snd body.cell2)
      (print_calls body.extern_calls)
      (print_cr @@ body.ret)
;;

let rec parser_action_to_string action =
  match action with
  | PSkip ty -> Printf.sprintf "skip %s;" (ty_to_string ty)
  | PRead (id, ty) ->
    Printf.sprintf "read %s : %s;" (id_to_string id) (ty_to_string ty)
  | PAssign (id, exp) ->
    Printf.sprintf "%s = %s;" (id_to_string id) (exp_to_string exp)

and parser_branch_to_string (pat, block) =
  Printf.sprintf "| %s -> %s" (pat_to_string pat) (parser_block_to_string block)

and parser_step_to_string step =
  match step with
  | PGen e -> Printf.sprintf "generate %s;" (exp_to_string e)
  | PCall e -> Printf.sprintf "%s;" (exp_to_string e)
  | PMatch (e, branches) ->
    Printf.sprintf
      "match %s with %s"
      (exp_to_string e)
      (concat_map "\n" parser_branch_to_string branches)
  | PDrop -> "drop"

and parser_block_to_string (actions, step) =
  concat_map "\n" (parser_action_to_string % fst) actions
  ^ "\n"
  ^ (parser_step_to_string % fst) step

and parser_to_string p = parser_block_to_string p

let d_to_string d =
  match d with
  | DGlobal (id, ty, exp) ->
    Printf.sprintf
      "global %s %s = %s;\n"
      (ty_to_string ty)
      (id_to_string id)
      (exp_to_string exp)
  | DHandler (id, hsort, (params, s)) ->
    Printf.sprintf
      "%shandle %s(%s) {\n%s\n}"
      (match hsort with
       | HControl -> "control "
       | HData -> ""
       | HEgress -> "@egress ")
      (id_to_string id)
      (params_to_string params)
      (stmt_to_string s)
  | DEvent (id, sort, params) ->
    Printf.sprintf
      "%s %s(%s);"
      (event_sort_to_string sort)
      (id_to_string id)
      (params_to_string params)
  | DMemop { mid; mparams; mbody } ->
    Printf.sprintf
      "memop %s(%s)\n {%s}"
      (id_to_string mid)
      (params_to_string mparams)
      (memop_to_string mbody)
  | DExtern (id, ty) ->
    Printf.sprintf "extern %s %s;" (id_to_string id) (ty_to_string ty)
  | DAction acn ->
    (* id, ret_tys, const_params, (dyn_params, acn_body)) ->  *)
    Printf.sprintf
      "action (%s) %s(%s)(%s) {\n\taction_return (%s)\n}\n"
      (comma_sep ty_to_string acn.artys)
      (id_to_string acn.aid)
      (params_to_string acn.aconst_params)
      (params_to_string acn.aparams)
      (comma_sep exp_to_string acn.abody)
  | DParser (id, params, parser) ->
    Printf.sprintf
      "parser %s(%s) {\n%s\n}\n"
      (id_to_string id)
      (params_to_string params)
      (parser_block_to_string parser)
;;

let decl_to_string d = d_to_string d.d
let decls_to_string ds = concat_map "\n\n" decl_to_string ds
