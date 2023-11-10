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

let location_to_string l = string_of_int l
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

let wrap l r str = if String.equal str "" then "" else l ^ str ^ r
let sizes_to_string sizes = comma_sep size_to_string sizes |> wrap "<<" ">>"

let rec effect_to_string e =
  let wrap_hd hd str =
    if hd <= 0 then str else "(" ^ str ^ "+" ^ string_of_int hd ^ ")"
  in
  let base_str, tl =
    match unwrap_effect e with
    | FZero, (_, hd) :: tl -> string_of_int hd, tl
    | FVar tqv, (_, hd) :: tl ->
      tqvar_to_string effect_to_string tqv |> wrap_hd hd, tl
    | _ -> failwith "impossible"
  in
  List.fold_left
    (fun acc (o, n) ->
      let str =
        match o with
        | None -> string_of_int n
        | Some id -> wrap_hd n (id_to_string id)
      in
      acc ^ "." ^ str)
    base_str
    tl
;;

let rec constraint_to_string c =
  match c with
  (* Possibly look into forall operators later *)
  (* | CForAll (i, cs) ->
      Printf.sprintf
        "forall %s > 0.(%s)"
        (id_to_string i)
        (comma_sep constraint_to_string cs) *)
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

let rec raw_ty_to_string t =
  match t with
  | TQVar tqv -> tqvar_to_string raw_ty_to_string tqv
  | TBool -> "bool"
  | TInt i -> "int<<" ^ size_to_string i ^ ">>"
  | TName (cid, sizes, b) ->
    cid_to_string cid
    ^ sizes_to_string sizes
    ^ if cfg.verbose_types then "{" ^ string_of_bool b ^ "}" else ""
  | TAbstract (cid, sizes, b, _) ->
    let base =
      if cfg.verbose_types
      then Printf.sprintf "Abs[%s]" (cid_to_string cid)
      else cid_to_string cid
    in
    base
    ^ sizes_to_string sizes
    ^ if cfg.verbose_types then "{" ^ string_of_bool b ^ "}" else ""
  | TEvent -> "event"
  | TFun func -> func_to_string func
  | TMemop (n, size) -> Printf.sprintf "memop%d<<%s>>" n (size_to_string size)
  | TVoid -> "void"
  | TGroup -> "group"
  | TRecord lst ->
    "{"
    ^ concat_map "; " (fun (str, ty) -> raw_ty_to_string ty ^ " " ^ str) lst
    ^ "}"
  | TVector (ty, size) ->
    Printf.sprintf "%s[%s]" (raw_ty_to_string ty) (size_to_string size)
  | TTuple tys -> "(" ^ concat_map " * " raw_ty_to_string tys ^ ")"
  | TTable t ->
    " table_type {"
    ^ "\n\tkey_size: "
    ^ comma_sep ty_to_string t.tkey_sizes
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
  | TPat s -> Printf.sprintf "pat<<%s>>" (size_to_string s)

and func_to_string func =
  let arg_tys = concat_map ", " ty_to_string func.arg_tys in
  let ret_ty = ty_to_string func.ret_ty in
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

and ty_to_string t =
  let eff_str =
    if cfg.show_effects && (is_global t || cfg.show_all_effects)
    then effect_to_string t.teffect |> wrap "(" ")"
    else ""
  in
  match !(t.tprint_as) with
  | Some raw_ty when cfg.use_type_names && false ->
    raw_ty_to_string raw_ty ^ eff_str
  | _ -> raw_ty_to_string t.raw_ty ^ eff_str
;;

let pat_to_string p =
  match p with
  | PWild -> "_"
  | PNum n -> Z.to_string n
  | PVar (cid, _) -> cid_to_string cid
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
  | Slice (n, m) ->
    Printf.sprintf "[%s : %s]" (size_to_string n) (size_to_string m)
  | TGet (n, m) -> Printf.sprintf "[%d get %d]" n m
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
  | ECall (cid, es, unordered) ->
    if (unordered) then 
      Printf.sprintf "%s(%s)" (cid_to_string cid) (es_to_string es)
    else
      Printf.sprintf "%s<unordered>(%s)" (cid_to_string cid) (es_to_string es)
  | EHash (size, es) ->
    Printf.sprintf "hash<<%s>>(%s)" (size_to_string size) (es_to_string es)
  | EFlood e -> Printf.sprintf "flood %s" (exp_to_string e)
  | EProj (e, l) -> exp_to_string e ^ "#" ^ l
  | ERecord lst ->
    Printf.sprintf
      "{%s}"
      (concat_map "; " (fun (str, exp) -> str ^ " = " ^ exp_to_string exp) lst)
  | EWith (base, lst) ->
    Printf.sprintf
      "{%s with %s}"
      (exp_to_string base)
      (concat_map "; " (fun (str, exp) -> str ^ " = " ^ exp_to_string exp) lst)
  | EVector es -> list_to_string exp_to_string es
  | EComp (e, i, k) ->
    Printf.sprintf
      "[%s for %s < %s]"
      (exp_to_string e)
      (id_to_string i)
      (size_to_string k)
  | EIndex (e, i) ->
    Printf.sprintf "%s[%s]" (exp_to_string e) (size_to_string i)
  | ETuple es -> "(" ^ concat_map ", " exp_to_string es ^ ")"
  | ESizeCast (sz1, sz2) ->
    Printf.sprintf "to_int<<%s>>(%s)" (size_to_string sz1) (size_to_string sz2)
  | EStmt (s, e) ->
    Printf.sprintf "{%s; return %s}" (stmt_to_string s) (exp_to_string e)
  | ETableCreate t ->
    Printf.sprintf
      "table_create<%s>((%s),%s, %s)"
      (ty_to_string t.tty)
      (concat_map "," exp_to_string t.tactions)
      (exp_to_string t.tsize)
      (exp_to_string t.tdefault)
      (* (cid_to_string (fst t.tdefault))
      (comma_sep exp_to_string (snd t.tdefault)) *)
  | ETableMatch tr ->
    Printf.sprintf "table_match(%s);" (comma_sep exp_to_string tr.args)
  | EPatWild _ -> "_"

and exp_to_string e = e_to_string e.e
(* ^ Printf.sprintf "[ty:%s]"
  @@ Option.map_default ty_to_string "" e.ety *)

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

and s_to_string s = 
  match s with 
  | SAssign (i, e) -> id_to_string i ^ " = " ^ exp_to_string e ^ ";"
  | SNoop -> "skip;"
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
  | SRet eopt ->
    let estr =
      match eopt with
      | Some e -> " " ^ exp_to_string e
      | None -> ""
    in
    Printf.sprintf "return%s;" estr
  | SSeq (s1, s2) ->
    let str1, str2 = stmt_to_string s1, stmt_to_string s2 in
    if str1 = "" then str2 else if str2 = "" then str1 else str1 ^ "\n" ^ str2
  | SMatch (es, branches) ->
    let estr =
      let s = comma_sep exp_to_string es in
      if List.length es = 1 then s else "(" ^ s ^ ")"
    in
    "match " ^ estr ^ " with \n" ^ concat_map "\n" branch_to_string branches
  | SLoop (s, i, k) ->
    Printf.sprintf
      "for (%s < %s) {\n%s\n}"
      (id_to_string i)
      (size_to_string k)
      (stmt_to_string s)
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
  | STableInstall (id, entries) ->
    Printf.sprintf
      "table_install(%s, {\n\t%s\n\t}\n);"
      (exp_to_string id)
      (List.map entry_to_string entries |> String.concat "\n")
and stmt_to_string stmt =
  let s_str = s_to_string stmt.s in
  let prag_str = match stmt.spragmas with
    | [] -> ""
    | _ -> " //" ^ (Pragma.to_strings stmt.spragmas)
  in
  s_str ^ prag_str
;;

let statement_to_string = stmt_to_string

let event_sort_to_string sort =
  match sort with
  | EPacket -> "entry event"
  | EBackground -> "event"
;;

let rec interface_spec_to_string spec =
  match spec.ispec with
  | InSize id -> Printf.sprintf "size %s;" (id_to_string id)
  | InVar (id, ty) ->
    Printf.sprintf "%s %s;" (ty_to_string ty) (id_to_string id)
  | InTy (id, sizes, tyo, b) ->
    let prefix = if b then "global " else "" in
    let decl = prefix ^ "type " ^ id_to_string id ^ sizes_to_string sizes in
    (match tyo with
     | None -> decl ^ ";"
     | Some ty -> decl ^ " = " ^ ty_to_string ty)
  | InConstr (id, ty, params) ->
    Printf.sprintf
      "constr %s %s(%s);"
      (ty_to_string ty)
      (id_to_string id)
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
  "{\n" ^ concat_map "\n\n" interface_spec_to_string specs ^ "\n}\n"

and memop_to_string body = stmt_to_string (memop_body_to_stmt body)

and parser_action_to_string action =
  match action with
  | PSkip ty -> Printf.sprintf "skip %s;" (ty_to_string ty)
  | PRead (id, ty) ->
    Printf.sprintf "read %s : %s;" (id_to_string id) (ty_to_string ty)
  | PAssign (lval, exp) ->
    Printf.sprintf "%s = %s;" (exp_to_string lval) (exp_to_string exp)
  | PLocal (id, ty, exp) ->
    Printf.sprintf "%s %s = %s;" (ty_to_string ty) (id_to_string id) (exp_to_string exp)
  
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
  | PDrop -> "drop;"

and parser_block_to_string (actions, step) =
  concat_map "\n" (parser_action_to_string % fst) actions
  ^ "\n"
  ^ (parser_step_to_string % fst) step

and parser_to_string p = parser_block_to_string p

and d_to_string d =
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
       | HEgress -> "@egress")
      (id_to_string id)
      (params_to_string params)
      (stmt_to_string s)
  | DEvent (id, annot, sort, cspecs, params) ->
    Printf.sprintf
      "%s %s%s(%s) %s;"
      (event_sort_to_string sort)
      (id_to_string id)
      (Option.map_default (fun n -> "@" ^ string_of_int n) "" annot)
      (params_to_string params)
      (cspecs_to_string cspecs)
  | DFun (id, rty, cspecs, (params, s)) ->
    Printf.sprintf
      "fun %s %s(%s)%s {\n%s\n}"
      (ty_to_string rty)
      (id_to_string id)
      (params_to_string params)
      (cspecs_to_string cspecs)
      (stmt_to_string s)
  | DMemop (id, params, mbody) ->
    Printf.sprintf
      "memop %s(%s)\n {%s}"
      (id_to_string id)
      (params_to_string params)
      (memop_to_string mbody)
  | DSize (id, szo) -> begin
    match szo with
    | None -> Printf.sprintf "size %s;" (id_to_string id)
    | Some size ->
      Printf.sprintf "size %s = %s;" (id_to_string id) (size_to_string size)
  end
  | DConst (id, ty, e) ->
    Printf.sprintf
      "const %s %s = %s;"
      (id_to_string id)
      (ty_to_string ty)
      (exp_to_string e)
  | DExtern (id, ty) ->
    Printf.sprintf "extern %s %s;" (id_to_string id) (ty_to_string ty)
  | DSymbolic (id, ty) ->
    Printf.sprintf "symbolic %s %s;" (id_to_string id) (ty_to_string ty)
  | DUserTy (id, sizes, ty) ->
    Printf.sprintf
      "type %s%s = %s"
      (id_to_string id)
      (sizes_to_string sizes)
      (ty_to_string ty)
  | DConstr (id, ty, params, exp) ->
    Printf.sprintf
      "constr %s %s(%s) = %s;"
      (ty_to_string ty)
      (id_to_string id)
      (params_to_string params)
      (exp_to_string exp)
  | DModule (id, intf, ds) ->
    let intf_str =
      if List.is_empty intf then "" else " : " ^ interface_to_string intf
    in
    Printf.sprintf
      "module %s%s {\n%s\n}"
      (id_to_string id)
      intf_str
      (decls_to_string ds)
  | DModuleAlias (id1, e, cid1, cid2) ->
    Printf.sprintf
      "module %s = %s if %s else %s"
      (id_to_string id1)
      (cid_to_string cid1)
      (exp_to_string e)
      (cid_to_string cid2)
  | DAction (id, ret_tys, const_params, (dyn_params, acn_body)) ->
    Printf.sprintf
      "action (%s) %s(%s)(%s) {\n\taction_return (%s)\n}\n"
      (comma_sep ty_to_string ret_tys)
      (id_to_string id)
      (params_to_string const_params)
      (params_to_string dyn_params)
      (comma_sep exp_to_string acn_body)
  | DParser (id, params, parser) ->
    Printf.sprintf
      "parser %s(%s) {\n%s\n}\n"
      (id_to_string id)
      (params_to_string params)
      (parser_block_to_string parser)

and decl_to_string d = d_to_string d.d
and decls_to_string ds = concat_map "\n\n" decl_to_string ds
