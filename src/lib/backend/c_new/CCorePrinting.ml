open CCoreSyntax
let def_tystr = true (* show type string in show_exp *)
let sprintf = Printf.sprintf

(* split on newlines, indent each newline by n spaces, combine back into string *)
let indent n str = str |> String.split_on_char '\n' |> List.map (fun s -> String.make n ' ' ^ s) |> String.concat "\n"

let op_to_string = show_op

let show_id id = fst id
let show_cid cid = Cid.names cid |> String.concat "."

let show_size = function 
  | SConst sz -> string_of_int sz
  | SVar id -> show_id id

let rec show_ty ty = show_raw_ty ty.raw_ty
and show_raw_ty = function 
  | TUnit -> "unit"
  | TInt sz -> sprintf "int<%s>" (show_size sz)
  | TBool -> "bool"
  | TRecord{labels; ts} -> (
    match labels with 
    | Some(labels) -> 
      let labels = List.map show_id labels in
      let ts = List.map show_ty ts in
      let fields = List.combine labels ts in
      let fields = List.map (fun (l, t) -> l ^ ": " ^ t) fields in
      sprintf "{%s}" (String.concat ";" fields)
    | None -> 
      let ts = List.map show_ty ts in
      sprintf "(%s)" (String.concat ", " ts)
  )
  | TFun{arg_tys; ret_ty; func_kind} -> 
    let arg_tys = List.map show_ty arg_tys in
    let arg_tys = String.concat ", " arg_tys in
    let ret_ty = show_ty ret_ty in
    let func_kind = show_func_kind func_kind in
    sprintf "%s<%s -> %s>" func_kind arg_tys ret_ty
  | TName(cid, ty_args) -> 
    let cid = show_cid cid in
    let ty_args = List.map show_ty ty_args in
    let ty_args = String.concat ", " ty_args in
    sprintf "%s<%s>" cid ty_args
  | TBits{ternary; len} -> 
    let ternary = if ternary then "pattern" else "bitstring" in
    sprintf "%s<%s>" ternary (show_size len)
  | TEvent -> "event"
  | TEnum(tags) -> 
    let tag_str = List.map (fun (tag, i) -> sprintf "%s = %d" (tag) i) tags in
    sprintf "{%s}" (String.concat " | " tag_str)
  | TList(ty, len) -> 
    sprintf "%s[%s]" (show_ty ty) (show_size len)

let show_params params = 
  let params = List.map (fun (id, ty) -> sprintf "%s: %s" (show_id id) (show_ty ty)) params in
  String.concat ", " params
;;

let rec show_value v = show_v v.v

and show_v = function
  | VUnit -> "()"
  | VInt {value} -> string_of_int value
  | VBool b -> string_of_bool b
  | VRecord{labels; es} -> (
    match labels with 
    | Some(labels) -> 
      let labels = List.map show_id labels in
      let values = List.map show_value es in
      let fields = List.combine labels values in
      let fields = List.map (fun (l, v) -> l ^ ": " ^ v) fields in
      sprintf "{%s}" (String.concat ";" fields)
    | None -> 
      let values = List.map show_value es in
      sprintf "(%s)" (String.concat ", " values)
  )
  | VList(values) -> 
    let values = List.map show_value values in
    sprintf "[%s]" (String.concat ", " values)
  (* | VClosure{env; params; fexp} -> 
    let env = List.map (fun (id, v) -> sprintf "%s = %s" (show_id id) (show_value v)) env in
    let env = String.concat "; " env in
    let params = show_params params in
    let fexp = show_exp fexp in
    sprintf "{%s} (%s) -> %s" env params fexp *)
  | VGlobal{global_id; global_pos; global_ty} -> 
    let id = show_id global_id in
    let int_addr = string_of_int global_pos in
    let ty = show_ty global_ty in
    sprintf "ref %s[@%s]: %s" id int_addr ty
  | VBits {bits} -> 
    let rec bits_to_str bits = 
      match bits with 
        | [] -> ""
        | 0::bits -> "0" ^ bits_to_str bits
        | 1::bits -> "1" ^ bits_to_str bits
        | -1::bits -> "*" ^ bits_to_str bits
        | _ -> failwith "invalid int to represent a bit"
    in
    sprintf "0b%s" (bits_to_str bits)
  | VEvent{evid; evdata} -> 
    let evid = show_cid evid in
    let evdata = List.map show_value evdata |> String.concat ", " in
    sprintf "%s(%s)" evid evdata
  | VEnum(tag, _) -> tag

let rec show_exp ?(tystr=def_tystr) exp  = 
  if (tystr) then sprintf "(%s: %s)" (show_e exp.e) (show_ty exp.ety)
  else sprintf "%s" (show_e exp.e)
and show_e = function 
    | EVal v -> show_value v
    | EVar(cid) -> show_cid cid
    | ERecord{labels; es} -> (
      match labels with 
      | Some(labels) -> 
        let labels = List.map show_id labels in
        let exps = List.map show_exp es in
        let fields = List.combine labels exps in
        let fields = List.map (fun (l, v) -> l ^ ": " ^ v) fields in
        sprintf "{%s}" (String.concat ";" fields)
      | None -> 
        let exps = List.map show_exp es in
        sprintf "(%s)" (String.concat ", " exps)
    )
    | ECall{f; args;} -> 
      let f = show_exp f in
      let args = List.map show_exp args in
      let args = String.concat ", " args in
      sprintf "%s(%s)" f args
    | EOp(op, args) -> 
      let op = show_op op in
      let args = List.map show_exp args in
      let args = String.concat ", " args in
      sprintf "%s(%s)" op args
    (* | EClosure{env; params; fexp} -> 
      let env = List.map (fun (id, v) -> sprintf "%s = %s" (show_id id) (show_exp v)) env in
      let env = String.concat "; " env in
      let params = show_params params in
      let fexp = show_exp fexp in
      sprintf "{%s} (%s) -> %s" env params fexp *)
;;

let show_pat = function 
  | PVal v -> show_value v
  | PEvent {event_id; params} -> 
    let event_id = show_cid event_id in
    let params = show_params params in
    sprintf "%s(%s)" event_id params
;;
let show_branch (pats, branch_tgt) = 
  let pats = List.map show_pat pats in
  let pats = String.concat ", " pats in
  let branch_tgt = match branch_tgt with 
  s -> show_statement s
  in
  sprintf "| %s -> {%s}" pats branch_tgt
;;


let rec show_statement s = show_s s.s
and show_s = function 
  | SNoop -> ""
  | SUnit(e) -> show_exp e ^ ";"
  | SAssign{ids; tys; new_vars; exp} -> 
    let ids = List.map show_cid ids in
    let tys = List.map show_ty tys in
    let ty_ids = List.combine ids tys in
    let ty_ids = List.map (fun (id, ty) -> id ^ ": " ^ ty) ty_ids in
    let ty_ids = String.concat ", " ty_ids in
    let exp = show_exp exp in
    if new_vars then sprintf "%s := %s;" ty_ids exp
    else sprintf "%s := %s;" (String.concat ", " ids) exp
  | SIf(exp, s1, s2) -> 
    let exp = show_exp exp in
    let s1 = show_statement s1 |> indent 2 in
    let s2 = show_statement s2 |> indent 2 in
    sprintf "if(%s){\n%s\n} else {\n%s\n}" exp s1 s2
  | SMatch(exp, branches) ->
    let exp = show_exp exp in
    let branches = List.map show_branch branches |> String.concat "\n" |> indent 2 in    
    sprintf "match(%s) with\n%s\n" exp branches
  | SListSet{arr; idx; exp} -> 
    let cid, _ = extract_evar arr in
    sprintf "%s[%s] := %s;" (show_cid cid) (show_size idx) (show_exp exp)
  | SSeq(s1, s2) -> 
    let s1 = show_statement s1 in
    let s2 = show_statement s2 in
    sprintf "%s\n%s" s1 s2
  | SRet(None) -> "return;"
  | SRet(Some(exp)) -> sprintf "return %s;" (show_exp exp)

let rec show_decl decl = show_d decl.d
and show_d = function 
    | DVar(id, ty, exp_opt) -> (
      match exp_opt with 
      | [] -> sprintf "extern %s: %s;" (show_id id) (show_ty ty)
      | [exp] -> sprintf "%s: %s := %s;" (show_id id) (show_ty ty) (show_exp exp)
      | exps -> 
        (* list constructor *)
        sprintf "%s: %s := {%s};" (show_id id) (show_ty ty) (List.map show_exp exps |> String.concat ", ")
    )
    | DFun(func_kind, id, ty, params, stmt_opt) -> (
      match stmt_opt with 
      | None -> sprintf "extern %s<%s> %s(%s);" 
        (show_func_kind func_kind)
        (show_ty ty)
        (show_id id)
        (show_params params)
      | Some(stmt) -> 
        let params = show_params params in
        let stmt = show_statement stmt |> indent 2 in
        let ty = show_ty ty in
        sprintf "%s<%s> %s(%s) {\n%s\n}" (show_func_kind func_kind) ty (show_id id) params stmt
    )
    | DTy(cid, ty_opt) -> (
      match ty_opt with 
      | None -> sprintf "extern type %s;" (show_cid cid)
      | Some(ty) -> sprintf "type %s = %s;" (show_cid cid) (show_ty ty)
    )
    | DEvent{evconstrid; evparams;} -> (
      let evconstrid = show_id evconstrid in
      let evparams = show_params evparams in
      sprintf "event %s(%s);" evconstrid evparams
    )

let show_decls decls  = List.map show_decl decls |> String.concat "\n"
