open CCoreSyntax
let sprintf = Printf.sprintf

(* split on newlines, indent each newline by n spaces, combine back into string *)
let indent n str = str |> String.split_on_char '\n' |> List.map (fun s -> String.make n ' ' ^ s) |> String.concat "\n"

let comment str = "/* "^str^"*/"

let id_to_string id = 
  fst id
  (* Id.to_string id *)
let cid_to_string (cid : Cid.t) = 
  String.concat "_" (List.map id_to_string (Cid.to_ids cid))
  (* Cid.to_string cid *)

let size_to_string size = string_of_int size
let arrlen_to_string = function 
  | IConst(i) -> string_of_int i
  | IVar(v) -> cid_to_string v

let func_kind_to_string = function
  | FNormal -> "fn"
  | FHandler -> "handler"
  | FParser -> "parser"
  | FAction -> "action"
  | FMemop -> "memop"
  | FForiegn -> "fn"
;;


(* use abstract names in parameter and function argument types. Possibly elsewhere. *)
let rec raw_ty_to_string ?(use_abstract_name=false) (r: raw_ty) : string =
  match r with
  | TUnit -> "void"
  | TInt 32 -> "int"
  | TInt size -> "int" ^ size_to_string size
  | TBool -> "bool"
  | TUnion(labels, ts) -> 
    let label_strs = List.map (cid_to_string) labels in
    let ts_strs = List.map (ty_to_string ~use_abstract_name:true) ts in
    let field_strs = List.map2 (fun l e -> l ^ ": " ^ e ^";") label_strs ts_strs in
    let fields_str = String.concat " " field_strs in  
    sprintf "union {%s}" fields_str 
    (* "{" ^ fields_str ^ "}" *)
  | TRecord(labels, ts) -> 
    let label_strs = List.map (cid_to_string) labels in
    let ts_strs = List.map (ty_to_string ~use_abstract_name:true) ts in
    let field_strs = List.map2 (fun l e -> l ^ ": " ^ e ^";") label_strs ts_strs in
    let fields_str = String.concat " " field_strs in    
    "{" ^ fields_str ^ "}"
  | TTuple ts -> 
    let label_strs = List.mapi (fun i _ -> "_" ^ string_of_int i) ts in
    let ts_strs = List.map (ty_to_string ~use_abstract_name:true) ts in
    let field_strs = List.map2 (fun l e -> l ^ ": " ^ e ^";") label_strs ts_strs in
    let fields_str = String.concat " " field_strs in    
    "{" ^ fields_str ^ "}"
  | TFun func_ty -> "function(" ^ func_ty_to_string func_ty ^ ")"
  | TBits {ternary; len} -> (if ternary then "ternary_" else "") ^ "bit[" ^ size_to_string len ^ "]"
  | TEvent -> "event"
  | TEnum list -> 
    let list_str = String.concat ", " (List.map (fun (s, i) -> cid_to_string s ^ " = " ^ string_of_int i) list) in
    "enum {" ^ list_str ^ "}"
  | TBuiltin (cid, ty_list) -> 
    let ty_list_str = String.concat ", " (List.map (ty_to_string ~use_abstract_name) ty_list) in
    cid_to_string cid ^ "<<" ^ ty_list_str ^ ">>"
  | TName cid -> cid_to_string cid
  | TAbstract (cid, ty) -> 
    if (use_abstract_name) then 
      cid_to_string cid
    else ty_to_string ty
  | TPtr(ty, None) -> sprintf "%s*" (ty_to_string ~use_abstract_name ty)
  | TPtr(ty, Some(arrlen)) -> 
    ty_to_string ~use_abstract_name:true ty ^ "[" ^ arrlen_to_string arrlen ^ "]"

and ty_to_string ?(use_abstract_name=false) ty = raw_ty_to_string ~use_abstract_name ty.raw_ty

and func_ty_to_string (f: func_ty) : string =
  let arg_tys_str = String.concat ", " (List.map (ty_to_string ~use_abstract_name:true) f.arg_tys) in
  let ret_ty_str = ty_to_string ~use_abstract_name:true f.ret_ty in
  "(" ^ arg_tys_str ^ ") -> " ^ ret_ty_str

let params_to_string params = 
  let params_str = String.concat ", " (List.map (fun (id, ty) -> ty_to_string ~use_abstract_name:true ty ^" "^ cid_to_string id ) params) in
  params_str
;;

let rec v_to_string (v: v) : string =
  match v with
  | VUnit -> "void"
  | VInt {value; _} -> string_of_int value
  | VBool b -> string_of_bool b
  | VRecord(labels, es) -> 
    let label_strs = List.map cid_to_string labels in
    let es_strs = List.map value_to_string es in
    let field_strs = List.map2 (fun l e -> "." ^l ^ " = " ^ e ^";") label_strs es_strs in
    let fields_str = String.concat " " field_strs in    
    "{" ^ fields_str ^ "}"
  | VUnion(label, v, _) -> 
    sprintf "{%s = %s}" (cid_to_string label) (value_to_string v)
  | VTuple(es) -> 
    let label_strs = List.mapi (fun i _ -> "_" ^ string_of_int i) es in
    let es_strs = List.map value_to_string es in
    let field_strs = List.map2 (fun l e -> "." ^l ^ " = " ^ e ^";") label_strs es_strs in
    let fields_str = String.concat " " field_strs in    
    "{" ^ fields_str ^ "}"
  | VList vs -> "{" ^ String.concat ", " (List.map value_to_string vs) ^ "}"
  | VBits {ternary; bits} -> 
    let bits_str = String.concat "" (List.map (fun i -> if i = -1 then "*" else string_of_int i) bits) in
    (if ternary then "ternary " else "") ^ "bits[" ^ bits_str ^ "]"
  | VEvent e -> "event(" ^ vevent_to_string e ^ ")"
  | VSymbol (s, _) -> cid_to_string s

and vevent_to_string (e: vevent) : string =
  sprintf "%s(%s)" (cid_to_string e.evid) (String.concat ", " (List.map value_to_string e.evdata))
and value_to_string value = 
  if is_tstring value.vty then "\""^charints_to_string value^"\""
  else v_to_string value.v

let rec e_to_string (e: e) : string =
  match e with
  | EVal v -> value_to_string v
  | EVar cid -> cid_to_string cid
  | EAddr(cid) -> sprintf "(&%s)" (cid_to_string cid)
  | ETuple es -> 
    let es_strs = List.map exp_to_string es in
    let field_strs = List.mapi (fun i e -> "." ^ "_" ^ string_of_int i ^ " = " ^ e ^";") es_strs in
    let fields_str = String.concat " " field_strs in    
    "{" ^ fields_str ^ "}"
  | ERecord(labels, es) -> 
    let label_strs = List.map cid_to_string labels in
    let es_strs = List.map exp_to_string es in
    let field_strs = List.map2 (fun l e -> "." ^l ^ " = " ^ e ^";") label_strs es_strs in
    let fields_str = String.concat " " field_strs in    
    "{" ^ fields_str ^ "}"
  | EUnion(label, exp, _) -> 
    sprintf "{.%s = %s}" (cid_to_string label) (exp_to_string exp)
  | ECall {f; args; call_kind=CEvent} -> 
    let f_str = exp_to_string f in
    let args_str = String.concat ", " (List.map exp_to_string args) in
    f_str ^ "(" ^ args_str ^ ")"
    | ECall {f; args; _} -> 
    let f_str = exp_to_string f in
    let args_str = String.concat ", " (List.map exp_to_string args) in
    let comment_str = match (extract_func_ty f.ety) with 
      (* | _, _, FForiegn -> " /* forien */" *)
      | _ -> "" 
    in
    f_str ^ "(" ^ args_str ^ ")" ^ comment_str
  | EOp (op, args) -> op_to_string op args
  (* special case: print deref of pointer arith as a subscript *)
  | EDeref({e=EOp(Plus, [arr_exp; idx_exp])}) -> 
    sprintf "%s[%s]" (exp_to_string arr_exp) (exp_to_string idx_exp)
  | EDeref(exp) -> sprintf "(*(%s))" (exp_to_string exp)
and exp_to_string exp : string = e_to_string exp.e
and exps_to_string exps = String.concat ", " (List.map exp_to_string exps)
and op_to_string (op: op) (args: exp list) : string =
  match op, args with 
  | And, [a; b] when is_eop a || is_eop b -> 
    sprintf "(%s) && (%s)" (exp_to_string a) (exp_to_string b)
  | And, [a; b] -> exp_to_string a ^ " && " ^ exp_to_string b
  | Or, [a; b] -> exp_to_string a ^ " || " ^ exp_to_string b
  | Not, [a] -> "!" ^ exp_to_string a
  | Eq, [a; b] when is_eop a || is_eop b -> 
    sprintf "(%s) == (%s)" (exp_to_string a) (exp_to_string b)
  | Eq, [a; b] -> exp_to_string a ^ " == " ^ exp_to_string b
  | Neq, [a; b] -> exp_to_string a ^ " != " ^ exp_to_string b
  | Less, [a; b] -> exp_to_string a ^ " < " ^ exp_to_string b
  | More, [a; b] -> exp_to_string a ^ " > " ^ exp_to_string b
  | Leq, [a; b] -> exp_to_string a ^ " <= " ^ exp_to_string b
  | Geq, [a; b] -> exp_to_string a ^ " >= " ^ exp_to_string b
  | Neg, [a] -> "-" ^ exp_to_string a
  | Plus, [a; b] -> exp_to_string a ^ " + " ^ exp_to_string b
  | Sub, [a; b] -> exp_to_string a ^ " - " ^ exp_to_string b
  | SatPlus, [a; b] -> exp_to_string a ^ " |+| " ^ exp_to_string b
  | SatSub, [a; b] -> exp_to_string a ^ " |-| " ^ exp_to_string b
  | BitAnd, [a; b] -> exp_to_string a ^ " & " ^ exp_to_string b
  | BitOr, [a; b] -> exp_to_string a ^ " | " ^ exp_to_string b
  | BitXor, [a; b] -> exp_to_string a ^ " ^ " ^ exp_to_string b
  | BitNot, [a] -> "~" ^ exp_to_string a
  | LShift, [a; b] -> exp_to_string a ^ " << " ^ exp_to_string b
  | RShift, [a; b] -> exp_to_string a ^ " >> " ^ exp_to_string b
  | Slice (i, j), [a] -> exp_to_string a ^ "[" ^ string_of_int i ^ ":" ^ string_of_int j ^ "]"
  | PatExact, [a] -> "PatExact(" ^ exp_to_string a ^ ")"
  | PatMask, [a] -> "PatMask(" ^ exp_to_string a ^ ")"
  | Hash 32, [seed; a] -> 
    let ref_arg = sprintf "(%s)&%s" (ty_to_string (tref (tint 8))) (exp_to_string a) in
    let seed_arg = sprintf "(%s)%s" (ty_to_string (tint 32)) (exp_to_string seed) in
    (* TODO: polymorphic hashes *)
    sprintf "hash_%i(%s, %s, sizeof(%s))" 32 seed_arg ref_arg (exp_to_string a)
  | Hash n, [seed; a] -> 
    let ref_arg = sprintf "(%s)&%s" (ty_to_string (tref (tint 8))) (exp_to_string a) in
    let seed_arg = sprintf "(%s)%s" (ty_to_string (tint 32)) (exp_to_string seed) in
    sprintf "(%s)hash_32(%s, %s, sizeof(%s))" (ty_to_string@@tint n) seed_arg ref_arg (exp_to_string a)
    (* "Hash_" ^ size_to_string size ^ "(" ^exps_to_string args ^ ")" *)
  | Cast new_ty, [a] ->
    let int_ty_str = ty_to_string ~use_abstract_name:true (new_ty) in
    "((" ^ int_ty_str ^ ")(" ^ exp_to_string a ^"))"
  | Conc, args -> String.concat "++" ((List.map exp_to_string args))
  (* use arrow notation shorthand for derefs, unless its a subscript op *)
  | Project id, [a] when (is_ederef (a) && (not@@is_eop (extract_ederef (a)))) -> 
      exp_to_string a ^ "->" ^ cid_to_string id
  | Project id, [a] -> exp_to_string a ^ "." ^ cid_to_string id
  | Get i, [a] -> exp_to_string a ^ "._" ^ string_of_int i
  | Mod, [x; m] -> Printf.sprintf "(%s mod %s)" (exp_to_string x) (exp_to_string m)
  | _, _ -> failwith ("Invalid number of arguments for operator: "^(show_op op))


let assign_op_to_string (op: assign_op) = 
  match op with
  | OLocal (cid, ty) -> 
    ty_to_string ~use_abstract_name:true ty ^ " " ^ cid_to_string cid
  | OTupleLocal (cids, tys) -> 
    let cids_str = ""^String.concat ", " (List.map cid_to_string cids) ^"" in
    let tys_str = "("^String.concat ", " (List.map (ty_to_string ~use_abstract_name:true) tys) ^")" in
    tys_str ^ " " ^ cids_str
  | OTupleAssign exps -> String.concat ", " (List.map exp_to_string exps)
  | OAssign(exp) -> exp_to_string exp
;;


let rec s_to_string (s: s) : string =
  match s with
  | SNoop -> ""
  | SUnit e ->  exp_to_string e ^ ";"
  | SAssign(op, exp) -> assign_op_to_string op ^ " = " ^ exp_to_string exp ^ ";"
  | SIf (e, s1, s2) -> 
    "if (" ^ exp_to_string e ^ ") {\n" ^ 
    indent 2 (statement_to_string s1) ^ "\n}"
    ^(if (statement_to_string s2 == "") then 
      "" else 
      "else {\n" ^ 
      indent 2 (statement_to_string s2) ^ "\n}")
  | SMatch (es, branches) -> 
    "match (" ^ (List.map exp_to_string es |> String.concat " , ") ^ ") {\n" 
    ^ indent 2 (String.concat "\n" (List.map branch_to_string branches)) 
    ^ "\n}"
  | SSeq (s1, s2) -> statement_to_string s1 ^"\n" ^ statement_to_string s2
  | SRet e_opt -> 
    "return " ^ (match e_opt with 
                  | Some e -> exp_to_string e 
                  | None -> "") ^ ";"  
  | SFor{idx; bound; stmt; guard=None} -> 
    "for (" ^ (cid_to_string idx) ^ " < " ^ arrlen_to_string bound ^ ") {\n" ^ 
    indent 2 (statement_to_string stmt) ^ "\n}"
  | SFor{idx; bound; stmt; guard=Some(guard)} -> 
    "for (" ^ (cid_to_string idx) ^ " < " ^ arrlen_to_string bound ^ ") while ("^(cid_to_string guard)^" == true) {\n" ^ 
    indent 2 (statement_to_string stmt) ^ "\n}"
  | SForEver(stmt) -> 
    "forever {\n" ^ indent 2 (statement_to_string stmt) ^ "\n}"

and pat_to_string (p: pat) : string =
  match p with
  | PVal v -> value_to_string v
  | PEvent {event_id; params} -> 
    let params_str = params_to_string params in
    (cid_to_string event_id) ^ "(" ^ params_str ^ ")"
  | PWild _ -> "_"

and branch_to_string (b: branch) : string =
  let (pats, s) = b in
  let pats_str = String.concat " | " (List.map pat_to_string pats) in
  "case " ^ pats_str ^ ": {\n" ^ indent 2 (statement_to_string s) ^ "\n}"

and statement_to_string statement = s_to_string statement.s


let rec d_to_string (d: d) : string =
  match d with
  | DVar (id, ty, exp_opt) -> 
    let id_str = cid_to_string id in
    let ty_str = ty_to_string ~use_abstract_name:true ty in
    let exp_str = match exp_opt with 
                  | Some exp -> " = " ^ exp_to_string exp 
                  | None -> " extern" in
    if exp_opt = None then 
      "extern " ^ ty_str ^ " " ^ id_str ^ ";"
    else
      ty_str ^ " " ^ id_str ^ exp_str ^ ";"
  | DFun fun_def -> fun_def_to_string fun_def
  | DTy (cid, ty_opt) -> 
    sprintf "type %s = %s;"
      (cid_to_string cid)
      (match ty_opt with 
                 | Some ty -> ty_to_string ty 
                 | None -> " extern")
  | DEvent event_def -> 
    let id_str = cid_to_string event_def.evconstrid in
    let params_str = params_to_string event_def.evparams in
    "event " ^ id_str ^ "(" ^ params_str ^ ");"
  | DForiegn str -> str

and fun_def_to_string (kind, id, ty, params, stmt_opt) = 
  let kind_str = func_kind_to_string kind in
  let id_str = cid_to_string id in
  let ret_ty_str = match kind with 
    | FHandler -> ""
    | _ -> ty_to_string ~use_abstract_name:true ty 
  in
  let params_str = params_to_string params in
  let stmt_str = match stmt_opt with 
                 | BStatement stmt -> "{\n" ^ indent 2 (statement_to_string stmt) ^ "\n}" 
                 | BExtern -> ";" 
                 | BForiegn s -> s  
  in
  match stmt_opt with 
    | BExtern -> 
      "extern " ^ kind_str ^ " " ^ ret_ty_str ^ " " ^id_str ^ "(" ^ params_str ^ ")" ^ stmt_str
    | _ -> kind_str ^ " "  ^ ret_ty_str ^ " " ^ id_str ^ "(" ^ params_str ^ ")" ^ stmt_str
  
and decl_to_string decl = d_to_string decl.d


and decls_to_string decls = String.concat "\n" (List.map decl_to_string decls)