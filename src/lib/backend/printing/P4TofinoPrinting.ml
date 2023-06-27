(* pretty printers for minimal P4-tofino IR *)

open PPrint
open P4TofinoSyntax

exception Error of string
let error s = raise (Error s)


let (!^) s = string s
;;
let s' = string
;;

let concat_with_nobreak_space a b =
    a^^(!^" ")^^b
;;
let (^-^) a b = concat_with_nobreak_space a b



(* convert "a b c d" into a flow where each space can be a newline *)
let as_flow s = flow_map 
  (break 1) 
  (string)
  (String.split_on_char ' ' s)
;;

let nested_block_better bdy = group (braces ((nest 2 (break 1^^bdy))^^break 1)) 
;;


let doc_to_string d = 
  ToFormatter.pretty 0.8 25 Format.str_formatter d;
  Format.flush_str_formatter ()
;;

let verbose_ids = false
;;

let str_of_int i = s' (string_of_int i)
let str_of_bool b = s' (string_of_bool b)

let string_of_id v = if (verbose_ids) 
  then (!^ (Id.to_string v)) 
  else (!^ (Id.name v))
let string_of_cid v = if (verbose_ids) 
  then (!^ (Cid.to_string v)) 
  else (!^(String.concat "." @@ Cid.names v))
;;


let rec string_of_ty t = match t with 
    | TInt i -> (s'"bit<"^^(str_of_int i)^^s'">")
    | TBool -> s'"bool"
    | TStruct i -> string_of_id i
    | TVoid -> s'"void"
    | TList _ -> error "there's no list type syntax in P4?"
    | TAuto -> s'"_"
    | TUnknown -> error "trying to print an unknown type"
    | TObj(i) -> string_of_id i
    | TKey(KTernary, _) -> s'"ternary"
    | TKey(KExact, _) -> s'"exact"
    | TFun(rty, argtys) -> s'"/*"^^(string_of_tys argtys)^^s'" -> "^^(string_of_ty rty)^^s'"*/"

and string_of_tys ts = 
  separate_map (s'", ") string_of_ty ts

let string_of_value v = match v with 
  | VInt (i, None) -> (str_of_int i)
  | VInt (i, Some(w)) -> (str_of_int w)^^(s'"w")^^(str_of_int i)
  | VBool b -> str_of_bool b
  | VStruct _ -> error "vstruct printing not implemented"
  | VString _ -> error "strings are not expected in the p4 program"
;;

let rec string_of_expr expr = match expr.ex with 
  | EVal v -> string_of_value v
  | EVar cid -> string_of_cid cid
  | EOp (op, args) -> parens (string_of_eop op args)
  | ECall(fcn_expr, args_opt) -> 
    let args_str = match args_opt with 
      | Some args -> 
        (separate_map (s'",") string_of_expr args)
      | None -> s'""
    in 
    (string_of_expr fcn_expr)^^(parens args_str)
  | EConstr(id, tys, args) -> (
    (string_of_id id)
    ^^(s'"<")^^(string_of_tys tys)^^s'(">")
    ^^(parens (string_of_exprs args))
  )
  | EList(args) -> braces (separate_map (s'", ") string_of_expr args)

(* and string_of_fcn_call c = s'"" *)

and string_of_eop op args : PPrint.document =  
  match op, args with
    | Add, [a; b] -> (string_of_expr a)^^s'"+"^^(string_of_expr b)
    | Add, _ -> error "[string_of_eop] wrong args for op."
    | Sub, [a; b] -> (string_of_expr a)^^s'"-"^^(string_of_expr b)
    | Sub, _ -> error "[string_of_eop] wrong args for op."
    | SatSub, [a; b] -> (string_of_expr a)^^s'"|-|"^^(string_of_expr b)
    | SatSub, _ -> error "[string_of_eop] wrong args for op."
    | SatAdd, [a; b] -> (string_of_expr a)^^s'"|+|"^^(string_of_expr b)
    | SatAdd, _ -> error "[string_of_eop] wrong args for op."
    | RShift, [a; b] -> (string_of_expr a)^^s'">>"^^(string_of_expr b)
    | RShift, _ -> error "[string_of_eop] wrong args for op."
    | LShift, [a; b] -> (string_of_expr a)^^s'"<<"^^(string_of_expr b)
    | LShift, _ -> error "[string_of_eop] wrong args for op."
    | BAnd, [a; b] -> (string_of_expr a)^^s'"&"^^(string_of_expr b)
    | BAnd, _ -> error "[string_of_eop] wrong args for op."
    | BOr, [a; b] -> (string_of_expr a)^^s'"|"^^(string_of_expr b)
    | BOr, _ -> error "[string_of_eop] wrong args for op."
    | BXor, [a; b]-> (string_of_expr a)^^s'"^"^^(string_of_expr b)
    | BXor, _ -> error "[string_of_eop] wrong args for op."
    | Concat, [a; b] -> (string_of_expr a)^^s'"++"^^(string_of_expr b)
    | Concat, _ -> error "[string_of_eop] wrong args for op."
    | Eq, [a; b] -> (string_of_expr a)^^s'"=="^^(string_of_expr b)
    | Eq, _ -> error "[string_of_eop] wrong args for op."
    | Neq, [a; b] -> (string_of_expr a)^^s'"!="^^(string_of_expr b)
    | Neq, _ -> error "[string_of_eop] wrong args for op."
    | Lt, [a; b] -> (string_of_expr a)^^s'"<"^^(string_of_expr b)
    | Lt, _ -> error "[string_of_eop] wrong args for op."
    | Gt, [a; b] -> (string_of_expr a)^^s'">"^^(string_of_expr b)
    | Gt, _ -> error "[string_of_eop] wrong args for op."
    | Lte, [a; b] -> (string_of_expr a)^^s'"<="^^(string_of_expr b)
    | Lte, _ -> error "[string_of_eop] wrong args for op."
    | Gte, [a; b] -> (string_of_expr a)^^s'">="^^(string_of_expr b)
    | Gte, _ -> error "[string_of_eop] wrong args for op."
    | And, [a; b] -> (string_of_expr a)^^s'"&&"^^(string_of_expr b)
    | And, _ -> error "[string_of_eop] wrong args for op."
    | Or, [a; b] -> (string_of_expr a)^^s'"||"^^(string_of_expr b)
    | Or, _ -> error "[string_of_eop] wrong args for op."
    | Not, [a] -> s'"!"^^(string_of_expr a)
    | Not, _ -> error "[string_of_eop] wrong args for op."
    | Cast, [new_wid; var] -> 
      let wid_int = match new_wid with 
        (* we are casting to the width value, 
           _not_ the width of the width value *)
        | {ex=EVal(VInt(v, _)); _} -> v
        | _ -> error "first argument of cast op must be an int constant"
      in
     (!^"(")^^(string_of_ty (tint wid_int))^^(!^")")^^(string_of_expr var)
    | Cast, _ -> error "[string_of_eop] wrong args for op."
    | Slice, [estart; efin; evar] -> 
            (string_of_expr evar)
            ^^ (!^"[")^^(string_of_expr estart)^^(!^":")^^(string_of_expr efin)^^(!^"]")    
    | Slice, _ -> error "[string_of_eop] wrong args for op."


and string_of_exprs es = 
  separate_map (s'", ") string_of_expr es
;;

let string_of_pragma pr =
    s'("@"^pr.pname^"("^(String.concat "," pr.pargs)^")")
;;
let string_of_pragmas prs =
  separate_map (hardline) string_of_pragma prs
;;
(* generic statement printer -- some statements need to be printed differently depending 
   on which kind of function they appear in. This only prints statements that have 
   the same format in any function. *)
let rec string_of_statement st =
  match st with 
  | Seq(st1, st2) -> (string_of_statement st1)^/^(string_of_statement st2)
  | Assign(cid, expr) -> (string_of_cid cid)^^s'"="^^(string_of_expr expr)^^s'";"
  | Local(id, ty, expr) -> (string_of_ty ty)^-^(string_of_id id)^^s'"="^^(string_of_expr expr)^^s'";"
  | Unit(expr) -> (string_of_expr expr)^^s'";"
  | Prag(p) -> (string_of_pragma p)
  | If(e, s1, None) -> s'"if("^^(string_of_expr e)^^s'")"^^(nested_block_better (string_of_statement s1))
  | If(e, s1, Some s2) -> 
    s'"if "^^(parens (string_of_expr e))
      ^^(nested_block_better (string_of_statement s1))
      ^^s'" else "^^(nested_block_better (string_of_statement s2))
(*     s'"if("^^(string_of_expr e)^^s'")"
    ^^(nest 2 (braces (string_of_statement s1)))^/^
    s'"else"
    ^^(nest 2 (braces (string_of_statement s2))) *)
  | Match(_) -> error "[string_of_statement] cannot print match statements generically -- match statement printing is function-dependent"
  | Noop -> s'"//NOOP"
;;

(* inside of a parse state, matches are printed differently *)
let string_of_parse_branch (ps, stmt) = 
  let fcn_cid = match stmt with 
    | Unit({ex=ECall({ex=EVar(fcn_id)}, _)}) -> fcn_id
    | _ -> error "[string_of_parse_branch] statement has invalid form"
  in 

  let string_of_pat pat = match pat with 
    | PNum i -> (str_of_int i)
    | PWild -> s'"_"
    | _ -> error "[string_of_parse_branch] printing of bitpat/masked parse branches not implemented"
  in
  let pat_str = parens (separate_map (s'", ") string_of_pat ps) in 
  pat_str^^s'" : "^^string_of_cid fcn_cid^^s'";"
;;

let rec string_of_parse_statement st = match st with 
  | Match([expr], branches) -> 
    s'"transition select"^^parens (string_of_expr expr)
    ^^nested_block_better (separate_map hardline string_of_parse_branch branches)
  | Match(exprs, branches) -> 
    s'"transition select"^^parens (string_of_exprs exprs)
    ^^nested_block_better (separate_map hardline string_of_parse_branch branches)
  | Unit({ex=ECall({ex=EVar(fcn_id)}, None)}) ->
    s'"transition"^-^(string_of_cid fcn_id)^^s'";"    
  | Seq(s1, s2) -> 
    (string_of_parse_statement s1)
    ^/^(string_of_parse_statement s2)
  | _ -> string_of_statement st


let string_of_dir d = match d with 
    | Pin -> string "in"
    | Pout -> string "out"
    | Pinout -> string "inout"
;;

let string_of_param p :PPrint.document = 
    let (dopt, t, i) = p in
    match dopt with 
    | None -> (string_of_ty t  ^-^ string_of_id i)
    | Some d -> (string_of_dir d ^-^ string_of_ty t ^-^ string_of_id i)
;;
let string_of_params ps =
    (* want: all params on 1 line, or 1 param per line with indent of 4 *)
    group (
      nest 4 (
        separate_map
        ((!^",")^/^(!^""))
        string_of_param
        ps
      )
    )
  ;;

let string_of_field (id, ty) = 
    (string_of_ty ty)^-^(string_of_id id)^^s'";"
    (* (string_of_id id)^^s'" : "^^(string_of_ty ty)^^s'";" *)
;;

let string_of_key (k:expr) = 
  (string_of_expr k)^^s'" : "^^(string_of_ty k.ety)^^s'";"

(*   match k with
  | Ternary f -> (string_of_cid f)^^s'" : ternary;"
  | Exact   f -> (string_of_cid f)^^s'" : exact;"
 *)
let string_of_pats ps =
  let string_of_pat p = match p with 
  | PWild -> s'"_"
  | PNum i -> str_of_int i
  | PBitstring bs -> 
    let (i, m) = bits_to_maskedint bs in 
    (str_of_int i)^^s'" &&& "^^(str_of_int m)
  in 
  parens (separate_map (s'",") string_of_pat ps)
;;

(* string of a branch that represents 
   a const entry in a table *)
let string_of_rule (ps, stmt) =
  let call_exp = match stmt with 
    | Unit(e) -> e
    | _ -> error "[string_of_rule] in a rule, the branch statement must be a unit call to an action function."
  in 
  (string_of_pats ps)^^s'" : "^^(string_of_expr call_exp)^^s'";"
;;

let string_of_mem_fcn (params, stmt) = 
    s'"void apply"^^parens (string_of_params params)
    ^^nested_block_better (string_of_statement stmt)
    ^^s'""
;;

let string_of_sty sty = 
  match sty with
  | THdr -> s'"header"
  | TMeta -> s'"struct"
;;

let rec string_of_decl dec = 
  match dec.d with
  | DReg{id=id; slot_ty=slot_ty; idxty=idxty; len=len; def=def;} -> 
        let argstr = match def with 
          | None -> (parens (string_of_exprs [len]))
          | Some edef -> (parens (string_of_exprs [len; edef]))
        in
        s'"Register"
          ^^(angles (separate_map (s'",") string_of_ty [slot_ty; idxty]))
          ^^argstr
          ^-^(string_of_id id)^^s'";"
  | DVar(id, ty, Some(expr)) -> 
    (string_of_ty ty)^-^(string_of_id id)^^s'"="^^(string_of_expr expr)^^s'";"
  | DVar(id, ty, None) -> 
    (string_of_ty ty)^-^(string_of_id id)^^s'";"
(*   | DInit(cid, expr) -> 
    (string_of_cid cid)^^s'"="^^(string_of_expr expr)^^s'";" *)
  | DTable{id=id; keys=keys; actions=actions; rules=rules; default=default;} -> 
    (string_of_pragmas dec.dpragma)^^
    s'"table "^^(string_of_id id)
    ^-^nested_block_better (
      s'"key = " ^^nested_block_better (
          separate_map hardline string_of_key keys 
        )
      ^^hardline
      ^^s'"actions = "^^nested_block_better (
          separate_map hardline (fun eacn -> string_of_expr eacn^^s'";") actions
        )
      ^^hardline
      (* printing an empty const entries messes up the compiler... *)
      ^^(match List.length rules with 
        | 0 -> s'""
        | _ -> 
          s'"const entries = "^^nested_block_better ( 
            separate_map hardline
              string_of_rule
              rules
          )
        )
      ^^(match default with 
        | None -> s'" "
        | Some default -> 
          hardline
          ^^s'"const default_action = "^^(string_of_statement default))
      )^^s'" "
  | DAction{id=id;params=params;body=body;} -> 
    s'"action "^^(string_of_id id)
    ^^parens (string_of_params params)
    ^^nested_block_better (
      string_of_statement body
    )
  | DHash{id=id; poly=poly; out_wid=out_wid;} ->
    let crc_name = (string_of_id id)^^string "_crc" in 
    let hasher_name = (string_of_id id) in 
    let crc = string "CRCPolynomial<bit<"^^(str_of_int out_wid)^^string ">>("
      ^^(str_of_int poly)^^string ",false, false, false, 0, 0) "^^crc_name^^string ";"
    in  
    let hasher = string "Hash<bit<"^^(str_of_int out_wid)
      ^^string ">>(HashAlgorithm_t.CUSTOM,"^^crc_name^^string ") "^^hasher_name^^string ";"
    in 
    crc^^hardline^^hasher
  | DRegAction{id=id;reg=reg;idx_ty=idx_ty;mem_fcn=mem_fcn;} -> (
    let param_tys = List.map (fun (_, ty, _) -> ty) (fst mem_fcn) in
    (* add the index param *)
    let param_tys = match param_tys with 
      | [p1; p2] -> [p1; idx_ty; p2]
      | [p1] -> [p1; idx_ty]
      | _ -> error "[string_of_decl.RegisterAction] unexpected number of parameters"
    in
    let regacn =  s'"RegisterAction"^^
      (angles (separate_map (s'",") string_of_ty param_tys))
      ^^(parens (string_of_id reg))^/^(string_of_id id)^^s'" = "
      ^^(nested_block_better (string_of_mem_fcn mem_fcn))
      ^^s'";"
    in 
    regacn
    )
    | DParseState{id=id; body=body;} -> 
      s'"state"^-^(string_of_id id)^-^group 
      (nested_block_better (string_of_parse_statement body))
    | DInclude(fn) -> s'"#include "^^(s' fn)
    | DStructTy{id=id; sty=sty; fields=fields;} -> 
        (string_of_sty sty)^-^(string_of_id id)^-^nested_block_better
          (separate_map (break 1) string_of_field fields)

(*         group (nested_block
          (
            (break 1)
            ^^(separate_map (break 1) string_of_field fields)
            ^^s'" "
          )
        ) *)
    | DConst(id, ty, expr) -> 
      s'"const "^^(string_of_ty ty)^-^(string_of_id id)^^s'"="^^(string_of_expr expr)^^s'";"
    | DControl{id=id; params=params; decls=decls; body=body_opt;}
    | DDeparse {id=id; params=params; decls=decls; body=body_opt;} -> 
      let body = match body_opt with
        | Some body -> body
        | None -> error "[string_of_decl] a control or parse block must have a body, even if it is empty"
      in 
      (* let bodystr = s'"apply" ^^ nested_block (string_of_statement body) in  *)
      s'"control"^-^(string_of_id id)
      ^^parens (string_of_params params)
      ^^ (nested_block_better (
          (string_of_decls decls)
          ^/^s'"apply "^^(nested_block_better (string_of_statement body)))^^s'" "
        )      
    | DParse{id=id; params=params; decls=decls;} -> 
      s'"parser"^-^(string_of_id id)
      ^^parens (string_of_params params)
      ^^ (nested_block_better (string_of_decls decls)) 
    | DObj(econstr, id) -> 
      (string_of_expr econstr)^-^(string_of_id id)^^s'";"
    | DMCGroup(_) -> s'"//multicast group"
    | DPort(_) -> s'"//port-up definition"
    | DPragma(p) -> string_of_pragma p
    | DNone -> s'""

and string_of_decls ds =
  separate_map hardline string_of_decl ds
;;

(* 
Pipeline(
    IngressParser(), Ingress(), IngressDeparser(),
    EgressParser(), Egress(), EgressDeparser()) pipe;

Switch(pipe) main;
*)
let mainstring_of_tofino_prog (prog:tofino_prog) = 
  s'"Pipeline"^^(group (nest 2 (parens (separate_map (s'","^^(break 1))
    (fun decl -> match decl.d with
      | DControl(o) | DParse(o) | DDeparse(o) -> 
        string_of_id (o.id) ^^ s'"()"
      | _ -> error "[mainstring_of_tofino_prog] one of the pipeline decls is not a control, parse, or deparse"
    )
    (blocks_of_prog prog)
  ))))
  ^^s'" pipe;"
  ^^hardline
  ^^(s'"Switch(pipe) main; ")
;;
let p4_of_prog (prog:tofino_prog) =
   s'"//Global declarations"^/^s'"//(includes, headers, structs, constants, register arrays)"
  ^/^(separate_map hardline string_of_decl prog.globals)
  ^/^s'"//Main program components (ingress/egress parser, control, deparser)"
  ^/^(separate_map hardline string_of_decl (blocks_of_prog prog))
 ^^hardline
  ^^s'"//Pipeline and main declarations"
  ^^hardline
  ^^(mainstring_of_tofino_prog prog)
  |> doc_to_string


(* ;; *)


(*   match f with 
  (* a P4 control block *)
  | DControl(f)->
    s'"control"^-^(string_of_id f.id)
    ^^s'"("^^(string_of_params f.params)^^s'")"
    ^^nested_block (
      (string_of_decls f.decls)^^hardline 
      ^^s'"apply" ^^ nested_block (string_of_statement f.body)                    
    )
  | DAction f -> 
    s'"action"^-^(string_of_id f.id)
    ^^s'"("^^(string_of_params f.params)^^s'")"
    ^^nested_block (string_of_statement f.body) 
  | DTable _ -> error "todo: table printer"
  | DHash _ -> error "todo: hash printer"
  | DParser _ -> error "todo: parser printer"
  | DParseState _-> error "todo: FParseState"
  | DRegAction _ -> error "todo: FRegAction"



 *)

