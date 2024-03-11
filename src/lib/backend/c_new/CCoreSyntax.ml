(* simpler c-like IR for lucid, with extensions
   for compatability with the current CoreSyntax IR
   (Extensions should be eliminated before any further processing) *)

type id = [%import: (Id.t[@opaque])]
and cid = [%import: (Cid.t[@opqaue])]
and tagval = [%import: (TaggedCid.tagval[@opqaue])]
and tcid = [%import: (TaggedCid.t[@opqaue])]
and sp = [%import: Span.t]

(* types *)
and size = int

(* array indices -- separate from sizes so we can figure out 
   exactly how they need to work *)
and arridx = 
    | IConst of int
    | IVar of id

and func_kind = | FNormal | FHandler | FParser | FAction | FMemop | FExtern
and raw_ty = 
  (* value types *)
  | TUnit
  | TInt of size 
  | TBool 
  | TRecord of id list * ty list
  | TTuple  of ty list 
  (* | TRecord of {labels : id list option; ts : ty list;} *)
  | TList of ty * arridx
  | TBits of {ternary: bool; len : size;}
  | TEvent
  | TEnum of (id * int) list 
  (* function type *)
  | TFun of func_ty
  (* alias types *)
  | TBuiltin of cid * (ty list) (* types built into the lucid language that must be eliminated for c *)
  | TName of cid (* tydef, basically *)
  | TAbstract of cid * ty (* just a wrapper around some other type. For convenience *)

and func_ty = {
  arg_tys : ty list; 
  ret_ty : ty; 
  func_kind : func_kind;
}
and ty = {raw_ty:raw_ty; tspan : sp;}
and params = (id * ty) list
(* values *)
and v =
  | VUnit
  | VInt of {value : int; size : size;}
  | VBool of bool
  | VRecord of id list * value list
  | VTuple of value list
  (* | VRecord of {labels : id list option; es : value list;} *)
  | VList  of value list
  | VBits of {ternary: bool; bits : int list;}
  | VEvent of vevent
  | VSymbol of id * ty 
    (* an id that is a  primitive value. 
       used to represent enum values, 
       globals in the interpreter pipeline, 
       constants, partially-interpreted values with 
       unbound variables, etc *)
and vevent = {evid : cid; evnum : value option; evdata: value list; meta : (string * value) list;}
and value = {v:v; vty:ty; vspan : sp;}

(* expressions *)
and op =    | And | Or | Not
            | Eq  | Neq | Less| More | Leq | Geq
            | Neg | Plus| Sub | SatPlus | SatSub
            | BitAnd  | BitOr | BitXor | BitNot | LShift | RShift
            | Slice of int * int
            | PatExact | PatMask
            | Hash of size
            | Cast of size 
            | Conc
            | Project of id | Get of int 
            
            (* record and tuple ops *)
and e = 
  | EVal of value
  | EVar of cid 
  | ETuple of exp list
  | ERecord of id list * exp list
  (* | ERecord of {labels : id list option; es : exp list;} *)
  | ECall of {f:exp; args:exp list; call_kind:call_kind;}
  | EOp of op * exp list
  | EListGet of exp * arridx (* Why not make this an Op? *)

and call_kind = 
  | CFun
  | CEvent 
and exp = {e:e; ety:ty; espan : sp;}

and pat = 
  | PVal of value
  | PEvent of {event_id : cid; params : params;}
and branch = pat list * statement
(* statements *)

and assign_op = 
  | OLocal  of cid * ty (* create a new variable *)
  | OAssign of cid      (* assign to variable *)
  | OTupleLocal of cid list * ty list (* create new variables, unpack tuple to them *)
  | OTupleAssign of cid list (* unpack tuple to variables *)
  | OListSet of {arr : exp; idx : arridx} (* set index in array *)
  | ORecordSet of {rec_exp : exp; field : id} (* set field in record *)

and s = 
  | SNoop
  | SUnit of exp
  | SAssign of assign_op * exp
  | SFor of {idx : id; bound : arridx; stmt: statement; guard : id option}
    (* for (idx < bound) while guard *)
  | SForEver of statement (* infinite loop *)
  | SIf of exp * statement * statement
  | SMatch of exp * branch list
  | SSeq of statement * statement
  | SRet of exp option

and statement = {s:s; sspan : sp;}

(* declarations *)
and event_def = {evconstrid : id; evconstrnum : int option; evparams : params; is_parsed : bool}
and d = 
  | DVar of id * ty * exp option (* constants and globals, possibly externs *)
  | DList of id * ty * (exp list) option (* mutable list *)
  | DFun of func_kind * id * ty * params * (statement option) (* functions and externs *)
  | DTy  of cid * ty option (* named types and external types *)
  | DEvent of event_def (* declare an event, which is a constructor for the datatype TEvent *)
  and decl = {d:d; dspan : sp;}
and decls = decl list
[@@deriving
  visitors
    { name = "s_iter"
    ; variety = "iter"
    ; polymorphic = false
    ; data = true
    ; concrete = true
    ; nude = false
    }, 
  visitors
    { name = "s_map"
    ; variety = "map"
    ; polymorphic = false
    ; data = true
    ; concrete = true
    ; nude = false
    },
  show]


  exception FormError of string

(* constructors and destructors *)

let arridx_ct = ref (-1);;
let fresh_arridx str = 
  arridx_ct := (!arridx_ct + 1);
  IVar (Id.fresh str)
;;
let arridx i = IConst i
let idxvar id = IVar id
let sz n = n

(**** types ****)
let ty raw_ty = {raw_ty=raw_ty; tspan=Span.default; }
let tunit () = ty TUnit
let tbool = ty TBool
let tint i = ty@@TInt(sz i)
let tpat len = ty (TBits {ternary=false; len})
let tevent = ty TEvent
let trecord labels tys = ty (TRecord(labels, tys))
let ttuple tys = ty (TTuple tys)
  
let tfun_kind func_kind arg_tys ret_ty = ty (TFun {arg_tys; ret_ty; func_kind})
let tfun arg_tys ret_ty = tfun_kind FNormal arg_tys ret_ty 
(* global type from CoreSyntax *)
let tlist ele_ty len = ty (TList(ele_ty, len))
let tname cid = ty (TName(cid))
let tbuiltin cid tyargs = ty (TBuiltin(cid, tyargs))
let tgroup_cid = Cid.create ["Group"]
let tgroup = tname tgroup_cid
let tabstract tname inner_ty = ty (TAbstract(Cid.create [tname], inner_ty))
let tabstract_cid tcid inner_ty = ty (TAbstract(tcid, inner_ty))
let tabstract_id id inner_ty = ty (TAbstract(Cid.create_ids [id], inner_ty))
let textern = tname (Cid.create ["_extern_ty_"])
let tenum_pairs (tagpairs : (Id.t * int) list) = ty (TEnum tagpairs)
let tenum ids = tenum_pairs (List.mapi (fun i id -> (id, i)) ids)


let rec base_type ty = 
  match ty.raw_ty with 
  | TAbstract(_, ty) -> base_type ty
  | _ -> ty
;;

(* derive the alias type used to print c *)
let rec alias_type ty = 
  match ty.raw_ty with 
  | TAbstract(cid, _) -> tname cid
  | TName(_)-> ty
  | _ -> ty
;;

let is_textern ty = match ty.raw_ty with TName cid -> Cid.equal cid (Cid.create ["_extern_ty_"]) | _ -> false
let is_tunit ty = match ty.raw_ty with TUnit -> true | _ -> false
let is_trecord ty = match (base_type ty).raw_ty with TRecord _ -> true | _ -> false
let is_ttuple ty = match ty.raw_ty with TTuple _ -> true | _ -> false
let is_tfun ty = match ty.raw_ty with TFun({func_kind=FNormal}) -> true | _ -> false
let is_tbool ty = match ty.raw_ty with TBool -> true | _ -> false
let is_tint ty = match ty.raw_ty with TInt(_) -> true | _ -> false
let is_tbits ty = match ty.raw_ty with TBits(_) -> true | _ -> false
let is_tlist ty = match ty.raw_ty with TList(_, _) -> true | _ -> false
let is_tevent ty = match ty.raw_ty with TEvent -> true | _ -> false
let is_tabstract name ty = match ty.raw_ty with TAbstract(cid, _) -> Cid.equal cid (Cid.create [name]) | _ -> false
let is_tstring ty = is_tabstract "string" ty
let is_tchar ty = is_tabstract "char" ty

let is_tbuiltin tycid ty = match ty.raw_ty with TBuiltin(cid, _) -> Cid.equal cid tycid | _ -> false



let extract_func_ty ty = match ty.raw_ty with 
  | TFun {arg_tys; ret_ty; func_kind} -> arg_tys, ret_ty, func_kind
  | _ -> raise (FormError "[extract_func_ty] expected TFun")


let extract_tint_size ty = match ty.raw_ty with 
  | TInt size -> size
  | _ -> raise (FormError "[extract_tint_size] expected TInt")

let extract_trecord ty = match (base_type ty).raw_ty with 
  | TRecord(labels, ts) -> labels, ts
  | _ -> raise (FormError "[extract_trecord] expected TRecord")
;;
let extract_ttuple ty = match ty.raw_ty with 
  | TTuple(ts) -> ts
  | _ -> raise (FormError "[extract_ttuple] expected TRecord")
;;
let extract_tlist ty = match ty.raw_ty with 
  | TList(ty, len) -> ty, len
  | _ -> raise (FormError "[extract_tlist] expected TList")
;;
let extract_tenum ty = match ty.raw_ty with 
  | TEnum tagpairs -> tagpairs 
  | _ -> failwith "expected TEnum"
;;

let extract_tbuiltin ty = match ty.raw_ty with 
  | TBuiltin(cid, tyargs) -> cid, tyargs
  | _ -> raise (FormError "[extract_tbuiltin] expected TBuiltin")

let extract_tabstract ty = match ty.raw_ty with 
  | TAbstract(cid, inner_ty) -> cid, inner_ty
  | _ -> raise (FormError "[extract_tabstract] expected TAbstract")

let split_tabstract ty = match ty.raw_ty with 
  | TAbstract(cid, inner_ty) -> tname cid, inner_ty
  | _ -> raise (FormError "[split_tabstract] expected TAbstract")

let rec extract_tname ty = match ty.raw_ty with
  | TName cid -> cid
  | TAbstract(cid, _) -> cid
  | _ -> raise (FormError "[extract_tname] expected TName")

(* value constructors *)
let rec infer_vty = function 
  | VUnit -> ty TUnit
  | VInt {size; _} -> ty (TInt size)
  | VBool _ -> ty TBool
  | VRecord(labels, vs) -> ty (TRecord(labels, List.map (fun v -> v.vty) vs))
  | VTuple(vs) -> ty (TTuple (List.map (fun v -> v.vty) vs))
  | VList(values) -> 
    if (List.length values) == 0 then failwith "cannot infer type of length 0 list" 
    else ty (TList((List.hd values).vty, IConst (List.length values)))
  | VBits {ternary; bits} -> ty (TBits {ternary; len=sz (List.length bits)})
  (* | VClosure {params; fexp; _} -> tfun (List.map snd params) fexp.ety *)
  | VEvent _ -> ty (TEvent)
  | VSymbol (_, inner_ty) -> inner_ty 
;;  

let value v = {v=v; vty=infer_vty v; vspan=Span.default}
let vunit () = {v=VUnit; vty=ty TUnit; vspan=Span.default}
let vint value size = {v=VInt {value; size = sz size}; vty=ty (TInt(sz size)); vspan=Span.default}
(* declare a vint with size derived from ty *)
let vint_ty value ty = match ty.raw_ty with 
  | TInt  size -> vint value size
  | _ -> failwith "vint_ty: expected TInt"
let vbool b = {v=VBool b; vty=ty TBool; vspan=Span.default}
let vlist vs = {v=VList vs; vty=ty (TList((List.hd vs).vty, IConst (List.length vs))); vspan=Span.default}
let vtup vs = {v=VTuple(vs); vty=ttuple (List.map (fun v -> v.vty) vs); vspan=Span.default}
let vpat ints = {v=VBits {ternary=true; bits=ints}; vty=ty (TBits {ternary=true; len=sz (List.length ints)}); vspan=Span.default}
let vbits ints = {v=VBits {ternary=false; bits=ints}; vty=ty (TBits {ternary=false; len=sz (List.length ints)}); vspan=Span.default}
let vrecord labels values = {v=VRecord(labels, values); vty=trecord labels (List.map (fun v -> v.vty) values); vspan=Span.default}
let vtuple vs = {v=VTuple(vs); vty=ttuple (List.map (fun v -> v.vty) vs); vspan=Span.default}
let vevent evid evnum evdata meta = {v=VEvent {evid; evnum; evdata; meta}; vty=ty TEvent; vspan=Span.default}
let vevent_simple evid evdata = vevent evid None evdata []
let venum tag ty = {v=VSymbol(tag, ty); vty=ty; vspan=Span.default}
let vsymbol str ty = venum str ty

(* cast a value to an abstract type *)
let abstr_cast_value cid value = 
  {value with vty=tabstract_cid cid value.vty}
;;

(* BUILTIN *)
let zero_list ty = vsymbol (Id.create "{0}") ty;;


let string_to_value (s:string) =
  let chars = List.of_seq (String.to_seq s) in
  let vchars = List.map (fun c -> vint (Char.code c) 8) chars in
  (* wrap chars in "char" type *)
  let vchars = List.map (fun value -> {value with vty=tabstract "char" value.vty}) vchars in
  let vstr = vlist vchars in
  {vstr with vty=tabstract "string" vstr.vty}
;;
let charints_to_string (v:value) =
  match v.v with
  | VList ints ->
    let charints = List.map 
      (fun v -> 
        match v.v with 
          | VInt({value}) -> value
          | _ -> failwith "strings are encoded as int tuples") 
      ints 
    in
    let chars = List.map (fun i -> Char.chr i) charints in
    String.of_seq (List.to_seq chars)
  | _ -> failwith "strings are encoded as int tuples"
;;

let rec default_value ty = match ty.raw_ty with 
  | TUnit -> vunit ()
  | TInt size -> vint 0 size
  | TBool -> vbool false
  | TRecord(labels, ts) -> 
    vrecord labels (List.map default_value ts)
  | TTuple(ts) -> 
    vtuple (List.map default_value ts)
  | TList(t, IConst n) -> 
    vlist (List.init n (fun _ -> default_value t))
  | TList(_, _) -> failwith "no default value non-constant list length"
  | TFun _ -> failwith "no default value for function type"
  | TBits{len} -> vbits (List.init len (fun _ -> 0))
  | TEvent -> failwith "no default value for event type"
  | TEnum(cases) -> 
    venum ((List.hd cases) |> fst) ty
  | TBuiltin _ -> failwith "no default value for builtin type"
  | TName _ -> failwith "no default value for named type"
  | TAbstract(_, ty) -> default_value ty
;;


let extract_vevent value = match value.v with 
  | VEvent ev -> ev
  | _ -> failwith "expected VEvent"
;;
let extract_vint value = match value.v with 
  | VInt {value; _} -> value
  | _ -> failwith "expected VInt"
;;
let extract_vsymbol v = match v.v with 
  | VSymbol(tag, _) -> tag
  | _ -> failwith "expected VEnum"
;;

let extract_vrecord value = match value.v with 
  | VRecord(labels, vs) -> labels, vs
  | _ -> failwith "expected VRecord"
;;
let extract_vtuple value = match value.v with 
  | VTuple(vs) -> vs
  | _ -> failwith "expected VRecord"
;;
(* expression constructors *)
let exp e ety espan = {e; ety; espan}
let efunref cid fty = {e=EVar (cid); ety=fty; espan=Span.default; }
let erecord labels es = {e=ERecord(labels, es); ety=trecord labels (List.map (fun (e:exp) -> e.ety) es); espan=Span.default; }
let etuple es = {e=ETuple es; ety=ttuple (List.map (fun (e:exp) -> e.ety) es); espan=Span.default; }

let eop op es = 
  let eop_ty = match op with 
    | And | Or | Not
    | Eq  | Neq | Less| More | Leq | Geq -> ty TBool
    | Neg | Plus| Sub | SatPlus | SatSub -> (List.hd es).ety
    | BitAnd  | BitOr | BitXor | BitNot | LShift | RShift -> (List.hd es).ety
    | Slice(hi, lo) -> ty (TInt (sz (hi - lo + 1)))
    | PatExact
    | PatMask ->      
      let sz = match (List.hd es).ety.raw_ty with 
      | TInt sz -> sz
      | _ -> failwith "pat op expects int args"
      in
      ty (TBits {ternary=true; len=sz})
    | Hash size -> ty (TInt size)
    | Cast size  -> ty (TInt size)
    | Conc -> 
      let arg_sizes = List.map (fun e -> match e.ety.raw_ty with TInt sz -> sz | _ -> failwith "conc expects int args") es in
      ty (TInt (sz (List.fold_left (+) 0 arg_sizes)))

    | Project(id) -> 
      let rec_arg = List.hd es in
      let labels, ts = match (base_type rec_arg.ety).raw_ty with 
        | TRecord(labels, ts) -> labels, ts
        | _ -> 
          failwith "project expects record arg"
      in
      let labels_ts = List.combine labels ts in
      let _, ty = List.find (fun (label, _) -> Id.equal label id) labels_ts in
      ty
    
    | Get(idx) -> 
      let ts = match (List.hd es).ety.raw_ty with 
        | TTuple ts -> ts
        | _ -> failwith "get expects tuple arg"
      in
      List.nth ts idx
  in 
  {e=EOp (op, es); ety=eop_ty; espan=Span.default; }
let eval value = {e=EVal value; ety=value.vty; espan=Span.default; }
let evar id ty = {e=EVar(id); ety=ty; espan=Span.default; }
let eunit () = eval (vunit ())

let default_exp ty = eval@@default_value ty

let eproj rec_exp field_id = 
  eop (Project(field_id)) [rec_exp]
;;

let ecall_kind call_kind f es = 
  let ety = match f.ety.raw_ty with 
    | TFun {ret_ty; _} -> ret_ty
    | TEvent -> tevent
    | _ -> failwith "ecall: expected function type"
  in
  {e=ECall {f; args=es; call_kind}; ety; espan=Span.default; }
;;
let ecall = ecall_kind CFun

let ecall_op (f: exp) args = ecall f args

let eevent = ecall_kind CEvent

let elistget exp arridx = 
  let cell_ty = extract_tlist exp.ety |> fst in
  {e=EListGet(exp, arridx); ety=cell_ty; espan=Span.default}


(* BUILTIN FUNCTIONS: generates *)
let fgen_ty = tfun_kind FExtern [tevent] (tunit ())
let egen_self ev = 
  ecall (efunref (Cid.create ["generate_self"]) fgen_ty) [ev]
let egen_switch loc ev = 
  ecall (efunref (Cid.create ["generate_switch"]) fgen_ty) [loc; ev]
;;
let egen_group loc ev = 
  ecall (efunref (Cid.create ["generate_group"]) fgen_ty) [loc; ev]
;;
let egen_port loc ev = 
  ecall (efunref (Cid.create ["generate_port"]) fgen_ty) [loc; ev]

(* form checking *)
let etup_form exp = match exp.e with
  | ETuple _ -> true
  | EVal {v=VTuple _} -> true
  | _ -> false
;;
let erec_form exp = match exp.e with
  | ERecord _ -> true
  | EVal {v=VRecord _} -> true
  | _ -> false
;;

let flatten_tuple exp = match exp.e with
  | ETuple es -> es
  | EVal {v=VTuple es} -> 
    List.map (fun v -> eval v) es
  | _ -> raise (FormError "[flatten_tuple] expected tuple")
;;
let rec flatten_exp exp = match exp.e with
  | ETuple es -> List.concat (List.map flatten_exp es)
  | ERecord(_, es) -> List.concat (List.map flatten_exp es)
  | _ -> [exp]
;;

let extract_evar exp = match exp.e with
  | EVar id -> id, exp.ety
  | _ -> raise (FormError "[extract_evar] expected EVar")
;;

let extract_evar_id exp = match exp.e with 
| EVar(cid) -> Cid.to_id(cid), exp.ety
| _ -> failwith "[evar_to_param] not an evar"
;;



(* let egen loc ev = ecall (efunref (Cid.create ["generate"]) (tfun [tevent] (tunit ()))) [loc; ev] *)


(* let emultiassign ids tys new_vars rhs_exp = exp (EAssign {ids; tys; new_vars; exp=rhs_exp}) (ty TUnit) Span.default *)
(* let elocal id ty exp = emultiassign [id] [ty] true exp *)
(* let eassign id exp = emultiassign [id] [exp.ety] false exp *)
(* let eif cond exp_then exp_else = exp (EIf(cond, exp_then, exp_else)) exp_then.ety Span.default *)
(* let ematch match_exp branches = exp (EMatch(match_exp, branches)) (List.hd branches |> snd).ety Span.default *)
(* let eseq exp1 exp2 = exp (ESeq(exp1, exp2)) exp2.ety (Span.extend exp1.espan exp2.espan) *)
(* let eret eret = exp (EReturn eret) (tunit ()) Span.default *)
let ewrap espan exp = {exp with espan}

let patval value = PVal(value)

let case enum_ty tag_id statement : branch = 
  ([patval (venum tag_id enum_ty)]), statement

(* statements *)
let s s sspan = {s; sspan;}
let sass op exp = s (SAssign(op, exp)) Span.default
let stupleassign ids rhs_exp = sass (OTupleAssign ids) rhs_exp
let stuplelocal ids tys rhs_exp = sass (OTupleLocal(ids, tys)) rhs_exp
let slocal id ty exp = sass (OLocal(id, ty)) exp
let sassign id exp = sass (OAssign id) exp
let slistset arr idx exp = sass (OListSet{arr; idx}) exp 
let srecordset rec_exp field exp = sass (ORecordSet{rec_exp; field}) exp
let sif cond s_then s_else = s (SIf(cond, s_then, s_else)) Span.default
let smatch match_exp branches = s (SMatch(match_exp, branches)) Span.default
let snoop = s SNoop Span.default
let sunit exp = s (SUnit exp) Span.default
let sret_none = s (SRet None) Span.default
let sret eret = s (SRet (Some eret)) Span.default


let sfor idx bound stmt = 
  s (SFor{idx; bound; stmt; guard=None}) Span.default
;;

let swhile idx bound guard stmt = 
  s (SFor{idx; bound; stmt; guard=Some(guard)}) Span.default
;;

let sseq s1 s2 = 
  let span = try Span.extend s1.sspan s2.sspan with _ -> Span.default in
  s (SSeq(s1, s2)) span
;;
let stmts stmts = 
  match stmts with 
  | [] -> snoop
  | _ ->
    List.fold_left (fun acc s -> sseq acc s) (List.hd stmts) (List.tl stmts)
;;


let swrap sspan s = {s with sspan}

(* declarations *)

(* declarations *)
let decl d dspan = {d; dspan;}
let dfun_kind fun_kind id rty params body = 
  decl (DFun(fun_kind, id, rty, params, Some body)) Span.default
let dfun = dfun_kind FNormal
let dhandler = dfun_kind FHandler
let dparser = dfun_kind FParser
let daction = dfun_kind FAction 
let dmemop = dfun_kind FMemop
(* global variables *)
let dglobal id ty exp = decl (DVar(id, ty, Some(exp))) Span.default
let dextern id ty = decl (DVar(id, ty, None)) Span.default
(* type declarations *)
let dty tycid ty = decl (DTy(tycid, Some ty)) Span.default
let dty_ext tycid = decl (DTy(tycid, None)) Span.default

let decl_tabstract ty = 
  let tname = alias_type ty in
  let ty = base_type ty in
  let name = extract_tname tname in
  dty name ty
;;


(* event declarations *)
let devent id evconstrnum params is_parsed = decl (DEvent {evconstrid=id; evconstrnum; evparams=params; is_parsed}) Span.default

let extract_daction_opt decl = match decl.d with 
  | DFun(FAction, id, ty, params, Some body) -> Some (id, ty, params, body)
  | _ -> None
;;
let extract_daction_id_opt decl = match decl.d with 
  | DFun(FAction, id, _, _, _) -> Some id
  | _ -> None

(* helpers *)
let untuple exp = match exp.e with
  | ETuple es -> es
  | _ -> [exp]
;;
let retuple exps = match exps with
  | [exp] -> exp
  | _ -> etuple exps
;;

let kind_of_tfun raw_ty = match raw_ty with 
  | TFun {func_kind; _} -> func_kind
  | _ -> failwith "kind_of_tfun: expected TFun"
;;

(* partial evaluation of constant expressions *)


exception EvalFailure of string
let eval_err msg = raise (EvalFailure msg)

let eval_vint value = match value.v with 
  | VInt {value; _} -> value
  | _ -> eval_err "expected VInt"
;;

let rec eval_exp exp = 
  match exp.e with 
  | EVal(value) -> value
  | ECall {f; args; _} -> 
    (* calls evaluate to event values *)
    let argvals = List.map eval_exp args in
    let f_cid = extract_evar f |> fst in
    vevent_simple f_cid argvals  
  | EVar (cid) -> vsymbol (Cid.to_id cid) exp.ety
  | ETuple es -> 
    let es = List.map eval_exp es in
    vtuple es
  | ERecord(labels, es) -> 
    let es = List.map eval_exp es in
    vrecord labels es
  | _ ->  eval_err "cannot evalute expression type"
;;



(* function call: f <op> args --> build expression that calls f on args *)
let ( /** ) f args = ecall_op f args

(* record projection: rec <op> str --> build expression that gets field str from record rec *)
let ( /-> ) rec_exp field_str = 
  eop (Project(Id.create field_str)) [rec_exp]
;;

let (/@) my_arr_exp idx_id = 
  elistget my_arr_exp (idxvar idx_id)
;;

let (/<-) (arr, idx) rhs = 
  slistset arr idx rhs
;;
(* assignment *)
let ( /:= ) var_id rhs_exp = 
  sassign (Cid.id var_id) rhs_exp
;;
let ( /::=) var_id rhs_exp = 
  slocal var_id rhs_exp.ety rhs_exp
;;

let ( /: ) stmt1 stmt2 = 
  sseq stmt1 stmt2
;;