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

(* length of an array. I think its just a size, 
   but not positive how it will work out yet. *)
and arrlen = 
    | IConst of int
    | IVar of cid

and func_kind = | FNormal | FHandler | FParser | FAction | FMemop | FForiegn
and raw_ty = 
  (* value types *)
  | TUnit
  | TInt of size 
  | TBool 
  | TBits of {ternary: bool; len : size;}
  | TEnum of (cid * int) list 
  | TUnion of cid list * ty list
  | TRecord of cid list * ty list
  | TTuple  of ty list
  | TPtr of ty * (arrlen option) (* a pointer to a memory cell of a certain type or an array of them *)
  | TEvent
  | TFun of func_ty
  (* alias types *)
  | TBuiltin of cid * (ty list) (* abstract types built into lucid that must be implemented in CCore *)
  | TAbstract of cid * ty (* a name for another type *)
  | TName of cid (* an opaque TAbstract -- we should choose one or the other*)

and func_ty = {
  arg_tys : ty list; 
  ret_ty : ty; 
  func_kind : func_kind;
}
and ty = {raw_ty:raw_ty; tspan : sp; timplements : (ty[@opaque]) option}

and params = (cid * ty) list
(* values *)
and v =
  | VUnit
  | VInt of {value : int; size : size;}
  | VBool of bool
  | VUnion of cid * value * ty (* ty is the union type *)
  | VRecord of cid list * value list
  | VTuple of value list
  | VList  of value list
  | VBits of {ternary: bool; bits : int list;}
  | VEvent of vevent
  | VSymbol of cid * ty 
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
            | Cast of ty 
            | Conc
            | Project of cid | Get of int 
            | Mod

and e = 
  | EVal of value
  | EOp of op * exp list
  | ECall of {f:exp; args:exp list; call_kind:call_kind;}
  | EVar    of cid 
  | EAddr   of cid (* tref *)
  | ETuple of exp list
  | EUnion  of cid * exp * ty
  | ERecord of cid list * exp list
  | EDeref of exp

and call_kind = 
  | CFun
  | CEvent 
  (* a call to a builtin is annotated with the original 
     builtin function and arguments *)

and exp = {e:e; ety:ty; espan : sp; eimplements : (exp[@opaque]) option}

and pat = 
  | PVal of value
  | PEvent of {event_id : cid; params : params;}
  | PWild of ty 

and branch = pat list * statement

and assign_op = 
  | OLocal  of cid * ty (* create a new variable *)
  | OAssign of exp          (* local variables, array and record elements, implicitly dereferenced globals *)
  | OTupleLocal of cid list * ty list (* create new variables, unpack tuple to them *)
  | OTupleAssign of exp list (* unpack tuple to variables *)

and s = 
  | SNoop
  | SUnit of exp
  | SAssign of assign_op * exp
  | SFor of {idx : cid; bound : arrlen; stmt: statement; guard : cid option}
    (* for (idx < bound) while guard *)
  | SForEver of statement (* infinite loop *)
  | SIf of exp * statement * statement
  | SMatch of exp list * branch list
  | SSeq of statement * statement
  | SRet of exp option

and statement = {s:s; sspan : sp;}

(* declarations *)
and event_def = {evconstrid : cid; evconstrnum : int option; evparams : params; is_packet : bool}


and fun_def = func_kind * cid * ty * params * fun_body

and fun_body = 
  | BExtern
  | BStatement of statement
  | BForiegn of string
  
and d = 
  | DForiegn of string (* misc things in the underlying language. Imports, etc. *)
  | DVar of cid * ty * (exp option)
  | DFun of fun_def
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


(* CONSTANTS *)
let event_tag_size = 16
let enum_size = 32

exception FormError of string

(* constructors and destructors *)

let id = Id.create
let cid s = Cid.create [s]
let arrlen_ct = ref (-1);;
let fresh_arrlen str = 
  arrlen_ct := (!arrlen_ct + 1);
  IVar (Cid.fresh str)
;;
let arrlen i = IConst i
let idxvar id = IVar id
let sz n = n
let cid s = Cid.create([s])

(**** types ****)
let ty raw_ty = {raw_ty=raw_ty; tspan=Span.default; timplements = None}
let tunit = ty TUnit
let tbool = ty TBool
let tint i = ty@@TInt(sz i)
let tpat len = ty (TBits {ternary=false; len})
let tptr base_ty = ty (TPtr(base_ty,None))
let tevent = ty TEvent
(* let trecord labels tys = ty (TRecord(labels, tys)) *)
let trecord pairs = 
  let cids, tys = List.split pairs in
  ty (TRecord(cids, tys))
let ttuple tys = ty (TTuple tys)
let tunion labels tys = ty (TUnion(labels, tys))  
let tfun_kind func_kind arg_tys ret_ty = ty (TFun {arg_tys; ret_ty; func_kind})
let tfun arg_tys ret_ty = tfun_kind FNormal arg_tys ret_ty 
(* global type from CoreSyntax *)
let tlist ele_ty len = ty (TPtr(ele_ty, Some(len)))
let tname cid = ty (TName(cid))
let textern = tname (Cid.create ["_extern_ty_"])
let tbuiltin cid tyargs = ty (TBuiltin(cid, tyargs))
let tgroup_cid = Cid.create ["Group"]
let tgroup = tbuiltin tgroup_cid []
let tabstract n inner_ty = ty (TAbstract(Cid.create [n], inner_ty))
let tabstract_cid tcid inner_ty = ty (TAbstract(tcid, inner_ty))
let tabstract_id id inner_ty = ty (TAbstract(Cid.create_ids [id], inner_ty))
let tenum_pairs (tagpairs : (Cid.t * int) list) = ty (TEnum tagpairs)
let tenum ids = tenum_pairs (List.mapi (fun i id -> (id, i)) ids)
let tref t = ty (TPtr(t, None))
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


let tunion_pairs pairs = 
  let cids, tys = List.split pairs in
  tunion cids tys
;;

let tchar = tabstract "char" (tint 8)


let is_textern ty = match ty.raw_ty with TName cid -> Cid.equal cid (Cid.create ["_extern_ty_"]) | _ -> false
let is_tunit ty = match ty.raw_ty with TUnit -> true | _ -> false
let is_tunion ty = match (base_type ty).raw_ty with TUnion _ -> true | _ -> false
let is_trecord ty = match (base_type ty).raw_ty with TRecord _ -> true | _ -> false
let is_ttuple ty = match ty.raw_ty with TTuple _ -> true | _ -> false
let is_tfun ty = match ty.raw_ty with TFun({func_kind=FNormal}) -> true | _ -> false
let is_tbool ty = match ty.raw_ty with TBool -> true | _ -> false
let is_tint ty = match ty.raw_ty with TInt(_) -> true | _ -> false
let is_tbits ty = match ty.raw_ty with TBits(_) -> true | _ -> false
let is_tlist ty = match ty.raw_ty with TPtr(_, Some _) -> true | _ -> false
let is_tevent ty = match ty.raw_ty with TEvent -> true | _ -> false
let is_tabstract name ty = match ty.raw_ty with TAbstract(cid, _) -> Cid.equal cid (Cid.create [name]) | _ -> false
let is_tstring ty = is_tabstract "string" ty
let is_tchar ty = is_tabstract "char" ty
let is_tbuiltin tycid ty = match ty.raw_ty with TBuiltin(cid, _) -> Cid.equal cid tycid | _ -> false
let is_tbuiltin_any ty = match ty.raw_ty with TBuiltin(_, _) -> true | _ -> false
let is_tref  ty = match ty.raw_ty with TPtr _ -> true | _ -> false
let is_tenum ty = match (base_type ty).raw_ty with TEnum _ -> true | _ -> false

let extract_func_ty ty = match ty.raw_ty with 
  | TFun {arg_tys; ret_ty; func_kind} -> arg_tys, ret_ty, func_kind
  | _ -> raise (FormError "[extract_func_ty] expected TFun")

let extract_tint_size ty = match (base_type ty).raw_ty with 
  | TInt size -> size
  | TEnum _ -> enum_size
  | _ -> raise (FormError "[extract_tint_size] expected TInt")

let extract_trecord_or_union ty = match ty.raw_ty with 
  | TRecord(labels, ts) -> labels, ts
  | TUnion(labels, ts) -> labels, ts
  | _ -> raise (FormError "[extract_trecord_or_union] expected TRecord or TUnion")
;;
let extract_trecord ty = match (base_type ty).raw_ty with 
  | TRecord(labels, ts) -> labels, ts
  | _ -> raise (FormError "[extract_trecord] expected TRecord")
;;
let extract_ttuple ty = match ty.raw_ty with 
  | TTuple(ts) -> ts
  | _ -> raise (FormError "[extract_ttuple] expected TRecord")
;;
let extract_tlist ty = match ty.raw_ty with 
  | TPtr(ty, Some(len)) -> ty, len
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

let extract_tref ty = match ty.raw_ty with 
  | TPtr(tinner, _) -> tinner
  | _ -> raise (FormError "[extract_tref] expected TGlobal")


let tuple_length ty = match ty.raw_ty with 
  | TTuple ts -> List.length ts
  | _ -> raise (FormError "[ttup_len] expected TTuple")
;;

let rec bitsizeof_ty ty = 
  match ty.raw_ty with 
  | TUnit -> 0 |> Option.some
  | TInt size -> size |> Option.some
  | TBool -> 8 |> Option.some (* uint8_t *)
  | TEnum _ -> 32 |> Option.some
  | TUnion(_, tys) -> 
    tys |> List.map bitsizeof_ty_exn |> List.fold_left (max) 0 |> Option.some
  | TRecord(_, tys)
  | TTuple(tys) -> 
      tys |> List.map bitsizeof_ty_exn |> List.fold_left (+) 0 |> Option.some
  | TPtr _ -> None
  | TBits {len} -> len |> Option.some
  | TEvent -> None
  | TFun _ -> None
  | TBuiltin _ -> None
  | TName _ -> None
  | TAbstract(_, ty) -> bitsizeof_ty ty
and bitsizeof_ty_exn ty = 
  match bitsizeof_ty ty with 
  | Some size -> size
  | None -> failwith "bitsizeof_ty_exn: got an unsizeable type"

let size_of_ty ty = 
  let n = bitsizeof_ty_exn ty in
  let byte_n = (n+7) / 8 in
  byte_n
;;
    

let sizeof_ty ty = 
  match ty.raw_ty with 
  | TInt size -> size
  | TBool -> 1
  | TBits {len} -> len
  | _ -> failwith "sizeof_ty: expected TInt or TBits"
;;

let timpl_wrap ty impl = {ty with timplements = Some impl}


(* value constructors *)
let value v vty = {v=v; vty=vty; vspan=Span.default}
let vunit () = {v=VUnit; vty=ty TUnit; vspan=Span.default}
let vint value size = {v=VInt {value; size = sz size}; vty=ty (TInt(sz size)); vspan=Span.default}
(* declare a vint with size derived from ty *)
let vint_ty value ty = match ty.raw_ty with 
  | TInt  size -> vint value size
  | _ -> failwith "vint_ty: expected TInt"
let vbool b = {v=VBool b; vty=ty TBool; vspan=Span.default}
let vlist vs = {v=VList vs; vty=ty (TPtr((List.hd vs).vty, Some(IConst (List.length vs)))); vspan=Span.default}
let vtup vs = {v=VTuple(vs); vty=ttuple (List.map (fun v -> v.vty) vs); vspan=Span.default}
let vpat ints = {v=VBits {ternary=true; bits=ints}; vty=ty (TBits {ternary=true; len=sz (List.length ints)}); vspan=Span.default}
let vbits ints = {v=VBits {ternary=false; bits=ints}; vty=ty (TBits {ternary=false; len=sz (List.length ints)}); vspan=Span.default}
let vrecord label_values = 
  let labels, values = List.split label_values in
  {v=VRecord(labels, values); vty=trecord (List.map (fun (id, value) -> (id, value.vty)) label_values); vspan=Span.default}
;;  
  
  (* labels (List.map (fun v -> v.vty) values); vspan=Span.default} *)

  (* vrecord labels values *)
let vunion label value ty = {v=VUnion(label, value, ty); vty=ty; vspan=Span.default}
let vtuple vs = {v=VTuple(vs); vty=ttuple (List.map (fun v -> v.vty) vs); vspan=Span.default}
let vevent evid evnum evdata meta = {v=VEvent {evid; evnum; evdata; meta}; vty=ty TEvent; vspan=Span.default}
let vevent_simple evid evdata = vevent evid None evdata []
let venum tag ty = {v=VSymbol(tag, ty); vty=ty; vspan=Span.default}
let vsymbol str ty = venum str ty

(* cast a value to an abstract type *)
(* this is WEIRD... *)
(* let abstr_cast_value cid value = 
  {value with vty=tabstract_cid cid value.vty}
;; *)

(* BUILTIN *)
let zero_list ty = vsymbol (cid "{0}") ty;;
let memzero ty = vsymbol (cid "{0}") ty;;


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
    vrecord (List.map (fun (label, ty) -> (label, default_value ty)) (List.combine labels ts))
  | TUnion(ids, tys) -> vunion (List.hd ids) (List.hd tys |> default_value) ty
  | TTuple(ts) -> 
    vtuple (List.map default_value ts)
  | TFun _ -> failwith "no default value for function type"
  | TBits{len} -> vbits (List.init len (fun _ -> 0))
  | TEvent -> vevent (Cid.create ["_none"]) None [] []
  | TEnum(cases) -> 
    venum ((List.hd cases) |> fst) ty
  | TBuiltin _ -> failwith "no default value for builtin type"
  | TName _ -> failwith "no default value for named type"
  | TAbstract(_, inner_ty) -> {(default_value inner_ty) with vty=ty}
  | TPtr(inner_ty, None) -> {(default_value inner_ty) with vty=ty}
  | TPtr(ty, Some(IConst(n))) -> vlist (List.init n (fun _ -> default_value ty))
  | TPtr(_, Some _) -> failwith "no default value for list of unknown length"
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
let exp e ety espan = {e; ety; espan; eimplements=None; }
let efunref cid fty = exp (EVar (cid)) fty (Span.default)
let erecord label_es = 
  let labels, es = List.split label_es in
  {e=ERecord(labels, es); 
   ety = trecord (List.map (fun (label, exp) -> (label, exp.ety)) label_es); 
   espan=Span.default;
   eimplements=None; }
;;
let eunion label e ety = 
  exp (EUnion(label, e, ety)) ety (Span.default)
let etuple es = 
  exp (ETuple es) (ttuple (List.map (fun e -> e.ety) es)) Span.default

let eop op es = 
  let eop_ty = match op with 
    | And | Or | Not
    | Eq  | Neq | Less| More | Leq | Geq -> ty TBool
    | Neg | Plus| Sub | SatPlus | SatSub -> (List.hd es).ety
    | BitAnd  | BitOr | BitXor | BitNot | LShift | RShift | Mod -> (List.hd es).ety
    | Slice(hi, lo) -> ty (TInt (sz (hi - lo + 1)))
    | PatExact
    | PatMask ->      
      let sz = match (List.hd es).ety.raw_ty with 
      | TInt sz -> sz
      | _ -> failwith "pat op expects int args"
      in
      ty (TBits {ternary=true; len=sz})
    | Hash size -> ty (TInt size)
    | Cast ty  -> ty
    | Conc -> 
      let arg_sizes = List.map (fun e -> match e.ety.raw_ty with TInt sz -> sz | _ -> failwith "conc expects int args") es in
      ty (TInt (sz (List.fold_left (+) 0 arg_sizes)))

    | Project(id) -> 
      let rec_arg = List.hd es in
      let labels, ts = match (base_type rec_arg.ety).raw_ty with 
        | TRecord(labels, ts) -> labels, ts
        | TUnion(labels, ts) -> labels, ts
        | _ -> failwith "project expects record or union arg"
      in
      let labels_ts = List.combine labels ts in
      (* print_endline ("looking for id: "^(Id.to_string id));
      print_endline ("in ids: "^(String.concat " , " (List.map Id.to_string labels))); *)
      (* let _, ty = List.find (fun (label, _) -> Id.equal label id) labels_ts in *)
      let _, ty = List.find (fun (label, _) -> Cid.name label = Cid.name id) labels_ts in
      ty
    
    | Get(idx) -> 
      let ts = match (List.hd es).ety.raw_ty with 
        | TTuple ts -> ts
        | _ -> failwith "get expects tuple arg"
      in
      List.nth ts idx

  in 
  exp (EOp(op, es)) eop_ty Span.default
let eval value = exp (EVal value) value.vty Span.default
let evar cid ty = exp (EVar cid) ty Span.default
let param_evar (id, ty) = evar id ty
let eunit () = eval (vunit ())

let ecast ty exp = eop (Cast ty) [exp]

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
  exp (ECall {f; args=es; call_kind}) ety Span.default
;;
let ecall = ecall_kind CFun

let ecall_op (f: exp) args = ecall f args

let eevent = ecall_kind CEvent

let ederef inner = 
  exp (EDeref inner) (extract_tref inner.ety) Span.default
;;
let eaddr cid ty = 
  exp (EAddr cid) (tref ty) Span.default
;;


let elistget arr idx = 
  (* a list get is just sugar for an add and deref *)
  (* update -- use pointer arith instead *)
  let pos = eop Plus [arr; idx] in
  ederef pos
  (* let cell_ty = extract_tlist arr.ety |> fst in
  {e=EListIdx(arr, idx); ety=cell_ty; espan=Span.default} *)


let to_ref exp = 
  (* turn an expression for a local value into 
     a reference to a ref value:
      1. wrap exp type in a ref
      2. wrap expression in a deref *)
  let gety = tref exp.ety in
  ederef {exp with ety=gety}
;;


(* form checking *)

let is_eop exp = match exp.e with 
  | EOp _ -> true
  | _ -> false

let is_eproject exp = match exp.e with 
  | EOp(Project _, _) -> true
  | _ -> false

let is_elistidx exp = match exp.e with 
  | EDeref(inner_exp) -> (
    match inner_exp.e, inner_exp.ety.raw_ty with 
      | EOp(Plus, _), TPtr _ -> true
      | _ -> false
  )
  | _ -> false

let is_evar exp = match exp.e with 
  | EVar _ -> true
  | _ -> false

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

let is_ederef exp = match exp.e with 
  | EDeref _ -> true
  | _ -> false
;;

let extract_ederef exp = match exp.e with 
  | EDeref inner -> inner
  | _ -> raise (FormError "[extract_ederef] expected EDeref")
;;


(* extracting components of expressions *)
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

let extract_etuple exp = match exp.e with
  | ETuple es -> es
  | _ -> raise (FormError "[flatten_tuple] expected tuple")
;;
let extract_evar_id exp = match exp.e with 
| EVar(cid) -> Cid.to_id(cid), exp.ety
| _ -> failwith "[evar_to_param] not an evar"
;;
let extract_ecall exp = match exp.e with
  | ECall {f; args; _} -> f, args
  | _ -> raise (FormError "[extract_ecall] expected ECall")
;;
let args exp = extract_ecall exp |> snd
let arg exp = args exp |> List.hd

(* generates are custom statements into CoreSyntax, 
   but extern functions of CCoreSyntax  *)
let fgen_ty = tfun_kind FNormal [tevent] tunit
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
;;
let is_egen_self exp = match exp.e with 
  | ECall {f; _} -> Cid.equal (extract_evar f |> fst) (Cid.create ["generate_self"])
  | _ -> false
;;
let is_egen_port exp = match exp.e with 
  | ECall {f; _} -> Cid.equal (extract_evar f |> fst) (Cid.create ["generate_port"])
  | _ -> false
;;
let is_egen_switch exp = match exp.e with 
  | ECall {f; _} -> Cid.equal (extract_evar f |> fst) (Cid.create ["generate_switch"])
  | _ -> false
let is_egen_group exp = match exp.e with 
  | ECall {f; _} -> Cid.equal (extract_evar f |> fst) (Cid.create ["generate_group"])
  | _ -> false
;;

let is_ecall_cid exp cid = match exp.e with 
| ECall {f; _} -> Cid.equal (extract_evar f |> fst) cid
| _ -> false
;;
let unbox_egen_self exp = match exp.e with 
  | ECall {args=[eport]} -> eport
  | _ -> failwith "unbox_egen_self: invalid form for generate"
;;
let unbox_egen_port exp = match exp.e with 
  | ECall {args=[eport; eevent]} -> (eport, eevent)
  | _ -> failwith "unbox_egen_port: invalid form for generate"
;;
let unbox_egen_switch exp = match exp.e with 
  | ECall {args=[eloc; eevent]} -> (eloc, eevent)
  | _ -> failwith "unbox_egen_switch: invalid form for generate"

(* let emultiassign ids tys new_vars rhs_exp = exp (EAssign {ids; tys; new_vars; exp=rhs_exp}) (ty TUnit) Span.default *)
(* let elocal id ty exp = emultiassign [id] [ty] true exp *)
(* let eassign id exp = emultiassign [id] [exp.ety] false exp *)
(* let eif cond exp_then exp_else = exp (EIf(cond, exp_then, exp_else)) exp_then.ety Span.default *)
(* let ematch match_exp branches = exp (EMatch(match_exp, branches)) (List.hd branches |> snd).ety Span.default *)
(* let eseq exp1 exp2 = exp (ESeq(exp1, exp2)) exp2.ety (Span.extend exp1.espan exp2.espan) *)
(* let eret eret = exp (EReturn eret) (tunit) Span.default *)
let ewrap espan exp = {exp with espan}

let eimpl_wrap e eimpl = 
  {e with eimplements = Some(eimpl)}

let patval value = PVal(value)

let pevent event_id params = 
  PEvent{event_id; params}

let case enum_ty tag_id statement : branch = 
  ([patval (venum tag_id enum_ty)]), statement

(* statements *)
let s s sspan = {s; sspan;}
let sass op exp = s (SAssign(op, exp)) Span.default
let stupleassign ids rhs_exp = sass (OTupleAssign ids) rhs_exp
let stuplelocal ids tys rhs_exp = sass (OTupleLocal(ids, tys)) rhs_exp
let slocal id ty exp = sass (OLocal(id, ty)) exp
let sassign id exp = sass (OAssign (evar id exp.ety)) exp
let slistset arr idx exp = sass (OAssign(elistget arr idx)) exp 
(* let slistset_exp arr idx bound exp = 
  let arrlen = arrlen_const_mod idx bound in 
  slistset arr arrlen exp *)
let srecordset rec_exp field exp = 
  let lexp = eproj rec_exp field in
  sass (OAssign(lexp)) exp
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

let rec to_stmt_block stmt = 
  match stmt.s with 
  | SSeq(s1, s2) -> 
    (to_stmt_block s1)@(to_stmt_block s2)
  | _ -> [stmt]
;;

let swrap sspan s = {s with sspan}

let slocal_evar (evar : exp) (exp : exp) = 
  let cid, ty = extract_evar evar in
  slocal cid ty exp
;;
let sassign_exp lhs rhs = 
  sass (OAssign lhs) rhs
;;

(* declarations *)
let decl d dspan = {d; dspan;}
let dfun_kind fun_kind id rty params body = 
  decl (DFun(fun_kind, id, rty, params, BStatement body)) Span.default
let dfun = dfun_kind FNormal
let dhandler = dfun_kind FHandler
let dparser = dfun_kind FParser
let daction = dfun_kind FAction 
let dmemop = dfun_kind FMemop
let dfun_extern id fun_kind param_tys ret_ty = 
  let params = List.map (fun ty -> (Cid.fresh_name "a", ty)) param_tys in
  decl (DFun(fun_kind, id, ret_ty, params, BExtern))
;;
let dvar_const id ty exp = decl (DVar(id, ty, Some(exp))) Span.default
let dvar_extern id ty = decl (DVar(id, ty, None)) Span.default
let dextern id ty = decl (DVar(id, ty, None)) Span.default
let default_checker = Some("gcc -x c - -fsyntax-only");;
let dfun_foriegn fid fparams fret_ty fstr = 
  (* foriegn function with default checker. *)
  decl (DFun(FForiegn, fid, fparams, fret_ty, BForiegn fstr)) Span.default
;;
(* toplevel variable. Should be declaring as a ref type. *)
let dglobal id ty exp = decl (DVar(id, ty, Some(exp))) Span.default


let dty tycid ty = decl (DTy(tycid, Some ty)) Span.default
let dty_ext tycid = decl (DTy(tycid, None)) Span.default
let decl_tabstract ty = 
  let tname = alias_type ty in
  let ty = base_type ty in
  let name = extract_tname tname in
  dty name ty
;;
let devent id evconstrnum params is_packet = decl (DEvent {evconstrid=id; evconstrnum; evparams=params; is_packet}) Span.default

let dforiegn str = decl (DForiegn(str)) Span.default
let dinclude str = dforiegn("#include "^str)


let is_devent decl = match decl.d with 
  | DEvent _ -> true
  | _ -> false
;;
let is_dhandler decl = match decl.d with 
  | DFun(FHandler, _, _, _, _) -> true
  | _ -> false
;;
let is_dparser decl = match decl.d with 
  | DFun(FParser, _, _, _, _) -> true
  | _ -> false
;;


let extract_devent_opt decl = match decl.d with 
  | DEvent ev -> Some ev
  | _ -> None
let extract_dhandle_opt decl = match decl.d with 
| DFun(FHandler, id, ty, params, BStatement body) -> Some (id, ty, params, body)
| _ -> None
;;
let extract_daction_opt decl = match decl.d with 
  | DFun(FAction, id, ty, params, BStatement body) -> Some (id, ty, params, body)
  | _ -> None
;;
let extract_dparser_opt decl = match decl.d with 
  | DFun(FParser, id, ty, params, BStatement body) -> Some(id, ty, params, body)
  | _ -> None
;;
let extract_dparser decl = Option.get (extract_dparser_opt decl)
;;
let extract_daction_id_opt decl = match decl.d with 
  | DFun(FAction, id, _, _, _) -> Some id
  | _ -> None

let extract_dfun_opt decl = match decl.d with 
  | DFun(FNormal, id, ty, params, BStatement body) -> 
    Some(id, ty, params, body)
  | _ -> None
  ;;

let extract_dfun_cid decl = match decl.d with 
  | DFun(_, cid, _, _, _) -> Some(cid)
  | _ -> None
;;

let extract_dvar_cid decl = match decl.d with 
  | DVar(cid, _, _) -> Some(cid)
  | _ -> None

(* derive the type of a declared function *)
(* let extract_dfun_ty decl = match decl.d with 
  | DFun(_, _, ty, params, _) -> tfun params ty
  | _ -> failwith "expected DFun" *)


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
  | EVar (cid) -> vsymbol cid exp.ety
  | ETuple es -> 
    let es = List.map eval_exp es in
    vtuple es
  | ERecord(labels, es) -> 
    let es = List.map eval_exp es in
    vrecord (List.combine labels es)
  | _ ->  eval_err "cannot evalute expression type"
;;


(**** substitute a variable for an expression ****)

(* function call: f <op> args --> build expression that calls f on args *)
let ( /** ) f args = ecall_op f args

(* rec_exp.field_id *)
let ( /. ) rec_exp field_id = 
  eop (Project(field_id)) [rec_exp]
;;
let ( /.@) tup_exp i = 
  eop (Get i) [tup_exp]

(* rec_exp->field_id *)
let ( /-> ) rec_exp field_id = 
  eop (Project(field_id)) [ederef rec_exp]
;;

let (/+) e1 e2 = eop Plus [e1; e2]
let (/&) e1 e2 = eop BitAnd [e1; e2]
let ( /== ) e1 e2 = eop Eq [e1; e2]
let vtrue = eval@@vbool true

let (/@) my_arr_exp idx_exp = 
  elistget my_arr_exp idx_exp
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

(* declarations that must be added to a program *)
let default_event_id = Cid.create ["_none"]
let default_event_decl = devent default_event_id None [] false
let is_default_event_decl decl = match decl.d with 
  | DEvent {evconstrid; _} -> Cid.equal evconstrid default_event_id
  | _ -> false





(* equivalence *)
let rec equiv_tys ty1 ty2 = match ty1.raw_ty, ty2.raw_ty with 
| TUnit, TUnit -> true
| TInt sz1, TInt sz2 -> sz1 = sz2
| TBool, TBool -> true
| TEnum(cid_nums1), TEnum(cid_nums2) -> 
  List.length cid_nums1 = List.length cid_nums2
  && List.for_all2 (fun (cid1, num1) (cid2, num2) -> Cid.equal cid1 cid2 && num1 = num2) cid_nums1 cid_nums2
| TUnion(ids1, tys1), TUnion(ids2, tys2)
| TRecord(ids1, tys1), TRecord(ids2, tys2) -> 
  List.length ids1 = List.length ids2
  && List.for_all2 (fun id1 id2 -> Cid.equal id1 id2) ids1 ids2
  && List.length tys1 = List.length tys2
  && List.for_all2 equiv_tys tys1 tys2
| TTuple(tys1), TTuple(tys2) -> 
  List.length tys1 = List.length tys2
  && List.for_all2 equiv_tys tys1 tys2
| TPtr(t1, None), TPtr(t2, None) -> equiv_tys t1 t2
| TPtr(t1, Some(IConst n1)), TPtr(t2, Some(IConst n2)) -> n1 = n2 && equiv_tys t1 t2
| TBits {ternary=b1; len=l1}, TBits {ternary=b2; len=l2} -> 
  (b1 = b2) && (l1 = l2)
| TEvent, TEvent -> true
| TFun {arg_tys=arg_tys1; ret_ty=ret_ty1; func_kind=fk1}, TFun {arg_tys=arg_tys2; ret_ty=ret_ty2; func_kind=fk2} -> 
  List.length arg_tys1 = List.length arg_tys2
  && List.for_all2 equiv_tys arg_tys1 arg_tys2
  && equiv_tys ret_ty1 ret_ty2
  && fk1 = fk2
| TBuiltin(cid1, tyargs1), TBuiltin(cid2, tyargs2) -> 
  Cid.equal cid1 cid2
  && List.length tyargs1 = List.length tyargs2
  && List.for_all2 equiv_tys tyargs1 tyargs2
| TAbstract(cid1, ty1), TAbstract(cid2, ty2) -> 
  Cid.equal cid1 cid2
  && equiv_tys ty1 ty2
| TName cid1, TName cid2 -> Cid.equal cid1 cid2
| (TUnit|TBool|TEvent|TInt _|TRecord _ | TTuple _ | TName _ | TPtr _ | TUnion _
| TFun _|TBits _|TEnum _|TBuiltin (_, _) |TAbstract _), _ -> false