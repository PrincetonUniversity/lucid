(* simpler functional IR for lucid, with extensions
   for compatability with the current CoreSyntax IR
   (Extensions should be eliminated before any further processing) *)
(* TODO: 
    1. figure out how we want to represent string types
      remember: it doesn't really matter, because the 
      string types don't need to get translated back to 
      coreSyntax.
    2. figure out if the conversion from Array / Table to 
      list is represented by a "Type Function" / "Type Abstraction"
      or just replaced in a transformation pass. 
        remember: if they are just replaced, you may be able to do 
        inference to recover them. Or, perhaps there's a tag. 
    3. add a simple for loop: 
        for $id < $value while $id {
          // the index and guard variables start at 0 and true
          // they can be read and set in the statement
          // the guard variable is optional
          // after each iteration, it gets checked
          // we can put various restrictions on the loop later.
          $statement
        }
*)

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
    | IVar of string

and func_kind = | FNormal | FHandler | FParser | FAction | FMemop | FExtern
and raw_ty = 
  | TUnit
  | TInt of size 
  | TBool 
  | TRecord of {labels : id list option; ts : ty list;}
  | TList of ty * arridx
  | TFun of func_ty
  | TBits of {ternary: bool; len : size;}
  | TEvent
  | TEnum of (string * int) list 
  | TBuiltin of cid * (ty list) (* types built into the lucid language that must be eliminated for c*)
  | TName of cid (* tydef, basically *)
  | TAbstract of cid * ty (* just a wrapper around some other type. For convenience. *)

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
  | VRecord of {labels : id list option; es : value list;}
  | VList  of value list
  (* no closures for now *)
  (* | VClosure of {env : (id * value) list; params: params; fexp : exp;} *)
  | VBits of {ternary: bool; bits : int list;}
  | VEvent of vevent
  | VEnum of string * ty (* symbol in enum * enum ty *)
  (* we should not need VGlobals. Globals should never 
     get to the point of being evaluated. *)
  | VGlobal of {
      global_id : id; (* name of the global, may be interpreted as a pointer variable *)
      global_pos : int; (* position of the global in the pipeline. Every global should have a unique position *)
      global_ty  : ty; (* type of the global *)
  }
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
  | ERecord of {labels : id list option; es : exp list;}
  | ECall of {f:exp; args:exp list; call_kind:call_kind;}
  | EOp of op * exp list
  | EListGet of exp * arridx

and call_kind = 
  | CFun
  | CEvent 
and exp = {e:e; ety:ty; espan : sp;}

and pat = 
  | PVal of value
  | PEvent of {event_id : cid; params : params;}
and branch = pat list * statement
(* statements *)
and s = 
  | SNoop
  | SUnit of exp
  (* assign may create a new variable, and can unpack tuples *)
  | SAssign of {ids : cid list; tys : ty list; new_vars : bool; exp : exp}
  | SListSet of {arr : exp; idx : arridx; exp : exp;} (* arr[idx] = exp; *)
  | SFor of {idx : id; bound : arridx; stmt: statement}
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



(* constructors *)
(* type constructors *)

let arridx_ct = ref (-1);;
let fresh_arridx str = 
  arridx_ct := (!arridx_ct + 1);
  IVar(str^"-"^(string_of_int (!arridx_ct)))
;;
let sz n = n
let ty raw_ty = {raw_ty=raw_ty; tspan=Span.default; }

let tunit () = ty TUnit
let tbool = ty TBool
let tint i = ty@@TInt(sz i)
let tpat len = ty (TBits {ternary=false; len})
let tevent = ty TEvent
let trecord labels tys = ty (TRecord {labels=Some labels; ts=tys})
let ttuple tys = ty (TRecord {labels=None; ts=tys})
let tfun_kind func_kind arg_tys ret_ty = ty (TFun {arg_tys; ret_ty; func_kind})
let tfun arg_tys ret_ty = tfun_kind FNormal arg_tys ret_ty 
(* global type from CoreSyntax *)
let tglobal cid global_tyargs = ty (TBuiltin(cid, global_tyargs))


let tlist ele_ty len = ty (TList(ele_ty, len))
let tname cid = ty (TName(cid))
let tbuiltin cid tyargs = ty (TBuiltin(cid, tyargs))
let tgroup_cid = Cid.create ["Group"]
let tgroup = tname tgroup_cid
let tabstract tname inner_ty = ty (TAbstract(Cid.create [tname], inner_ty))

let textern = tname (Cid.create ["_extern_ty_"])
let is_textern ty = match ty.raw_ty with TName cid -> Cid.equal cid (Cid.create ["_extern_ty_"]) | _ -> false

let is_tunit ty = match ty.raw_ty with TUnit -> true | _ -> false
let is_trecord ty = match ty.raw_ty with TRecord{labels=Some(_)} -> true | _ -> false
let is_ttuple ty = match ty.raw_ty with TRecord{labels=None} -> true | _ -> false
let is_tfun ty = match ty.raw_ty with TFun({func_kind=FNormal}) -> true | _ -> false
let is_tbool ty = match ty.raw_ty with TBool -> true | _ -> false
let is_tint ty = match ty.raw_ty with TInt(_) -> true | _ -> false
let is_tbits ty = match ty.raw_ty with TBits(_) -> true | _ -> false
let is_tlist ty = match ty.raw_ty with TList(_, _) -> true | _ -> false
let is_tevent ty = match ty.raw_ty with TEvent -> true | _ -> false
let is_tabstract name ty = match ty.raw_ty with TAbstract(cid, _) -> Cid.equal cid (Cid.create [name]) | _ -> false
let is_tstring ty = is_tabstract "string" ty
let is_tchar ty = is_tabstract "char" ty
(* value constructors *)
let infer_vty = function 
  | VUnit -> ty TUnit
  | VInt {size; _} -> ty (TInt size)
  | VBool _ -> ty TBool
  | VRecord {labels; es} -> ty (TRecord {labels; ts=List.map (fun v -> v.vty) es})
  | VList(values) -> 
    if (List.length values) == 0 then failwith "cannot infer type of length 0 list" 
    else ty (TList((List.hd values).vty, IConst (List.length values)))
  | VBits {ternary; bits} -> ty (TBits {ternary; len=sz (List.length bits)})
  | VGlobal {global_ty} -> global_ty
  (* | VClosure {params; fexp; _} -> tfun (List.map snd params) fexp.ety *)
  | VEvent _ -> ty (TEvent)
  | VEnum (_, ty) -> ty 
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
let vtup vs = {v=VRecord {labels=None; es=vs}; vty=ttuple (List.map (fun v -> v.vty) vs); vspan=Span.default}
let vpat ints = {v=VBits {ternary=true; bits=ints}; vty=ty (TBits {ternary=true; len=sz (List.length ints)}); vspan=Span.default}
let vbits ints = {v=VBits {ternary=false; bits=ints}; vty=ty (TBits {ternary=false; len=sz (List.length ints)}); vspan=Span.default}
let vrecord labels values = {v=VRecord {labels=Some labels; es=values}; vty=ty (TRecord {labels=Some labels; ts=List.map (fun v -> v.vty) values}); vspan=Span.default}
let vtuple vs = {v=VRecord {labels=None; es=vs}; vty=ty (TRecord {labels=None; ts=List.map (fun v -> v.vty) vs}); vspan=Span.default}
let vglobal global_id global_pos global_ty = {v=VGlobal {global_id; global_pos; global_ty}; vty=global_ty; vspan=Span.default}
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


(* expression constructors *)
let exp e ety espan = {e; ety; espan}
let efunref cid fty = {e=EVar (cid); ety=fty; espan=Span.default; }
let erecord labels es = {e=ERecord {labels=Some labels; es}; ety=trecord labels (List.map (fun (e:exp) -> e.ety) es); espan=Span.default; }
let etuple es = {e=ERecord {labels=None; es}; ety=ttuple (List.map (fun (e:exp) -> e.ety) es); espan=Span.default; }
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
      let labels, ts = match rec_arg.ety.raw_ty with 
        | TRecord {labels=Some(labels); ts} -> labels, ts
        | _ -> failwith "project expects record arg"
      in
      let labels_ts = List.combine labels ts in
      let _, ty = List.find (fun (label, _) -> Id.equal label id) labels_ts in
      ty
    
    | Get(idx) -> 
      let ts = match (List.hd es).ety.raw_ty with 
        | TRecord {labels=None; ts} -> ts
        | _ -> failwith "get expects tuple arg"
      in
      List.nth ts idx
  in 
  {e=EOp (op, es); ety=eop_ty; espan=Span.default; }
let eval value = {e=EVal value; ety=value.vty; espan=Span.default; }
let evar id ty = {e=EVar(id); ety=ty; espan=Span.default; }
let eunit () = eval (vunit ())

let ecall_kind call_kind f es = 
  let ety = match f.ety.raw_ty with 
    | TFun {ret_ty; _} -> ret_ty
    | TEvent -> tevent
    | _ -> failwith "ecall: expected function type"
  in
  {e=ECall {f; args=es; call_kind}; ety; espan=Span.default; }
;;
let ecall = ecall_kind CFun
let eevent = ecall_kind CEvent


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
  | ERecord {labels=None; _} -> true
  | EVal {v=VRecord {labels=None; _}} -> true
  | _ -> false
;;
let erec_form exp = match exp.e with
  | ERecord {labels=Some _; _} -> true
  | EVal {v=VRecord {labels=Some _; _}} -> true
  | _ -> false
;;

exception FormError of string
let flatten_tuple exp = match exp.e with
  | ERecord {labels=None; es} -> es
  | EVal {v=VRecord {labels=None; es}} -> 
    List.map (fun v -> eval v) es
  | _ -> raise (FormError "[flatten_tuple] expected tuple")
;;
let rec flatten_exp exp = match exp.e with
  | ERecord {labels=_; es} -> List.concat (List.map flatten_exp es)
  | _ -> [exp]
;;

let extract_evar exp = match exp.e with
  | EVar id -> id, exp.ety
  | _ -> raise (FormError "[extract_evar] expected EVar")
;;

let extract_func_ty ty = match ty.raw_ty with 
  | TFun {arg_tys; ret_ty; func_kind} -> arg_tys, ret_ty, func_kind
  | _ -> raise (FormError "[extract_func_ty] expected TFun")

let extract_tint_size ty = match ty.raw_ty with 
  | TInt size -> size
  | _ -> raise (FormError "[extract_tint_size] expected TInt")

let extract_trecord ty = match ty.raw_ty with 
  | TRecord {labels=Some labels; ts} -> labels, ts
  | _ -> raise (FormError "[extract_trecord] expected TRecord")
;;
let extract_ttuple ty = match ty.raw_ty with 
  | TRecord {labels=None; ts} -> ts
  | _ -> raise (FormError "[extract_ttuple] expected TRecord")

let extract_tlist ty = match ty.raw_ty with 
  | TList(ty, len) -> ty, len
  | _ -> raise (FormError "[extract_tlist] expected TList")
(* let egen loc ev = ecall (efunref (Cid.create ["generate"]) (tfun [tevent] (tunit ()))) [loc; ev] *)


(* let emultiassign ids tys new_vars rhs_exp = exp (EAssign {ids; tys; new_vars; exp=rhs_exp}) (ty TUnit) Span.default *)
(* let elocal id ty exp = emultiassign [id] [ty] true exp *)
(* let eassign id exp = emultiassign [id] [exp.ety] false exp *)
(* let eif cond exp_then exp_else = exp (EIf(cond, exp_then, exp_else)) exp_then.ety Span.default *)
(* let ematch match_exp branches = exp (EMatch(match_exp, branches)) (List.hd branches |> snd).ety Span.default *)
(* let eseq exp1 exp2 = exp (ESeq(exp1, exp2)) exp2.ety (Span.extend exp1.espan exp2.espan) *)
(* let eret eret = exp (EReturn eret) (tunit ()) Span.default *)
let ewrap espan exp = {exp with espan}

(* statements *)
let s s sspan = {s; sspan;}
let smultiassign ids tys new_vars rhs_exp = s (SAssign {ids; tys; new_vars; exp=rhs_exp}) Span.default
let slocal id ty exp = smultiassign [id] [ty] true exp
let sassign id exp = smultiassign [id] [exp.ety] false exp
let sif cond s_then s_else = s (SIf(cond, s_then, s_else)) Span.default
let smatch match_exp branches = s (SMatch(match_exp, branches)) Span.default
let sseq s1 s2 = 
  let span = try Span.extend s1.sspan s2.sspan with _ -> Span.default in
  s (SSeq(s1, s2)) span
let snoop = s SNoop Span.default
let sunit exp = s (SUnit exp) Span.default
let sret_none = s (SRet None) Span.default
let sret eret = s (SRet (Some eret)) Span.default
let swrap sspan s = {s with sspan}

(* declarations *)

(* declarations *)
let decl d dspan = {d; dspan;}
(* different kinds of first-order functions *)
let dfun_kind fun_kind id rty params body = 
  decl (DFun(fun_kind, id, rty, params, Some body)) Span.default
(* let dfun_kind func_kind id (params:params) body = 
  let fun_ty = tfun_kind (List.map snd params) body.ety func_kind in  
  decl (DFun(id, params, Some body)) Span.default *)
  (* decl (DVar(id, fun_ty, Some body)) Span.default *)
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

(* event declarations *)
let devent id evconstrnum params is_parsed = decl (DEvent {evconstrid=id; evconstrnum; evparams=params; is_parsed}) Span.default


(* helpers *)
let untuple exp = match exp.e with
  | ERecord {labels=None; es} -> es
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

