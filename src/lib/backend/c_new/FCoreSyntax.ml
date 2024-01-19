(* simpler functional IR for lucid, with extensions
   for compatability with the current CoreSyntax IR
   (Extensions should be eliminated before any further processing) *)
type sp = Span.t
type id = Id.t
type cid = Cid.t
type size = SConst of int | SPlatformDependent
type pragma = Pragma.t

(* annotations (not part of core IR) *)
type ty_annot = ..
type exp_annot = ..

type op =  | And | Or | Not
          | Eq  | Neq | Less| More  | Leq | Geq
          | Neg | Plus| Sub | SatPlus | SatSub
          | BitAnd  | BitOr | BitXor | BitNot | LShift | RShift
          | Slice of int * int
          | PatExact | PatMask
          | Hash of size
          | Cast of size 
          | Conc
          | Project of id | Get of int (* record and tuple ops *)

type raw_ty = 
  | TUnit
  | TInt of size 
  | TBool 
  | TRecord of {labels : id list option; ts : ty list;}
  | TFun of func_ty
  | TName of cid * ty list * bool
  | TBits of {ternary: bool; len : size;}
  | TEvent
and func_ty = {arg_tys : ty list; ret_ty : ty; func_kind : func_kind;}
and func_kind = FNormal | FHandler | FParser | FAction | FMemop
and ty = {raw_ty:raw_ty; tspan : sp; ty_annot : ty_annot option;}

type params = (id * ty) list
type func = { fun_args: id list; fun_body: exp; }
(* convention: it should always possible to infer 
   the type of a value from the value itself *)
and v =
  | VUnit
  | VInt of {value : int; size : size;}
  | VBool of bool
  | VRecord of {labels : id list option; es : value list;}
  | VBits of {ternary: bool; bits : int list;}
  | VTyRef of id * int * ty (* typed reference to a global variable *)
  | VClosure of {env : (id * value) list; params: params; fexp : exp;}
  | VEvent of vevent
and vevent = {evid : cid; evnum : int option; evdata: value list; meta : (string * value) list;}
and value = {v:v; vty:ty; vspan : sp;}

and pat = 
  | PVal of value
  | PEvent of {event_id : id; params : params;}
and branch = pat list * exp
and e = 
  | EVal of value
  | EVar of cid
  | ERecord of {labels : id list option; es : exp list;}
  | ECall of exp * exp list
  | EOp of op * exp list 
  | EEvent of {event_id : id; args : exp list;}
  | EClosure of {env : (id * exp) list; args: id list; fexp : exp;}
  | ELet of id list * exp * exp
  | ESeq of exp * exp
  | EIf of exp * exp * exp
  | EMatch of exp * branch list
and exp = {e:e; ety:ty; espan : sp; exp_annot : exp_annot option;}

type global_def = {eid : id; ety : ty; econstr : exp option;}
type event_def = {evid : id; evnum : int option; evparams : params; is_parsed : bool}
type func_def = {
  fid : id;
  fkind : func_kind;
  fparams : params;
  fret_ty : ty;
  fbody : exp;
}
type ty_def = {tid : id; tty : ty;}
type d = ..
type d += DGlobal of global_def
type d += DEvent of event_def  
type d += DFun of func_def
type d += DUserTy of ty_def
type fdecl = {d:d; dspan : sp;}
type fdecls = fdecl list

(* extensions for translation to / from CoreIr *)
type ty_annot += TGroup
type exp_annot += ECallUnordered of bool
(* Statements and handlers with statement bodies are only for translation to / from the IR *)
type s = 
  | SNoop
  | SUnit of exp
  | SAssign of assign
  | SSeq of statement * statement
  | SIf of exp * statement * statement
  | SMatch of exp * sbranch list
and assign = {ids : id list; tys : ty list; new_vars : bool; exp : exp}
and sbranch = pat list * statement
and statement = {s:s; sspan : sp; spragmas : pragma list;}
type d += DHandler of {hdl_id : id; hdl_params: id list; hdl_body : statement;}
type decl = {d:d; dspan : sp; dpragma : pragma option}
type decls = decl list


(* constructors *)
let sz n = SConst n
let sz_platform = SPlatformDependent
let ty raw_ty = {raw_ty=raw_ty; tspan=Span.default; ty_annot=None}
let ty_annot raw_ty ty_annot = {raw_ty=raw_ty; tspan=Span.default; ty_annot=Some ty_annot}

let trecord labels tys = ty (TRecord {labels=Some labels; ts=tys})
let ttuple tys = ty (TRecord {labels=None; ts=tys})
let tfun arg_tys ret_ty func_kind = ty (TFun {arg_tys; ret_ty; func_kind})

let infer_vty = function 
  | VUnit -> ty TUnit
  | VInt {size; _} -> ty (TInt size)
  | VBool _ -> ty TBool
  | VRecord {labels; es} -> ty (TRecord {labels; ts=List.map (fun v -> v.vty) es})
  | VBits {ternary; bits} -> ty (TBits {ternary; len=sz (List.length bits)})
  | VTyRef (_, _, ty) -> ty
  | VClosure {params; fexp; _} -> tfun (List.map snd params) fexp.ety FNormal
  | VEvent _ -> ty (TEvent)
;;  

let value v = {v=v; vty=infer_vty v; vspan=Span.default}
let vint_unsized value = {v=VInt {value; size = sz_platform}; vty=ty (TInt sz_platform); vspan=Span.default}
let vint value size = {v=VInt {value; size = sz size}; vty=ty (TInt(sz size)); vspan=Span.default}
let vbool b = {v=VBool b; vty=ty TBool; vspan=Span.default}
let vtup vs = {v=VRecord {labels=None; es=vs}; vty=ttuple (List.map (fun v -> v.vty) vs); vspan=Span.default}
let vpat ints = {v=VBits {ternary=true; bits=ints}; vty=ty (TBits {ternary=true; len=sz (List.length ints)}); vspan=Span.default}
let vbits ints = {v=VBits {ternary=false; bits=ints}; vty=ty (TBits {ternary=false; len=sz (List.length ints)}); vspan=Span.default}
let vrecord labels values = {v=VRecord {labels=Some labels; es=values}; vty=ty (TRecord {labels=Some labels; ts=List.map (fun v -> v.vty) values}); vspan=Span.default}
let vtuple vs = {v=VRecord {labels=None; es=vs}; vty=ty (TRecord {labels=None; ts=List.map (fun v -> v.vty) vs}); vspan=Span.default}


(* expressions *)
let efunref cid fty = {e=EVar cid; ety=fty; espan=Span.default; exp_annot=None}
let erecord labels es = {e=ERecord {labels=Some labels; es}; ety=trecord labels (List.map (fun (e:exp) -> e.ety) es); espan=Span.default; exp_annot=None}
let etuple es = {e=ERecord {labels=None; es}; ety=ttuple (List.map (fun (e:exp) -> e.ety) es); espan=Span.default; exp_annot=None}

let eop op es = {e=EOp (op, es); ety=ty TBool; espan=Span.default; exp_annot=None}