(* simpler functional IR for lucid, with extensions
   for compatability with the current CoreSyntax IR
   (Extensions should be eliminated before any further processing) *)
type sp = Span.t
type id = Id.t
type cid = Cid.t
type size = SConst of int | SPlatformDependent
type pragma = Pragma.t

(* tag to indicate types in other IRs *)
type ty_ext = ..

type op =  | And | Or | Not
          | Eq  | Neq | Less| More  | Leq | Geq
          | Neg | Plus| Sub | SatPlus | SatSub
          | BitAnd  | BitOr | BitXor | BitNot | LShift | RShift
          | Slice of int * int
          | PatExact | PatMask
          | Hash of size
          | Cast of size 
          | Conc

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
and ty = {raw_ty:raw_ty; tspan : sp; ty_ext : ty_ext option;}

type params = (id * ty) list
type func = { fun_args: id list; fun_body: exp; }
and v =
  | VUnit
  | VInt of int
  | VBool of bool
  | VRecord of {labels : id list option; es : value list;}
  | VBits of {ternary: bool; bits : int list;}
  | VGlobal of id * int (* global variable reference, int is an address *)
  | VClosure of {env : (id * value) list; args: id list; fexp : exp;}
  | VEvent of {event_id : id; event_arg: value list;}
and value = {v:v; vty:ty; vspan : sp;}
and pat = 
  | PVal of value
  | PEvent of {event_id : id; params : params;}
and branch = pat list * exp
and e = 
  | EVal of value
  | EVar of id
  | ERecord of {labels : id list option; es : exp list;}
  | ECall of exp * exp list
  | EOp of op * exp list 
  | EEvent of {event_id : id; args : exp list;}
  | EClosure of {env : (id * exp) list; args: id list; fexp : exp;}
  | ELet of id list * exp * exp
  | ESeq of exp * exp
  | EIf of exp * exp * exp
  | EMatch of exp * branch list
and exp = {e:e; ety:ty; espan : sp;}

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
type ty_ext += TGroup
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
let ty raw_ty = {raw_ty=raw_ty; tspan=Span.default; ty_ext=None}
let ty_ext raw_ty ty_ext = {raw_ty=raw_ty; tspan=Span.default; ty_ext=Some ty_ext}

let ttuple tys = ty (TRecord {labels=None; ts=tys})
let tfun arg_tys ret_ty func_kind = ty (TFun {arg_tys; ret_ty; func_kind})
