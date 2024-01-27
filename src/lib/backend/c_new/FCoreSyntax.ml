(* simpler functional IR for lucid, with extensions
   for compatability with the current CoreSyntax IR
   (Extensions should be eliminated before any further processing) *)
type sp = Span.t
type id = Id.t
type cid = Cid.t
type size = SConst of int | SPlatformDependent
type pragma = Pragma.t

type annot = 
  | ATag of string 
  | ATags of string list
let annot s = ATag s
let has_annot annots s = List.exists (fun a -> a = ATag s) annots

type func_kind = .. (* function kinds *)
type func_kind += | FNormal | FHandler | FParser | FAction | FMemop | FExtern
type d_ext = ..     (* additional declarations *)

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
  | TName of cid (* a type reference *)
  | TPrimitive of cid * (ty list) * bool (* a primitive type that takes some arguments. 
     It could be "Table.t<...>", or something lower-level, 
     like "Union" or "Closure"      
     - bool indicates that it was a global in the surface syntax.
     *)
  | TBits of {ternary: bool; len : size;}
  | TEvent
and func_ty = {arg_tys : ty list; ret_ty : ty; func_kind : func_kind;}
and ty = {raw_ty:raw_ty; tspan : sp; ty_annot : annot list;}

type params = (id * ty) list
(* convention: it should always possible to infer 
   the type of a value from the value itself *)
and v =
  | VUnit
  | VInt of {value : int; size : size;}
  | VBool of bool
  | VRecord of {labels : id list option; es : value list;}
  | VBits of {ternary: bool; bits : int list;}
  | VGlobal of id * int * ty (* typed reference to a global variable *)
  | VClosure of {env : (id * value) list; params: params; fexp : exp;}
  | VEvent of vevent
and vevent = {evid : cid; evnum : value option; evdata: value list; meta : (string * value) list;}
and value = {v:v; vty:ty; vspan : sp;}

and pat = 
  | PVal of value
  | PEvent of {event_id : cid; params : params;}
and branch = pat list * branch_tgt
and branch_tgt = 
  | S of statement
  | E of exp
and e = 
  | EVal of value
  | EVar of cid * bool (* true if mutable *)
  | ERecord of {labels : id list option; es : exp list;}
  | ECall of exp * exp list * bool
  | EOp of op * exp list 
  | EEvent of {event_id : id; args : exp list;}
  | EClosure of {env : (id * exp) list; params: params; fexp : exp;}
  (* for a functional ast, add these *)
  (* | ELet of cid * exp * exp *)
  (* | EIf of exp * exp * exp *)
  (* | EMatch of exp * branch list *)
  (* | ESeq of exp * exp *)

and exp = {e:e; ety:ty; espan : sp; exp_annot : annot list;}

and s = 
  | SNoop
  | SUnit of exp
  | SAssign of {ids : cid list; tys : ty list; new_vars : bool; exp : exp}
  | SIf of exp * statement * statement
  | SMatch of exp * branch list
  | SSeq of statement * statement
  | SRet of exp option

and statement = {s:s; sspan : sp; s_annot : annot list;}



type event_def = {evid : id; evnum : int option; evparams : params; is_parsed : bool}
type d = 
  | DVar of id * ty * (exp option) (* constants and globals, possibly externs *)
  | DFun of func_kind * id * ty * params * (statement option) (* first-order functions, possibly extern. ty is return type. *)
  | DTy  of cid * ty option (* declare named types, which may be external *)
  | DEvent of event_def (* declare an event, which is a constructor for the datatype TEvent *)
  | DExt of d_ext         (* a declaration from an extended AST*)


type fdecl = {d:d; dspan : sp; d_annot : annot list;}
type fdecls = fdecl list



(*
  construct a global: exp is a call to a builtin constructor
    let (myarr : Array.t<int>) = Array.create(...);
  declare a function or handler: exp is an EClosure (i.e., a function definition)
    let (foo : TFun<handler, (int, int -> ())) = fun arg1 arg2 -> foo_body;
  declare an extern variable or function: no exp
    let (extern_do_foo : TFun<function, int -> int);)
  declare an event: exp 
  let (foo : event) = ???
  
  declare a type: 
    type some_struct = {x : int; y : int};

  
*)

(* constructors *)
(* type constructors *)
let sz n = SConst n
let sz_platform = SPlatformDependent
let ty raw_ty = {raw_ty=raw_ty; tspan=Span.default; ty_annot=[]}
let ty_annot raw_ty ty_annot = {raw_ty=raw_ty; tspan=Span.default; ty_annot=[ty_annot]}

let tunit () = ty TUnit
let tevent = ty TEvent
let trecord labels tys = ty (TRecord {labels=Some labels; ts=tys})
let ttuple tys = ty (TRecord {labels=None; ts=tys})
let tfun_kind arg_tys ret_ty func_kind = ty (TFun {arg_tys; ret_ty; func_kind})
let tfun arg_tys ret_ty = tfun_kind arg_tys ret_ty FNormal
(* global type *)
let tglobal cid global_tyargs b = ty (TPrimitive(cid, global_tyargs, b))
(* named type *)
let tname cid = ty (TName cid)

(* test to see if an ast node represents one of the functions defined above *)
  let is_tunit ty = match ty.raw_ty with TUnit -> true | _ -> false
  let is_trecord ty = match ty.raw_ty with TRecord{labels=Some(_)} -> true | _ -> false
  let is_ttuple ty = match ty.raw_ty with TRecord{labels=None} -> true | _ -> false
  let is_tfun ty = match ty.raw_ty with TFun({func_kind=FNormal}) -> true | _ -> false



(* value constructors *)
let infer_vty = function 
  | VUnit -> ty TUnit
  | VInt {size; _} -> ty (TInt size)
  | VBool _ -> ty TBool
  | VRecord {labels; es} -> ty (TRecord {labels; ts=List.map (fun v -> v.vty) es})
  | VBits {ternary; bits} -> ty (TBits {ternary; len=sz (List.length bits)})
  | VGlobal (_, _, ty) -> ty
  | VClosure {params; fexp; _} -> tfun (List.map snd params) fexp.ety
  | VEvent _ -> ty (TEvent)
;;  

let value v = {v=v; vty=infer_vty v; vspan=Span.default}
let vint_unsized value = {v=VInt {value; size = sz_platform}; vty=ty (TInt sz_platform); vspan=Span.default}
let vunit () = {v=VUnit; vty=ty TUnit; vspan=Span.default}
let vint value size = {v=VInt {value; size = sz size}; vty=ty (TInt(sz size)); vspan=Span.default}
(* declare a vint with size derived from ty *)
let vint_ty value ty = match ty.raw_ty with 
  | TInt (SConst size) -> vint value size
  | _ -> failwith "vint_ty: expected TInt"
let vbool b = {v=VBool b; vty=ty TBool; vspan=Span.default}
let vtup vs = {v=VRecord {labels=None; es=vs}; vty=ttuple (List.map (fun v -> v.vty) vs); vspan=Span.default}
let vpat ints = {v=VBits {ternary=true; bits=ints}; vty=ty (TBits {ternary=true; len=sz (List.length ints)}); vspan=Span.default}
let vbits ints = {v=VBits {ternary=false; bits=ints}; vty=ty (TBits {ternary=false; len=sz (List.length ints)}); vspan=Span.default}
let vrecord labels values = {v=VRecord {labels=Some labels; es=values}; vty=ty (TRecord {labels=Some labels; ts=List.map (fun v -> v.vty) values}); vspan=Span.default}
let vtuple vs = {v=VRecord {labels=None; es=vs}; vty=ty (TRecord {labels=None; ts=List.map (fun v -> v.vty) vs}); vspan=Span.default}

let vglobal cid addr ty = {v=VGlobal (cid, addr, ty); vty=ty; vspan=Span.default}
let string_to_value (s:string) =
  let chars = List.of_seq (String.to_seq s) in
  let vchars = List.map (fun c -> vint (Char.code c) 8) chars in
  let vstr = vtup vchars in
  {vstr with vty={vstr.vty with ty_annot=[];}}
;;
let value_to_string (v:value) =
  match v.v with
  | VRecord {labels=None; es} ->
    let charints = List.map 
      (fun v -> 
        match v.v with 
          | VInt({value}) -> value
          | _ -> failwith "strings are encoded as int tuples") 
      es 
    in
    let chars = List.map (fun i -> Char.chr i) charints in
    String.of_seq (List.to_seq chars)
  | _ -> failwith "strings are encoded as int tuples"
;;


(* expression constructors *)
let exp e ety espan = {e; ety; espan; exp_annot=[]}
let efunref cid fty = {e=EVar (cid, false); ety=fty; espan=Span.default; exp_annot=[]}
let erecord labels es = {e=ERecord {labels=Some labels; es}; ety=trecord labels (List.map (fun (e:exp) -> e.ety) es); espan=Span.default; exp_annot=[]}
let etuple es = {e=ERecord {labels=None; es}; ety=ttuple (List.map (fun (e:exp) -> e.ety) es); espan=Span.default; exp_annot=[]}
let eop op es = {e=EOp (op, es); ety=ty TBool; espan=Span.default; exp_annot=[]}
let eval value = {e=EVal value; ety=value.vty; espan=Span.default; exp_annot=[]}
let evar mut id ty = {e=EVar(id, mut); ety=ty; espan=Span.default; exp_annot=[]}
let local_var id ty = evar true id ty
let const_var id ty = evar false id ty
let eunit () = eval (vunit ())
let ecall f es unordered_flag = 
  let rty = match f.ety.raw_ty with 
    | TFun {ret_ty; _} -> ret_ty
    | _ -> failwith "ecall: expected function type"
  in
  {e=ECall (f, es, unordered_flag); ety=rty; espan=Span.default; exp_annot=[]}
let efun_kind func_kind params fexp = {e=EClosure {env=[]; params; fexp}; ety=tfun_kind (List.map snd params) fexp.ety func_kind; espan=Span.default; exp_annot=[]}
let efun = efun_kind FNormal
let eaction = efun_kind FAction
(* let elet id ety exp_rhs exp_rest espan = exp (ELet([id], exp_rhs, exp_rest)) ety espan *)

let egen_self ev = 
  let gen_ty = tfun [tevent] (tunit ()) in
  ecall (efunref (Cid.create ["generate_self"]) gen_ty) [ev]
let egen_switch loc ev = 
  ecall (efunref (Cid.create ["generate_switch"]) (tfun [tevent] (tunit ()))) [loc; ev]
;;
let egen_group loc ev = 
  ecall (efunref (Cid.create ["generate_group"]) (tfun [tevent] (tunit ()))) [loc; ev]
;;
let egen_port loc ev = 
  ecall (efunref (Cid.create ["generate_port"]) (tfun [tevent] (tunit ()))) [loc; ev]

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
let s s sspan = {s; sspan; s_annot=[]}
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
let decl d dspan = {d; dspan; d_annot=[]}
let annot_decl decl d_annot = {decl with d_annot=[d_annot]}
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
let daction = dfun_kind FAction (* note: actions are higher-order -- returned from action constructors *)
let dmemop = dfun_kind FMemop
(* global variables *)
let dglobal id ty exp = decl (DVar(id, ty, Some exp)) Span.default
let dextern id ty = decl (DVar(id, ty, None)) Span.default

(* type declarations *)
let dty tycid ty = decl (DTy(tycid, Some ty)) Span.default
let dty_ext tycid = decl (DTy(tycid, None)) Span.default

(* event declarations *)
let devent id evnum params is_parsed = decl (DEvent {evid=id; evnum; evparams=params; is_parsed}) Span.default


(* traversal utils *)


(* let rec exp_map (map_f : exp -> exp) exp =  *)
