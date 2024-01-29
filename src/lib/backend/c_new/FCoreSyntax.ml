(* simpler functional IR for lucid, with extensions
   for compatability with the current CoreSyntax IR
   (Extensions should be eliminated before any further processing) *)
type sp = Span.t
type id = Id.t
type cid = Cid.t
type pragma = Pragma.t

type size = int
type func_kind = | FNormal | FHandler | FParser | FAction | FMemop | FExtern

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
  | TPrimitive of cid * (ty list) * bool 
    (* A primitive type whose value 
        depends on the given types, but is 
        represented in a backend-dependent way. 
       It could be something from the surface language, 
        like "Table.t<...>", or something for a backend IR, 
        like "Union" or "Ref".
      - The bool is vestigal, it represents whether the type 
        is "global" in CoreSyntax. I don't think that matters 
        anymore at the point of CoreSyntax or this IR, but 
        not positive yet.
      bool indicates that it was a global in the surface syntax. *)
  | TBits of {ternary: bool; len : size;}
  | TEvent
and func_ty = {arg_tys : ty list; ret_ty : ty; func_kind : func_kind;}
and ty = {raw_ty:raw_ty; tspan : sp;}

type params = (id * ty) list

and v =
  | VUnit
  | VInt of {value : int; size : size;}
  | VBool of bool
  | VRecord of {labels : id list option; es : value list;}
  | VClosure of {env : (id * value) list; params: params; fexp : exp;}
  | VTyRef of id * int * ty (* typed reference to a value memory, e.g., a global *)
  | VBits of {ternary: bool; bits : int list;}
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
  | ECall of {f:exp; args:exp list; call_kind:call_kind;}
  | EOp of op * exp list
  | EClosure of {env : (id * exp) list; params: params; fexp : exp;}
  (* for a functional ast, add these *)
  (* | ELet of cid * exp * exp *)
  (* | EIf of exp * exp * exp *)
  (* | EMatch of exp * branch list *)
  (* | ESeq of exp * exp *)
and call_kind = 
  | CNormal
  | CUnordered
  | CEvent 
and exp = {e:e; ety:ty; espan : sp;}

and s = 
  | SNoop
  | SUnit of exp
  | SAssign of {ids : cid list; tys : ty list; new_vars : bool; exp : exp}
  | SIf of exp * statement * statement
  | SMatch of exp * branch list
  | SSeq of statement * statement
  | SRet of exp option

and statement = {s:s; sspan : sp;}

type event_def = {evid : id; evnum : int option; evparams : params; is_parsed : bool}
type d = 
  | DVar of id * ty * (exp option) (* constants and globals, possibly externs *)
  | DFun of func_kind * id * ty * params * (statement option) (* first-order functions, possibly extern. ty is return type. *)
  | DTy  of cid * ty option (* declare named types, which may be external *)
  | DEvent of event_def (* declare an event, which is a constructor for the datatype TEvent *)

type fdecl = {d:d; dspan : sp;}
type fdecls = fdecl list



(* constructors *)
(* type constructors *)
let sz n = n
let ty raw_ty = {raw_ty=raw_ty; tspan=Span.default; }

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
let tgroup_cid = Cid.create ["Group"]
let tgroup = TPrimitive(tgroup_cid, [], false)

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
  | VTyRef (_, _, ty) -> ty
  | VClosure {params; fexp; _} -> tfun (List.map snd params) fexp.ety
  | VEvent _ -> ty (TEvent)
;;  

let value v = {v=v; vty=infer_vty v; vspan=Span.default}
let vunit () = {v=VUnit; vty=ty TUnit; vspan=Span.default}
let vint value size = {v=VInt {value; size = sz size}; vty=ty (TInt(sz size)); vspan=Span.default}
(* declare a vint with size derived from ty *)
let vint_ty value ty = match ty.raw_ty with 
  | TInt (size) -> vint value size
  | _ -> failwith "vint_ty: expected TInt"
let vbool b = {v=VBool b; vty=ty TBool; vspan=Span.default}
let vtup vs = {v=VRecord {labels=None; es=vs}; vty=ttuple (List.map (fun v -> v.vty) vs); vspan=Span.default}
let vpat ints = {v=VBits {ternary=true; bits=ints}; vty=ty (TBits {ternary=true; len=sz (List.length ints)}); vspan=Span.default}
let vbits ints = {v=VBits {ternary=false; bits=ints}; vty=ty (TBits {ternary=false; len=sz (List.length ints)}); vspan=Span.default}
let vrecord labels values = {v=VRecord {labels=Some labels; es=values}; vty=ty (TRecord {labels=Some labels; ts=List.map (fun v -> v.vty) values}); vspan=Span.default}
let vtuple vs = {v=VRecord {labels=None; es=vs}; vty=ty (TRecord {labels=None; ts=List.map (fun v -> v.vty) vs}); vspan=Span.default}

let vglobal cid addr ty = {v=VTyRef (cid, addr, ty); vty=ty; vspan=Span.default}
let string_to_value (s:string) =
  let chars = List.of_seq (String.to_seq s) in
  let vchars = List.map (fun c -> vint (Char.code c) 8) chars in
  let vstr = vtup vchars in
  {vstr with vty=vstr.vty}
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
let exp e ety espan = {e; ety; espan}
let efunref cid fty = {e=EVar (cid, false); ety=fty; espan=Span.default; }
let erecord labels es = {e=ERecord {labels=Some labels; es}; ety=trecord labels (List.map (fun (e:exp) -> e.ety) es); espan=Span.default; }
let etuple es = {e=ERecord {labels=None; es}; ety=ttuple (List.map (fun (e:exp) -> e.ety) es); espan=Span.default; }
let eop op es = {e=EOp (op, es); ety=ty TBool; espan=Span.default; }
let eval value = {e=EVal value; ety=value.vty; espan=Span.default; }
let evar mut id ty = {e=EVar(id, mut); ety=ty; espan=Span.default; }
let local_var id ty = evar true id ty
let const_var id ty = evar false id ty
let eunit () = eval (vunit ())

let ecall_kind call_kind f es = 
  let ety = match f.ety.raw_ty with 
    | TFun {ret_ty; _} -> ret_ty
    | TEvent -> tevent
    | _ -> failwith "ecall: expected function type"
  in
  {e=ECall {f; args=es; call_kind}; ety; espan=Span.default; }
;;
let ecall = ecall_kind CNormal
let eevent = ecall_kind CEvent
let ecall_unordered = ecall_kind CUnordered
let efun_kind func_kind params fexp = {e=EClosure {env=[]; params; fexp}; ety=tfun_kind (List.map snd params) fexp.ety func_kind; espan=Span.default; }
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
