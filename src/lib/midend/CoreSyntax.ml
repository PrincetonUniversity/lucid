(* Abstract core syntax of Lucid -- this is the part that remains after all the
   frontend transformations have eliminated the rest of it. *)
open Batteries

type id = [%import: (Id.t[@opaque])]

and cid = [%import: (Cid.t[@opqaue])]

and sp = [%import: Span.t]

and z = [%import: (Z.t[@opaque])]

and zint = [%import: (Integer.t[@with Z.t := (Z.t [@opaque])])]

and location = int

(* All sizes should be inlined and precomputed *)
and size = int

and sizes = size list

and raw_ty =
  | TBool
  | TGroup
  | TInt of size (* Number of bits *)
  | TEvent
  | TFun of func_ty (* Only used for Array/event functions at this point *)
  | TName of cid * sizes * bool (* Named type: e.g. "Array.t<<32>>". Bool is true if it represents a global type *)
  | TMemop of size * size

(* Don't need effects or constraints since we passed typechecking ages ago *)
and func_ty =
  { arg_tys : tys
  ; ret_ty : ty
  }

and ty =
  { raw_ty : raw_ty
  ; tspan : sp
  }

and tys = ty list

and op =
  | And
  | Or
  | Not
  | Eq
  | Neq
  | Less
  | More
  | Leq
  | Geq
  | Neg
  | Plus
  | Sub
  | SatPlus
  | SatSub
  | Cast of size
  | Conc
  | BitAnd
  | BitOr
  | BitXor
  | BitNot
  | LShift
  | RShift
  | Slice of int * int

and pat =
  | PWild
  | PNum of z
  | PBit of int list

(* values *)
and v =
  | VBool of bool
  | VInt of zint
  | VEvent of event
  | VGlobal of int (* Stage number *)
  | VGroup of location list

and event =
  { eid : cid
  ; data : value list
  ; edelay : int
  }

and value =
  { v : v
  ; vty : ty
  ; vspan : sp
  }

(* expressions *)
and e =
  | EVal of value
  | EVar of cid
  | EOp of op * exp list
  | ECall of cid * exp list
  | EHash of size * exp list
  | EFlood of exp

and exp =
  { e : e
  ; ety : ty
  ; espan : sp
  }

and branch = pat list * statement

and gen_type =
  | GSingle of exp option
  | GMulti of exp
  | GPort of exp

(* statements *)
and s =
  | SNoop
  | SUnit of exp
  | SLocal of id * ty * exp
  | SAssign of id * exp
  | SPrintf of string * exp list
  | SIf of exp * statement * statement
  | SGen of gen_type * exp
  | SSeq of statement * statement
  | SMatch of exp list * branch list
  | SRet of exp option

and statement =
  { s : s
  ; sspan : sp
  }

and params = (id * ty) list

and body = params * statement

and event_sort =
  | EEntry of bool (* true iff "control", i.e. it can generate non-continue events *)
  | EExit
  | EBackground

(* declarations *)
and d =
  | DGlobal of id * ty * exp
  | DEvent of id * event_sort * params
  | DHandler of id * body
  | DMemop of id * body
  | DExtern of id * ty

(* name, return type, args & body *)
and decl =
  { d : d
  ; dspan : sp
  }

(* a program is a list of declarations *)
and decls = decl list
[@@deriving
  visitors
    { name = "s_iter"
    ; variety = "iter"
    ; polymorphic = false
    ; data = true
    ; concrete = true
    ; nude = false
    }
  , visitors
      { name = "s_map"
      ; variety = "map"
      ; polymorphic = false
      ; data = true
      ; concrete = true
      ; nude = false
      }]

(********************************)
(* Constructors and Destructors *)
(********************************)

exception Error of string

let error s = raise (Error s)

(* Types *)
let ty_sp raw_ty tspan = { raw_ty; tspan }
let ty raw_ty = { raw_ty; tspan = Span.default }

let infer_vty v =
  match v with
  | VBool _ -> TBool
  | VInt z -> TInt (Integer.size z)
  | VEvent _ -> TEvent
  | VGroup _ -> TGroup
  | VGlobal _ -> failwith "Cannot infer type of global value"
;;

(* Values *)
let avalue v vty vspan = { v; vty; vspan }
let value_sp v vspan = { v; vty = infer_vty v |> ty; vspan }
let value v = { v; vty = infer_vty v |> ty; vspan = Span.default }
let vint i size = value (VInt (Integer.create i size))
let vinteger i = value (VInt i)
let vbool b = value (VBool b)
let default_vint size = value (VInt (Integer.create 0 size))
let default_vbool = value (VBool false)
let vint_sp i span = value_sp (VInt i) span
let vbool_sp b span = value_sp (VBool b) span
let vevent event = value (VEvent event)
let vevent_sp event span = value_sp (VEvent event) span
let vglobal idx ty = avalue (VGlobal idx) ty Span.default
let vgroup locs = value (VGroup locs)

(* Expressions *)
let exp e ety = { e; ety; espan = Span.default }
let aexp e ety espan = { e; ety; espan }
let value_to_exp v = aexp (EVal v) v.vty v.vspan
let var_sp cid ety span = aexp (EVar cid) ety span
let op_sp op args ety span = aexp (EOp (op, args)) ety span
let call_sp cid args ety span = aexp (ECall (cid, args)) ety span
let hash_sp size args ety span = aexp (EHash (size, args)) ety span

(* Statements *)

let statement s = { s; sspan = Span.default }
let statement_sp s span = { s; sspan = span }
let snoop = statement SNoop
let sseq s1 s2 = statement (SSeq (s1, s2))
let slocal id ty e = statement (SLocal (id, ty, e))
let sassign id e = statement (SAssign (id, e))
let sprintf s es = statement (SPrintf (s, es))
let sprintf_sp s es span = statement_sp (SPrintf (s, es)) span
let sifte e s1 s2 = statement (SIf (e, s1, s2))
let snoop_sp span = statement_sp SNoop span
let slocal_sp id ty e span = statement_sp (SLocal (id, ty, e)) span
let sassign_sp id e span = statement_sp (SAssign (id, e)) span
let sseq_sp s1 s2 span = statement_sp (SSeq (s1, s2)) span
let sifte_sp e s1 s2 span = statement_sp (SIf (e, s1, s2)) span
let gen_sp b e span = statement_sp (SGen (b, e)) span

let scall_sp cid args rty span =
  statement_sp (SUnit (call_sp cid args rty span)) span
;;

let match_sp es bs span = statement_sp (SMatch (es, bs)) span
let sexp_sp e span = statement_sp (SUnit e) span

(* Declarations *)
let decl d = { d; dspan = Span.default }
let decl_sp d span = { d; dspan = span }
let dglobal_sp id ty exp span = decl_sp (DGlobal (id, ty, exp)) span
let dextern_sp id ty span = decl_sp (DExtern (id, ty)) span
let handler_sp id p body span = decl_sp (DHandler (id, (p, body))) span
let memop_sp id p body span = decl_sp (DMemop (id, (p, body))) span

(*** Utility -- may split into a separate file if it gets big *)

let equiv_list f lst1 lst2 =
  try List.for_all2 f lst1 lst2 with
  | Invalid_argument _ -> false
;;

let rec equiv_value v1 v2 =
  match v1.v, v2.v with
  | VBool b1, VBool b2 -> b1 = b2
  | VInt n1, VInt n2 -> Integer.equal n1 n2
  | VGlobal n1, VGlobal n2 -> n1 = n2
  | VGroup locs1, VGroup locs2 -> locs1 = locs2
  | VEvent e1, VEvent e2 ->
    Cid.equal e1.eid e2.eid
    && e1.edelay = e2.edelay
    && equiv_list equiv_value e1.data e2.data
  | _ -> false
;;

let rec equiv_exp e1 e2 =
  match e1.e, e2.e with
  | EVal v1, EVal v2 -> equiv_value v1 v2
  | EVar cid1, EVar cid2 -> Cid.equal cid1 cid2
  | ECall (cid1, es1), ECall (cid2, es2) ->
    Cid.equal cid1 cid2 && equiv_list equiv_exp es1 es2
  | EHash (sz1, es1), EHash (sz2, es2) ->
    sz1 = sz2 && equiv_list equiv_exp es1 es2
  | EOp (op1, es1), EOp (op2, es2) -> op1 = op2 && equiv_list equiv_exp es1 es2
  | _ -> false
;;
