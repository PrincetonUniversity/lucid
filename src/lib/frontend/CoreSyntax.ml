(* Abstract core syntax of Lucid -- this is the part that remains after all the
   frontend transformations have eliminated the rest of it. *)
open Batteries

type id = [%import: (Id.t[@opaque])]

and cid = [%import: (Cid.t[@opqaue])]

and sp = [%import: Span.t]

and z = [%import: (Z.t[@opaque])]

and zint = [%import: (Integer.t[@with Z.t := (Z.t [@opaque])])]

and location = zint

(* All sizes should be inlined and precomputed *)
and size = int

and sizes = size list

and raw_ty =
  | TBool
  | TVoid
  | TGroup
  | TInt of size (* Number of bits *)
  | TEvent of bool (* True iff multicast *)
  | TFun of func_ty (* Only used for Array update functions at this point *)
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
  ; elocations : location list
  }

and value =
  { v : v
  ; vty : ty option
  ; vspan : sp
  }

(* expressions *)
and e =
  | EVal of value
  | EInt of z * size option (* Differs from VInt since size may be polymorphic *)
  | EVar of cid
  | EOp of op * exp list
  | ECall of cid * exp list
  | EHash of size * exp list

and exp =
  { e : e
  ; ety : ty option
  ; espan : sp
  }

and branch = pat list * statement

(* statements *)
and s =
  | SNoop
  | SUnit of exp
  | SLocal of id * ty * exp
  | SAssign of id * exp
  | SPrintf of string * exp list
  | SIf of exp * statement * statement
  | SGen of bool * exp (* Bool is true iff multicast *)
  | SSeq of statement * statement
  | SMatch of exp list * branch list

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
  | DGroup of id * exp list
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

(* Values *)
let avalue v vty vspan = { v; vty; vspan }
let value_sp v vspan = { v; vty = None; vspan }
let value v = { v; vty = None; vspan = Span.default }

(* Expressions *)
let exp e = { e; ety = None; espan = Span.default }
let aexp e ety espan = { e; ety; espan }
let exp_sp e espan = { e; ety = None; espan }
let value_to_exp v = aexp (EVal v) v.vty v.vspan

(* Declarations *)
let decl d = { d; dspan = Span.default }
let decl_sp d span = { d; dspan = span }
