(* Abstract syntax of DPT *)
open Batteries
include TQVar.TQVar_tys

(* These types need to be declared mutually recursive so that the visitors
   deriving plugin develops a visitor for each of them. The above four types
   have a visitor already defined in TQVar.ml, so they're placed separately *)
type cid = [%import: Cid.t]

and sp = [%import: Span.t]

and z = [%import: (Z.t[@opaque])]

and zint = [%import: (Integer.t[@with Z.t := (Z.t [@opaque])])]

and location = zint

and size =
  | IConst of int
  | IUser of cid (* User-defined size *)
  | IVar of size tqvar
  (* Normal form: list is non-emoty, sorted, and no entries are Link, IConst, or ISum *)
  | ISum of sizes * int

and sizes = size list

and effect =
  | FZero
  | FProj of effect
  | FIndex of id * effect
  | FSucc of effect
  | FVar of effect tqvar

and constr = CLeq of effect * effect

and constr_spec_cmp =
  | SpecLess
  | SpecLeq

and constr_spec =
  | CSpec of (cid * constr_spec_cmp) list
  | CEnd of cid

and raw_ty =
  | TQVar of raw_ty tqvar
  | TBool
  | TVoid
  | TGroup
  | TInt of size (* Number of bits *)
  | TEvent of bool (* True iff multicast *)
  | TFun of func_ty
  | TMemop of size * size
  | TName of cid * sizes * bool (* Named type: e.g. "Array.t<<32>>". Bool is true if it represents a global type *)
  | TRecord of (string * raw_ty) list
  | TVector of raw_ty * size
  | TTuple of raw_ty list

and func_ty =
  { arg_tys : tys
  ; ret_ty : ty
  ; start_eff : effect
  ; end_eff : effect
        (* This has to be a ref to perform unification during typechecking.
   Do not mutate it anywhere else! *)
  ; constraints : constr list Stdlib.ref
  }

and ty =
  { raw_ty : raw_ty
  ; teffect : effect
  ; tspan : sp
  ; tprint_as : raw_ty option ref [@opaque] (* Only used for pretty-printing *)
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
  | TGet of int * int (* Size of the tuple, index to get *)
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
  | ESizeCast of size * size (* Cast a size to int *)
  | EStmt of statement * exp
  | ERecord of (string * exp) list
  | EWith of exp * (string * exp) list (* { e with ...} syntax *)
  | EProj of exp * string
  | EVector of exp list
  | EComp of exp * id * size (* Vector comprehension *)
  | EIndex of exp * size
  | ETuple of exp list

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
  | SRet of exp option
  | SSeq of statement * statement
  | SMatch of exp list * branch list
  | SLoop of statement * id * size

and statement =
  { s : s
  ; sspan : sp
  }

(* event handler bodies *)
and params = (id * ty) list

and body = params * statement

and event_sort =
  | EEntry of bool (* true iff "control", i.e. it can generate non-continue events *)
  | EExit
  | EBackground

and ispec =
  | InSize of id
  | InVar of id * ty
  | InTy of id * sizes * ty option * bool (* True if type is global *)
  | InConstr of id * ty * params
  | InFun of id * ty * constr_spec list * params
  | InEvent of id * constr_spec list * params
  | InModule of id * interface

and interface_spec =
  { ispec : ispec
  ; ispan : sp
  }

and interface = interface_spec list

(* declarations *)
and d =
  | DSize of id * size
  | DGlobal of id * ty * exp
  | DEvent of id * event_sort * constr_spec list * params
  | DHandler of id * body
  | DFun of id * ty * constr_spec list * body
  | DMemop of id * body
  | DConst of id * ty * exp
  | DGroup of id * exp list
  | DExtern of id * ty
  | DUserTy of id * sizes * ty
  | DConstr of id * ty * params * exp
  | DModule of id * interface * decls

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
    ; ancestors = ["tqvar_iter"]
    ; nude = true
    }
  , visitors
      { name = "s_map"
      ; ancestors = ["tqvar_map"]
      ; variety = "map"
      ; polymorphic = false
      ; data = true
      ; concrete = true
      ; nude = true
      }
  , visitors (* fold into a new tree but ignore tqvars *)
      { name = "s_fold"; variety = "fold"; ancestors = ["tqvar_map"] }]

(********************************)
(* Constructors and Destructors *)
(********************************)

exception Error of string

let error s = raise (Error s)

(* types *)
let ty_sp raw_ty tspan =
  { raw_ty
  ; teffect = FVar (QVar (Id.fresh "eff"))
  ; tspan
  ; tprint_as = ref None
  }
;;

let ty_eff raw_ty teffect =
  { raw_ty; teffect; tspan = Span.default; tprint_as = ref None }
;;

let ty raw_ty =
  { raw_ty
  ; teffect = FVar (QVar (Id.fresh "eff"))
  ; tspan = Span.default
  ; tprint_as = ref None
  }
;;

(* values *)
let avalue v vty vspan = { v; vty; vspan }
let value_sp v vspan = { v; vty = None; vspan }
let value v = { v; vty = None; vspan = Span.default }
let vint i size = value (VInt (Integer.create i size))
let vinteger i = value (VInt i)
let vbool b = value (VBool b)
let default_vint size = value (VInt (Integer.create 0 size))
let default_vbool = value (VBool false)
let vint_sp i span = value_sp (VInt i) span
let vbool_sp b span = value_sp (VBool b) span
let vevent event = value (VEvent event)
let vevent_sp event span = value_sp (VEvent event) span
let vglobal idx = value (VGlobal idx)
let vgroup locs = value (VGroup locs)

(***********************************)
(* Modules for manipulating tqvars *)
(***********************************)

module STQVar = TQVar.Make (struct
  type a = size

  let proj a =
    match a with
    | IVar x -> Some x
    | _ -> None
  ;;

  let constr t = IVar t
end)

module FTQVar = TQVar.Make (struct
  type a = effect

  let proj a =
    match a with
    | FVar x -> Some x
    | _ -> None
  ;;

  let constr t = FVar t
end)

module TyTQVar = TQVar.Make (struct
  type a = raw_ty

  let proj a =
    match a with
    | TQVar x -> Some x
    | _ -> None
  ;;

  let constr t = TQVar t
end)

(* expressions *)
let exp e = { e; ety = None; espan = Span.default }
let aexp e ety espan = { e; ety; espan }
let exp_sp e espan = { e; ety = None; espan }
let value_to_exp v = aexp (EVal v) v.vty v.vspan
let var_sp cid span = exp_sp (EVar cid) span
let eint z size = exp (EInt (z, size))
let eint_sp z size span = exp_sp (EInt (z, size)) span
let op_sp op args span = exp_sp (EOp (op, args)) span
let call_sp cid args span = exp_sp (ECall (cid, args)) span
let hash_sp size args span = exp_sp (EHash (size, args)) span
let proj_sp e l span = exp_sp (EProj (e, l)) span
let record_sp lst span = exp_sp (ERecord lst) span
let with_sp base lst span = exp_sp (EWith (base, lst)) span
let index_sp lst idx span = exp_sp (EIndex (lst, idx)) span
let comp_sp e i k span = exp_sp (EComp (e, i, k)) span
let vector_sp es span = exp_sp (EVector es) span
let szcast_sp sz1 sz2 span = exp_sp (ESizeCast (sz1, sz2)) span

(* declarations *)
let decl d = { d; dspan = Span.default }
let decl_sp d span = { d; dspan = span }
let dglobal_sp id ty exp span = decl_sp (DGlobal (id, ty, exp)) span
let dconst_sp id ty e span = decl_sp (DConst (id, ty, e)) span
let dextern_sp id ty span = decl_sp (DExtern (id, ty)) span
let handler_sp id p body span = decl_sp (DHandler (id, (p, body))) span
let dsize_sp id size span = decl_sp (DSize (id, size)) span
let fun_sp id rty cs p body span = decl_sp (DFun (id, rty, cs, (p, body))) span
let memop_sp id p body span = decl_sp (DMemop (id, (p, body))) span
let group_sp id es span = decl_sp (DGroup (id, es)) span
let duty_sp id sizes rty span = decl_sp (DUserTy (id, sizes, rty)) span

let dconstr_sp id ty params exp span =
  decl_sp (DConstr (id, ty, params, exp)) span
;;

let module_sp id intf ds span = decl_sp (DModule (id, intf, ds)) span

(* let func_sp id p body span =
   decl_sp (DProc (id, (p,body))) span *)

let event_sp id s cs p span = decl_sp (DEvent (id, s, cs, p)) span

(* statements *)
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
let sret_sp e span = statement_sp (SRet e) span
let sifte_sp e s1 s2 span = statement_sp (SIf (e, s1, s2)) span
let gen_sp b e span = statement_sp (SGen (b, e)) span
let scall_sp cid args span = statement_sp (SUnit (call_sp cid args span)) span
let match_sp es bs span = statement_sp (SMatch (es, bs)) span
let loop_sp e i k span = statement_sp (SLoop (e, i, k)) span

(* Interface spefications *)
let spec ispec = { ispec; ispan = Span.default }
let spec_sp ispec ispan = { ispec; ispan }
let invar_sp id ty span = spec_sp (InVar (id, ty)) span
let insize_sp id span = spec_sp (InSize id) span

let infun_sp id ty cspecs params span =
  spec_sp (InFun (id, ty, cspecs, params)) span
;;

let inty_sp id sizes tyo b span = spec_sp (InTy (id, sizes, tyo, b)) span
let inconstr_sp id ty params span = spec_sp (InConstr (id, ty, params)) span

let inevent_sp id cspecs params span =
  spec_sp (InEvent (id, cspecs, params)) span
;;

let inmodule_sp id intf span = spec_sp (InModule (id, intf)) span
