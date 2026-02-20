(* Abstract core syntax of Lucid -- this is the part that remains after all the
   frontend transformations have eliminated the rest of it. *)
open Batteries

type id = [%import: (Id.t[@opaque])]
and cid = [%import: (Cid.t[@opqaue])]
and tagval = [%import: (TaggedCid.tagval[@opqaue])]
and tcid = [%import: (TaggedCid.t[@opqaue])]
and sp = [%import: Span.t]
and z = [%import: (Z.t[@opaque])]
and pragma = [%import: Pragma.t]
and zint = [%import: (Integer.t[@with Z.t := (Z.t [@opaque])])]
and location = int
and bit = [%import: (BitString.bit[@opaque])]
and bits = [%import: (BitString.bits[@opaque])]

(* All sizes should be inlined and precomputed *)
and size = 
  | Sz of int
  | Szs of int list

and sizes = size list
and action_sig = string * size list * size list

and raw_ty =
  | TBool
  | TInt of size (* Number of bits *)
  | TEvent
  | TFun of func_ty (* Only used for Array/event functions at this point *)
  | TName of cid * sizes
    (* Named type: e.g. "Array.t<<32>>". Bool is true if it represents a global type *)
  | TBuiltin of cid * raw_ty list
    (* Named type that is a builtin *)
  | TMemop of int * size
  | TAction of acn_ty
  | TActionConstr of acn_ctor_ty
  | TRecord of (id * raw_ty) list
  | TTuple of raw_ty list
  (* group, pat, and bits all seem like instances of a "list" type? *)
  | TGroup
  | TPat of size
  | TBits of size

and acn_ty = {
  aarg_tys : tys;
  aret_tys : tys;
}

and acn_ctor_ty =
  { aconst_param_tys : tys
  ; aacn_ty : acn_ty
  }

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
  | PatExact
  | PatMask

and pat =
  | PWild
  | PNum of z
  | PBit of int list
  | PEvent of cid * params

(* values *)
and v =
  | VBool of bool
  | VInt of zint
  | VEvent of event_val
  | VGlobal of id * int (* Name * Stage number *)
  | VTuple of v list (* Only used in the interpreter during complex memops *)
  | VGroup of location list
  | VPat of int list
  | VBits of bits
  | VRecord of (id * v) list

and event_val =
  { eid : cid
  ; evnum : value option
  ; data : value list
  ; edelay : int
  ; eserialized : bool (* Only used in parsing and deparsing. Maybe remove. *)
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
  | ECall of cid * exp list * bool
  | EHash of size * exp list
  | EFlood of exp
  | ERecord of (id * exp) list
  | EProj of exp * id
  | ETuple of exp list 

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
  | SAssign of cid * exp
  | SPrintf of string * exp list
  | SIf of exp * statement * statement
  | SGen of gen_type * exp
  | SSeq of statement * statement
  | SMatch of exp list * branch list
  | SRet of exp option
  (* | STableInstall of exp * tbl_entry list *)
  | STupleAssign of tuple_assign (* unpack a tuple exp and use it to create or set variables *)


and tuple_assign = {
  ids : id list;
  tys : (ty list) option;
  exp : exp;
}


and statement =
  { s : s
  ; sspan : sp
  ; spragmas : pragma list
  }

and params = (id * ty) list
and cid_params = (cid * ty) list
and body = params * statement

and handler_sort =
  | HControl
  | HData
  | HEgress

and event_sort =
  | EPacket
  | EBackground

(* For memops -- Boolean condition * return value *)
and conditional_return = exp * exp

and complex_body =
  { b1 : (id * exp) option
  ; b2 : (id * exp) option
  ; cell1 : conditional_return option * conditional_return option
  ; cell2 : conditional_return option * conditional_return option
  ; extern_calls : (cid * exp list) list
  ; ret : conditional_return option
  }

and memop_body =
  | MBReturn of exp
  | MBIf of exp * exp * exp
  | MBComplex of complex_body

and memop =
  { mid : id
  ; mparams : params
  ; mbody : memop_body
  }

and action_body = exp list

and action =
  { aid : id
  ; artys : ty list
  ; aconst_params : params
  ; aparams : params
  ; abody : action_body
  }

and parser_action =
  | PRead of cid * ty * exp (* exp should always be ECall(Payload.read, [_ : Payload.t]) *)
  | PPeek of cid * ty * exp (* peek is a read from the payload without advancing the header *)
  | PSkip of ty
  | PAssign of cid * exp
  | PLocal of cid * ty * exp

and parser_branch = pat list * parser_block

and parser_step =
  | PMatch of exp list * parser_branch list
  | PGen of exp
  | PCall of exp (* Call another parser *)
  | PDrop

(* Include span for error reporting *)
and parser_block = {
  pactions : (parser_action * sp) list;
  pstep : parser_step * sp;
}
(* and parser_block = (parser_action * sp) list * (parser_step * sp) *)

and event_constr = (id * int option * event_sort * params)

(* declarations *)
and d =
  | DGlobal of id * ty * exp
  | DEvent of event_constr
  | DHandler of id * handler_sort * body
  | DMemop of memop
  | DExtern of id * ty
  | DUserTy of id * ty
  | DActionConstr of action
  | DParser of id * params * parser_block
      (* name, return type, args & body *)
  | DFun of id * ty * body
      (* dfun should only ever be a "main" function, 
         when using lucid as a function compiler *)

         
and decl =
  { d : d
  ; dspan : sp
  ; dpragma : pragma option
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
let tint sz = ty (TInt sz)
let tevent = ty (TEvent)
let tname_sp cid sizes tspan = ty_sp (TName(cid, sizes)) tspan
let tname cid sizes = tname_sp cid sizes Span.default
let payload_ty = ty@@TName(Cid.create ["Payload"; "t"], [])
let ttuple raw_tys = {raw_ty = TTuple(raw_tys); tspan = Span.default}

let rec infer_vty v =
  match v with
  | VBool _ -> TBool
  | VInt z -> TInt(Sz(Integer.size z))
  | VEvent _ -> TEvent
  | VGroup _ -> TGroup
  | VGlobal _ -> failwith "Cannot infer type of global value"
  | VPat bs -> TPat(Sz(List.length bs))
  | VTuple(vs) -> TTuple(List.map infer_vty vs)
  | VBits bits -> TBits(Sz(List.length bits))
  | VRecord fields ->
    TRecord (List.map (fun (id, v) -> id, infer_vty v) fields)
;;

(* Values *)
let avalue v vty vspan = { v; vty; vspan }
let value_sp v vspan = { v; vty = infer_vty v |> ty; vspan }
let value v = { v; vty = infer_vty v |> ty; vspan = Span.default }
let vint i size = value (VInt (Integer.create i size))
let vwild size = value (VPat (List.init size (fun _ -> -1)))
let vinteger i = value (VInt i)
let vpat bs = value (VPat bs)
let vbool b = value (VBool b)
let default_vint size = value (VInt (Integer.create 0 size))
let default_vbool = value (VBool false)
let vint_sp i span = value_sp (VInt i) span
let vbool_sp b span = value_sp (VBool b) span
let vevent event = value (VEvent event)
let vevent_sp event span = value_sp (VEvent event) span
let vglobal id idx ty = avalue (VGlobal(id, idx)) ty Span.default
let vgroup locs = value (VGroup locs)
let vtup vs = avalue (VTuple vs) (ty (TTuple(List.map infer_vty vs)))
let vrecord fields = value (VRecord fields)
(* int, size tups -> vtup(sized_ints) *)
let vint_tups i_s =
  vtup (List.map (fun (i, s) -> VInt(Integer.create i s)) i_s) (Span.default)
;;  
let vbits bits = value (VBits bits)


(* Expressions *)
let exp e ety = { e; ety; espan = Span.default }
let aexp e ety espan = { e; ety; espan }
let value_to_exp v = aexp (EVal v) v.vty v.vspan
let var_sp cid ety span = aexp (EVar cid) ety span
let var cid ety = var_sp cid ety Span.default
let op_sp op args ety span = aexp (EOp (op, args)) ety span
let op op args ety = op_sp op args ety Span.default
let call_sp cid args ety span = aexp (ECall (cid, args, false)) ety span
let call cid args ety = call_sp cid args ety Span.default
let hash_sp size args ety span = aexp (EHash (size, args)) ety span
let vint_exp i size = value_to_exp (vint i size)
let vint_exp_ty i (ty:ty) = 
  match ty.raw_ty with
  | TInt(Sz sz) -> 
    value_to_exp (vint i sz)
  | _ -> error "[vint_exp_ty] type mismatch"
;;
let tup_sp es span = 
  let tys = List.map (fun e -> e.ety) es in
  aexp (ETuple es) (ty (TTuple (List.map (fun ty -> ty.raw_ty) tys))) span
;;

(* Statements *)

let statement s = { s; sspan = Span.default; spragmas = [] }
let statement_sp s span = { s; sspan = span; spragmas = [] }
let snoop = statement SNoop
let sseq s1 s2 = statement (SSeq (s1, s2))
let slocal id ty e = statement (SLocal (id, ty, e))
let sassign id e = statement (SAssign (id, e))
let sprintf s es = statement (SPrintf (s, es))
let sprintf_sp s es span = statement_sp (SPrintf (s, es)) span
let sifte e s1 s2 = statement (SIf (e, s1, s2))
let smatch es ss = statement (SMatch (es, ss))
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
let decl d = { d; dspan = Span.default; dpragma = None }
let decl_sp d span = { d; dspan = span; dpragma = None }
let decl_pragma d dspan dpragma = { d; dspan; dpragma }
let dglobal_sp id ty exp span = decl_sp (DGlobal (id, ty, exp)) span
let dextern_sp id ty span = decl_sp (DExtern (id, ty)) span

let handler_sp id p sort body span =
  decl_sp (DHandler (id, sort, (p, body))) span
;;

let memop_sp mid mparams mbody span =
  decl_sp (DMemop { mid; mparams; mbody }) span
;;


(* parser constructors *)
let block actions step : parser_block = 
  {pactions=List.map (fun a -> a, Span.default) actions; pstep=(step, Span.default)}
;;

(* actions *)
(* let read cid ty exp = PRead(cid, ty, exp) *)
(* let read_id exp (id, ty) = read (Cid.id id) ty exp *)

let peek cid ty exp = PPeek(cid, ty, exp)
let skip ty = PSkip(ty)
let assign cid exp = PAssign(cid, exp)

(* steps *)
let pgen exp = PGen(exp)
let pdrop = PDrop
let pcall exp = PCall(exp)
let pcall_cid cid args = PCall(call cid args tevent)
let pmatch exps branches = PMatch(exps, branches)
(* match branches *)
let pbranch ints block : parser_branch  = (List.map (fun i -> PNum (Z.of_int i)) ints), block
let pbranch_wild num_pats block : parser_branch = (List.init num_pats (fun _ -> PWild)), block

let parser id params block = 
  DParser(id, params, block)
;;

let empty_block () :parser_block = 
  block [] pdrop
;;



(*** Utility -- may split into a separate file if it gets big *)

let equiv_list f lst1 lst2 =
  try List.for_all2 f lst1 lst2 with
  | Invalid_argument _ -> false
;;

let rec equiv_ty t1 t2 =
  match t1.raw_ty, t2.raw_ty with
  | TBool, TBool -> true
  | TInt sz1, TInt sz2 -> sz1 = sz2
  | TEvent, TEvent -> true
  | TGroup, TGroup -> true
  | TPat sz1, TPat sz2 -> sz1 = sz2
  | TBits sz1, TBits sz2 -> sz1 = sz2
  | TName(n1, []), TName(n2, []) -> Cid.equal n1 n2
  | TRecord(fields1), TRecord(fields2) ->
    List.for_all2 (fun (id1, ty1) (id2, ty2) -> Id.equal id1 id2 && equiv_ty (ty ty1) (ty ty2)) fields1 fields2
  | _ -> false
;;

let equiv_options f o1 o2 =
  match o1, o2 with
  | None, None -> true
  | Some _, None | None, Some _ -> false
  | Some o1, Some o2 -> f o1 o2
;;

let rec equiv_value v1 v2 =
  match v1.v, v2.v with
  | VBool b1, VBool b2 -> b1 = b2
  | VInt n1, VInt n2 -> Integer.equal n1 n2
  | VGlobal(_, n1), VGlobal(_, n2) -> n1 = n2
  | VGroup locs1, VGroup locs2 -> locs1 = locs2
  | VEvent e1, VEvent e2 ->
    Cid.equal e1.eid e2.eid
    && e1.edelay = e2.edelay
    && equiv_list equiv_value e1.data e2.data
  | _ -> false
;;


let rec equiv_e e1 e2 = 
  match e1, e2 with
  | EVal v1, EVal v2 -> equiv_value v1 v2
  | EVar cid1, EVar cid2 -> Cid.equal cid1 cid2
  | ECall (cid1, es1, u1), ECall (cid2, es2, u2) ->
    Cid.equal cid1 cid2 && equiv_list equiv_exp es1 es2 && (u1 = u2)
  | EHash (sz1, es1), EHash (sz2, es2) ->
    sz1 = sz2 && equiv_list equiv_exp es1 es2
  | EOp (op1, es1), EOp (op2, es2) -> op1 = op2 && equiv_list equiv_exp es1 es2
  | _ -> false

and equiv_exp e1 e2 =
  equiv_e e1.e e2.e
;;

let equiv_pat p1 p2 =
  match p1, p2 with
  | PWild, PWild -> true
  | PNum z1, PNum z2 -> Z.equal z1 z2
  | PBit is1, PBit is2 -> equiv_list Int.equal is1 is2
  | _, _ -> false
;;

let rec equiv_stmt s1 s2 =
  match s1.s, s2.s with
  | SNoop, SNoop -> true
  | SUnit e1, SUnit e2 -> equiv_exp e1 e2
  | SLocal (id1, _, exp1), SLocal (id2, _, exp2) -> 
    Id.equal id1 id2 && equiv_exp exp1 exp2
  | SAssign (id1, exp1), SAssign (id2, exp2) ->
    Cid.equal id1 id2 && equiv_exp exp1 exp2
  | SPrintf (s1, es1), SPrintf (s2, es2) ->
    String.equal s1 s2 && equiv_list equiv_exp es1 es2
  | SIf (e1, s11, s12), SIf (e2, s21, s22) ->
    equiv_exp e1 e2 && equiv_stmt s11 s21 && equiv_stmt s12 s22
  | SGen (GSingle (Some e1), e11), SGen (GSingle (Some e2), e22)
  | SGen (GMulti e1, e11), SGen (GMulti e2, e22)
  | SGen (GPort e1, e11), SGen (GPort e2, e22) ->
    equiv_exp e1 e2 && equiv_exp e11 e22
  | SGen (GSingle None, e11), SGen (GSingle None, e22) -> equiv_exp e11 e22
  | SSeq (s11, s12), SSeq (s21, s22) -> equiv_stmt s11 s21 && equiv_stmt s12 s22
  | SMatch (es1, bs1), SMatch (es2, bs2) ->
    equiv_list equiv_exp es1 es2 && equiv_list equiv_branch bs1 bs2
  | SRet None, SRet None -> true
  | SRet (Some e1), SRet (Some e2) -> equiv_exp e1 e2
  | _ -> false

and equiv_branch (ps1, s1) (ps2, s2) =
  let res = equiv_list equiv_pat ps1 ps2 && equiv_stmt s1 s2 in 
  if not res then 
    Console.error@@"[equiv_branch] branches not equivalent";
  res
;;

let equiv_bool_assign (b1, e1) (b2, e2) = 
  Id.equal b1 b2 && equiv_exp e1 e2
;;

let equiv_conditional_return (e1, e2) (e1', e2') = 
  equiv_exp e1 e1' && equiv_exp e2 e2'
;;


let equiv_memop_body mb1 mb2 = 
  match (mb1, mb2) with 
  | (MBReturn e1, MBReturn e2) -> equiv_exp e1 e2
  | (MBIf (e1, e2, e3), MBIf (e1', e2', e3')) -> 
    equiv_exp e1 e1' && equiv_exp e2 e2' && equiv_exp e3 e3'
  | (MBComplex(c1), MBComplex(c2)) -> 
  begin
       equiv_options equiv_bool_assign c1.b1 c2.b1
    && equiv_options equiv_bool_assign c1.b2 c2.b2
    && equiv_options equiv_conditional_return (fst c1.cell1) (fst c2.cell1)
    && equiv_options equiv_conditional_return (snd c1.cell1) (snd c2.cell1)
    && equiv_options equiv_conditional_return (fst c1.cell2) (fst c2.cell2)
    && equiv_options equiv_conditional_return (snd c1.cell2) (snd c2.cell2)
    && equiv_options equiv_conditional_return c1.ret c2.ret
  end
  | _ -> false
;;

let equiv_memop (m1 : memop) (m2 : memop) = 
  Id.equal m1.mid m2.mid
  && equiv_list equiv_ty (List.map snd m1.mparams) (List.map snd m2.mparams)
  && equiv_memop_body m1.mbody m2.mbody
;;

(* bit pattern helpers, for interp *)
let int_to_bitpat n len =
  let bs = Array.create len 0 in
  for i = 0 to len - 1 do
    let pos = len - 1 - i in
    if n land (1 lsl i) != 0 then bs.(pos) <- 1 else bs.(pos) <- 0
  done;
  Array.to_list bs
;;

let int_mask_to_bitpat n mask len =
  let bs = Array.create len 0 in
  for i = 0 to len - 1 do
    let pos = len - 1 - i in
    (* if the mask's value is 1 at pos, use value *)
    if mask land (1 lsl i) != 0
    then
      if n land (1 lsl i) != 0
      then bs.(pos) <- 1
      else bs.(pos) <- 0 (* otherwise, use -1 *)
    else bs.(pos) <- -1
  done;
  Array.to_list bs
;;

let ty_to_size ty =
  match ty.raw_ty with
  | TBool -> 1
  | TInt (Sz sz) -> sz
  (* | TGroup -> 16 *)
  | _ -> error "[ty_to_size] can only get size of ints or bools"
;;

let rec size_of_rawty raw_ty = match raw_ty with 
  | TBool -> 1
  | TInt (Sz sz) -> sz
  | TRecord(params) -> 
    List.split params |> snd |> List.map size_of_rawty |> List.sum    
  | TTuple(raw_tys) -> 
    List.map size_of_rawty raw_tys |> List.sum
  | _ -> error@@"[size_of_rawty] unsupported type."
  ;;
let size_of_tint = ty_to_size

(* Turn a list of statements into an SSeq (or a SNoop, if empty) *)
let rec sequence_stmts lst =
  match lst with
  | [] -> snoop
  | { s = SNoop } :: tl -> sequence_stmts tl
  | [hd] -> hd
  | hd :: tl -> sseq hd (sequence_stmts tl)
;;
let rec flatten_stmt (stmt : statement) : statement list = 
  match stmt.s with 
  | SSeq(s1, s2) -> (flatten_stmt s1) @ (flatten_stmt s2)
  | _ -> [stmt]
;;


let exp_to_cid exp =
  match exp.e with
  | EVar cid -> cid
  | _ -> error "[id_of_exp] expression is not an evar"
;;

let id_of_exp exp =
  match exp.e with
  | EVar (Id id) -> id
  | _ -> error "[id_of_exp] expression is not an evar"
;;

let exp_to_id = id_of_exp
let exp_of_id id ty = exp (EVar (Cid.id id)) ty

let exp_to_int exp =
  match exp.e with
  | EVal { v = VInt z; _ } -> Integer.to_int z
  | _ -> error "[exp_to_int] exp is not an EVal(EInt(...))"
;;

(* bitstrings *)
let hexstr_to_vbits hexstr = vbits@@BitString.hexstr_to_bits hexstr

let extract_bits value = 
  match value.v with
  | VBits bits -> bits
  | _ -> error "[extract_bits] value is not a VBits"
;;

(* is an argument to a parser its packet arg? *)
let pkt_arg_ty = ty(TBits (Sz 1500))
let is_pkt_arg (_, ty) = match ty.raw_ty with | TBits (Sz 1500) -> true | _ -> false 


let to_singleton_sizes szs = List.map (fun sz -> match sz with | Sz s -> s | _ -> error "[to_singleton_sizes] non-singleton size") szs
let size_to_ints sz = match sz with | Sz s -> [s] | Szs ss -> ss
let size_to_int sz = match sz with | Sz s -> s | _ -> error "[size_to_int] non-singleton size"


