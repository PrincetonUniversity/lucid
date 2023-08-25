(* minimal P4-tofino IR *)
(* open CoreSyntax *)
open Span
exception Error of string

let error s = raise (Error s)

type id = [%import: (Id.t[@opaque])]
and cid = [%import: (Cid.t[@opqaue])]
and tagval = [%import: (TaggedCid.tagval[@opqaue])]
and tcid = [%import: (TaggedCid.t[@opqaue])]
and sp = [%import: Span.t]
and struct_ty = 
  | THdr
  | TMeta
and key_ty = 
  | KTernary
  | KExact
and ty = 
    | TInt  of int (* unsigned int of width *)
    | TBool
    | TStruct of id
    | TList of ty list
    | TVoid
    | TAuto
    | TUnknown
    | TObj of id
    | TKey of (key_ty * int) (* match type, key length *)
    | TFun of ty * ty list

and value = 
    | VInt of int * (int option)
    | VBool of bool    
    | VStruct of id * value list
    | VString of string (* for control plane programs *)

and op = 
  | Add
  | Sub (* A - B *)
  | SatAdd
  | SatSub (* A |-| B (i.e., max(A - B, 0)) *)
  | RShift
  | LShift
  | BAnd
  | BOr
  | BXor
  | Concat
  | Eq
  | Neq
  | Lt
  | Gt
  | Lte
  | Gte
  | And
  | Or
  | Not
  | Cast (* cast B to width A *)
  | Slice

(* and call_sig = {fcn_id:cid; fcn_kind:fkind; args:expr list} *)

and ex = 
  | EVal of value 
  | EVar of cid 
  | EOp of op * expr list
  | EConstr of id * ty list * expr list (* constructor:  id<ty list>(expr list)   *)
  | ECall of expr * expr list option 
  (* ECall args are optional so that a call 
     can represent things like parse state transitions, 
     which never have arguments and don't get ()'s.*)
  (* a call with type arguments *)
  | ETyCall of expr * ty list * expr list
  | EList of expr list

and expr = {ex : ex; espan : sp; ety : ty;}


and args = expr list

and pragma = {pname : string; pargs : string list;}

and bit = 
    | B0
    | B1
    | BANY
and pat =
  | PWild
  | PNum of int
  | PBitstring of bit list
and branch = (pat list * statement) 

and statement = 
  | Noop
  | Seq of statement * statement
  | Assign of cid * expr
  | Local of id * ty * expr
  | Unit of expr
  | Prag of pragma
  | If of expr * statement * (statement option)
  | Match of (expr list) * (branch list)

and direction = 
    | Pin
    | Pout
    | Pinout
and param = (direction option * ty * id)
and params = param list

and toplevel_block = {id:id; params : params; decls : decl list; body : statement option;}
and regaction_apply = params * statement
and d = 
  (* slot_ty: the type of the whole slot, which might be a named struct type.
     slot_sz: the sizes of the cells in the slot *)
  | DReg of {id:id; slot_ty:ty; slot_sz: int list; idxty:ty; len:expr; def:expr option;}
  | DVar of id * ty * (expr option) (* variables may be declared but not initialized... *)
  (* | DInit of cid * expr *)
  | DTable of {id:id; keys:expr list; actions: expr list; rules: branch list; default: statement option; size: int option;}
  | DAction of {id:id; params : params; body: statement;}
  | DHash of {id:id; poly: int; out_wid:int;}
  | DRegAction of {  
      id : id; 
      reg : id;
      idx_ty : ty;
      mem_fcn : regaction_apply;
    }
  | DParseState of {id:id; body:statement;}
  (* toplevel decls *)
  | DInclude of string
  | DStructTy of {id : id; sty : struct_ty; fields : (id * ty) list;}
  | DConst of id * ty * expr
  | DControl of toplevel_block
  | DParse of toplevel_block
  | DDeparse of toplevel_block
  (* external object constructor *)
  | DObj of expr * id (* EConstr<...>(...) id; *)  
  (* declarations for objects that get 
     initialized by the P4 control script *)
  | DMCGroup of {gid:int; replicas : (int * int) list;}
  | DPort of {dpid:int; speed:int;}
  | DPragma of pragma
  | DNone

and decl = {d:d; dpragma:pragma list; dspan : sp;}

and pipe = {parse : decl; process : decl; deparse : decl;}

and tofino_prog = 
{
    globals : decl list;
    ingress : pipe;
    control_config : decl list;
    egress  : pipe;
}
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

(**** constructors ****)
let id = Id.create
let cid s = Cid.create (String.split_on_char '.' s)
;;

let vint i w = VInt (i, w)

let tbool = TBool
let tint i = TInt(i)
let tstruct sid = TStruct sid
let tfun rty atys = TFun(rty, atys)
let tnoretmethod = TFun(TVoid, [])


(* expressions *)
let eval_ty_sp v ty sp = {ex=EVal(v); espan = sp; ety = ty;}
let eval_ty v ty = {ex=EVal(v); espan = Span.default; ety = ty;}
let eval v = eval_ty v TUnknown

let evar_ty_sp cid ty sp = {ex=EVar(cid); espan = sp; ety = ty;}
let evar_ty cid ty = {ex=EVar(cid); espan = Span.default; ety = ty;}
let evar cid = evar_ty cid TUnknown

let eval_int_ty i ty = {ex=EVal(VInt(i, None)); espan = Span.default; ety=ty;}
let eval_int i = eval_int_ty i TUnknown
let ecall_ty fcn_id args ty = 
  {ex=ECall(evar fcn_id, Some(args)); espan = Span.default; ety = ty;}
let ecall fcn_id args = ecall_ty fcn_id args TUnknown

let etycall_ty fcn_id tys args ty = 
  {ex=ETyCall(evar fcn_id, tys, args); espan = Span.default; ety=ty;}
;;
let etycall fcn_id tys args = 
  etycall_ty fcn_id tys args TUnknown
;;
let ecall_method fcn_id = 
  {ex=ECall(evar fcn_id, None); espan = Span.default; ety=TVoid;}
let ejump = ecall_method 
let ecall_table tid = ecall (Cid.create_ids [tid; id "apply"]) [] ;;
let ecall_action aid const_args = 
  ecall aid const_args

let elist_ty es ty = {ex=EList es; espan = Span.default; ety = ty;}
let elist es = elist_ty es TUnknown

let eop_ty_sp op args ty sp = {ex=EOp(op, args); espan = sp; ety = ty;}
let eop_ty op args ty = {ex=EOp(op, args); espan = Span.default; ety = ty;}
let eop op args = eop_ty op args TUnknown

let eadd cid i = eop Add [evar cid; eval_int i]
let eeq cid i = eop Eq [evar cid; eval_int i]

let econstr_ty class_id class_tys class_args sp ety = 
  {ex=EConstr(class_id, class_tys, class_args); espan = sp; ety;}
let econstr class_id class_tys class_args sp = 
  econstr_ty class_id class_tys class_args sp TUnknown

let evar_noretmethod cid = evar_ty cid tnoretmethod

let exp_to_ternary_key expr = 
  let ety = match expr.ety with
    | TInt(sz) -> TKey(KTernary, sz)
    | _ -> error "[exp_to_ternary_key] keys must be integer variables"
  in
  {expr with ety}
;;

let exp_to_exact_key expr = 
  let ety = match expr.ety with
    | TInt(sz) -> TKey(KExact, sz)
    | _ -> error "[exp_to_exact_key] keys must be integer variables"
  in
  {expr with ety}
;;


let param ty id = 
    (None, ty, id)
let inparam ty id = 
    (Some Pin, ty, id)
let outparam ty id = 
    (Some Pout, ty, id)
let inoutparam ty id =
    (Some Pinout, ty, id)
;;

(* statements *)
let sunit e = Unit(e)
let smatch es bs = Match(es, bs)
let sif e s1 = If(e, s1, None)
let sifelse e s1 s2 = If(e, s1, Some(s2))
let snoop = Noop


let sassign cid e = Assign(cid,e)
let slocal id ty e = Local(id, ty, e)
let local i t e = Local(i, t, e)

let scall cid args = 
  sunit (ecall (cid) args)

let scall_action acn_id =
  sunit (ecall (Cid.id acn_id) [])
let scall_table tid =
  sunit (ecall_table tid)
;;

(* declarations *)
let decl d = {d=d; dpragma=[]; dspan = Span.default}

let dpragma name args =
  decl (DPragma {pname=name; pargs=args;})
;;

let decl_full d prag span = {d=d; dpragma=prag; dspan=span;}

let dstruct hid sty fields =
  decl (DStructTy{id=hid; sty; fields})
;;

let dinclude str = decl (DInclude(str))
let daction id params body = decl (DAction{id; params; body;})

let dtable_sp id keys actions rules default size pragmas sp =
  {d=DTable({id; keys; actions; rules; default; size});
  dpragma=pragmas; dspan = sp;}
;;

let dtable id keys actions rules default size pragmas = 
  dtable_sp id keys actions rules default size pragmas Span.default
;;

let dreg_sp id slot_ty slot_sz idxty len def pragmas span = 
  decl_full (DReg{id; slot_ty; slot_sz; idxty; len; def}) pragmas span
;;  

let dreg id slot_ty slot_sz idxty len def pragmas = 
  dreg_sp id slot_ty slot_sz idxty len def pragmas Span.default
;;

let dcontrol id params decls stmt = 
  decl (DControl{id; params; decls; body=Some(stmt);})
;;

let dparse id params decls = 
  decl (DParse{id=id; params=params; decls=decls; body=None;})
;;

let dvar_uninit id ty = decl (DVar(id, ty, None))
let dvar id ty expr = decl (DVar(id, ty, Some(expr)))
let dvar_cid id ty cid = dvar id ty (evar cid) 
let dvar_int id ty i = dvar id ty (eval_int i)

let dobj class_id class_tys class_args obj_id sp =
  decl (DObj(econstr class_id class_tys class_args sp, obj_id))

let drandom out_wid obj_id =
  dobj (id "Random") [tint out_wid] [] obj_id Span.default




let pnum i: pat = PNum i
let pwild : pat = PWild

let rec sseq stmts:statement =
  match stmts with 
  | [] -> Noop
  | [stmt] -> stmt
  | [st1; Noop] -> st1
  | [Noop; st2] -> st2
  | [st1; st2] -> Seq(st1,st2)
  | Noop::stmts -> sseq stmts
  | st1::stmts -> Seq(st1, sseq stmts)
;;

(**** helpers ****)

let ty_to_size ty = match ty with
  | TInt(sz) -> sz
  | TBool -> 1
  | TKey(_, sz) -> sz
  | _ -> error "[ty_to_size] cannot get size of this type"
;;

let expr_to_int (expr:expr) = 
  match expr.ex with
  | EVal(VInt(i, _)) -> i
  | _ -> error "[expr_to_int] expression is not a value"
;;
(** bitstring operations **)
let bits_to_maskedint (bits : bit list) : (int * int) = 
  let to_val_and_mask bit = 
    match bit with 
    | B0    -> (0, 1) (* val: 0, mask 1 *) 
    | B1    -> (1, 1) (* val: 1, mask 1 *) 
    | BANY  -> (0, 0) (* val: 0, mask :0 *)
  in
  let rec bitstring_to_int bits =  
    match bits with 
      | [] -> 0
      | hd::tl ->
        (Int.shift_left hd (List.length tl)) + (bitstring_to_int tl)
  in      
  let vbits, mbits = List.map to_val_and_mask bits |> List.split in 
  (bitstring_to_int vbits, bitstring_to_int mbits)
;;  

let int_to_bits (i:int) : bit list = 
  let rec int_to_bits_rev (i:int) : bit list = 
    match i with 
    | 0 -> []
    | _ -> 
      let lastbit = match (Base.Int.(land) i 1) with
        | 0 -> B0
        | 1 -> B1
        | _ -> error "[int_to_bits_rev] impossible case: (i && 1) > 1"
      in 
      lastbit::(int_to_bits_rev (Int.shift_right i 1))
  in 
  List.rev (int_to_bits_rev i)
;;

let rec pad_to_w w bs = 
  let bits_to_add = w - (List.length bs) in 
  if (bits_to_add <= 0)
  then (bs)
  else (pad_to_w (w) (B0::bs))
;;

let blocks_of_prog tprog = 
  let block_of_pipe p = 
    [p.parse; p.process; p.deparse]
  in
  (block_of_pipe tprog.ingress) @ (block_of_pipe tprog.egress)
;;

let mcgroups_of_decls (decls:decl list) =
  let is_mcgroup dec = match dec.d with 
    | DMCGroup(_) -> true | _ -> false
  in
  List.filter is_mcgroup decls
;;
let non_mcgroups_of_decls (decls:decl list) =
  let is_mcgroup dec = match dec.d with 
    | DMCGroup(_) -> false | _ -> true
  in
  List.filter is_mcgroup decls
;;

let find_mcgroup decls ps =
  let finder decl = match decl.d with 
    | DMCGroup{replicas=replicas;} ->
      MiscUtils.list_eq ps (List.split replicas |> fst)
    | _ -> false
  in
  match (List.find_opt finder decls) with 
    | Some({d=DMCGroup{gid=gid;}})-> Some(gid)
    | _ -> None
;;

let mcgroups decls =
  List.filter_map 
    (fun decl -> match decl.d with 
    | DMCGroup{gid=gid; replicas=replicas;} -> Some(gid, replicas)
    | _ -> None
    )
    decls
;;

let ports decls = 
  List.filter_map 
    (fun decl -> match decl.d with 
    | DPort{dpid=dpid; speed=speed;} -> Some(dpid, speed)
    | _ -> None
    )
    decls
;;
