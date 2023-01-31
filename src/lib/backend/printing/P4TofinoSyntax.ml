(* minimal P4-tofino IR *)
(* open CoreSyntax *)
exception Error of string

let error s = raise (Error s)

type struct_ty = 
  | THdr
  | TMeta
and id = Id.t
and cid = Cid.t
and ty = 
    | TInt  of int (* unsigned int of width *)
    | TBool
    | TStruct of id
    | TList of ty list
    | TVoid
    | TAuto

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

and expr = 
  | EVal of value 
  | EVar of cid 
  | EOp of op * expr list
  | EConstr of id * ty list * expr list (* constructor:  id<ty list>(expr list)   *)
  | ECall of expr * expr list option 
  (* ECall args are optional so that a call 
     can represent things like parse state transitions, 
     which never have arguments and don't get ()'s.*)
  | EList of expr list



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

and key = 
  | Ternary of cid
  | Exact of cid

 and toplevel_block = {id:id; params : params; decls : decl list; body : statement option;}
 and regaction_apply = params * statement
 and d = 
  | DReg of {id:id; cellty:ty; idxty:ty; len:expr; def:expr option;}
  | DVar of id * ty * (expr option) (* variables may be declared but not initialized... *)
  (* | DInit of cid * expr *)
  | DTable of {id:id; keys:key list; actions: id list; rules: branch list; default: statement option; }
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
  (* call an object constructor *)
  | DObj of expr * id (* EConstr<...>(...) id; *)  
  (* declarations for objects that get 
     initialized by the P4 control script *)
  | DMCGroup of {gid:int; replicas : (int * int) list;}
  | DPort of {dpid:int; speed:int;}

and decl = {d:d; dpragma:pragma list;}

and pipe = {parse : decl; process : decl; deparse : decl;}

and tofino_prog = 
{
    globals : decl list;
    ingress : pipe;
    control_config : decl list;
    egress  : pipe;
}

(**** constructors ****)
let id = Id.create
let cid s = Cid.create (String.split_on_char '.' s)
;;

let vint i w = VInt (i, w)
let tint i = TInt(i)
let tstruct sid = TStruct sid
let tbool = TBool


let ecall fcn_id args = 
  ECall(EVar(fcn_id), Some(args))
let ecall_table tid = ecall (Cid.create_ids [tid; id "apply"]) [] ;;

let ecall_action aid const_args = 
  ecall aid const_args
;;

let ejump label = 
  ECall(EVar(label), None)

let evar cid = EVar(cid)
let eval_int i = EVal(VInt(i, None))

let elist es = EList es

let eop op args = EOp(op, args)
let eadd cid i = eop Add [evar cid; eval_int i]
let eeq cid i = eop Eq [evar cid; eval_int i]


let tern_key_of_exp expr = 
  match expr with 
  | EVar(cid) -> Ternary(cid)
  | _ -> error "[tern_key_of_exp] exp must be a EVar to convert into a key field"
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


let decl d = {d=d; dpragma=[];}

let dstruct hid sty fields =
  decl (DStructTy{id=hid; sty; fields})
;;

let dinclude str = decl (DInclude(str))
let daction id params body = decl (DAction{id; params; body;})

let dtable id keys actions rules default pragmas =
  {d=DTable({id; keys; actions; rules; default});
  dpragma=pragmas;}
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

let econstr class_id class_tys class_args = 
  EConstr(class_id, class_tys, class_args)

let dobj class_id class_tys class_args obj_id =
  decl (DObj(econstr class_id class_tys class_args, obj_id))

let drandom out_wid obj_id =
  dobj (id "Random") [tint out_wid] [] obj_id




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
