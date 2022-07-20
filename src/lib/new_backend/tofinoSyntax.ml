(*
next:
    - define op, pluck the 
      important elements out of llsyntax

    - let the control flow of a block be defined as:
        1. a control flow graph
        2. a dataflow 
        3. a sequence of statements
        - then, we parse directly into the from IR and 
          just do transformations to get from 1 --> 3

    - add optional pragmas, attached to compute objects 
      and maybe local variables? (do we want local variables?)

    - do we want to include control plane logic 
      or other "support engine" configuration?
      - maybe.. that would tie _everything_ together

    - this really isn't tofino specific.. should it be called 
      something else?

    - write printer
    - write parser (maybe.. or maybe from P4?)
    - write interpreter (also maybe)

*)

(* 
Notes:

   we might be able to describe a parser 
   as a restricted type of match-action 
   pipeline, with a special printer. 
   Do we want to do that? 
   One complication is that, the match-action 
   seems inverted. We start with an action, then have 
   a match table that does the selection. 
   We _could_ start with an empty table as the first 
   state. It just matches on nothing and runs an 
   action to extract the first field(s). Then we branch
   from there. That should work.
    
   Also, the parser has special instruction that 
   operate on packets -- peek, advance, and extract.

   It might be the case that you can represent a 
   data-plane element as a lookup table pipeline 
   parameterized on:
   1. syntactic check
   2. printer
   3. instruction set

*)


(* a program for the tofino, ready to print to P4_16 tofino *)
open LLSyntax
(* This syntax represents a complete tofino program.
It is mostly a subset of P4, and is designed 
to be easy to print to P4 *)

type vid = Cid.t (* variable id *)
type lvid = vid
type oid = Cid.t (* operation id *)
type rid = Cid.t (* register id *)

(* a variable can be a bit, struct, header, or extern *)
(* a header has slightly (annoyingly) different semantics than a struct... *)
and ty = 
    | TBit  of int
    | TStruct of vid
    | THeader of vid 
    (* a struct that is defined outside of this program. *)
    | TExtern of vid 

and value = 
    | VInt of int


and params = (ty * direction * vid) list
(* input or output parameters *)
and direction = 
    | TDin
    | TDout

(* control blocks *)
(* an operator is a compute object, 
   like a table or stateful alu *)
and op = decl

(* A table contains rules
   rules contain patterns and actions
   actions contain instructions
   instructions contain stateless, stateful, and hash operators
   The program is a graph of tables 
   TODO: should keys be an attribute of the table, 
         rather than the rules? Of the rules makes 
         it hard to do control-plane updated tables.
*)
and compute_unit = 
    | Table of {tid : oid; rules : rule list;}
and rule  = {pat : pat; acn : action;}
and pat   = {key : vid; cond : condition;}
and condition = 
    | Bitstring of bit list
    | Exact of int
    | Any
and bit = 
    | B0
    | B1
    | BANY

and action = {instrs : instruction list;}
and instruction = 
    | IAssign of {out:lvid; lhs:alu_expr;}
    | ISetValid of {out:lvid; valid:bool;} (* enable or disable hdr "out" *)
    | IHash of {out:lvid; wid:int; poly:int; args:basic_expr list;}
    | IStatefulOp of {
        out : lvid option;
        rid : rid; 
        idx : basic_expr;
        ty  : ty; (* cell type *)
        body : stateful_expr;
        }

(* basic exprs can appear pretty much anywhere *)
and basic_expr = 
    | EVal of int 
    | EVar of vid 
    | ESlice of int * int * vid
    | ERegVar of memcell 

(* an alu expr requires an alu to evaluate *)
and alu_expr = 
    | IExpr of basic_expr
    | IBinOp of binOp * basic_expr list

(* stateful expr requires a stateful alu to evaluate *)
and stateful_expr = {
    cell1 : supdate_expr option * supdate_expr option;
    cell2 : supdate_expr option * supdate_expr option;
    ret   : supdate_expr option;
}

and supdate_expr = scond_expr * alu_expr

and scond_expr = 
  | CTrue 
  | CBool of sbool_expr
  | COp of boolop * scond_expr list 

and sbool_expr = 
  | BRel of alu_expr * cmpop * basic_expr
  | BVal of bool (* true or false*)

and cmpop =
  | Eq
  | Neq
  | Lt
  | Gt

and boolop =
  | And
  | Or
  | Not

and memcell = 
  | Lo 
  | Hi 
  | LoNew
  | HiNew
  | MemOut


(* a match action pipeline *)
and match_action_pipe = {
    params : params;
    units  : compute_unit list;
    control  : control;
}
and control = 
    | CStatements of stmt list
    | CControlGraph of string
    | CDataGraph of string
(* TODO: what graph library do we use? *)

and stmt =
    | TNoop 
    | TCall of oid
    | TSeq of stmt * stmt


(* parsers 
   a parser has: 
    - input parameters 
        (header and metadata stacks, 
         packet is implicit) 
    - a list of states, with 
      the first one as the root
   *)
and parser = {
    params : params;
    states : state list;
}

(* state name, list of parse instructions, 
   pointer or branch to next state(s) *)
and state = {
    name : oid;
    instrs : pinstr list;
    next : parselink option;
}

and pinstr = 
    (* peek / advance can be used on any struct *)
    (* foo = peek<my_hdr_t>(pkt); *)
    | TPPeek of {toutid : vid; touty : ty; tpktid : vid;}
    (* advance<my_hdr_t>(pkt); *)
    | TAdvance of {touty : ty; tpktid : vid;}
    (* extract can only be used on a header *)
    | TPExtract of {toutid : vid; touty : ty; tpktid : vid; }
    | TPSet of {toutid : vid; tnew : texpr;}

and parselink = 
    | TPnext of oid
    | TPBranch of vid * (parsepat * oid) list

and parsepat = 
    | TPatConst of int
    | TPatVar of mid
    | TPatAny

(* a basic expression *)
and texpr = 
    | TVal of int
    | TVar of vid





(* a global declaration *)
and tdecl = 
    | TDConst  of vid * ty * texpr
    | TDStructy of {id : vid; fields : (vid * ty) list}
    | TDHeaderTy of {id : vid; fields : (vid * ty) list}
    | TDExternTy of vid
    | TDImport of string



(* multicast engine config block *)
and mc_tbl = mc_group list
and mc_group =
  { mc_id : int
  ; mc_instrs : mc_copy_instr list
  }

(* copy a packet to a port and annotates it with an id.*)
and mc_copy_instr =
  { port : int
  ; pkt_copy_id : int
  }

and tofinoProg = {
    globals : tdecl;    
    ing_par : parser;
    igr_map : match_action_pipe;
    igr_dpr : match_action_pipe;
    mc_eng  : mc_tbl;
    egr_par : parser;
    egr_map : match_action_pipe;
    egr_dpr : match_action_pipe;
}
