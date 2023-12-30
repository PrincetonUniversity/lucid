(* Simple direct translation pass from core parts of the Lucid IR to c 

  Progress: 
    - expressions and operations
    - statements (besides match)
    - sized integers
    - user-defined record types
    - entry / main functions

  TODO: 
    - match statements
    - hash statements
    - arrays & memops
    - tables & actions
    - patterns
    - events, handlers, and event generation
    - bitstrings & event parsers


*)
open CoreSyntax
open StrUtils
open InterpHelpers
open Printf

[@@@ocaml.warning "-27-26"]

exception Error of string
let error s = raise (Error s)

let dprint_endline str = 
  if Cmdline.cfg.debug then print_endline str else ()

(* 

The goal of this pass is to translate a program with a single entry point 
into a c program with declarations for globals and the entry function. 
The function arguments and returns should be values, not pointers. 


**** libraries ****


Compilation strategies: 

 **** Sized integers ****
Lucid supports sized integers, such as int<32> x = 5, where 32 is the size in bits.
In C, we have fixed-size integer types like uint32_t, uint64_t, etc.

Our translation strategy is to map each Lucid integer to the smallest C unsigned integer type 
that can accommodate the same number of bits. This ensures efficient use of memory while 
preserving the semantics of the Lucid code.

For example:
- A Lucid integer of size 5 (int<5> x) is translated to a C uint8_t, which can hold up to 8 bits.
- A Lucid integer of size 11 (int<11> x) is translated to a C uint16_t, which can hold up to 16 bits.
- A Lucid integer of size 32 (int<32> x) is translated to a C uint32_t, which can hold up to 32 bits.
- A Lucid integer of size 48 (int<48> x) is translated to a C uint64_t, which can hold up to 64 bits.

This strategy allows us to handle Lucid integers of any size, as long as they fit within 64 bits.


 **** Events ****
  The "event" type is a tagged union of event constructors. 
    - Each event turns into a structure and a uint_16_t tag.

      const uint16_t foo_tag = 1;
      typedef struct foo {
          int a;
          int b;
      } foo;

      const uint16_t bar_tag = 1;
      typedef struct bar {
          int c;
      } bar;

      typedef union event {
          uint16_t tag;
          struct foo foo_event;
          struct bar bar_event;
      } event;


  *** Tables ***
  TODO
  (possibly complicated)

  *** Arrays and PairArrays ***
  TODO
    get types named based on their sizes: 
      Array.t<32> my_arr = Array.create(1024); // an array of 32-bit ints

      typedef struct array_32_t {
          uint32_t size;
          uint32_t* data;
      } array_32_t;





  **** Functions ****
  fun int foo(int x, int y, event e) {
      return x + y;
  }

  memop foo(int from_mem, from_whatever) {

  }
  
*)


(* c code generators *)
let struct_string name (fields : (string * string) list) = 
  sprintf "typedef struct %s {\n%s\n} %s;\n" 
    name 
    ((List.map 
      (fun (ts, ns) -> sprintf "%s %s;" ts ns) 
      fields) |> String.concat "\n" |> indent_def) 
    name
;;



let id_string = Id.name
let cid_string cid = String.concat "_" (Cid.names cid);;


(* naming functions *)
let memopty_string n_args arg_sz = sprintf "memop_%d_%d" n_args arg_sz

let builtin_ty_string cid sizes = sprintf "%s_%s" (cid_string cid) (underscore_sep string_of_int sizes)  

(* the name of an events argument struct *)
let event_struct evid = sprintf "event_%s" (id_string evid)
(* the type tag of the event -- part of the event_type enum *)
let event_type evid = sprintf "EVENT_TYPE_%s" (String.uppercase_ascii (id_string evid))
let unknown_event_type, unknown_event_num = "EVENT_TYPE_UNKNOWN", 0
(* the name of an event constructor *)



(* misc helpers *)
let rty_to_size rty =
  match rty with
  | TBool -> 1
  | TInt sz -> sz
  | _ -> error "[ty_to_size] can only get size of ints or bools"
;;

let maxint n = Z.sub (Z.(lsl) (Z.of_int 1) n) (Z.of_int 1) 
let zhexstr z = "0x"^(Z.format "%x" z)
;;



let translate_tint (sz:int) =
  if sz >= 1 && sz <= 8 then "uint8_t"
  else if sz >= 9 && sz <= 16 then "uint16_t"
  else if sz >= 17 && sz <= 32 then "uint32_t"
  else if sz >= 33 && sz <= 64 then "uint64_t"
  else error "Unsupported integer size"
;;

let mk_memop_tys n_args arg_sz = 
  let arg_ty = translate_tint arg_sz in
  sprintf "typedef %s (*%s)(%s);" 
    arg_ty (* return type *)
    (memopty_string n_args arg_sz) (* memop type name *)
    (List.init n_args (fun _ -> arg_ty) |> String.concat ", ") (* argument types *)
;;

let mk_name_ty cid sizes = 
  match Cid.names cid with 
  | "Counter"::_ -> error "counter not implemented"
  | "Array"::_
  | "PairArrays"::_ -> (
    let name_ty = builtin_ty_string cid sizes in
    let sizes = List.map (fun sz -> sprintf "uint%d_t" sz) sizes in
    sprintf "typedef struct %s { uint16_t size; %s* data; } %s;" 
      name_ty
      (String.concat ", " sizes)
      name_ty  
  )
  | module_str::_ -> error (sprintf "%s not implemented" module_str)
  | _ -> error "unknown name type"
;;

let rec translate_ty ty = translate_raw_ty ty.raw_ty
and translate_raw_ty rty = match rty with
  | TBool -> "bool" 
  | TInt(sz) -> translate_tint sz
  | TGroup -> error "Group types not implemented"
  | TEvent -> "event"
  | TFun({arg_tys; ret_ty;}) -> 
    let _, _ = arg_tys, ret_ty in
    error "function types cannot be translated alone"
    (* let arg_tys = List.map translate_ty arg_tys in
    let ret_ty = translate_ty ret_ty in
    sprintf "%s (*func_type)(%s)" ret_ty (String.concat ", " arg_tys) *)    
  | TName(cid, sizes, true) -> builtin_ty_string cid sizes
  | TName(cid, [], false) -> cid_string cid
  | TName(cid, _, false) -> error "user-defined types should not be size polymorphic"    
    (* error "user-defined named types should be inlined by now" *)
  | TMemop(n_args, arg_sz) -> memopty_string n_args arg_sz
  (* an action type is really an action constructor type *)
  | TTable _ -> error "table types cannot be translated alone"
  | TAction _ -> error "action type not implemented"
  | TActionConstr _ -> error "action types cannot be translated alone"
  | TPat _ -> error "patterns not implemented"
  | TRecord _ -> error "record types cannot be translated alone"
  | TTuple(tys) -> 
    let sizes = List.map rty_to_size tys in
    sprintf "tup_%s" (underscore_sep string_of_int sizes)
  (* | TTable *) 
  | TBits _ -> error "bitstrings not implemented"
;;

let translate_params params = 
  let params = List.map (fun (id, ty) -> sprintf "%s %s" (translate_ty ty) (id_string id)) params in
  String.concat ", " params
;;

let rec translate_v v = 
  match v with 
  | VBool(true) -> "1"
  | VBool(false) -> "0"
  | VInt(zint) -> Integer.value_string zint
  | VEvent(event_val) -> 
    sprintf "{%s}" (comma_sep translate_value event_val.data)
  | VTuple(vs) -> 
    sprintf "{%s}" (underscore_sep translate_v vs)
  | VGlobal _ -> error "global value cannot be translated alone"
  | VGroup _ -> error "group valus not implemented"
  | VPat _ -> error "pattern values not implemented"
  | VBits _ -> error "bitstring values not implemented"
  | VRecord _ -> error "record values not implemented"
and translate_value value = 
  translate_v value.v

let translate_op op = match op with 
  | And -> "&&"
  | Or -> "||"
  | Not -> "!"
  | Eq -> "=="
  | Neq -> "!="
  | Less -> "<"
  | More -> ">"
  | Leq -> "<="
  | Geq -> ">="
  | Neg -> "-"
  | Plus -> "+"
  | Sub -> "-"
  | Cast(sz) -> sprintf "(%s)" (translate_tint sz)
  | Conc -> error "concat cannot be translated alone"
  | Slice _ -> error "slice cannot be translated alone"
  | SatSub -> error "satsub cannot be translated alone"
  | SatPlus -> error "satplus cannot be translated alone"
  | BitAnd -> "&"
  | BitOr -> "|"
  | BitXor -> "^"
  | BitNot -> "~"
  | LShift -> "<<"
  | RShift -> ">>"
  | PatExact -> error "pattern ops cannot be translated alone"
  | PatMask -> error "pattern ops cannot be translated alone"
;;

let rec translate_e e ety = match e with 
  | EVal(value) -> translate_value value
  | EVar(cid) -> cid_string cid
  (* op expression -- there are a decent number of special cases *)
  | EOp(Conc, [larg; rarg]) -> 
    let larg_str = translate_exp larg in
    let rarg_str = translate_exp rarg in
    sprintf "(%s << %i) | %s" 
      larg_str 
      (size_of_tint rarg.ety) 
      rarg_str
  | EOp(Slice(hi, lo), [arg]) ->
    let arg_str = translate_exp arg in
    let mask = maxint (hi - lo + 1) |> zhexstr in
    sprintf "((%s >> %i) & %s)" arg_str lo mask
  | EOp(SatSub, [larg; rarg]) -> 
    let larg_str = translate_exp larg in
    let rarg_str = translate_exp rarg in
    sprintf "(%s < %s ? 0 : %s - %s)" larg_str rarg_str larg_str rarg_str
  | EOp(SatPlus, [larg; rarg]) when 
    (* for lucid ints stored as uints of the same size, 
       call a sat_add library function *)
    (List.mem (size_of_tint ety) [8; 16; 32; 64]) -> 
    let larg_str = translate_exp larg in
    let rarg_str = translate_exp rarg in
    let sz = size_of_tint ety in
    sprintf "sat_add%d(%s, %s)" sz larg_str rarg_str
  | EOp(SatPlus, [larg; rarg]) -> 
    (* for all other lucid ints, they are stored in a larger 
       uint, so we can do the operation and then check for an 
       overflow. *)
    let larg_str = translate_exp larg in
    let rarg_str = translate_exp rarg in
    let sz = size_of_tint ety in
    let max_val_str = maxint sz |> zhexstr in
    sprintf "(%s + %s > %s ? %s : %s + %s)" 
      larg_str 
      rarg_str 
      max_val_str 
      max_val_str 
      larg_str 
      rarg_str  
  (* catch expressions that overflow in Lucid and mask their results 
     to preserve same semantics.  *)
  | EOp(op, [larg; rarg]) 
    when (op = Plus || op = Sub || op = LShift || op = RShift) 
          && (not (List.mem (size_of_tint ety) [8; 16; 32; 64])) -> 
    let op = translate_op op in
    let larg = translate_exp larg in
    let rarg = translate_exp rarg in
    let sz = rty_to_size ety.raw_ty in
    let mask_str = maxint sz |> zhexstr in
    sprintf "((%s %s %s) & %s)" larg op rarg mask_str
  | EOp(op, [larg; rarg]) -> 
    let op = translate_op op in
    let larg = translate_exp larg in
    let rarg = translate_exp rarg in
    sprintf "(%s %s %s)" larg op rarg
  | EOp(op, [binarg]) -> 
    let op = translate_op op in
    let binarg = translate_exp binarg in
    sprintf "(%s%s)" op binarg
  | EOp(_, _) -> error@@"unimplemented eop expression ("^(CorePrinting.e_to_string e)^")"
  (* int<k> hash_res = hash<k>(poly, v0, v1, v2, ..., vn);  *)
  | ECall(cid, args, _) -> (
    (* a call may either be an function or an event constructor. If its an 
       event constructor, we translate it into a struct initializer for the 
       event union that sets the field corresponding to this 
       event's constructor. *)
    match (ety.raw_ty) with 
    | TEvent -> 
      let args = List.map translate_exp args in
      let args = String.concat ", " args in
      sprintf "{.%s={%s}}" (cid_string cid) args
    | _ -> 
      let args = List.map translate_exp args in
      let args = String.concat ", " args in
      let cid = cid_string cid in
      sprintf "%s(%s)" cid args
  )
  | ERecord(fields) -> 
    let fields = List.map (fun (id, exp) -> sprintf ".%s=%s" (id_string id) (translate_exp exp)) fields in
    sprintf "(%s){%s}" 
      (translate_ty ety) (* in some places, like return statements, we need an explicit cast *)
      (String.concat ", " fields)
  | ETuple(exps) -> error "tuple expressions cannot be compositionally translated"
  | EProj(exp, cid) -> 
    sprintf "%s.%s" (translate_exp exp) (id_string cid)
  | EHash _ -> error "hash expressions must be evaluated at the statement level"
  | EFlood _ -> error "the flood builtin is not implemented"
  | ETableCreate _ -> error "table create expressions must be evaluated at the declaration level"
and translate_exp exp = 
  translate_e exp.e exp.ety

let rec translate_s s = 
  match s with 
  | SNoop -> ""
  | SLocal(id, ty, exp) -> 
    let ty = translate_ty ty in
    let exp = translate_exp exp in
    sprintf "%s %s = %s;" ty (id_string id) exp
  | SAssign(cid, exp) -> 
    let exp = translate_exp exp in
    sprintf "%s = %s;" (cid_string cid) exp
  | SPrintf(str, args) -> 
    let args = List.map translate_exp args in
    let args = String.concat ", " args in
    sprintf "printf(\"%s\", %s);" str args
  | SIf(cond, s1, s2) ->
    let cond = translate_exp cond in
    let s1 = translate_statement s1 in
    let s2 = translate_statement s2 in
    sprintf "if (%s) {%s} else {%s}" cond (indent_def s1) (indent_def s2)
  | SGen _ -> error "generate not implemented"
  | SSeq(s1, s2) -> 
    let s1 = translate_statement s1 in
    let s2 = translate_statement s2 in
    sprintf "%s\n%s" (s1) (s2)
  | SMatch _ -> error "match not implemented"
  | SRet(exp_opt) -> (
    let exp = match exp_opt with 
      | Some(exp) -> translate_exp exp
      | None -> "" in
    (* if we are returning an event, we have to create a variable and return that, 
       because compound literals aren't allowed in return statements *)
    match exp_opt with
    | Some(exp) when equiv_ty exp.ety tevent  -> 
      let tmp_var = "tmp_event" in
      let exp = translate_exp exp in
      sprintf "event %s = %s;\nreturn %s;" tmp_var exp tmp_var
    | _ -> 
      sprintf "return %s;" exp
  )
  | STableMatch _ -> error "table match not implemented"
  | STableInstall _ -> error "table install not implemented"
  | STupleAssign _ -> error "tuple assignment not implemented"
  | SUnit(exp) -> 
    let exp = translate_exp exp in
    sprintf "%s;" exp
and translate_statement s = translate_s s.s



let rec translate_decl  decl = translate_d  decl.d

and translate_d d = match d with 
    | DGlobal(id, ty, exp) -> 
      translate_dglobal  id ty exp
    | DEvent _ -> "" (* handled in event struct generator *)
    | DMemop _ -> 
      error "memops not implemented"
    | DExtern _ -> 
      error "externs not implemented"
    | DActionConstr _ -> 
      error "actions not implemented"
    | DFun(id, ty, (params, statement)) -> 
      let ty = translate_ty ty in
      let params = List.map (fun (id, ty) -> sprintf "%s %s" (translate_ty ty) (id_string id)) params in
      let params = String.concat ", " params in
      let statement = translate_statement statement in
      sprintf "%s %s(%s) {\n%s\n}" ty (id_string id) params (indent_def statement)
    | DHandler _ -> error "handlers not implemented"
    | DParser _ -> error "parsers not implemented"
    | DUserTy(id, {raw_ty=TRecord(fields)}) -> 
      (* declare a typedef struct with fields in type. Type must be trecord *)
      let field_strs = List.map (fun (id, ty) -> translate_raw_ty ty, id_string id) fields in
      struct_string (id_string id) field_strs
    | DUserTy(_, _) ->error "user types must be records"

and translate_dglobal  id ty e = 
  (* tables are special *)
  match ty.raw_ty with 
  | TTable _ -> error "tables not implemented"
  | _ -> (
    let gty_name, gty_sizes =
      match ty.raw_ty with
      | TName (cid, sizes, _) -> Cid.names cid, sizes
      | _ -> failwith "Bad DGlobal"
    in
    let args =
      match e.e with
      | ECall (_, args, _) -> args
      | _ -> failwith "Bad constructor"
    in
    match gty_name, gty_sizes, args with
    | ["Array"; "t"], [size], [e] -> 
      (* this should be all we need when its time to fill it in *)
      let len = int_from_exp e in
      let cell_size = size in      
      error "array not implemented"
    | ["Counter"; "t"], [size], [e] -> error "counter not implemented"
    | ["PairArray"; "t"], [size], [e] -> error "pairarray not implemented"
    | _ -> error "unknown global type"
  )
;;

(* translate computation declarations (everything except events) *)
let rec translate_compute decls = 
  match decls with 
  | [] -> ""
  | decl::decls -> 
    let decl = translate_decl  decl in
    let decls = translate_compute  decls in
    (decl ^ "\n" ^ decls)
;;

(* generate all event-related data structures *)
let translate_events decls = 
  let events = List.filter_map (fun decl -> 
    match decl.d with 
    | DEvent(ev_constr) -> Some(ev_constr)
    | _ -> None
  ) decls in
  if (List.length events = 0) then "" else
    (* 1. an enum of event tags *)
    let event_to_enum_field ((id, num_opt, _, _):event_constr) = 
      match num_opt with 
      | None -> error "event constructors must have a number"
      | Some(num) -> sprintf "%s = %i" (event_type id) num
    in
    let fields_str = (sprintf "%s = %i" unknown_event_type unknown_event_num)
      ::(List.map event_to_enum_field events) |> String.concat ",\n" in
    let event_enum = sprintf 
      "typedef enum event_type {\n%s\n} event_type;" 
      (indent_def fields_str)
      ^"\n"
    in
    (* 2. a struct for each event *)
    let event_to_struct (id, _, _, params) =         
      struct_string 
        (event_struct id) 
        (List.map (fun (id, ty) -> (translate_ty ty, id_string id)) params)^"\n"
    in
    let event_structs = List.map event_to_struct events |> String.concat "\n" in
    (* 3. a union of all events *)
    let event_union = sprintf
      "typedef union event {\n%s\n} event;"
      (indent_def 
        (List.map 
          (fun (id, _, _, _) -> sprintf "%s %s;" (event_struct id) (id_string id)) 
          events 
        |> String.concat "\n"))
    in
    line_sep (fun s -> s) ["//event data structures"; event_enum; event_structs; event_union]
;; 

let translate decls = 
  let libs = (CLibs.libs ["base"]) in
  let event_structs = translate_events decls in
  let fcn_structs = translate_compute decls in
  line_sep (fun s -> s) [libs; event_structs; fcn_structs]
;;