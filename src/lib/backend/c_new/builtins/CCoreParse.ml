(* implementation of parse functions *)

(* 
Design:  

Parser functions: 
  A parse function is originally a function that takes a 
  bytestring and generates an event or calls drop. 
  e.g.:  parser foo(bytes_t bs)
  It is transformed into a function that takes a 
  reference to a bytestring and a reference to an output
  event. It fills the output events, updates the bytestring, 
  and returns a boolean indicating success / failure. 

Bytestrings:
  A bytestring represents the unparsed part of a packet. 
  It is a record with three pointers: 
    typedef struct bytes_t {
      char* start;  // pointer to start of buffer
      char* cur;    // pointer to current position
      char* end;    // pointer to end of buffer
    } bytes_t;
  - Before parsing, "cur" should equal start.
  - After parsing, "cur" points to the payload.
  - "cur" should always be less than or equal to end.

Parse helpers: 
  There are three parse methods that operate on bytestrings:
    skip, peek, and read. 
  The functions are polymorphic on the type of the extracted 
  or skipped value. An instance is generated for each 
  type used in a parser.
  void skip_int(bytes_t* bs) {
    bs->cur = bs->cur + sizeof(int);
  }
  int peek_int(bytes_t* bs) {
    return ((int * ) (bs->cur))[0];
  }
  int read_int(bytes_t* bs) {
      int rv = ((int 
      skip_int(bs);
      return rv;
  } 

Generate: 
  event generation is translated into setting the 
  value of the event return variable, similar to 
  the transformation done in handlers. 
Background event parsing:
  The background event parser is inlined 
  wherever calls to it appear. 


Deparsing notes (this goes into a separate pass)
    - Payload is a flag (carry input payload or not)
    - the function that writes out the packet will get a 
      value of the event and the bytes_t of the packet. 
         - what happens next is platform dependent. 
          - in ebpf, it will shrink or grow the buffer of the 
            current packet to hold the new event. 
          - in userspace c it will likely allocate 
            output buffer and copy the event header and payload there. 
*)
open CCoreSyntax
open CCoreExceptions

(* helpers *)
let ty_to_namestr ty = match ty.raw_ty with 
  | TInt _ | TBool | TAbstract _ -> CCorePPrint.ty_to_string ~use_abstract_name:true ty
  | _ -> err_expected_ty ty "to convert a type to a string for a generated function, the type must be an int, bool, or abstract"
;;

let tyapp cid ty = 
  Cid.str_cons_plain (ty_to_namestr ty) cid
;;

(* ( *bs).cur *)

let n_bytes ty = 
  match bitsizeof_ty ty with
  | None -> err_expected_ty ty "to generate a bytesize_of_ty_exp, the type must have a known size"
  | Some n ->     
    let byte_n = (n+7) / 8 in
    eval (vint byte_n 32)
;;


(*** primary type and code generators ***)
(* note: these can be made "safe" for ebpf by
   adding runtime checks on the bounds (cur + $ty_size <= end) *)
(* typedef struct bytes_t {
  char* start;  // pointer to start of buffer
  char* cur;    // pointer to current position
  char* end;    // pointer to end of buffer
} bytes_t; *)
let bytes_t = 
  tabstract 
    "bytes_t"@@trecord_pairs
      [
        id"start", tref tchar;
        id"cur", tref tchar;
        id"end", tref tchar;
      ]
;;
(* bytes_t param for generated functions *)
let bs_param = (id"bs", tref bytes_t)
(* bytes_t var within generated functions *)
let bs = param_evar bs_param


(* void skip_int(bytes_t* bs) {
    bs->cur = bs->cur + sizeof(int);
} *)
(* generate a skip function for the given type *)
let skip_name ty = tyapp (cid"skip") ty
let skip_ty = tfun [snd bs_param] tunit
let skip_var ty = efunref (skip_name ty) skip_ty
let call_skip ty bs = ecall (skip_var ty) [bs]
let mk_skip ty = 
  dfun (skip_name ty)
    tunit
    [bs_param]
    @@stmts [
      sassign_exp (bs/->id"cur") ((bs/->id"cur")/+(n_bytes ty));
      sret_none
    ]
;;

(* int peek_int(bytes_t* bs) {
    return ((int * ) *(bs->cur));
} *)
(* generate a peek function for the given type *)
let peek_name ty = tyapp (cid"peek") ty
let peek_ty ty = tfun [snd bs_param] ty
let peek_var ty = efunref (peek_name ty) (peek_ty ty)
let call_peek ty bs = ecall (peek_var ty) [bs]
let mk_peek ty = 
  dfun (peek_name ty)
    ty
    [bs_param]
    @@stmts [
      (* get pointer, cast to correct pointer type, deref *)
      sret @@ (ederef @@ ecast (tref ty) @@ (bs/->id"cur"))
    ]
;;

(* int read_int(bytes_t* bs) {
    int rv = peek(bs);
    skip_int(bs);
    return rv;
} *)
(* generate a read function for the given type *)
let read_name ty = tyapp (cid"read") ty
let read_ty ty = tfun [snd bs_param] ty
let read_var ty = efunref (read_name ty) (read_ty ty)
let call_read ty bs = ecall (read_var ty) [bs]
let mk_read ty = 
  dfun (read_name ty)
    ty
    [bs_param]
    @@stmts [
      slocal (cid"rv") ty @@ call_peek ty bs;
      sunit @@ call_skip ty bs;
      sret @@ evar (cid"rv") ty
    ]
;;

(*** parse function transformations ***)
let transform_exp bs read_tys exp : exp = 
  match exp.e with 
  | ECall{f; call_kind=CFun} -> (
    match (eval_exp f |> extract_vsymbol |> Cid.names) with 
    | ["parse";"peek"] -> 
      read_tys := exp.ety::(!read_tys);
      call_peek exp.ety bs    
    | ["parse";"skip"] -> 
      read_tys := exp.ety::(!read_tys);
      call_skip exp.ety bs
    | ["parse";"read"] ->
      read_tys := exp.ety::(!read_tys);
      call_read exp.ety bs
    | ["Payload"; "parse"] -> 
      (* this is annoying because of how we currently cast 
         the byte_t params to pointers but skip inner variables. 
         TODO: clean up. *)
      let payload_arg = arg exp in
      let arg_ety = payload_arg.ety in
      let arg_ety = tref arg_ety in 
      let payload_arg = {payload_arg with ety=arg_ety} in
      let res = ederef payload_arg in  
      res 
    | _ -> exp
  )
  | _ -> exp
;;

let rv_ty = 
  tabstract "parse_ret_t"
  @@trecord_pairs 
[
  id"next_ev", tevent;
  id"success", tint 8
]
;;

let transform_stmt bs rv read_tys stmt : statement = 
  match stmt.s with 
  (* generate(foo(...)); --> rv.next_ev = foo(...); rv.success=1; *)
  | SUnit(exp) when is_egen_self exp -> 
    sseq 
      (sassign_exp (rv/->id"next_ev") (arg exp))
      (sassign_exp (rv/->id"success") (eval@@vint 1 8))
  (* drop(); --> rv.success=0; *)
  | SUnit(exp) when is_ecall_cid exp (Cid.create ["parse";"drop"]) -> 
    sassign_exp (rv/->id"success") (eval@@vint 0 8)
  (* transform builtin calls *)
  | _ -> CCoreTransformers.subst_exp#visit_statement (transform_exp bs read_tys) stmt
;;

(* transform all types that represent bytes_t into bytes_t *)
let transform_bytestrings = 
  let is_bytes_t_placeholder ty = match ty.raw_ty with 
    | TBits{ternary=false; len=1500} -> true
    | TBuiltin(cid, _) when (Cid.names cid = ["Payload"; "t"]) -> 
      true
    | TName(cid) when (Cid.names cid = ["Payload"; "t"]) -> 
      print_endline ("TName Payload.t?");
      exit 1;
    | _ -> false
  in  
  object (_) inherit [_] s_map as super 
  method! visit_ty () ty = 
    let ty = super#visit_ty () ty in 
    if is_bytes_t_placeholder ty 
      then bytes_t
      else ty 
  end
;;
let process_decl read_tys decl = 
  let rv = evar (cid"rv") (tref rv_ty) in
  match extract_dparser_opt decl with 
  | None -> decl
  | Some(id, _, params, body) -> 
    let byte_s_params = List.filter 
      (fun (_, ty) -> 
        match ty.raw_ty with 
        | TAbstract(tcid, _) when Cid.equal tcid (cid"bytes_t") -> true
        | _ -> false)
      params
    in
    let bs_param = match byte_s_params with 
      | [] -> err "parser with no bytestring / packet input arg?"
      | [(bs_id, bs_ty)] -> bs_id, tref bs_ty
      | _ -> err "parser with multiple bytestring / packet input args?"
    in
    let bs = param_evar bs_param in
    let body = stmts [
      CCoreTransformers.subst_statement#visit_statement (transform_stmt bs rv read_tys) body; (* do the statement-level transforms *)
    ]
    in
    (* convert the bs parameter to a ref *)
    let params = List.map 
      (fun (pid, ty) -> 
        match ty.raw_ty with 
        | TAbstract(tcid, _) when Cid.equal tcid (cid"bytes_t") -> (pid,tref ty)
        | _ -> (pid, ty))
      params
    in
    (* add the return parameter *)
    let params = params@[extract_evar_id rv] in
    (* return type and body change *)
    dfun id tunit params body 
;;



let process decls = 
  print_endline ("implementing parser functions");
  let decls = transform_bytestrings#visit_decls () decls in
  print_endline ("---- transformed Payload.t and bits[1500] to bytestrings ----");
  let read_tys = ref [] in
  let decls = List.map (process_decl read_tys) decls in
  (* finally, add new declarations for: 1) bytes_t; 2) skip, peek, and read functions for all types read *)
  let read_tys = MiscUtils.unique_list_of_eq (equiv_tys) !read_tys in
  let read_usertys, read_primitive_tys = List.fold_left 
    (fun (read_usertys, read_tys) ty -> 
      match ty.raw_ty with TAbstract(cid, _) -> read_usertys@[Cid.to_string cid, ty], read_tys
      | _ -> read_usertys, read_tys@[ty])
    ([], [])
    read_tys
  in
  let generated_decls = 
    decl_tabstract bytes_t::
    decl_tabstract rv_ty::
    ((List.map  (*parse helpers for all primitive types *)
        (fun ty -> [mk_skip ty;mk_peek ty;mk_read ty])
        read_primitive_tys) 
      |> List.flatten)    
    @
    (List.fold_left (* the input program, with user type readers placed 
                       just after the corresponding type declarations *)
      (fun decls decl -> 
        match decl.d with 
        | DTy(cid, _) -> 
          let user_ty_opt = List.assoc_opt (Cid.to_string cid) read_usertys in
          let new_decls = match user_ty_opt with 
            | Some(ty) -> [decl]@[mk_skip ty;mk_peek ty;mk_read ty]
            | None -> [decl]
          in
          decls@new_decls
        | _ -> decls@[decl])
      []
      decls)
  in
  let decls = generated_decls@decls in
  decls
;;
