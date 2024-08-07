(* implementation of parse functions *)

(* 
Design:  

Parser functions: 
  A parse function is originally a function that takes a 
  bytestring and generates an event or calls drop. 
  e.g.:  parser foo(packet_t bs)
  It is transformed into a function that takes a 
  reference to a bytestring and a reference to an output
  event. It fills the output events, updates the bytestring, 
  and returns a boolean indicating success / failure. 

Bytestrings:
  A bytestring represents the unparsed part of a packet. 
  It is a record with three pointers: 
    typedef struct packet_t {
      char* start;  // pointer to start of buffer
      char* cur;    // pointer to current position
      char* end;    // pointer to end of buffer
    } packet_t;
  - Before parsing, "cur" should equal start.
  - After parsing, "cur" points to the payload.
  - "cur" should always be less than or equal to end.

Parse helpers: 
  There are three parse methods that operate on bytestrings:
    skip, peek, and read. 
  The functions are polymorphic on the type of the extracted 
  or skipped value. An instance is generated for each 
  type used in a parser.
  void skip_int(packet_t* bs) {
    bs->cur = bs->cur + sizeof(int);
  }
  int peek_int(packet_t* bs) {
    return ((int * ) (bs->cur))[0];
  }
  int read_int(packet_t* bs) {
      int rv = ((int 
      skip_int(bs);
      return rv;
  } 

Bits:
  The alignreads pass ensures that all contiguous reads 
  in a parser are byte aligned, and wraps them in byte-aligned structs:

  align_bit_read_block pkt bit_read_block =  (* bit_read_block is the block of read commands that must be aligned *)
    let bitrec = fresh_rec bit_read_block in (* make the record *)
    let read_stmt = sread bitrec pkt in (* read the record *)
    let id_subst_exps = List.map (fun stmt -> assigned_id stmt, bitrec/.(assigned_id stmt) ) bit_read_block in (* replace each variable with its struct field *)
    
    let assign_stmts 
    for stmt in bit_read_block: 
      stmt' =  (bitrec/.(var_of stmt))
    for (sz, id) in bit_read_block:



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
      value of the event and the packet_t of the packet. 
         - what happens next is platform dependent. 
          - in ebpf, it will shrink or grow the buffer of the 
            current packet to hold the new event. 
          - in userspace c it will likely allocate 
            output buffer and copy the event header and payload there. 
*)
open CCoreSyntax
open CCoreExceptions
open CCoreUtils

let n_bytes ty = eval (vint (size_of_ty ty) 32) ;;

(*** primary type and code generators ***)
(* note: these can be made "safe" for ebpf by
   adding runtime checks on the bounds (cur + $ty_size <= end) *)
(* typedef struct packet_t {
  char* start;  // pointer to start of buffer
  char* cur;    // pointer to current position
  char* end;    // pointer to end of buffer
} packet_t; *)
let packet_t = 
  tabstract 
    "packet_t"@@trecord
      [
        cid"start", tref tchar;
        cid"payload", tref tchar;
        cid"end", tref tchar;
      ]
;;
(* packet_t param for generated functions *)
let _bs_param = (cid"bs", tref packet_t)
(* packet_t var within generated functions *)
let _bs = param_evar _bs_param


(* void skip_int(packet_t* bs) {
    bs->cur = bs->cur + sizeof(int);
} *)
(* generate a skip function for the given type *)
let skip_name ty = cid_for_ty (cid"skip") ty
let skip_ty = tfun [snd _bs_param] tunit
let skip_var ty = efunref (skip_name ty) skip_ty
let call_skip ty bs = ecall (skip_var ty) [bs]
let mk_skip ty = 
  dfun (skip_name ty)
    tunit
    [_bs_param]
    @@stmts [
      sassign_exp (_bs/->cid"payload") ((_bs/->cid"payload")/+(n_bytes ty));
      sret_none
    ]
;;

(* int peek_int(packet_t* bs) {
    return ((int * ) *(bs->cur));
} *)
(* generate a peek function for the given type *)
let peek_name ty = cid_for_ty (cid"peek") ty
let peek_ty ty = tfun [snd _bs_param] ty
let peek_var ty = efunref (peek_name ty) (peek_ty ty)
let call_peek ty bs = ecall (peek_var ty) [bs]
let mk_peek ty = 
  dfun (peek_name ty)
    ty
    [_bs_param]
    @@stmts [
      (* get pointer, cast to correct pointer type, deref *)
      sret @@ (ederef @@ ecast (tref ty) @@ (_bs/->cid"payload"))
    ]
;;

(* int read_int(packet_t* bs) {
    int rv = peek(bs);
    skip_int(bs);
    return rv;
} *)
(* generate a read function for the given type *)
let read_name ty = cid_for_ty (cid"read") ty
let read_ty ty = tfun [snd _bs_param] ty
let read_var ty = efunref (read_name ty) (read_ty ty)
let call_read ty bs = ecall (read_var ty) [bs]
let mk_read ty = 
  dfun (read_name ty)
    ty
    [_bs_param]
    @@stmts [
      slocal (cid"rv") ty @@ call_peek ty _bs;
      sunit @@ call_skip ty _bs;
      sret @@ evar (cid"rv") ty
    ]
;;

(*** parse function transformations ***)
(* the parser fills a next event and returns a success flag *)
let parser_cid = Cid.create ["parse_event"] ;;
let parser_out_event_param = cid"next_event", tref tevent;;
let parser_out_event = param_evar parser_out_event_param;;
let parser_ret_ty = tint 8
let parser_ret_cont = eval@@vint 1 8
let parser_ret_drop = eval@@vint 0 8

(* transform all types that represent packet_t into packet_t *)
let transform_bytestrings = 
  let is_packet_t_placeholder ty = match ty.raw_ty with 
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
    if is_packet_t_placeholder ty 
      then packet_t
      else ty 
  end
;;
(* transform expressions that call parse helpers into 
   calls to the appropriate generated function *)
let transform_exp pkt_bytestring read_tys exp : exp = 
  let orig_exp = exp in
  match exp.e with 
  | ECall{f; call_kind=CFun} -> (
    match (eval_exp f |> extract_vsymbol |> Cid.names) with 
    | ["parse";"peek"] -> 
      read_tys := exp.ety::(!read_tys);
      call_peek exp.ety pkt_bytestring
    | ["parse";"skip"] -> 
      read_tys := exp.ety::(!read_tys);
      call_skip exp.ety pkt_bytestring
    | ["parse";"read"] ->
      read_tys := exp.ety::(!read_tys);
      call_read exp.ety pkt_bytestring
    | ["Payload"; "parse"] -> 
      (* this is annoying because of how we currently cast 
         the byte_t params to pointers but skip inner variables. 
         TODO: clean up. *)
      let payload_arg = arg exp in
      let arg_ety = payload_arg.ety in
      let arg_ety = tref arg_ety in 
      let payload_arg = {payload_arg with ety=arg_ety} in
      let res = ederef payload_arg in  
      eimpl_wrap res orig_exp
    | _ -> exp
  )
  | _ -> exp
;;
(* transform statements in a parser *)
let transform_stmt pkt_bytestring read_tys stmt : statement = 
  match stmt.s with 
  (* generate becomes return *)
  | SUnit(exp) when is_egen_self exp -> 
    sseq 
      (sassign_exp (ederef parser_out_event) (arg exp))
      (sret parser_ret_cont)
  (* drop becomes return *)
  | SUnit(exp) when is_ecall_cid exp (Cid.create ["parse";"drop"]) -> 
    (sret parser_ret_drop)
  (* builtin calls get transformed *)
  | _ -> CCoreTransformers.subst_exp#visit_statement 
    (transform_exp pkt_bytestring read_tys) 
    stmt
;;
(* transform the parser *)
let transform_parser id params body = 
  let parsed_tys_acc = ref [] in (* accumulate types that we need to make helpers for *)
  (* if the parser's name is main, change it to parse_event *)
  let id = if (Cid.names id = ["main"]) then parser_cid else id in
  (* a parser should already have a packet bytestring, 
     but it needs to be converted to a reference. *)
  let params, byte_s_params = List.fold_left 
    (fun (params, pkt_param) (pid, ty) -> 
      match ty.raw_ty with 
      | TAbstract(tcid, _) when Cid.equal tcid (cid"packet_t") 
          -> params@[(pid,tref ty)], Some((pid,tref ty))
      | _ -> params@[(pid, ty)], pkt_param  )
    ([], None)
    params
  in
  (* add the out event parameter *)
  let params = params@[parser_out_event_param] in

  (* transform the body *)
  let pkt_param = Option.get byte_s_params in     
  let pkt_var = param_evar pkt_param in
  let body = CCoreTransformers.subst_statement#visit_statement 
    (transform_stmt pkt_var parsed_tys_acc) 
    body
  in
  let body = if ends_with_smatch body 
    then sseq body (sret parser_ret_drop)
    else body
  in

  (* return type and body change *)
  !parsed_tys_acc, dfun id parser_ret_ty params body   
;;

let process_decl read_tys decl = 
  match extract_dparser_opt decl with 
  | None -> decl
  | Some(id, _, params, body) -> 
    let new_read_tys, decl = transform_parser id params body in
    read_tys := (!read_tys)@(new_read_tys);
    decl
;;

let process decls = 
  let decls = transform_bytestrings#visit_decls () decls in
  let read_tys = ref [] in
  let decls = List.map (process_decl read_tys) decls in
  (* finally, add new declarations for: 1) packet_t; 2) skip, peek, and read functions for all types read *)
  let read_tys = MiscUtils.unique_list_of_eq (equiv_tys) !read_tys in
  let read_usertys, read_primitive_tys = List.fold_left 
    (fun (read_usertys, read_tys) ty -> 
      match ty.raw_ty with TAbstract(cid, _) -> read_usertys@[Cid.to_string cid, ty], read_tys
      | _ -> read_usertys, read_tys@[ty])
    ([], [])
    read_tys
  in
  (* rebuilding the decls is convoluted because we have to put things in 
     a particular order. 
     - packet_t at the top
     - primitive parse helpers after packet_t
     - parse helpers for _user defined types_ must go after those type defs *)
  let primitive_parse_helpers = (List.map 
      (fun ty -> [mk_skip ty;mk_peek ty;mk_read ty])
      read_primitive_tys) 
    |> List.flatten    
  in
  let decls = 
    decl_tabstract packet_t
    ::primitive_parse_helpers
    @(List.fold_left (* the input program, with user type readers placed 
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
  decls
  (* let decls = 
    (List.fold_left (fun (decls, fin) decl -> 
      if fin then (decls@[decl], fin) else 
      match decl.d with 
      | DEvent _ -> decls@[decl_tabstract parser_ret_ty; decl], true
      | _ -> decls@[decl], fin)
      ([], false)
      (List.rev decls))
    |> fst |> List.rev
  in
  decls *)
;;
