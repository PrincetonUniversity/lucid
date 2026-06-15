(* implementation of parse functions, in two phases:

   Phase 1 (process): produce the *value-semantic* parser. The unparsed packet
     is the opaque bytes ADT (TBytes); reads/peeks/skips become Peek/Skip ops
     that thread the bytes as a value; a parser returns (ok, event); generate
     becomes "return (bytes_ok pkt, event)" and drop becomes "return (false, _)".

   Phase 2 (lower): lower the value-semantic parser to the pointer form -- the
     packet_t record with start/cur/end pointers, the skip/peek/read helpers,
     and the (event_t* out-param, int8 return) calling convention. A read shows
     up at the waist as the pair "s = peek<ty>(pkt); pkt = skip<ty>(pkt)", which
     this phase fuses back into a single read_<ty>(packet) so the C is unchanged.

   Bytestrings (lowered form):
     typedef struct packet_t { char* start; char* payload; char* end; } packet_t;
   Parse helpers (lowered form): skip/peek/read, one instance per type read.
*)
open CCoreSyntax
open CCoreExceptions
open CCoreUtils

let n_bytes ty = eval (vint (size_of_ty ty) 32) ;;

(* ===== lowered (pointer) representation: packet_t + parse helpers ===== *)
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
let _bs = param_evar _bs_param

(* skip_<ty>(packet_t* bs) { bs->payload += sizeof(ty); } *)
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

(* peek_<ty>(bs): return the next ty at bs->payload, without advancing *)
let peek_name ty = cid_for_ty (cid"peek") ty
let peek_ty ty = tfun [snd _bs_param] ty
let peek_var ty = efunref (peek_name ty) (peek_ty ty)
let call_peek ty bs = ecall (peek_var ty) [bs]
let mk_peek ty =
  dfun (peek_name ty)
    ty
    [_bs_param]
    @@stmts [
      (* bounds guard: never read past the end. On a too-short packet return a
         default value; the parser drops the packet via its ok check (a consume
         past the end pushes payload past end, so payload <= end becomes false). *)
      sif (eop More [(_bs/->cid"payload")/+(n_bytes ty); _bs/->cid"end"])
        (sret (eval (default_value ty)))
        (sret (ederef @@ ecast (tref ty) @@ (_bs/->cid"payload")))
    ]
;;

(* read_<ty>(packet_t* bs) { ty rv = peek(bs); skip(bs); return rv; } *)
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

(* parser conventions (lowered form) *)
let parser_cid = Cid.create ["parse_event"] ;;
let parser_out_event_param = cid"next_event", tref tevent;;
let parser_out_event = param_evar parser_out_event_param;;
let parser_ret_ty = tint 8
let parser_ret_cont = eval@@vint 1 8
let parser_ret_drop = eval@@vint 0 8


(* ============================ Phase 1 ============================= *)
(* produce the value-semantic parser *)

(* Payload.t / 1500-bit placeholder -> the opaque TBytes type *)
let to_tbytes =
  let is_placeholder ty = match ty.raw_ty with
    | TBits{ternary=false; len=1500} -> true
    | TBuiltin(cid, _) when (Cid.names cid = ["Payload"; "t"]) -> true
    | _ -> false
  in
  object (_) inherit [_] s_map as super
    method! visit_ty () ty =
      let ty = super#visit_ty () ty in
      if is_placeholder ty then tbytes else ty
  end
;;

(* name of the builtin a call expression invokes, if any *)
let parse_op_name e = match e.e with
  | ECall{f; call_kind=CFun} -> (try Some (eval_exp f |> extract_vsymbol |> Cid.names) with _ -> None)
  | _ -> None
;;

(* the value-semantic "drop": return (false, <placeholder event>). The event is
   never used (lower_parser turns drop into "return 0" with no event write); we
   use a VSymbol so it types directly as tevent without referencing a real event
   constructor (a VEvent here would be rewritten by CCoreEvents into a call to a
   nonexistent constructor). *)
let no_event = { v = VSymbol(Cid.create ["_no_event"], tevent); vty = tevent; vspan = Span.default }
let drop_return = sret (etuple [eval (vbool false); eval no_event])

(* thread the packet value [pkt] through a parser body, rewriting the parse
   actions (which all operate on the ambient packet variable) into ops. *)
let rec thread_body pkt stmt =
  let pktv = evar pkt tbytes in
  match stmt.s with
  | SSeq(s1, s2) -> sseq (thread_body pkt s1) (thread_body pkt s2)
  | SMatch(es, branches) -> smatch es (List.map (fun (ps, b) -> (ps, thread_body pkt b)) branches)
  | SIf(e, s1, s2) -> sif e (thread_body pkt s1) (thread_body pkt s2)
  | SAssign(OLocal(lid, ty), e) -> (
    match parse_op_name e with
    (* read = peek the value, then advance past it *)
    | Some ["parse"; "read"] -> sseq (slocal lid ty (epeek ty pktv)) (sassign_exp pktv (eskip ty pktv))
    | Some ["parse"; "peek"] -> slocal lid ty (epeek ty pktv)
    | _ -> stmt)
  | SUnit(e) -> (
    match parse_op_name e with
    | Some ["parse"; "skip"] -> sassign_exp pktv (eskip e.ety pktv)
    | Some ["parse"; "drop"] -> drop_return
    | _ -> if is_egen_self e then sret (etuple [ebytesok pktv; arg e]) else stmt)
  | _ -> stmt
;;

let process_parser id params body =
  let id = if (Cid.names id = ["main"]) then parser_cid else id in
  let pkt = match List.find_opt (fun (_, ty) -> is_tbytes ty) params with
    | Some (pid, _) -> pid
    | None -> err "[CCoreParse] parser has no bytes parameter"
  in
  let body = thread_body pkt body in
  (* a match that doesn't generate falls through to a drop *)
  let body = if ends_with_smatch body then sseq body drop_return else body in
  dparser id (ttuple [tbool; tevent]) params body
;;

let process decls =
  let decls = to_tbytes#visit_decls () decls in
  List.map
    (fun decl ->
      match extract_dparser_opt decl with
      | None -> decl
      | Some(id, _, params, body) -> process_parser id params body)
    decls
;;


(* ============================ Phase 2 ============================= *)
(* lower the value-semantic parser to the pointer form *)

let rec flatten_seq s = match s.s with
  | SSeq(a, b) -> flatten_seq a @ flatten_seq b
  | SNoop -> []
  | _ -> [s]
let rec unflatten_seq = function
  | [] -> snoop
  | [s] -> s
  | s :: tl -> sseq s (unflatten_seq tl)

let rec lower_body pkt out_event read_tys stmt =
  match stmt.s with
  | SSeq _ -> unflatten_seq (lower_list pkt out_event read_tys (flatten_seq stmt))
  | _ -> lower_one pkt out_event read_tys stmt

and lower_list pkt out_event read_tys stmts =
  let pktv = evar pkt (tref packet_t) in
  match stmts with
  (* fuse: s = peek<ty>(pkt); pkt = skip<ty>(pkt)  -->  s = read<ty>(packet) *)
  | { s = SAssign(OLocal(lid, ty), { e = EOp(Peek pty, _); _ }); _ }
    :: { s = SAssign(OAssign _, { e = EOp(Skip sty, _); _ }); _ }
    :: rest
    when (equiv_tys pty sty) ->
      read_tys := ty :: !read_tys;
      (slocal lid ty (call_read ty pktv)) :: lower_list pkt out_event read_tys rest
  | s1 :: rest -> (lower_one pkt out_event read_tys s1) :: lower_list pkt out_event read_tys rest
  | [] -> []

and lower_one pkt out_event read_tys stmt =
  let pktv = evar pkt (tref packet_t) in
  match stmt.s with
  | SAssign(OLocal(lid, ty), { e = EOp(Peek _, _); _ }) ->
      read_tys := ty :: !read_tys;
      slocal lid ty (call_peek ty pktv)
  | SAssign(OAssign _, { e = EOp(Skip ty, _); _ }) ->
      read_tys := ty :: !read_tys;
      sunit (call_skip ty pktv)
  | SRet(Some { e = ETuple [ ok_e; ev ]; _ }) -> (
      match ok_e.e with
      (* drop: return 0 (no event written) *)
      | EVal { v = VBool false; _ } -> sret parser_ret_drop
      (* generate: *next_event = ev; return ok  (ok = no consume overran the
         packet, i.e. payload <= end -- this is the BytesOk lowering) *)
      | _ ->
        let ok = eop Leq [pktv/->cid"payload"; pktv/->cid"end"] in
        sseq (sassign_exp (ederef out_event) ev) (sret ok))
  | SMatch(es, branches) -> smatch es (List.map (fun (ps, b) -> (ps, lower_body pkt out_event read_tys b)) branches)
  | SIf(e, s1, s2) -> sif e (lower_body pkt out_event read_tys s1) (lower_body pkt out_event read_tys s2)
  | _ -> stmt
;;

let lower_parser id ret_ty params body =
  let read_tys = ref [] in
  (* the parser's return type is (bool, event); take the (already-lowered) event
     type from it so the out-event param uses event_t, not raw TEvent. *)
  let ev_ty = match ret_ty.raw_ty with
    | TTuple [_; ev_ty] -> ev_ty
    | _ -> err "[CCoreParse.lower] parser return type is not (bool, event)"
  in
  let out_event_param = (cid"next_event", tref ev_ty) in
  let out_event = param_evar out_event_param in
  let pkt = match List.find_opt (fun (_, ty) -> is_tbytes ty) params with
    | Some (pid, _) -> pid
    | None -> err "[CCoreParse.lower] parser has no bytes parameter"
  in
  let body = lower_body pkt out_event read_tys body in
  (* bytes param -> packet_t* ; add the out-event param ; return int8 *)
  let params = List.map (fun (pid, ty) -> if is_tbytes ty then (pid, tref packet_t) else (pid, ty)) params in
  let params = params @ [out_event_param] in
  !read_tys, dfun id parser_ret_ty params body
;;

let lower decls =
  let read_tys = ref [] in
  let decls = List.map
    (fun decl ->
      match extract_dparser_opt decl with
      | None -> decl
      | Some(id, ret_ty, params, body) ->
        let rts, d = lower_parser id ret_ty params body in
        read_tys := (!read_tys) @ rts;
        d)
    decls
  in
  (* add declarations for packet_t and the skip/peek/read helpers, in the order
     C needs: packet_t first, primitive readers next, user-type readers right
     after the corresponding type declarations. *)
  let read_tys = MiscUtils.unique_list_of_eq (equiv_tys) !read_tys in
  let read_usertys, read_primitive_tys = List.fold_left
    (fun (read_usertys, read_tys) ty ->
      match ty.raw_ty with
      | TAbstract(cid, _) -> read_usertys @ [Cid.to_string cid, ty], read_tys
      | _ -> read_usertys, read_tys @ [ty])
    ([], [])
    read_tys
  in
  let primitive_parse_helpers =
    List.map (fun ty -> [mk_skip ty; mk_peek ty; mk_read ty]) read_primitive_tys
    |> List.flatten
  in
  decl_tabstract packet_t
  :: primitive_parse_helpers
  @ (List.fold_left
      (fun decls decl ->
        match decl.d with
        | DTy(cid, _) ->
          let user_ty_opt = List.assoc_opt (Cid.to_string cid) read_usertys in
          let new_decls = match user_ty_opt with
            | Some(ty) -> [decl] @ [mk_skip ty; mk_peek ty; mk_read ty]
            | None -> [decl]
          in
          decls @ new_decls
        | _ -> decls @ [decl])
      []
      decls)
;;
