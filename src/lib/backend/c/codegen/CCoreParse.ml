(* the packet (de)serialization codec: parser + deparser, each in two phases.

   The parser and deparser are duals over one packet substrate: the parser
   *consumes* the input with Read/Peek/Skip ops, the deparser *produces* an
   output with Write ops that prepend to the front.

   Phase 1 (process) -- pointer-free forms:
       parser   -- the unparsed packet is the opaque TPacket; parse.read/peek/skip
                   calls become Read/Peek/Skip ops on it; a parser returns
                   (ok, event): generate becomes "return (bytes_ok pkt, event)",
                   drop "return (false, _)".
       deparser -- deparse_event is synthesized from the event variant's arms:
                   each arm Writes its fields (and, for background events, the
                   event tag + ethernet framing).

   Phase 2 (lower) -- the C form: packet_t (a start/cursor/end/bit_off view over
     the driver's buffer) plus four width-generic bit-codec helpers
     (read/peek/skip/write_bits). Each op lowers 1:1 to a helper call; the
     parser gets the (packet_t*, event_t* out-param) -> int8 calling convention,
     the deparser gets (event_t*, packet_t ptr) -> void.

   TODO: deparser payload support (currently only event headers serialize;
   payload bytes are carried by the driver's buffer copy). *)
open CCoreSyntax
open CCoreExceptions
open CCoreUtils



(* parser conventions (lowered form) *)
let parser_cid = Cid.create ["parse_event"] ;;
(* deparser conventions *)
let deparse_id = Cid.create ["deparse_event"]
let ev_out_cid = cid "ev_out"
let buf_out_cid = cid "buf_out"


(* ============================================================================
   Phase 1: value-semantic forms (above the value-semantics boundary)
   ============================================================================ *)

(* -------------------------------- parser --------------------------------- *)
(* produce the value-semantic parser *)

(* Payload.t -> the opaque TPacket type (the 1500-bit placeholder form is
   already translated to TPacket by CoreToCCore) *)
let to_tpacket =
  let is_placeholder ty = match ty.raw_ty with
    | TBuiltin(cid, _) when (Cid.names cid = ["Payload"; "t"]) -> true
    | _ -> false
  in
  object (_) inherit [_] s_map as super
    method! visit_ty () ty =
      let ty = super#visit_ty () ty in
      if is_placeholder ty then tpacket else ty
  end
;;

(* name of the builtin a call expression invokes, if any *)
let parse_op_name e = match e.e with
  | ECall{f; call_kind=CFun} -> (try Some (extract_evar f |> fst |> Cid.names) with _ -> None)
  | _ -> None
;;

(* the value-semantic "drop": return (false, <placeholder event>). The event is
   never used (lower_parser turns drop into "return 0" with no event write); the
   VSymbol types directly as tevent without referencing a real event constructor. *)
let no_event = { v = VSymbol(Cid.create ["_no_event"], tevent); vty = tevent; vspan = Span.default }
let drop_return = sret (etuple [eval (vbool false); eval no_event])

(* Payload.parse(pkt) captures "the rest of the packet" as a Payload.t. In the C
   backend the tail is carried implicitly by the driver's copy_packet (which copies the
   whole input into the out buffer, after which deparse prepends the new header), so the
   captured value carries no data here -- it is purely the marker that makes the event a
   payload event (recorded as meta.has_payload upstream). MiscCorePasses.implicit_payloads
   has already stripped the Payload.t event arg + the generate arg that used it, leaving
   the `Payload.t x = Payload.parse(pkt)` parser local dead, so we drop it here. *)
let is_payload_parse e = match parse_op_name e with
  | Some ["Payload"; "parse"] | Some ["Payload_parse"] -> true
  | _ -> false
;;

(* rewrite the parse actions (which all operate on the ambient packet variable
   [pkt]) into bytes ops on the cursor resource. read/skip advance the cursor in
   place, so nothing is threaded/rebound -- pkt is read by each op. *)
let rec thread_body pkt stmt =
  let pktv = evar pkt tpacket in
  match stmt.s with
  | SSeq(s1, s2) -> sseq (thread_body pkt s1) (thread_body pkt s2)
  | SMatch(es, branches) -> smatch es (List.map (fun (ps, b) -> (ps, thread_body pkt b)) branches)
  | SIf(e, s1, s2) -> sif e (thread_body pkt s1) (thread_body pkt s2)
  (* drop a dead `Payload.t x = Payload.parse(pkt)` local (see note above) *)
  | SAssign(OLocal(_, _), e) when is_payload_parse e -> snoop
  | SUnit(e) when is_payload_parse e -> snoop
  | SAssign(OLocal(lid, ty), e) -> (
    match parse_op_name e with
    (* read = decode one value and advance the cursor past it, in place *)
    | Some ["parse"; "read"] -> slocal lid ty (eread ty pktv)
    | Some ["parse"; "peek"] -> slocal lid ty (epeek ty pktv)
    | _ -> stmt)
  | SUnit(e) -> (
    match parse_op_name e with
    | Some ["parse"; "skip"] -> sunit (eskip e.ety pktv)
    | Some ["parse"; "drop"] -> drop_return
    | _ -> if is_egen_self e then sret (etuple [ebytesok pktv; arg e]) else stmt)
  | _ -> stmt
;;

let process_parser id params body =
  let id = if (Cid.names id = ["main"]) then parser_cid else id in
  let pkt = match List.find_opt (fun (_, ty) -> is_tpacket ty) params with
    | Some (pid, _) -> pid
    | None -> err "[CCoreParse] parser has no bytes parameter"
  in
  let body = thread_body pkt body in
  (* a match that doesn't generate falls through to a drop *)
  let body = if ends_with_smatch body then sseq body drop_return else body in
  dparser id (ttuple [tbool; tevent]) params body
;;

let process_parse decls =
  let decls = to_tpacket#visit_decls () decls in
  List.map
    (fun decl ->
      match extract_dparser_opt decl with
      | None -> decl
      | Some(id, _, params, body) -> process_parser id params body)
    decls
;;

(* ------------------------------- deparser -------------------------------- *)
(* synthesize deparse_event from the event variant's arms. Writes prepend, so
   everything is emitted back-to-front and the wire order is
   [dst mac][src mac][ethertype][tag][fields]:

     fn bytes deparse_event(event ev_out, bytes out) {
       match (ev_out) {
         | foo(a, b) ->
             write<tb>(out, b);  write<ta>(out, a);   // fields, last first
             if (ev_out.meta.is_packet == 0) {        // background events also
               write<u16>(out, foo_tag);              //   get the tag + eth
               ..ethernet preamble writes..           //   framing (packet
             }                                        //   events: fields only)
             return out;
       }
     }

   Arm fields and tags come from the variant type; is_packet is read at runtime
   from the event's meta (stamped by mk_event). The match itself is lowered
   later by CCoreVariants, like any other event match. *)

(* the ethernet framing for background events -- the same bytes the interpreter
   writes (InterpDeparsing.lucid_eth_fields): dst mac = 1, src mac = 2 (48 bits
   each), ethertype = LUCID_ETHERTY (666). *)
let eth_preamble_writes do_write =
  [
    do_write (tint 16) (eval (vint Constants.lucid_ety_int 16));  (* ethertype *)
    do_write (tint 48) (eval (vint 2 48));                        (* src mac   *)
    do_write (tint 48) (eval (vint 1 48));                        (* dst mac   *)
  ]
;;

let process_deparse decls =
  let arms =
    match CCoreVariants.find_variant_sigs decls with
    | Some sigs -> List.map CCoreVariants.arm_of_sig sigs
    | None -> err "[CCoreParse] no event_variant type definition found"
  in
  let ev_out = evar ev_out_cid tevent in
  let bufv = evar buf_out_cid tpacket in
  (* write<ty>(out, v) -- prepend v of type ty to the front of the bytes, in place *)
  let do_write ty v = sunit (ewrite ty bufv v) in
  let mk_arm (arm : CCoreVariants.arm) =
    let fields = arm.params in
    (* last field first, so wire order matches declaration order *)
    let field_writes =
      List.rev_map (fun (fid, fty) -> do_write fty (evar fid fty)) fields
    in
    let preamble_writes =
      (do_write (tint event_tag_size)
         (eval (vint arm.tag event_tag_size)))
      :: eth_preamble_writes do_write
    in
    let preamble_stmt =
      sif (event_is_packet ev_out /== eval (vint 0 8))
        (stmts preamble_writes) snoop
    in
    let body = stmts (field_writes @ [ preamble_stmt; sret bufv ]) in
    let pat = PVariant { event_id = arm.ctor; params = arm.params } in
    ([ pat ], body)
  in
  let body = smatch [ event_data ev_out ] (List.map mk_arm arms) in
  let deparse_decl =
    dfun deparse_id tpacket [ (ev_out_cid, tevent); (buf_out_cid, tpacket) ] body
  in
  decls @ [ deparse_decl ]
;;

(* ------------------------------- combined -------------------------------- *)
let process decls = process_deparse (process_parse decls)


(* ============================================================================
   Phase 2: lower the value-semantic forms to the pointer (packet_t) form
   ============================================================================ *)

(* ===== lowered (pointer) representation: packet_t + the bit-packed codec =====
   The packet is a byte buffer addressed at the BIT level: `cursor` is the byte
   holding the current position and `bit_off` (0-7) the bit within it. Fields pack
   contiguously, MSB-first (network/big-endian bit order), matching Lucid's
   interpreter / Tofino wire layout. The four primitives below read/write n bits at
   the cursor; the parser's read/peek/skip advance forward, the deparser's write
   PREPENDS (moves the front back by n, then fills). They are raw C with simple bit
   loops -- restriction R1' (CCoreWellformedC.check_event_fields) guarantees every
   field crosses a byte boundary only when aligned to one, so these could be
   specialized to a constant load/shift/mask later; the loops here stay general. *)
let packet_t =
  tabstract
    "packet_t"@@trecord
      [
        (* A pure *view* over a byte buffer: the buffer's ownership lives with the
           driver's queue element (an rte_mbuf under DPDK; a static buffer under
           pcap/rawsock), never here -- so packet_t carries no handle to it. *)
        cid"start", tref (tint 8);
        cid"cursor", tref (tint 8);
        cid"end", tref (tint 8);
        cid"bit_off", tint 32;
      ]
;;

let read_bits_cid  = Cid.create ["read_bits"]
let peek_bits_cid  = Cid.create ["peek_bits"]
let skip_bits_cid  = Cid.create ["skip_bits"]
let write_bits_cid = Cid.create ["write_bits"]

(* the codec primitives, emitted once (raw C; they reference packet_t by name) *)
let codec_helpers = [
  dforiegn {|
uint64_t read_bits(packet_t* bs, int n) {
    /* assemble n bits MSB-first from (cursor,bit_off), then advance forward n bits.
       a read past the end yields 0 bits and still advances, so the parser drops via
       its `cursor <= end` check. */
    uint64_t v = 0;
    for (int i = 0; i < n; i++) {
        int b = bs->bit_off + i;
        int in_bounds = (bs->cursor + (b >> 3)) < bs->end;
        v = (v << 1) | (in_bounds ? ((bs->cursor[b >> 3] >> (7 - (b & 7))) & 1) : 0);
    }
    int tot = bs->bit_off + n;
    bs->cursor += tot >> 3;
    bs->bit_off = tot & 7;
    return v;
}|};
  dforiegn {|
uint64_t peek_bits(packet_t* bs, int n) {
    uint64_t v = 0;
    for (int i = 0; i < n; i++) {
        int b = bs->bit_off + i;
        int in_bounds = (bs->cursor + (b >> 3)) < bs->end;
        v = (v << 1) | (in_bounds ? ((bs->cursor[b >> 3] >> (7 - (b & 7))) & 1) : 0);
    }
    return v;
}|};
  dforiegn {|
void skip_bits(packet_t* bs, int n) {
    int tot = bs->bit_off + n;
    bs->cursor += tot >> 3;
    bs->bit_off = tot & 7;
}|};
  dforiegn {|
void write_bits(packet_t* bs, uint64_t v, int n) {
    /* prepend: move the front back n bits, then write n bits MSB-first. clear-then-set
       each bit so a field sharing a boundary byte with an already-written neighbour
       composes correctly without pre-zeroing the buffer. */
    int no = (int)bs->bit_off - n;
    while (no < 0) { no += 8; bs->cursor -= 1; }
    bs->bit_off = no;
    for (int i = 0; i < n; i++) {
        int b = bs->bit_off + i;
        uint8_t m = (uint8_t)(1u << (7 - (b & 7)));
        uint8_t bit = (uint8_t)((v >> (n - 1 - i)) & 1);
        uint8_t* p = &bs->cursor[b >> 3];
        *p = (uint8_t)((*p & (uint8_t)(~m)) | (bit ? m : 0));
    }
}|};
]

(* call wrappers: n is the type's on-wire bit width -- the LOGICAL width (`sizeof_ty`:
   int<n> = n, bool = 1), not the container width (`bitsizeof`: bool = 8). So a bool
   serializes as a single bit, matching the interpreter (InterpDeparsing pwrite writes
   `int_to_bits 1` for a bool). read/peek return uint64, cast back to the container type. *)
let nbits_arg ty = eval (vint (sizeof_ty ty) 32)
let read_bits_fun  = efunref read_bits_cid  (tfun [tref packet_t; tint 32] (tint 64))
let peek_bits_fun  = efunref peek_bits_cid  (tfun [tref packet_t; tint 32] (tint 64))
let skip_bits_fun  = efunref skip_bits_cid  (tfun [tref packet_t; tint 32] tunit)
let write_bits_fun = efunref write_bits_cid (tfun [tref packet_t; tint 64; tint 32] tunit)
let call_read  ty bs   = ecast ty (ecall read_bits_fun  [bs; nbits_arg ty])
let call_peek  ty bs   = ecast ty (ecall peek_bits_fun  [bs; nbits_arg ty])
let call_skip  ty bs   = ecall skip_bits_fun  [bs; nbits_arg ty]
let call_write ty bs v = ecall write_bits_fun [bs; ecast (tint 64) v; nbits_arg ty]

let parser_ret_ty = tint 8
let parser_ret_drop = eval@@vint 0 8




(* lower the bytes ops to packet_t* helper calls -- each op maps 1:1.
   Read/Peek are lowered at the *expression* level (not as a bare SAssign rhs) so
   the rewrite survives a wrapping op -- notably the width mask CCoreMaskWidths
   inserts around a sub-byte read (`read<int7>(p) & 127`). Skip (a unit op) and
   the return are handled at the statement level. *)
let lower_parser_body pkt out_event body =
  let pktv = evar pkt (tref packet_t) in
  let visitor = object (_) inherit [_] s_map as super
    method! visit_exp () e =
      let e = super#visit_exp () e in
      match e.e with
      | EOp(Read ty, _) -> call_read ty pktv
      | EOp(Peek ty, _) -> call_peek ty pktv
      | _ -> e
    method! visit_statement () stmt =
      match stmt.s with
      | SUnit { e = EOp(Skip ty, _); _ } ->
          sunit (call_skip ty pktv)
      (* return (ok, ev): drop -> return 0; generate -> *next_event = ev; return ok *)
      | SRet(Some { e = ETuple [ ok_e; ev ]; _ }) -> (
          match ok_e.e with
          | EVal { v = VBool false; _ } -> sret parser_ret_drop
          (* ok = no consume overran the packet (cursor <= end) -- the BytesOk lowering *)
          | _ ->
            let ok = eop Leq [pktv/->cid"cursor"; pktv/->cid"end"] in
            sseq (sassign_exp (ederef out_event) ev) (sret ok))
      | _ -> super#visit_statement () stmt
  end
  in
  visitor#visit_statement () body
;;

let lower_parser id ret_ty params body =
  (* the parser's return type is (bool, event); take the (already-lowered) event
     type from it so the out-event param uses event_t, not raw TVariant. *)
  let ev_ty = match ret_ty.raw_ty with
    | TTuple [_; ev_ty] -> ev_ty
    | _ -> err "[CCoreParse.lower] parser return type is not (bool, event)"
  in
  let out_event_param = (cid"next_event", tref ev_ty) in
  let out_event = param_evar out_event_param in
  let pkt = match List.find_opt (fun (_, ty) -> is_tpacket ty) params with
    | Some (pid, _) -> pid
    | None -> err "[CCoreParse.lower] parser has no bytes parameter"
  in
  let body = lower_parser_body pkt out_event body in
  (* bytes param -> packet_t* ; add the out-event param ; return int8 *)
  let params = List.map (fun (pid, ty) -> if is_tpacket ty then (pid, tref packet_t) else (pid, ty)) params in
  let params = params @ [out_event_param] in
  dfun id parser_ret_ty params body
;;

let lower_parse_bodies decls =
  List.map
    (fun decl ->
      match extract_dparser_opt decl with
      | None -> decl
      | Some(id, ret_ty, params, body) -> lower_parser id ret_ty params body)
    decls
;;

(* ------------------------------- deparser -------------------------------- *)

(* rewrite references to the by-value event param into derefs of a by-ref param:
   ev_out  -->  ( *ev_out ), so ev_out.tag / ev_out.data.. become ev_out->.. *)
let deref_event_param ev_cid =
  object (_) inherit [_] s_map as super
    method! visit_exp () exp =
      let exp = super#visit_exp () exp in
      match exp.e with
      | EVar c when Cid.equal c ev_cid -> ederef (evar c (tref exp.ety))
      | _ -> exp
  end
;;

let rec lower_deparse_body bufv stmt =
  match stmt.s with
  | SSeq (a, b) -> sseq (lower_deparse_body bufv a) (lower_deparse_body bufv b)
  | SMatch (es, brs) -> smatch es (List.map (fun (ps, b) -> (ps, lower_deparse_body bufv b)) brs)
  | SIf (c, a, b) -> sif c (lower_deparse_body bufv a) (lower_deparse_body bufv b)
  (* write<ty>(out, v) --> write_bits(out, v, n): prepend v's bits in front of
     the cursor (which sits at the header/payload boundary on entry) *)
  | SUnit { e = EOp (Write ty, [ _; v ]); _ } -> sunit (call_write ty bufv v)
  (* return out  -->  return; (the driver ignores the result) *)
  | SRet _ -> sret_none
  | _ -> stmt
;;

let lower_deparse_fun id params body =
  let ev_cid, ev_ty =
    match List.find_opt (fun (_, ty) -> not (is_tpacket ty)) params with
    | Some p -> p
    | None -> err "[CCoreParse.lower_deparse] deparse has no event parameter"
  in
  let bs_cid =
    match List.find_opt (fun (_, ty) -> is_tpacket ty) params with
    | Some (c, _) -> c
    | None -> err "[CCoreParse.lower_deparse] deparse has no bytes parameter"
  in
  let bufv = evar bs_cid (tref packet_t) in
  (* event param: by value -> by reference, derefing its uses *)
  let body = (deref_event_param ev_cid)#visit_statement () body in
  let body = lower_deparse_body bufv body in
  (* bytes param -> packet_t* ; event param -> event_t* ; return void *)
  let params =
    List.map
      (fun (c, ty) ->
        if is_tpacket ty then (c, tref packet_t)
        else if Cid.equal c ev_cid then (c, tref ev_ty)
        else (c, ty))
      params
  in
  dfun id tunit params body
;;

let lower_deparse decls =
  List.map
    (fun decl ->
      match decl.d with
      | DFun (FNormal, id, _, params, BStatement body) when Cid.equal id deparse_id ->
        lower_deparse_fun id params body
      | _ -> decl)
    decls
;;

(* ------------------------------- combined -------------------------------- *)
(* lower both halves to the packet_t pointer form, then emit packet_t and the
   four bit-codec helpers once, ahead of the rest. (Requires the event matches
   already lowered -- CCoreVariants.lower runs first.) The event carries no
   packet buffer of its own: the driver's queue element owns the bytes, and
   packet_t is a transient view over that buffer. *)
let lower decls =
  let decls = lower_deparse decls in
  let decls = lower_parse_bodies decls in
  decl_tabstract packet_t :: codec_helpers @ decls
;;
