(* packet <--> event parser and deparser. 
   Before lowering to pointer form, synthesize_deparser generates a 
   deparser function to serialize events to packets. 
   This module also contains functions to lower the parser and deparser 
   to pointer form, and emits the parse helpers as inlined C functions.
*)
open CCoreSyntax
open CCoreExceptions
open CCoreUtils


(* parser conventions (lowered form) *)
let parser_cid = Cid.create ["parse_event"] ;;
(* deparser conventions *)
let deparse_id = Cid.create ["deparse_event"]
let ev_out_cid = cid "ev_out"
let buf_out_cid = cid "buf_out"


(* ------------------------------- deparser -------------------------------- *)
(* Deparser is synthesized from the event variant. Writes prepend, so
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
*)

(* Lucid ethernet header definition *)
let eth_preamble_writes do_write =
  [
    do_write (tint 16) (eval (vint Constants.lucid_ety_int 16));  (* ethertype *)
    do_write (tint 48) (eval (vint 2 48));                        (* src mac   *)
    do_write (tint 48) (eval (vint 1 48));                        (* dst mac   *)
  ]
;;

let make_deparser decls =
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



(* ===== lowered (pointer) representation of packet_t =====
   A packet is a byte buffer, with a cursor to address bytes and bits in each byte.
   Fields of a packet (event parameters) are packed contiguously, 
   MSB-first (network/big-endian bit order). They are raw C with simple bit
   loops. Note that CCoreWellformedC.check_event_fields ensures 
   that unaligned event parameters do not cross byte boundaries. This 
   may allow optimizations down the line. *)
let packet_t =
  tabstract
    "packet_t"@@trecord
      [
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

(* parser and deparser primitives *)
let parser_primitives = [
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

(* call wrappers: n is the type's on-wire bit width, not its container width. So a bool
   serializes as a single bit. Read/peek return uint64, cast back to the container type. *)
let nbits_arg ty = eval (vint (sizeof_ty ty) 32)
let read_bits_fun  = efunref read_bits_cid  (tfun [tref packet_t; tint 32] (tint 64))
let peek_bits_fun  = efunref peek_bits_cid  (tfun [tref packet_t; tint 32] (tint 64))
let skip_bits_fun  = efunref skip_bits_cid  (tfun [tref packet_t; tint 32] tunit)
let write_bits_fun = efunref write_bits_cid (tfun [tref packet_t; tint 64; tint 32] tunit)
let call_read  ty bs   = ecast ty (ecall read_bits_fun  [bs; nbits_arg ty])
let call_peek  ty bs   = ecast ty (ecall peek_bits_fun  [bs; nbits_arg ty])
let call_skip  ty bs   = ecall skip_bits_fun  [bs; nbits_arg ty]
let call_write ty bs v = ecall write_bits_fun [bs; ecast (tint 64) v; nbits_arg ty]

(* lower the bytes ops to packet_t* helper calls. Skip and return are
   handled at the statement level. *)
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
      | SRet(Some { e = ETuple [ success; ev ]; _ }) -> (
          match success.e with
          | EVal { v = VBool false; _ } -> sret (eval@@vint 0 8)
          | _ ->
            let ok = eop Leq [pktv/->cid"cursor"; pktv/->cid"end"] in
            sseq (sassign_exp (ederef out_event) ev) (sret ok))
      | _ -> super#visit_statement () stmt
  end
  in
  visitor#visit_statement () body
;;

let lower_parser id ret_ty params body =
  (* make the event output param a reference param *)
  let flag_ty, ev_ty = match ret_ty.raw_ty with
    | TTuple [flag_ty; ev_ty] -> flag_ty, ev_ty
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
  dfun id flag_ty params body
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
  | SUnit { e = EOp (Write ty, [ _; v ]); _ } -> sunit (call_write ty bufv v)
  | SRet _ -> sret_none
  | _ -> stmt
;;

let lower_deparse_fun id params body =
  let ev_cid, ev_ty =
    match List.find_opt (fun (_, ty) -> not (is_tpacket ty)) params with
    | Some p -> p
    | None -> err "[CCoreParse.lower_deparse] deparse has no event parameter"
  in
  let pkt_cid =
    match List.find_opt (fun (_, ty) -> is_tpacket ty) params with
    | Some (c, _) -> c
    | None -> err "[CCoreParse.lower_deparse] deparse has no packet parameter"
  in
  let bufv = evar pkt_cid (tref packet_t) in
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

let lower decls =
  let decls = lower_deparse decls in
  let decls = lower_parse_bodies decls in
  decl_tabstract packet_t :: parser_primitives @ decls
;;
