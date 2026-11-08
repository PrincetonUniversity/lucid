typedef struct {
  uint8_t*  start;
  uint8_t*  cursor;
  uint8_t*  end;
  uint32_t bit_off;
} packet_t;

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
}

uint64_t peek_bits(packet_t* bs, int n) {
    uint64_t v = 0;
    for (int i = 0; i < n; i++) {
        int b = bs->bit_off + i;
        int in_bounds = (bs->cursor + (b >> 3)) < bs->end;
        v = (v << 1) | (in_bounds ? ((bs->cursor[b >> 3] >> (7 - (b & 7))) & 1) : 0);
    }
    return v;
}

void skip_bits(packet_t* bs, int n) {
    int tot = bs->bit_off + n;
    bs->cursor += tot >> 3;
    bs->bit_off = tot & 7;
}

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
}
uint32_t flood(uint32_t port ){ return port + 10000;/* TODO!*/ }
uint32_t hash_32(uint32_t seed , uint8_t*  str , uint32_t len_bits ){
  // len_bits is the value's bit width. Sum the whole bytes, then add the last
  // partial byte masked to its valid low bits (the value is stored little-endian,
  // so the leftover bits live in the low end of the final byte). Placeholder hash
  // (a byte sum) -- collisions are not a concern yet.
  int hashValue = seed;
  uint32_t full_bytes = len_bits / 8;
  for (uint32_t i = 0; i < full_bytes; i++) {
      hashValue += str[i];
  }
  uint32_t rem = len_bits % 8;
  if (rem != 0) {
      hashValue += str[full_bytes] & ((1 << rem) - 1);
  }
  return hashValue;
}
typedef struct {
  uint16_t len;
  uint8_t is_packet;
  uint8_t has_payload;
  uint64_t timestamp;
  uint32_t in_port;
} event_meta;
uint16_t pkt_tag  = 1;
typedef struct {
  uint16_t tag;
  union {
    struct {
      uint64_t a_1363;
      uint8_t a_wrapped_1364;
      uint64_t b_1365;
      uint8_t b_wrapped_1366;
    } pkt_1368;
  } args;
} event_variant_t;
event_variant_t pkt_1368(uint64_t a_1363 , uint8_t a_wrapped_1364 , uint64_t b_1365 , uint8_t b_wrapped_1366 ){
  event_variant_t ev  = {0};
  ev.args.pkt_1368.a_1363 = a_1363;
  ev.args.pkt_1368.a_wrapped_1364 = a_wrapped_1364;
  ev.args.pkt_1368.b_1365 = b_1365;
  ev.args.pkt_1368.b_wrapped_1366 = b_wrapped_1366;
  ev.tag = pkt_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_pkt(uint64_t a_1363 , uint8_t a_wrapped_1364 , uint64_t b_1365 , uint8_t b_wrapped_1366 ){
  event_t tmp_1463  = {.meta = {.len = 16, .is_packet = 1, .has_payload = 1, .timestamp = 0, .in_port = 0}, .data = pkt_1368(a_1363, a_wrapped_1364, b_1365, b_wrapped_1366)};
  return tmp_1463;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
uint8_t parse_event(packet_t*  pkt , event_t*  next_event , uint32_t ingress_port ){
  uint64_t a_1363  = ((uint64_t)(read_bits(pkt, 62))) & 4611686018427387903;
  uint8_t a_wrapped_1364  = ((uint8_t)(read_bits(pkt, 2))) & 3;
  uint64_t b_1365  = ((uint64_t)(read_bits(pkt, 63))) & 9223372036854775807;
  uint8_t b_wrapped_1366  = ((uint8_t)(read_bits(pkt, 1))) & 1;
  (*(next_event)) = mk_pkt(a_1363, a_wrapped_1364, b_1365, b_wrapped_1366);
  return pkt->cursor <= pkt->end;
}
void deparse_event(event_t*  ev_out , packet_t*  buf_out ){
  switch (ev_out->data.tag) {
    case 1: {
      uint64_t a_1363  = ev_out->data.args.pkt_1368.a_1363;
      uint8_t a_wrapped_1364  = ev_out->data.args.pkt_1368.a_wrapped_1364;
      uint64_t b_1365  = ev_out->data.args.pkt_1368.b_1365;
      uint8_t b_wrapped_1366  = ev_out->data.args.pkt_1368.b_wrapped_1366;
      write_bits(buf_out, ((uint64_t)(b_wrapped_1366)), 1);
      write_bits(buf_out, ((uint64_t)(b_1365)), 63);
      write_bits(buf_out, ((uint64_t)(a_wrapped_1364)), 2);
      write_bits(buf_out, ((uint64_t)(a_1363)), 62);
      if (ev_out->meta.is_packet == 0) {
        write_bits(buf_out, ((uint64_t)(1)), 16);
        write_bits(buf_out, ((uint64_t)(666)), 16);
        write_bits(buf_out, ((uint64_t)(2)), 48);
        write_bits(buf_out, ((uint64_t)(1)), 48);
      }
      return ;
      break;
    }
  }
}
typedef struct {
  event_t ev;
  uint32_t port;
} out_event_t;
uint16_t handle_event(event_t*  ev_in , out_event_t out_events [64]){
  uint16_t n  = 0;
  switch (ev_in->data.tag) {
    case 1: {
      uint64_t a_1369  = ev_in->data.args.pkt_1368.a_1363;
      uint8_t a_wrapped_1370  = ev_in->data.args.pkt_1368.a_wrapped_1364;
      uint64_t b_1371  = ev_in->data.args.pkt_1368.b_1365;
      uint8_t b_wrapped_1372  = ev_in->data.args.pkt_1368.b_wrapped_1366;
      event_t this  = mk_pkt(a_1369, a_wrapped_1370, b_1371, b_wrapped_1372);
      uint8_t wa_1376  = 0;
      uint8_t wb_1377  = 0;
      if (((a_1369 + 1) & 4611686018427387903) == 0) {
        wa_1376 = 1;
      }
      if (((b_1371 + 1) & 9223372036854775807) == 0) {
        wb_1377 = 1;
      }
      out_event_t tmp_1464  = {.ev = mk_pkt((a_1369 + 1) & 4611686018427387903, wa_1376, (b_1371 + 1) & 9223372036854775807, wb_1377), .port = ev_in->meta.in_port};
      out_events[n] = tmp_1464;
      n = n + 1;
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

