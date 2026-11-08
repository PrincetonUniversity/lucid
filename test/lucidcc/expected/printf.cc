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
      uint32_t a_858;
      uint64_t b_859;
      uint64_t c_860;
    } pkt_862;
  } args;
} event_variant_t;
event_variant_t pkt_862(uint32_t a_858 , uint64_t b_859 , uint64_t c_860 ){
  event_variant_t ev  = {0};
  ev.args.pkt_862.a_858 = a_858;
  ev.args.pkt_862.b_859 = b_859;
  ev.args.pkt_862.c_860 = c_860;
  ev.tag = pkt_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_pkt(uint32_t a_858 , uint64_t b_859 , uint64_t c_860 ){
  event_t tmp_914  = {.meta = {.len = 18, .is_packet = 1, .has_payload = 1, .timestamp = 0, .in_port = 0}, .data = pkt_862(a_858, b_859, c_860)};
  return tmp_914;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
typedef const char* cstr;
cstr lucid_bool_str(uint8_t b ){ return b ? "true" : "false"; }
uint8_t parse_event(packet_t*  pkt , event_t*  next_event , uint32_t ingress_port ){
  uint32_t a_858  = ((uint32_t)(read_bits(pkt, 32)));
  uint64_t b_859  = ((uint64_t)(read_bits(pkt, 48))) & 281474976710655;
  uint64_t c_860  = ((uint64_t)(read_bits(pkt, 64)));
  (*(next_event)) = mk_pkt(a_858, b_859, c_860);
  return pkt->cursor <= pkt->end;
}
void deparse_event(event_t*  ev_out , packet_t*  buf_out ){
  switch (ev_out->data.tag) {
    case 1: {
      uint32_t a_858  = ev_out->data.args.pkt_862.a_858;
      uint64_t b_859  = ev_out->data.args.pkt_862.b_859;
      uint64_t c_860  = ev_out->data.args.pkt_862.c_860;
      write_bits(buf_out, ((uint64_t)(c_860)), 64);
      write_bits(buf_out, ((uint64_t)(b_859)), 48);
      write_bits(buf_out, ((uint64_t)(a_858)), 32);
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
      uint32_t a_863  = ev_in->data.args.pkt_862.a_858;
      uint64_t b_864  = ev_in->data.args.pkt_862.b_859;
      uint64_t c_865  = ev_in->data.args.pkt_862.c_860;
      event_t this  = mk_pkt(a_863, b_864, c_865);
      printf("a=%u b=%llu c=%llu big=%s\n", a_863, ((unsigned long long)(b_864)), ((unsigned long long)(c_865)), lucid_bool_str(a_863 == 4294967280));
      out_event_t tmp_915  = {.ev = mk_pkt(a_863, b_864, c_865), .port = ev_in->meta.in_port};
      out_events[n] = tmp_915;
      n = n + 1;
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

