typedef struct {
  uint32_t _0;
  uint32_t _1;
} tuple_1;
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
enum {tag_add_pair_1178 = 0};
typedef struct {
  uint16_t len;
  uint8_t is_packet;
  uint8_t has_payload;
  uint64_t timestamp;
  uint32_t in_port;
} event_meta;
uint16_t foo_tag  = 1;
typedef struct {
  uint16_t tag;
  union {
    struct {
      uint32_t p_0_1180;
      uint32_t p_1_1181;
    } foo_1182;
  } args;
} event_variant_t;
event_variant_t foo_1182(uint32_t p_0_1180 , uint32_t p_1_1181 ){
  event_variant_t ev  = {0};
  ev.args.foo_1182.p_0_1180 = p_0_1180;
  ev.args.foo_1182.p_1_1181 = p_1_1181;
  ev.tag = foo_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_foo(uint32_t p_0_1180 , uint32_t p_1_1181 ){
  event_t tmp_1266  = {.meta = {.len = 8, .is_packet = 1, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = foo_1182(p_0_1180, p_1_1181)};
  return tmp_1266;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
typedef tuple_1 pair_t_1174;
pair_t_1174 add_pair_1178(uint32_t c_1175 , pair_t_1174 p ){
  pair_t_1174 tmp_1267  = {._0 = p._0, ._1 = c_1175,};
  return tmp_1267;
}
typedef struct {
  uint8_t valid;
  uint32_t key;
  uint32_t mask;
  uint32_t action_tag;
  uint32_t action_arg;
} cellty_my_table_1179;
typedef struct {
  struct {
    uint32_t action_tag;
    uint32_t action_arg;
  } _default;
  cellty_my_table_1179 entries [32];
} ty_my_table_1179;
ty_my_table_1179 my_table_1179  = {._default = {.action_tag = tag_add_pair_1178, .action_arg = 0}, .entries = {0}};
void install_my_table_1179(uint32_t key , uint32_t action , uint32_t const_arg ){
  for (int _idx = 0; _idx < 32; _idx++) {
    if (my_table_1179.entries[_idx].valid == false) {
      cellty_my_table_1179 tmp_1268  = {.valid = true, .key = key, .mask = 4294967295, .action_tag = action, .action_arg = const_arg};
      my_table_1179.entries[_idx] = tmp_1268;
      return ;
    }
  }
  return ;
}
void install_ternary_my_table_1179(uint32_t key , uint32_t mask , uint32_t action , uint32_t const_arg ){
  for (int _idx = 0; _idx < 32; _idx++) {
    if (my_table_1179.entries[_idx].valid == false) {
      cellty_my_table_1179 tmp_1269  = {.valid = true, .key = key, .mask = mask, .action_tag = action, .action_arg = const_arg};
      my_table_1179.entries[_idx] = tmp_1269;
      return ;
    }
  }
  return ;
}
pair_t_1174 lookup_my_table_1179(uint32_t key , pair_t_1174 arg ){
  for (int _idx = 0; _idx < 32; _idx++) {
    if (my_table_1179.entries[_idx].valid && ((key & my_table_1179.entries[_idx].mask) == (my_table_1179.entries[_idx].key & my_table_1179.entries[_idx].mask))) {
      switch (my_table_1179.entries[_idx].action_tag) {
        case tag_add_pair_1178: {
          return add_pair_1178(my_table_1179.entries[_idx].action_arg, arg);
          break;
        }
      }
    }
  }
  switch (my_table_1179._default.action_tag) {
    default: {
      return add_pair_1178(my_table_1179._default.action_arg, arg);
      break;
    }
  }
}
uint8_t parse_event(packet_t*  pkt , event_t*  next_event , uint32_t ingress_port ){
  uint32_t p_0_1180  = ((uint32_t)(read_bits(pkt, 32)));
  uint32_t p_1_1181  = ((uint32_t)(read_bits(pkt, 32)));
  (*(next_event)) = mk_foo(p_0_1180, p_1_1181);
  return pkt->cursor <= pkt->end;
}
void deparse_event(event_t*  ev_out , packet_t*  buf_out ){
  switch (ev_out->data.tag) {
    case 1: {
      uint32_t p_0_1180  = ev_out->data.args.foo_1182.p_0_1180;
      uint32_t p_1_1181  = ev_out->data.args.foo_1182.p_1_1181;
      write_bits(buf_out, ((uint64_t)(p_1_1181)), 32);
      write_bits(buf_out, ((uint64_t)(p_0_1180)), 32);
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
      uint32_t p_0_1183  = ev_in->data.args.foo_1182.p_0_1180;
      uint32_t p_1_1184  = ev_in->data.args.foo_1182.p_1_1181;
      event_t this  = mk_foo(p_0_1183, p_1_1184);
      pair_t_1174 tmp_1270  = {._0 = p_0_1183, ._1 = p_1_1184,};
      pair_t_1174 tup_1265  = lookup_my_table_1179(1, tmp_1270);
      uint32_t new_pair_0_1185  = tup_1265._0;
      uint32_t new_pair_1_1186  = tup_1265._1;
      out_event_t tmp_1271  = {.ev = mk_foo(new_pair_0_1185, new_pair_1_1186), .port = 1};
      out_events[n] = tmp_1271;
      n = n + 1;
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

