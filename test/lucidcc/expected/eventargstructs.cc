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
enum {tag_add_pair_1287 = 0};
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
      uint32_t p_0_1289;
      uint32_t p_1_1290;
    } foo_1292;
  } args;
} event_variant_t;
event_variant_t foo_1292(uint32_t p_0_1289 , uint32_t p_1_1290 ){
  event_variant_t ev  = {0};
  ev.args.foo_1292.p_0_1289 = p_0_1289;
  ev.args.foo_1292.p_1_1290 = p_1_1290;
  ev.tag = foo_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_foo(uint32_t p_0_1289 , uint32_t p_1_1290 ){
  event_t tmp_1381  = {.meta = {.len = 8, .is_packet = 1, .has_payload = 1, .timestamp = 0, .in_port = 0}, .data = foo_1292(p_0_1289, p_1_1290)};
  return tmp_1381;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
typedef tuple_1 pair_t_1283;
pair_t_1283 add_pair_1287(uint32_t c_1284 , pair_t_1283 p ){
  pair_t_1283 tmp_1382  = {._0 = p._0, ._1 = c_1284,};
  return tmp_1382;
}
typedef struct {
  uint8_t valid;
  uint32_t key;
  uint32_t mask;
  uint32_t action_tag;
  uint32_t action_arg;
} cellty_my_table_1288;
typedef struct {
  struct {
    uint32_t action_tag;
    uint32_t action_arg;
  } _default;
  uint32_t max_idx;
  cellty_my_table_1288 entries [32];
} ty_my_table_1288;
ty_my_table_1288 my_table_1288  = {._default = {.action_tag = tag_add_pair_1287, .action_arg = 0}, .max_idx = 0, .entries = {0}};
void install_my_table_1288(uint32_t key , uint32_t action , uint32_t const_arg ){
  for (int _idx = 0; _idx < 32; _idx++) {
    if (my_table_1288.entries[_idx].valid == false) {
      cellty_my_table_1288 tmp_1383  = {.valid = true, .key = key, .mask = 4294967295, .action_tag = action, .action_arg = const_arg};
      my_table_1288.entries[_idx] = tmp_1383;
      if (_idx >= my_table_1288.max_idx) {
        my_table_1288.max_idx = _idx + 1;
      }
      return ;
    }
  }
  return ;
}
void install_ternary_my_table_1288(uint32_t key , uint32_t mask , uint32_t action , uint32_t const_arg ){
  for (int _idx = 0; _idx < 32; _idx++) {
    if (my_table_1288.entries[_idx].valid == false) {
      cellty_my_table_1288 tmp_1384  = {.valid = true, .key = key, .mask = mask, .action_tag = action, .action_arg = const_arg};
      my_table_1288.entries[_idx] = tmp_1384;
      if (_idx >= my_table_1288.max_idx) {
        my_table_1288.max_idx = _idx + 1;
      }
      return ;
    }
  }
  return ;
}
pair_t_1283 lookup_my_table_1288(uint32_t key , pair_t_1283 arg ){
  for (int _idx = 0; _idx < 32; _idx++) {
    if (_idx >= my_table_1288.max_idx) {
      switch (my_table_1288._default.action_tag) {
        default: {
          return add_pair_1287(my_table_1288._default.action_arg, arg);
          break;
        }
      }
    }
    if (my_table_1288.entries[_idx].valid && ((key & my_table_1288.entries[_idx].mask) == (my_table_1288.entries[_idx].key & my_table_1288.entries[_idx].mask))) {
      switch (my_table_1288.entries[_idx].action_tag) {
        case tag_add_pair_1287: {
          return add_pair_1287(my_table_1288.entries[_idx].action_arg, arg);
          break;
        }
      }
    }
  }
  switch (my_table_1288._default.action_tag) {
    default: {
      return add_pair_1287(my_table_1288._default.action_arg, arg);
      break;
    }
  }
}
uint8_t parse_event(packet_t*  pkt , event_t*  next_event , uint32_t ingress_port ){
  uint32_t p_0_1289  = ((uint32_t)(read_bits(pkt, 32)));
  uint32_t p_1_1290  = ((uint32_t)(read_bits(pkt, 32)));
  (*(next_event)) = mk_foo(p_0_1289, p_1_1290);
  return pkt->cursor <= pkt->end;
}
void deparse_event(event_t*  ev_out , packet_t*  buf_out ){
  switch (ev_out->data.tag) {
    case 1: {
      uint32_t p_0_1289  = ev_out->data.args.foo_1292.p_0_1289;
      uint32_t p_1_1290  = ev_out->data.args.foo_1292.p_1_1290;
      write_bits(buf_out, ((uint64_t)(p_1_1290)), 32);
      write_bits(buf_out, ((uint64_t)(p_0_1289)), 32);
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
      uint32_t p_0_1293  = ev_in->data.args.foo_1292.p_0_1289;
      uint32_t p_1_1294  = ev_in->data.args.foo_1292.p_1_1290;
      event_t this  = mk_foo(p_0_1293, p_1_1294);
      pair_t_1283 tmp_1385  = {._0 = p_0_1293, ._1 = p_1_1294,};
      pair_t_1283 tup_1380  = lookup_my_table_1288(1, tmp_1385);
      uint32_t new_pair_0_1296  = tup_1380._0;
      uint32_t new_pair_1_1297  = tup_1380._1;
      out_event_t tmp_1386  = {.ev = mk_foo(new_pair_0_1296, new_pair_1_1297), .port = 1};
      out_events[n] = tmp_1386;
      n = n + 1;
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

