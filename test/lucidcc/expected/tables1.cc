typedef struct {
  uint32_t _0;
  uint8_t _1;
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
enum {tag_hit_acn_2244 = 0, tag_miss_acn_2247 = 1};

/* installs that a full table rejected */
static uint64_t table_full_installs = 0;
static void report_table_stats(void) {
    if (table_full_installs)
        fprintf(stderr, "table installs rejected (table full): %llu\n", (unsigned long long)table_full_installs);
}
typedef struct {
  uint16_t len;
  uint8_t is_packet;
  uint8_t has_payload;
  uint64_t timestamp;
  uint32_t in_port;
} event_meta;
uint16_t do_match_tag  = 1;
uint16_t do_install_tag  = 2;
typedef struct {
  uint16_t tag;
  union {
    struct {
      uint32_t s_2249;
    } do_match_2250;
    struct {
      uint32_t v_2251;
    } do_install_2252;
  } args;
} event_variant_t;
event_variant_t do_match_2250(uint32_t s_2249 ){
  event_variant_t ev  = {0};
  ev.args.do_match_2250.s_2249 = s_2249;
  ev.tag = do_match_tag;
  return ev;
}
event_variant_t do_install_2252(uint32_t v_2251 ){
  event_variant_t ev  = {0};
  ev.args.do_install_2252.v_2251 = v_2251;
  ev.tag = do_install_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_do_match(uint32_t s_2249 ){
  event_t tmp_2410  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_match_2250(s_2249)};
  return tmp_2410;
}
event_t mk_do_install(uint32_t v_2251 ){
  event_t tmp_2411  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_install_2252(v_2251)};
  return tmp_2411;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
typedef tuple_1 res_t_2241;
res_t_2241 hit_acn_2244(uint32_t x_2242 , uint32_t a_2243 ){
  res_t_2241 tmp_2412  = {._0 = x_2242, ._1 = true,};
  return tmp_2412;
}
res_t_2241 miss_acn_2247(uint32_t b_2245 , uint32_t a_2246 ){
  res_t_2241 tmp_2413  = {._0 = a_2246, ._1 = false,};
  return tmp_2413;
}
typedef struct {
  uint32_t key;
  uint32_t mask;
  uint32_t prio;
  uint32_t action_tag;
  uint32_t action_arg;
} cellty_ftbl_2248;
typedef struct {
  struct {
    uint32_t action_tag;
    uint32_t action_arg;
  } _default;
  uint32_t max_idx;
  cellty_ftbl_2248 entries [1024];
} ty_ftbl_2248;
ty_ftbl_2248 ftbl_2248  = {._default = {.action_tag = tag_miss_acn_2247, .action_arg = 1}, .max_idx = 0, .entries = {0}};
void warn_full_ftbl_2248(){
  static int warned = 0;
  table_full_installs++;
  if (!warned) { warned = 1; fprintf(stderr, "warning: table ftbl is full, an entry was not installed\n"); }
}
void warn_update_ftbl_2248(){
  static int warned = 0;
  if (!warned) { warned = 1; fprintf(stderr, "warning: table ftbl has no such entry to update\n"); }
}
void warn_remove_ftbl_2248(){
  static int warned = 0;
  if (!warned) { warned = 1; fprintf(stderr, "warning: table ftbl has no such entry to remove\n"); }
}
void install_ftbl_2248(uint32_t key , uint32_t mask , uint32_t prio , uint32_t action , uint32_t const_arg ){
  for (int _idx = 0; _idx < 1024; _idx++) {
    if (_idx < ftbl_2248.max_idx) {
      if ((ftbl_2248.entries[_idx].prio == prio) && ((ftbl_2248.entries[_idx].mask == mask) && ((ftbl_2248.entries[_idx].key & mask) == (key & mask)))) {
        return ;
      }
    }
  }
  if (ftbl_2248.max_idx == 1024) {
    warn_full_ftbl_2248();
    return ;
  }
  cellty_ftbl_2248 carry  = {.key = key, .mask = mask, .prio = prio, .action_tag = action, .action_arg = const_arg};
  for (int _idx = 0; _idx < 1024; _idx++) {
    if ((_idx < ftbl_2248.max_idx) && (ftbl_2248.entries[_idx].prio < prio)) {
      cellty_ftbl_2248 tmp  = ftbl_2248.entries[_idx];
      ftbl_2248.entries[_idx] = carry;
      carry = tmp;
    }
    if (_idx == ftbl_2248.max_idx) {
      ftbl_2248.entries[_idx] = carry;
      ftbl_2248.max_idx = _idx + 1;
      return ;
    }
  }
  return ;
}
void update_ftbl_2248(uint32_t key , uint32_t mask , uint32_t prio , uint32_t action , uint32_t const_arg ){
  for (int _idx = 0; _idx < 1024; _idx++) {
    if (_idx < ftbl_2248.max_idx) {
      if ((ftbl_2248.entries[_idx].prio == prio) && ((ftbl_2248.entries[_idx].mask == mask) && ((ftbl_2248.entries[_idx].key & mask) == (key & mask)))) {
        ftbl_2248.entries[_idx].action_tag = action;
        ftbl_2248.entries[_idx].action_arg = const_arg;
        return ;
      }
    }
  }
  warn_update_ftbl_2248();
  return ;
}
void remove_ftbl_2248(uint32_t key , uint32_t mask , uint32_t prio ){
  uint8_t removed  = false;
  for (int _idx = 0; _idx < 1024; _idx++) {
    if (_idx < ftbl_2248.max_idx) {
      if ((ftbl_2248.entries[_idx].prio == prio) && ((ftbl_2248.entries[_idx].mask == mask) && ((ftbl_2248.entries[_idx].key & mask) == (key & mask)))) {
        removed = true;
      }
      if (removed && ((_idx + 1) < ftbl_2248.max_idx)) {
        ftbl_2248.entries[_idx] = ftbl_2248.entries[_idx + 1];
      }
    }
  }
  if (removed) {
    ftbl_2248.max_idx = ftbl_2248.max_idx - 1;
    return ;
  }
  warn_remove_ftbl_2248();
  return ;
}
res_t_2241 lookup_ftbl_2248(uint32_t key , uint32_t arg ){
  for (int _idx = 0; _idx < 1024; _idx++) {
    if (_idx >= ftbl_2248.max_idx) {
      switch (ftbl_2248._default.action_tag) {
        case tag_hit_acn_2244: {
          return hit_acn_2244(ftbl_2248._default.action_arg, arg);
          break;
        }
        default: {
          return miss_acn_2247(ftbl_2248._default.action_arg, arg);
          break;
        }
      }
    }
    if ((key & ftbl_2248.entries[_idx].mask) == (ftbl_2248.entries[_idx].key & ftbl_2248.entries[_idx].mask)) {
      switch (ftbl_2248.entries[_idx].action_tag) {
        case tag_hit_acn_2244: {
          return hit_acn_2244(ftbl_2248.entries[_idx].action_arg, arg);
          break;
        }
        case tag_miss_acn_2247: {
          return miss_acn_2247(ftbl_2248.entries[_idx].action_arg, arg);
          break;
        }
      }
    }
  }
  switch (ftbl_2248._default.action_tag) {
    case tag_hit_acn_2244: {
      return hit_acn_2244(ftbl_2248._default.action_arg, arg);
      break;
    }
    default: {
      return miss_acn_2247(ftbl_2248._default.action_arg, arg);
      break;
    }
  }
}
uint8_t parse_event(packet_t*  packet , event_t*  next_event , uint32_t ingress_port ){
  skip_bits(packet, 32);
  skip_bits(packet, 16);
  skip_bits(packet, 32);
  skip_bits(packet, 16);
  uint16_t ety  = ((uint16_t)(read_bits(packet, 16)));
  switch (ety) {
    case 666: {
      uint16_t tag  = ((uint16_t)(read_bits(packet, 16)));
      switch (tag) {
        case 1: {
          uint32_t s_2249  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_do_match(s_2249);
          return packet->cursor <= packet->end;
          break;
        }
        case 2: {
          uint32_t v_2251  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_do_install(v_2251);
          return packet->cursor <= packet->end;
          break;
        }
        default: {
          return 0;
          break;
        }
      }
      break;
    }
    default: {
      return 0;
      break;
    }
  }
  return 0;
}
void deparse_event(event_t*  ev_out , packet_t*  buf_out ){
  switch (ev_out->data.tag) {
    case 1: {
      uint32_t s_2249  = ev_out->data.args.do_match_2250.s_2249;
      write_bits(buf_out, ((uint64_t)(s_2249)), 32);
      if (ev_out->meta.is_packet == 0) {
        write_bits(buf_out, ((uint64_t)(1)), 16);
        write_bits(buf_out, ((uint64_t)(666)), 16);
        write_bits(buf_out, ((uint64_t)(2)), 48);
        write_bits(buf_out, ((uint64_t)(1)), 48);
      }
      return ;
      break;
    }
    case 2: {
      uint32_t v_2251  = ev_out->data.args.do_install_2252.v_2251;
      write_bits(buf_out, ((uint64_t)(v_2251)), 32);
      if (ev_out->meta.is_packet == 0) {
        write_bits(buf_out, ((uint64_t)(2)), 16);
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
      uint32_t s_2253  = ev_in->data.args.do_match_2250.s_2249;
      event_t this  = mk_do_match(s_2253);
      res_t_2241 tup_2409  = lookup_ftbl_2248(s_2253, 1000);
      uint32_t tbl_out_0_2255  = tup_2409._0;
      uint8_t tbl_out_1_2256  = tup_2409._1;
      printf("return of table match: %u\n", tbl_out_0_2255);
      if (tbl_out_1_2256 == true) {
        printf("table HIT.\n");
      }else {
        printf("table MISS. Installing hit_acn(3) for current key. \n");
        install_ftbl_2248(s_2253, 4294967295, 0, tag_hit_acn_2244, 3);
      }
      break;
    }
    case 2: {
      uint32_t v_2257  = ev_in->data.args.do_install_2252.v_2251;
      event_t this  = mk_do_install(v_2257);
      printf("installing entry: %u\n", v_2257);
      install_ftbl_2248(v_2257, 4294967295, 0, tag_hit_acn_2244, 2);
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

