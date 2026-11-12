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
enum {tag_hit_acn_2744 = 0, tag_miss_acn_2747 = 1};
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
      uint32_t s_2749;
    } do_match_2750;
    struct {
      uint32_t v_2751;
      uint32_t m_2752;
    } do_install_2753;
  } args;
} event_variant_t;
event_variant_t do_match_2750(uint32_t s_2749 ){
  event_variant_t ev  = {0};
  ev.args.do_match_2750.s_2749 = s_2749;
  ev.tag = do_match_tag;
  return ev;
}
event_variant_t do_install_2753(uint32_t v_2751 , uint32_t m_2752 ){
  event_variant_t ev  = {0};
  ev.args.do_install_2753.v_2751 = v_2751;
  ev.args.do_install_2753.m_2752 = m_2752;
  ev.tag = do_install_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_do_match(uint32_t s_2749 ){
  event_t tmp_2954  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_match_2750(s_2749)};
  return tmp_2954;
}
event_t mk_do_install(uint32_t v_2751 , uint32_t m_2752 ){
  event_t tmp_2955  = {.meta = {.len = 8, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_install_2753(v_2751, m_2752)};
  return tmp_2955;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
typedef tuple_1 res_t_2741;
res_t_2741 hit_acn_2744(uint32_t x_2742 , uint32_t a_2743 ){
  res_t_2741 tmp_2956  = {._0 = x_2742, ._1 = true,};
  return tmp_2956;
}
res_t_2741 miss_acn_2747(uint32_t b_2745 , uint32_t a_2746 ){
  res_t_2741 tmp_2957  = {._0 = a_2746, ._1 = false,};
  return tmp_2957;
}
typedef struct {
  uint8_t valid;
  uint32_t key;
  uint32_t mask;
  uint32_t action_tag;
  uint32_t action_arg;
} cellty_ftbl_2748;
typedef struct {
  struct {
    uint32_t action_tag;
    uint32_t action_arg;
  } _default;
  uint32_t max_idx;
  cellty_ftbl_2748 entries [1024];
} ty_ftbl_2748;
ty_ftbl_2748 ftbl_2748  = {._default = {.action_tag = tag_miss_acn_2747, .action_arg = 1}, .max_idx = 0, .entries = {0}};
void install_ftbl_2748(uint32_t key , uint32_t action , uint32_t const_arg ){
  for (int _idx = 0; _idx < 1024; _idx++) {
    if (ftbl_2748.entries[_idx].valid == false) {
      cellty_ftbl_2748 tmp_2958  = {.valid = true, .key = key, .mask = 4294967295, .action_tag = action, .action_arg = const_arg};
      ftbl_2748.entries[_idx] = tmp_2958;
      if (_idx >= ftbl_2748.max_idx) {
        ftbl_2748.max_idx = _idx + 1;
      }
      return ;
    }
  }
  return ;
}
void install_ternary_ftbl_2748(uint32_t key , uint32_t mask , uint32_t action , uint32_t const_arg ){
  for (int _idx = 0; _idx < 1024; _idx++) {
    if (ftbl_2748.entries[_idx].valid == false) {
      cellty_ftbl_2748 tmp_2959  = {.valid = true, .key = key, .mask = mask, .action_tag = action, .action_arg = const_arg};
      ftbl_2748.entries[_idx] = tmp_2959;
      if (_idx >= ftbl_2748.max_idx) {
        ftbl_2748.max_idx = _idx + 1;
      }
      return ;
    }
  }
  return ;
}
res_t_2741 lookup_ftbl_2748(uint32_t key , uint32_t arg ){
  for (int _idx = 0; _idx < 1024; _idx++) {
    if (_idx >= ftbl_2748.max_idx) {
      switch (ftbl_2748._default.action_tag) {
        case tag_hit_acn_2744: {
          return hit_acn_2744(ftbl_2748._default.action_arg, arg);
          break;
        }
        default: {
          return miss_acn_2747(ftbl_2748._default.action_arg, arg);
          break;
        }
      }
    }
    if (ftbl_2748.entries[_idx].valid && ((key & ftbl_2748.entries[_idx].mask) == (ftbl_2748.entries[_idx].key & ftbl_2748.entries[_idx].mask))) {
      switch (ftbl_2748.entries[_idx].action_tag) {
        case tag_hit_acn_2744: {
          return hit_acn_2744(ftbl_2748.entries[_idx].action_arg, arg);
          break;
        }
        case tag_miss_acn_2747: {
          return miss_acn_2747(ftbl_2748.entries[_idx].action_arg, arg);
          break;
        }
      }
    }
  }
  switch (ftbl_2748._default.action_tag) {
    case tag_hit_acn_2744: {
      return hit_acn_2744(ftbl_2748._default.action_arg, arg);
      break;
    }
    default: {
      return miss_acn_2747(ftbl_2748._default.action_arg, arg);
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
          uint32_t s_2749  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_do_match(s_2749);
          return packet->cursor <= packet->end;
          break;
        }
        case 2: {
          uint32_t v_2751  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t m_2752  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_do_install(v_2751, m_2752);
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
      uint32_t s_2749  = ev_out->data.args.do_match_2750.s_2749;
      write_bits(buf_out, ((uint64_t)(s_2749)), 32);
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
      uint32_t v_2751  = ev_out->data.args.do_install_2753.v_2751;
      uint32_t m_2752  = ev_out->data.args.do_install_2753.m_2752;
      write_bits(buf_out, ((uint64_t)(m_2752)), 32);
      write_bits(buf_out, ((uint64_t)(v_2751)), 32);
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
      uint32_t s_2754  = ev_in->data.args.do_match_2750.s_2749;
      event_t this  = mk_do_match(s_2754);
      res_t_2741 tup_2953  = lookup_ftbl_2748(s_2754, 1000);
      uint32_t tbl_out_0_2756  = tup_2953._0;
      uint8_t tbl_out_1_2757  = tup_2953._1;
      printf("return of table match: %u\n", tbl_out_0_2756);
      if (tbl_out_1_2757 == true) {
        printf("table HIT.\n");
      }else {
        printf("table MISS. Installing hit_acn(3) for current key. \n");
        install_ftbl_2748(s_2754, tag_hit_acn_2744, 3);
      }
      break;
    }
    case 2: {
      uint32_t v_2758  = ev_in->data.args.do_install_2753.v_2751;
      uint32_t m_2759  = ev_in->data.args.do_install_2753.m_2752;
      event_t this  = mk_do_install(v_2758, m_2759);
      printf("installing entry: (%u &&& %u)\n", v_2758, m_2759);
      install_ftbl_2748(v_2758, tag_hit_acn_2744, 2);
      install_ternary_ftbl_2748(v_2758, m_2759, tag_hit_acn_2744, 3);
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

