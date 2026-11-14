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
enum {tag_hit_acn_2420 = 0, tag_miss_acn_2423 = 1};
typedef struct {
  uint16_t len;
  uint8_t is_packet;
  uint8_t has_payload;
  uint64_t timestamp;
  uint32_t in_port;
} event_meta;
uint16_t query_result_tag  = 1;
uint16_t do_install_tag  = 2;
uint16_t do_query_tag  = 3;
typedef struct {
  uint16_t tag;
  union {
    struct {
      uint32_t key_2425;
      uint32_t val_2426;
      uint32_t hit_2427;
    } query_result_2428;
    struct {
      uint32_t key_2429;
      uint32_t val_2430;
    } do_install_2431;
    struct {
      uint32_t key_2432;
    } do_query_2433;
  } args;
} event_variant_t;
event_variant_t query_result_2428(uint32_t key_2425 , uint32_t val_2426 , uint32_t hit_2427 ){
  event_variant_t ev  = {0};
  ev.args.query_result_2428.key_2425 = key_2425;
  ev.args.query_result_2428.val_2426 = val_2426;
  ev.args.query_result_2428.hit_2427 = hit_2427;
  ev.tag = query_result_tag;
  return ev;
}
event_variant_t do_install_2431(uint32_t key_2429 , uint32_t val_2430 ){
  event_variant_t ev  = {0};
  ev.args.do_install_2431.key_2429 = key_2429;
  ev.args.do_install_2431.val_2430 = val_2430;
  ev.tag = do_install_tag;
  return ev;
}
event_variant_t do_query_2433(uint32_t key_2432 ){
  event_variant_t ev  = {0};
  ev.args.do_query_2433.key_2432 = key_2432;
  ev.tag = do_query_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_query_result(uint32_t key_2425 , uint32_t val_2426 , uint32_t hit_2427 ){
  event_t tmp_2597  = {.meta = {.len = 12, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = query_result_2428(key_2425, val_2426, hit_2427)};
  return tmp_2597;
}
event_t mk_do_install(uint32_t key_2429 , uint32_t val_2430 ){
  event_t tmp_2598  = {.meta = {.len = 8, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_install_2431(key_2429, val_2430)};
  return tmp_2598;
}
event_t mk_do_query(uint32_t key_2432 ){
  event_t tmp_2599  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_query_2433(key_2432)};
  return tmp_2599;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
typedef tuple_1 res_t_2417;
res_t_2417 hit_acn_2420(uint32_t x_2418 , uint32_t a_2419 ){
  res_t_2417 tmp_2600  = {._0 = x_2418, ._1 = true,};
  return tmp_2600;
}
res_t_2417 miss_acn_2423(uint32_t b_2421 , uint32_t a_2422 ){
  res_t_2417 tmp_2601  = {._0 = a_2422, ._1 = false,};
  return tmp_2601;
}
typedef struct {
  uint8_t valid;
  uint32_t key;
  uint32_t mask;
  uint32_t action_tag;
  uint32_t action_arg;
} cellty_ftbl_2424;
typedef struct {
  struct {
    uint32_t action_tag;
    uint32_t action_arg;
  } _default;
  uint32_t max_idx;
  cellty_ftbl_2424 entries [1024];
} ty_ftbl_2424;
ty_ftbl_2424 ftbl_2424  = {._default = {.action_tag = tag_miss_acn_2423, .action_arg = 1}, .max_idx = 0, .entries = {0}};
void install_ftbl_2424(uint32_t key , uint32_t action , uint32_t const_arg ){
  for (int _idx = 0; _idx < 1024; _idx++) {
    if (ftbl_2424.entries[_idx].valid == false) {
      cellty_ftbl_2424 tmp_2602  = {.valid = true, .key = key, .mask = 4294967295, .action_tag = action, .action_arg = const_arg};
      ftbl_2424.entries[_idx] = tmp_2602;
      if (_idx >= ftbl_2424.max_idx) {
        ftbl_2424.max_idx = _idx + 1;
      }
      return ;
    }
  }
  return ;
}
void install_ternary_ftbl_2424(uint32_t key , uint32_t mask , uint32_t action , uint32_t const_arg ){
  for (int _idx = 0; _idx < 1024; _idx++) {
    if (ftbl_2424.entries[_idx].valid == false) {
      cellty_ftbl_2424 tmp_2603  = {.valid = true, .key = key, .mask = mask, .action_tag = action, .action_arg = const_arg};
      ftbl_2424.entries[_idx] = tmp_2603;
      if (_idx >= ftbl_2424.max_idx) {
        ftbl_2424.max_idx = _idx + 1;
      }
      return ;
    }
  }
  return ;
}
res_t_2417 lookup_ftbl_2424(uint32_t key , uint32_t arg ){
  for (int _idx = 0; _idx < 1024; _idx++) {
    if (_idx >= ftbl_2424.max_idx) {
      switch (ftbl_2424._default.action_tag) {
        case tag_hit_acn_2420: {
          return hit_acn_2420(ftbl_2424._default.action_arg, arg);
          break;
        }
        default: {
          return miss_acn_2423(ftbl_2424._default.action_arg, arg);
          break;
        }
      }
    }
    if (ftbl_2424.entries[_idx].valid && ((key & ftbl_2424.entries[_idx].mask) == (ftbl_2424.entries[_idx].key & ftbl_2424.entries[_idx].mask))) {
      switch (ftbl_2424.entries[_idx].action_tag) {
        case tag_hit_acn_2420: {
          return hit_acn_2420(ftbl_2424.entries[_idx].action_arg, arg);
          break;
        }
        case tag_miss_acn_2423: {
          return miss_acn_2423(ftbl_2424.entries[_idx].action_arg, arg);
          break;
        }
      }
    }
  }
  switch (ftbl_2424._default.action_tag) {
    case tag_hit_acn_2420: {
      return hit_acn_2420(ftbl_2424._default.action_arg, arg);
      break;
    }
    default: {
      return miss_acn_2423(ftbl_2424._default.action_arg, arg);
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
          uint32_t key_2425  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t val_2426  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t hit_2427  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_query_result(key_2425, val_2426, hit_2427);
          return packet->cursor <= packet->end;
          break;
        }
        case 2: {
          uint32_t key_2429  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t val_2430  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_do_install(key_2429, val_2430);
          return packet->cursor <= packet->end;
          break;
        }
        case 3: {
          uint32_t key_2432  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_do_query(key_2432);
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
      uint32_t key_2425  = ev_out->data.args.query_result_2428.key_2425;
      uint32_t val_2426  = ev_out->data.args.query_result_2428.val_2426;
      uint32_t hit_2427  = ev_out->data.args.query_result_2428.hit_2427;
      write_bits(buf_out, ((uint64_t)(hit_2427)), 32);
      write_bits(buf_out, ((uint64_t)(val_2426)), 32);
      write_bits(buf_out, ((uint64_t)(key_2425)), 32);
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
      uint32_t key_2429  = ev_out->data.args.do_install_2431.key_2429;
      uint32_t val_2430  = ev_out->data.args.do_install_2431.val_2430;
      write_bits(buf_out, ((uint64_t)(val_2430)), 32);
      write_bits(buf_out, ((uint64_t)(key_2429)), 32);
      if (ev_out->meta.is_packet == 0) {
        write_bits(buf_out, ((uint64_t)(2)), 16);
        write_bits(buf_out, ((uint64_t)(666)), 16);
        write_bits(buf_out, ((uint64_t)(2)), 48);
        write_bits(buf_out, ((uint64_t)(1)), 48);
      }
      return ;
      break;
    }
    case 3: {
      uint32_t key_2432  = ev_out->data.args.do_query_2433.key_2432;
      write_bits(buf_out, ((uint64_t)(key_2432)), 32);
      if (ev_out->meta.is_packet == 0) {
        write_bits(buf_out, ((uint64_t)(3)), 16);
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
      uint32_t key_2434  = ev_in->data.args.query_result_2428.key_2425;
      uint32_t val_2435  = ev_in->data.args.query_result_2428.val_2426;
      uint32_t hit_2436  = ev_in->data.args.query_result_2428.hit_2427;
      event_t this  = mk_query_result(key_2434, val_2435, hit_2436);
      break;
    }
    case 2: {
      uint32_t key_2437  = ev_in->data.args.do_install_2431.key_2429;
      uint32_t val_2438  = ev_in->data.args.do_install_2431.val_2430;
      event_t this  = mk_do_install(key_2437, val_2438);
      printf("installing entry: key=%u -> hit_acn(%u)\n", key_2437, val_2438);
      install_ftbl_2424(key_2437, tag_hit_acn_2420, val_2438);
      break;
    }
    case 3: {
      uint32_t key_2439  = ev_in->data.args.do_query_2433.key_2432;
      event_t this  = mk_do_query(key_2439);
      res_t_2417 tup_2596  = lookup_ftbl_2424(key_2439, 1000);
      uint32_t r_0_2440  = tup_2596._0;
      uint8_t r_1_2441  = tup_2596._1;
      uint32_t hit_2442  = 0;
      if (r_1_2441 == true) {
        hit_2442 = 1;
      }
      printf("query key=%u -> val=%u hit=%u\n", key_2439, r_0_2440, hit_2442);
      out_event_t tmp_2604  = {.ev = mk_query_result(key_2439, r_0_2440, hit_2442), .port = 1};
      out_events[n] = tmp_2604;
      n = n + 1;
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

