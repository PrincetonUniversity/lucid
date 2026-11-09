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
enum {tag_hit_acn_2380 = 0, tag_miss_acn_2383 = 1};
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
      uint32_t key_2385;
      uint32_t val_2386;
      uint32_t hit_2387;
    } query_result_2388;
    struct {
      uint32_t key_2389;
      uint32_t val_2390;
    } do_install_2391;
    struct {
      uint32_t key_2392;
    } do_query_2393;
  } args;
} event_variant_t;
event_variant_t query_result_2388(uint32_t key_2385 , uint32_t val_2386 , uint32_t hit_2387 ){
  event_variant_t ev  = {0};
  ev.args.query_result_2388.key_2385 = key_2385;
  ev.args.query_result_2388.val_2386 = val_2386;
  ev.args.query_result_2388.hit_2387 = hit_2387;
  ev.tag = query_result_tag;
  return ev;
}
event_variant_t do_install_2391(uint32_t key_2389 , uint32_t val_2390 ){
  event_variant_t ev  = {0};
  ev.args.do_install_2391.key_2389 = key_2389;
  ev.args.do_install_2391.val_2390 = val_2390;
  ev.tag = do_install_tag;
  return ev;
}
event_variant_t do_query_2393(uint32_t key_2392 ){
  event_variant_t ev  = {0};
  ev.args.do_query_2393.key_2392 = key_2392;
  ev.tag = do_query_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_query_result(uint32_t key_2385 , uint32_t val_2386 , uint32_t hit_2387 ){
  event_t tmp_2557  = {.meta = {.len = 12, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = query_result_2388(key_2385, val_2386, hit_2387)};
  return tmp_2557;
}
event_t mk_do_install(uint32_t key_2389 , uint32_t val_2390 ){
  event_t tmp_2558  = {.meta = {.len = 8, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_install_2391(key_2389, val_2390)};
  return tmp_2558;
}
event_t mk_do_query(uint32_t key_2392 ){
  event_t tmp_2559  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_query_2393(key_2392)};
  return tmp_2559;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
typedef tuple_1 res_t_2377;
res_t_2377 hit_acn_2380(uint32_t x_2378 , uint32_t a_2379 ){
  res_t_2377 tmp_2560  = {._0 = x_2378, ._1 = true,};
  return tmp_2560;
}
res_t_2377 miss_acn_2383(uint32_t b_2381 , uint32_t a_2382 ){
  res_t_2377 tmp_2561  = {._0 = a_2382, ._1 = false,};
  return tmp_2561;
}
typedef struct {
  uint8_t valid;
  uint32_t key;
  uint32_t mask;
  uint32_t action_tag;
  uint32_t action_arg;
} cellty_ftbl_2384;
typedef struct {
  struct {
    uint32_t action_tag;
    uint32_t action_arg;
  } _default;
  cellty_ftbl_2384 entries [1024];
} ty_ftbl_2384;
ty_ftbl_2384 ftbl_2384  = {._default = {.action_tag = tag_miss_acn_2383, .action_arg = 1}, .entries = {0}};
void install_ftbl_2384(uint32_t key , uint32_t action , uint32_t const_arg ){
  for (int _idx = 0; _idx < 1024; _idx++) {
    if (ftbl_2384.entries[_idx].valid == false) {
      cellty_ftbl_2384 tmp_2562  = {.valid = true, .key = key, .mask = 4294967295, .action_tag = action, .action_arg = const_arg};
      ftbl_2384.entries[_idx] = tmp_2562;
      return ;
    }
  }
  return ;
}
void install_ternary_ftbl_2384(uint32_t key , uint32_t mask , uint32_t action , uint32_t const_arg ){
  for (int _idx = 0; _idx < 1024; _idx++) {
    if (ftbl_2384.entries[_idx].valid == false) {
      cellty_ftbl_2384 tmp_2563  = {.valid = true, .key = key, .mask = mask, .action_tag = action, .action_arg = const_arg};
      ftbl_2384.entries[_idx] = tmp_2563;
      return ;
    }
  }
  return ;
}
res_t_2377 lookup_ftbl_2384(uint32_t key , uint32_t arg ){
  for (int _idx = 0; _idx < 1024; _idx++) {
    if (ftbl_2384.entries[_idx].valid && ((key & ftbl_2384.entries[_idx].mask) == (ftbl_2384.entries[_idx].key & ftbl_2384.entries[_idx].mask))) {
      switch (ftbl_2384.entries[_idx].action_tag) {
        case tag_hit_acn_2380: {
          return hit_acn_2380(ftbl_2384.entries[_idx].action_arg, arg);
          break;
        }
        case tag_miss_acn_2383: {
          return miss_acn_2383(ftbl_2384.entries[_idx].action_arg, arg);
          break;
        }
      }
    }
  }
  switch (ftbl_2384._default.action_tag) {
    case tag_hit_acn_2380: {
      return hit_acn_2380(ftbl_2384._default.action_arg, arg);
      break;
    }
    default: {
      return miss_acn_2383(ftbl_2384._default.action_arg, arg);
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
          uint32_t key_2385  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t val_2386  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t hit_2387  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_query_result(key_2385, val_2386, hit_2387);
          return packet->cursor <= packet->end;
          break;
        }
        case 2: {
          uint32_t key_2389  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t val_2390  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_do_install(key_2389, val_2390);
          return packet->cursor <= packet->end;
          break;
        }
        case 3: {
          uint32_t key_2392  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_do_query(key_2392);
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
      uint32_t key_2385  = ev_out->data.args.query_result_2388.key_2385;
      uint32_t val_2386  = ev_out->data.args.query_result_2388.val_2386;
      uint32_t hit_2387  = ev_out->data.args.query_result_2388.hit_2387;
      write_bits(buf_out, ((uint64_t)(hit_2387)), 32);
      write_bits(buf_out, ((uint64_t)(val_2386)), 32);
      write_bits(buf_out, ((uint64_t)(key_2385)), 32);
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
      uint32_t key_2389  = ev_out->data.args.do_install_2391.key_2389;
      uint32_t val_2390  = ev_out->data.args.do_install_2391.val_2390;
      write_bits(buf_out, ((uint64_t)(val_2390)), 32);
      write_bits(buf_out, ((uint64_t)(key_2389)), 32);
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
      uint32_t key_2392  = ev_out->data.args.do_query_2393.key_2392;
      write_bits(buf_out, ((uint64_t)(key_2392)), 32);
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
      uint32_t key_2394  = ev_in->data.args.query_result_2388.key_2385;
      uint32_t val_2395  = ev_in->data.args.query_result_2388.val_2386;
      uint32_t hit_2396  = ev_in->data.args.query_result_2388.hit_2387;
      event_t this  = mk_query_result(key_2394, val_2395, hit_2396);
      break;
    }
    case 2: {
      uint32_t key_2397  = ev_in->data.args.do_install_2391.key_2389;
      uint32_t val_2398  = ev_in->data.args.do_install_2391.val_2390;
      event_t this  = mk_do_install(key_2397, val_2398);
      printf("installing entry: key=%u -> hit_acn(%u)\n", key_2397, val_2398);
      install_ftbl_2384(key_2397, tag_hit_acn_2380, val_2398);
      break;
    }
    case 3: {
      uint32_t key_2399  = ev_in->data.args.do_query_2393.key_2392;
      event_t this  = mk_do_query(key_2399);
      res_t_2377 tup_2556  = lookup_ftbl_2384(key_2399, 1000);
      uint32_t r_0_2400  = tup_2556._0;
      uint8_t r_1_2401  = tup_2556._1;
      uint32_t hit_2402  = 0;
      if (r_1_2401 == true) {
        hit_2402 = 1;
      }
      printf("query key=%u -> val=%u hit=%u\n", key_2399, r_0_2400, hit_2402);
      out_event_t tmp_2564  = {.ev = mk_query_result(key_2399, r_0_2400, hit_2402), .port = 1};
      out_events[n] = tmp_2564;
      n = n + 1;
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

