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
enum {tag_WriteCacheTable_hit_acn_4315 = 0, tag_WriteCacheTable_miss_acn_4317 = 1};
typedef struct {
  uint16_t len;
  uint8_t is_packet;
  uint8_t has_payload;
  uint64_t timestamp;
  uint32_t in_port;
} event_meta;
uint16_t do_set_tag  = 1;
uint16_t do_get_tag  = 2;
typedef struct {
  uint16_t tag;
  union {
    struct {
      uint32_t k_4324;
      uint32_t v_4325;
    } do_set_4326;
    struct {
      uint32_t k_4327;
    } do_get_4328;
  } args;
} event_variant_t;
event_variant_t do_set_4326(uint32_t k_4324 , uint32_t v_4325 ){
  event_variant_t ev  = {0};
  ev.args.do_set_4326.k_4324 = k_4324;
  ev.args.do_set_4326.v_4325 = v_4325;
  ev.tag = do_set_tag;
  return ev;
}
event_variant_t do_get_4328(uint32_t k_4327 ){
  event_variant_t ev  = {0};
  ev.args.do_get_4328.k_4327 = k_4327;
  ev.tag = do_get_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_do_set(uint32_t k_4324 , uint32_t v_4325 ){
  event_t tmp_4596  = {.meta = {.len = 8, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_set_4326(k_4324, v_4325)};
  return tmp_4596;
}
event_t mk_do_get(uint32_t k_4327 ){
  event_t tmp_4597  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_get_4328(k_4327)};
  return tmp_4597;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
typedef tuple_1 res_t_4313;
res_t_4313 WriteCacheTable_hit_acn_4315(uint32_t x_4314 ){
  res_t_4313 tmp_4598  = {._0 = x_4314, ._1 = true,};
  return tmp_4598;
}
res_t_4313 WriteCacheTable_miss_acn_4317(uint32_t unused_4316 ){
  res_t_4313 tmp_4599  = {._0 = 0, ._1 = false,};
  return tmp_4599;
}
uint32_t global_cached_table_0_4321 [256] = {0};
uint32_t Array_update_complex_global_cached_table_0_4321_combined_memop_WriteCacheTable_set_if_empty_WriteCacheTable_set_if_empty_4592(uint8_t _idx , uint32_t newval_4319 , uint32_t newval_set_4589 ){
  uint32_t cell1  = global_cached_table_0_4321[(((uint32_t)(_idx)) % 256)];
  uint32_t cell2  = 0;
  uint32_t ret  = 0;
  uint8_t mbool_4590  = global_cached_table_0_4321[(((uint32_t)(_idx)) % 256)] == 0;
  uint8_t mbool_4591  = global_cached_table_0_4321[(((uint32_t)(_idx)) % 256)] == 0;
  if (mbool_4590) {
    cell1 = newval_set_4589;
  }
  if (!mbool_4590) {
    cell1 = global_cached_table_0_4321[(((uint32_t)(_idx)) % 256)];
  }
  if (mbool_4591) {
    cell2 = newval_4319;
  }
  if (!mbool_4591) {
    cell2 = global_cached_table_0_4321[(((uint32_t)(_idx)) % 256)];
  }
  if (true) {
    ret = cell2;
  }
  global_cached_table_0_4321[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint32_t Array_update_complex_global_cached_table_0_4321_get_get_memop_32_bit(uint8_t _idx , uint32_t unused1_get_memop_32_bit , uint32_t unused2_get_memop_32_bit ){
  uint32_t cell1  = global_cached_table_0_4321[(((uint32_t)(_idx)) % 256)];
  uint32_t cell2  = 0;
  uint32_t ret  = 0;
  if (true) {
    ret = global_cached_table_0_4321[(((uint32_t)(_idx)) % 256)];
  }
  global_cached_table_0_4321[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint32_t global_cached_table_1_4322 [256] = {0};
uint32_t Array_update_complex_global_cached_table_1_4322_set_set_memop_32_bit(uint8_t _idx , uint32_t new_val_set_memop_32_bit , uint32_t unused_set_memop_32_bit ){
  uint32_t cell1  = global_cached_table_1_4322[(((uint32_t)(_idx)) % 256)];
  uint32_t cell2  = 0;
  uint32_t ret  = 0;
  if (true) {
    cell1 = new_val_set_memop_32_bit;
  }
  global_cached_table_1_4322[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint32_t Array_update_complex_global_cached_table_1_4322_get_get_memop_32_bit(uint8_t _idx , uint32_t unused1_get_memop_32_bit , uint32_t unused2_get_memop_32_bit ){
  uint32_t cell1  = global_cached_table_1_4322[(((uint32_t)(_idx)) % 256)];
  uint32_t cell2  = 0;
  uint32_t ret  = 0;
  if (true) {
    ret = global_cached_table_1_4322[(((uint32_t)(_idx)) % 256)];
  }
  global_cached_table_1_4322[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
typedef struct {
  uint8_t valid;
  uint32_t key;
  uint32_t mask;
  uint32_t action_tag;
  uint32_t action_arg;
} cellty_global_cached_table_2_4323;
typedef struct {
  struct {
    uint32_t action_tag;
    uint32_t action_arg;
  } _default;
  cellty_global_cached_table_2_4323 entries [1024];
} ty_global_cached_table_2_4323;
ty_global_cached_table_2_4323 global_cached_table_2_4323  = {._default = {.action_tag = tag_WriteCacheTable_miss_acn_4317, .action_arg = 0}, .entries = {0}};
void install_global_cached_table_2_4323(uint32_t key , uint32_t action , uint32_t const_arg ){
  for (int _idx = 0; _idx < 1024; _idx++) {
    if (global_cached_table_2_4323.entries[_idx].valid == false) {
      cellty_global_cached_table_2_4323 tmp_4600  = {.valid = true, .key = key, .mask = 4294967295, .action_tag = action, .action_arg = const_arg};
      global_cached_table_2_4323.entries[_idx] = tmp_4600;
      return ;
    }
  }
  return ;
}
void install_ternary_global_cached_table_2_4323(uint32_t key , uint32_t mask , uint32_t action , uint32_t const_arg ){
  for (int _idx = 0; _idx < 1024; _idx++) {
    if (global_cached_table_2_4323.entries[_idx].valid == false) {
      cellty_global_cached_table_2_4323 tmp_4601  = {.valid = true, .key = key, .mask = mask, .action_tag = action, .action_arg = const_arg};
      global_cached_table_2_4323.entries[_idx] = tmp_4601;
      return ;
    }
  }
  return ;
}
res_t_4313 lookup_global_cached_table_2_4323(uint32_t key ){
  for (int _idx = 0; _idx < 1024; _idx++) {
    if (global_cached_table_2_4323.entries[_idx].valid && ((key & global_cached_table_2_4323.entries[_idx].mask) == (global_cached_table_2_4323.entries[_idx].key & global_cached_table_2_4323.entries[_idx].mask))) {
      switch (global_cached_table_2_4323.entries[_idx].action_tag) {
        case tag_WriteCacheTable_hit_acn_4315: {
          return WriteCacheTable_hit_acn_4315(global_cached_table_2_4323.entries[_idx].action_arg);
          break;
        }
        case tag_WriteCacheTable_miss_acn_4317: {
          return WriteCacheTable_miss_acn_4317(global_cached_table_2_4323.entries[_idx].action_arg);
          break;
        }
      }
    }
  }
  switch (global_cached_table_2_4323._default.action_tag) {
    case tag_WriteCacheTable_hit_acn_4315: {
      return WriteCacheTable_hit_acn_4315(global_cached_table_2_4323._default.action_arg);
      break;
    }
    default: {
      return WriteCacheTable_miss_acn_4317(global_cached_table_2_4323._default.action_arg);
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
          uint32_t k_4324  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t v_4325  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_do_set(k_4324, v_4325);
          return packet->cursor <= packet->end;
          break;
        }
        case 2: {
          uint32_t k_4327  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_do_get(k_4327);
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
      uint32_t k_4324  = ev_out->data.args.do_set_4326.k_4324;
      uint32_t v_4325  = ev_out->data.args.do_set_4326.v_4325;
      write_bits(buf_out, ((uint64_t)(v_4325)), 32);
      write_bits(buf_out, ((uint64_t)(k_4324)), 32);
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
      uint32_t k_4327  = ev_out->data.args.do_get_4328.k_4327;
      write_bits(buf_out, ((uint64_t)(k_4327)), 32);
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
      uint32_t k_4329  = ev_in->data.args.do_set_4326.k_4324;
      uint32_t v_4330  = ev_in->data.args.do_set_4326.v_4325;
      event_t this  = mk_do_set(k_4329, v_4330);
      uint8_t idx_4331  = hash_32((uint32_t)1, (uint8_t* )&k_4329, 32);
      printf("hash size arg: %u\n", 8);
      printf("[set] index: %u\n", idx_4331);
      uint32_t cache_key_4332  = Array_update_complex_global_cached_table_0_4321_combined_memop_WriteCacheTable_set_if_empty_WriteCacheTable_set_if_empty_4592(idx_4331, k_4329, k_4329);
      if (cache_key_4332 == k_4329) {
        printf("installing entry for %u into CACHE at index %u\n", k_4329, idx_4331);
        Array_update_complex_global_cached_table_1_4322_set_set_memop_32_bit(idx_4331, v_4330, 0);
      }else {
        printf("installing entry for %u into TABLE\n", k_4329);
        install_global_cached_table_2_4323(k_4329, tag_WriteCacheTable_hit_acn_4315, v_4330);
      }
      false;
      break;
    }
    case 2: {
      uint32_t k_4333  = ev_in->data.args.do_get_4328.k_4327;
      event_t this  = mk_do_get(k_4333);
      uint32_t WriteCacheTable_get_ret_0_4334  = 0;
      uint8_t WriteCacheTable_get_ret_1_4335  = false;
      uint8_t idx_4336  = hash_32((uint32_t)1, (uint8_t* )&k_4333, 32);
      uint32_t stored_key_4337  = Array_update_complex_global_cached_table_0_4321_get_get_memop_32_bit(idx_4336, 0, 0);
      if (stored_key_4337 == k_4333) {
        uint32_t stored_val_4338  = Array_update_complex_global_cached_table_1_4322_get_get_memop_32_bit(idx_4336, 0, 0);
        WriteCacheTable_get_ret_0_4334 = stored_val_4338;
        WriteCacheTable_get_ret_1_4335 = true;
      }else {
        
        res_t_4313 tup_4595  = lookup_global_cached_table_2_4323(k_4333);
        WriteCacheTable_get_ret_0_4334 = tup_4595._0;
        WriteCacheTable_get_ret_1_4335 = tup_4595._1;
      }
      if (WriteCacheTable_get_ret_1_4335) {
        printf("key: %u result: %u\n", k_4333, WriteCacheTable_get_ret_0_4334);
      }else {
        printf("key: %u result: NOT FOUND\n", k_4333);
      }
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

