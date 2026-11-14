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

/* write the low n bits of v into buf at bit offset `off`, MSB-first.
   buf must be pre-zeroed (bits are only OR'd in). Packs hash and checksum args. */
static void put_bits(uint8_t* buf, uint32_t off, uint64_t v, int n) {
    for (int i = 0; i < n; i++) {
        if ((v >> (n - 1 - i)) & 1) {
            uint32_t b = off + i;
            buf[b >> 3] |= (uint8_t)(1u << (7 - (b & 7)));
        }
    }
}
uint64_t hash_64(uint32_t seed , uint8_t*  buf , uint32_t len_bits ){
  if (seed == 1) {
    // the identity hash: the low 64 bits of the bit string (the caller keeps
    // the low <size> bits)
    uint64_t v = 0;
    for (uint32_t i = len_bits > 64 ? len_bits - 64 : 0; i < len_bits; i++)
      v = (v << 1) | ((buf[i >> 3] >> (7 - (i & 7))) & 1);
    return v;
  }
  // placeholder hash (a byte sum of the zero-padded bit string)
  uint64_t h = seed;
  for (uint32_t i = 0; i < (len_bits + 7) / 8; i++) h += buf[i];
  return h;
}
uint8_t hash_args_1(uint32_t seed , uint32_t f0 ){
  uint8_t buf[4] = {0};
  put_bits(buf, 0, (uint64_t)f0, 32);
  return hash_64((uint32_t)seed, buf, 32) & 0xffULL;
}
uint8_t hash_args_0(uint8_t seed , uint32_t f0 ){
  uint8_t buf[4] = {0};
  put_bits(buf, 0, (uint64_t)f0, 32);
  return hash_64((uint32_t)seed, buf, 32) & 0xffULL;
}
enum {tag_WriteCacheTable_hit_acn_4355 = 0, tag_WriteCacheTable_miss_acn_4357 = 1};
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
      uint32_t k_4364;
      uint32_t v_4365;
    } do_set_4366;
    struct {
      uint32_t k_4367;
    } do_get_4368;
  } args;
} event_variant_t;
event_variant_t do_set_4366(uint32_t k_4364 , uint32_t v_4365 ){
  event_variant_t ev  = {0};
  ev.args.do_set_4366.k_4364 = k_4364;
  ev.args.do_set_4366.v_4365 = v_4365;
  ev.tag = do_set_tag;
  return ev;
}
event_variant_t do_get_4368(uint32_t k_4367 ){
  event_variant_t ev  = {0};
  ev.args.do_get_4368.k_4367 = k_4367;
  ev.tag = do_get_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_do_set(uint32_t k_4364 , uint32_t v_4365 ){
  event_t tmp_4636  = {.meta = {.len = 8, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_set_4366(k_4364, v_4365)};
  return tmp_4636;
}
event_t mk_do_get(uint32_t k_4367 ){
  event_t tmp_4637  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_get_4368(k_4367)};
  return tmp_4637;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
typedef tuple_1 res_t_4353;
res_t_4353 WriteCacheTable_hit_acn_4355(uint32_t x_4354 ){
  res_t_4353 tmp_4638  = {._0 = x_4354, ._1 = true,};
  return tmp_4638;
}
res_t_4353 WriteCacheTable_miss_acn_4357(uint32_t unused_4356 ){
  res_t_4353 tmp_4639  = {._0 = 0, ._1 = false,};
  return tmp_4639;
}
uint32_t global_cached_table_0_4361 [256] = {0};
uint32_t Array_update_complex_global_cached_table_0_4361_combined_memop_WriteCacheTable_set_if_empty_WriteCacheTable_set_if_empty_4632(uint8_t _idx , uint32_t newval_4359 , uint32_t newval_set_4629 ){
  uint32_t cell1  = global_cached_table_0_4361[(((uint32_t)(_idx)) % 256)];
  uint32_t cell2  = 0;
  uint32_t ret  = 0;
  uint8_t mbool_4630  = global_cached_table_0_4361[(((uint32_t)(_idx)) % 256)] == 0;
  uint8_t mbool_4631  = global_cached_table_0_4361[(((uint32_t)(_idx)) % 256)] == 0;
  if (mbool_4630) {
    cell1 = newval_set_4629;
  }
  if (!mbool_4630) {
    cell1 = global_cached_table_0_4361[(((uint32_t)(_idx)) % 256)];
  }
  if (mbool_4631) {
    cell2 = newval_4359;
  }
  if (!mbool_4631) {
    cell2 = global_cached_table_0_4361[(((uint32_t)(_idx)) % 256)];
  }
  if (true) {
    ret = cell2;
  }
  global_cached_table_0_4361[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint32_t Array_update_complex_global_cached_table_0_4361_get_get_memop_32_bit(uint8_t _idx , uint32_t unused1_get_memop_32_bit , uint32_t unused2_get_memop_32_bit ){
  uint32_t cell1  = global_cached_table_0_4361[(((uint32_t)(_idx)) % 256)];
  uint32_t cell2  = 0;
  uint32_t ret  = 0;
  if (true) {
    ret = global_cached_table_0_4361[(((uint32_t)(_idx)) % 256)];
  }
  global_cached_table_0_4361[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint32_t global_cached_table_1_4362 [256] = {0};
uint32_t Array_update_complex_global_cached_table_1_4362_set_set_memop_32_bit(uint8_t _idx , uint32_t new_val_set_memop_32_bit , uint32_t unused_set_memop_32_bit ){
  uint32_t cell1  = global_cached_table_1_4362[(((uint32_t)(_idx)) % 256)];
  uint32_t cell2  = 0;
  uint32_t ret  = 0;
  if (true) {
    cell1 = new_val_set_memop_32_bit;
  }
  global_cached_table_1_4362[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint32_t Array_update_complex_global_cached_table_1_4362_get_get_memop_32_bit(uint8_t _idx , uint32_t unused1_get_memop_32_bit , uint32_t unused2_get_memop_32_bit ){
  uint32_t cell1  = global_cached_table_1_4362[(((uint32_t)(_idx)) % 256)];
  uint32_t cell2  = 0;
  uint32_t ret  = 0;
  if (true) {
    ret = global_cached_table_1_4362[(((uint32_t)(_idx)) % 256)];
  }
  global_cached_table_1_4362[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
typedef struct {
  uint8_t valid;
  uint32_t key;
  uint32_t mask;
  uint32_t action_tag;
  uint32_t action_arg;
} cellty_global_cached_table_2_4363;
typedef struct {
  struct {
    uint32_t action_tag;
    uint32_t action_arg;
  } _default;
  uint32_t max_idx;
  cellty_global_cached_table_2_4363 entries [1024];
} ty_global_cached_table_2_4363;
ty_global_cached_table_2_4363 global_cached_table_2_4363  = {._default = {.action_tag = tag_WriteCacheTable_miss_acn_4357, .action_arg = 0}, .max_idx = 0, .entries = {0}};
void install_global_cached_table_2_4363(uint32_t key , uint32_t action , uint32_t const_arg ){
  for (int _idx = 0; _idx < 1024; _idx++) {
    if (global_cached_table_2_4363.entries[_idx].valid == false) {
      cellty_global_cached_table_2_4363 tmp_4640  = {.valid = true, .key = key, .mask = 4294967295, .action_tag = action, .action_arg = const_arg};
      global_cached_table_2_4363.entries[_idx] = tmp_4640;
      if (_idx >= global_cached_table_2_4363.max_idx) {
        global_cached_table_2_4363.max_idx = _idx + 1;
      }
      return ;
    }
  }
  return ;
}
void install_ternary_global_cached_table_2_4363(uint32_t key , uint32_t mask , uint32_t action , uint32_t const_arg ){
  for (int _idx = 0; _idx < 1024; _idx++) {
    if (global_cached_table_2_4363.entries[_idx].valid == false) {
      cellty_global_cached_table_2_4363 tmp_4641  = {.valid = true, .key = key, .mask = mask, .action_tag = action, .action_arg = const_arg};
      global_cached_table_2_4363.entries[_idx] = tmp_4641;
      if (_idx >= global_cached_table_2_4363.max_idx) {
        global_cached_table_2_4363.max_idx = _idx + 1;
      }
      return ;
    }
  }
  return ;
}
res_t_4353 lookup_global_cached_table_2_4363(uint32_t key ){
  for (int _idx = 0; _idx < 1024; _idx++) {
    if (_idx >= global_cached_table_2_4363.max_idx) {
      switch (global_cached_table_2_4363._default.action_tag) {
        case tag_WriteCacheTable_hit_acn_4355: {
          return WriteCacheTable_hit_acn_4355(global_cached_table_2_4363._default.action_arg);
          break;
        }
        default: {
          return WriteCacheTable_miss_acn_4357(global_cached_table_2_4363._default.action_arg);
          break;
        }
      }
    }
    if (global_cached_table_2_4363.entries[_idx].valid && ((key & global_cached_table_2_4363.entries[_idx].mask) == (global_cached_table_2_4363.entries[_idx].key & global_cached_table_2_4363.entries[_idx].mask))) {
      switch (global_cached_table_2_4363.entries[_idx].action_tag) {
        case tag_WriteCacheTable_hit_acn_4355: {
          return WriteCacheTable_hit_acn_4355(global_cached_table_2_4363.entries[_idx].action_arg);
          break;
        }
        case tag_WriteCacheTable_miss_acn_4357: {
          return WriteCacheTable_miss_acn_4357(global_cached_table_2_4363.entries[_idx].action_arg);
          break;
        }
      }
    }
  }
  switch (global_cached_table_2_4363._default.action_tag) {
    case tag_WriteCacheTable_hit_acn_4355: {
      return WriteCacheTable_hit_acn_4355(global_cached_table_2_4363._default.action_arg);
      break;
    }
    default: {
      return WriteCacheTable_miss_acn_4357(global_cached_table_2_4363._default.action_arg);
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
          uint32_t k_4364  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t v_4365  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_do_set(k_4364, v_4365);
          return packet->cursor <= packet->end;
          break;
        }
        case 2: {
          uint32_t k_4367  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_do_get(k_4367);
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
      uint32_t k_4364  = ev_out->data.args.do_set_4366.k_4364;
      uint32_t v_4365  = ev_out->data.args.do_set_4366.v_4365;
      write_bits(buf_out, ((uint64_t)(v_4365)), 32);
      write_bits(buf_out, ((uint64_t)(k_4364)), 32);
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
      uint32_t k_4367  = ev_out->data.args.do_get_4368.k_4367;
      write_bits(buf_out, ((uint64_t)(k_4367)), 32);
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
      uint32_t k_4369  = ev_in->data.args.do_set_4366.k_4364;
      uint32_t v_4370  = ev_in->data.args.do_set_4366.v_4365;
      event_t this  = mk_do_set(k_4369, v_4370);
      uint8_t idx_4371  = hash_args_0(1, k_4369);
      printf("hash size arg: %u\n", 8);
      printf("[set] index: %u\n", idx_4371);
      uint32_t cache_key_4372  = Array_update_complex_global_cached_table_0_4361_combined_memop_WriteCacheTable_set_if_empty_WriteCacheTable_set_if_empty_4632(idx_4371, k_4369, k_4369);
      if (cache_key_4372 == k_4369) {
        printf("installing entry for %u into CACHE at index %u\n", k_4369, idx_4371);
        Array_update_complex_global_cached_table_1_4362_set_set_memop_32_bit(idx_4371, v_4370, 0);
      }else {
        printf("installing entry for %u into TABLE\n", k_4369);
        install_global_cached_table_2_4363(k_4369, tag_WriteCacheTable_hit_acn_4355, v_4370);
      }
      false;
      break;
    }
    case 2: {
      uint32_t k_4373  = ev_in->data.args.do_get_4368.k_4367;
      event_t this  = mk_do_get(k_4373);
      uint32_t WriteCacheTable_get_ret_0_4374  = 0;
      uint8_t WriteCacheTable_get_ret_1_4375  = false;
      uint8_t idx_4376  = hash_args_1(1, k_4373);
      uint32_t stored_key_4377  = Array_update_complex_global_cached_table_0_4361_get_get_memop_32_bit(idx_4376, 0, 0);
      if (stored_key_4377 == k_4373) {
        uint32_t stored_val_4378  = Array_update_complex_global_cached_table_1_4362_get_get_memop_32_bit(idx_4376, 0, 0);
        WriteCacheTable_get_ret_0_4374 = stored_val_4378;
        WriteCacheTable_get_ret_1_4375 = true;
      }else {
        
        res_t_4353 tup_4635  = lookup_global_cached_table_2_4363(k_4373);
        WriteCacheTable_get_ret_0_4374 = tup_4635._0;
        WriteCacheTable_get_ret_1_4375 = tup_4635._1;
      }
      if (WriteCacheTable_get_ret_1_4375) {
        printf("key: %u result: %u\n", k_4373, WriteCacheTable_get_ret_0_4374);
      }else {
        printf("key: %u result: NOT FOUND\n", k_4373);
      }
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

