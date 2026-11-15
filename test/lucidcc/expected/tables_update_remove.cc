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

/* mix a field of a key into the hash of a table key (the 64-bit finalizer of MurmurHash3) */
static inline uint64_t table_hash_mix(uint64_t h, uint64_t v) {
    h ^= v;
    h ^= h >> 33; h *= 0xff51afd7ed558ccdULL;
    h ^= h >> 33; h *= 0xc4ceb9fe1a85ec53ULL;
    h ^= h >> 33;
    return h;
}
enum {tag_hit_acn_3187 = 0, tag_miss_acn_3190 = 1};

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
uint16_t query_result_tag  = 1;
uint16_t do_install_tag  = 2;
uint16_t do_update_tag  = 3;
uint16_t do_remove_tag  = 4;
uint16_t do_query_tag  = 5;
typedef struct {
  uint16_t tag;
  union {
    struct {
      uint32_t key_3192;
      uint32_t val_3193;
      uint32_t hit_3194;
    } query_result_3195;
    struct {
      uint32_t key_3196;
      uint32_t val_3197;
    } do_install_3198;
    struct {
      uint32_t key_3199;
      uint32_t val_3200;
    } do_update_3201;
    struct {
      uint32_t key_3202;
    } do_remove_3203;
    struct {
      uint32_t key_3204;
    } do_query_3205;
  } args;
} event_variant_t;
event_variant_t query_result_3195(uint32_t key_3192 , uint32_t val_3193 , uint32_t hit_3194 ){
  event_variant_t ev  = {0};
  ev.args.query_result_3195.key_3192 = key_3192;
  ev.args.query_result_3195.val_3193 = val_3193;
  ev.args.query_result_3195.hit_3194 = hit_3194;
  ev.tag = query_result_tag;
  return ev;
}
event_variant_t do_install_3198(uint32_t key_3196 , uint32_t val_3197 ){
  event_variant_t ev  = {0};
  ev.args.do_install_3198.key_3196 = key_3196;
  ev.args.do_install_3198.val_3197 = val_3197;
  ev.tag = do_install_tag;
  return ev;
}
event_variant_t do_update_3201(uint32_t key_3199 , uint32_t val_3200 ){
  event_variant_t ev  = {0};
  ev.args.do_update_3201.key_3199 = key_3199;
  ev.args.do_update_3201.val_3200 = val_3200;
  ev.tag = do_update_tag;
  return ev;
}
event_variant_t do_remove_3203(uint32_t key_3202 ){
  event_variant_t ev  = {0};
  ev.args.do_remove_3203.key_3202 = key_3202;
  ev.tag = do_remove_tag;
  return ev;
}
event_variant_t do_query_3205(uint32_t key_3204 ){
  event_variant_t ev  = {0};
  ev.args.do_query_3205.key_3204 = key_3204;
  ev.tag = do_query_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_query_result(uint32_t key_3192 , uint32_t val_3193 , uint32_t hit_3194 ){
  event_t tmp_3433  = {.meta = {.len = 12, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = query_result_3195(key_3192, val_3193, hit_3194)};
  return tmp_3433;
}
event_t mk_do_install(uint32_t key_3196 , uint32_t val_3197 ){
  event_t tmp_3434  = {.meta = {.len = 8, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_install_3198(key_3196, val_3197)};
  return tmp_3434;
}
event_t mk_do_update(uint32_t key_3199 , uint32_t val_3200 ){
  event_t tmp_3435  = {.meta = {.len = 8, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_update_3201(key_3199, val_3200)};
  return tmp_3435;
}
event_t mk_do_remove(uint32_t key_3202 ){
  event_t tmp_3436  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_remove_3203(key_3202)};
  return tmp_3436;
}
event_t mk_do_query(uint32_t key_3204 ){
  event_t tmp_3437  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_query_3205(key_3204)};
  return tmp_3437;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
typedef tuple_1 res_t_3184;
res_t_3184 hit_acn_3187(uint32_t x_3185 , uint32_t a_3186 ){
  res_t_3184 tmp_3438  = {._0 = x_3185, ._1 = true,};
  return tmp_3438;
}
res_t_3184 miss_acn_3190(uint32_t b_3188 , uint32_t a_3189 ){
  res_t_3184 tmp_3439  = {._0 = a_3189, ._1 = false,};
  return tmp_3439;
}
typedef struct {
  uint8_t valid;
  uint32_t key;
  uint32_t action_tag;
  uint32_t action_arg;
} cellty_ftbl_3191;
typedef struct {
  struct {
    uint32_t action_tag;
    uint32_t action_arg;
  } _default;
  uint32_t num_entries;
  cellty_ftbl_3191 entries [4];
} ty_ftbl_3191;
ty_ftbl_3191 ftbl_3191  = {._default = {.action_tag = tag_miss_acn_3190, .action_arg = 1}, .num_entries = 0, .entries = {0}};
void warn_full_ftbl_3191(){
  static int warned = 0;
  table_full_installs++;
  if (!warned) { warned = 1; fprintf(stderr, "warning: table ftbl is full, an entry was not installed\n"); }
}
void warn_update_ftbl_3191(){
  static int warned = 0;
  if (!warned) { warned = 1; fprintf(stderr, "warning: table ftbl has no such entry to update\n"); }
}
void warn_remove_ftbl_3191(){
  static int warned = 0;
  if (!warned) { warned = 1; fprintf(stderr, "warning: table ftbl has no such entry to remove\n"); }
}
uint32_t hash_ftbl_3191(uint32_t f0 ){
  uint64_t h = 0;
  h = table_hash_mix(h, (uint64_t)f0);
  return (uint32_t)h & 3;
}
void install_ftbl_3191(uint32_t key , uint32_t action , uint32_t const_arg ){
  uint32_t home  = hash_ftbl_3191(key);
  for (int _idx = 0; _idx < 4; _idx++) {
    uint32_t slot  = (home + _idx) & 3;
    if (ftbl_3191.entries[slot].valid == false) {
      if (ftbl_3191.num_entries == 2) {
        warn_full_ftbl_3191();
        return ;
      }
      cellty_ftbl_3191 tmp_3440  = {.valid = true, .key = key, .action_tag = action, .action_arg = const_arg};
      ftbl_3191.entries[slot] = tmp_3440;
      ftbl_3191.num_entries = ftbl_3191.num_entries + 1;
      return ;
    }
    if (ftbl_3191.entries[slot].key == key) {
      return ;
    }
  }
  return ;
}
void update_ftbl_3191(uint32_t key , uint32_t action , uint32_t const_arg ){
  uint32_t home  = hash_ftbl_3191(key);
  for (int _idx = 0; _idx < 4; _idx++) {
    uint32_t slot  = (home + _idx) & 3;
    if (ftbl_3191.entries[slot].valid == false) {
      warn_update_ftbl_3191();
      return ;
    }
    if (ftbl_3191.entries[slot].key == key) {
      ftbl_3191.entries[slot].action_tag = action;
      ftbl_3191.entries[slot].action_arg = const_arg;
      return ;
    }
  }
  return ;
}
void remove_ftbl_3191(uint32_t key ){
  uint8_t removed  = false;
  uint32_t hole  = 0;
  uint32_t home  = hash_ftbl_3191(key);
  for (int _idx = 0; _idx < 4; _idx++) {
    uint32_t slot  = (home + _idx) & 3;
    if (removed) {
      if (ftbl_3191.entries[slot].valid == false) {
        ftbl_3191.entries[hole].valid = false;
        ftbl_3191.num_entries = ftbl_3191.num_entries - 1;
        return ;
      }
      uint32_t entry_home  = hash_ftbl_3191(ftbl_3191.entries[slot].key);
      if (((slot - entry_home) & 3) >= ((slot - hole) & 3)) {
        ftbl_3191.entries[hole] = ftbl_3191.entries[slot];
        hole = slot;
      }
    }else {
      if (ftbl_3191.entries[slot].valid == false) {
        warn_remove_ftbl_3191();
        return ;
      }
      if (ftbl_3191.entries[slot].key == key) {
        removed = true;
        hole = slot;
      }
    }
  }
  return ;
}
res_t_3184 lookup_ftbl_3191(uint32_t key , uint32_t arg ){
  uint32_t home  = hash_ftbl_3191(key);
  for (int _idx = 0; _idx < 4; _idx++) {
    uint32_t slot  = (home + _idx) & 3;
    if (ftbl_3191.entries[slot].valid == false) {
      switch (ftbl_3191._default.action_tag) {
        case tag_hit_acn_3187: {
          return hit_acn_3187(ftbl_3191._default.action_arg, arg);
          break;
        }
        default: {
          return miss_acn_3190(ftbl_3191._default.action_arg, arg);
          break;
        }
      }
    }
    if (ftbl_3191.entries[slot].key == key) {
      switch (ftbl_3191.entries[slot].action_tag) {
        case tag_hit_acn_3187: {
          return hit_acn_3187(ftbl_3191.entries[slot].action_arg, arg);
          break;
        }
        case tag_miss_acn_3190: {
          return miss_acn_3190(ftbl_3191.entries[slot].action_arg, arg);
          break;
        }
      }
    }
  }
  switch (ftbl_3191._default.action_tag) {
    case tag_hit_acn_3187: {
      return hit_acn_3187(ftbl_3191._default.action_arg, arg);
      break;
    }
    default: {
      return miss_acn_3190(ftbl_3191._default.action_arg, arg);
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
          uint32_t key_3192  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t val_3193  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t hit_3194  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_query_result(key_3192, val_3193, hit_3194);
          return packet->cursor <= packet->end;
          break;
        }
        case 2: {
          uint32_t key_3196  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t val_3197  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_do_install(key_3196, val_3197);
          return packet->cursor <= packet->end;
          break;
        }
        case 3: {
          uint32_t key_3199  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t val_3200  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_do_update(key_3199, val_3200);
          return packet->cursor <= packet->end;
          break;
        }
        case 4: {
          uint32_t key_3202  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_do_remove(key_3202);
          return packet->cursor <= packet->end;
          break;
        }
        case 5: {
          uint32_t key_3204  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_do_query(key_3204);
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
      uint32_t key_3192  = ev_out->data.args.query_result_3195.key_3192;
      uint32_t val_3193  = ev_out->data.args.query_result_3195.val_3193;
      uint32_t hit_3194  = ev_out->data.args.query_result_3195.hit_3194;
      write_bits(buf_out, ((uint64_t)(hit_3194)), 32);
      write_bits(buf_out, ((uint64_t)(val_3193)), 32);
      write_bits(buf_out, ((uint64_t)(key_3192)), 32);
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
      uint32_t key_3196  = ev_out->data.args.do_install_3198.key_3196;
      uint32_t val_3197  = ev_out->data.args.do_install_3198.val_3197;
      write_bits(buf_out, ((uint64_t)(val_3197)), 32);
      write_bits(buf_out, ((uint64_t)(key_3196)), 32);
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
      uint32_t key_3199  = ev_out->data.args.do_update_3201.key_3199;
      uint32_t val_3200  = ev_out->data.args.do_update_3201.val_3200;
      write_bits(buf_out, ((uint64_t)(val_3200)), 32);
      write_bits(buf_out, ((uint64_t)(key_3199)), 32);
      if (ev_out->meta.is_packet == 0) {
        write_bits(buf_out, ((uint64_t)(3)), 16);
        write_bits(buf_out, ((uint64_t)(666)), 16);
        write_bits(buf_out, ((uint64_t)(2)), 48);
        write_bits(buf_out, ((uint64_t)(1)), 48);
      }
      return ;
      break;
    }
    case 4: {
      uint32_t key_3202  = ev_out->data.args.do_remove_3203.key_3202;
      write_bits(buf_out, ((uint64_t)(key_3202)), 32);
      if (ev_out->meta.is_packet == 0) {
        write_bits(buf_out, ((uint64_t)(4)), 16);
        write_bits(buf_out, ((uint64_t)(666)), 16);
        write_bits(buf_out, ((uint64_t)(2)), 48);
        write_bits(buf_out, ((uint64_t)(1)), 48);
      }
      return ;
      break;
    }
    case 5: {
      uint32_t key_3204  = ev_out->data.args.do_query_3205.key_3204;
      write_bits(buf_out, ((uint64_t)(key_3204)), 32);
      if (ev_out->meta.is_packet == 0) {
        write_bits(buf_out, ((uint64_t)(5)), 16);
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
      uint32_t key_3206  = ev_in->data.args.query_result_3195.key_3192;
      uint32_t val_3207  = ev_in->data.args.query_result_3195.val_3193;
      uint32_t hit_3208  = ev_in->data.args.query_result_3195.hit_3194;
      event_t this  = mk_query_result(key_3206, val_3207, hit_3208);
      break;
    }
    case 2: {
      uint32_t key_3209  = ev_in->data.args.do_install_3198.key_3196;
      uint32_t val_3210  = ev_in->data.args.do_install_3198.val_3197;
      event_t this  = mk_do_install(key_3209, val_3210);
      install_ftbl_3191(key_3209, tag_hit_acn_3187, val_3210);
      break;
    }
    case 3: {
      uint32_t key_3211  = ev_in->data.args.do_update_3201.key_3199;
      uint32_t val_3212  = ev_in->data.args.do_update_3201.val_3200;
      event_t this  = mk_do_update(key_3211, val_3212);
      update_ftbl_3191(key_3211, tag_hit_acn_3187, val_3212);
      break;
    }
    case 4: {
      uint32_t key_3213  = ev_in->data.args.do_remove_3203.key_3202;
      event_t this  = mk_do_remove(key_3213);
      remove_ftbl_3191(key_3213);
      break;
    }
    case 5: {
      uint32_t key_3214  = ev_in->data.args.do_query_3205.key_3204;
      event_t this  = mk_do_query(key_3214);
      res_t_3184 tup_3432  = lookup_ftbl_3191(key_3214, 1000);
      uint32_t r_0_3215  = tup_3432._0;
      uint8_t r_1_3216  = tup_3432._1;
      uint32_t hit_3217  = 0;
      if (r_1_3216 == true) {
        hit_3217 = 1;
      }
      printf("query key=%u -> val=%u hit=%u\n", key_3214, r_0_3215, hit_3217);
      out_event_t tmp_3441  = {.ev = mk_query_result(key_3214, r_0_3215, hit_3217), .port = 1};
      out_events[n] = tmp_3441;
      n = n + 1;
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

