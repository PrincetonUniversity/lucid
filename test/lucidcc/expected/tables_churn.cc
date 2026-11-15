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
enum {tag_hit_acn_2550 = 0, tag_miss_acn_2553 = 1};

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
uint16_t do_remove_tag  = 3;
uint16_t do_query_tag  = 4;
typedef struct {
  uint16_t tag;
  union {
    struct {
      uint32_t key_2555;
      uint32_t val_2556;
      uint32_t hit_2557;
    } query_result_2558;
    struct {
      uint32_t key_2559;
      uint32_t val_2560;
    } do_install_2561;
    struct {
      uint32_t key_2562;
    } do_remove_2563;
    struct {
      uint32_t key_2564;
    } do_query_2565;
  } args;
} event_variant_t;
event_variant_t query_result_2558(uint32_t key_2555 , uint32_t val_2556 , uint32_t hit_2557 ){
  event_variant_t ev  = {0};
  ev.args.query_result_2558.key_2555 = key_2555;
  ev.args.query_result_2558.val_2556 = val_2556;
  ev.args.query_result_2558.hit_2557 = hit_2557;
  ev.tag = query_result_tag;
  return ev;
}
event_variant_t do_install_2561(uint32_t key_2559 , uint32_t val_2560 ){
  event_variant_t ev  = {0};
  ev.args.do_install_2561.key_2559 = key_2559;
  ev.args.do_install_2561.val_2560 = val_2560;
  ev.tag = do_install_tag;
  return ev;
}
event_variant_t do_remove_2563(uint32_t key_2562 ){
  event_variant_t ev  = {0};
  ev.args.do_remove_2563.key_2562 = key_2562;
  ev.tag = do_remove_tag;
  return ev;
}
event_variant_t do_query_2565(uint32_t key_2564 ){
  event_variant_t ev  = {0};
  ev.args.do_query_2565.key_2564 = key_2564;
  ev.tag = do_query_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_query_result(uint32_t key_2555 , uint32_t val_2556 , uint32_t hit_2557 ){
  event_t tmp_2745  = {.meta = {.len = 12, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = query_result_2558(key_2555, val_2556, hit_2557)};
  return tmp_2745;
}
event_t mk_do_install(uint32_t key_2559 , uint32_t val_2560 ){
  event_t tmp_2746  = {.meta = {.len = 8, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_install_2561(key_2559, val_2560)};
  return tmp_2746;
}
event_t mk_do_remove(uint32_t key_2562 ){
  event_t tmp_2747  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_remove_2563(key_2562)};
  return tmp_2747;
}
event_t mk_do_query(uint32_t key_2564 ){
  event_t tmp_2748  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_query_2565(key_2564)};
  return tmp_2748;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
typedef tuple_1 res_t_2547;
res_t_2547 hit_acn_2550(uint32_t x_2548 , uint32_t a_2549 ){
  res_t_2547 tmp_2749  = {._0 = x_2548, ._1 = true,};
  return tmp_2749;
}
res_t_2547 miss_acn_2553(uint32_t b_2551 , uint32_t a_2552 ){
  res_t_2547 tmp_2750  = {._0 = a_2552, ._1 = false,};
  return tmp_2750;
}
typedef struct {
  uint8_t valid;
  uint32_t key;
  uint32_t action_tag;
  uint32_t action_arg;
} cellty_ftbl_2554;
typedef struct {
  struct {
    uint32_t action_tag;
    uint32_t action_arg;
  } _default;
  uint32_t num_entries;
  cellty_ftbl_2554 entries [64];
} ty_ftbl_2554;
ty_ftbl_2554 ftbl_2554  = {._default = {.action_tag = tag_miss_acn_2553, .action_arg = 1}, .num_entries = 0, .entries = {0}};
void warn_full_ftbl_2554(){
  static int warned = 0;
  table_full_installs++;
  if (!warned) { warned = 1; fprintf(stderr, "warning: table ftbl is full, an entry was not installed\n"); }
}
void warn_update_ftbl_2554(){
  static int warned = 0;
  if (!warned) { warned = 1; fprintf(stderr, "warning: table ftbl has no such entry to update\n"); }
}
void warn_remove_ftbl_2554(){
  static int warned = 0;
  if (!warned) { warned = 1; fprintf(stderr, "warning: table ftbl has no such entry to remove\n"); }
}
uint32_t hash_ftbl_2554(uint32_t f0 ){
  uint64_t h = 0;
  h = table_hash_mix(h, (uint64_t)f0);
  return (uint32_t)h & 63;
}
void install_ftbl_2554(uint32_t key , uint32_t action , uint32_t const_arg ){
  uint32_t home  = hash_ftbl_2554(key);
  for (int _idx = 0; _idx < 64; _idx++) {
    uint32_t slot  = (home + _idx) & 63;
    if (ftbl_2554.entries[slot].valid == false) {
      if (ftbl_2554.num_entries == 32) {
        warn_full_ftbl_2554();
        return ;
      }
      cellty_ftbl_2554 tmp_2751  = {.valid = true, .key = key, .action_tag = action, .action_arg = const_arg};
      ftbl_2554.entries[slot] = tmp_2751;
      ftbl_2554.num_entries = ftbl_2554.num_entries + 1;
      return ;
    }
    if (ftbl_2554.entries[slot].key == key) {
      return ;
    }
  }
  return ;
}
void update_ftbl_2554(uint32_t key , uint32_t action , uint32_t const_arg ){
  uint32_t home  = hash_ftbl_2554(key);
  for (int _idx = 0; _idx < 64; _idx++) {
    uint32_t slot  = (home + _idx) & 63;
    if (ftbl_2554.entries[slot].valid == false) {
      warn_update_ftbl_2554();
      return ;
    }
    if (ftbl_2554.entries[slot].key == key) {
      ftbl_2554.entries[slot].action_tag = action;
      ftbl_2554.entries[slot].action_arg = const_arg;
      return ;
    }
  }
  return ;
}
void remove_ftbl_2554(uint32_t key ){
  uint8_t removed  = false;
  uint32_t hole  = 0;
  uint32_t home  = hash_ftbl_2554(key);
  for (int _idx = 0; _idx < 64; _idx++) {
    uint32_t slot  = (home + _idx) & 63;
    if (removed) {
      if (ftbl_2554.entries[slot].valid == false) {
        ftbl_2554.entries[hole].valid = false;
        ftbl_2554.num_entries = ftbl_2554.num_entries - 1;
        return ;
      }
      uint32_t entry_home  = hash_ftbl_2554(ftbl_2554.entries[slot].key);
      if (((slot - entry_home) & 63) >= ((slot - hole) & 63)) {
        ftbl_2554.entries[hole] = ftbl_2554.entries[slot];
        hole = slot;
      }
    }else {
      if (ftbl_2554.entries[slot].valid == false) {
        warn_remove_ftbl_2554();
        return ;
      }
      if (ftbl_2554.entries[slot].key == key) {
        removed = true;
        hole = slot;
      }
    }
  }
  return ;
}
res_t_2547 lookup_ftbl_2554(uint32_t key , uint32_t arg ){
  uint32_t home  = hash_ftbl_2554(key);
  for (int _idx = 0; _idx < 64; _idx++) {
    uint32_t slot  = (home + _idx) & 63;
    if (ftbl_2554.entries[slot].valid == false) {
      switch (ftbl_2554._default.action_tag) {
        case tag_hit_acn_2550: {
          return hit_acn_2550(ftbl_2554._default.action_arg, arg);
          break;
        }
        default: {
          return miss_acn_2553(ftbl_2554._default.action_arg, arg);
          break;
        }
      }
    }
    if (ftbl_2554.entries[slot].key == key) {
      switch (ftbl_2554.entries[slot].action_tag) {
        case tag_hit_acn_2550: {
          return hit_acn_2550(ftbl_2554.entries[slot].action_arg, arg);
          break;
        }
        case tag_miss_acn_2553: {
          return miss_acn_2553(ftbl_2554.entries[slot].action_arg, arg);
          break;
        }
      }
    }
  }
  switch (ftbl_2554._default.action_tag) {
    case tag_hit_acn_2550: {
      return hit_acn_2550(ftbl_2554._default.action_arg, arg);
      break;
    }
    default: {
      return miss_acn_2553(ftbl_2554._default.action_arg, arg);
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
          uint32_t key_2555  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t val_2556  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t hit_2557  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_query_result(key_2555, val_2556, hit_2557);
          return packet->cursor <= packet->end;
          break;
        }
        case 2: {
          uint32_t key_2559  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t val_2560  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_do_install(key_2559, val_2560);
          return packet->cursor <= packet->end;
          break;
        }
        case 3: {
          uint32_t key_2562  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_do_remove(key_2562);
          return packet->cursor <= packet->end;
          break;
        }
        case 4: {
          uint32_t key_2564  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_do_query(key_2564);
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
      uint32_t key_2555  = ev_out->data.args.query_result_2558.key_2555;
      uint32_t val_2556  = ev_out->data.args.query_result_2558.val_2556;
      uint32_t hit_2557  = ev_out->data.args.query_result_2558.hit_2557;
      write_bits(buf_out, ((uint64_t)(hit_2557)), 32);
      write_bits(buf_out, ((uint64_t)(val_2556)), 32);
      write_bits(buf_out, ((uint64_t)(key_2555)), 32);
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
      uint32_t key_2559  = ev_out->data.args.do_install_2561.key_2559;
      uint32_t val_2560  = ev_out->data.args.do_install_2561.val_2560;
      write_bits(buf_out, ((uint64_t)(val_2560)), 32);
      write_bits(buf_out, ((uint64_t)(key_2559)), 32);
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
      uint32_t key_2562  = ev_out->data.args.do_remove_2563.key_2562;
      write_bits(buf_out, ((uint64_t)(key_2562)), 32);
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
      uint32_t key_2564  = ev_out->data.args.do_query_2565.key_2564;
      write_bits(buf_out, ((uint64_t)(key_2564)), 32);
      if (ev_out->meta.is_packet == 0) {
        write_bits(buf_out, ((uint64_t)(4)), 16);
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
      uint32_t key_2566  = ev_in->data.args.query_result_2558.key_2555;
      uint32_t val_2567  = ev_in->data.args.query_result_2558.val_2556;
      uint32_t hit_2568  = ev_in->data.args.query_result_2558.hit_2557;
      event_t this  = mk_query_result(key_2566, val_2567, hit_2568);
      break;
    }
    case 2: {
      uint32_t key_2569  = ev_in->data.args.do_install_2561.key_2559;
      uint32_t val_2570  = ev_in->data.args.do_install_2561.val_2560;
      event_t this  = mk_do_install(key_2569, val_2570);
      install_ftbl_2554(key_2569, tag_hit_acn_2550, val_2570);
      break;
    }
    case 3: {
      uint32_t key_2571  = ev_in->data.args.do_remove_2563.key_2562;
      event_t this  = mk_do_remove(key_2571);
      remove_ftbl_2554(key_2571);
      break;
    }
    case 4: {
      uint32_t key_2572  = ev_in->data.args.do_query_2565.key_2564;
      event_t this  = mk_do_query(key_2572);
      res_t_2547 tup_2744  = lookup_ftbl_2554(key_2572, 1000);
      uint32_t r_0_2573  = tup_2744._0;
      uint8_t r_1_2574  = tup_2744._1;
      uint32_t hit_2575  = 0;
      if (r_1_2574 == true) {
        hit_2575 = 1;
      }
      out_event_t tmp_2752  = {.ev = mk_query_result(key_2572, r_0_2573, hit_2575), .port = 1};
      out_events[n] = tmp_2752;
      n = n + 1;
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

