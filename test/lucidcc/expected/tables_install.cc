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
enum {tag_hit_acn_2409 = 0, tag_miss_acn_2412 = 1};

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
uint16_t do_query_tag  = 3;
typedef struct {
  uint16_t tag;
  union {
    struct {
      uint32_t key_2414;
      uint32_t val_2415;
      uint32_t hit_2416;
    } query_result_2417;
    struct {
      uint32_t key_2418;
      uint32_t val_2419;
    } do_install_2420;
    struct {
      uint32_t key_2421;
    } do_query_2422;
  } args;
} event_variant_t;
event_variant_t query_result_2417(uint32_t key_2414 , uint32_t val_2415 , uint32_t hit_2416 ){
  event_variant_t ev  = {0};
  ev.args.query_result_2417.key_2414 = key_2414;
  ev.args.query_result_2417.val_2415 = val_2415;
  ev.args.query_result_2417.hit_2416 = hit_2416;
  ev.tag = query_result_tag;
  return ev;
}
event_variant_t do_install_2420(uint32_t key_2418 , uint32_t val_2419 ){
  event_variant_t ev  = {0};
  ev.args.do_install_2420.key_2418 = key_2418;
  ev.args.do_install_2420.val_2419 = val_2419;
  ev.tag = do_install_tag;
  return ev;
}
event_variant_t do_query_2422(uint32_t key_2421 ){
  event_variant_t ev  = {0};
  ev.args.do_query_2422.key_2421 = key_2421;
  ev.tag = do_query_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_query_result(uint32_t key_2414 , uint32_t val_2415 , uint32_t hit_2416 ){
  event_t tmp_2586  = {.meta = {.len = 12, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = query_result_2417(key_2414, val_2415, hit_2416)};
  return tmp_2586;
}
event_t mk_do_install(uint32_t key_2418 , uint32_t val_2419 ){
  event_t tmp_2587  = {.meta = {.len = 8, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_install_2420(key_2418, val_2419)};
  return tmp_2587;
}
event_t mk_do_query(uint32_t key_2421 ){
  event_t tmp_2588  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_query_2422(key_2421)};
  return tmp_2588;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
typedef tuple_1 res_t_2406;
res_t_2406 hit_acn_2409(uint32_t x_2407 , uint32_t a_2408 ){
  res_t_2406 tmp_2589  = {._0 = x_2407, ._1 = true,};
  return tmp_2589;
}
res_t_2406 miss_acn_2412(uint32_t b_2410 , uint32_t a_2411 ){
  res_t_2406 tmp_2590  = {._0 = a_2411, ._1 = false,};
  return tmp_2590;
}
typedef struct {
  uint8_t valid;
  uint32_t key;
  uint32_t action_tag;
  uint32_t action_arg;
} cellty_ftbl_2413;
typedef struct {
  struct {
    uint32_t action_tag;
    uint32_t action_arg;
  } _default;
  uint32_t num_entries;
  cellty_ftbl_2413 entries [2048];
} ty_ftbl_2413;
ty_ftbl_2413 ftbl_2413  = {._default = {.action_tag = tag_miss_acn_2412, .action_arg = 1}, .num_entries = 0, .entries = {0}};
void warn_full_ftbl_2413(){
  static int warned = 0;
  table_full_installs++;
  if (!warned) { warned = 1; fprintf(stderr, "warning: table ftbl is full, an entry was not installed\n"); }
}
void warn_update_ftbl_2413(){
  static int warned = 0;
  if (!warned) { warned = 1; fprintf(stderr, "warning: table ftbl has no such entry to update\n"); }
}
void warn_remove_ftbl_2413(){
  static int warned = 0;
  if (!warned) { warned = 1; fprintf(stderr, "warning: table ftbl has no such entry to remove\n"); }
}
uint32_t hash_ftbl_2413(uint32_t f0 ){
  uint64_t h = 0;
  h = table_hash_mix(h, (uint64_t)f0);
  return (uint32_t)h & 2047;
}
void install_ftbl_2413(uint32_t key , uint32_t action , uint32_t const_arg ){
  uint32_t home  = hash_ftbl_2413(key);
  for (int _idx = 0; _idx < 2048; _idx++) {
    uint32_t slot  = (home + _idx) & 2047;
    if (ftbl_2413.entries[slot].valid == false) {
      if (ftbl_2413.num_entries == 1024) {
        warn_full_ftbl_2413();
        return ;
      }
      cellty_ftbl_2413 tmp_2591  = {.valid = true, .key = key, .action_tag = action, .action_arg = const_arg};
      ftbl_2413.entries[slot] = tmp_2591;
      ftbl_2413.num_entries = ftbl_2413.num_entries + 1;
      return ;
    }
    if (ftbl_2413.entries[slot].key == key) {
      return ;
    }
  }
  return ;
}
void update_ftbl_2413(uint32_t key , uint32_t action , uint32_t const_arg ){
  uint32_t home  = hash_ftbl_2413(key);
  for (int _idx = 0; _idx < 2048; _idx++) {
    uint32_t slot  = (home + _idx) & 2047;
    if (ftbl_2413.entries[slot].valid == false) {
      warn_update_ftbl_2413();
      return ;
    }
    if (ftbl_2413.entries[slot].key == key) {
      ftbl_2413.entries[slot].action_tag = action;
      ftbl_2413.entries[slot].action_arg = const_arg;
      return ;
    }
  }
  return ;
}
void remove_ftbl_2413(uint32_t key ){
  uint8_t removed  = false;
  uint32_t hole  = 0;
  uint32_t home  = hash_ftbl_2413(key);
  for (int _idx = 0; _idx < 2048; _idx++) {
    uint32_t slot  = (home + _idx) & 2047;
    if (removed) {
      if (ftbl_2413.entries[slot].valid == false) {
        ftbl_2413.entries[hole].valid = false;
        ftbl_2413.num_entries = ftbl_2413.num_entries - 1;
        return ;
      }
      uint32_t entry_home  = hash_ftbl_2413(ftbl_2413.entries[slot].key);
      if (((slot - entry_home) & 2047) >= ((slot - hole) & 2047)) {
        ftbl_2413.entries[hole] = ftbl_2413.entries[slot];
        hole = slot;
      }
    }else {
      if (ftbl_2413.entries[slot].valid == false) {
        warn_remove_ftbl_2413();
        return ;
      }
      if (ftbl_2413.entries[slot].key == key) {
        removed = true;
        hole = slot;
      }
    }
  }
  return ;
}
res_t_2406 lookup_ftbl_2413(uint32_t key , uint32_t arg ){
  uint32_t home  = hash_ftbl_2413(key);
  for (int _idx = 0; _idx < 2048; _idx++) {
    uint32_t slot  = (home + _idx) & 2047;
    if (ftbl_2413.entries[slot].valid == false) {
      switch (ftbl_2413._default.action_tag) {
        case tag_hit_acn_2409: {
          return hit_acn_2409(ftbl_2413._default.action_arg, arg);
          break;
        }
        default: {
          return miss_acn_2412(ftbl_2413._default.action_arg, arg);
          break;
        }
      }
    }
    if (ftbl_2413.entries[slot].key == key) {
      switch (ftbl_2413.entries[slot].action_tag) {
        case tag_hit_acn_2409: {
          return hit_acn_2409(ftbl_2413.entries[slot].action_arg, arg);
          break;
        }
        case tag_miss_acn_2412: {
          return miss_acn_2412(ftbl_2413.entries[slot].action_arg, arg);
          break;
        }
      }
    }
  }
  switch (ftbl_2413._default.action_tag) {
    case tag_hit_acn_2409: {
      return hit_acn_2409(ftbl_2413._default.action_arg, arg);
      break;
    }
    default: {
      return miss_acn_2412(ftbl_2413._default.action_arg, arg);
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
          uint32_t key_2414  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t val_2415  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t hit_2416  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_query_result(key_2414, val_2415, hit_2416);
          return packet->cursor <= packet->end;
          break;
        }
        case 2: {
          uint32_t key_2418  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t val_2419  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_do_install(key_2418, val_2419);
          return packet->cursor <= packet->end;
          break;
        }
        case 3: {
          uint32_t key_2421  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_do_query(key_2421);
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
      uint32_t key_2414  = ev_out->data.args.query_result_2417.key_2414;
      uint32_t val_2415  = ev_out->data.args.query_result_2417.val_2415;
      uint32_t hit_2416  = ev_out->data.args.query_result_2417.hit_2416;
      write_bits(buf_out, ((uint64_t)(hit_2416)), 32);
      write_bits(buf_out, ((uint64_t)(val_2415)), 32);
      write_bits(buf_out, ((uint64_t)(key_2414)), 32);
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
      uint32_t key_2418  = ev_out->data.args.do_install_2420.key_2418;
      uint32_t val_2419  = ev_out->data.args.do_install_2420.val_2419;
      write_bits(buf_out, ((uint64_t)(val_2419)), 32);
      write_bits(buf_out, ((uint64_t)(key_2418)), 32);
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
      uint32_t key_2421  = ev_out->data.args.do_query_2422.key_2421;
      write_bits(buf_out, ((uint64_t)(key_2421)), 32);
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
      uint32_t key_2423  = ev_in->data.args.query_result_2417.key_2414;
      uint32_t val_2424  = ev_in->data.args.query_result_2417.val_2415;
      uint32_t hit_2425  = ev_in->data.args.query_result_2417.hit_2416;
      event_t this  = mk_query_result(key_2423, val_2424, hit_2425);
      break;
    }
    case 2: {
      uint32_t key_2426  = ev_in->data.args.do_install_2420.key_2418;
      uint32_t val_2427  = ev_in->data.args.do_install_2420.val_2419;
      event_t this  = mk_do_install(key_2426, val_2427);
      printf("installing entry: key=%u -> hit_acn(%u)\n", key_2426, val_2427);
      install_ftbl_2413(key_2426, tag_hit_acn_2409, val_2427);
      break;
    }
    case 3: {
      uint32_t key_2428  = ev_in->data.args.do_query_2422.key_2421;
      event_t this  = mk_do_query(key_2428);
      res_t_2406 tup_2585  = lookup_ftbl_2413(key_2428, 1000);
      uint32_t r_0_2429  = tup_2585._0;
      uint8_t r_1_2430  = tup_2585._1;
      uint32_t hit_2431  = 0;
      if (r_1_2430 == true) {
        hit_2431 = 1;
      }
      printf("query key=%u -> val=%u hit=%u\n", key_2428, r_0_2429, hit_2431);
      out_event_t tmp_2592  = {.ev = mk_query_result(key_2428, r_0_2429, hit_2431), .port = 1};
      out_events[n] = tmp_2592;
      n = n + 1;
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

