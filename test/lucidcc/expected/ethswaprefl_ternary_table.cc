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
enum {tag_set_ety_2433 = 0, tag_keep_ety_2436 = 1};

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
uint16_t ethpkt_tag  = 1;
uint16_t do_install_tag  = 2;
typedef struct {
  uint16_t tag;
  union {
    struct {
      uint64_t dst_2426;
      uint64_t src_2427;
      uint16_t ety_2428;
    } ethpkt_2430;
    struct {
      uint32_t key_2438;
      uint16_t new_ety_2439;
    } do_install_2440;
  } args;
} event_variant_t;
event_variant_t ethpkt_2430(uint64_t dst_2426 , uint64_t src_2427 , uint16_t ety_2428 ){
  event_variant_t ev  = {0};
  ev.args.ethpkt_2430.dst_2426 = dst_2426;
  ev.args.ethpkt_2430.src_2427 = src_2427;
  ev.args.ethpkt_2430.ety_2428 = ety_2428;
  ev.tag = ethpkt_tag;
  return ev;
}
event_variant_t do_install_2440(uint32_t key_2438 , uint16_t new_ety_2439 ){
  event_variant_t ev  = {0};
  ev.args.do_install_2440.key_2438 = key_2438;
  ev.args.do_install_2440.new_ety_2439 = new_ety_2439;
  ev.tag = do_install_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_ethpkt(uint64_t dst_2426 , uint64_t src_2427 , uint16_t ety_2428 ){
  event_t tmp_2619  = {.meta = {.len = 14, .is_packet = 1, .has_payload = 1, .timestamp = 0, .in_port = 0}, .data = ethpkt_2430(dst_2426, src_2427, ety_2428)};
  return tmp_2619;
}
event_t mk_do_install(uint32_t key_2438 , uint16_t new_ety_2439 ){
  event_t tmp_2620  = {.meta = {.len = 6, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_install_2440(key_2438, new_ety_2439)};
  return tmp_2620;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
uint16_t set_ety_2433(uint16_t new_ety_2431 , uint16_t ety_2432 ){
  return new_ety_2431;
}
uint16_t keep_ety_2436(uint16_t unused_2434 , uint16_t ety_2435 ){
  return ety_2435;
}
typedef struct {
  uint32_t key;
  uint32_t mask;
  uint32_t prio;
  uint32_t action_tag;
  uint16_t action_arg;
} cellty_ety_tbl_2437;
typedef struct {
  struct {
    uint32_t action_tag;
    uint16_t action_arg;
  } _default;
  uint32_t max_idx;
  cellty_ety_tbl_2437 entries [4096];
} ty_ety_tbl_2437;
ty_ety_tbl_2437 ety_tbl_2437  = {._default = {.action_tag = tag_keep_ety_2436, .action_arg = 0}, .max_idx = 0, .entries = {0}};
void warn_full_ety_tbl_2437(){
  static int warned = 0;
  table_full_installs++;
  if (!warned) { warned = 1; fprintf(stderr, "warning: table ety_tbl is full, an entry was not installed\n"); }
}
void warn_update_ety_tbl_2437(){
  static int warned = 0;
  if (!warned) { warned = 1; fprintf(stderr, "warning: table ety_tbl has no such entry to update\n"); }
}
void warn_remove_ety_tbl_2437(){
  static int warned = 0;
  if (!warned) { warned = 1; fprintf(stderr, "warning: table ety_tbl has no such entry to remove\n"); }
}
void install_ety_tbl_2437(uint32_t key , uint32_t mask , uint32_t prio , uint32_t action , uint16_t const_arg ){
  for (int _idx = 0; _idx < 4096; _idx++) {
    if (_idx < ety_tbl_2437.max_idx) {
      if ((ety_tbl_2437.entries[_idx].prio == prio) && ((ety_tbl_2437.entries[_idx].mask == mask) && ((ety_tbl_2437.entries[_idx].key & mask) == (key & mask)))) {
        return ;
      }
    }
  }
  if (ety_tbl_2437.max_idx == 4096) {
    warn_full_ety_tbl_2437();
    return ;
  }
  cellty_ety_tbl_2437 carry  = {.key = key, .mask = mask, .prio = prio, .action_tag = action, .action_arg = const_arg};
  for (int _idx = 0; _idx < 4096; _idx++) {
    if ((_idx < ety_tbl_2437.max_idx) && (ety_tbl_2437.entries[_idx].prio < prio)) {
      cellty_ety_tbl_2437 tmp  = ety_tbl_2437.entries[_idx];
      ety_tbl_2437.entries[_idx] = carry;
      carry = tmp;
    }
    if (_idx == ety_tbl_2437.max_idx) {
      ety_tbl_2437.entries[_idx] = carry;
      ety_tbl_2437.max_idx = _idx + 1;
      return ;
    }
  }
  return ;
}
void update_ety_tbl_2437(uint32_t key , uint32_t mask , uint32_t prio , uint32_t action , uint16_t const_arg ){
  for (int _idx = 0; _idx < 4096; _idx++) {
    if (_idx < ety_tbl_2437.max_idx) {
      if ((ety_tbl_2437.entries[_idx].prio == prio) && ((ety_tbl_2437.entries[_idx].mask == mask) && ((ety_tbl_2437.entries[_idx].key & mask) == (key & mask)))) {
        ety_tbl_2437.entries[_idx].action_tag = action;
        ety_tbl_2437.entries[_idx].action_arg = const_arg;
        return ;
      }
    }
  }
  warn_update_ety_tbl_2437();
  return ;
}
void remove_ety_tbl_2437(uint32_t key , uint32_t mask , uint32_t prio ){
  uint8_t removed  = false;
  for (int _idx = 0; _idx < 4096; _idx++) {
    if (_idx < ety_tbl_2437.max_idx) {
      if ((ety_tbl_2437.entries[_idx].prio == prio) && ((ety_tbl_2437.entries[_idx].mask == mask) && ((ety_tbl_2437.entries[_idx].key & mask) == (key & mask)))) {
        removed = true;
      }
      if (removed && ((_idx + 1) < ety_tbl_2437.max_idx)) {
        ety_tbl_2437.entries[_idx] = ety_tbl_2437.entries[_idx + 1];
      }
    }
  }
  if (removed) {
    ety_tbl_2437.max_idx = ety_tbl_2437.max_idx - 1;
    return ;
  }
  warn_remove_ety_tbl_2437();
  return ;
}
uint16_t lookup_ety_tbl_2437(uint32_t key , uint16_t arg ){
  for (int _idx = 0; _idx < 4096; _idx++) {
    if (_idx >= ety_tbl_2437.max_idx) {
      switch (ety_tbl_2437._default.action_tag) {
        case tag_set_ety_2433: {
          return set_ety_2433(ety_tbl_2437._default.action_arg, arg);
          break;
        }
        default: {
          return keep_ety_2436(ety_tbl_2437._default.action_arg, arg);
          break;
        }
      }
    }
    if ((key & ety_tbl_2437.entries[_idx].mask) == (ety_tbl_2437.entries[_idx].key & ety_tbl_2437.entries[_idx].mask)) {
      switch (ety_tbl_2437.entries[_idx].action_tag) {
        case tag_set_ety_2433: {
          return set_ety_2433(ety_tbl_2437.entries[_idx].action_arg, arg);
          break;
        }
        case tag_keep_ety_2436: {
          return keep_ety_2436(ety_tbl_2437.entries[_idx].action_arg, arg);
          break;
        }
      }
    }
  }
  switch (ety_tbl_2437._default.action_tag) {
    case tag_set_ety_2433: {
      return set_ety_2433(ety_tbl_2437._default.action_arg, arg);
      break;
    }
    default: {
      return keep_ety_2436(ety_tbl_2437._default.action_arg, arg);
      break;
    }
  }
}
uint8_t parse_event(packet_t*  pkt_2441 , event_t*  next_event , uint32_t ingress_port ){
  uint64_t dst_2442  = ((uint64_t)(read_bits(pkt_2441, 48))) & 281474976710655;
  uint64_t src_2443  = ((uint64_t)(read_bits(pkt_2441, 48))) & 281474976710655;
  uint16_t ety_2444  = ((uint16_t)(read_bits(pkt_2441, 16)));
  switch (ety_2444) {
    case 666: {
      uint16_t tag  = ((uint16_t)(read_bits(pkt_2441, 16)));
      switch (tag) {
        case 2: {
          uint32_t key_2438  = ((uint32_t)(read_bits(pkt_2441, 32)));
          uint16_t new_ety_2439  = ((uint16_t)(read_bits(pkt_2441, 16)));
          (*(next_event)) = mk_do_install(key_2438, new_ety_2439);
          return pkt_2441->cursor <= pkt_2441->end;
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
      (*(next_event)) = mk_ethpkt(dst_2442, src_2443, ety_2444);
      return pkt_2441->cursor <= pkt_2441->end;
      break;
    }
  }
  return 0;
}
void deparse_event(event_t*  ev_out , packet_t*  buf_out ){
  switch (ev_out->data.tag) {
    case 1: {
      uint64_t dst_2426  = ev_out->data.args.ethpkt_2430.dst_2426;
      uint64_t src_2427  = ev_out->data.args.ethpkt_2430.src_2427;
      uint16_t ety_2428  = ev_out->data.args.ethpkt_2430.ety_2428;
      write_bits(buf_out, ((uint64_t)(ety_2428)), 16);
      write_bits(buf_out, ((uint64_t)(src_2427)), 48);
      write_bits(buf_out, ((uint64_t)(dst_2426)), 48);
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
      uint32_t key_2438  = ev_out->data.args.do_install_2440.key_2438;
      uint16_t new_ety_2439  = ev_out->data.args.do_install_2440.new_ety_2439;
      write_bits(buf_out, ((uint64_t)(new_ety_2439)), 16);
      write_bits(buf_out, ((uint64_t)(key_2438)), 32);
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
    case 2: {
      uint32_t key_2445  = ev_in->data.args.do_install_2440.key_2438;
      uint16_t new_ety_2446  = ev_in->data.args.do_install_2440.new_ety_2439;
      event_t this  = mk_do_install(key_2445, new_ety_2446);
      install_ety_tbl_2437(key_2445, 4294967295, 1, tag_set_ety_2433, new_ety_2446);
      break;
    }
    case 1: {
      uint64_t dst_2447  = ev_in->data.args.ethpkt_2430.dst_2426;
      uint64_t src_2448  = ev_in->data.args.ethpkt_2430.src_2427;
      uint16_t ety_2449  = ev_in->data.args.ethpkt_2430.ety_2428;
      event_t this  = mk_ethpkt(dst_2447, src_2448, ety_2449);
      uint16_t out_ety_2451  = lookup_ety_tbl_2437(((uint32_t)(dst_2447)), ety_2449);
      out_event_t tmp_2621  = {.ev = mk_ethpkt(src_2448, dst_2447, out_ety_2451), .port = ev_in->meta.in_port};
      out_events[n] = tmp_2621;
      n = n + 1;
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

