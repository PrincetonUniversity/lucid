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
enum {tag_set_ety_2346 = 0, tag_keep_ety_2349 = 1};
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
      uint64_t dst_2339;
      uint64_t src_2340;
      uint16_t ety_2341;
    } ethpkt_2343;
    struct {
      uint32_t key_2351;
      uint16_t new_ety_2352;
    } do_install_2353;
  } args;
} event_variant_t;
event_variant_t ethpkt_2343(uint64_t dst_2339 , uint64_t src_2340 , uint16_t ety_2341 ){
  event_variant_t ev  = {0};
  ev.args.ethpkt_2343.dst_2339 = dst_2339;
  ev.args.ethpkt_2343.src_2340 = src_2340;
  ev.args.ethpkt_2343.ety_2341 = ety_2341;
  ev.tag = ethpkt_tag;
  return ev;
}
event_variant_t do_install_2353(uint32_t key_2351 , uint16_t new_ety_2352 ){
  event_variant_t ev  = {0};
  ev.args.do_install_2353.key_2351 = key_2351;
  ev.args.do_install_2353.new_ety_2352 = new_ety_2352;
  ev.tag = do_install_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_ethpkt(uint64_t dst_2339 , uint64_t src_2340 , uint16_t ety_2341 ){
  event_t tmp_2524  = {.meta = {.len = 14, .is_packet = 1, .has_payload = 1, .timestamp = 0, .in_port = 0}, .data = ethpkt_2343(dst_2339, src_2340, ety_2341)};
  return tmp_2524;
}
event_t mk_do_install(uint32_t key_2351 , uint16_t new_ety_2352 ){
  event_t tmp_2525  = {.meta = {.len = 6, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_install_2353(key_2351, new_ety_2352)};
  return tmp_2525;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
uint16_t set_ety_2346(uint16_t new_ety_2344 , uint16_t ety_2345 ){
  return new_ety_2344;
}
uint16_t keep_ety_2349(uint16_t unused_2347 , uint16_t ety_2348 ){
  return ety_2348;
}
typedef struct {
  uint8_t valid;
  uint32_t key;
  uint32_t mask;
  uint32_t action_tag;
  uint16_t action_arg;
} cellty_ety_tbl_2350;
typedef struct {
  struct {
    uint32_t action_tag;
    uint16_t action_arg;
  } _default;
  uint32_t max_idx;
  cellty_ety_tbl_2350 entries [4096];
} ty_ety_tbl_2350;
ty_ety_tbl_2350 ety_tbl_2350  = {._default = {.action_tag = tag_keep_ety_2349, .action_arg = 0}, .max_idx = 0, .entries = {0}};
void install_ety_tbl_2350(uint32_t key , uint32_t action , uint16_t const_arg ){
  for (int _idx = 0; _idx < 4096; _idx++) {
    if (ety_tbl_2350.entries[_idx].valid == false) {
      cellty_ety_tbl_2350 tmp_2526  = {.valid = true, .key = key, .mask = 4294967295, .action_tag = action, .action_arg = const_arg};
      ety_tbl_2350.entries[_idx] = tmp_2526;
      if (_idx >= ety_tbl_2350.max_idx) {
        ety_tbl_2350.max_idx = _idx + 1;
      }
      return ;
    }
  }
  return ;
}
void install_ternary_ety_tbl_2350(uint32_t key , uint32_t mask , uint32_t action , uint16_t const_arg ){
  for (int _idx = 0; _idx < 4096; _idx++) {
    if (ety_tbl_2350.entries[_idx].valid == false) {
      cellty_ety_tbl_2350 tmp_2527  = {.valid = true, .key = key, .mask = mask, .action_tag = action, .action_arg = const_arg};
      ety_tbl_2350.entries[_idx] = tmp_2527;
      if (_idx >= ety_tbl_2350.max_idx) {
        ety_tbl_2350.max_idx = _idx + 1;
      }
      return ;
    }
  }
  return ;
}
uint16_t lookup_ety_tbl_2350(uint32_t key , uint16_t arg ){
  for (int _idx = 0; _idx < 4096; _idx++) {
    if (_idx >= ety_tbl_2350.max_idx) {
      switch (ety_tbl_2350._default.action_tag) {
        case tag_set_ety_2346: {
          return set_ety_2346(ety_tbl_2350._default.action_arg, arg);
          break;
        }
        default: {
          return keep_ety_2349(ety_tbl_2350._default.action_arg, arg);
          break;
        }
      }
    }
    if (ety_tbl_2350.entries[_idx].valid && ((key & ety_tbl_2350.entries[_idx].mask) == (ety_tbl_2350.entries[_idx].key & ety_tbl_2350.entries[_idx].mask))) {
      switch (ety_tbl_2350.entries[_idx].action_tag) {
        case tag_set_ety_2346: {
          return set_ety_2346(ety_tbl_2350.entries[_idx].action_arg, arg);
          break;
        }
        case tag_keep_ety_2349: {
          return keep_ety_2349(ety_tbl_2350.entries[_idx].action_arg, arg);
          break;
        }
      }
    }
  }
  switch (ety_tbl_2350._default.action_tag) {
    case tag_set_ety_2346: {
      return set_ety_2346(ety_tbl_2350._default.action_arg, arg);
      break;
    }
    default: {
      return keep_ety_2349(ety_tbl_2350._default.action_arg, arg);
      break;
    }
  }
}
uint8_t parse_event(packet_t*  pkt_2354 , event_t*  next_event , uint32_t ingress_port ){
  uint64_t dst_2355  = ((uint64_t)(read_bits(pkt_2354, 48))) & 281474976710655;
  uint64_t src_2356  = ((uint64_t)(read_bits(pkt_2354, 48))) & 281474976710655;
  uint16_t ety_2357  = ((uint16_t)(read_bits(pkt_2354, 16)));
  switch (ety_2357) {
    case 666: {
      uint16_t tag  = ((uint16_t)(read_bits(pkt_2354, 16)));
      switch (tag) {
        case 2: {
          uint32_t key_2351  = ((uint32_t)(read_bits(pkt_2354, 32)));
          uint16_t new_ety_2352  = ((uint16_t)(read_bits(pkt_2354, 16)));
          (*(next_event)) = mk_do_install(key_2351, new_ety_2352);
          return pkt_2354->cursor <= pkt_2354->end;
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
      (*(next_event)) = mk_ethpkt(dst_2355, src_2356, ety_2357);
      return pkt_2354->cursor <= pkt_2354->end;
      break;
    }
  }
  return 0;
}
void deparse_event(event_t*  ev_out , packet_t*  buf_out ){
  switch (ev_out->data.tag) {
    case 1: {
      uint64_t dst_2339  = ev_out->data.args.ethpkt_2343.dst_2339;
      uint64_t src_2340  = ev_out->data.args.ethpkt_2343.src_2340;
      uint16_t ety_2341  = ev_out->data.args.ethpkt_2343.ety_2341;
      write_bits(buf_out, ((uint64_t)(ety_2341)), 16);
      write_bits(buf_out, ((uint64_t)(src_2340)), 48);
      write_bits(buf_out, ((uint64_t)(dst_2339)), 48);
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
      uint32_t key_2351  = ev_out->data.args.do_install_2353.key_2351;
      uint16_t new_ety_2352  = ev_out->data.args.do_install_2353.new_ety_2352;
      write_bits(buf_out, ((uint64_t)(new_ety_2352)), 16);
      write_bits(buf_out, ((uint64_t)(key_2351)), 32);
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
      uint32_t key_2358  = ev_in->data.args.do_install_2353.key_2351;
      uint16_t new_ety_2359  = ev_in->data.args.do_install_2353.new_ety_2352;
      event_t this  = mk_do_install(key_2358, new_ety_2359);
      install_ety_tbl_2350(key_2358, tag_set_ety_2346, new_ety_2359);
      break;
    }
    case 1: {
      uint64_t dst_2360  = ev_in->data.args.ethpkt_2343.dst_2339;
      uint64_t src_2361  = ev_in->data.args.ethpkt_2343.src_2340;
      uint16_t ety_2362  = ev_in->data.args.ethpkt_2343.ety_2341;
      event_t this  = mk_ethpkt(dst_2360, src_2361, ety_2362);
      uint16_t out_ety_2364  = lookup_ety_tbl_2350(((uint32_t)(dst_2360)), ety_2362);
      out_event_t tmp_2528  = {.ev = mk_ethpkt(src_2361, dst_2360, out_ety_2364), .port = ev_in->meta.in_port};
      out_events[n] = tmp_2528;
      n = n + 1;
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

