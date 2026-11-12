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
enum {tag_set_ety_2306 = 0, tag_keep_ety_2309 = 1};
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
      uint64_t dst_2299;
      uint64_t src_2300;
      uint16_t ety_2301;
    } ethpkt_2303;
    struct {
      uint32_t key_2311;
      uint16_t new_ety_2312;
    } do_install_2313;
  } args;
} event_variant_t;
event_variant_t ethpkt_2303(uint64_t dst_2299 , uint64_t src_2300 , uint16_t ety_2301 ){
  event_variant_t ev  = {0};
  ev.args.ethpkt_2303.dst_2299 = dst_2299;
  ev.args.ethpkt_2303.src_2300 = src_2300;
  ev.args.ethpkt_2303.ety_2301 = ety_2301;
  ev.tag = ethpkt_tag;
  return ev;
}
event_variant_t do_install_2313(uint32_t key_2311 , uint16_t new_ety_2312 ){
  event_variant_t ev  = {0};
  ev.args.do_install_2313.key_2311 = key_2311;
  ev.args.do_install_2313.new_ety_2312 = new_ety_2312;
  ev.tag = do_install_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_ethpkt(uint64_t dst_2299 , uint64_t src_2300 , uint16_t ety_2301 ){
  event_t tmp_2484  = {.meta = {.len = 14, .is_packet = 1, .has_payload = 1, .timestamp = 0, .in_port = 0}, .data = ethpkt_2303(dst_2299, src_2300, ety_2301)};
  return tmp_2484;
}
event_t mk_do_install(uint32_t key_2311 , uint16_t new_ety_2312 ){
  event_t tmp_2485  = {.meta = {.len = 6, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_install_2313(key_2311, new_ety_2312)};
  return tmp_2485;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
uint16_t set_ety_2306(uint16_t new_ety_2304 , uint16_t ety_2305 ){
  return new_ety_2304;
}
uint16_t keep_ety_2309(uint16_t unused_2307 , uint16_t ety_2308 ){
  return ety_2308;
}
typedef struct {
  uint8_t valid;
  uint32_t key;
  uint32_t mask;
  uint32_t action_tag;
  uint16_t action_arg;
} cellty_ety_tbl_2310;
typedef struct {
  struct {
    uint32_t action_tag;
    uint16_t action_arg;
  } _default;
  uint32_t max_idx;
  cellty_ety_tbl_2310 entries [4096];
} ty_ety_tbl_2310;
ty_ety_tbl_2310 ety_tbl_2310  = {._default = {.action_tag = tag_keep_ety_2309, .action_arg = 0}, .max_idx = 0, .entries = {0}};
void install_ety_tbl_2310(uint32_t key , uint32_t action , uint16_t const_arg ){
  for (int _idx = 0; _idx < 4096; _idx++) {
    if (ety_tbl_2310.entries[_idx].valid == false) {
      cellty_ety_tbl_2310 tmp_2486  = {.valid = true, .key = key, .mask = 4294967295, .action_tag = action, .action_arg = const_arg};
      ety_tbl_2310.entries[_idx] = tmp_2486;
      if (_idx >= ety_tbl_2310.max_idx) {
        ety_tbl_2310.max_idx = _idx + 1;
      }
      return ;
    }
  }
  return ;
}
void install_ternary_ety_tbl_2310(uint32_t key , uint32_t mask , uint32_t action , uint16_t const_arg ){
  for (int _idx = 0; _idx < 4096; _idx++) {
    if (ety_tbl_2310.entries[_idx].valid == false) {
      cellty_ety_tbl_2310 tmp_2487  = {.valid = true, .key = key, .mask = mask, .action_tag = action, .action_arg = const_arg};
      ety_tbl_2310.entries[_idx] = tmp_2487;
      if (_idx >= ety_tbl_2310.max_idx) {
        ety_tbl_2310.max_idx = _idx + 1;
      }
      return ;
    }
  }
  return ;
}
uint16_t lookup_ety_tbl_2310(uint32_t key , uint16_t arg ){
  for (int _idx = 0; _idx < 4096; _idx++) {
    if (_idx >= ety_tbl_2310.max_idx) {
      switch (ety_tbl_2310._default.action_tag) {
        case tag_set_ety_2306: {
          return set_ety_2306(ety_tbl_2310._default.action_arg, arg);
          break;
        }
        default: {
          return keep_ety_2309(ety_tbl_2310._default.action_arg, arg);
          break;
        }
      }
    }
    if (ety_tbl_2310.entries[_idx].valid && ((key & ety_tbl_2310.entries[_idx].mask) == (ety_tbl_2310.entries[_idx].key & ety_tbl_2310.entries[_idx].mask))) {
      switch (ety_tbl_2310.entries[_idx].action_tag) {
        case tag_set_ety_2306: {
          return set_ety_2306(ety_tbl_2310.entries[_idx].action_arg, arg);
          break;
        }
        case tag_keep_ety_2309: {
          return keep_ety_2309(ety_tbl_2310.entries[_idx].action_arg, arg);
          break;
        }
      }
    }
  }
  switch (ety_tbl_2310._default.action_tag) {
    case tag_set_ety_2306: {
      return set_ety_2306(ety_tbl_2310._default.action_arg, arg);
      break;
    }
    default: {
      return keep_ety_2309(ety_tbl_2310._default.action_arg, arg);
      break;
    }
  }
}
uint8_t parse_event(packet_t*  pkt_2314 , event_t*  next_event , uint32_t ingress_port ){
  uint64_t dst_2315  = ((uint64_t)(read_bits(pkt_2314, 48))) & 281474976710655;
  uint64_t src_2316  = ((uint64_t)(read_bits(pkt_2314, 48))) & 281474976710655;
  uint16_t ety_2317  = ((uint16_t)(read_bits(pkt_2314, 16)));
  switch (ety_2317) {
    case 666: {
      uint16_t tag  = ((uint16_t)(read_bits(pkt_2314, 16)));
      switch (tag) {
        case 2: {
          uint32_t key_2311  = ((uint32_t)(read_bits(pkt_2314, 32)));
          uint16_t new_ety_2312  = ((uint16_t)(read_bits(pkt_2314, 16)));
          (*(next_event)) = mk_do_install(key_2311, new_ety_2312);
          return pkt_2314->cursor <= pkt_2314->end;
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
      (*(next_event)) = mk_ethpkt(dst_2315, src_2316, ety_2317);
      return pkt_2314->cursor <= pkt_2314->end;
      break;
    }
  }
  return 0;
}
void deparse_event(event_t*  ev_out , packet_t*  buf_out ){
  switch (ev_out->data.tag) {
    case 1: {
      uint64_t dst_2299  = ev_out->data.args.ethpkt_2303.dst_2299;
      uint64_t src_2300  = ev_out->data.args.ethpkt_2303.src_2300;
      uint16_t ety_2301  = ev_out->data.args.ethpkt_2303.ety_2301;
      write_bits(buf_out, ((uint64_t)(ety_2301)), 16);
      write_bits(buf_out, ((uint64_t)(src_2300)), 48);
      write_bits(buf_out, ((uint64_t)(dst_2299)), 48);
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
      uint32_t key_2311  = ev_out->data.args.do_install_2313.key_2311;
      uint16_t new_ety_2312  = ev_out->data.args.do_install_2313.new_ety_2312;
      write_bits(buf_out, ((uint64_t)(new_ety_2312)), 16);
      write_bits(buf_out, ((uint64_t)(key_2311)), 32);
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
      uint32_t key_2318  = ev_in->data.args.do_install_2313.key_2311;
      uint16_t new_ety_2319  = ev_in->data.args.do_install_2313.new_ety_2312;
      event_t this  = mk_do_install(key_2318, new_ety_2319);
      install_ety_tbl_2310(key_2318, tag_set_ety_2306, new_ety_2319);
      break;
    }
    case 1: {
      uint64_t dst_2320  = ev_in->data.args.ethpkt_2303.dst_2299;
      uint64_t src_2321  = ev_in->data.args.ethpkt_2303.src_2300;
      uint16_t ety_2322  = ev_in->data.args.ethpkt_2303.ety_2301;
      event_t this  = mk_ethpkt(dst_2320, src_2321, ety_2322);
      uint16_t out_ety_2324  = lookup_ety_tbl_2310(((uint32_t)(dst_2320)), ety_2322);
      out_event_t tmp_2488  = {.ev = mk_ethpkt(src_2321, dst_2320, out_ety_2324), .port = ev_in->meta.in_port};
      out_events[n] = tmp_2488;
      n = n + 1;
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

