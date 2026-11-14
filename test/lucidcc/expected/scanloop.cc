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
typedef struct {
  uint16_t len;
  uint8_t is_packet;
  uint8_t has_payload;
  uint64_t timestamp;
  uint32_t in_port;
} event_meta;
uint16_t tick_tag  = 1;
uint16_t pkt_in_tag  = 2;
uint16_t pkt_out_tag  = 3;
typedef struct {
  uint16_t tag;
  union {
    struct {
      
    } tick_710;
    struct {
      uint32_t x_711;
    } pkt_in_712;
    struct {
      uint32_t x_713;
    } pkt_out_714;
  } args;
} event_variant_t;
event_variant_t tick_710(){
  event_variant_t ev  = {0};
  
  ev.tag = tick_tag;
  return ev;
}
event_variant_t pkt_in_712(uint32_t x_711 ){
  event_variant_t ev  = {0};
  ev.args.pkt_in_712.x_711 = x_711;
  ev.tag = pkt_in_tag;
  return ev;
}
event_variant_t pkt_out_714(uint32_t x_713 ){
  event_variant_t ev  = {0};
  ev.args.pkt_out_714.x_713 = x_713;
  ev.tag = pkt_out_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_tick(){
  event_t tmp_756  = {.meta = {.len = 0, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = tick_710()};
  return tmp_756;
}
event_t mk_pkt_in(uint32_t x_711 ){
  event_t tmp_757  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = pkt_in_712(x_711)};
  return tmp_757;
}
event_t mk_pkt_out(uint32_t x_713 ){
  event_t tmp_758  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = pkt_out_714(x_713)};
  return tmp_758;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
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
          (*(next_event)) = mk_tick();
          return packet->cursor <= packet->end;
          break;
        }
        case 2: {
          uint32_t x_711  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_pkt_in(x_711);
          return packet->cursor <= packet->end;
          break;
        }
        case 3: {
          uint32_t x_713  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_pkt_out(x_713);
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
      uint32_t x_711  = ev_out->data.args.pkt_in_712.x_711;
      write_bits(buf_out, ((uint64_t)(x_711)), 32);
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
      uint32_t x_713  = ev_out->data.args.pkt_out_714.x_713;
      write_bits(buf_out, ((uint64_t)(x_713)), 32);
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
    case 3: {
      uint32_t x_715  = ev_in->data.args.pkt_out_714.x_713;
      event_t this  = mk_pkt_out(x_715);
      break;
    }
    case 1: {
      
      event_t this  = mk_tick();
      out_event_t tmp_759  = {.ev = mk_tick(), .port = 4294967295};
      out_events[n] = tmp_759;
      n = n + 1;
      break;
    }
    case 2: {
      uint32_t x_716  = ev_in->data.args.pkt_in_712.x_711;
      event_t this  = mk_pkt_in(x_716);
      out_event_t tmp_760  = {.ev = mk_pkt_out(x_716), .port = 0};
      out_events[n] = tmp_760;
      n = n + 1;
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

