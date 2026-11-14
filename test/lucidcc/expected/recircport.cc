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
uint16_t pkt_tag  = 1;
uint16_t bg_tag  = 2;
uint16_t report_tag  = 3;
typedef struct {
  uint16_t tag;
  union {
    struct {
      uint32_t x_971;
    } pkt_972;
    struct {
      uint32_t x_973;
    } bg_974;
    struct {
      uint32_t port_in_975;
      uint32_t x_976;
    } report_977;
  } args;
} event_variant_t;
event_variant_t pkt_972(uint32_t x_971 ){
  event_variant_t ev  = {0};
  ev.args.pkt_972.x_971 = x_971;
  ev.tag = pkt_tag;
  return ev;
}
event_variant_t bg_974(uint32_t x_973 ){
  event_variant_t ev  = {0};
  ev.args.bg_974.x_973 = x_973;
  ev.tag = bg_tag;
  return ev;
}
event_variant_t report_977(uint32_t port_in_975 , uint32_t x_976 ){
  event_variant_t ev  = {0};
  ev.args.report_977.port_in_975 = port_in_975;
  ev.args.report_977.x_976 = x_976;
  ev.tag = report_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_pkt(uint32_t x_971 ){
  event_t tmp_1038  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = pkt_972(x_971)};
  return tmp_1038;
}
event_t mk_bg(uint32_t x_973 ){
  event_t tmp_1039  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = bg_974(x_973)};
  return tmp_1039;
}
event_t mk_report(uint32_t port_in_975 , uint32_t x_976 ){
  event_t tmp_1040  = {.meta = {.len = 8, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = report_977(port_in_975, x_976)};
  return tmp_1040;
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
          uint32_t x_971  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_pkt(x_971);
          return packet->cursor <= packet->end;
          break;
        }
        case 2: {
          uint32_t x_973  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_bg(x_973);
          return packet->cursor <= packet->end;
          break;
        }
        case 3: {
          uint32_t port_in_975  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t x_976  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_report(port_in_975, x_976);
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
      uint32_t x_971  = ev_out->data.args.pkt_972.x_971;
      write_bits(buf_out, ((uint64_t)(x_971)), 32);
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
      uint32_t x_973  = ev_out->data.args.bg_974.x_973;
      write_bits(buf_out, ((uint64_t)(x_973)), 32);
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
      uint32_t port_in_975  = ev_out->data.args.report_977.port_in_975;
      uint32_t x_976  = ev_out->data.args.report_977.x_976;
      write_bits(buf_out, ((uint64_t)(x_976)), 32);
      write_bits(buf_out, ((uint64_t)(port_in_975)), 32);
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
      uint32_t x_978  = ev_in->data.args.pkt_972.x_971;
      event_t this  = mk_pkt(x_978);
      out_event_t tmp_1041  = {.ev = mk_report(ev_in->meta.in_port, x_978), .port = 1};
      out_events[n] = tmp_1041;
      n = n + 1;
      out_event_t tmp_1042  = {.ev = mk_bg(x_978), .port = 4294967295};
      out_events[n] = tmp_1042;
      n = n + 1;
      break;
    }
    case 2: {
      uint32_t x_979  = ev_in->data.args.bg_974.x_973;
      event_t this  = mk_bg(x_979);
      out_event_t tmp_1043  = {.ev = mk_report(ev_in->meta.in_port, x_979), .port = 1};
      out_events[n] = tmp_1043;
      n = n + 1;
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

