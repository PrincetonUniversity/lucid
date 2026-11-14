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
uint16_t pkt_in_tag  = 1;
uint16_t bg_cmd_tag  = 2;
uint16_t pkt_out_tag  = 3;
typedef struct {
  uint16_t tag;
  union {
    struct {
      uint32_t src_ip_1224;
      uint32_t src_port_1225;
    } pkt_in_1226;
    struct {
      uint32_t src_port_1227;
    } bg_cmd_1228;
    struct {
      uint32_t src_ip_1229;
      uint32_t src_port_1230;
    } pkt_out_1231;
  } args;
} event_variant_t;
event_variant_t pkt_in_1226(uint32_t src_ip_1224 , uint32_t src_port_1225 ){
  event_variant_t ev  = {0};
  ev.args.pkt_in_1226.src_ip_1224 = src_ip_1224;
  ev.args.pkt_in_1226.src_port_1225 = src_port_1225;
  ev.tag = pkt_in_tag;
  return ev;
}
event_variant_t bg_cmd_1228(uint32_t src_port_1227 ){
  event_variant_t ev  = {0};
  ev.args.bg_cmd_1228.src_port_1227 = src_port_1227;
  ev.tag = bg_cmd_tag;
  return ev;
}
event_variant_t pkt_out_1231(uint32_t src_ip_1229 , uint32_t src_port_1230 ){
  event_variant_t ev  = {0};
  ev.args.pkt_out_1231.src_ip_1229 = src_ip_1229;
  ev.args.pkt_out_1231.src_port_1230 = src_port_1230;
  ev.tag = pkt_out_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_pkt_in(uint32_t src_ip_1224 , uint32_t src_port_1225 ){
  event_t tmp_1310  = {.meta = {.len = 8, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = pkt_in_1226(src_ip_1224, src_port_1225)};
  return tmp_1310;
}
event_t mk_bg_cmd(uint32_t src_port_1227 ){
  event_t tmp_1311  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = bg_cmd_1228(src_port_1227)};
  return tmp_1311;
}
event_t mk_pkt_out(uint32_t src_ip_1229 , uint32_t src_port_1230 ){
  event_t tmp_1312  = {.meta = {.len = 8, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = pkt_out_1231(src_ip_1229, src_port_1230)};
  return tmp_1312;
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
          uint32_t src_ip_1224  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t src_port_1225  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_pkt_in(src_ip_1224, src_port_1225);
          return packet->cursor <= packet->end;
          break;
        }
        case 2: {
          uint32_t src_port_1227  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_bg_cmd(src_port_1227);
          return packet->cursor <= packet->end;
          break;
        }
        case 3: {
          uint32_t src_ip_1229  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t src_port_1230  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_pkt_out(src_ip_1229, src_port_1230);
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
      uint32_t src_ip_1224  = ev_out->data.args.pkt_in_1226.src_ip_1224;
      uint32_t src_port_1225  = ev_out->data.args.pkt_in_1226.src_port_1225;
      write_bits(buf_out, ((uint64_t)(src_port_1225)), 32);
      write_bits(buf_out, ((uint64_t)(src_ip_1224)), 32);
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
      uint32_t src_port_1227  = ev_out->data.args.bg_cmd_1228.src_port_1227;
      write_bits(buf_out, ((uint64_t)(src_port_1227)), 32);
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
      uint32_t src_ip_1229  = ev_out->data.args.pkt_out_1231.src_ip_1229;
      uint32_t src_port_1230  = ev_out->data.args.pkt_out_1231.src_port_1230;
      write_bits(buf_out, ((uint64_t)(src_port_1230)), 32);
      write_bits(buf_out, ((uint64_t)(src_ip_1229)), 32);
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
    case 2: {
      uint32_t src_port_1232  = ev_in->data.args.bg_cmd_1228.src_port_1227;
      event_t this  = mk_bg_cmd(src_port_1232);
      break;
    }
    case 3: {
      uint32_t src_ip_1233  = ev_in->data.args.pkt_out_1231.src_ip_1229;
      uint32_t src_port_1234  = ev_in->data.args.pkt_out_1231.src_port_1230;
      event_t this  = mk_pkt_out(src_ip_1233, src_port_1234);
      break;
    }
    case 1: {
      uint32_t src_ip_1235  = ev_in->data.args.pkt_in_1226.src_ip_1224;
      uint32_t src_port_1236  = ev_in->data.args.pkt_in_1226.src_port_1225;
      event_t this  = mk_pkt_in(src_ip_1235, src_port_1236);
      printf("Received packet from (ip: %u, port: %u)\n", src_ip_1235, src_port_1236);
      event_t MY_EVENT_1237  = mk_pkt_out(src_ip_1235, src_port_1236);
      out_event_t tmp_1313  = {.ev = mk_bg_cmd(src_port_1236), .port = 4294967295};
      out_events[n] = tmp_1313;
      n = n + 1;
      switch (MY_EVENT_1237.data.tag) {
        case 3: {
          uint32_t ip_1238  = MY_EVENT_1237.data.args.pkt_out_1231.src_ip_1229;
          uint32_t port_1239  = MY_EVENT_1237.data.args.pkt_out_1231.src_port_1230;
          printf("Sending packet to (ip: %u, port: %u)\n", ip_1238, port_1239);
          out_event_t tmp_1314  = {.ev = mk_pkt_out(src_ip_1235, src_port_1236), .port = 0};
          out_events[n] = tmp_1314;
          n = n + 1;
          break;
        }
        default: {
          
          break;
        }
      }
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

