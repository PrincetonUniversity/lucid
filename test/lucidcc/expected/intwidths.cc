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
uint32_t hash_32(uint32_t seed , uint8_t*  str , uint32_t len_bits ){
  // len_bits is the value's bit width. Sum the whole bytes, then add the last
  // partial byte masked to its valid low bits (the value is stored little-endian,
  // so the leftover bits live in the low end of the final byte). Placeholder hash
  // (a byte sum) -- collisions are not a concern yet.
  int hashValue = seed;
  uint32_t full_bytes = len_bits / 8;
  for (uint32_t i = 0; i < full_bytes; i++) {
      hashValue += str[i];
  }
  uint32_t rem = len_bits % 8;
  if (rem != 0) {
      hashValue += str[full_bytes] & ((1 << rem) - 1);
  }
  return hashValue;
}
typedef struct {
  uint16_t len;
  uint8_t is_packet;
  uint8_t has_payload;
  uint64_t timestamp;
  uint32_t in_port;
} event_meta;
uint16_t pkt_in_tag  = 1;
uint16_t bg_cmd_tag  = 2;
typedef struct {
  uint16_t tag;
  union {
    struct {
      uint8_t src_ip_693;
      uint8_t src_port_694;
    } pkt_in_695;
    struct {
      uint8_t res_696;
      uint8_t flag_697;
    } bg_cmd_698;
  } args;
} event_variant_t;
event_variant_t pkt_in_695(uint8_t src_ip_693 , uint8_t src_port_694 ){
  event_variant_t ev  = {0};
  ev.args.pkt_in_695.src_ip_693 = src_ip_693;
  ev.args.pkt_in_695.src_port_694 = src_port_694;
  ev.tag = pkt_in_tag;
  return ev;
}
event_variant_t bg_cmd_698(uint8_t res_696 , uint8_t flag_697 ){
  event_variant_t ev  = {0};
  ev.args.bg_cmd_698.res_696 = res_696;
  ev.args.bg_cmd_698.flag_697 = flag_697;
  ev.tag = bg_cmd_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_pkt_in(uint8_t src_ip_693 , uint8_t src_port_694 ){
  event_t tmp_741  = {.meta = {.len = 1, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = pkt_in_695(src_ip_693, src_port_694)};
  return tmp_741;
}
event_t mk_bg_cmd(uint8_t res_696 , uint8_t flag_697 ){
  event_t tmp_742  = {.meta = {.len = 1, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = bg_cmd_698(res_696, flag_697)};
  return tmp_742;
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
          uint8_t src_ip_693  = ((uint8_t)(read_bits(packet, 7))) & 127;
          uint8_t src_port_694  = ((uint8_t)(read_bits(packet, 1))) & 1;
          (*(next_event)) = mk_pkt_in(src_ip_693, src_port_694);
          return packet->cursor <= packet->end;
          break;
        }
        case 2: {
          uint8_t res_696  = ((uint8_t)(read_bits(packet, 7))) & 127;
          uint8_t flag_697  = ((uint8_t)(read_bits(packet, 1))) & 1;
          (*(next_event)) = mk_bg_cmd(res_696, flag_697);
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
      uint8_t src_ip_693  = ev_out->data.args.pkt_in_695.src_ip_693;
      uint8_t src_port_694  = ev_out->data.args.pkt_in_695.src_port_694;
      write_bits(buf_out, ((uint64_t)(src_port_694)), 1);
      write_bits(buf_out, ((uint64_t)(src_ip_693)), 7);
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
      uint8_t res_696  = ev_out->data.args.bg_cmd_698.res_696;
      uint8_t flag_697  = ev_out->data.args.bg_cmd_698.flag_697;
      write_bits(buf_out, ((uint64_t)(flag_697)), 1);
      write_bits(buf_out, ((uint64_t)(res_696)), 7);
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
      uint8_t res_699  = ev_in->data.args.bg_cmd_698.res_696;
      uint8_t flag_700  = ev_in->data.args.bg_cmd_698.flag_697;
      event_t this  = mk_bg_cmd(res_699, flag_700);
      break;
    }
    case 1: {
      uint8_t src_ip_701  = ev_in->data.args.pkt_in_695.src_ip_693;
      uint8_t src_port_702  = ev_in->data.args.pkt_in_695.src_port_694;
      event_t this  = mk_pkt_in(src_ip_701, src_port_702);
      out_event_t tmp_743  = {.ev = mk_bg_cmd((src_ip_701 + 1) & 127, src_port_702), .port = 4294967295};
      out_events[n] = tmp_743;
      n = n + 1;
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

