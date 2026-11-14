typedef struct {
  uint64_t _0;
  uint64_t _1;
  uint16_t _2;
} tuple_1;
typedef struct {
  uint32_t _0;
  uint32_t _1;
  uint32_t _2;
  uint32_t _3;
} tuple_2;
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
uint16_t background_tag  = 1;
uint16_t eth_tag  = 2;
uint16_t eth_ip_tag  = 3;
typedef struct {
  uint16_t tag;
  union {
    struct {
      uint16_t x_1665;
    } background_1666;
    struct {
      uint64_t e_0_1667;
      uint64_t e_1_1668;
      uint16_t e_2_1669;
    } eth_1670;
    struct {
      uint64_t e_0_1671;
      uint64_t e_1_1672;
      uint16_t e_2_1673;
      uint32_t ip_0_1674;
      uint32_t ip_1_1675;
      uint32_t ip_2_1676;
      uint32_t ip_3_1677;
    } eth_ip_1679;
  } args;
} event_variant_t;
event_variant_t background_1666(uint16_t x_1665 ){
  event_variant_t ev  = {0};
  ev.args.background_1666.x_1665 = x_1665;
  ev.tag = background_tag;
  return ev;
}
event_variant_t eth_1670(uint64_t e_0_1667 , uint64_t e_1_1668 , uint16_t e_2_1669 ){
  event_variant_t ev  = {0};
  ev.args.eth_1670.e_0_1667 = e_0_1667;
  ev.args.eth_1670.e_1_1668 = e_1_1668;
  ev.args.eth_1670.e_2_1669 = e_2_1669;
  ev.tag = eth_tag;
  return ev;
}
event_variant_t eth_ip_1679(uint64_t e_0_1671 , uint64_t e_1_1672 , uint16_t e_2_1673 , uint32_t ip_0_1674 , uint32_t ip_1_1675 , uint32_t ip_2_1676 , uint32_t ip_3_1677 ){
  event_variant_t ev  = {0};
  ev.args.eth_ip_1679.e_0_1671 = e_0_1671;
  ev.args.eth_ip_1679.e_1_1672 = e_1_1672;
  ev.args.eth_ip_1679.e_2_1673 = e_2_1673;
  ev.args.eth_ip_1679.ip_0_1674 = ip_0_1674;
  ev.args.eth_ip_1679.ip_1_1675 = ip_1_1675;
  ev.args.eth_ip_1679.ip_2_1676 = ip_2_1676;
  ev.args.eth_ip_1679.ip_3_1677 = ip_3_1677;
  ev.tag = eth_ip_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_background(uint16_t x_1665 ){
  event_t tmp_1841  = {.meta = {.len = 2, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = background_1666(x_1665)};
  return tmp_1841;
}
event_t mk_eth(uint64_t e_0_1667 , uint64_t e_1_1668 , uint16_t e_2_1669 ){
  event_t tmp_1842  = {.meta = {.len = 14, .is_packet = 1, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = eth_1670(e_0_1667, e_1_1668, e_2_1669)};
  return tmp_1842;
}
event_t mk_eth_ip(uint64_t e_0_1671 , uint64_t e_1_1672 , uint16_t e_2_1673 , uint32_t ip_0_1674 , uint32_t ip_1_1675 , uint32_t ip_2_1676 , uint32_t ip_3_1677 ){
  event_t tmp_1843  = {.meta = {.len = 30, .is_packet = 1, .has_payload = 1, .timestamp = 0, .in_port = 0}, .data = eth_ip_1679(e_0_1671, e_1_1672, e_2_1673, ip_0_1674, ip_1_1675, ip_2_1676, ip_3_1677)};
  return tmp_1843;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
typedef tuple_1 eth_1663;
typedef tuple_2 ip_1664;
uint8_t parse_event(packet_t*  pkt_1690 , event_t*  next_event , uint32_t ingress_port ){
  uint64_t e_0_1691  = ((uint64_t)(read_bits(pkt_1690, 48))) & 281474976710655;
  uint64_t e_1_1692  = ((uint64_t)(read_bits(pkt_1690, 48))) & 281474976710655;
  uint16_t e_2_1693  = ((uint16_t)(read_bits(pkt_1690, 16)));
  switch (e_2_1693) {
    case 666: {
      uint16_t tag  = ((uint16_t)(read_bits(pkt_1690, 16)));
      switch (tag) {
        case 1: {
          uint16_t x_1665  = ((uint16_t)(read_bits(pkt_1690, 16)));
          (*(next_event)) = mk_background(x_1665);
          return pkt_1690->cursor <= pkt_1690->end;
          break;
        }
        default: {
          return 0;
          break;
        }
      }
      break;
    }
    case 2048: {
      uint32_t ip_0_1684  = ((uint32_t)(read_bits(pkt_1690, 32)));
      uint32_t ip_1_1685  = ((uint32_t)(read_bits(pkt_1690, 32)));
      uint32_t ip_2_1686  = ((uint32_t)(read_bits(pkt_1690, 32)));
      uint32_t ip_3_1687  = ((uint32_t)(read_bits(pkt_1690, 32)));
      
      (*(next_event)) = mk_eth_ip(e_0_1691, e_1_1692, e_2_1693, ip_0_1684, ip_1_1685, ip_2_1686, ip_3_1687);
      return pkt_1690->cursor <= pkt_1690->end;
      break;
    }
    case 0: {
      return 0;
      break;
    }
    default: {
      e_2_1693 = 39321;
      (*(next_event)) = mk_eth(e_0_1691, e_1_1692, e_2_1693);
      return pkt_1690->cursor <= pkt_1690->end;
      break;
    }
  }
  return 0;
}
void deparse_event(event_t*  ev_out , packet_t*  buf_out ){
  switch (ev_out->data.tag) {
    case 1: {
      uint16_t x_1665  = ev_out->data.args.background_1666.x_1665;
      write_bits(buf_out, ((uint64_t)(x_1665)), 16);
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
      uint64_t e_0_1667  = ev_out->data.args.eth_1670.e_0_1667;
      uint64_t e_1_1668  = ev_out->data.args.eth_1670.e_1_1668;
      uint16_t e_2_1669  = ev_out->data.args.eth_1670.e_2_1669;
      write_bits(buf_out, ((uint64_t)(e_2_1669)), 16);
      write_bits(buf_out, ((uint64_t)(e_1_1668)), 48);
      write_bits(buf_out, ((uint64_t)(e_0_1667)), 48);
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
      uint64_t e_0_1671  = ev_out->data.args.eth_ip_1679.e_0_1671;
      uint64_t e_1_1672  = ev_out->data.args.eth_ip_1679.e_1_1672;
      uint16_t e_2_1673  = ev_out->data.args.eth_ip_1679.e_2_1673;
      uint32_t ip_0_1674  = ev_out->data.args.eth_ip_1679.ip_0_1674;
      uint32_t ip_1_1675  = ev_out->data.args.eth_ip_1679.ip_1_1675;
      uint32_t ip_2_1676  = ev_out->data.args.eth_ip_1679.ip_2_1676;
      uint32_t ip_3_1677  = ev_out->data.args.eth_ip_1679.ip_3_1677;
      write_bits(buf_out, ((uint64_t)(ip_3_1677)), 32);
      write_bits(buf_out, ((uint64_t)(ip_2_1676)), 32);
      write_bits(buf_out, ((uint64_t)(ip_1_1675)), 32);
      write_bits(buf_out, ((uint64_t)(ip_0_1674)), 32);
      write_bits(buf_out, ((uint64_t)(e_2_1673)), 16);
      write_bits(buf_out, ((uint64_t)(e_1_1672)), 48);
      write_bits(buf_out, ((uint64_t)(e_0_1671)), 48);
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
      uint16_t x_1694  = ev_in->data.args.background_1666.x_1665;
      event_t this  = mk_background(x_1694);
      printf("%u\n", x_1694);
      break;
    }
    case 2: {
      uint64_t e_0_1695  = ev_in->data.args.eth_1670.e_0_1667;
      uint64_t e_1_1696  = ev_in->data.args.eth_1670.e_1_1668;
      uint16_t e_2_1697  = ev_in->data.args.eth_1670.e_2_1669;
      event_t this  = mk_eth(e_0_1695, e_1_1696, e_2_1697);
      out_event_t tmp_1844  = {.ev = mk_background(e_2_1697), .port = 4294967295};
      out_events[n] = tmp_1844;
      n = n + 1;
      break;
    }
    case 3: {
      uint64_t e_0_1698  = ev_in->data.args.eth_ip_1679.e_0_1671;
      uint64_t e_1_1699  = ev_in->data.args.eth_ip_1679.e_1_1672;
      uint16_t e_2_1700  = ev_in->data.args.eth_ip_1679.e_2_1673;
      uint32_t ip_0_1701  = ev_in->data.args.eth_ip_1679.ip_0_1674;
      uint32_t ip_1_1702  = ev_in->data.args.eth_ip_1679.ip_1_1675;
      uint32_t ip_2_1703  = ev_in->data.args.eth_ip_1679.ip_2_1676;
      uint32_t ip_3_1704  = ev_in->data.args.eth_ip_1679.ip_3_1677;
      event_t this  = mk_eth_ip(e_0_1698, e_1_1699, e_2_1700, ip_0_1701, ip_1_1702, ip_2_1703, ip_3_1704);
      out_event_t tmp_1845  = {.ev = mk_background(e_2_1700), .port = 4294967295};
      out_events[n] = tmp_1845;
      n = n + 1;
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

