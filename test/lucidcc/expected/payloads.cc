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
      uint16_t x_1625;
    } background_1626;
    struct {
      uint64_t e_0_1627;
      uint64_t e_1_1628;
      uint16_t e_2_1629;
    } eth_1630;
    struct {
      uint64_t e_0_1631;
      uint64_t e_1_1632;
      uint16_t e_2_1633;
      uint32_t ip_0_1634;
      uint32_t ip_1_1635;
      uint32_t ip_2_1636;
      uint32_t ip_3_1637;
    } eth_ip_1639;
  } args;
} event_variant_t;
event_variant_t background_1626(uint16_t x_1625 ){
  event_variant_t ev  = {0};
  ev.args.background_1626.x_1625 = x_1625;
  ev.tag = background_tag;
  return ev;
}
event_variant_t eth_1630(uint64_t e_0_1627 , uint64_t e_1_1628 , uint16_t e_2_1629 ){
  event_variant_t ev  = {0};
  ev.args.eth_1630.e_0_1627 = e_0_1627;
  ev.args.eth_1630.e_1_1628 = e_1_1628;
  ev.args.eth_1630.e_2_1629 = e_2_1629;
  ev.tag = eth_tag;
  return ev;
}
event_variant_t eth_ip_1639(uint64_t e_0_1631 , uint64_t e_1_1632 , uint16_t e_2_1633 , uint32_t ip_0_1634 , uint32_t ip_1_1635 , uint32_t ip_2_1636 , uint32_t ip_3_1637 ){
  event_variant_t ev  = {0};
  ev.args.eth_ip_1639.e_0_1631 = e_0_1631;
  ev.args.eth_ip_1639.e_1_1632 = e_1_1632;
  ev.args.eth_ip_1639.e_2_1633 = e_2_1633;
  ev.args.eth_ip_1639.ip_0_1634 = ip_0_1634;
  ev.args.eth_ip_1639.ip_1_1635 = ip_1_1635;
  ev.args.eth_ip_1639.ip_2_1636 = ip_2_1636;
  ev.args.eth_ip_1639.ip_3_1637 = ip_3_1637;
  ev.tag = eth_ip_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_background(uint16_t x_1625 ){
  event_t tmp_1801  = {.meta = {.len = 2, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = background_1626(x_1625)};
  return tmp_1801;
}
event_t mk_eth(uint64_t e_0_1627 , uint64_t e_1_1628 , uint16_t e_2_1629 ){
  event_t tmp_1802  = {.meta = {.len = 14, .is_packet = 1, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = eth_1630(e_0_1627, e_1_1628, e_2_1629)};
  return tmp_1802;
}
event_t mk_eth_ip(uint64_t e_0_1631 , uint64_t e_1_1632 , uint16_t e_2_1633 , uint32_t ip_0_1634 , uint32_t ip_1_1635 , uint32_t ip_2_1636 , uint32_t ip_3_1637 ){
  event_t tmp_1803  = {.meta = {.len = 30, .is_packet = 1, .has_payload = 1, .timestamp = 0, .in_port = 0}, .data = eth_ip_1639(e_0_1631, e_1_1632, e_2_1633, ip_0_1634, ip_1_1635, ip_2_1636, ip_3_1637)};
  return tmp_1803;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
typedef tuple_1 eth_1623;
typedef tuple_2 ip_1624;
uint8_t parse_event(packet_t*  pkt_1650 , event_t*  next_event , uint32_t ingress_port ){
  uint64_t e_0_1651  = ((uint64_t)(read_bits(pkt_1650, 48))) & 281474976710655;
  uint64_t e_1_1652  = ((uint64_t)(read_bits(pkt_1650, 48))) & 281474976710655;
  uint16_t e_2_1653  = ((uint16_t)(read_bits(pkt_1650, 16)));
  switch (e_2_1653) {
    case 666: {
      uint16_t tag  = ((uint16_t)(read_bits(pkt_1650, 16)));
      switch (tag) {
        case 1: {
          uint16_t x_1625  = ((uint16_t)(read_bits(pkt_1650, 16)));
          (*(next_event)) = mk_background(x_1625);
          return pkt_1650->cursor <= pkt_1650->end;
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
      uint32_t ip_0_1644  = ((uint32_t)(read_bits(pkt_1650, 32)));
      uint32_t ip_1_1645  = ((uint32_t)(read_bits(pkt_1650, 32)));
      uint32_t ip_2_1646  = ((uint32_t)(read_bits(pkt_1650, 32)));
      uint32_t ip_3_1647  = ((uint32_t)(read_bits(pkt_1650, 32)));
      
      (*(next_event)) = mk_eth_ip(e_0_1651, e_1_1652, e_2_1653, ip_0_1644, ip_1_1645, ip_2_1646, ip_3_1647);
      return pkt_1650->cursor <= pkt_1650->end;
      break;
    }
    case 0: {
      return 0;
      break;
    }
    default: {
      e_2_1653 = 39321;
      (*(next_event)) = mk_eth(e_0_1651, e_1_1652, e_2_1653);
      return pkt_1650->cursor <= pkt_1650->end;
      break;
    }
  }
  return 0;
}
void deparse_event(event_t*  ev_out , packet_t*  buf_out ){
  switch (ev_out->data.tag) {
    case 1: {
      uint16_t x_1625  = ev_out->data.args.background_1626.x_1625;
      write_bits(buf_out, ((uint64_t)(x_1625)), 16);
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
      uint64_t e_0_1627  = ev_out->data.args.eth_1630.e_0_1627;
      uint64_t e_1_1628  = ev_out->data.args.eth_1630.e_1_1628;
      uint16_t e_2_1629  = ev_out->data.args.eth_1630.e_2_1629;
      write_bits(buf_out, ((uint64_t)(e_2_1629)), 16);
      write_bits(buf_out, ((uint64_t)(e_1_1628)), 48);
      write_bits(buf_out, ((uint64_t)(e_0_1627)), 48);
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
      uint64_t e_0_1631  = ev_out->data.args.eth_ip_1639.e_0_1631;
      uint64_t e_1_1632  = ev_out->data.args.eth_ip_1639.e_1_1632;
      uint16_t e_2_1633  = ev_out->data.args.eth_ip_1639.e_2_1633;
      uint32_t ip_0_1634  = ev_out->data.args.eth_ip_1639.ip_0_1634;
      uint32_t ip_1_1635  = ev_out->data.args.eth_ip_1639.ip_1_1635;
      uint32_t ip_2_1636  = ev_out->data.args.eth_ip_1639.ip_2_1636;
      uint32_t ip_3_1637  = ev_out->data.args.eth_ip_1639.ip_3_1637;
      write_bits(buf_out, ((uint64_t)(ip_3_1637)), 32);
      write_bits(buf_out, ((uint64_t)(ip_2_1636)), 32);
      write_bits(buf_out, ((uint64_t)(ip_1_1635)), 32);
      write_bits(buf_out, ((uint64_t)(ip_0_1634)), 32);
      write_bits(buf_out, ((uint64_t)(e_2_1633)), 16);
      write_bits(buf_out, ((uint64_t)(e_1_1632)), 48);
      write_bits(buf_out, ((uint64_t)(e_0_1631)), 48);
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
      uint16_t x_1654  = ev_in->data.args.background_1626.x_1625;
      event_t this  = mk_background(x_1654);
      printf("%u\n", x_1654);
      break;
    }
    case 2: {
      uint64_t e_0_1655  = ev_in->data.args.eth_1630.e_0_1627;
      uint64_t e_1_1656  = ev_in->data.args.eth_1630.e_1_1628;
      uint16_t e_2_1657  = ev_in->data.args.eth_1630.e_2_1629;
      event_t this  = mk_eth(e_0_1655, e_1_1656, e_2_1657);
      out_event_t tmp_1804  = {.ev = mk_background(e_2_1657), .port = 4294967295};
      out_events[n] = tmp_1804;
      n = n + 1;
      break;
    }
    case 3: {
      uint64_t e_0_1658  = ev_in->data.args.eth_ip_1639.e_0_1631;
      uint64_t e_1_1659  = ev_in->data.args.eth_ip_1639.e_1_1632;
      uint16_t e_2_1660  = ev_in->data.args.eth_ip_1639.e_2_1633;
      uint32_t ip_0_1661  = ev_in->data.args.eth_ip_1639.ip_0_1634;
      uint32_t ip_1_1662  = ev_in->data.args.eth_ip_1639.ip_1_1635;
      uint32_t ip_2_1663  = ev_in->data.args.eth_ip_1639.ip_2_1636;
      uint32_t ip_3_1664  = ev_in->data.args.eth_ip_1639.ip_3_1637;
      event_t this  = mk_eth_ip(e_0_1658, e_1_1659, e_2_1660, ip_0_1661, ip_1_1662, ip_2_1663, ip_3_1664);
      out_event_t tmp_1805  = {.ev = mk_background(e_2_1660), .port = 4294967295};
      out_events[n] = tmp_1805;
      n = n + 1;
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

