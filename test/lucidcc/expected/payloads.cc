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
uint16_t background_tag  = 1;
uint16_t eth_tag  = 2;
uint16_t eth_ip_tag  = 3;
typedef struct {
  uint16_t tag;
  union {
    struct {
      uint16_t x_1654;
    } background_1655;
    struct {
      uint64_t e_0_1656;
      uint64_t e_1_1657;
      uint16_t e_2_1658;
    } eth_1659;
    struct {
      uint64_t e_0_1660;
      uint64_t e_1_1661;
      uint16_t e_2_1662;
      uint32_t ip_0_1663;
      uint32_t ip_1_1664;
      uint32_t ip_2_1665;
      uint32_t ip_3_1666;
    } eth_ip_1668;
  } args;
} event_variant_t;
event_variant_t background_1655(uint16_t x_1654 ){
  event_variant_t ev  = {0};
  ev.args.background_1655.x_1654 = x_1654;
  ev.tag = background_tag;
  return ev;
}
event_variant_t eth_1659(uint64_t e_0_1656 , uint64_t e_1_1657 , uint16_t e_2_1658 ){
  event_variant_t ev  = {0};
  ev.args.eth_1659.e_0_1656 = e_0_1656;
  ev.args.eth_1659.e_1_1657 = e_1_1657;
  ev.args.eth_1659.e_2_1658 = e_2_1658;
  ev.tag = eth_tag;
  return ev;
}
event_variant_t eth_ip_1668(uint64_t e_0_1660 , uint64_t e_1_1661 , uint16_t e_2_1662 , uint32_t ip_0_1663 , uint32_t ip_1_1664 , uint32_t ip_2_1665 , uint32_t ip_3_1666 ){
  event_variant_t ev  = {0};
  ev.args.eth_ip_1668.e_0_1660 = e_0_1660;
  ev.args.eth_ip_1668.e_1_1661 = e_1_1661;
  ev.args.eth_ip_1668.e_2_1662 = e_2_1662;
  ev.args.eth_ip_1668.ip_0_1663 = ip_0_1663;
  ev.args.eth_ip_1668.ip_1_1664 = ip_1_1664;
  ev.args.eth_ip_1668.ip_2_1665 = ip_2_1665;
  ev.args.eth_ip_1668.ip_3_1666 = ip_3_1666;
  ev.tag = eth_ip_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_background(uint16_t x_1654 ){
  event_t tmp_1830  = {.meta = {.len = 2, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = background_1655(x_1654)};
  return tmp_1830;
}
event_t mk_eth(uint64_t e_0_1656 , uint64_t e_1_1657 , uint16_t e_2_1658 ){
  event_t tmp_1831  = {.meta = {.len = 14, .is_packet = 1, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = eth_1659(e_0_1656, e_1_1657, e_2_1658)};
  return tmp_1831;
}
event_t mk_eth_ip(uint64_t e_0_1660 , uint64_t e_1_1661 , uint16_t e_2_1662 , uint32_t ip_0_1663 , uint32_t ip_1_1664 , uint32_t ip_2_1665 , uint32_t ip_3_1666 ){
  event_t tmp_1832  = {.meta = {.len = 30, .is_packet = 1, .has_payload = 1, .timestamp = 0, .in_port = 0}, .data = eth_ip_1668(e_0_1660, e_1_1661, e_2_1662, ip_0_1663, ip_1_1664, ip_2_1665, ip_3_1666)};
  return tmp_1832;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
typedef tuple_1 eth_1652;
typedef tuple_2 ip_1653;
uint8_t parse_event(packet_t*  pkt_1679 , event_t*  next_event , uint32_t ingress_port ){
  uint64_t e_0_1680  = ((uint64_t)(read_bits(pkt_1679, 48))) & 281474976710655;
  uint64_t e_1_1681  = ((uint64_t)(read_bits(pkt_1679, 48))) & 281474976710655;
  uint16_t e_2_1682  = ((uint16_t)(read_bits(pkt_1679, 16)));
  switch (e_2_1682) {
    case 666: {
      uint16_t tag  = ((uint16_t)(read_bits(pkt_1679, 16)));
      switch (tag) {
        case 1: {
          uint16_t x_1654  = ((uint16_t)(read_bits(pkt_1679, 16)));
          (*(next_event)) = mk_background(x_1654);
          return pkt_1679->cursor <= pkt_1679->end;
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
      uint32_t ip_0_1673  = ((uint32_t)(read_bits(pkt_1679, 32)));
      uint32_t ip_1_1674  = ((uint32_t)(read_bits(pkt_1679, 32)));
      uint32_t ip_2_1675  = ((uint32_t)(read_bits(pkt_1679, 32)));
      uint32_t ip_3_1676  = ((uint32_t)(read_bits(pkt_1679, 32)));
      
      (*(next_event)) = mk_eth_ip(e_0_1680, e_1_1681, e_2_1682, ip_0_1673, ip_1_1674, ip_2_1675, ip_3_1676);
      return pkt_1679->cursor <= pkt_1679->end;
      break;
    }
    case 0: {
      return 0;
      break;
    }
    default: {
      e_2_1682 = 39321;
      (*(next_event)) = mk_eth(e_0_1680, e_1_1681, e_2_1682);
      return pkt_1679->cursor <= pkt_1679->end;
      break;
    }
  }
  return 0;
}
void deparse_event(event_t*  ev_out , packet_t*  buf_out ){
  switch (ev_out->data.tag) {
    case 1: {
      uint16_t x_1654  = ev_out->data.args.background_1655.x_1654;
      write_bits(buf_out, ((uint64_t)(x_1654)), 16);
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
      uint64_t e_0_1656  = ev_out->data.args.eth_1659.e_0_1656;
      uint64_t e_1_1657  = ev_out->data.args.eth_1659.e_1_1657;
      uint16_t e_2_1658  = ev_out->data.args.eth_1659.e_2_1658;
      write_bits(buf_out, ((uint64_t)(e_2_1658)), 16);
      write_bits(buf_out, ((uint64_t)(e_1_1657)), 48);
      write_bits(buf_out, ((uint64_t)(e_0_1656)), 48);
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
      uint64_t e_0_1660  = ev_out->data.args.eth_ip_1668.e_0_1660;
      uint64_t e_1_1661  = ev_out->data.args.eth_ip_1668.e_1_1661;
      uint16_t e_2_1662  = ev_out->data.args.eth_ip_1668.e_2_1662;
      uint32_t ip_0_1663  = ev_out->data.args.eth_ip_1668.ip_0_1663;
      uint32_t ip_1_1664  = ev_out->data.args.eth_ip_1668.ip_1_1664;
      uint32_t ip_2_1665  = ev_out->data.args.eth_ip_1668.ip_2_1665;
      uint32_t ip_3_1666  = ev_out->data.args.eth_ip_1668.ip_3_1666;
      write_bits(buf_out, ((uint64_t)(ip_3_1666)), 32);
      write_bits(buf_out, ((uint64_t)(ip_2_1665)), 32);
      write_bits(buf_out, ((uint64_t)(ip_1_1664)), 32);
      write_bits(buf_out, ((uint64_t)(ip_0_1663)), 32);
      write_bits(buf_out, ((uint64_t)(e_2_1662)), 16);
      write_bits(buf_out, ((uint64_t)(e_1_1661)), 48);
      write_bits(buf_out, ((uint64_t)(e_0_1660)), 48);
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
      uint16_t x_1683  = ev_in->data.args.background_1655.x_1654;
      event_t this  = mk_background(x_1683);
      printf("%u\n", x_1683);
      break;
    }
    case 2: {
      uint64_t e_0_1684  = ev_in->data.args.eth_1659.e_0_1656;
      uint64_t e_1_1685  = ev_in->data.args.eth_1659.e_1_1657;
      uint16_t e_2_1686  = ev_in->data.args.eth_1659.e_2_1658;
      event_t this  = mk_eth(e_0_1684, e_1_1685, e_2_1686);
      out_event_t tmp_1833  = {.ev = mk_background(e_2_1686), .port = 4294967295};
      out_events[n] = tmp_1833;
      n = n + 1;
      break;
    }
    case 3: {
      uint64_t e_0_1687  = ev_in->data.args.eth_ip_1668.e_0_1660;
      uint64_t e_1_1688  = ev_in->data.args.eth_ip_1668.e_1_1661;
      uint16_t e_2_1689  = ev_in->data.args.eth_ip_1668.e_2_1662;
      uint32_t ip_0_1690  = ev_in->data.args.eth_ip_1668.ip_0_1663;
      uint32_t ip_1_1691  = ev_in->data.args.eth_ip_1668.ip_1_1664;
      uint32_t ip_2_1692  = ev_in->data.args.eth_ip_1668.ip_2_1665;
      uint32_t ip_3_1693  = ev_in->data.args.eth_ip_1668.ip_3_1666;
      event_t this  = mk_eth_ip(e_0_1687, e_1_1688, e_2_1689, ip_0_1690, ip_1_1691, ip_2_1692, ip_3_1693);
      out_event_t tmp_1834  = {.ev = mk_background(e_2_1689), .port = 4294967295};
      out_events[n] = tmp_1834;
      n = n + 1;
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

