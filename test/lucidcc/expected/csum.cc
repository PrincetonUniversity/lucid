typedef struct {
  uint64_t _0;
  uint64_t _1;
  uint16_t _2;
} tuple_1;
typedef struct {
  uint8_t _0;
  uint8_t _1;
  uint8_t _2;
  uint8_t _3;
  uint16_t _4;
  uint16_t _5;
  uint8_t _6;
  uint16_t _7;
  uint8_t _8;
  uint8_t _9;
  uint16_t _10;
  uint32_t _11;
  uint32_t _12;
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

/* write the low n bits of v into buf at bit offset `off`, MSB-first.
   buf must be pre-zeroed (bits are only OR'd in). Packs hash and checksum args. */
static void put_bits(uint8_t* buf, uint32_t off, uint64_t v, int n) {
    for (int i = 0; i < n; i++) {
        if ((v >> (n - 1 - i)) & 1) {
            uint32_t b = off + i;
            buf[b >> 3] |= (uint8_t)(1u << (7 - (b & 7)));
        }
    }
}

/* RFC 1071 ones-complement sum of buf[0..nbytes), 16-bit big-endian words;
   an odd final byte is the high byte of its word (right zero-padding). */
static uint16_t csum16_fold(const uint8_t* buf, uint32_t nbytes) {
    uint32_t sum = 0;
    for (uint32_t i = 0; i + 1 < nbytes; i += 2) sum += ((uint32_t)buf[i] << 8) | buf[i+1];
    if (nbytes & 1) sum += (uint32_t)buf[nbytes-1] << 8;
    while (sum >> 16) sum = (sum & 0xFFFF) + (sum >> 16);
    return (uint16_t)~sum;
}
uint16_t checksum16_0(uint8_t f0 , uint8_t f1 , uint8_t f2 , uint8_t f3 , uint16_t f4 , uint16_t f5 , uint8_t f6 , uint16_t f7 , uint8_t f8 , uint8_t f9 , uint16_t f10 , uint32_t f11 , uint32_t f12 ){
  uint8_t buf[20] = {0};
  put_bits(buf, 0, (uint64_t)f0, 4);
  put_bits(buf, 4, (uint64_t)f1, 4);
  put_bits(buf, 8, (uint64_t)f2, 6);
  put_bits(buf, 14, (uint64_t)f3, 2);
  put_bits(buf, 16, (uint64_t)f4, 16);
  put_bits(buf, 32, (uint64_t)f5, 16);
  put_bits(buf, 48, (uint64_t)f6, 3);
  put_bits(buf, 51, (uint64_t)f7, 13);
  put_bits(buf, 64, (uint64_t)f8, 8);
  put_bits(buf, 72, (uint64_t)f9, 8);
  put_bits(buf, 80, (uint64_t)f10, 16);
  put_bits(buf, 96, (uint64_t)f11, 32);
  put_bits(buf, 128, (uint64_t)f12, 32);
  return csum16_fold(buf, 20);
}
typedef struct {
  uint16_t len;
  uint8_t is_packet;
  uint8_t has_payload;
  uint64_t timestamp;
  uint32_t in_port;
} event_meta;
uint16_t ippkt_tag  = 1;
uint16_t other_tag  = 2;
typedef struct {
  uint16_t tag;
  union {
    struct {
      uint64_t eth_0_3597;
      uint64_t eth_1_3598;
      uint16_t eth_2_3599;
      uint8_t ip_0_3600;
      uint8_t ip_1_3601;
      uint8_t ip_2_3602;
      uint8_t ip_3_3603;
      uint16_t ip_4_3604;
      uint16_t ip_5_3605;
      uint8_t ip_6_3606;
      uint16_t ip_7_3607;
      uint8_t ip_8_3608;
      uint8_t ip_9_3609;
      uint16_t ip_10_3610;
      uint32_t ip_11_3611;
      uint32_t ip_12_3612;
    } ippkt_3614;
    struct {
      uint64_t eth_0_3615;
      uint64_t eth_1_3616;
      uint16_t eth_2_3617;
    } other_3619;
  } args;
} event_variant_t;
event_variant_t ippkt_3614(uint64_t eth_0_3597 , uint64_t eth_1_3598 , uint16_t eth_2_3599 , uint8_t ip_0_3600 , uint8_t ip_1_3601 , uint8_t ip_2_3602 , uint8_t ip_3_3603 , uint16_t ip_4_3604 , uint16_t ip_5_3605 , uint8_t ip_6_3606 , uint16_t ip_7_3607 , uint8_t ip_8_3608 , uint8_t ip_9_3609 , uint16_t ip_10_3610 , uint32_t ip_11_3611 , uint32_t ip_12_3612 ){
  event_variant_t ev  = {0};
  ev.args.ippkt_3614.eth_0_3597 = eth_0_3597;
  ev.args.ippkt_3614.eth_1_3598 = eth_1_3598;
  ev.args.ippkt_3614.eth_2_3599 = eth_2_3599;
  ev.args.ippkt_3614.ip_0_3600 = ip_0_3600;
  ev.args.ippkt_3614.ip_1_3601 = ip_1_3601;
  ev.args.ippkt_3614.ip_2_3602 = ip_2_3602;
  ev.args.ippkt_3614.ip_3_3603 = ip_3_3603;
  ev.args.ippkt_3614.ip_4_3604 = ip_4_3604;
  ev.args.ippkt_3614.ip_5_3605 = ip_5_3605;
  ev.args.ippkt_3614.ip_6_3606 = ip_6_3606;
  ev.args.ippkt_3614.ip_7_3607 = ip_7_3607;
  ev.args.ippkt_3614.ip_8_3608 = ip_8_3608;
  ev.args.ippkt_3614.ip_9_3609 = ip_9_3609;
  ev.args.ippkt_3614.ip_10_3610 = ip_10_3610;
  ev.args.ippkt_3614.ip_11_3611 = ip_11_3611;
  ev.args.ippkt_3614.ip_12_3612 = ip_12_3612;
  ev.tag = ippkt_tag;
  return ev;
}
event_variant_t other_3619(uint64_t eth_0_3615 , uint64_t eth_1_3616 , uint16_t eth_2_3617 ){
  event_variant_t ev  = {0};
  ev.args.other_3619.eth_0_3615 = eth_0_3615;
  ev.args.other_3619.eth_1_3616 = eth_1_3616;
  ev.args.other_3619.eth_2_3617 = eth_2_3617;
  ev.tag = other_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_ippkt(uint64_t eth_0_3597 , uint64_t eth_1_3598 , uint16_t eth_2_3599 , uint8_t ip_0_3600 , uint8_t ip_1_3601 , uint8_t ip_2_3602 , uint8_t ip_3_3603 , uint16_t ip_4_3604 , uint16_t ip_5_3605 , uint8_t ip_6_3606 , uint16_t ip_7_3607 , uint8_t ip_8_3608 , uint8_t ip_9_3609 , uint16_t ip_10_3610 , uint32_t ip_11_3611 , uint32_t ip_12_3612 ){
  event_t tmp_3952  = {.meta = {.len = 34, .is_packet = 1, .has_payload = 1, .timestamp = 0, .in_port = 0}, .data = ippkt_3614(eth_0_3597, eth_1_3598, eth_2_3599, ip_0_3600, ip_1_3601, ip_2_3602, ip_3_3603, ip_4_3604, ip_5_3605, ip_6_3606, ip_7_3607, ip_8_3608, ip_9_3609, ip_10_3610, ip_11_3611, ip_12_3612)};
  return tmp_3952;
}
event_t mk_other(uint64_t eth_0_3615 , uint64_t eth_1_3616 , uint16_t eth_2_3617 ){
  event_t tmp_3953  = {.meta = {.len = 14, .is_packet = 1, .has_payload = 1, .timestamp = 0, .in_port = 0}, .data = other_3619(eth_0_3615, eth_1_3616, eth_2_3617)};
  return tmp_3953;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
typedef tuple_1 eth_t_3595;
typedef tuple_2 ip_t_3596;
uint8_t parse_event(packet_t*  pkt_3620 , event_t*  next_event , uint32_t ingress_port ){
  uint64_t e_0_3621  = ((uint64_t)(read_bits(pkt_3620, 48))) & 281474976710655;
  uint64_t e_1_3622  = ((uint64_t)(read_bits(pkt_3620, 48))) & 281474976710655;
  uint16_t e_2_3623  = ((uint16_t)(read_bits(pkt_3620, 16)));
  switch (e_2_3623) {
    case 666: {
      return 0;
      break;
    }
    case 2048: {
      uint8_t ip_0_3624  = ((uint8_t)(read_bits(pkt_3620, 4))) & 15;
      uint8_t ip_1_3625  = ((uint8_t)(read_bits(pkt_3620, 4))) & 15;
      uint8_t ip_2_3626  = ((uint8_t)(read_bits(pkt_3620, 6))) & 63;
      uint8_t ip_3_3627  = ((uint8_t)(read_bits(pkt_3620, 2))) & 3;
      uint16_t ip_4_3628  = ((uint16_t)(read_bits(pkt_3620, 16)));
      uint16_t ip_5_3629  = ((uint16_t)(read_bits(pkt_3620, 16)));
      uint8_t ip_6_3630  = ((uint8_t)(read_bits(pkt_3620, 3))) & 7;
      uint16_t ip_7_3631  = ((uint16_t)(read_bits(pkt_3620, 13))) & 8191;
      uint8_t ip_8_3632  = ((uint8_t)(read_bits(pkt_3620, 8)));
      uint8_t ip_9_3633  = ((uint8_t)(read_bits(pkt_3620, 8)));
      uint16_t ip_10_3634  = ((uint16_t)(read_bits(pkt_3620, 16)));
      uint32_t ip_11_3635  = ((uint32_t)(read_bits(pkt_3620, 32)));
      uint32_t ip_12_3636  = ((uint32_t)(read_bits(pkt_3620, 32)));
      (*(next_event)) = mk_ippkt(e_0_3621, e_1_3622, e_2_3623, ip_0_3624, ip_1_3625, ip_2_3626, ip_3_3627, ip_4_3628, ip_5_3629, ip_6_3630, ip_7_3631, ip_8_3632, ip_9_3633, ip_10_3634, ip_11_3635, ip_12_3636);
      return pkt_3620->cursor <= pkt_3620->end;
      break;
    }
    default: {
      (*(next_event)) = mk_other(e_0_3621, e_1_3622, e_2_3623);
      return pkt_3620->cursor <= pkt_3620->end;
      break;
    }
  }
  return 0;
}
void deparse_event(event_t*  ev_out , packet_t*  buf_out ){
  switch (ev_out->data.tag) {
    case 1: {
      uint64_t eth_0_3597  = ev_out->data.args.ippkt_3614.eth_0_3597;
      uint64_t eth_1_3598  = ev_out->data.args.ippkt_3614.eth_1_3598;
      uint16_t eth_2_3599  = ev_out->data.args.ippkt_3614.eth_2_3599;
      uint8_t ip_0_3600  = ev_out->data.args.ippkt_3614.ip_0_3600;
      uint8_t ip_1_3601  = ev_out->data.args.ippkt_3614.ip_1_3601;
      uint8_t ip_2_3602  = ev_out->data.args.ippkt_3614.ip_2_3602;
      uint8_t ip_3_3603  = ev_out->data.args.ippkt_3614.ip_3_3603;
      uint16_t ip_4_3604  = ev_out->data.args.ippkt_3614.ip_4_3604;
      uint16_t ip_5_3605  = ev_out->data.args.ippkt_3614.ip_5_3605;
      uint8_t ip_6_3606  = ev_out->data.args.ippkt_3614.ip_6_3606;
      uint16_t ip_7_3607  = ev_out->data.args.ippkt_3614.ip_7_3607;
      uint8_t ip_8_3608  = ev_out->data.args.ippkt_3614.ip_8_3608;
      uint8_t ip_9_3609  = ev_out->data.args.ippkt_3614.ip_9_3609;
      uint16_t ip_10_3610  = ev_out->data.args.ippkt_3614.ip_10_3610;
      uint32_t ip_11_3611  = ev_out->data.args.ippkt_3614.ip_11_3611;
      uint32_t ip_12_3612  = ev_out->data.args.ippkt_3614.ip_12_3612;
      write_bits(buf_out, ((uint64_t)(ip_12_3612)), 32);
      write_bits(buf_out, ((uint64_t)(ip_11_3611)), 32);
      write_bits(buf_out, ((uint64_t)(ip_10_3610)), 16);
      write_bits(buf_out, ((uint64_t)(ip_9_3609)), 8);
      write_bits(buf_out, ((uint64_t)(ip_8_3608)), 8);
      write_bits(buf_out, ((uint64_t)(ip_7_3607)), 13);
      write_bits(buf_out, ((uint64_t)(ip_6_3606)), 3);
      write_bits(buf_out, ((uint64_t)(ip_5_3605)), 16);
      write_bits(buf_out, ((uint64_t)(ip_4_3604)), 16);
      write_bits(buf_out, ((uint64_t)(ip_3_3603)), 2);
      write_bits(buf_out, ((uint64_t)(ip_2_3602)), 6);
      write_bits(buf_out, ((uint64_t)(ip_1_3601)), 4);
      write_bits(buf_out, ((uint64_t)(ip_0_3600)), 4);
      write_bits(buf_out, ((uint64_t)(eth_2_3599)), 16);
      write_bits(buf_out, ((uint64_t)(eth_1_3598)), 48);
      write_bits(buf_out, ((uint64_t)(eth_0_3597)), 48);
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
      uint64_t eth_0_3615  = ev_out->data.args.other_3619.eth_0_3615;
      uint64_t eth_1_3616  = ev_out->data.args.other_3619.eth_1_3616;
      uint16_t eth_2_3617  = ev_out->data.args.other_3619.eth_2_3617;
      write_bits(buf_out, ((uint64_t)(eth_2_3617)), 16);
      write_bits(buf_out, ((uint64_t)(eth_1_3616)), 48);
      write_bits(buf_out, ((uint64_t)(eth_0_3615)), 48);
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
    case 1: {
      uint64_t eth_0_3637  = ev_in->data.args.ippkt_3614.eth_0_3597;
      uint64_t eth_1_3638  = ev_in->data.args.ippkt_3614.eth_1_3598;
      uint16_t eth_2_3639  = ev_in->data.args.ippkt_3614.eth_2_3599;
      uint8_t ip_0_3640  = ev_in->data.args.ippkt_3614.ip_0_3600;
      uint8_t ip_1_3641  = ev_in->data.args.ippkt_3614.ip_1_3601;
      uint8_t ip_2_3642  = ev_in->data.args.ippkt_3614.ip_2_3602;
      uint8_t ip_3_3643  = ev_in->data.args.ippkt_3614.ip_3_3603;
      uint16_t ip_4_3644  = ev_in->data.args.ippkt_3614.ip_4_3604;
      uint16_t ip_5_3645  = ev_in->data.args.ippkt_3614.ip_5_3605;
      uint8_t ip_6_3646  = ev_in->data.args.ippkt_3614.ip_6_3606;
      uint16_t ip_7_3647  = ev_in->data.args.ippkt_3614.ip_7_3607;
      uint8_t ip_8_3648  = ev_in->data.args.ippkt_3614.ip_8_3608;
      uint8_t ip_9_3649  = ev_in->data.args.ippkt_3614.ip_9_3609;
      uint16_t ip_10_3650  = ev_in->data.args.ippkt_3614.ip_10_3610;
      uint32_t ip_11_3651  = ev_in->data.args.ippkt_3614.ip_11_3611;
      uint32_t ip_12_3652  = ev_in->data.args.ippkt_3614.ip_12_3612;
      event_t this  = mk_ippkt(eth_0_3637, eth_1_3638, eth_2_3639, ip_0_3640, ip_1_3641, ip_2_3642, ip_3_3643, ip_4_3644, ip_5_3645, ip_6_3646, ip_7_3647, ip_8_3648, ip_9_3649, ip_10_3650, ip_11_3651, ip_12_3652);
      uint16_t fixed_10_3677  = checksum16_0(ip_0_3640, ip_1_3641, ip_2_3642, ip_3_3643, ip_4_3644, ip_5_3645, ip_6_3646, ip_7_3647, ip_8_3648, ip_9_3649, 0, ip_11_3651, ip_12_3652);
      out_event_t tmp_3954  = {.ev = mk_ippkt(eth_0_3637, eth_1_3638, eth_2_3639, ip_0_3640, ip_1_3641, ip_2_3642, ip_3_3643, ip_4_3644, ip_5_3645, ip_6_3646, ip_7_3647, ip_8_3648, ip_9_3649, fixed_10_3677, ip_11_3651, ip_12_3652), .port = ev_in->meta.in_port};
      out_events[n] = tmp_3954;
      n = n + 1;
      break;
    }
    case 2: {
      uint64_t eth_0_3680  = ev_in->data.args.other_3619.eth_0_3615;
      uint64_t eth_1_3681  = ev_in->data.args.other_3619.eth_1_3616;
      uint16_t eth_2_3682  = ev_in->data.args.other_3619.eth_2_3617;
      event_t this  = mk_other(eth_0_3680, eth_1_3681, eth_2_3682);
      out_event_t tmp_3955  = {.ev = this, .port = ev_in->meta.in_port};
      out_events[n] = tmp_3955;
      n = n + 1;
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

