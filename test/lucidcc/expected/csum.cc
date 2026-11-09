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
      uint64_t eth_0_3557;
      uint64_t eth_1_3558;
      uint16_t eth_2_3559;
      uint8_t ip_0_3560;
      uint8_t ip_1_3561;
      uint8_t ip_2_3562;
      uint8_t ip_3_3563;
      uint16_t ip_4_3564;
      uint16_t ip_5_3565;
      uint8_t ip_6_3566;
      uint16_t ip_7_3567;
      uint8_t ip_8_3568;
      uint8_t ip_9_3569;
      uint16_t ip_10_3570;
      uint32_t ip_11_3571;
      uint32_t ip_12_3572;
    } ippkt_3574;
    struct {
      uint64_t eth_0_3575;
      uint64_t eth_1_3576;
      uint16_t eth_2_3577;
    } other_3579;
  } args;
} event_variant_t;
event_variant_t ippkt_3574(uint64_t eth_0_3557 , uint64_t eth_1_3558 , uint16_t eth_2_3559 , uint8_t ip_0_3560 , uint8_t ip_1_3561 , uint8_t ip_2_3562 , uint8_t ip_3_3563 , uint16_t ip_4_3564 , uint16_t ip_5_3565 , uint8_t ip_6_3566 , uint16_t ip_7_3567 , uint8_t ip_8_3568 , uint8_t ip_9_3569 , uint16_t ip_10_3570 , uint32_t ip_11_3571 , uint32_t ip_12_3572 ){
  event_variant_t ev  = {0};
  ev.args.ippkt_3574.eth_0_3557 = eth_0_3557;
  ev.args.ippkt_3574.eth_1_3558 = eth_1_3558;
  ev.args.ippkt_3574.eth_2_3559 = eth_2_3559;
  ev.args.ippkt_3574.ip_0_3560 = ip_0_3560;
  ev.args.ippkt_3574.ip_1_3561 = ip_1_3561;
  ev.args.ippkt_3574.ip_2_3562 = ip_2_3562;
  ev.args.ippkt_3574.ip_3_3563 = ip_3_3563;
  ev.args.ippkt_3574.ip_4_3564 = ip_4_3564;
  ev.args.ippkt_3574.ip_5_3565 = ip_5_3565;
  ev.args.ippkt_3574.ip_6_3566 = ip_6_3566;
  ev.args.ippkt_3574.ip_7_3567 = ip_7_3567;
  ev.args.ippkt_3574.ip_8_3568 = ip_8_3568;
  ev.args.ippkt_3574.ip_9_3569 = ip_9_3569;
  ev.args.ippkt_3574.ip_10_3570 = ip_10_3570;
  ev.args.ippkt_3574.ip_11_3571 = ip_11_3571;
  ev.args.ippkt_3574.ip_12_3572 = ip_12_3572;
  ev.tag = ippkt_tag;
  return ev;
}
event_variant_t other_3579(uint64_t eth_0_3575 , uint64_t eth_1_3576 , uint16_t eth_2_3577 ){
  event_variant_t ev  = {0};
  ev.args.other_3579.eth_0_3575 = eth_0_3575;
  ev.args.other_3579.eth_1_3576 = eth_1_3576;
  ev.args.other_3579.eth_2_3577 = eth_2_3577;
  ev.tag = other_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_ippkt(uint64_t eth_0_3557 , uint64_t eth_1_3558 , uint16_t eth_2_3559 , uint8_t ip_0_3560 , uint8_t ip_1_3561 , uint8_t ip_2_3562 , uint8_t ip_3_3563 , uint16_t ip_4_3564 , uint16_t ip_5_3565 , uint8_t ip_6_3566 , uint16_t ip_7_3567 , uint8_t ip_8_3568 , uint8_t ip_9_3569 , uint16_t ip_10_3570 , uint32_t ip_11_3571 , uint32_t ip_12_3572 ){
  event_t tmp_3912  = {.meta = {.len = 34, .is_packet = 1, .has_payload = 1, .timestamp = 0, .in_port = 0}, .data = ippkt_3574(eth_0_3557, eth_1_3558, eth_2_3559, ip_0_3560, ip_1_3561, ip_2_3562, ip_3_3563, ip_4_3564, ip_5_3565, ip_6_3566, ip_7_3567, ip_8_3568, ip_9_3569, ip_10_3570, ip_11_3571, ip_12_3572)};
  return tmp_3912;
}
event_t mk_other(uint64_t eth_0_3575 , uint64_t eth_1_3576 , uint16_t eth_2_3577 ){
  event_t tmp_3913  = {.meta = {.len = 14, .is_packet = 1, .has_payload = 1, .timestamp = 0, .in_port = 0}, .data = other_3579(eth_0_3575, eth_1_3576, eth_2_3577)};
  return tmp_3913;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
typedef tuple_1 eth_t_3555;
typedef tuple_2 ip_t_3556;
uint8_t parse_event(packet_t*  pkt_3580 , event_t*  next_event , uint32_t ingress_port ){
  uint64_t e_0_3581  = ((uint64_t)(read_bits(pkt_3580, 48))) & 281474976710655;
  uint64_t e_1_3582  = ((uint64_t)(read_bits(pkt_3580, 48))) & 281474976710655;
  uint16_t e_2_3583  = ((uint16_t)(read_bits(pkt_3580, 16)));
  switch (e_2_3583) {
    case 666: {
      return 0;
      break;
    }
    case 2048: {
      uint8_t ip_0_3584  = ((uint8_t)(read_bits(pkt_3580, 4))) & 15;
      uint8_t ip_1_3585  = ((uint8_t)(read_bits(pkt_3580, 4))) & 15;
      uint8_t ip_2_3586  = ((uint8_t)(read_bits(pkt_3580, 6))) & 63;
      uint8_t ip_3_3587  = ((uint8_t)(read_bits(pkt_3580, 2))) & 3;
      uint16_t ip_4_3588  = ((uint16_t)(read_bits(pkt_3580, 16)));
      uint16_t ip_5_3589  = ((uint16_t)(read_bits(pkt_3580, 16)));
      uint8_t ip_6_3590  = ((uint8_t)(read_bits(pkt_3580, 3))) & 7;
      uint16_t ip_7_3591  = ((uint16_t)(read_bits(pkt_3580, 13))) & 8191;
      uint8_t ip_8_3592  = ((uint8_t)(read_bits(pkt_3580, 8)));
      uint8_t ip_9_3593  = ((uint8_t)(read_bits(pkt_3580, 8)));
      uint16_t ip_10_3594  = ((uint16_t)(read_bits(pkt_3580, 16)));
      uint32_t ip_11_3595  = ((uint32_t)(read_bits(pkt_3580, 32)));
      uint32_t ip_12_3596  = ((uint32_t)(read_bits(pkt_3580, 32)));
      (*(next_event)) = mk_ippkt(e_0_3581, e_1_3582, e_2_3583, ip_0_3584, ip_1_3585, ip_2_3586, ip_3_3587, ip_4_3588, ip_5_3589, ip_6_3590, ip_7_3591, ip_8_3592, ip_9_3593, ip_10_3594, ip_11_3595, ip_12_3596);
      return pkt_3580->cursor <= pkt_3580->end;
      break;
    }
    default: {
      (*(next_event)) = mk_other(e_0_3581, e_1_3582, e_2_3583);
      return pkt_3580->cursor <= pkt_3580->end;
      break;
    }
  }
  return 0;
}
void deparse_event(event_t*  ev_out , packet_t*  buf_out ){
  switch (ev_out->data.tag) {
    case 1: {
      uint64_t eth_0_3557  = ev_out->data.args.ippkt_3574.eth_0_3557;
      uint64_t eth_1_3558  = ev_out->data.args.ippkt_3574.eth_1_3558;
      uint16_t eth_2_3559  = ev_out->data.args.ippkt_3574.eth_2_3559;
      uint8_t ip_0_3560  = ev_out->data.args.ippkt_3574.ip_0_3560;
      uint8_t ip_1_3561  = ev_out->data.args.ippkt_3574.ip_1_3561;
      uint8_t ip_2_3562  = ev_out->data.args.ippkt_3574.ip_2_3562;
      uint8_t ip_3_3563  = ev_out->data.args.ippkt_3574.ip_3_3563;
      uint16_t ip_4_3564  = ev_out->data.args.ippkt_3574.ip_4_3564;
      uint16_t ip_5_3565  = ev_out->data.args.ippkt_3574.ip_5_3565;
      uint8_t ip_6_3566  = ev_out->data.args.ippkt_3574.ip_6_3566;
      uint16_t ip_7_3567  = ev_out->data.args.ippkt_3574.ip_7_3567;
      uint8_t ip_8_3568  = ev_out->data.args.ippkt_3574.ip_8_3568;
      uint8_t ip_9_3569  = ev_out->data.args.ippkt_3574.ip_9_3569;
      uint16_t ip_10_3570  = ev_out->data.args.ippkt_3574.ip_10_3570;
      uint32_t ip_11_3571  = ev_out->data.args.ippkt_3574.ip_11_3571;
      uint32_t ip_12_3572  = ev_out->data.args.ippkt_3574.ip_12_3572;
      write_bits(buf_out, ((uint64_t)(ip_12_3572)), 32);
      write_bits(buf_out, ((uint64_t)(ip_11_3571)), 32);
      write_bits(buf_out, ((uint64_t)(ip_10_3570)), 16);
      write_bits(buf_out, ((uint64_t)(ip_9_3569)), 8);
      write_bits(buf_out, ((uint64_t)(ip_8_3568)), 8);
      write_bits(buf_out, ((uint64_t)(ip_7_3567)), 13);
      write_bits(buf_out, ((uint64_t)(ip_6_3566)), 3);
      write_bits(buf_out, ((uint64_t)(ip_5_3565)), 16);
      write_bits(buf_out, ((uint64_t)(ip_4_3564)), 16);
      write_bits(buf_out, ((uint64_t)(ip_3_3563)), 2);
      write_bits(buf_out, ((uint64_t)(ip_2_3562)), 6);
      write_bits(buf_out, ((uint64_t)(ip_1_3561)), 4);
      write_bits(buf_out, ((uint64_t)(ip_0_3560)), 4);
      write_bits(buf_out, ((uint64_t)(eth_2_3559)), 16);
      write_bits(buf_out, ((uint64_t)(eth_1_3558)), 48);
      write_bits(buf_out, ((uint64_t)(eth_0_3557)), 48);
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
      uint64_t eth_0_3575  = ev_out->data.args.other_3579.eth_0_3575;
      uint64_t eth_1_3576  = ev_out->data.args.other_3579.eth_1_3576;
      uint16_t eth_2_3577  = ev_out->data.args.other_3579.eth_2_3577;
      write_bits(buf_out, ((uint64_t)(eth_2_3577)), 16);
      write_bits(buf_out, ((uint64_t)(eth_1_3576)), 48);
      write_bits(buf_out, ((uint64_t)(eth_0_3575)), 48);
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
      uint64_t eth_0_3597  = ev_in->data.args.ippkt_3574.eth_0_3557;
      uint64_t eth_1_3598  = ev_in->data.args.ippkt_3574.eth_1_3558;
      uint16_t eth_2_3599  = ev_in->data.args.ippkt_3574.eth_2_3559;
      uint8_t ip_0_3600  = ev_in->data.args.ippkt_3574.ip_0_3560;
      uint8_t ip_1_3601  = ev_in->data.args.ippkt_3574.ip_1_3561;
      uint8_t ip_2_3602  = ev_in->data.args.ippkt_3574.ip_2_3562;
      uint8_t ip_3_3603  = ev_in->data.args.ippkt_3574.ip_3_3563;
      uint16_t ip_4_3604  = ev_in->data.args.ippkt_3574.ip_4_3564;
      uint16_t ip_5_3605  = ev_in->data.args.ippkt_3574.ip_5_3565;
      uint8_t ip_6_3606  = ev_in->data.args.ippkt_3574.ip_6_3566;
      uint16_t ip_7_3607  = ev_in->data.args.ippkt_3574.ip_7_3567;
      uint8_t ip_8_3608  = ev_in->data.args.ippkt_3574.ip_8_3568;
      uint8_t ip_9_3609  = ev_in->data.args.ippkt_3574.ip_9_3569;
      uint16_t ip_10_3610  = ev_in->data.args.ippkt_3574.ip_10_3570;
      uint32_t ip_11_3611  = ev_in->data.args.ippkt_3574.ip_11_3571;
      uint32_t ip_12_3612  = ev_in->data.args.ippkt_3574.ip_12_3572;
      event_t this  = mk_ippkt(eth_0_3597, eth_1_3598, eth_2_3599, ip_0_3600, ip_1_3601, ip_2_3602, ip_3_3603, ip_4_3604, ip_5_3605, ip_6_3606, ip_7_3607, ip_8_3608, ip_9_3609, ip_10_3610, ip_11_3611, ip_12_3612);
      uint16_t fixed_10_3637  = checksum16_0(ip_0_3600, ip_1_3601, ip_2_3602, ip_3_3603, ip_4_3604, ip_5_3605, ip_6_3606, ip_7_3607, ip_8_3608, ip_9_3609, 0, ip_11_3611, ip_12_3612);
      out_event_t tmp_3914  = {.ev = mk_ippkt(eth_0_3597, eth_1_3598, eth_2_3599, ip_0_3600, ip_1_3601, ip_2_3602, ip_3_3603, ip_4_3604, ip_5_3605, ip_6_3606, ip_7_3607, ip_8_3608, ip_9_3609, fixed_10_3637, ip_11_3611, ip_12_3612), .port = ev_in->meta.in_port};
      out_events[n] = tmp_3914;
      n = n + 1;
      break;
    }
    case 2: {
      uint64_t eth_0_3640  = ev_in->data.args.other_3579.eth_0_3575;
      uint64_t eth_1_3641  = ev_in->data.args.other_3579.eth_1_3576;
      uint16_t eth_2_3642  = ev_in->data.args.other_3579.eth_2_3577;
      event_t this  = mk_other(eth_0_3640, eth_1_3641, eth_2_3642);
      out_event_t tmp_3915  = {.ev = this, .port = ev_in->meta.in_port};
      out_events[n] = tmp_3915;
      n = n + 1;
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

