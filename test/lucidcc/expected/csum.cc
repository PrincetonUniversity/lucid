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
uint16_t ippkt_tag  = 1;
uint16_t other_tag  = 2;
typedef struct {
  uint16_t tag;
  union {
    struct {
      uint64_t eth_0_3586;
      uint64_t eth_1_3587;
      uint16_t eth_2_3588;
      uint8_t ip_0_3589;
      uint8_t ip_1_3590;
      uint8_t ip_2_3591;
      uint8_t ip_3_3592;
      uint16_t ip_4_3593;
      uint16_t ip_5_3594;
      uint8_t ip_6_3595;
      uint16_t ip_7_3596;
      uint8_t ip_8_3597;
      uint8_t ip_9_3598;
      uint16_t ip_10_3599;
      uint32_t ip_11_3600;
      uint32_t ip_12_3601;
    } ippkt_3603;
    struct {
      uint64_t eth_0_3604;
      uint64_t eth_1_3605;
      uint16_t eth_2_3606;
    } other_3608;
  } args;
} event_variant_t;
event_variant_t ippkt_3603(uint64_t eth_0_3586 , uint64_t eth_1_3587 , uint16_t eth_2_3588 , uint8_t ip_0_3589 , uint8_t ip_1_3590 , uint8_t ip_2_3591 , uint8_t ip_3_3592 , uint16_t ip_4_3593 , uint16_t ip_5_3594 , uint8_t ip_6_3595 , uint16_t ip_7_3596 , uint8_t ip_8_3597 , uint8_t ip_9_3598 , uint16_t ip_10_3599 , uint32_t ip_11_3600 , uint32_t ip_12_3601 ){
  event_variant_t ev  = {0};
  ev.args.ippkt_3603.eth_0_3586 = eth_0_3586;
  ev.args.ippkt_3603.eth_1_3587 = eth_1_3587;
  ev.args.ippkt_3603.eth_2_3588 = eth_2_3588;
  ev.args.ippkt_3603.ip_0_3589 = ip_0_3589;
  ev.args.ippkt_3603.ip_1_3590 = ip_1_3590;
  ev.args.ippkt_3603.ip_2_3591 = ip_2_3591;
  ev.args.ippkt_3603.ip_3_3592 = ip_3_3592;
  ev.args.ippkt_3603.ip_4_3593 = ip_4_3593;
  ev.args.ippkt_3603.ip_5_3594 = ip_5_3594;
  ev.args.ippkt_3603.ip_6_3595 = ip_6_3595;
  ev.args.ippkt_3603.ip_7_3596 = ip_7_3596;
  ev.args.ippkt_3603.ip_8_3597 = ip_8_3597;
  ev.args.ippkt_3603.ip_9_3598 = ip_9_3598;
  ev.args.ippkt_3603.ip_10_3599 = ip_10_3599;
  ev.args.ippkt_3603.ip_11_3600 = ip_11_3600;
  ev.args.ippkt_3603.ip_12_3601 = ip_12_3601;
  ev.tag = ippkt_tag;
  return ev;
}
event_variant_t other_3608(uint64_t eth_0_3604 , uint64_t eth_1_3605 , uint16_t eth_2_3606 ){
  event_variant_t ev  = {0};
  ev.args.other_3608.eth_0_3604 = eth_0_3604;
  ev.args.other_3608.eth_1_3605 = eth_1_3605;
  ev.args.other_3608.eth_2_3606 = eth_2_3606;
  ev.tag = other_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_ippkt(uint64_t eth_0_3586 , uint64_t eth_1_3587 , uint16_t eth_2_3588 , uint8_t ip_0_3589 , uint8_t ip_1_3590 , uint8_t ip_2_3591 , uint8_t ip_3_3592 , uint16_t ip_4_3593 , uint16_t ip_5_3594 , uint8_t ip_6_3595 , uint16_t ip_7_3596 , uint8_t ip_8_3597 , uint8_t ip_9_3598 , uint16_t ip_10_3599 , uint32_t ip_11_3600 , uint32_t ip_12_3601 ){
  event_t tmp_3941  = {.meta = {.len = 34, .is_packet = 1, .has_payload = 1, .timestamp = 0, .in_port = 0}, .data = ippkt_3603(eth_0_3586, eth_1_3587, eth_2_3588, ip_0_3589, ip_1_3590, ip_2_3591, ip_3_3592, ip_4_3593, ip_5_3594, ip_6_3595, ip_7_3596, ip_8_3597, ip_9_3598, ip_10_3599, ip_11_3600, ip_12_3601)};
  return tmp_3941;
}
event_t mk_other(uint64_t eth_0_3604 , uint64_t eth_1_3605 , uint16_t eth_2_3606 ){
  event_t tmp_3942  = {.meta = {.len = 14, .is_packet = 1, .has_payload = 1, .timestamp = 0, .in_port = 0}, .data = other_3608(eth_0_3604, eth_1_3605, eth_2_3606)};
  return tmp_3942;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
typedef tuple_1 eth_t_3584;
typedef tuple_2 ip_t_3585;
uint8_t parse_event(packet_t*  pkt_3609 , event_t*  next_event , uint32_t ingress_port ){
  uint64_t e_0_3610  = ((uint64_t)(read_bits(pkt_3609, 48))) & 281474976710655;
  uint64_t e_1_3611  = ((uint64_t)(read_bits(pkt_3609, 48))) & 281474976710655;
  uint16_t e_2_3612  = ((uint16_t)(read_bits(pkt_3609, 16)));
  switch (e_2_3612) {
    case 666: {
      return 0;
      break;
    }
    case 2048: {
      uint8_t ip_0_3613  = ((uint8_t)(read_bits(pkt_3609, 4))) & 15;
      uint8_t ip_1_3614  = ((uint8_t)(read_bits(pkt_3609, 4))) & 15;
      uint8_t ip_2_3615  = ((uint8_t)(read_bits(pkt_3609, 6))) & 63;
      uint8_t ip_3_3616  = ((uint8_t)(read_bits(pkt_3609, 2))) & 3;
      uint16_t ip_4_3617  = ((uint16_t)(read_bits(pkt_3609, 16)));
      uint16_t ip_5_3618  = ((uint16_t)(read_bits(pkt_3609, 16)));
      uint8_t ip_6_3619  = ((uint8_t)(read_bits(pkt_3609, 3))) & 7;
      uint16_t ip_7_3620  = ((uint16_t)(read_bits(pkt_3609, 13))) & 8191;
      uint8_t ip_8_3621  = ((uint8_t)(read_bits(pkt_3609, 8)));
      uint8_t ip_9_3622  = ((uint8_t)(read_bits(pkt_3609, 8)));
      uint16_t ip_10_3623  = ((uint16_t)(read_bits(pkt_3609, 16)));
      uint32_t ip_11_3624  = ((uint32_t)(read_bits(pkt_3609, 32)));
      uint32_t ip_12_3625  = ((uint32_t)(read_bits(pkt_3609, 32)));
      (*(next_event)) = mk_ippkt(e_0_3610, e_1_3611, e_2_3612, ip_0_3613, ip_1_3614, ip_2_3615, ip_3_3616, ip_4_3617, ip_5_3618, ip_6_3619, ip_7_3620, ip_8_3621, ip_9_3622, ip_10_3623, ip_11_3624, ip_12_3625);
      return pkt_3609->cursor <= pkt_3609->end;
      break;
    }
    default: {
      (*(next_event)) = mk_other(e_0_3610, e_1_3611, e_2_3612);
      return pkt_3609->cursor <= pkt_3609->end;
      break;
    }
  }
  return 0;
}
void deparse_event(event_t*  ev_out , packet_t*  buf_out ){
  switch (ev_out->data.tag) {
    case 1: {
      uint64_t eth_0_3586  = ev_out->data.args.ippkt_3603.eth_0_3586;
      uint64_t eth_1_3587  = ev_out->data.args.ippkt_3603.eth_1_3587;
      uint16_t eth_2_3588  = ev_out->data.args.ippkt_3603.eth_2_3588;
      uint8_t ip_0_3589  = ev_out->data.args.ippkt_3603.ip_0_3589;
      uint8_t ip_1_3590  = ev_out->data.args.ippkt_3603.ip_1_3590;
      uint8_t ip_2_3591  = ev_out->data.args.ippkt_3603.ip_2_3591;
      uint8_t ip_3_3592  = ev_out->data.args.ippkt_3603.ip_3_3592;
      uint16_t ip_4_3593  = ev_out->data.args.ippkt_3603.ip_4_3593;
      uint16_t ip_5_3594  = ev_out->data.args.ippkt_3603.ip_5_3594;
      uint8_t ip_6_3595  = ev_out->data.args.ippkt_3603.ip_6_3595;
      uint16_t ip_7_3596  = ev_out->data.args.ippkt_3603.ip_7_3596;
      uint8_t ip_8_3597  = ev_out->data.args.ippkt_3603.ip_8_3597;
      uint8_t ip_9_3598  = ev_out->data.args.ippkt_3603.ip_9_3598;
      uint16_t ip_10_3599  = ev_out->data.args.ippkt_3603.ip_10_3599;
      uint32_t ip_11_3600  = ev_out->data.args.ippkt_3603.ip_11_3600;
      uint32_t ip_12_3601  = ev_out->data.args.ippkt_3603.ip_12_3601;
      write_bits(buf_out, ((uint64_t)(ip_12_3601)), 32);
      write_bits(buf_out, ((uint64_t)(ip_11_3600)), 32);
      write_bits(buf_out, ((uint64_t)(ip_10_3599)), 16);
      write_bits(buf_out, ((uint64_t)(ip_9_3598)), 8);
      write_bits(buf_out, ((uint64_t)(ip_8_3597)), 8);
      write_bits(buf_out, ((uint64_t)(ip_7_3596)), 13);
      write_bits(buf_out, ((uint64_t)(ip_6_3595)), 3);
      write_bits(buf_out, ((uint64_t)(ip_5_3594)), 16);
      write_bits(buf_out, ((uint64_t)(ip_4_3593)), 16);
      write_bits(buf_out, ((uint64_t)(ip_3_3592)), 2);
      write_bits(buf_out, ((uint64_t)(ip_2_3591)), 6);
      write_bits(buf_out, ((uint64_t)(ip_1_3590)), 4);
      write_bits(buf_out, ((uint64_t)(ip_0_3589)), 4);
      write_bits(buf_out, ((uint64_t)(eth_2_3588)), 16);
      write_bits(buf_out, ((uint64_t)(eth_1_3587)), 48);
      write_bits(buf_out, ((uint64_t)(eth_0_3586)), 48);
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
      uint64_t eth_0_3604  = ev_out->data.args.other_3608.eth_0_3604;
      uint64_t eth_1_3605  = ev_out->data.args.other_3608.eth_1_3605;
      uint16_t eth_2_3606  = ev_out->data.args.other_3608.eth_2_3606;
      write_bits(buf_out, ((uint64_t)(eth_2_3606)), 16);
      write_bits(buf_out, ((uint64_t)(eth_1_3605)), 48);
      write_bits(buf_out, ((uint64_t)(eth_0_3604)), 48);
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
      uint64_t eth_0_3626  = ev_in->data.args.ippkt_3603.eth_0_3586;
      uint64_t eth_1_3627  = ev_in->data.args.ippkt_3603.eth_1_3587;
      uint16_t eth_2_3628  = ev_in->data.args.ippkt_3603.eth_2_3588;
      uint8_t ip_0_3629  = ev_in->data.args.ippkt_3603.ip_0_3589;
      uint8_t ip_1_3630  = ev_in->data.args.ippkt_3603.ip_1_3590;
      uint8_t ip_2_3631  = ev_in->data.args.ippkt_3603.ip_2_3591;
      uint8_t ip_3_3632  = ev_in->data.args.ippkt_3603.ip_3_3592;
      uint16_t ip_4_3633  = ev_in->data.args.ippkt_3603.ip_4_3593;
      uint16_t ip_5_3634  = ev_in->data.args.ippkt_3603.ip_5_3594;
      uint8_t ip_6_3635  = ev_in->data.args.ippkt_3603.ip_6_3595;
      uint16_t ip_7_3636  = ev_in->data.args.ippkt_3603.ip_7_3596;
      uint8_t ip_8_3637  = ev_in->data.args.ippkt_3603.ip_8_3597;
      uint8_t ip_9_3638  = ev_in->data.args.ippkt_3603.ip_9_3598;
      uint16_t ip_10_3639  = ev_in->data.args.ippkt_3603.ip_10_3599;
      uint32_t ip_11_3640  = ev_in->data.args.ippkt_3603.ip_11_3600;
      uint32_t ip_12_3641  = ev_in->data.args.ippkt_3603.ip_12_3601;
      event_t this  = mk_ippkt(eth_0_3626, eth_1_3627, eth_2_3628, ip_0_3629, ip_1_3630, ip_2_3631, ip_3_3632, ip_4_3633, ip_5_3634, ip_6_3635, ip_7_3636, ip_8_3637, ip_9_3638, ip_10_3639, ip_11_3640, ip_12_3641);
      uint16_t fixed_10_3666  = checksum16_0(ip_0_3629, ip_1_3630, ip_2_3631, ip_3_3632, ip_4_3633, ip_5_3634, ip_6_3635, ip_7_3636, ip_8_3637, ip_9_3638, 0, ip_11_3640, ip_12_3641);
      out_event_t tmp_3943  = {.ev = mk_ippkt(eth_0_3626, eth_1_3627, eth_2_3628, ip_0_3629, ip_1_3630, ip_2_3631, ip_3_3632, ip_4_3633, ip_5_3634, ip_6_3635, ip_7_3636, ip_8_3637, ip_9_3638, fixed_10_3666, ip_11_3640, ip_12_3641), .port = ev_in->meta.in_port};
      out_events[n] = tmp_3943;
      n = n + 1;
      break;
    }
    case 2: {
      uint64_t eth_0_3669  = ev_in->data.args.other_3608.eth_0_3604;
      uint64_t eth_1_3670  = ev_in->data.args.other_3608.eth_1_3605;
      uint16_t eth_2_3671  = ev_in->data.args.other_3608.eth_2_3606;
      event_t this  = mk_other(eth_0_3669, eth_1_3670, eth_2_3671);
      out_event_t tmp_3944  = {.ev = this, .port = ev_in->meta.in_port};
      out_events[n] = tmp_3944;
      n = n + 1;
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

