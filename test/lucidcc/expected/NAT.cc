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
uint64_t hash_64(uint32_t seed , uint8_t*  buf , uint32_t len_bits ){
  if (seed == 1) {
    // the identity hash: the low 64 bits of the bit string (the caller keeps
    // the low <size> bits)
    uint64_t v = 0;
    for (uint32_t i = len_bits > 64 ? len_bits - 64 : 0; i < len_bits; i++)
      v = (v << 1) | ((buf[i >> 3] >> (7 - (i & 7))) & 1);
    return v;
  }
  // placeholder hash (a byte sum of the zero-padded bit string)
  uint64_t h = seed;
  for (uint32_t i = 0; i < (len_bits + 7) / 8; i++) h += buf[i];
  return h;
}
uint8_t hash_args_0(uint32_t seed , uint32_t f0 , uint32_t f1 ){
  uint8_t buf[8] = {0};
  put_bits(buf, 0, (uint64_t)f0, 32);
  put_bits(buf, 32, (uint64_t)f1, 32);
  return hash_64((uint32_t)seed, buf, 64) & 0xfULL;
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
uint16_t inside_packet_tag  = 1;
uint16_t inside_continue_tag  = 2;
uint16_t outside_packet_tag  = 3;
uint16_t outside_continue_tag  = 4;
uint16_t add_to_nat_tag  = 5;
typedef struct {
  uint16_t tag;
  union {
    struct {
      uint32_t src_ip_4066;
      uint32_t src_port_4067;
    } inside_packet_4068;
    struct {
      uint32_t src_port_4069;
    } inside_continue_4070;
    struct {
      uint32_t dst_port_4071;
    } outside_packet_4072;
    struct {
      uint32_t dst_ip_4073;
      uint32_t dst_port_4074;
    } outside_continue_4075;
    struct {
      uint32_t src_ip_4076;
      uint32_t src_port_4077;
    } add_to_nat_4078;
  } args;
} event_variant_t;
event_variant_t inside_packet_4068(uint32_t src_ip_4066 , uint32_t src_port_4067 ){
  event_variant_t ev  = {0};
  ev.args.inside_packet_4068.src_ip_4066 = src_ip_4066;
  ev.args.inside_packet_4068.src_port_4067 = src_port_4067;
  ev.tag = inside_packet_tag;
  return ev;
}
event_variant_t inside_continue_4070(uint32_t src_port_4069 ){
  event_variant_t ev  = {0};
  ev.args.inside_continue_4070.src_port_4069 = src_port_4069;
  ev.tag = inside_continue_tag;
  return ev;
}
event_variant_t outside_packet_4072(uint32_t dst_port_4071 ){
  event_variant_t ev  = {0};
  ev.args.outside_packet_4072.dst_port_4071 = dst_port_4071;
  ev.tag = outside_packet_tag;
  return ev;
}
event_variant_t outside_continue_4075(uint32_t dst_ip_4073 , uint32_t dst_port_4074 ){
  event_variant_t ev  = {0};
  ev.args.outside_continue_4075.dst_ip_4073 = dst_ip_4073;
  ev.args.outside_continue_4075.dst_port_4074 = dst_port_4074;
  ev.tag = outside_continue_tag;
  return ev;
}
event_variant_t add_to_nat_4078(uint32_t src_ip_4076 , uint32_t src_port_4077 ){
  event_variant_t ev  = {0};
  ev.args.add_to_nat_4078.src_ip_4076 = src_ip_4076;
  ev.args.add_to_nat_4078.src_port_4077 = src_port_4077;
  ev.tag = add_to_nat_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_inside_packet(uint32_t src_ip_4066 , uint32_t src_port_4067 ){
  event_t tmp_4360  = {.meta = {.len = 8, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = inside_packet_4068(src_ip_4066, src_port_4067)};
  return tmp_4360;
}
event_t mk_inside_continue(uint32_t src_port_4069 ){
  event_t tmp_4361  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = inside_continue_4070(src_port_4069)};
  return tmp_4361;
}
event_t mk_outside_packet(uint32_t dst_port_4071 ){
  event_t tmp_4362  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = outside_packet_4072(dst_port_4071)};
  return tmp_4362;
}
event_t mk_outside_continue(uint32_t dst_ip_4073 , uint32_t dst_port_4074 ){
  event_t tmp_4363  = {.meta = {.len = 8, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = outside_continue_4075(dst_ip_4073, dst_port_4074)};
  return tmp_4363;
}
event_t mk_add_to_nat(uint32_t src_ip_4076 , uint32_t src_port_4077 ){
  event_t tmp_4364  = {.meta = {.len = 8, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = add_to_nat_4078(src_ip_4076, src_port_4077)};
  return tmp_4364;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
uint32_t nat_to_ip_4064 [16] = {0};
uint32_t Array_update_complex_nat_to_ip_4064_set_set_memop_32_bit(uint32_t _idx , uint32_t new_val_set_memop_32_bit , uint32_t unused_set_memop_32_bit ){
  uint32_t cell1  = nat_to_ip_4064[(((uint32_t)(_idx)) % 16)];
  uint32_t cell2  = 0;
  uint32_t ret  = 0;
  if (true) {
    cell1 = new_val_set_memop_32_bit;
  }
  nat_to_ip_4064[(((uint32_t)(_idx)) % 16)] = cell1;
  return ret;
}
uint32_t Array_update_complex_nat_to_ip_4064_get_get_memop_32_bit(uint32_t _idx , uint32_t unused1_get_memop_32_bit , uint32_t unused2_get_memop_32_bit ){
  uint32_t cell1  = nat_to_ip_4064[(((uint32_t)(_idx)) % 16)];
  uint32_t cell2  = 0;
  uint32_t ret  = 0;
  if (true) {
    ret = nat_to_ip_4064[(((uint32_t)(_idx)) % 16)];
  }
  nat_to_ip_4064[(((uint32_t)(_idx)) % 16)] = cell1;
  return ret;
}
uint32_t nat_to_port_4065 [16] = {0};
uint32_t Array_update_complex_nat_to_port_4065_set_set_memop_32_bit(uint32_t _idx , uint32_t new_val_set_memop_32_bit , uint32_t unused_set_memop_32_bit ){
  uint32_t cell1  = nat_to_port_4065[(((uint32_t)(_idx)) % 16)];
  uint32_t cell2  = 0;
  uint32_t ret  = 0;
  if (true) {
    cell1 = new_val_set_memop_32_bit;
  }
  nat_to_port_4065[(((uint32_t)(_idx)) % 16)] = cell1;
  return ret;
}
uint32_t Array_update_complex_nat_to_port_4065_get_get_memop_32_bit(uint32_t _idx , uint32_t unused1_get_memop_32_bit , uint32_t unused2_get_memop_32_bit ){
  uint32_t cell1  = nat_to_port_4065[(((uint32_t)(_idx)) % 16)];
  uint32_t cell2  = 0;
  uint32_t ret  = 0;
  if (true) {
    ret = nat_to_port_4065[(((uint32_t)(_idx)) % 16)];
  }
  nat_to_port_4065[(((uint32_t)(_idx)) % 16)] = cell1;
  return ret;
}
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
          uint32_t src_ip_4066  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t src_port_4067  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_inside_packet(src_ip_4066, src_port_4067);
          return packet->cursor <= packet->end;
          break;
        }
        case 2: {
          uint32_t src_port_4069  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_inside_continue(src_port_4069);
          return packet->cursor <= packet->end;
          break;
        }
        case 3: {
          uint32_t dst_port_4071  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_outside_packet(dst_port_4071);
          return packet->cursor <= packet->end;
          break;
        }
        case 4: {
          uint32_t dst_ip_4073  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t dst_port_4074  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_outside_continue(dst_ip_4073, dst_port_4074);
          return packet->cursor <= packet->end;
          break;
        }
        case 5: {
          uint32_t src_ip_4076  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t src_port_4077  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_add_to_nat(src_ip_4076, src_port_4077);
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
      uint32_t src_ip_4066  = ev_out->data.args.inside_packet_4068.src_ip_4066;
      uint32_t src_port_4067  = ev_out->data.args.inside_packet_4068.src_port_4067;
      write_bits(buf_out, ((uint64_t)(src_port_4067)), 32);
      write_bits(buf_out, ((uint64_t)(src_ip_4066)), 32);
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
      uint32_t src_port_4069  = ev_out->data.args.inside_continue_4070.src_port_4069;
      write_bits(buf_out, ((uint64_t)(src_port_4069)), 32);
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
      uint32_t dst_port_4071  = ev_out->data.args.outside_packet_4072.dst_port_4071;
      write_bits(buf_out, ((uint64_t)(dst_port_4071)), 32);
      if (ev_out->meta.is_packet == 0) {
        write_bits(buf_out, ((uint64_t)(3)), 16);
        write_bits(buf_out, ((uint64_t)(666)), 16);
        write_bits(buf_out, ((uint64_t)(2)), 48);
        write_bits(buf_out, ((uint64_t)(1)), 48);
      }
      return ;
      break;
    }
    case 4: {
      uint32_t dst_ip_4073  = ev_out->data.args.outside_continue_4075.dst_ip_4073;
      uint32_t dst_port_4074  = ev_out->data.args.outside_continue_4075.dst_port_4074;
      write_bits(buf_out, ((uint64_t)(dst_port_4074)), 32);
      write_bits(buf_out, ((uint64_t)(dst_ip_4073)), 32);
      if (ev_out->meta.is_packet == 0) {
        write_bits(buf_out, ((uint64_t)(4)), 16);
        write_bits(buf_out, ((uint64_t)(666)), 16);
        write_bits(buf_out, ((uint64_t)(2)), 48);
        write_bits(buf_out, ((uint64_t)(1)), 48);
      }
      return ;
      break;
    }
    case 5: {
      uint32_t src_ip_4076  = ev_out->data.args.add_to_nat_4078.src_ip_4076;
      uint32_t src_port_4077  = ev_out->data.args.add_to_nat_4078.src_port_4077;
      write_bits(buf_out, ((uint64_t)(src_port_4077)), 32);
      write_bits(buf_out, ((uint64_t)(src_ip_4076)), 32);
      if (ev_out->meta.is_packet == 0) {
        write_bits(buf_out, ((uint64_t)(5)), 16);
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
      uint32_t src_port_4079  = ev_in->data.args.inside_continue_4070.src_port_4069;
      event_t this  = mk_inside_continue(src_port_4079);
      break;
    }
    case 4: {
      uint32_t dst_ip_4080  = ev_in->data.args.outside_continue_4075.dst_ip_4073;
      uint32_t dst_port_4081  = ev_in->data.args.outside_continue_4075.dst_port_4074;
      event_t this  = mk_outside_continue(dst_ip_4080, dst_port_4081);
      break;
    }
    case 5: {
      uint32_t src_ip_4082  = ev_in->data.args.add_to_nat_4078.src_ip_4076;
      uint32_t src_port_4083  = ev_in->data.args.add_to_nat_4078.src_port_4077;
      event_t this  = mk_add_to_nat(src_ip_4082, src_port_4083);
      uint32_t NAT_port_4084  = ((uint32_t)(hash_args_0(1, src_ip_4082, src_port_4083)));
      printf("Mapped (ip: %u, port: %u) to port %u\n", src_ip_4082, src_port_4083, NAT_port_4084);
      Array_update_complex_nat_to_ip_4064_set_set_memop_32_bit(NAT_port_4084, src_ip_4082, 0);
      Array_update_complex_nat_to_port_4065_set_set_memop_32_bit(NAT_port_4084, src_port_4083, 0);
      out_event_t tmp_4365  = {.ev = mk_inside_continue(NAT_port_4084), .port = 4294967295};
      out_events[n] = tmp_4365;
      n = n + 1;
      break;
    }
    case 1: {
      uint32_t src_ip_4085  = ev_in->data.args.inside_packet_4068.src_ip_4066;
      uint32_t src_port_4086  = ev_in->data.args.inside_packet_4068.src_port_4067;
      event_t this  = mk_inside_packet(src_ip_4085, src_port_4086);
      uint8_t ret_4088  = false;
      uint32_t idx_4089  = ((uint32_t)(hash_args_0(1, src_ip_4085, src_port_4086)));
      uint32_t ip_4090  = Array_update_complex_nat_to_ip_4064_get_get_memop_32_bit(idx_4089, 0, 0);
      uint32_t port_4091  = Array_update_complex_nat_to_port_4065_get_get_memop_32_bit(idx_4089, 0, 0);
      if (ip_4090 == src_ip_4085) {
        if (port_4091 == src_port_4086) {
          ret_4088 = true;
        }
      }
      if (ret_4088 == true) {
        uint32_t NAT_port_4093  = ((uint32_t)(hash_args_0(1, src_ip_4085, src_port_4086)));
        printf("IP already in NAT, maps to port %u\n", NAT_port_4093);
        out_event_t tmp_4366  = {.ev = mk_inside_continue(NAT_port_4093), .port = 4294967295};
        out_events[n] = tmp_4366;
        n = n + 1;
      }else {
        printf("Adding to NAT\n");
        out_event_t tmp_4367  = {.ev = mk_add_to_nat(src_ip_4085, src_port_4086), .port = 4294967295};
        out_events[n] = tmp_4367;
        n = n + 1;
      }
      break;
    }
    case 3: {
      uint32_t dst_port_4094  = ev_in->data.args.outside_packet_4072.dst_port_4071;
      event_t this  = mk_outside_packet(dst_port_4094);
      uint32_t ip_4095  = Array_update_complex_nat_to_ip_4064_get_get_memop_32_bit(dst_port_4094, 0, 0);
      uint32_t port_4096  = Array_update_complex_nat_to_port_4065_get_get_memop_32_bit(dst_port_4094, 0, 0);
      printf("Mapped port %u to (ip: %u, port: %u)\n", dst_port_4094, ip_4095, port_4096);
      if (ip_4095 == 0) {
        printf("dropped\n");
      }else {
        out_event_t tmp_4368  = {.ev = mk_outside_continue(ip_4095, port_4096), .port = 4294967295};
        out_events[n] = tmp_4368;
        n = n + 1;
      }
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

