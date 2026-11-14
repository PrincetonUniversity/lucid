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
      uint32_t src_ip_4077;
      uint32_t src_port_4078;
    } inside_packet_4079;
    struct {
      uint32_t src_port_4080;
    } inside_continue_4081;
    struct {
      uint32_t dst_port_4082;
    } outside_packet_4083;
    struct {
      uint32_t dst_ip_4084;
      uint32_t dst_port_4085;
    } outside_continue_4086;
    struct {
      uint32_t src_ip_4087;
      uint32_t src_port_4088;
    } add_to_nat_4089;
  } args;
} event_variant_t;
event_variant_t inside_packet_4079(uint32_t src_ip_4077 , uint32_t src_port_4078 ){
  event_variant_t ev  = {0};
  ev.args.inside_packet_4079.src_ip_4077 = src_ip_4077;
  ev.args.inside_packet_4079.src_port_4078 = src_port_4078;
  ev.tag = inside_packet_tag;
  return ev;
}
event_variant_t inside_continue_4081(uint32_t src_port_4080 ){
  event_variant_t ev  = {0};
  ev.args.inside_continue_4081.src_port_4080 = src_port_4080;
  ev.tag = inside_continue_tag;
  return ev;
}
event_variant_t outside_packet_4083(uint32_t dst_port_4082 ){
  event_variant_t ev  = {0};
  ev.args.outside_packet_4083.dst_port_4082 = dst_port_4082;
  ev.tag = outside_packet_tag;
  return ev;
}
event_variant_t outside_continue_4086(uint32_t dst_ip_4084 , uint32_t dst_port_4085 ){
  event_variant_t ev  = {0};
  ev.args.outside_continue_4086.dst_ip_4084 = dst_ip_4084;
  ev.args.outside_continue_4086.dst_port_4085 = dst_port_4085;
  ev.tag = outside_continue_tag;
  return ev;
}
event_variant_t add_to_nat_4089(uint32_t src_ip_4087 , uint32_t src_port_4088 ){
  event_variant_t ev  = {0};
  ev.args.add_to_nat_4089.src_ip_4087 = src_ip_4087;
  ev.args.add_to_nat_4089.src_port_4088 = src_port_4088;
  ev.tag = add_to_nat_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_inside_packet(uint32_t src_ip_4077 , uint32_t src_port_4078 ){
  event_t tmp_4371  = {.meta = {.len = 8, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = inside_packet_4079(src_ip_4077, src_port_4078)};
  return tmp_4371;
}
event_t mk_inside_continue(uint32_t src_port_4080 ){
  event_t tmp_4372  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = inside_continue_4081(src_port_4080)};
  return tmp_4372;
}
event_t mk_outside_packet(uint32_t dst_port_4082 ){
  event_t tmp_4373  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = outside_packet_4083(dst_port_4082)};
  return tmp_4373;
}
event_t mk_outside_continue(uint32_t dst_ip_4084 , uint32_t dst_port_4085 ){
  event_t tmp_4374  = {.meta = {.len = 8, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = outside_continue_4086(dst_ip_4084, dst_port_4085)};
  return tmp_4374;
}
event_t mk_add_to_nat(uint32_t src_ip_4087 , uint32_t src_port_4088 ){
  event_t tmp_4375  = {.meta = {.len = 8, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = add_to_nat_4089(src_ip_4087, src_port_4088)};
  return tmp_4375;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
uint32_t nat_to_ip_4075 [16] = {0};
uint32_t Array_update_complex_nat_to_ip_4075_set_set_memop_32_bit(uint32_t _idx , uint32_t new_val_set_memop_32_bit , uint32_t unused_set_memop_32_bit ){
  uint32_t cell1  = nat_to_ip_4075[(((uint32_t)(_idx)) % 16)];
  uint32_t cell2  = 0;
  uint32_t ret  = 0;
  if (true) {
    cell1 = new_val_set_memop_32_bit;
  }
  nat_to_ip_4075[(((uint32_t)(_idx)) % 16)] = cell1;
  return ret;
}
uint32_t Array_update_complex_nat_to_ip_4075_get_get_memop_32_bit(uint32_t _idx , uint32_t unused1_get_memop_32_bit , uint32_t unused2_get_memop_32_bit ){
  uint32_t cell1  = nat_to_ip_4075[(((uint32_t)(_idx)) % 16)];
  uint32_t cell2  = 0;
  uint32_t ret  = 0;
  if (true) {
    ret = nat_to_ip_4075[(((uint32_t)(_idx)) % 16)];
  }
  nat_to_ip_4075[(((uint32_t)(_idx)) % 16)] = cell1;
  return ret;
}
uint32_t nat_to_port_4076 [16] = {0};
uint32_t Array_update_complex_nat_to_port_4076_set_set_memop_32_bit(uint32_t _idx , uint32_t new_val_set_memop_32_bit , uint32_t unused_set_memop_32_bit ){
  uint32_t cell1  = nat_to_port_4076[(((uint32_t)(_idx)) % 16)];
  uint32_t cell2  = 0;
  uint32_t ret  = 0;
  if (true) {
    cell1 = new_val_set_memop_32_bit;
  }
  nat_to_port_4076[(((uint32_t)(_idx)) % 16)] = cell1;
  return ret;
}
uint32_t Array_update_complex_nat_to_port_4076_get_get_memop_32_bit(uint32_t _idx , uint32_t unused1_get_memop_32_bit , uint32_t unused2_get_memop_32_bit ){
  uint32_t cell1  = nat_to_port_4076[(((uint32_t)(_idx)) % 16)];
  uint32_t cell2  = 0;
  uint32_t ret  = 0;
  if (true) {
    ret = nat_to_port_4076[(((uint32_t)(_idx)) % 16)];
  }
  nat_to_port_4076[(((uint32_t)(_idx)) % 16)] = cell1;
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
          uint32_t src_ip_4077  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t src_port_4078  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_inside_packet(src_ip_4077, src_port_4078);
          return packet->cursor <= packet->end;
          break;
        }
        case 2: {
          uint32_t src_port_4080  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_inside_continue(src_port_4080);
          return packet->cursor <= packet->end;
          break;
        }
        case 3: {
          uint32_t dst_port_4082  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_outside_packet(dst_port_4082);
          return packet->cursor <= packet->end;
          break;
        }
        case 4: {
          uint32_t dst_ip_4084  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t dst_port_4085  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_outside_continue(dst_ip_4084, dst_port_4085);
          return packet->cursor <= packet->end;
          break;
        }
        case 5: {
          uint32_t src_ip_4087  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t src_port_4088  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_add_to_nat(src_ip_4087, src_port_4088);
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
      uint32_t src_ip_4077  = ev_out->data.args.inside_packet_4079.src_ip_4077;
      uint32_t src_port_4078  = ev_out->data.args.inside_packet_4079.src_port_4078;
      write_bits(buf_out, ((uint64_t)(src_port_4078)), 32);
      write_bits(buf_out, ((uint64_t)(src_ip_4077)), 32);
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
      uint32_t src_port_4080  = ev_out->data.args.inside_continue_4081.src_port_4080;
      write_bits(buf_out, ((uint64_t)(src_port_4080)), 32);
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
      uint32_t dst_port_4082  = ev_out->data.args.outside_packet_4083.dst_port_4082;
      write_bits(buf_out, ((uint64_t)(dst_port_4082)), 32);
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
      uint32_t dst_ip_4084  = ev_out->data.args.outside_continue_4086.dst_ip_4084;
      uint32_t dst_port_4085  = ev_out->data.args.outside_continue_4086.dst_port_4085;
      write_bits(buf_out, ((uint64_t)(dst_port_4085)), 32);
      write_bits(buf_out, ((uint64_t)(dst_ip_4084)), 32);
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
      uint32_t src_ip_4087  = ev_out->data.args.add_to_nat_4089.src_ip_4087;
      uint32_t src_port_4088  = ev_out->data.args.add_to_nat_4089.src_port_4088;
      write_bits(buf_out, ((uint64_t)(src_port_4088)), 32);
      write_bits(buf_out, ((uint64_t)(src_ip_4087)), 32);
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
      uint32_t src_port_4090  = ev_in->data.args.inside_continue_4081.src_port_4080;
      event_t this  = mk_inside_continue(src_port_4090);
      break;
    }
    case 4: {
      uint32_t dst_ip_4091  = ev_in->data.args.outside_continue_4086.dst_ip_4084;
      uint32_t dst_port_4092  = ev_in->data.args.outside_continue_4086.dst_port_4085;
      event_t this  = mk_outside_continue(dst_ip_4091, dst_port_4092);
      break;
    }
    case 5: {
      uint32_t src_ip_4093  = ev_in->data.args.add_to_nat_4089.src_ip_4087;
      uint32_t src_port_4094  = ev_in->data.args.add_to_nat_4089.src_port_4088;
      event_t this  = mk_add_to_nat(src_ip_4093, src_port_4094);
      uint32_t NAT_port_4095  = ((uint32_t)(hash_args_0(1, src_ip_4093, src_port_4094)));
      printf("Mapped (ip: %u, port: %u) to port %u\n", src_ip_4093, src_port_4094, NAT_port_4095);
      Array_update_complex_nat_to_ip_4075_set_set_memop_32_bit(NAT_port_4095, src_ip_4093, 0);
      Array_update_complex_nat_to_port_4076_set_set_memop_32_bit(NAT_port_4095, src_port_4094, 0);
      out_event_t tmp_4376  = {.ev = mk_inside_continue(NAT_port_4095), .port = 4294967295};
      out_events[n] = tmp_4376;
      n = n + 1;
      break;
    }
    case 1: {
      uint32_t src_ip_4096  = ev_in->data.args.inside_packet_4079.src_ip_4077;
      uint32_t src_port_4097  = ev_in->data.args.inside_packet_4079.src_port_4078;
      event_t this  = mk_inside_packet(src_ip_4096, src_port_4097);
      uint8_t ret_4099  = false;
      uint32_t idx_4100  = ((uint32_t)(hash_args_0(1, src_ip_4096, src_port_4097)));
      uint32_t ip_4101  = Array_update_complex_nat_to_ip_4075_get_get_memop_32_bit(idx_4100, 0, 0);
      uint32_t port_4102  = Array_update_complex_nat_to_port_4076_get_get_memop_32_bit(idx_4100, 0, 0);
      if (ip_4101 == src_ip_4096) {
        if (port_4102 == src_port_4097) {
          ret_4099 = true;
        }
      }
      if (ret_4099 == true) {
        uint32_t NAT_port_4104  = ((uint32_t)(hash_args_0(1, src_ip_4096, src_port_4097)));
        printf("IP already in NAT, maps to port %u\n", NAT_port_4104);
        out_event_t tmp_4377  = {.ev = mk_inside_continue(NAT_port_4104), .port = 4294967295};
        out_events[n] = tmp_4377;
        n = n + 1;
      }else {
        printf("Adding to NAT\n");
        out_event_t tmp_4378  = {.ev = mk_add_to_nat(src_ip_4096, src_port_4097), .port = 4294967295};
        out_events[n] = tmp_4378;
        n = n + 1;
      }
      break;
    }
    case 3: {
      uint32_t dst_port_4105  = ev_in->data.args.outside_packet_4083.dst_port_4082;
      event_t this  = mk_outside_packet(dst_port_4105);
      uint32_t ip_4106  = Array_update_complex_nat_to_ip_4075_get_get_memop_32_bit(dst_port_4105, 0, 0);
      uint32_t port_4107  = Array_update_complex_nat_to_port_4076_get_get_memop_32_bit(dst_port_4105, 0, 0);
      printf("Mapped port %u to (ip: %u, port: %u)\n", dst_port_4105, ip_4106, port_4107);
      if (ip_4106 == 0) {
        printf("dropped\n");
      }else {
        out_event_t tmp_4379  = {.ev = mk_outside_continue(ip_4106, port_4107), .port = 4294967295};
        out_events[n] = tmp_4379;
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

