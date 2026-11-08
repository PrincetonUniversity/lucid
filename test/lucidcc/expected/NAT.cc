typedef struct {
  uint32_t _0;
  uint32_t _1;
} tuple_1;
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
uint16_t inside_packet_tag  = 1;
uint16_t inside_continue_tag  = 2;
uint16_t outside_packet_tag  = 3;
uint16_t outside_continue_tag  = 4;
uint16_t add_to_nat_tag  = 5;
typedef struct {
  uint16_t tag;
  union {
    struct {
      uint32_t src_ip_4037;
      uint32_t src_port_4038;
    } inside_packet_4039;
    struct {
      uint32_t src_port_4040;
    } inside_continue_4041;
    struct {
      uint32_t dst_port_4042;
    } outside_packet_4043;
    struct {
      uint32_t dst_ip_4044;
      uint32_t dst_port_4045;
    } outside_continue_4046;
    struct {
      uint32_t src_ip_4047;
      uint32_t src_port_4048;
    } add_to_nat_4049;
  } args;
} event_variant_t;
event_variant_t inside_packet_4039(uint32_t src_ip_4037 , uint32_t src_port_4038 ){
  event_variant_t ev  = {0};
  ev.args.inside_packet_4039.src_ip_4037 = src_ip_4037;
  ev.args.inside_packet_4039.src_port_4038 = src_port_4038;
  ev.tag = inside_packet_tag;
  return ev;
}
event_variant_t inside_continue_4041(uint32_t src_port_4040 ){
  event_variant_t ev  = {0};
  ev.args.inside_continue_4041.src_port_4040 = src_port_4040;
  ev.tag = inside_continue_tag;
  return ev;
}
event_variant_t outside_packet_4043(uint32_t dst_port_4042 ){
  event_variant_t ev  = {0};
  ev.args.outside_packet_4043.dst_port_4042 = dst_port_4042;
  ev.tag = outside_packet_tag;
  return ev;
}
event_variant_t outside_continue_4046(uint32_t dst_ip_4044 , uint32_t dst_port_4045 ){
  event_variant_t ev  = {0};
  ev.args.outside_continue_4046.dst_ip_4044 = dst_ip_4044;
  ev.args.outside_continue_4046.dst_port_4045 = dst_port_4045;
  ev.tag = outside_continue_tag;
  return ev;
}
event_variant_t add_to_nat_4049(uint32_t src_ip_4047 , uint32_t src_port_4048 ){
  event_variant_t ev  = {0};
  ev.args.add_to_nat_4049.src_ip_4047 = src_ip_4047;
  ev.args.add_to_nat_4049.src_port_4048 = src_port_4048;
  ev.tag = add_to_nat_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_inside_packet(uint32_t src_ip_4037 , uint32_t src_port_4038 ){
  event_t tmp_4331  = {.meta = {.len = 8, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = inside_packet_4039(src_ip_4037, src_port_4038)};
  return tmp_4331;
}
event_t mk_inside_continue(uint32_t src_port_4040 ){
  event_t tmp_4332  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = inside_continue_4041(src_port_4040)};
  return tmp_4332;
}
event_t mk_outside_packet(uint32_t dst_port_4042 ){
  event_t tmp_4333  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = outside_packet_4043(dst_port_4042)};
  return tmp_4333;
}
event_t mk_outside_continue(uint32_t dst_ip_4044 , uint32_t dst_port_4045 ){
  event_t tmp_4334  = {.meta = {.len = 8, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = outside_continue_4046(dst_ip_4044, dst_port_4045)};
  return tmp_4334;
}
event_t mk_add_to_nat(uint32_t src_ip_4047 , uint32_t src_port_4048 ){
  event_t tmp_4335  = {.meta = {.len = 8, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = add_to_nat_4049(src_ip_4047, src_port_4048)};
  return tmp_4335;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
uint32_t nat_to_ip_4035 [16] = {0};
uint32_t Array_update_complex_nat_to_ip_4035_set_set_memop_32_bit(uint32_t _idx , uint32_t new_val_set_memop_32_bit , uint32_t unused_set_memop_32_bit ){
  uint32_t cell1  = nat_to_ip_4035[(((uint32_t)(_idx)) % 16)];
  uint32_t cell2  = 0;
  uint32_t ret  = 0;
  if (true) {
    cell1 = new_val_set_memop_32_bit;
  }
  nat_to_ip_4035[(((uint32_t)(_idx)) % 16)] = cell1;
  return ret;
}
uint32_t Array_update_complex_nat_to_ip_4035_get_get_memop_32_bit(uint32_t _idx , uint32_t unused1_get_memop_32_bit , uint32_t unused2_get_memop_32_bit ){
  uint32_t cell1  = nat_to_ip_4035[(((uint32_t)(_idx)) % 16)];
  uint32_t cell2  = 0;
  uint32_t ret  = 0;
  if (true) {
    ret = nat_to_ip_4035[(((uint32_t)(_idx)) % 16)];
  }
  nat_to_ip_4035[(((uint32_t)(_idx)) % 16)] = cell1;
  return ret;
}
uint32_t nat_to_port_4036 [16] = {0};
uint32_t Array_update_complex_nat_to_port_4036_set_set_memop_32_bit(uint32_t _idx , uint32_t new_val_set_memop_32_bit , uint32_t unused_set_memop_32_bit ){
  uint32_t cell1  = nat_to_port_4036[(((uint32_t)(_idx)) % 16)];
  uint32_t cell2  = 0;
  uint32_t ret  = 0;
  if (true) {
    cell1 = new_val_set_memop_32_bit;
  }
  nat_to_port_4036[(((uint32_t)(_idx)) % 16)] = cell1;
  return ret;
}
uint32_t Array_update_complex_nat_to_port_4036_get_get_memop_32_bit(uint32_t _idx , uint32_t unused1_get_memop_32_bit , uint32_t unused2_get_memop_32_bit ){
  uint32_t cell1  = nat_to_port_4036[(((uint32_t)(_idx)) % 16)];
  uint32_t cell2  = 0;
  uint32_t ret  = 0;
  if (true) {
    ret = nat_to_port_4036[(((uint32_t)(_idx)) % 16)];
  }
  nat_to_port_4036[(((uint32_t)(_idx)) % 16)] = cell1;
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
          uint32_t src_ip_4037  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t src_port_4038  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_inside_packet(src_ip_4037, src_port_4038);
          return packet->cursor <= packet->end;
          break;
        }
        case 2: {
          uint32_t src_port_4040  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_inside_continue(src_port_4040);
          return packet->cursor <= packet->end;
          break;
        }
        case 3: {
          uint32_t dst_port_4042  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_outside_packet(dst_port_4042);
          return packet->cursor <= packet->end;
          break;
        }
        case 4: {
          uint32_t dst_ip_4044  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t dst_port_4045  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_outside_continue(dst_ip_4044, dst_port_4045);
          return packet->cursor <= packet->end;
          break;
        }
        case 5: {
          uint32_t src_ip_4047  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t src_port_4048  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_add_to_nat(src_ip_4047, src_port_4048);
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
      uint32_t src_ip_4037  = ev_out->data.args.inside_packet_4039.src_ip_4037;
      uint32_t src_port_4038  = ev_out->data.args.inside_packet_4039.src_port_4038;
      write_bits(buf_out, ((uint64_t)(src_port_4038)), 32);
      write_bits(buf_out, ((uint64_t)(src_ip_4037)), 32);
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
      uint32_t src_port_4040  = ev_out->data.args.inside_continue_4041.src_port_4040;
      write_bits(buf_out, ((uint64_t)(src_port_4040)), 32);
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
      uint32_t dst_port_4042  = ev_out->data.args.outside_packet_4043.dst_port_4042;
      write_bits(buf_out, ((uint64_t)(dst_port_4042)), 32);
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
      uint32_t dst_ip_4044  = ev_out->data.args.outside_continue_4046.dst_ip_4044;
      uint32_t dst_port_4045  = ev_out->data.args.outside_continue_4046.dst_port_4045;
      write_bits(buf_out, ((uint64_t)(dst_port_4045)), 32);
      write_bits(buf_out, ((uint64_t)(dst_ip_4044)), 32);
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
      uint32_t src_ip_4047  = ev_out->data.args.add_to_nat_4049.src_ip_4047;
      uint32_t src_port_4048  = ev_out->data.args.add_to_nat_4049.src_port_4048;
      write_bits(buf_out, ((uint64_t)(src_port_4048)), 32);
      write_bits(buf_out, ((uint64_t)(src_ip_4047)), 32);
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
      uint32_t src_port_4050  = ev_in->data.args.inside_continue_4041.src_port_4040;
      event_t this  = mk_inside_continue(src_port_4050);
      break;
    }
    case 4: {
      uint32_t dst_ip_4051  = ev_in->data.args.outside_continue_4046.dst_ip_4044;
      uint32_t dst_port_4052  = ev_in->data.args.outside_continue_4046.dst_port_4045;
      event_t this  = mk_outside_continue(dst_ip_4051, dst_port_4052);
      break;
    }
    case 5: {
      uint32_t src_ip_4053  = ev_in->data.args.add_to_nat_4049.src_ip_4047;
      uint32_t src_port_4054  = ev_in->data.args.add_to_nat_4049.src_port_4048;
      event_t this  = mk_add_to_nat(src_ip_4053, src_port_4054);
      tuple_1 tmp_4336  = {._0 = src_ip_4053, ._1 = src_port_4054,};
      uint32_t NAT_port_4055  = ((uint32_t)(hash_32((uint32_t)1234, (uint8_t* )&tmp_4336, 64) & 15));
      printf("Mapped (ip: %u, port: %u) to port %u\n", src_ip_4053, src_port_4054, NAT_port_4055);
      Array_update_complex_nat_to_ip_4035_set_set_memop_32_bit(NAT_port_4055, src_ip_4053, 0);
      Array_update_complex_nat_to_port_4036_set_set_memop_32_bit(NAT_port_4055, src_port_4054, 0);
      out_event_t tmp_4337  = {.ev = mk_inside_continue(NAT_port_4055), .port = 4294967295};
      out_events[n] = tmp_4337;
      n = n + 1;
      break;
    }
    case 1: {
      uint32_t src_ip_4056  = ev_in->data.args.inside_packet_4039.src_ip_4037;
      uint32_t src_port_4057  = ev_in->data.args.inside_packet_4039.src_port_4038;
      event_t this  = mk_inside_packet(src_ip_4056, src_port_4057);
      uint8_t ret_4059  = false;
      tuple_1 tmp_4338  = {._0 = src_ip_4056, ._1 = src_port_4057,};
      uint32_t idx_4060  = ((uint32_t)(hash_32((uint32_t)1234, (uint8_t* )&tmp_4338, 64) & 15));
      uint32_t ip_4061  = Array_update_complex_nat_to_ip_4035_get_get_memop_32_bit(idx_4060, 0, 0);
      uint32_t port_4062  = Array_update_complex_nat_to_port_4036_get_get_memop_32_bit(idx_4060, 0, 0);
      if (ip_4061 == src_ip_4056) {
        if (port_4062 == src_port_4057) {
          ret_4059 = true;
        }
      }
      if (ret_4059 == true) {
        tuple_1 tmp_4339  = {._0 = src_ip_4056, ._1 = src_port_4057,};
        uint32_t NAT_port_4064  = ((uint32_t)(hash_32((uint32_t)1234, (uint8_t* )&tmp_4339, 64) & 15));
        printf("IP already in NAT, maps to port %u\n", NAT_port_4064);
        out_event_t tmp_4340  = {.ev = mk_inside_continue(NAT_port_4064), .port = 4294967295};
        out_events[n] = tmp_4340;
        n = n + 1;
      }else {
        printf("Adding to NAT\n");
        out_event_t tmp_4341  = {.ev = mk_add_to_nat(src_ip_4056, src_port_4057), .port = 4294967295};
        out_events[n] = tmp_4341;
        n = n + 1;
      }
      break;
    }
    case 3: {
      uint32_t dst_port_4065  = ev_in->data.args.outside_packet_4043.dst_port_4042;
      event_t this  = mk_outside_packet(dst_port_4065);
      uint32_t ip_4066  = Array_update_complex_nat_to_ip_4035_get_get_memop_32_bit(dst_port_4065, 0, 0);
      uint32_t port_4067  = Array_update_complex_nat_to_port_4036_get_get_memop_32_bit(dst_port_4065, 0, 0);
      printf("Mapped port %u to (ip: %u, port: %u)\n", dst_port_4065, ip_4066, port_4067);
      if (ip_4066 == 0) {
        printf("dropped\n");
      }else {
        out_event_t tmp_4342  = {.ev = mk_outside_continue(ip_4066, port_4067), .port = 4294967295};
        out_events[n] = tmp_4342;
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

