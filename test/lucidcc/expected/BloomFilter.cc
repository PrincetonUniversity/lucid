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
uint8_t hash_args_0(uint32_t seed , uint32_t f0 ){
  uint8_t buf[4] = {0};
  put_bits(buf, 0, (uint64_t)f0, 32);
  return hash_64((uint32_t)seed, buf, 32) & 0xffULL;
}
typedef struct {
  uint16_t len;
  uint8_t is_packet;
  uint8_t has_payload;
  uint64_t timestamp;
  uint32_t in_port;
} event_meta;
uint16_t BloomFilter_clear_all_bf1_tag  = 1;
uint16_t BloomFilter_clear_helper_bf1_tag  = 2;
uint16_t allowed_tag  = 3;
uint16_t denied_tag  = 4;
uint16_t in_tag  = 5;
typedef struct {
  uint16_t tag;
  union {
    struct {
      
    } BloomFilter_clear_all_bf1_5950;
    struct {
      uint8_t idx_5951;
    } BloomFilter_clear_helper_bf1_5952;
    struct {
      uint32_t x_5957;
    } allowed_5958;
    struct {
      uint32_t x_5959;
    } denied_5960;
    struct {
      uint8_t add_5961;
      uint8_t pad_5962;
      uint32_t x_5963;
    } in_5964;
  } args;
} event_variant_t;
event_variant_t BloomFilter_clear_all_bf1_5950(){
  event_variant_t ev  = {0};
  
  ev.tag = BloomFilter_clear_all_bf1_tag;
  return ev;
}
event_variant_t BloomFilter_clear_helper_bf1_5952(uint8_t idx_5951 ){
  event_variant_t ev  = {0};
  ev.args.BloomFilter_clear_helper_bf1_5952.idx_5951 = idx_5951;
  ev.tag = BloomFilter_clear_helper_bf1_tag;
  return ev;
}
event_variant_t allowed_5958(uint32_t x_5957 ){
  event_variant_t ev  = {0};
  ev.args.allowed_5958.x_5957 = x_5957;
  ev.tag = allowed_tag;
  return ev;
}
event_variant_t denied_5960(uint32_t x_5959 ){
  event_variant_t ev  = {0};
  ev.args.denied_5960.x_5959 = x_5959;
  ev.tag = denied_tag;
  return ev;
}
event_variant_t in_5964(uint8_t add_5961 , uint8_t pad_5962 , uint32_t x_5963 ){
  event_variant_t ev  = {0};
  ev.args.in_5964.add_5961 = add_5961;
  ev.args.in_5964.pad_5962 = pad_5962;
  ev.args.in_5964.x_5963 = x_5963;
  ev.tag = in_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_BloomFilter_clear_all_bf1(){
  event_t tmp_6417  = {.meta = {.len = 0, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = BloomFilter_clear_all_bf1_5950()};
  return tmp_6417;
}
event_t mk_BloomFilter_clear_helper_bf1(uint8_t idx_5951 ){
  event_t tmp_6418  = {.meta = {.len = 1, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = BloomFilter_clear_helper_bf1_5952(idx_5951)};
  return tmp_6418;
}
event_t mk_allowed(uint32_t x_5957 ){
  event_t tmp_6419  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = allowed_5958(x_5957)};
  return tmp_6419;
}
event_t mk_denied(uint32_t x_5959 ){
  event_t tmp_6420  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = denied_5960(x_5959)};
  return tmp_6420;
}
event_t mk_in(uint8_t add_5961 , uint8_t pad_5962 , uint32_t x_5963 ){
  event_t tmp_6421  = {.meta = {.len = 6, .is_packet = 1, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = in_5964(add_5961, pad_5962, x_5963)};
  return tmp_6421;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
uint8_t bf1_0_0_5953 [256] = {0};
uint8_t Array_update_complex_bf1_0_0_5953_set_set_memop_8_bit(uint8_t _idx , uint8_t new_val_set_memop_8_bit , uint8_t unused_set_memop_8_bit ){
  uint8_t cell1  = bf1_0_0_5953[(((uint32_t)(_idx)) % 256)];
  uint8_t cell2  = 0;
  uint8_t ret  = 0;
  if (true) {
    cell1 = new_val_set_memop_8_bit;
  }
  bf1_0_0_5953[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint8_t Array_update_complex_bf1_0_0_5953_get_get_memop_8_bit(uint8_t _idx , uint8_t unused1_get_memop_8_bit , uint8_t unused2_get_memop_8_bit ){
  uint8_t cell1  = bf1_0_0_5953[(((uint32_t)(_idx)) % 256)];
  uint8_t cell2  = 0;
  uint8_t ret  = 0;
  if (true) {
    ret = bf1_0_0_5953[(((uint32_t)(_idx)) % 256)];
  }
  bf1_0_0_5953[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint8_t bf1_0_1_5954 [256] = {0};
uint8_t Array_update_complex_bf1_0_1_5954_set_set_memop_8_bit(uint8_t _idx , uint8_t new_val_set_memop_8_bit , uint8_t unused_set_memop_8_bit ){
  uint8_t cell1  = bf1_0_1_5954[(((uint32_t)(_idx)) % 256)];
  uint8_t cell2  = 0;
  uint8_t ret  = 0;
  if (true) {
    cell1 = new_val_set_memop_8_bit;
  }
  bf1_0_1_5954[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint8_t Array_update_complex_bf1_0_1_5954_get_get_memop_8_bit(uint8_t _idx , uint8_t unused1_get_memop_8_bit , uint8_t unused2_get_memop_8_bit ){
  uint8_t cell1  = bf1_0_1_5954[(((uint32_t)(_idx)) % 256)];
  uint8_t cell2  = 0;
  uint8_t ret  = 0;
  if (true) {
    ret = bf1_0_1_5954[(((uint32_t)(_idx)) % 256)];
  }
  bf1_0_1_5954[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint8_t bf1_0_2_5955 [256] = {0};
uint8_t Array_update_complex_bf1_0_2_5955_set_set_memop_8_bit(uint8_t _idx , uint8_t new_val_set_memop_8_bit , uint8_t unused_set_memop_8_bit ){
  uint8_t cell1  = bf1_0_2_5955[(((uint32_t)(_idx)) % 256)];
  uint8_t cell2  = 0;
  uint8_t ret  = 0;
  if (true) {
    cell1 = new_val_set_memop_8_bit;
  }
  bf1_0_2_5955[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint8_t Array_update_complex_bf1_0_2_5955_get_get_memop_8_bit(uint8_t _idx , uint8_t unused1_get_memop_8_bit , uint8_t unused2_get_memop_8_bit ){
  uint8_t cell1  = bf1_0_2_5955[(((uint32_t)(_idx)) % 256)];
  uint8_t cell2  = 0;
  uint8_t ret  = 0;
  if (true) {
    ret = bf1_0_2_5955[(((uint32_t)(_idx)) % 256)];
  }
  bf1_0_2_5955[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint8_t bf1_0_3_5956 [256] = {0};
uint8_t Array_update_complex_bf1_0_3_5956_set_set_memop_8_bit(uint8_t _idx , uint8_t new_val_set_memop_8_bit , uint8_t unused_set_memop_8_bit ){
  uint8_t cell1  = bf1_0_3_5956[(((uint32_t)(_idx)) % 256)];
  uint8_t cell2  = 0;
  uint8_t ret  = 0;
  if (true) {
    cell1 = new_val_set_memop_8_bit;
  }
  bf1_0_3_5956[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint8_t Array_update_complex_bf1_0_3_5956_get_get_memop_8_bit(uint8_t _idx , uint8_t unused1_get_memop_8_bit , uint8_t unused2_get_memop_8_bit ){
  uint8_t cell1  = bf1_0_3_5956[(((uint32_t)(_idx)) % 256)];
  uint8_t cell2  = 0;
  uint8_t ret  = 0;
  if (true) {
    ret = bf1_0_3_5956[(((uint32_t)(_idx)) % 256)];
  }
  bf1_0_3_5956[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint8_t parse_event(packet_t*  pkt , event_t*  next_event , uint32_t ingress_port ){
  uint8_t add_5961  = ((uint8_t)(read_bits(pkt, 1)));
  uint8_t pad_5962  = ((uint8_t)(read_bits(pkt, 7))) & 127;
  uint32_t x_5963  = ((uint32_t)(read_bits(pkt, 32)));
  (*(next_event)) = mk_in(add_5961, pad_5962, x_5963);
  return pkt->cursor <= pkt->end;
}
void deparse_event(event_t*  ev_out , packet_t*  buf_out ){
  switch (ev_out->data.tag) {
    case 1: {
      
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
      uint8_t idx_5951  = ev_out->data.args.BloomFilter_clear_helper_bf1_5952.idx_5951;
      write_bits(buf_out, ((uint64_t)(idx_5951)), 8);
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
      uint32_t x_5957  = ev_out->data.args.allowed_5958.x_5957;
      write_bits(buf_out, ((uint64_t)(x_5957)), 32);
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
      uint32_t x_5959  = ev_out->data.args.denied_5960.x_5959;
      write_bits(buf_out, ((uint64_t)(x_5959)), 32);
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
      uint8_t add_5961  = ev_out->data.args.in_5964.add_5961;
      uint8_t pad_5962  = ev_out->data.args.in_5964.pad_5962;
      uint32_t x_5963  = ev_out->data.args.in_5964.x_5963;
      write_bits(buf_out, ((uint64_t)(x_5963)), 32);
      write_bits(buf_out, ((uint64_t)(pad_5962)), 7);
      write_bits(buf_out, ((uint64_t)(add_5961)), 1);
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
    case 1: {
      
      event_t this  = mk_BloomFilter_clear_all_bf1();
      out_event_t tmp_6422  = {.ev = mk_BloomFilter_clear_helper_bf1(0), .port = 4294967295};
      out_events[n] = tmp_6422;
      n = n + 1;
      break;
    }
    case 2: {
      uint8_t idx_5965  = ev_in->data.args.BloomFilter_clear_helper_bf1_5952.idx_5951;
      event_t this  = mk_BloomFilter_clear_helper_bf1(idx_5965);
      Array_update_complex_bf1_0_0_5953_set_set_memop_8_bit(idx_5965, 0, 0);
      Array_update_complex_bf1_0_1_5954_set_set_memop_8_bit(idx_5965, 0, 0);
      Array_update_complex_bf1_0_2_5955_set_set_memop_8_bit(idx_5965, 0, 0);
      Array_update_complex_bf1_0_3_5956_set_set_memop_8_bit(idx_5965, 0, 0);
      false;
      if (idx_5965 < 255) {
        out_event_t tmp_6423  = {.ev = mk_BloomFilter_clear_helper_bf1(idx_5965 + 1), .port = 4294967295};
        out_events[n] = tmp_6423;
        n = n + 1;
      }
      break;
    }
    case 3: {
      uint32_t x_5966  = ev_in->data.args.allowed_5958.x_5957;
      event_t this  = mk_allowed(x_5966);
      printf("allowed %u\n", x_5966);
      break;
    }
    case 4: {
      uint32_t x_5967  = ev_in->data.args.denied_5960.x_5959;
      event_t this  = mk_denied(x_5967);
      printf("denied %u\n", x_5967);
      break;
    }
    case 5: {
      uint8_t add_5968  = ev_in->data.args.in_5964.add_5961;
      uint8_t pad_5969  = ev_in->data.args.in_5964.pad_5962;
      uint32_t x_5970  = ev_in->data.args.in_5964.x_5963;
      event_t this  = mk_in(add_5968, pad_5969, x_5970);
      if (add_5968) {
        Array_update_complex_bf1_0_0_5953_set_set_memop_8_bit(hash_args_0(0, x_5970), 1, 0);
        Array_update_complex_bf1_0_1_5954_set_set_memop_8_bit(hash_args_0(1, x_5970), 1, 0);
        Array_update_complex_bf1_0_2_5955_set_set_memop_8_bit(hash_args_0(2, x_5970), 1, 0);
        Array_update_complex_bf1_0_3_5956_set_set_memop_8_bit(hash_args_0(3, x_5970), 1, 0);
        false;
      }else {
        uint8_t x_5973  = Array_update_complex_bf1_0_0_5953_get_get_memop_8_bit(hash_args_0(0, x_5970), 0, 0);
        uint8_t x_5974  = Array_update_complex_bf1_0_1_5954_get_get_memop_8_bit(hash_args_0(1, x_5970), 0, 0);
        uint8_t x_5975  = Array_update_complex_bf1_0_2_5955_get_get_memop_8_bit(hash_args_0(2, x_5970), 0, 0);
        uint8_t x_5976  = Array_update_complex_bf1_0_3_5956_get_get_memop_8_bit(hash_args_0(3, x_5970), 0, 0);
        if ((((x_5973 == 1) && (x_5974 == 1)) && (x_5975 == 1)) && (x_5976 == 1)) {
          out_event_t tmp_6424  = {.ev = mk_allowed(x_5970), .port = 4294967295};
          out_events[n] = tmp_6424;
          n = n + 1;
        }else {
          out_event_t tmp_6425  = {.ev = mk_denied(x_5970), .port = 4294967295};
          out_events[n] = tmp_6425;
          n = n + 1;
        }
      }
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

