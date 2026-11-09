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
uint16_t BloomFilter_clear_all_bf1_tag  = 1;
uint16_t BloomFilter_clear_helper_bf1_tag  = 2;
uint16_t allowed_tag  = 3;
uint16_t denied_tag  = 4;
uint16_t in_tag  = 5;
typedef struct {
  uint16_t tag;
  union {
    struct {
      
    } BloomFilter_clear_all_bf1_5910;
    struct {
      uint8_t idx_5911;
    } BloomFilter_clear_helper_bf1_5912;
    struct {
      uint32_t x_5917;
    } allowed_5918;
    struct {
      uint32_t x_5919;
    } denied_5920;
    struct {
      uint8_t add_5921;
      uint8_t pad_5922;
      uint32_t x_5923;
    } in_5924;
  } args;
} event_variant_t;
event_variant_t BloomFilter_clear_all_bf1_5910(){
  event_variant_t ev  = {0};
  
  ev.tag = BloomFilter_clear_all_bf1_tag;
  return ev;
}
event_variant_t BloomFilter_clear_helper_bf1_5912(uint8_t idx_5911 ){
  event_variant_t ev  = {0};
  ev.args.BloomFilter_clear_helper_bf1_5912.idx_5911 = idx_5911;
  ev.tag = BloomFilter_clear_helper_bf1_tag;
  return ev;
}
event_variant_t allowed_5918(uint32_t x_5917 ){
  event_variant_t ev  = {0};
  ev.args.allowed_5918.x_5917 = x_5917;
  ev.tag = allowed_tag;
  return ev;
}
event_variant_t denied_5920(uint32_t x_5919 ){
  event_variant_t ev  = {0};
  ev.args.denied_5920.x_5919 = x_5919;
  ev.tag = denied_tag;
  return ev;
}
event_variant_t in_5924(uint8_t add_5921 , uint8_t pad_5922 , uint32_t x_5923 ){
  event_variant_t ev  = {0};
  ev.args.in_5924.add_5921 = add_5921;
  ev.args.in_5924.pad_5922 = pad_5922;
  ev.args.in_5924.x_5923 = x_5923;
  ev.tag = in_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_BloomFilter_clear_all_bf1(){
  event_t tmp_6377  = {.meta = {.len = 0, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = BloomFilter_clear_all_bf1_5910()};
  return tmp_6377;
}
event_t mk_BloomFilter_clear_helper_bf1(uint8_t idx_5911 ){
  event_t tmp_6378  = {.meta = {.len = 1, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = BloomFilter_clear_helper_bf1_5912(idx_5911)};
  return tmp_6378;
}
event_t mk_allowed(uint32_t x_5917 ){
  event_t tmp_6379  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = allowed_5918(x_5917)};
  return tmp_6379;
}
event_t mk_denied(uint32_t x_5919 ){
  event_t tmp_6380  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = denied_5920(x_5919)};
  return tmp_6380;
}
event_t mk_in(uint8_t add_5921 , uint8_t pad_5922 , uint32_t x_5923 ){
  event_t tmp_6381  = {.meta = {.len = 6, .is_packet = 1, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = in_5924(add_5921, pad_5922, x_5923)};
  return tmp_6381;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
uint8_t bf1_0_0_5913 [256] = {0};
uint8_t Array_update_complex_bf1_0_0_5913_set_set_memop_8_bit(uint8_t _idx , uint8_t new_val_set_memop_8_bit , uint8_t unused_set_memop_8_bit ){
  uint8_t cell1  = bf1_0_0_5913[(((uint32_t)(_idx)) % 256)];
  uint8_t cell2  = 0;
  uint8_t ret  = 0;
  if (true) {
    cell1 = new_val_set_memop_8_bit;
  }
  bf1_0_0_5913[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint8_t Array_update_complex_bf1_0_0_5913_get_get_memop_8_bit(uint8_t _idx , uint8_t unused1_get_memop_8_bit , uint8_t unused2_get_memop_8_bit ){
  uint8_t cell1  = bf1_0_0_5913[(((uint32_t)(_idx)) % 256)];
  uint8_t cell2  = 0;
  uint8_t ret  = 0;
  if (true) {
    ret = bf1_0_0_5913[(((uint32_t)(_idx)) % 256)];
  }
  bf1_0_0_5913[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint8_t bf1_0_1_5914 [256] = {0};
uint8_t Array_update_complex_bf1_0_1_5914_set_set_memop_8_bit(uint8_t _idx , uint8_t new_val_set_memop_8_bit , uint8_t unused_set_memop_8_bit ){
  uint8_t cell1  = bf1_0_1_5914[(((uint32_t)(_idx)) % 256)];
  uint8_t cell2  = 0;
  uint8_t ret  = 0;
  if (true) {
    cell1 = new_val_set_memop_8_bit;
  }
  bf1_0_1_5914[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint8_t Array_update_complex_bf1_0_1_5914_get_get_memop_8_bit(uint8_t _idx , uint8_t unused1_get_memop_8_bit , uint8_t unused2_get_memop_8_bit ){
  uint8_t cell1  = bf1_0_1_5914[(((uint32_t)(_idx)) % 256)];
  uint8_t cell2  = 0;
  uint8_t ret  = 0;
  if (true) {
    ret = bf1_0_1_5914[(((uint32_t)(_idx)) % 256)];
  }
  bf1_0_1_5914[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint8_t bf1_0_2_5915 [256] = {0};
uint8_t Array_update_complex_bf1_0_2_5915_set_set_memop_8_bit(uint8_t _idx , uint8_t new_val_set_memop_8_bit , uint8_t unused_set_memop_8_bit ){
  uint8_t cell1  = bf1_0_2_5915[(((uint32_t)(_idx)) % 256)];
  uint8_t cell2  = 0;
  uint8_t ret  = 0;
  if (true) {
    cell1 = new_val_set_memop_8_bit;
  }
  bf1_0_2_5915[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint8_t Array_update_complex_bf1_0_2_5915_get_get_memop_8_bit(uint8_t _idx , uint8_t unused1_get_memop_8_bit , uint8_t unused2_get_memop_8_bit ){
  uint8_t cell1  = bf1_0_2_5915[(((uint32_t)(_idx)) % 256)];
  uint8_t cell2  = 0;
  uint8_t ret  = 0;
  if (true) {
    ret = bf1_0_2_5915[(((uint32_t)(_idx)) % 256)];
  }
  bf1_0_2_5915[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint8_t bf1_0_3_5916 [256] = {0};
uint8_t Array_update_complex_bf1_0_3_5916_set_set_memop_8_bit(uint8_t _idx , uint8_t new_val_set_memop_8_bit , uint8_t unused_set_memop_8_bit ){
  uint8_t cell1  = bf1_0_3_5916[(((uint32_t)(_idx)) % 256)];
  uint8_t cell2  = 0;
  uint8_t ret  = 0;
  if (true) {
    cell1 = new_val_set_memop_8_bit;
  }
  bf1_0_3_5916[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint8_t Array_update_complex_bf1_0_3_5916_get_get_memop_8_bit(uint8_t _idx , uint8_t unused1_get_memop_8_bit , uint8_t unused2_get_memop_8_bit ){
  uint8_t cell1  = bf1_0_3_5916[(((uint32_t)(_idx)) % 256)];
  uint8_t cell2  = 0;
  uint8_t ret  = 0;
  if (true) {
    ret = bf1_0_3_5916[(((uint32_t)(_idx)) % 256)];
  }
  bf1_0_3_5916[(((uint32_t)(_idx)) % 256)] = cell1;
  return ret;
}
uint8_t parse_event(packet_t*  pkt , event_t*  next_event , uint32_t ingress_port ){
  uint8_t add_5921  = ((uint8_t)(read_bits(pkt, 1)));
  uint8_t pad_5922  = ((uint8_t)(read_bits(pkt, 7))) & 127;
  uint32_t x_5923  = ((uint32_t)(read_bits(pkt, 32)));
  (*(next_event)) = mk_in(add_5921, pad_5922, x_5923);
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
      uint8_t idx_5911  = ev_out->data.args.BloomFilter_clear_helper_bf1_5912.idx_5911;
      write_bits(buf_out, ((uint64_t)(idx_5911)), 8);
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
      uint32_t x_5917  = ev_out->data.args.allowed_5918.x_5917;
      write_bits(buf_out, ((uint64_t)(x_5917)), 32);
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
      uint32_t x_5919  = ev_out->data.args.denied_5920.x_5919;
      write_bits(buf_out, ((uint64_t)(x_5919)), 32);
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
      uint8_t add_5921  = ev_out->data.args.in_5924.add_5921;
      uint8_t pad_5922  = ev_out->data.args.in_5924.pad_5922;
      uint32_t x_5923  = ev_out->data.args.in_5924.x_5923;
      write_bits(buf_out, ((uint64_t)(x_5923)), 32);
      write_bits(buf_out, ((uint64_t)(pad_5922)), 7);
      write_bits(buf_out, ((uint64_t)(add_5921)), 1);
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
      out_event_t tmp_6382  = {.ev = mk_BloomFilter_clear_helper_bf1(0), .port = 4294967295};
      out_events[n] = tmp_6382;
      n = n + 1;
      break;
    }
    case 2: {
      uint8_t idx_5925  = ev_in->data.args.BloomFilter_clear_helper_bf1_5912.idx_5911;
      event_t this  = mk_BloomFilter_clear_helper_bf1(idx_5925);
      Array_update_complex_bf1_0_0_5913_set_set_memop_8_bit(idx_5925, 0, 0);
      Array_update_complex_bf1_0_1_5914_set_set_memop_8_bit(idx_5925, 0, 0);
      Array_update_complex_bf1_0_2_5915_set_set_memop_8_bit(idx_5925, 0, 0);
      Array_update_complex_bf1_0_3_5916_set_set_memop_8_bit(idx_5925, 0, 0);
      false;
      if (idx_5925 < 255) {
        out_event_t tmp_6383  = {.ev = mk_BloomFilter_clear_helper_bf1(idx_5925 + 1), .port = 4294967295};
        out_events[n] = tmp_6383;
        n = n + 1;
      }
      break;
    }
    case 3: {
      uint32_t x_5926  = ev_in->data.args.allowed_5918.x_5917;
      event_t this  = mk_allowed(x_5926);
      printf("allowed %u\n", x_5926);
      break;
    }
    case 4: {
      uint32_t x_5927  = ev_in->data.args.denied_5920.x_5919;
      event_t this  = mk_denied(x_5927);
      printf("denied %u\n", x_5927);
      break;
    }
    case 5: {
      uint8_t add_5928  = ev_in->data.args.in_5924.add_5921;
      uint8_t pad_5929  = ev_in->data.args.in_5924.pad_5922;
      uint32_t x_5930  = ev_in->data.args.in_5924.x_5923;
      event_t this  = mk_in(add_5928, pad_5929, x_5930);
      if (add_5928) {
        Array_update_complex_bf1_0_0_5913_set_set_memop_8_bit(hash_32((uint32_t)0, (uint8_t* )&x_5930, 32), 1, 0);
        Array_update_complex_bf1_0_1_5914_set_set_memop_8_bit(hash_32((uint32_t)1, (uint8_t* )&x_5930, 32), 1, 0);
        Array_update_complex_bf1_0_2_5915_set_set_memop_8_bit(hash_32((uint32_t)2, (uint8_t* )&x_5930, 32), 1, 0);
        Array_update_complex_bf1_0_3_5916_set_set_memop_8_bit(hash_32((uint32_t)3, (uint8_t* )&x_5930, 32), 1, 0);
        false;
      }else {
        uint8_t x_5933  = Array_update_complex_bf1_0_0_5913_get_get_memop_8_bit(hash_32((uint32_t)0, (uint8_t* )&x_5930, 32), 0, 0);
        uint8_t x_5934  = Array_update_complex_bf1_0_1_5914_get_get_memop_8_bit(hash_32((uint32_t)1, (uint8_t* )&x_5930, 32), 0, 0);
        uint8_t x_5935  = Array_update_complex_bf1_0_2_5915_get_get_memop_8_bit(hash_32((uint32_t)2, (uint8_t* )&x_5930, 32), 0, 0);
        uint8_t x_5936  = Array_update_complex_bf1_0_3_5916_get_get_memop_8_bit(hash_32((uint32_t)3, (uint8_t* )&x_5930, 32), 0, 0);
        if ((((x_5933 == 1) && (x_5934 == 1)) && (x_5935 == 1)) && (x_5936 == 1)) {
          out_event_t tmp_6384  = {.ev = mk_allowed(x_5930), .port = 4294967295};
          out_events[n] = tmp_6384;
          n = n + 1;
        }else {
          out_event_t tmp_6385  = {.ev = mk_denied(x_5930), .port = 4294967295};
          out_events[n] = tmp_6385;
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

