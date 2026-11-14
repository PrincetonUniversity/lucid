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
uint16_t ping_tag  = 1;
uint16_t out_tag  = 2;
typedef struct {
  uint16_t tag;
  union {
    struct {
      uint32_t x_1397;
    } ping_1398;
    struct {
      uint32_t x_1399;
    } out_1400;
  } args;
} event_variant_t;
event_variant_t ping_1398(uint32_t x_1397 ){
  event_variant_t ev  = {0};
  ev.args.ping_1398.x_1397 = x_1397;
  ev.tag = ping_tag;
  return ev;
}
event_variant_t out_1400(uint32_t x_1399 ){
  event_variant_t ev  = {0};
  ev.args.out_1400.x_1399 = x_1399;
  ev.tag = out_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_ping(uint32_t x_1397 ){
  event_t tmp_1497  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = ping_1398(x_1397)};
  return tmp_1497;
}
event_t mk_out(uint32_t x_1399 ){
  event_t tmp_1498  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = out_1400(x_1399)};
  return tmp_1498;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
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
          uint32_t x_1397  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_ping(x_1397);
          return packet->cursor <= packet->end;
          break;
        }
        case 2: {
          uint32_t x_1399  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_out(x_1399);
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
      uint32_t x_1397  = ev_out->data.args.ping_1398.x_1397;
      write_bits(buf_out, ((uint64_t)(x_1397)), 32);
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
      uint32_t x_1399  = ev_out->data.args.out_1400.x_1399;
      write_bits(buf_out, ((uint64_t)(x_1399)), 32);
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
  uint32_t delay;
} out_event_t;
uint16_t handle_event(event_t*  ev_in , out_event_t out_events [64]){
  uint16_t n  = 0;
  switch (ev_in->data.tag) {
    case 1: {
      uint32_t x_1401  = ev_in->data.args.ping_1398.x_1397;
      event_t this  = mk_ping(x_1401);
      out_event_t tmp_1499  = {.ev = mk_out(100 + x_1401), .port = 1, .delay = 0};
      out_events[n] = tmp_1499;
      n = n + 1;
      out_event_t tmp_1500  = {.ev = mk_out(x_1401), .port = 1, .delay = 2500000};
      out_events[n] = tmp_1500;
      n = n + 1;
      if (x_1401 < 2) {
        out_event_t tmp_1501  = {.ev = mk_ping(x_1401 + 1), .port = 4294967295, .delay = 1000000};
        out_events[n] = tmp_1501;
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

