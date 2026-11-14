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
uint16_t ethhdr_tag  = 1;
typedef struct {
  uint16_t tag;
  union {
    struct {
      uint64_t dst_628;
      uint64_t src_629;
      uint16_t ety_630;
    } ethhdr_631;
  } args;
} event_variant_t;
event_variant_t ethhdr_631(uint64_t dst_628 , uint64_t src_629 , uint16_t ety_630 ){
  event_variant_t ev  = {0};
  ev.args.ethhdr_631.dst_628 = dst_628;
  ev.args.ethhdr_631.src_629 = src_629;
  ev.args.ethhdr_631.ety_630 = ety_630;
  ev.tag = ethhdr_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_ethhdr(uint64_t dst_628 , uint64_t src_629 , uint16_t ety_630 ){
  event_t tmp_666  = {.meta = {.len = 14, .is_packet = 1, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = ethhdr_631(dst_628, src_629, ety_630)};
  return tmp_666;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
uint8_t parse_event(packet_t*  pkt , event_t*  next_event , uint32_t ingress_port ){
  uint64_t dst_628  = ((uint64_t)(read_bits(pkt, 48))) & 281474976710655;
  uint64_t src_629  = ((uint64_t)(read_bits(pkt, 48))) & 281474976710655;
  uint16_t ety_630  = ((uint16_t)(read_bits(pkt, 16)));
  (*(next_event)) = mk_ethhdr(dst_628, src_629, ety_630);
  return pkt->cursor <= pkt->end;
}
void deparse_event(event_t*  ev_out , packet_t*  buf_out ){
  switch (ev_out->data.tag) {
    case 1: {
      uint64_t dst_628  = ev_out->data.args.ethhdr_631.dst_628;
      uint64_t src_629  = ev_out->data.args.ethhdr_631.src_629;
      uint16_t ety_630  = ev_out->data.args.ethhdr_631.ety_630;
      write_bits(buf_out, ((uint64_t)(ety_630)), 16);
      write_bits(buf_out, ((uint64_t)(src_629)), 48);
      write_bits(buf_out, ((uint64_t)(dst_628)), 48);
      if (ev_out->meta.is_packet == 0) {
        write_bits(buf_out, ((uint64_t)(1)), 16);
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
      uint64_t dst_632  = ev_in->data.args.ethhdr_631.dst_628;
      uint64_t src_633  = ev_in->data.args.ethhdr_631.src_629;
      uint16_t ety_634  = ev_in->data.args.ethhdr_631.ety_630;
      event_t this  = mk_ethhdr(dst_632, src_633, ety_634);
      out_event_t tmp_667  = {.ev = mk_ethhdr(dst_632, src_633, ety_634), .port = ev_in->meta.in_port};
      out_events[n] = tmp_667;
      n = n + 1;
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

