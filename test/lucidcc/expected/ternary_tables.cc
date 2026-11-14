typedef struct {
  uint32_t _0;
  uint8_t _1;
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
enum {tag_hit_acn_3601 = 0, tag_miss_acn_3604 = 1};

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
uint16_t query_result_tag  = 1;
uint16_t do_install_tag  = 2;
uint16_t do_update_tag  = 3;
uint16_t do_remove_tag  = 4;
uint16_t do_query_tag  = 5;
typedef struct {
  uint16_t tag;
  union {
    struct {
      uint32_t key_3606;
      uint32_t val_3607;
      uint32_t hit_3608;
    } query_result_3609;
    struct {
      uint32_t key_3610;
      uint32_t mask_3611;
      uint32_t prio_3612;
      uint32_t val_3613;
    } do_install_3614;
    struct {
      uint32_t key_3615;
      uint32_t mask_3616;
      uint32_t prio_3617;
      uint32_t val_3618;
    } do_update_3619;
    struct {
      uint32_t key_3620;
      uint32_t mask_3621;
      uint32_t prio_3622;
    } do_remove_3623;
    struct {
      uint32_t key_3624;
    } do_query_3625;
  } args;
} event_variant_t;
event_variant_t query_result_3609(uint32_t key_3606 , uint32_t val_3607 , uint32_t hit_3608 ){
  event_variant_t ev  = {0};
  ev.args.query_result_3609.key_3606 = key_3606;
  ev.args.query_result_3609.val_3607 = val_3607;
  ev.args.query_result_3609.hit_3608 = hit_3608;
  ev.tag = query_result_tag;
  return ev;
}
event_variant_t do_install_3614(uint32_t key_3610 , uint32_t mask_3611 , uint32_t prio_3612 , uint32_t val_3613 ){
  event_variant_t ev  = {0};
  ev.args.do_install_3614.key_3610 = key_3610;
  ev.args.do_install_3614.mask_3611 = mask_3611;
  ev.args.do_install_3614.prio_3612 = prio_3612;
  ev.args.do_install_3614.val_3613 = val_3613;
  ev.tag = do_install_tag;
  return ev;
}
event_variant_t do_update_3619(uint32_t key_3615 , uint32_t mask_3616 , uint32_t prio_3617 , uint32_t val_3618 ){
  event_variant_t ev  = {0};
  ev.args.do_update_3619.key_3615 = key_3615;
  ev.args.do_update_3619.mask_3616 = mask_3616;
  ev.args.do_update_3619.prio_3617 = prio_3617;
  ev.args.do_update_3619.val_3618 = val_3618;
  ev.tag = do_update_tag;
  return ev;
}
event_variant_t do_remove_3623(uint32_t key_3620 , uint32_t mask_3621 , uint32_t prio_3622 ){
  event_variant_t ev  = {0};
  ev.args.do_remove_3623.key_3620 = key_3620;
  ev.args.do_remove_3623.mask_3621 = mask_3621;
  ev.args.do_remove_3623.prio_3622 = prio_3622;
  ev.tag = do_remove_tag;
  return ev;
}
event_variant_t do_query_3625(uint32_t key_3624 ){
  event_variant_t ev  = {0};
  ev.args.do_query_3625.key_3624 = key_3624;
  ev.tag = do_query_tag;
  return ev;
}
typedef struct {
  event_meta meta;
  event_variant_t data;
} event_t;
event_t mk_query_result(uint32_t key_3606 , uint32_t val_3607 , uint32_t hit_3608 ){
  event_t tmp_3883  = {.meta = {.len = 12, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = query_result_3609(key_3606, val_3607, hit_3608)};
  return tmp_3883;
}
event_t mk_do_install(uint32_t key_3610 , uint32_t mask_3611 , uint32_t prio_3612 , uint32_t val_3613 ){
  event_t tmp_3884  = {.meta = {.len = 16, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_install_3614(key_3610, mask_3611, prio_3612, val_3613)};
  return tmp_3884;
}
event_t mk_do_update(uint32_t key_3615 , uint32_t mask_3616 , uint32_t prio_3617 , uint32_t val_3618 ){
  event_t tmp_3885  = {.meta = {.len = 16, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_update_3619(key_3615, mask_3616, prio_3617, val_3618)};
  return tmp_3885;
}
event_t mk_do_remove(uint32_t key_3620 , uint32_t mask_3621 , uint32_t prio_3622 ){
  event_t tmp_3886  = {.meta = {.len = 12, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_remove_3623(key_3620, mask_3621, prio_3622)};
  return tmp_3886;
}
event_t mk_do_query(uint32_t key_3624 ){
  event_t tmp_3887  = {.meta = {.len = 4, .is_packet = 0, .has_payload = 0, .timestamp = 0, .in_port = 0}, .data = do_query_3625(key_3624)};
  return tmp_3887;
}
uint32_t recirculation_port  = 196;
uint32_t self  = 0;
typedef tuple_1 res_t_3598;
res_t_3598 hit_acn_3601(uint32_t x_3599 , uint32_t a_3600 ){
  res_t_3598 tmp_3888  = {._0 = x_3599, ._1 = true,};
  return tmp_3888;
}
res_t_3598 miss_acn_3604(uint32_t b_3602 , uint32_t a_3603 ){
  res_t_3598 tmp_3889  = {._0 = a_3603, ._1 = false,};
  return tmp_3889;
}
typedef struct {
  uint32_t key;
  uint32_t mask;
  uint32_t prio;
  uint32_t action_tag;
  uint32_t action_arg;
} cellty_ftbl_3605;
typedef struct {
  struct {
    uint32_t action_tag;
    uint32_t action_arg;
  } _default;
  uint32_t max_idx;
  cellty_ftbl_3605 entries [4];
} ty_ftbl_3605;
ty_ftbl_3605 ftbl_3605  = {._default = {.action_tag = tag_miss_acn_3604, .action_arg = 1}, .max_idx = 0, .entries = {0}};
void warn_full_ftbl_3605(){
  static int warned = 0;
  table_full_installs++;
  if (!warned) { warned = 1; fprintf(stderr, "warning: table ftbl is full, an entry was not installed\n"); }
}
void warn_update_ftbl_3605(){
  static int warned = 0;
  if (!warned) { warned = 1; fprintf(stderr, "warning: table ftbl has no such entry to update\n"); }
}
void warn_remove_ftbl_3605(){
  static int warned = 0;
  if (!warned) { warned = 1; fprintf(stderr, "warning: table ftbl has no such entry to remove\n"); }
}
void install_ftbl_3605(uint32_t key , uint32_t mask , uint32_t prio , uint32_t action , uint32_t const_arg ){
  for (int _idx = 0; _idx < 4; _idx++) {
    if (_idx < ftbl_3605.max_idx) {
      if ((ftbl_3605.entries[_idx].prio == prio) && ((ftbl_3605.entries[_idx].mask == mask) && ((ftbl_3605.entries[_idx].key & mask) == (key & mask)))) {
        return ;
      }
    }
  }
  if (ftbl_3605.max_idx == 4) {
    warn_full_ftbl_3605();
    return ;
  }
  cellty_ftbl_3605 carry  = {.key = key, .mask = mask, .prio = prio, .action_tag = action, .action_arg = const_arg};
  for (int _idx = 0; _idx < 4; _idx++) {
    if ((_idx < ftbl_3605.max_idx) && (ftbl_3605.entries[_idx].prio < prio)) {
      cellty_ftbl_3605 tmp  = ftbl_3605.entries[_idx];
      ftbl_3605.entries[_idx] = carry;
      carry = tmp;
    }
    if (_idx == ftbl_3605.max_idx) {
      ftbl_3605.entries[_idx] = carry;
      ftbl_3605.max_idx = _idx + 1;
      return ;
    }
  }
  return ;
}
void update_ftbl_3605(uint32_t key , uint32_t mask , uint32_t prio , uint32_t action , uint32_t const_arg ){
  for (int _idx = 0; _idx < 4; _idx++) {
    if (_idx < ftbl_3605.max_idx) {
      if ((ftbl_3605.entries[_idx].prio == prio) && ((ftbl_3605.entries[_idx].mask == mask) && ((ftbl_3605.entries[_idx].key & mask) == (key & mask)))) {
        ftbl_3605.entries[_idx].action_tag = action;
        ftbl_3605.entries[_idx].action_arg = const_arg;
        return ;
      }
    }
  }
  warn_update_ftbl_3605();
  return ;
}
void remove_ftbl_3605(uint32_t key , uint32_t mask , uint32_t prio ){
  uint8_t removed  = false;
  for (int _idx = 0; _idx < 4; _idx++) {
    if (_idx < ftbl_3605.max_idx) {
      if ((ftbl_3605.entries[_idx].prio == prio) && ((ftbl_3605.entries[_idx].mask == mask) && ((ftbl_3605.entries[_idx].key & mask) == (key & mask)))) {
        removed = true;
      }
      if (removed && ((_idx + 1) < ftbl_3605.max_idx)) {
        ftbl_3605.entries[_idx] = ftbl_3605.entries[_idx + 1];
      }
    }
  }
  if (removed) {
    ftbl_3605.max_idx = ftbl_3605.max_idx - 1;
    return ;
  }
  warn_remove_ftbl_3605();
  return ;
}
res_t_3598 lookup_ftbl_3605(uint32_t key , uint32_t arg ){
  for (int _idx = 0; _idx < 4; _idx++) {
    if (_idx >= ftbl_3605.max_idx) {
      switch (ftbl_3605._default.action_tag) {
        case tag_hit_acn_3601: {
          return hit_acn_3601(ftbl_3605._default.action_arg, arg);
          break;
        }
        default: {
          return miss_acn_3604(ftbl_3605._default.action_arg, arg);
          break;
        }
      }
    }
    if ((key & ftbl_3605.entries[_idx].mask) == (ftbl_3605.entries[_idx].key & ftbl_3605.entries[_idx].mask)) {
      switch (ftbl_3605.entries[_idx].action_tag) {
        case tag_hit_acn_3601: {
          return hit_acn_3601(ftbl_3605.entries[_idx].action_arg, arg);
          break;
        }
        case tag_miss_acn_3604: {
          return miss_acn_3604(ftbl_3605.entries[_idx].action_arg, arg);
          break;
        }
      }
    }
  }
  switch (ftbl_3605._default.action_tag) {
    case tag_hit_acn_3601: {
      return hit_acn_3601(ftbl_3605._default.action_arg, arg);
      break;
    }
    default: {
      return miss_acn_3604(ftbl_3605._default.action_arg, arg);
      break;
    }
  }
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
          uint32_t key_3606  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t val_3607  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t hit_3608  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_query_result(key_3606, val_3607, hit_3608);
          return packet->cursor <= packet->end;
          break;
        }
        case 2: {
          uint32_t key_3610  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t mask_3611  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t prio_3612  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t val_3613  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_do_install(key_3610, mask_3611, prio_3612, val_3613);
          return packet->cursor <= packet->end;
          break;
        }
        case 3: {
          uint32_t key_3615  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t mask_3616  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t prio_3617  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t val_3618  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_do_update(key_3615, mask_3616, prio_3617, val_3618);
          return packet->cursor <= packet->end;
          break;
        }
        case 4: {
          uint32_t key_3620  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t mask_3621  = ((uint32_t)(read_bits(packet, 32)));
          uint32_t prio_3622  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_do_remove(key_3620, mask_3621, prio_3622);
          return packet->cursor <= packet->end;
          break;
        }
        case 5: {
          uint32_t key_3624  = ((uint32_t)(read_bits(packet, 32)));
          (*(next_event)) = mk_do_query(key_3624);
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
      uint32_t key_3606  = ev_out->data.args.query_result_3609.key_3606;
      uint32_t val_3607  = ev_out->data.args.query_result_3609.val_3607;
      uint32_t hit_3608  = ev_out->data.args.query_result_3609.hit_3608;
      write_bits(buf_out, ((uint64_t)(hit_3608)), 32);
      write_bits(buf_out, ((uint64_t)(val_3607)), 32);
      write_bits(buf_out, ((uint64_t)(key_3606)), 32);
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
      uint32_t key_3610  = ev_out->data.args.do_install_3614.key_3610;
      uint32_t mask_3611  = ev_out->data.args.do_install_3614.mask_3611;
      uint32_t prio_3612  = ev_out->data.args.do_install_3614.prio_3612;
      uint32_t val_3613  = ev_out->data.args.do_install_3614.val_3613;
      write_bits(buf_out, ((uint64_t)(val_3613)), 32);
      write_bits(buf_out, ((uint64_t)(prio_3612)), 32);
      write_bits(buf_out, ((uint64_t)(mask_3611)), 32);
      write_bits(buf_out, ((uint64_t)(key_3610)), 32);
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
      uint32_t key_3615  = ev_out->data.args.do_update_3619.key_3615;
      uint32_t mask_3616  = ev_out->data.args.do_update_3619.mask_3616;
      uint32_t prio_3617  = ev_out->data.args.do_update_3619.prio_3617;
      uint32_t val_3618  = ev_out->data.args.do_update_3619.val_3618;
      write_bits(buf_out, ((uint64_t)(val_3618)), 32);
      write_bits(buf_out, ((uint64_t)(prio_3617)), 32);
      write_bits(buf_out, ((uint64_t)(mask_3616)), 32);
      write_bits(buf_out, ((uint64_t)(key_3615)), 32);
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
      uint32_t key_3620  = ev_out->data.args.do_remove_3623.key_3620;
      uint32_t mask_3621  = ev_out->data.args.do_remove_3623.mask_3621;
      uint32_t prio_3622  = ev_out->data.args.do_remove_3623.prio_3622;
      write_bits(buf_out, ((uint64_t)(prio_3622)), 32);
      write_bits(buf_out, ((uint64_t)(mask_3621)), 32);
      write_bits(buf_out, ((uint64_t)(key_3620)), 32);
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
      uint32_t key_3624  = ev_out->data.args.do_query_3625.key_3624;
      write_bits(buf_out, ((uint64_t)(key_3624)), 32);
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
      uint32_t key_3626  = ev_in->data.args.query_result_3609.key_3606;
      uint32_t val_3627  = ev_in->data.args.query_result_3609.val_3607;
      uint32_t hit_3628  = ev_in->data.args.query_result_3609.hit_3608;
      event_t this  = mk_query_result(key_3626, val_3627, hit_3628);
      break;
    }
    case 2: {
      uint32_t key_3629  = ev_in->data.args.do_install_3614.key_3610;
      uint32_t mask_3630  = ev_in->data.args.do_install_3614.mask_3611;
      uint32_t prio_3631  = ev_in->data.args.do_install_3614.prio_3612;
      uint32_t val_3632  = ev_in->data.args.do_install_3614.val_3613;
      event_t this  = mk_do_install(key_3629, mask_3630, prio_3631, val_3632);
      install_ftbl_3605(key_3629, mask_3630, prio_3631, tag_hit_acn_3601, val_3632);
      break;
    }
    case 3: {
      uint32_t key_3633  = ev_in->data.args.do_update_3619.key_3615;
      uint32_t mask_3634  = ev_in->data.args.do_update_3619.mask_3616;
      uint32_t prio_3635  = ev_in->data.args.do_update_3619.prio_3617;
      uint32_t val_3636  = ev_in->data.args.do_update_3619.val_3618;
      event_t this  = mk_do_update(key_3633, mask_3634, prio_3635, val_3636);
      update_ftbl_3605(key_3633, mask_3634, prio_3635, tag_hit_acn_3601, val_3636);
      break;
    }
    case 4: {
      uint32_t key_3637  = ev_in->data.args.do_remove_3623.key_3620;
      uint32_t mask_3638  = ev_in->data.args.do_remove_3623.mask_3621;
      uint32_t prio_3639  = ev_in->data.args.do_remove_3623.prio_3622;
      event_t this  = mk_do_remove(key_3637, mask_3638, prio_3639);
      remove_ftbl_3605(key_3637, mask_3638, prio_3639);
      break;
    }
    case 5: {
      uint32_t key_3640  = ev_in->data.args.do_query_3625.key_3624;
      event_t this  = mk_do_query(key_3640);
      res_t_3598 tup_3882  = lookup_ftbl_3605(key_3640, 1000);
      uint32_t r_0_3641  = tup_3882._0;
      uint8_t r_1_3642  = tup_3882._1;
      uint32_t hit_3643  = 0;
      if (r_1_3642 == true) {
        hit_3643 = 1;
      }
      printf("query key=%u -> val=%u hit=%u\n", key_3640, r_0_3641, hit_3643);
      out_event_t tmp_3890  = {.ev = mk_query_result(key_3640, r_0_3641, hit_3643), .port = 1};
      out_events[n] = tmp_3890;
      n = n + 1;
      break;
    }
    default: {
      
      break;
    }
  }
  return n;
}

