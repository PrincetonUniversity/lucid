open CCoreSyntax
open CCoreExceptions
open CCoreUtils


(* Simple Libpcap toplevel. 
    Just uses the default helpers, imports, pkt_handler and main_fun, 
    and has a simple main function that opens some pcaps from stdin *)   
     
(**** helpers ****)
let get_event_tag t_event = 
    let ev_param = cid"ev", tref t_event in
    dfun 
      (cid"get_event_tag")
      (tint event_tag_size)
      [ev_param]
      (sret (ecast (tint event_tag_size) ((param_evar ev_param)/->cid"tag")))
  ;;
  let reset_event_tag t_event = 
    (* this isn't right. Need an address.. *)
    let ev_param = cid"ev", tref t_event in
    let enum_ty = ((param_evar ev_param)/->cid"tag").ety in 
    dfun 
      (cid"reset_event_tag")
      (tunit)
      [ev_param]
      (sassign_exp ((param_evar ev_param)/->cid"tag") (ecast (enum_ty) (default_exp (tint event_tag_size))))
  ;;
  
  let init_cursor = 
    let bytes_ptr_t = tref CCoreParse.packet_t |> CCorePPrint.ty_to_string ~use_abstract_name:true in
    dforiegn [%string
 {|
 void init_cursor(char*  buf , uint32_t len , %{bytes_ptr_t}  bytes ){
    bytes->start = buf;
    bytes->payload = buf;
    bytes->end = buf + len;
 }|}]
 ;;
 let reset_cursor = 
    let bytes_ptr_t = tref CCoreParse.packet_t |> CCorePPrint.ty_to_string ~use_abstract_name:true in
    dforiegn [%string
 {|
 void reset_cursor(%{bytes_ptr_t}  bytes){
    bytes->payload = bytes->start;
 }|}]
 
 (* copy an input packet to an output packet *)
 (* Assumption: buf_out and buf_in are allocated by the same 
    internal function and have the same maximum size.
    This ensures that if data fits in buf_in, it will also fit in buf_out, 
    eliminating the need for explicit bounds checking within this function. *)
 let copy_packet = 
    dforiegn [%string
 {|
 void copy_packet(packet_t*  buf_out , packet_t*  buf_in ) {
    memcpy(buf_out->start, buf_in->start, buf_in->end - buf_in->start);
    buf_out->payload = buf_out->start + (buf_in->payload - buf_in->start);
    buf_out->end = buf_out->start + (buf_in->end - buf_in->start);
 }|}]    
 ;;
 

let helpers decls = 

    let teventstruct = match (find_ty_opt (CCoreEvents.event_ty_id) decls) with 
        | Some(ty) -> ty
        | _ -> err "no tevent"
    in  
    [
        get_event_tag teventstruct;
        reset_event_tag teventstruct;
        init_cursor;
        reset_cursor;
        copy_packet;
    ]
;;

let imports = [
        CCoreSyntax.dinclude "<stdio.h>";
        CCoreSyntax.dinclude "<stdlib.h>";
        CCoreSyntax.dinclude "<stdint.h>";
        CCoreSyntax.dinclude "<stdbool.h>";
        dinclude "<pcap.h>"; 
        dinclude "<string.h>";
        dforiegn 
{|
#ifdef DEBUG
    #define debug_printf(...) printf(__VA_ARGS__)
    #else
    #define debug_printf(...)
#endif            
|};
        dforiegn 
{|
#ifdef __GNUC__
    #define unroll GCC unroll
#endif
|}    
]
let pkt_handler = dforiegn [%string
{|
uint8_t pkt_handler(uint8_t ingress_port, packet_t* pkt_in, packet_t* pkt_out){
   // locals
   event_t ev1_v = {0};
   event_t ev2_v = {0};
   event_t ev_out_v = {0};
   int generated_port = 0; // return value

   event_t * ev1 = &ev1_v;
   event_t * ev2 = &ev2_v;
   event_t * ev_out = &ev_out_v;
   event_t * ev_tmp;
   uint8_t parse_success = parse_event(pkt_in, ev1);       
   if (parse_success == 1) {
      // event continuation trampoline
      #pragma unroll 4
      for (int i=0; i < 100; i++) {
         reset_event_tag(ev2);
         generated_port = handle_event(ingress_port, ev1, ev2, ev_out);
         if (get_event_tag(ev2) == 0) {
            break;
         }
         ev_tmp = ev1;
         ev1 = ev2; 
         ev2 = ev_tmp;
      }
      // we have generated an output event, write it to the buffer
      if (generated_port != 0) {
            copy_packet(pkt_out, pkt_in);
            deparse_event(ev_out, pkt_out);
      }
   }
   else {
      debug_printf ("parse failed!\n");
      exit(1);
   }
   return generated_port;
}|}]
;;
let main = 
  dforiegn 
  [%string{| 
/********* lpcap packet driver ***********/
uint64_t pkt_ct = 0;


typedef struct pkt_hdl_ctx_t {
    uint8_t ingress_port;
    u_char *out_pcap;
    packet_t in_pkt;
    packet_t out_pkt;
    struct pcap_pkthdr out_pkthdr;
} pkt_hdl_ctx_t;


void fill_out_pkthdr(const struct pcap_pkthdr *in_pkthdr, packet_t* out_pkt, struct pcap_pkthdr* out_pkthdr) {
    out_pkthdr->ts = in_pkthdr->ts;
    out_pkthdr->caplen = out_pkt->payload - out_pkt->start;
    out_pkthdr->len = in_pkthdr->len;
}

void lpcap_packet_handler(u_char *ctx, const struct pcap_pkthdr *pkthdr, const u_char *packet) {
    pkt_hdl_ctx_t * hdl_ctx = (pkt_hdl_ctx_t *)ctx;
    init_cursor((char *)packet, pkthdr->len, &hdl_ctx->in_pkt); // construct a new cursor
    reset_cursor(&hdl_ctx->out_pkt); // reset the out cursor, it never changes
    uint8_t out_port = pkt_handler(hdl_ctx->ingress_port, &hdl_ctx->in_pkt, &hdl_ctx->out_pkt);

    if(out_port != 0) {
        fill_out_pkthdr(pkthdr, &hdl_ctx->out_pkt, &hdl_ctx->out_pkthdr);
        pcap_dump(hdl_ctx->out_pcap, &hdl_ctx->out_pkthdr, (u_char *)hdl_ctx->out_pkt.start);
    }
    pkt_ct++;
}
/********** allocation + main ***********/
pkt_hdl_ctx_t mk_pkt_hdl_ctx(pcap_dumper_t* out_pcap, u_char* out_buf, uint32_t out_buf_len) {
pkt_hdl_ctx_t ctx = {
    .ingress_port = 1, // always 1 in this driver
    .out_pcap = (u_char *)out_pcap,
    .in_pkt = {0},
    .out_pkt = {
    .start = (char *)out_buf,
    .payload = (char *)out_buf,
    .end = (char *)out_buf + out_buf_len
    },
    .out_pkthdr = {0}
};
return ctx;
}

int main(int argc, char const *argv[]){
    if (argc != 3) {
        debug_printf(stderr, "Usage: %s <input pcap file> <output pcap file>\n", argv[0]);
        return 1;
    }

    char errbuf[PCAP_ERRBUF_SIZE];    
    // Open the input pcap file in read mode
    pcap_t *in_pcap = pcap_open_offline(argv[1], errbuf);
    if (in_pcap == NULL) {
        debug_printf(stderr, "Error opening input pcap file: %s\n", errbuf);
        return 1;
    }

    // Open the output pcap file in write mode
    pcap_dumper_t *out_pcap = pcap_dump_open(in_pcap, argv[2]);
    if (out_pcap == NULL) {
        debug_printf(stderr, "Error opening output pcap file: %s\n", pcap_geterr(in_pcap));
        return 1;
    }

    // prepare the context for the packet handler
    u_char outbuf[1600];
    pkt_hdl_ctx_t ctx = mk_pkt_hdl_ctx(out_pcap, outbuf, 1600);

    pcap_loop(in_pcap, 0, lpcap_packet_handler, (u_char *)&ctx);

    printf("Processed %llu packets\n", pkt_ct);

    // Close the pcap files
    pcap_dump_close(out_pcap);
    pcap_close(in_pcap);

    return 0;
}|}]

let package_prog decls = 
    [
        "lucidprog.c", `Decls (imports @ decls @ helpers decls @ [pkt_handler] @ [main]);
        "makefile", `String ("all: lucidprog\n\nlucidprog: lucidprog.c\n\tgcc -o lucidprog lucidprog.c -lpcap\n\n")
    ]
;;

