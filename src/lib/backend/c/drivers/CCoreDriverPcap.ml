open CCoreSyntax
open CCoreExceptions
open CCoreUtils
open CCoreDriverInterface


(* Simple Libpcap toplevel. 
    Just uses the default helpers, imports, pkt_handler and main_fun, 
    and has a simple main function that opens some pcaps from stdin *)   

let helpers = default_helpers
let imports = default_imports
    @[
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
let pkt_handler = default_pkt_handler
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


let cflags = "-lpcap"

(* *)

