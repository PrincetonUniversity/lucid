// Snippets from P4 harness.
// Parser for eth/ip.
const bit<16> ETHERTYPE_IPV4 = 16w0x0800;
parser EthIpParser(packet_in pkt, 
 header_t hdr, out metadata_t md){
    state start {
        pkt.extract(hdr.ethernet);
        transition select(hdr.ethernet.ether_type) {
            ETHERTYPE_IPV4 : parse_ip;
            default : accept;
        }
    }
    state parse_ip {
        pkt.extract(hdr.ip);
        transition accept;
    }
    state parse_tcp {}
    state parse_udp {}
    state parse_dns {}

}

control Ingress(
        inout header_t hdr, 
        inout metadata_t md,
        in ingress_intrinsic_metadata_t ig_intr_md,
        in ingress_intrinsic_metadata_from_parser_t ig_prsr_md,
        inout ingress_intrinsic_metadata_for_deparser_t ig_dprsr_md,
        inout ingress_intrinsic_metadata_for_tm_t ig_tm_md) {

    apply {
        md.ig_tm_md.egr_port = mg.ig_intr_md.ingress_port;

        if (eventtype = report){ ff(); }
    }
}