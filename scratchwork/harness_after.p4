// Snippets from transformed P4 harness.

// Parser for eth/ip.
const bit<16> ETHERTYPE_IPV4 = 16w0x0800;
const bit<16> ETHERTYPE_DPT = 0x1111;
parser EthIpParser(packet_in pkt, out header_t hdr, out metadata_t md){
    DptIngressParser() dptIngressParser; // FIXED INTEGRATION.
    state start {
        pkt.extract(hdr.ethernet);
        transition select(hdr.ethernet.ether_type) {
            ETHERTYPE_IPV4 : parse_ip;
            ETHERTYPE_DPT  : parse_dpt;
            default : accept;
        }
    }
    state parse_ip {
        pkt.extract(hdr.ip);
        md.dptMeta.eventType = e_ip_in; // Added by linker.
        transition accept;
    }
    state parse_dpt { // elided
    }
}

control Ingress(
        inout header_t hdr, 
        inout metadata_t md,
        in ingress_intrinsic_metadata_t ig_intr_md,
        in ingress_intrinsic_metadata_from_parser_t ig_prsr_md,
        inout ingress_intrinsic_metadata_for_deparser_t ig_dprsr_md,
        inout ingress_intrinsic_metadata_for_tm_t ig_tm_md) {

    // Linker-generated table.
    table tbl_lucid_setup() {
        key = {
            md.dptMeta.eventType : exact;
        } actions = {
            fill_ip_in;
            noop;
        }
    }
    // Linker-generated actions.
    action fill_ip_in() {
        md.ip_in.tos = hdr.ip.tos;
        md.ip_in.src = hdr.ip.src_addr;
        md.ip_in.dst = hdr.ip.dst_addr;
        md.ip_in.len = hdr.ip.total_len;                
    }

    apply {
        apply(lucid_setup);
        // call lucid tables (elided)

        md.ig_tm_md.egr_port = mg.ig_intr_md.ingress_port;
    }

}