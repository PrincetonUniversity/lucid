ip_harness.p4 is a minimal harness for lucid programs that process IP packets with fixed entry and exit events. 

Programs that use this harness should have 1 entry and exit event: 
    - ``entry event ip_in(int<9> igr_port, int src, int dst, int<16> len);``            
    - ``exit event ip_out(int<1> drop, int<9> egr_port);``


The ingress parser sets up the IP event, by copying fields to an appropriate metadata struct. The ingress control flow will  then either call a lucid block to process the packet (if it is  IP or a lucid control event), or otherwise call a P4 block  to handle non-IP packets.  

Psuedocode: 
```
ingress_parse {
    extract(md.intrinsic_md.igr_port);    
    md.lucid.igr_port  = md.intrinsic_md.igr_port;
    extract(hdr.eth);
    if (hdr.etype == IP){
        extract(hdr.ip);
        md.lucid.eventType = e_ip_in;
        md.lucid.ip_in.src = hdr.ip.src;
        md.lucid.ip_in.dst = hdr.ip.dst;
        md.lucid.ip_in.len = hdr.ip.len;
    }
}
ingress_control {
    if (md.lucid.eventType != 0) {
        call_lucid_code();    
        md.egr_port = md.lucid.ip_out.egr_port;
    } else {
        apply_user_p4_tables();    
    }
}
```
This harness uses a few Lucid compiler-generated variables that are not defined: 

- struct ``ip_in_t``, ``struct ip_out_t`` -- Structs for the entry and exit events. 
- ``md.ip_in``, ``md.ip_out`` -- Struct instances for entry and exit events. 
    - ``md.ip_in`` has fields: {``igr_port``, ``src``, ``dst``, ``len``}
    - md.ip_out has fields: {``drop``, ``egr_port``}
- ``e_ip_in``, ``e_ip_out`` -- event identifiers
- ``md.dptMeta.eventType`` -- the event that will execute when the current packet reaches the Lucid block
- ``md.dptMeta.exitEventType`` -- the event that the Lucid block returned to P4.

Lines that begin with ``@``, such as ``@DPT_HEADERS`` and ``@DPT_DISPATCH``, are macros that the Lucid compiler replaces with blocks of generated code.
