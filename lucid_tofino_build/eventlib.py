
import socket, time, os, struct
from collections import namedtuple

 
lucid_etype = 0x666
EthHdr = namedtuple("EthHdr", "dst_addr src_addr etype")
EthHdr.fmt = "!6s6sH"
WireEvHdr = namedtuple("WireEv", "event_id port_event_id event_bitvec_pad")
WireEvHdr.fmt = "!BB1s"


# static
def parse_header(data, HdrDef):
    # return parsed fields and remaining data.
    if (len(data) < struct.calcsize(HdrDef.fmt)):
        return None, []
    return HdrDef( *struct.unpack(HdrDef.fmt, data[0:struct.calcsize(HdrDef.fmt)])), data[struct.calcsize(HdrDef.fmt):]
def deparse_header(hdr, HdrDef, payload):
  return struct.pack(HdrDef.fmt, *hdr) + payload

# more static code -- parsers and deparsers
def parse_eventpacket(pktbuf):
  eth, payload = parse_header(pktbuf, EthHdr)
  if (eth.etype == lucid_etype):
    wireev, payload = parse_header(payload, WireEvHdr)
    for HdrDef in events:
      if (wireev.event_id == HdrDef.id):
        event, payload = parse_header(payload, HdrDef)
        return event, payload
    # if we got here, its an unknown event type
    return None

def deparse_eventpacket(event, payload):
  # smac and dmac are fixed for now
  smac = b'\x01'*6
  dmac = b'\x02'*6 
  # deparse event header
  for HdrDef in events:
    if (event.id == HdrDef.id):
      # add event header
      pktbuf = deparse_header(event, HdrDef, payload)
      # add event metadata header -- includes the bridged header
      wireev = WireEvHdr(event.id, 0, b'\x00'*(struct.calcsize(WireEvHdr.fmt)-2))
      pktbuf = deparse_header(wireev, WireEvHdr, pktbuf)
      # finally add the lucid ethernet hdr
      lucid_eth = EthHdr(dmac, smac, lucid_etype)
      pktbuf = deparse_header(lucid_eth, EthHdr, pktbuf)
      return pktbuf
  # if we got here, its an unknown event type
  return None


mysrereset = namedtuple("mysrereset", "mysrereset_mysreidx")
mysrereset.fmt = "!I"
mysrereset.id  = 4
mysrereset.name = mysrereset


ttl_changes_found = namedtuple("ttl_changes_found", "ttl_changes_found_idx ttl_changes_found_count")
ttl_changes_found.fmt = "!IB"
ttl_changes_found.id  = 3
ttl_changes_found.name = ttl_changes_found


DNS_packet_out = namedtuple("DNS_packet_out", "DNS_packet_out_sip DNS_packet_out_cip DNS_packet_out_smac DNS_packet_out_cmac DNS_packet_out_ttl")
DNS_packet_out.fmt = "!IIIII"
DNS_packet_out.id  = 2
DNS_packet_out.name = DNS_packet_out


DNS_packet_fwd = namedtuple("DNS_packet_fwd", "DNS_packet_fwd_sip DNS_packet_fwd_cip DNS_packet_fwd_smac DNS_packet_fwd_cmac DNS_packet_fwd_ttl")
DNS_packet_fwd.fmt = "!IIIII"
DNS_packet_fwd.id  = 1
DNS_packet_fwd.name = DNS_packet_fwd


events = [mysrereset, ttl_changes_found, DNS_packet_out, DNS_packet_fwd]


############ raw socket helpers ############
def open_socket(iface):
  s = socket.socket(socket.PF_PACKET, socket.SOCK_RAW, socket.htons(0x0003))
  s.bind((iface, 0))
  return s

def tx_pkt(s, pkt):
  s.send(pkt)

def rx_pkt(s):
  # get the next incoming packet
  pkt, addr = s.recvfrom(2048)
  (intf, proto, pkttype, hatype, addr) = addr
  while (pkttype == socket.PACKET_OUTGOING):
    pkt, (intf, proto, pkttype, hatype, addr) = s.recvfrom(2048)
  return pkt

def close_socket(s):
  s.close()

############ end raw socket helpers ############


# mapping based on p4tapp assignment
def dpid_to_veth(dpid):
  return ("veth%i"%(dpid * 2 + 1))
