#!/usr/bin/env python3
"""Generate qos.json for the Lucid qos example.

Single switch (default 1-switch sim). Sends three packets in different
L4 protocols and checks the diffserv field is rewritten according to
the per-protocol policy:
   UDP  → diffserv = 46 (EF)
   TCP  → diffserv = 44 (Voice Admit)
   ICMP → unchanged
"""

import ipaddress
import json
from pathlib import Path

from scapy.all import Ether, IP, TCP, UDP, ICMP

def ipv4_int(s): return int(ipaddress.IPv4Address(s))
def mac_int(s):  return int(s.replace(":", ""), 16)

H1_MAC = "08:00:00:00:01:01"
H2_MAC = "08:00:00:00:02:02"
S1_MAC = "08:00:00:00:01:00"

def install_lpm(dst_ip, dmac, port):
    return {
        "type": "command", "name": "Table.install",
        "args": {
            "table": "ipv4_lpm",
            "key": [f"{ipv4_int(dst_ip)}<32>"],
            "action": "ipv4_lpm.ipv4_forward",
            "args": [f"{mac_int(dmac)}<48>", f"{port}<32>"],
        },
    }

def packet(l4, dst_ip="10.0.2.2", src_ip="10.0.1.1", tos=0):
    """Build h1→h2 packet with the given L4 layer. `tos` is the full
    8-bit TOS byte (diffserv:6 + ecn:2)."""
    ip = IP(src=src_ip, dst=dst_ip, ttl=64, id=0, flags=0, frag=0,
            tos=tos, len=20 + len(bytes(l4)))
    return bytes(Ether(dst=S1_MAC, src=H1_MAC, type=0x0800) / ip / l4).hex()

events = [
    install_lpm("10.0.2.2", H2_MAC, port=2),
    install_lpm("10.0.1.1", H1_MAC, port=1),
]

ts = 5000
for label, pkt in [
    ("UDP h1→h2  (expect dscp=46)",   packet(UDP(sport=1111, dport=80))),
    ("TCP h1→h2  (expect dscp=44)",   packet(TCP(sport=2222, dport=80))),
    ("ICMP h1→h2 (dscp unchanged=0)", packet(ICMP())),
    ("UDP h1→h2 with tos=0xfc (preserve ecn=00, mark dscp=46)",
                                      packet(UDP(sport=3333, dport=80), tos=0xfc)),
]:
    events.append({
        "type": "packet",
        "bytes": pkt,
        "locations": ["0:1"],
        "timestamp": ts,
    })
    ts += 1000

spec = {
    "max time": 15000,
    "default_input_gap": 100,
    "events": events,
}

out = Path(__file__).with_name("qos.json")
out.write_text(json.dumps(spec, indent=2) + "\n")
print(f"wrote {out} with {len(events)} events")
