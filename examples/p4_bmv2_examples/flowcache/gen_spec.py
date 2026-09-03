#!/usr/bin/env python3
"""Generate flowcache.json for the Lucid flowcache example.

Single switch (no topology block — default 1-switch sim is fine). The
test traces a typical flowcache lifecycle:

  1. A burst of packets in flow A arrives; the cache is empty, all miss
     and produce PacketIn events that show up in `Exits`.
  2. The "controller" (this JSON spec) installs a flow_cache entry for
     flow A.
  3. A second burst of flow-A packets arrives — they hit the cache and
     get forwarded.
  4. A packet in flow B arrives — still misses (no rule installed).
"""

import ipaddress
import json
from pathlib import Path

from scapy.all import Ether, IP, TCP

# ---- helpers ------------------------------------------------------------

def ipv4_int(s): return int(ipaddress.IPv4Address(s))
def mac_int(s):  return int(s.replace(":", ""), 16)

H1_MAC = "08:00:00:00:01:01"
H2_MAC = "08:00:00:00:02:02"
S1_MAC = "08:00:00:00:01:00"

def ipv4_tcp(src_ip, dst_ip, sport=1111, dport=80,
             src_mac=H1_MAC, dst_mac=S1_MAC, ttl=64):
    p = (Ether(dst=dst_mac, src=src_mac, type=0x0800) /
         IP(src=src_ip, dst=dst_ip, ttl=ttl, id=0, flags=0, frag=0,
            tos=0, len=40) /
         TCP(sport=sport, dport=dport, seq=0, ack=0, dataofs=5,
             reserved=0, flags=0, window=0, urgptr=0))
    return bytes(p).hex()

def install_flow(key_proto, key_src, key_dst, dmac, port):
    """Install a flow_cache entry. Key is the (proto, src, dst) record."""
    return {
        "type": "command", "name": "Table.install",
        "args": {
            "table": "flow_cache",
            "key": [f"{key_proto}<8>",
                    f"{ipv4_int(key_src)}<32>",
                    f"{ipv4_int(key_dst)}<32>"],
            "action": "flow_cache.cached_action",
            "args": [f"{mac_int(dmac)}<48>", f"{port}<32>"],
        },
    }

PROTO_TCP = 6
FLOW_A = (PROTO_TCP, "10.0.1.1", "10.0.2.2")
FLOW_B = (PROTO_TCP, "10.0.1.1", "10.0.3.3")

# ---- timeline ----------------------------------------------------------

events = []
ts = 1000

# (1) initial burst: 3 packets in flow A, cache is empty → all miss
for _ in range(3):
    events.append({
        "type": "packet",
        "bytes": ipv4_tcp(FLOW_A[1], FLOW_A[2]),
        "locations": ["0:1"],
        "timestamp": ts,
    })
    ts += 200

# (2) controller installs the rule for flow A
events.append({**install_flow(FLOW_A[0], FLOW_A[1], FLOW_A[2],
                              dmac=H2_MAC, port=2),
               "timestamp": ts})
ts += 200

# (3) second burst on flow A — should hit
for _ in range(3):
    events.append({
        "type": "packet",
        "bytes": ipv4_tcp(FLOW_A[1], FLOW_A[2]),
        "locations": ["0:1"],
        "timestamp": ts,
    })
    ts += 200

# (4) a single packet in flow B — still misses
events.append({
    "type": "packet",
    "bytes": ipv4_tcp(FLOW_B[1], FLOW_B[2]),
    "locations": ["0:1"],
    "timestamp": ts,
})

spec = {
    "max time": 10000,
    "default_input_gap": 100,
    "events": events,
}

out = Path(__file__).with_name("flowcache.json")
out.write_text(json.dumps(spec, indent=2) + "\n")
print(f"wrote {out} with {len(events)} events")
