#!/usr/bin/env python3
"""Generate ecn.json for the Lucid ecn example.

Plan:
  1. Install a forwarding rule for h2's IP.
  2. Kick off the recursive queue_decr drain at t=0.
  3. Burst of IPv4 packets at densely-packed timestamps so the queue
     depth climbs faster than the drain can keep up.
  4. Long enough pause + a few more packets to confirm the drain
     brings the depth back below threshold.

Tuning notes:
  - `default_input_gap` is the per-event timestamp spacing applied
     when an event's `timestamp` is omitted. For the burst phase we
     give every packet the *same* explicit timestamp so they all hit
     the queue within one simulator window (depth ramps up cleanly).
  - The drain rate is whatever generate(queue_decr()) -> self-handler
     decides — empirically about one tick per ~600 simulator units in
     the default config, which is plenty slow that a tight packet
     burst will overrun it.
"""

import ipaddress
import json
from pathlib import Path

from scapy.all import Ether, IP

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

def ipv4(src_ip="10.0.1.1", dst_ip="10.0.2.2",
         src_mac=H1_MAC, dst_mac=S1_MAC, ttl=64,
         ecn=1):    # ECT(1): ECN-capable transport
    p = (Ether(dst=dst_mac, src=src_mac, type=0x0800) /
         IP(src=src_ip, dst=dst_ip, ttl=ttl, id=0, flags=0, frag=0,
            tos=ecn, len=20))
    return bytes(p).hex()

events = [
    install_lpm("10.0.2.2", H2_MAC, port=2),
    # Kick off the drain — single event, the handler recurses.
    {"name": "queue_decr", "args": [], "locations": ["0:0"], "timestamp": 100},
]

# A burst of 14 packets all at t=200 — they get processed in succession
# by the interpreter before any queue_decr tick fires.
for i in range(14):
    events.append({
        "type": "packet",
        "bytes": ipv4(),
        "locations": ["0:1"],
        "timestamp": 200,
    })

# A long pause then a couple of trailing packets — by now the drain
# should have caught up, so these should be back in the "OK" regime.
for i in range(3):
    events.append({
        "type": "packet",
        "bytes": ipv4(),
        "locations": ["0:1"],
        "timestamp": 30000 + i * 100,
    })

spec = {
    "max time": 40000,
    "default_input_gap": 50,
    "events": events,
}

out = Path(__file__).with_name("ecn.json")
out.write_text(json.dumps(spec, indent=2) + "\n")
print(f"wrote {out} with {len(events)} events")
