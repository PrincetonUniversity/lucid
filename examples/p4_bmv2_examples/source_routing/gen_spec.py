#!/usr/bin/env python3
"""Generate source_routing.json for the Lucid source_routing example.

Run with `python gen_spec.py`. Builds the topology, table-free spec
(this example has no control-plane state), and source-routed test
packets via scapy.
"""

import json
from pathlib import Path

from scapy.all import (
    Ether, IP, Packet, BitField, bind_layers,
)

ETY_SRC_ROUTE = 0x1234

# 16-bit per-hop label: bos (1 bit) + port (15 bits). Same on-wire layout
# as the P4 tutorial's `srcRoute_t`.
class SR(Packet):
    name = "SR"
    fields_desc = [
        BitField("bos", 0, 1),
        BitField("port", 0, 15),
    ]

bind_layers(Ether, SR, type=ETY_SRC_ROUTE)
bind_layers(SR,    SR, bos=0)
bind_layers(SR,    IP, bos=1)

# ---- topology ------------------------------------------------------------
# Node IDs: 0=s1, 1=s2, 2=s3. Triangle, one host per switch on port 1
# (undeclared → exits there).

TOPOLOGY = {
    "nodes": {
        "0": {"ports": {"2": {"type": "link"}, "3": {"type": "link"}}},
        "1": {"ports": {"2": {"type": "link"}, "3": {"type": "link"}}},
        "2": {"ports": {"2": {"type": "link"}, "3": {"type": "link"}}},
    },
    "links": [
        {"0:2": "1:2"},   # s1:p2 <-> s2:p2
        {"0:3": "2:2"},   # s1:p3 <-> s3:p2
        {"1:3": "2:3"},   # s2:p3 <-> s3:p3
    ],
}

# ---- helpers -------------------------------------------------------------

H1_MAC = "08:00:00:00:01:01"
S1_MAC = "08:00:00:00:01:00"

def sr_packet(labels, ipv4_dst="10.0.2.2", ipv4_src="10.0.1.1",
              src=H1_MAC, dst=S1_MAC, ttl=64):
    """Build a source-routed packet.

    `labels` is a list of egress ports. Each is wrapped in a 16-bit
    SR label; the last one gets bos=1 (the hop that strips the header).
    """
    assert 1 <= len(labels) <= 4
    layers = [
        SR(bos=(1 if i == len(labels) - 1 else 0), port=p)
        for i, p in enumerate(labels)
    ]
    stack = layers[0]
    for layer in layers[1:]:
        stack = stack / layer
    pkt = (Ether(dst=dst, src=src, type=ETY_SRC_ROUTE) /
           stack /
           IP(src=ipv4_src, dst=ipv4_dst, ttl=ttl, id=0, flags=0, frag=0,
              tos=0, len=20))
    return bytes(pkt).hex()

def overflow_packet(n=5, src=H1_MAC, dst=S1_MAC):
    """Build a packet whose SR stack has n labels with no bos=1 in
    the first `min(n, 4)` — used to confirm the MAX_HOPS=4 overflow drop."""
    stack = None
    for i in range(n):
        layer = SR(bos=0, port=i + 1)
        stack = layer if stack is None else stack / layer
    pkt = (Ether(dst=dst, src=src, type=ETY_SRC_ROUTE) / stack)
    return bytes(pkt).hex()

# ---- test scenarios -----------------------------------------------------

TESTS = [
    # h1 → h2 via s1, s2. Two labels.
    #   s1 reads (bos=0,port=2): pop, forward port 2 → enters s2:2.
    #   s2 reads (bos=1,port=1): last hop, strip SR, exit port 1 (h2).
    ("h1→h2 via s1,s2 (2 labels)", sr_packet([2, 1], ipv4_dst="10.0.2.2")),

    # h1 → h3 via s1, s3.
    ("h1→h3 via s1,s3 (2 labels)", sr_packet([3, 1], ipv4_dst="10.0.3.3")),

    # h1 → h2 via the LONG path s1, s3, s2. Three labels.
    #   s1 → port 3 (s3:2). s3 → port 3 (s2:3). s2 → port 1 (h2).
    ("h1→h2 via s1,s3,s2 (3 labels)", sr_packet([3, 3, 1],
                                                ipv4_dst="10.0.2.2")),

    # 4-label route exercising MAX_HOPS exactly: h1 → s1 → s2 → s3 → s2 → h2.
    # Loops back through s2 unnecessarily, but lets us hit the sr4 path.
    ("h1→h2 via s1,s2,s3,s2 (4 labels, MAX_HOPS)",
     sr_packet([2, 3, 3, 1], ipv4_dst="10.0.2.2")),

    # Stack overflow: 5 labels, none bos=1 in first 4 — sr_chain_3 drops.
    ("stack overflow at MAX_HOPS=4", overflow_packet(5)),
]

events = []
ts = 5000
for label, bytes_hex in TESTS:
    events.append({
        "type": "packet",
        "bytes": bytes_hex,
        "locations": ["0:1"],
        "timestamp": ts,
    })
    ts += 1000

spec = {
    "max time": 20000,
    "default_input_gap": 100,
    "topology": TOPOLOGY,
    "events": events,
}

out = Path(__file__).with_name("source_routing.json")
out.write_text(json.dumps(spec, indent=2) + "\n")
print(f"wrote {out} with {len(events)} packet events")
for (label, _), ev in zip(TESTS, events):
    print(f"  t={ev['timestamp']:>5}  {label}")
