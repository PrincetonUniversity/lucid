#!/usr/bin/env python3
"""Generate mri.json for the Lucid MRI example.

MRI packets are IPv4 with `ihl > 5`, a 4-byte option (option=MRI), an
mri header (count), and `count` 8-byte swtrace entries. Each Lucid switch
pushes a fresh (swid=self, qdepth=0) swtrace as the packet leaves it.

`gen_spec.py` only needs to emit the *initial* packet with count=0;
intermediate hops grow the stack inside the Lucid program.
"""

import ipaddress
import json
from pathlib import Path

from scapy.all import (
    Ether, IP, Packet, ByteField, ShortField, IntField, bind_layers,
)

IPV4_OPT_MRI = 31

# ---- wire-format scapy layers --------------------------------------------
#
# IPOption_MRI: 4 bytes — (copy:1, class:2, number:5) + length + count(16).
# Wraps the (option header + mri header) into a single 4-byte block.
# We omit a swtrace layer entirely; senders always start with count=0 so
# the initial packet has no swtraces. Intermediate hops fill them in.

class IPOption_MRI(Packet):
    name = "IPOption_MRI"
    fields_desc = [
        # IPv4 option header (2 bytes): copy(1)+class(2)+number(5) + length
        ByteField("opt_type", 0b00000000 | IPV4_OPT_MRI),  # copy=0, class=0, number=31
        ByteField("opt_len", 4),  # 2 bytes opt header + 2 bytes mri count
        # MRI header (2 bytes): count of trailing swtraces
        ShortField("count", 0),
    ]

# Bind option after IP when ihl > 5. scapy doesn't do this automatically;
# we'll just build the packet as Ether/IP/IPOption_MRI/Raw().

# ---- topology ------------------------------------------------------------
# Same triangle as source_routing: 0=s1, 1=s2, 2=s3. Hosts on port 1 of
# each switch (undeclared, packets exit there).

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

def ipv4_int(s):
    return int(ipaddress.IPv4Address(s))

def mac_int(s):
    return int(s.replace(":", ""), 16)

def install_lpm(node, dst_ip, dmac, port):
    return {
        "type": "command", "name": "Table.install", "locations": [node],
        "args": {
            "table": "ipv4_lpm",
            "key": [f"{ipv4_int(dst_ip)}<32>"],
            "action": "ipv4_lpm.ipv4_forward",
            "args": [f"{mac_int(dmac)}<48>", f"{port}<32>"],
        },
    }

H1_MAC = "08:00:00:00:01:01"
H2_MAC = "08:00:00:00:02:02"
H3_MAC = "08:00:00:00:03:03"
S1_MAC = "08:00:00:00:01:00"
S2_MAC = "08:00:00:00:02:00"
S3_MAC = "08:00:00:00:03:00"

# ---- table installs ------------------------------------------------------
#
# Each switch has two routing modes:
#   - "shortest" prefix (10.0.X.X) — direct route to the destination host.
#   - "detour" prefix (10.0.99.X) — route via s3 first to force a longer
#     path; lets us exercise the 3-hop mri_2 case.
#
# Concretely, packets to 10.0.99.99 traverse s1 -> s3 -> s2 -> h2.

events = []

# s1 (node 0): standard direct routes
events += [
    install_lpm(0, "10.0.1.1", H1_MAC, port=1),
    install_lpm(0, "10.0.2.2", S2_MAC, port=2),
    install_lpm(0, "10.0.3.3", S3_MAC, port=3),
    # detour route: send via s3 even though dst is on s2's side
    install_lpm(0, "10.0.99.99", S3_MAC, port=3),
]

# s2 (node 1)
events += [
    install_lpm(1, "10.0.1.1", S1_MAC, port=2),
    install_lpm(1, "10.0.2.2", H2_MAC, port=1),
    install_lpm(1, "10.0.3.3", S3_MAC, port=3),
    install_lpm(1, "10.0.99.99", H2_MAC, port=1),  # detour terminates here
]

# s3 (node 2)
events += [
    install_lpm(2, "10.0.1.1", S1_MAC, port=2),
    install_lpm(2, "10.0.2.2", S2_MAC, port=3),
    install_lpm(2, "10.0.3.3", H3_MAC, port=1),
    install_lpm(2, "10.0.99.99", S2_MAC, port=3),  # forward detour traffic to s2
]

# ---- helpers: build the initial MRI packet ------------------------------

def mri_packet(dst_ip="10.0.2.2", src_ip="10.0.1.1",
               src=H1_MAC, dst=S1_MAC, ttl=64):
    """Build an IPv4 packet with the MRI option header, count=0.

    Total IP header length = 24 bytes (20 + 4 option-and-mri header).
    """
    ip = IP(src=src_ip, dst=dst_ip, ttl=ttl, id=0, flags=0, frag=0,
            tos=0, len=24, ihl=6)
    opt = IPOption_MRI(count=0)
    pkt = (Ether(dst=dst, src=src, type=0x0800) / ip / opt)
    # scapy doesn't recompute checksum once we hand it a custom option
    # layer, so re-blat the raw bytes through IP() to force re-checksum.
    raw = bytes(pkt)
    # Recompute IPv4 checksum manually over the 24-byte header
    eth_bytes = raw[:14]
    ip_bytes = bytearray(raw[14:14+24])
    ip_bytes[10:12] = b"\x00\x00"  # zero csum
    s = 0
    for i in range(0, 24, 2):
        s += (ip_bytes[i] << 8) | ip_bytes[i + 1]
    while s >> 16:
        s = (s & 0xFFFF) + (s >> 16)
    csum = (~s) & 0xFFFF
    ip_bytes[10:12] = csum.to_bytes(2, "big")
    return (eth_bytes + bytes(ip_bytes) + raw[14+24:]).hex()

# ---- test packets --------------------------------------------------------

TESTS = [
    # 2-hop route: h1 -> s1 -> s2 -> h2. Should accumulate 2 swtraces
    # (swid=0 from s1, swid=1 from s2). Exit at 1:1, count=2.
    ("h1->h2 (2 hops, expect swtraces s1, s2)",
     mri_packet(dst_ip="10.0.2.2")),

    # 2-hop route: h1 -> s1 -> s3 -> h3. Swtraces s1, s3.
    ("h1->h3 (2 hops, expect swtraces s1, s3)",
     mri_packet(dst_ip="10.0.3.3")),

    # 3-hop detour: h1 -> s1 -> s3 -> s2 -> h2 (10.0.99.99 is steered
    # through s3 instead of direct s2). Swtraces s1, s3, s2.
    ("h1->h2 detour via s3 (3 hops, expect swtraces s1, s3, s2)",
     mri_packet(dst_ip="10.0.99.99")),

    # Unroutable: no entry for 10.99.99.99. Drops at s1.
    ("h1->10.99.99.99 (unroutable, drop)",
     mri_packet(dst_ip="10.99.99.99")),
]

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

out = Path(__file__).with_name("mri.json")
out.write_text(json.dumps(spec, indent=2) + "\n")
print(f"wrote {out} with {len(events)} events "
      f"({len(events) - len(TESTS)} installs + {len(TESTS)} packets)")
for (label, _), ev in zip(TESTS, events[-len(TESTS):]):
    print(f"  t={ev['timestamp']:>5}  {label}")
